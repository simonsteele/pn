/**
 * @file project.cpp
 * @brief Projects
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "project.h"

#include <sstream>
#include <fstream>

namespace Projects
{

//////////////////////////////////////////////////////////////////////////////
// ProjectType
//////////////////////////////////////////////////////////////////////////////

ProjectType::ProjectType(PROJECT_TYPE type_)
{
	type = type_;
}

PROJECT_TYPE ProjectType::GetType()
{
	return type;
}

//////////////////////////////////////////////////////////////////////////////
// File
//////////////////////////////////////////////////////////////////////////////

File::File(LPCTSTR basePath, LPCTSTR path, Projects::Folder* parent) : ProjectType(ptFile)
{
	CFileName fn(path);

	if(fn.IsRelativePath())
	{
		relPath = path;
		fn.Root(basePath);
	}
	else
	{
		tstring actualPath = fn.GetPath();
		if(actualPath != basePath)
		{
			// This will do its best to make a relative path, eventually
			// giving up and returning the full path if it's unreasonable to make one.
			relPath = fn.GetRelativePath(basePath);
		}
		else
		{
			relPath = fn.GetFileName();
		}
	}
	
	displayName = fn.GetFileName();
	fullPath = fn;
	parentFolder = parent;
}

LPCTSTR File::GetDisplayName()
{
	return displayName.c_str();
}

LPCTSTR File::GetFileName()
{
	return fullPath.c_str();
}

Projects::Folder* File::GetFolder()
{
	return parentFolder;
}

bool File::Rename(LPCTSTR newFilePart)
{
	CFileName fn(fullPath.c_str());
	tstring path = fn.GetPath();
	
	CFileName fn2(newFilePart);
	fn2.Root(path.c_str());

	if( FileExists(fn2.c_str()) )
	{
		g_Context.m_frame->SetStatusText(_T("Cannot rename file, a file with the new name already exists."));
	}
	else
	{
		if( MoveFile(fullPath.c_str(), fn2.c_str()) != 0 )
		{
			fullPath = fn2;
			displayName = newFilePart;

			CFileName fnRel(relPath.c_str());
			if(fnRel.IsRelativePath())
			{
				relPath = fnRel.GetPath();
				relPath += fnRel.GetFileName();
			}
			else
			{
				relPath = fullPath;
			}

			setDirty();

			return true;
		}
		else
		{
			g_Context.m_frame->SetStatusText(_T("Rename file failed."));
		}
	}

	return false;
}

void File::WriteDefinition(ofstream& definition)
{
	definition << "<File path=\"" << relPath << "\" />\n";
}

void File::setDirty()
{
	parentFolder->setDirty();
}

//////////////////////////////////////////////////////////////////////////////
// Folder
//////////////////////////////////////////////////////////////////////////////

Folder::Folder() : ProjectType(ptFolder)
{
	parent = NULL;
}

Folder::Folder(LPCTSTR name_, LPCTSTR basepath) : ProjectType(ptFolder)
{
	parent = NULL;
	name = name_;
	basePath = basepath;
}

Folder::~Folder()
{
	Clear();
}

void Folder::SetName(LPCTSTR name_)
{
	name = name_;
}

LPCTSTR Folder::GetName()
{
	return name.c_str();
}

LPCTSTR Folder::GetBasePath()
{
	return basePath.c_str();
}

const FOLDER_LIST& Folder::GetFolders()
{
	return children;
}

const FILE_LIST& Folder::GetFiles()
{
	return files;
}

File* Folder::FindFile(LPCTSTR filename)
{
	CFileName cfn1(filename);
	const tstring& fn1 = cfn1.ToLower();

	for(FILE_IT i = files.begin(); i != files.end(); ++i)
	{
		CFileName cfn2((*i)->GetFileName());

		if(fn1 == cfn2.ToLower())
			return (*i);
	}

	File* pF = NULL;

	for(FL_IT i = children.begin(); i != children.end(); ++i)
	{
		pF = (*i)->FindFile(filename);
		if(pF)
			return pF;
	}
	
	return pF;
}

void Folder::AddChild(Folder* folder)
{
	folder->SetParent(this);
	children.insert(children.end(), folder);
	setDirty();
}

File* Folder::AddFile(LPCTSTR file)
{
	File* pFile = new File(basePath.c_str(), file, this);
	files.insert(files.begin(), pFile);
	setDirty();
	return pFile;
}

void Folder::RemoveFile(File* file)
{
	files.remove(file);
	setDirty();
	delete file;
}

void Folder::RemoveChild(Folder* folder)
{
	children.remove(folder);
	setDirty();
	delete folder;
}

void Folder::Clear()
{
	for(FL_IT i = children.begin(); i != children.end(); ++i)
	{
		delete (*i);
	}

	children.clear();

	for(FILE_IT i = files.begin(); i != files.end(); ++i)
	{
		delete (*i);
	}

	files.clear();
}

void Folder::SetParent(Folder* folder)
{
	parent = folder;
}

Folder* Folder::GetParent()
{
	return parent;
}

void Folder::WriteDefinition(ofstream& definition)
{
	definition << "<Folder name=\"" << name << "\">\n";
	
	writeContents(definition);

	definition << "</Folder>\n";
}

void Folder::writeContents(ofstream& definition)
{
	for(FOLDER_LIST::const_iterator i = children.begin();
		i != children.end();
		++i)
	{
		(*i)->WriteDefinition(definition);
	}

	for(FILE_LIST::const_iterator j = files.begin(); 
		j != files.end(); 
		++j)
	{
		(*j)->WriteDefinition(definition);
	}
}

void Folder::setDirty()
{
	if(parent)
		parent->setDirty();
}

//////////////////////////////////////////////////////////////////////////////
// Project
//////////////////////////////////////////////////////////////////////////////

#define FILENODE	_T("File")
#define FOLDERNODE	_T("Folder")
#define PROJECTNODE	_T("Project")

#define PS_START	0x1
#define PS_PROJECT	0x2
#define PS_FOLDER	0x3

bool Project::CreateEmptyProject(LPCTSTR projectname, LPCTSTR filename)
{
	Project Fake;
	Fake.basePath = CFileName(filename).GetPath();
	Fake.SetName(projectname);
	Fake.fileName = filename;
	Fake.Save();

	return FileExists(filename);
}

Project::Project()
{
	type = ptProject;

	bDirty = true;
	bExists = true;
}

Project::Project(LPCTSTR projectFile) : Folder()
{
	type = ptProject;

	fileName = projectFile;
	bExists = FileExists(projectFile);
		
	basePath = CFileName(projectFile).GetPath();

	if(bExists)
	{
		parse();
	}

	bDirty = !bExists;
}

bool Project::Exists()
{
	return bExists;
}

void Project::Save()
{
	ofstream str;
	str.open(fileName.c_str(), ios_base::out);
	
	writeDefinition(str);

	str.close();
	bDirty = false;
}

void Project::SetFileName(LPCTSTR filename)
{
	fileName = filename;
}

bool Project::IsDirty()
{
	return bDirty;
}

void Project::parse()
{
	currentFolder = this;
	parseState = PS_START;
	
	XMLParser parser;
	parser.SetParseState(this);
	try
	{
		parser.LoadFile(fileName.c_str());
	}
	catch (XMLParserException& ex)
	{
		::OutputDebugString(ex.GetMessage());
	}
}

#define MATCH(ename) \
	(_tcscmp(name, ename) == 0)

#define IN_STATE(state) \
	(parseState == state)

#define STATE(state) \
	parseState = state

#define ATTVAL(attname) \
	atts.getValue(attname)

void Project::startElement(LPCTSTR name, XMLAttributes& atts)
{
	if( IN_STATE(PS_START) )
	{
		if( MATCH( PROJECTNODE ) )
		{
			processProject(atts);
			STATE(PS_PROJECT);
		}
	}
	else if( IN_STATE(PS_PROJECT) )
	{
		if( MATCH( FOLDERNODE ) )
		{
			nestingLevel = 0;
			processFolder(atts);
			STATE(PS_FOLDER);
		}
		else if( MATCH( FILENODE ) )
		{
			processFile(atts);
		}

	}
	else if( IN_STATE(PS_FOLDER) )
	{
		if( MATCH( FILENODE ) )
		{
			processFile(atts);
		}
		else if( MATCH( FOLDERNODE ) )
		{
			nestingLevel++;
			processFolder(atts);
		}
	}
}

void Project::endElement(LPCTSTR name)
{
	if( IN_STATE(PS_PROJECT) )
	{
		if( MATCH( PROJECTNODE ) )
		{
			STATE(PS_START);
		}
	}
	else if( IN_STATE(PS_FOLDER) )
	{
		if( MATCH( FOLDERNODE ) )
		{
			currentFolder = currentFolder->GetParent();

			if( nestingLevel == 0 )
				STATE(PS_PROJECT);
			else
				nestingLevel--;
		}
	}
}

void Project::writeDefinition(ofstream& definition)
{
	definition << "<?xml version=\"1.0\"?>\n";
	definition << "<Project name=\"" << name << "\">\n";

	writeContents(definition);

	definition << "</Project>";
}

void Project::processProject(XMLAttributes& atts)
{
	name = ATTVAL(_T("name"));
}

void Project::processFolder(XMLAttributes& atts)
{
	Folder* folder = new Folder(ATTVAL(_T("name")), basePath.c_str());
	currentFolder->AddChild(folder);
	currentFolder = folder;
}

void Project::processFile(XMLAttributes& atts)
{
	currentFolder->AddFile(ATTVAL(_T("path")));
}

void Project::setDirty()
{
	bDirty = true;
}

//////////////////////////////////////////////////////////////////////////////
// Workspace
//////////////////////////////////////////////////////////////////////////////

Workspace::Workspace() : ProjectType(ptWorkspace)
{
	fileName = _T("");
	bDirty = false;
}

Workspace::Workspace(LPCTSTR projectFile) : ProjectType(ptWorkspace)
{
	// TODO:
	// 1. Parse XML Workspace File, collect project file names.
	// 2. Parse individual projects.
	fileName = projectFile;
}

Workspace::~Workspace()
{
	Clear();
}

void Workspace::AddProject(Project* project)
{
	projects.insert(projects.begin(), project);
	bDirty = true;
}

void Workspace::RemoveProject(Project* project)
{
	projects.remove(project);
	delete project;
	bDirty = true;
}

const PROJECT_LIST Workspace::GetProjects()
{
	return projects;
}

void Workspace::SetName(LPCTSTR name_)
{
	bDirty = true;
	name = name_;
}

void Workspace::SetFileName(LPCTSTR filename_)
{
	fileName = filename_;
}

LPCTSTR Workspace::GetName()
{
	return name.c_str();
}

bool Workspace::CanSave()
{
	return (fileName != _T(""));
}

void Workspace::Save()
{
	//TODO: Implement Workspace Saving.
}

void Workspace::ClearDirty()
{
	bDirty = false;
}

bool Workspace::IsDirty(bool bRecurse)
{
	if(bDirty)
		return true;

	if(bRecurse)
	{
		for(PROJECT_LIST::const_iterator i = projects.begin(); i != projects.end(); ++i)
		{
			if((*i)->IsDirty())
				return true;
		}
	}

	return false;
}

File* Workspace::FindFile(LPCTSTR filename)
{
	File* pF = NULL;
	
	for(PL_IT i = projects.begin(); i != projects.end(); ++i)
	{
		pF = (*i)->FindFile(filename);
		if(pF)
			return pF;
	}

	return pF;
}

void Workspace::startElement(LPCTSTR name, XMLAttributes& atts)
{

}

void Workspace::endElement(LPCTSTR name)
{

}

void Workspace::Clear()
{
	for(PL_IT i = projects.begin(); i != projects.end(); ++i)
	{
		delete (*i);
	}
}

void Workspace::parse()
{
	parseState = PS_START;
	
	XMLParser parser;
	parser.SetParseState(this);
	try
	{
		parser.LoadFile(fileName.c_str());
	}
	catch (XMLParserException& ex)
	{
		::OutputDebugString(ex.GetMessage());
	}
}

} // namespace Projects