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

File::File(LPCTSTR basePath, LPCTSTR path) : ProjectType(ptFile)
{
	CFileName fn(path);
	fn.Root(basePath);
	
	displayName = fn.GetFileName();
	fullPath = fn;
}

LPCTSTR File::GetDisplayName()
{
	return displayName.c_str();
}

LPCTSTR File::GetFileName()
{
	return fullPath.c_str();
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

void Folder::AddChild(Folder* folder)
{
	children.insert(children.begin(), folder);
}

void Folder::AddFile(LPCTSTR file)
{
	File* pFile = new File(basePath.c_str(), file);
	files.insert(files.begin(), pFile);
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

//////////////////////////////////////////////////////////////////////////////
// Project
//////////////////////////////////////////////////////////////////////////////

Project::Project(LPCTSTR projectFile) : Folder()
{
	type = ptProject;

	fileName = projectFile;
	bExists = FileExists(projectFile);
	CFileName(projectFile).GetPath(basePath);

	if(bExists)
	{
		parse();
	}
}

bool Project::Exists()
{
	return bExists;
}

#define PS_START	0x1
#define PS_PROJECT	0x2
#define PS_FOLDER	0x3

#define FILENODE	_T("File")
#define FOLDERNODE	_T("Folder")
#define PROJECTNODE	_T("Project")

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

void Project::processProject(XMLAttributes& atts)
{
	name = ATTVAL(_T("name"));
}

void Project::processFolder(XMLAttributes& atts)
{
	Folder* folder = new Folder(ATTVAL(_T("name")), basePath.c_str());
	folder->SetParent(currentFolder);
	currentFolder->AddChild(folder);
	currentFolder = folder;
}

void Project::processFile(XMLAttributes& atts)
{
	currentFolder->AddFile(ATTVAL(_T("path")));
}

//////////////////////////////////////////////////////////////////////////////
// Workspace
//////////////////////////////////////////////////////////////////////////////

Workspace::Workspace() : ProjectType(ptWorkspace)
{
	fileName = _T("");
}

Workspace::Workspace(LPCTSTR projectFile) : ProjectType(ptWorkspace)
{
	// TO DO:
	// 1. Parse XML Workspace File, collect project file names.
	// 2. Parse individual projects.
}

Workspace::~Workspace()
{
	Clear();
}

void Workspace::AddProject(Project* project)
{
	projects.insert(projects.begin(), project);
}

const PROJECT_LIST Workspace::GetProjects()
{
	return projects;
}

void Workspace::SetName(LPCTSTR name_)
{
	name = name_;
}

LPCTSTR Workspace::GetName()
{
	return name.c_str();
}

void Workspace::Clear()
{
	for(PL_IT i = projects.begin(); i != projects.end(); ++i)
	{
		delete (*i);
	}
}

bool Workspace::CanSave()
{
	return (fileName != _T(""));
}

} // namespace Projects