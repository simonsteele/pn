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
#include "include/filefinder.h"
#include "include/genx/genx.h"

#define u(x) (utf8)x

namespace Projects
{

//////////////////////////////////////////////////////////////////////////////
// Parsing Gubbins...
//////////////////////////////////////////////////////////////////////////////

#define FILENODE	_T("File")
#define FOLDERNODE	_T("Folder")
#define PROJECTNODE	_T("Project")
#define WORKSPACENODE	_T("Workspace")

#define PS_START		0x1
#define PS_PROJECT		0x2
#define PS_FOLDER		0x3
#define PS_WORKSPACE	0x4
#define PS_USERDATA		0x5
#define PS_FILE			0x7

#define MATCH(ename) \
	(_tcscmp(name, ename) == 0)

#define IN_STATE(state) \
	(parseState == state)

#define STATE(state) \
	parseState = state

#define ATTVAL(attname) \
	atts.getValue(attname)

#define SETVALIDATTSTR(str, attname) \
	{ \
		LPCTSTR ppppppszAtt = ATTVAL(attname); \
		if(ppppppszAtt != NULL) \
			str = ppppppszAtt; \
		else \
			str = "error"; \
	}

typedef struct tagProjectWriter
{
	genxElement eFile;
	genxAttribute aFilePath;
	genxWriter w;
} SProjectWriter;

//////////////////////////////////////////////////////////////////////////////
// Xml Data Storage
//////////////////////////////////////////////////////////////////////////////

XmlNode::XmlNode(LPCTSTR lpszNamespace, LPCTSTR lpszName)
{
	sNamespace = lpszNamespace;
	sName = lpszName;
}

XmlNode::XmlNode(LPCTSTR qualifiedName)
{
	TCHAR* pSep = _tcsrchr(qualifiedName, _T(':'));

	if(pSep != NULL)
	{
		TCHAR* buf = new TCHAR[_tcslen(qualifiedName)+1];

		sName = pSep+1;
		int nslen = (pSep - qualifiedName) / sizeof(TCHAR);
		_tcsncpy(buf, qualifiedName, (pSep - qualifiedName) / sizeof(TCHAR));
		buf[nslen] = _T('\0');
		
		delete [] buf;
	}
	else
		sName = qualifiedName;
}

void XmlNode::AddAttributes(XMLAttributes& atts)
{
	TCHAR* pSep;
	for(int i = 0; i < atts.getCount(); i++)
	{
		XmlAttribute* a;

		LPCTSTR nm = atts.getName(i);
		pSep = _tcsrchr(nm, _T(':'));
		if(pSep)
		{
			int nslen = (pSep - nm) / sizeof(TCHAR);
			TCHAR* buf = new TCHAR[nslen+1];
			_tcsncpy(buf, nm, nslen);
			buf[nslen] = _T('\0');
			
			a = new XmlAttribute(buf, pSep+1, atts.getValue(i));

			delete [] buf;
		}
		else
		{
			a = new XmlAttribute(NULL, nm, atts.getValue(i));
		}
		
		attributes.insert(attributes.end(), a);
	}
}

void XmlNode::Write(ProjectWriter writer)
{
	genxStartElementLiteral(writer->w, (utf8)sNamespace.c_str(), (utf8)sName.c_str());
	
	for(LIST_ATTRS::iterator i = attributes.begin(); i != attributes.end(); ++i)
	{
		(*i)->Write(writer);
	}
	
	for(LIST_NODES::iterator j = children.begin(); j != children.end(); ++j)
	{
		(*j)->Write(writer);
	}
	
	genxEndElement(writer->w);
}

XmlAttribute::XmlAttribute(LPCTSTR lpszNamespace, LPCTSTR lpszName, LPCTSTR lpszValue)
{
	sNamespace = lpszNamespace;
	sName = lpszName;
	sValue = lpszValue;
}

void XmlAttribute::Write(ProjectWriter writer)
{
	genxAddAttributeLiteral(writer->w, (utf8)sNamespace.c_str(), (utf8)sName.c_str(), (utf8)sValue.c_str());
}

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

void File::WriteDefinition(SProjectWriter* definition)
{
	genxStartElement(definition->eFile);
	genxAddAttribute(definition->aFilePath, (const utf8)relPath.c_str());
	genxEndElement(definition->w);
}

void File::setDirty()
{
	parentFolder->setDirty();
}

LIST_NODES& File::GetUserData()
{
	return userData;
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

	for(FL_IT j = children.begin(); j != children.end(); ++j)
	{
		pF = (*j)->FindFile(filename);
		if(pF)
			return pF;
	}
	
	return pF;
}

/**
 * Find a file purely by it's filename part, ignore all path.
 */
File* Folder::FindRelativeFile(LPCTSTR filename)
{
	CFileName cfn1(filename);
	cfn1.ToLower();
	const tstring& fn1 = cfn1.GetFileName();

	tstring tmp;

	for(FILE_IT i = files.begin(); i != files.end(); ++i)
	{
		CFileName cfn2((*i)->GetFileName());
		cfn2.ToLower();

		if(fn1 == cfn2.GetFileName())
			return (*i);
	}

	File* pF = NULL;

	for(FL_IT j = children.begin(); j != children.end(); ++j)
	{
		pF = (*j)->FindRelativeFile(filename);
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

class FolderAdder : FileFinderImpl<FolderAdder, FolderAdder>
{
	typedef FileFinderImpl<FolderAdder, FolderAdder> base;
	friend base;

public:
	FolderAdder() : base(this, &FolderAdder::onFind)
	{
		pFolder = NULL;
		pCursor = NULL;
		setDirChangeCallback(&FolderAdder::onEnterDir);
		setFinishedDirCallback(&FolderAdder::onLeaveDir);
	}

	Folder* GetFolder(LPCTSTR path, LPCTSTR filter, LPCTSTR basePath, bool recurse)
	{
		lpszBasePath = basePath;

		pFolder = newFolder(path);
		pCursor = pFolder;

		// make sure the path has a trailing slash, CPathName will do that.
		CPathName pn(path);

		Find(pn.c_str(), filter, recurse);

		return pFolder;
	}

protected:
	Folder* pFolder;
	Folder* pCursor;
	LPCTSTR lpszBasePath;

	void onFind(LPCTSTR path, LPCTSTR filename)
	{
		tstring fullpath(path);
		fullpath += filename;
		pCursor->AddFile(fullpath.c_str());
	}

	void onEnterDir(LPCTSTR path)
	{
		Folder* pFolder = newFolder(path);
		pCursor->AddChild(pFolder);
		pCursor = pFolder;
	}

	void onLeaveDir(LPCTSTR path)
	{
		pCursor = pCursor->GetParent();
	}

	Folder* newFolder(LPCTSTR path)
	{
		CPathName fn(path);
		tstring dirName = fn.GetDirectoryName();
		return new Folder(dirName.c_str(), lpszBasePath);
	}
};

Folder* Folder::AddFolder(LPCTSTR path, LPCTSTR filter, bool recursive)
{
	FolderAdder fa;
	Folder* folder = fa.GetFolder(path, filter, basePath.c_str(), recursive);
	AddChild(folder);
	setDirty();
	return folder;
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

void Folder::WriteDefinition(SProjectWriter* definition)
{
	genxStartElementLiteral(definition->w, NULL, u("Folder"));
	genxAddAttributeLiteral(definition->w, NULL, u("name"), u(name.c_str()));
	
	writeContents(definition);

	genxEndElement(definition->w);
}

void Folder::writeContents(SProjectWriter* definition)
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

LIST_NODES& Folder::GetUserData()
{
	return userData;
}

//////////////////////////////////////////////////////////////////////////////
// Project
//////////////////////////////////////////////////////////////////////////////

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

	bDirty = false;
}

bool Project::Exists()
{
	return bExists;
}

void Project::Save()
{
	//ofstream str;
	//str.open(fileName.c_str(), ios_base::out);

	SProjectWriter writer;

	writer.w = genxNew(NULL, NULL, NULL);
	genxStatus s;
	
	writer.eFile = genxDeclareElement(writer.w, NULL, u("File"), &s);
	writer.aFilePath = genxDeclareAttribute(writer.w, NULL, u("path"), &s);

	FILE* hFile = _tfopen(fileName.c_str(), "wb");

	genxStartDocFile(writer.w, hFile);
	
	writeDefinition(&writer);

	if (genxEndDocument(writer.w))
	{
		// error...
	}

	fclose(hFile);

	genxDispose(writer.w);
	
	bDirty = false;
}

void Project::SetFileName(LPCTSTR filename)
{
	fileName = filename;
}

tstring Project::GetFileName()
{
	return fileName;
}

bool Project::IsDirty()
{
	return bDirty;
}

void Project::parse()
{
	currentFolder = this;
	parseState = PS_START;
	
	// create a namespace aware parser...
	XMLParser parser(true);
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
			nestingLevel = -1;
			processFile(atts);
			STATE(PS_FILE);
		}
		else
		{
			processUserData(name, atts);
		}
	}
	else if( IN_STATE(PS_FOLDER) )
	{
		if( MATCH( FILENODE ) )
		{
			processFile(atts);
			STATE(PS_FILE);
		}
		else if( MATCH( FOLDERNODE ) )
		{
			nestingLevel++;
			processFolder(atts);
		}
	}
	else if( IN_STATE(PS_USERDATA) )
	{
		processUserData(name, atts);
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
	else if( IN_STATE(PS_FILE) )
	{
		if( nestingLevel == -1 )
			STATE(PS_PROJECT);
		else
			STATE(PS_FOLDER);
	}
	else if( IN_STATE(PS_USERDATA) )
	{
		if(udNestingLevel == 0)
			STATE(udBase);
		else
			udNestingLevel--;
	}
}

void Project::writeDefinition(SProjectWriter* definition)
{
	genxStartElementLiteral(definition->w, NULL, u("Project"));
	genxAddAttributeLiteral(definition->w, NULL, u("name"), u(name.c_str()));

	writeContents(definition);

	genxEndElement(definition->w);
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
	lastParsedFile = currentFolder->AddFile(ATTVAL(_T("path")));
}

void Project::processUserData(LPCTSTR name, XMLAttributes& atts)
{
	if(parseState != PS_USERDATA)
		udBase = parseState;
	STATE(PS_USERDATA);
	udNestingLevel++;
	
	XmlNode* pNode = new XmlNode(name);

	switch(udBase)
	{
		case PS_PROJECT:
			userData.insert(userData.end(), pNode);
		break;

		case PS_FOLDER:
			currentFolder->GetUserData().insert(userData.end(), pNode);
		break;

		case PS_FILE:
		break;
	}
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
	activeProject = NULL;
}

Workspace::Workspace(LPCTSTR projectFile) : ProjectType(ptWorkspace)
{
	fileName = projectFile;
	activeProject = NULL;
	parse();
}

Workspace::~Workspace()
{
	Clear();
}

void Workspace::AddProject(Project* project)
{
	projects.insert(projects.begin(), project);
	bDirty = true;

	if(activeProject == NULL)
		activeProject = project;
}

void Workspace::RemoveProject(Project* project)
{
	if(project->IsDirty())
	{
		tstring msg = _T("Do you want to save changes to the project: ");
		msg += project->GetName();
		msg += _T("?");
		DWORD dwRes = ::MessageBox(g_Context.m_frame->GetWindow()->m_hWnd, msg.c_str(), _T("Programmers Notepad"), MB_YESNOCANCEL | MB_ICONQUESTION);

		if ( dwRes == IDCANCEL )
		{
			return;
		}
		else if( dwRes == IDYES )
		{
			project->Save();
		}
	}

	projects.remove(project);
	delete project;
	bDirty = true;

	if(activeProject == project)
	{
		if(projects.size() > 0)
			activeProject = (*projects.begin());
		else
			activeProject = NULL;
	}
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

LPCTSTR Workspace::GetFileName()
{
	return fileName.c_str();
}

bool Workspace::CanSave()
{
	return (fileName != _T(""));
}

void Workspace::Save()
{
	FILE* hFile = _tfopen(fileName.c_str(), _T("wb"));

	genxWriter w = genxNew(NULL, NULL, NULL);

	genxStartDocFile(w, hFile);
	genxStartElementLiteral(w, NULL, u("Workspace"));
	genxAddAttributeLiteral(w, NULL, u("name"), u(name.c_str()));

	CFileName wspFN(fileName.c_str());
	tstring wspPath = wspFN.GetPath();

	for(PROJECT_LIST::const_iterator i = projects.begin();
		i != projects.end();
		++i)
	{
		Project* p = (*i);
		
		CFileName pfn(p->GetFileName().c_str());
		tstring relpath = pfn.GetRelativePath(wspPath.c_str());

		genxStartElementLiteral(w, NULL, u("Project"));
		genxAddAttributeLiteral(w, NULL, u("path"), u(relpath.c_str()));
		genxEndElement(w);
	}

	genxEndElement(w);
	genxDispose(w);

	fclose(hFile);

	ClearDirty();
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

Projects::Project* Workspace::GetActiveProject()
{
	return activeProject;
}

void Workspace::SetActiveProject(Projects::Project* project)
{
	activeProject = project;
}

void Workspace::startElement(LPCTSTR name, XMLAttributes& atts)
{
	if ( IN_STATE(PS_START) )
	{
		if ( MATCH(WORKSPACENODE) )
		{
			SETVALIDATTSTR(this->name, "name");
			STATE(PS_WORKSPACE);
		}
	}
	else if ( IN_STATE(PS_WORKSPACE) )
	{
		if ( MATCH(PROJECTNODE) )
		{
			LPCTSTR path = ATTVAL("path");
			if(path != NULL && _tcslen(path) > 0)
			{
				CFileName fn(path);
				if(fn.IsRelativePath())
				{
					CFileName fn2(fileName.c_str());
					tstring path = fn2.GetPath();
					fn.Root(path.c_str());
				}

				Project* pProject = new Project(fn.c_str());
				AddProject(pProject);
			}
		}
	}
}

void Workspace::endElement(LPCTSTR name)
{
	if ( IN_STATE(PS_WORKSPACE) && MATCH(WORKSPACENODE) )
		STATE( PS_START );
}

void Workspace::Clear()
{
	for(PL_IT i = projects.begin(); i != projects.end(); ++i)
	{
		delete (*i);
	}

	activeProject = NULL;
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

	bDirty = false;
}

} // namespace Projects