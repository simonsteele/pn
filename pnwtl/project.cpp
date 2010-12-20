/**
 * @file project.cpp
 * @brief Projects
 * @author Simon Steele
 * @note Copyright (c) 2003-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "project.h"

#include "projectwriter.h"

#include "include/filefinder.h"
#include "include/filematcher.h"
#include "include/encoding.h"
#include "folderadder.h"
#include "projectregistry.h"

#if defined (_DEBUG)
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#if (_MSC_VER >= 1300)
	#pragma warning( push )
	#pragma warning(disable: 4996) // see MSDN on hash_map
	#include <hash_map>
#else
	#include <map>
#endif

namespace Projects
{

//////////////////////////////////////////////////////////////////////////////
// Parsing Gubbins...
//////////////////////////////////////////////////////////////////////////////

#define FILENODE		_T("File")
#define FOLDERNODE		_T("Folder")
#define PROJECTNODE		_T("Project")
#define WORKSPACENODE	_T("Workspace")
#define MAGICFOLDERNODE	_T("MagicFolder")

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
			str = _T("error"); \
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

UserData& ProjectType::GetUserData()
{
	return userData;
}

//////////////////////////////////////////////////////////////////////////////
// File
//////////////////////////////////////////////////////////////////////////////

File::File(LPCTSTR basePath, LPCTSTR path, Projects::Folder* parent) : ProjectType(ptFile)
{
	CFileName fn(path);

	if(fn.IsRelativePath() && _tcslen(basePath) > 0)
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

LPCTSTR File::GetRelativePath()
{
	return relPath.c_str();
}

void File::SetFolder(Projects::Folder* folder)
{
	parentFolder = folder;
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

			SetDirty();

			return true;
		}
		else
		{
			g_Context.m_frame->SetStatusText(_T("Rename file failed."));
		}
	}

	return false;
}

//void File::WriteDefinition(SProjectWriter* definition)
//{
//	genxStartElement(definition->eFile);
//	
//	genxStatus writeOk = genxAddAttribute(definition->aFilePath, (const utf8)relPath.c_str());
//	if(writeOk == GENX_BAD_UTF8)
//	{
//		Tcs_Utf8 conv(relPath.c_str());
//
//		writeOk = genxAddAttribute(definition->aFilePath, conv);
//
//		if(writeOk != GENX_SUCCESS)
//			UNEXPECTED(_T("Could not write file name."));
//	}
//
//	// Save user data...
//	userData.Write(definition);
//
//	genxEndElement(definition->w);
//}

void File::SetDirty()
{
	parentFolder->SetDirty();
}

//////////////////////////////////////////////////////////////////////////////
// Folder
//////////////////////////////////////////////////////////////////////////////

Folder::Folder() : ProjectType(ptFolder), m_canNotify(true)
{
	parent = NULL;
}

Folder::Folder(LPCTSTR name_, LPCTSTR basepath) : ProjectType(ptFolder), m_canNotify(true)
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
	if (name != name_)
	{
		name = name_;
		SetDirty();
	}
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
	SetDirty();
	notify(pcAdd, folder);
}

File* Folder::AddFile(LPCTSTR file)
{
	File* pFile = new File(basePath.c_str(), file, this);
	files.insert(files.end(), pFile);
	
	SetDirty();
	notify(pcAdd, pFile);
	
	return pFile;
}

void Folder::AddFile(File* file)
{
	//TODO: file->SetBasePath(basePath.c_str());
	files.insert(files.end(), file);
	
	SetDirty();
	notify(pcAdd, file);

	file->SetFolder(this);
}

Folder* Folder::AddFolder(LPCTSTR path, LPCTSTR filter, bool recursive)
{
	FolderAdder fa;
	Folder* folder = fa.GetFolder(path, filter, basePath.c_str(), recursive);
	AddChild(folder);
	SetDirty();
	
	return folder;
}

void Folder::RemoveChild(Folder* folder)
{
	DetachChild(folder);
	delete folder;
}

void Folder::RemoveFile(File* file)
{
	DetachFile(file);
	delete file;
}

void Folder::DetachChild(Folder* folder)
{
	children.remove(folder);
	
	SetDirty();
	notify(pcRemove, folder);
}

void Folder::DetachFile(File* file)
{
	files.remove(file);
	
	SetDirty();
	notify(pcRemove, file);
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

	notify(pcClear, this);
}

void Folder::SetParent(Folder* folder)
{
	parent = folder;
}

Folder* Folder::GetParent()
{
	return parent;
}

Project* Folder::GetProject()
{
	if(parent == NULL && type == ptProject)
		return static_cast<Projects::Project*>(this);

	Folder* pParent = parent;
	while(pParent != NULL && pParent->GetType() != ptProject)
	{
		pParent = pParent->parent;
	}

	if(pParent)
	{
		return static_cast<Projects::Project*>(pParent);
	}
	else
		return NULL;
}

//void Folder::WriteDefinition(SProjectWriter* definition)
//{
//	genxStartElementLiteral(definition->w, NULL, u("Folder"));
//	genxStatus writeOk = genxAddAttributeLiteral(definition->w, NULL, u("name"), u(name.c_str()));
//	if(writeOk == GENX_BAD_UTF8)
//	{
//		Tcs_Utf8 conv(name.c_str());
//
//		writeOk = genxAddAttributeLiteral(definition->w, NULL, u("name"), conv);
//		if(writeOk != GENX_SUCCESS)
//			UNEXPECTED(_T("Could not write folder name."));
//	}
//	
//	writeContents(definition);
//
//	genxEndElement(definition->w);
//}
//
//void Folder::writeContents(SProjectWriter* definition)
//{
//	// Save user data...
//	userData.Write(definition);
//
//	// Save children folders
//	for(FOLDER_LIST::const_iterator i = children.begin();
//		i != children.end();
//		++i)
//	{
//		(*i)->WriteDefinition(definition);
//	}
//
//	// Save files
//	for(FILE_LIST::const_iterator j = files.begin(); 
//		j != files.end(); 
//		++j)
//	{
//		(*j)->WriteDefinition(definition);
//	}
//}

void Folder::SetDirty()
{
	if(parent)
		parent->SetDirty();
}

bool Folder::MoveFile(File* file, Folder* into)
{
	if(into == NULL)
		return false;

	Folder* from = file->GetFolder();
	
	if(from != NULL)
	{
		if(from->GetType() == ptMagicFolder)
		{
			// We can't *move* from a magic folder to a normal folder,
			// so we copy instead:

			//TODO: We *could* move from a magic folder to another magic
			// folder, thus doing a real file move. Can't be bothered right now.

			File* newfile = new File(into->GetBasePath(), file->GetFileName(), into);
			// Copy any user data
			newfile->GetUserData() = file->GetUserData();
			
			into->AddFile(newfile);
			return true;
		}
		else
		{
			// Remove the current instance from this folder...
			from->DetachFile(file);
		}
	}

	into->AddFile(file);

	return true;
}

bool Folder::MoveChild(Folder* folder, Folder* into)
{
	Folder* from = folder->GetParent();
	if(into == NULL)
		return false;

	if(from != NULL)
		from->DetachChild(folder);
	into->AddChild(folder);
	
	return true;
}

bool Folder::hasUserData()
{
	if(userData.GetCount() != 0)
		return true;

	for(FILE_IT i = files.begin(); i != files.end(); ++i)
	{
		if( (*i)->GetUserData().GetCount() != 0 )
			return true;
	}

	for(FL_IT j = children.begin(); j != children.end(); ++j)
	{
		if( (*j)->hasUserData() )
			return true;
	}

	return false;
}

void Folder::notify(PROJECT_CHANGE_TYPE changeType)
{
	notify(changeType, this, this);
}

void Folder::notify(PROJECT_CHANGE_TYPE changeType, ProjectType* changeItem)
{
	notify(changeType, this, changeItem);
}

void Folder::notify(PROJECT_CHANGE_TYPE changeType, Folder* changeContainer, ProjectType* changeItem)
{
	if (m_canNotify && parent != NULL)
	{
		parent->notify(changeType, changeContainer, changeItem);
	}
}

void Folder::GetAllFiles(std::vector<tstring>& files)
{
	for(FOLDER_LIST::const_iterator i = GetFolders().begin();
		i != GetFolders().end();
		++i)
	{
		(*i)->GetAllFiles(files);
	}

	for(FILE_LIST::const_iterator j = GetFiles().begin();
		j != GetFiles().end();
		++j)
	{
		bool found = false;
		
		for(std::vector<tstring>::const_iterator k = files.begin();
			k != files.end();
			++k)
		{
			CFileName cfn1(*k);
			const tstring& fn1 = cfn1.ToLower();
			CFileName cfn2((*j)->GetFileName());

			if (fn1 == cfn2.ToLower())
			{
				found = true;
			}
		}
		
		if (found == false)
		{
			files.push_back((*j)->GetFileName());
		}
	}
}

//////////////////////////////////////////////////////////////////////////////
// Project
//////////////////////////////////////////////////////////////////////////////

bool Project::CreateEmptyProject(LPCTSTR projectname, LPCTSTR filename, LPCTSTR templateGuid)
{
	Project Fake;
	Fake.basePath = CFileName(filename).GetPath();
	Fake.SetName(projectname);
	Fake.fileName = filename;

	if(templateGuid)
	{
		Fake.typeID = templateGuid;
	}

	Fake.Save();

	return FileExists(filename);
}

Project::Project()
{
	parentWorkspace = NULL;

	type = ptProject;

	bDirty = true;
	bExists = true;
	
	m_template = NULL;
	m_viewState = NULL;
}

Project::Project(LPCTSTR projectFile) : Folder()
{
	parentWorkspace = NULL;

	type = ptProject;

	fileName = projectFile;
	bExists = FileExists(projectFile);
		
	basePath = CFileName(projectFile).GetPath();

	m_template = NULL;
	m_viewState = NULL;

	if(bExists)
	{
		parse();
	}

	bDirty = false;
}

Project::~Project()
{
	if(m_viewState != NULL)
	{
		delete m_viewState;
	}
}

bool Project::Exists()
{
	return bExists;
}

void Project::Save()
{
	ProjectWriter writer;
	
	writer.Start(fileName.c_str());
	
	if (writer.IsValid())
	{
		writer.WriteProject(this);
		writer.Close();
	}
	else
	{
		UNEXPECTED(_T("Could not open the project file for writing"));
		return;
	}
	
	bDirty = false;
	Folder::notify(pcClean);

	// Do this after the notify, it gives the view a chance to store state.
	if(m_viewState != NULL)
		SaveViewState();
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

void Project::notify(PROJECT_CHANGE_TYPE changeType, Folder* changeContainer, ProjectType* changeItem)
{
	if(parentWorkspace != NULL)
	{
		parentWorkspace->Notify(changeType, changeContainer, changeItem);
	}
}

void Project::setWorkspace(Workspace* workspace)
{
	parentWorkspace = workspace;
}

void Project::parse()
{
	currentFolder = this;
	parseState = PS_START;
	udNestingLevel = 0;
	lastNode = NULL;
	
	// create a namespace aware parser...
	XMLParser parser(true);
	theParser = &parser;
	parser.SetParseState(this);
	try
	{
		parser.LoadFile(fileName.c_str());
	}
	catch (XMLParserException& ex)
	{
		::OutputDebugString(ex.GetMessage());
	}
	theParser = NULL;
}

void Project::startElement(XML_CSTR name, const XMLAttributes& atts)
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
		else if( MATCH( MAGICFOLDERNODE ) )
		{
			processMagicFolder(atts);
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
		else if( MATCH( MAGICFOLDERNODE ) )
		{
			processMagicFolder(atts);
		}
		else
		{
			processUserData(name, atts);
		}
	}
	else if( IN_STATE(PS_FILE) )
	{
		processUserData(name, atts);
	}
	else if( IN_STATE(PS_USERDATA) )
	{
		processUserData(name, atts);
	}
}

void Project::endElement(XML_CSTR name)
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
		if(lastNode)
		{
			lastNode->SetText(udText.c_str());
			lastNode = lastNode->GetParent();
			if(lastNode)
				udText = lastNode->GetText();
		}
		udNestingLevel--;
		if(udNestingLevel == 0)
			STATE(udBase);
	}
}

void Project::characterData(XML_CSTR data, int len)
{
	if( IN_STATE(PS_USERDATA) )
	{
		udText.append(data, len);
	}
}

void Project::processProject(const XMLAttributes& atts)
{
	if(atts.getValue(_T("name")) != NULL)
	{
		name = Xml_Tcs( ATTVAL(_T("name")) );
	}
	else
	{
		// TODO - not valid...
		name = _T("error");
	}

	if(atts.getValue(_T("typeId")) != NULL)
	{
		typeID = Xml_Tcs( ATTVAL(_T("typeId")) );
		if(typeID.length() > 0)
		{
			ProjectTemplate* pTemplate = Registry::GetInstance()->FromID(typeID.c_str());
			if(pTemplate != NULL)
			{
				m_template = pTemplate;
			}
			else
			{
				// Log could not find template
			}
		}
	}
}

void Project::processFolder(const XMLAttributes& atts)
{
	Xml_Tcs nm( ATTVAL(_T("name")) );
	Folder* folder = new Folder(nm, basePath.c_str());
	currentFolder->AddChild(folder);
	currentFolder = folder;
}

void Project::processFile(const XMLAttributes& atts)
{
	Xml_Tcs path( ATTVAL(_T("path")) );
	lastParsedFile = currentFolder->AddFile(path);
}

void Project::processMagicFolder(const XMLAttributes& atts)
{
	Xml_Tcs path( ATTVAL(_T("path")) );
	Xml_Tcs name( ATTVAL(_T("name")) );
	Xml_Tcs filter( ATTVAL(_T("filter")) );
	Xml_Tcs excludedFileFilter( ATTVAL(_T("excludeFiles")) );
	Xml_Tcs folderFilter( ATTVAL(_T("excludeFolders")) );

	if(!path.IsValid() || !name.IsValid())
		return;

	CPathName mfPath(path);
	if(mfPath.IsRelativePath())
	{
		mfPath.Root( basePath.c_str() );
	}

	MagicFolder* mf = new MagicFolder(name, mfPath.c_str());
	
	if(filter.IsValid())
		mf->SetFilter(filter);

	if(excludedFileFilter.IsValid())
		mf->SetExcludedFileFilter(excludedFileFilter);

	if(folderFilter.IsValid())
		mf->SetFolderFilter(folderFilter);

	currentFolder->AddChild(mf);
	
	// This will pass over the XML Parsing to the MagicFolder
	// until the MagicFolder element finishes, when it will
	// set the handling back to us for the next element.
	mf->HandleReadCache(theParser, this);
}

void Project::processUserData(XML_CSTR name, const XMLAttributes& atts)
{
	if(parseState != PS_USERDATA)
		udBase = parseState;
	STATE(PS_USERDATA);
	udNestingLevel++;
	
	XmlNode* pNode = new XmlNode(name);
	pNode->AddAttributes(atts);

	if(lastNode)
	{
		lastNode->SetText(udText.c_str());
		lastNode->AddChild(pNode);
	}
	else
	{
		switch(udBase)
		{
			case PS_PROJECT:
				userData.Add(pNode);
			break;

			case PS_FOLDER:
				currentFolder->GetUserData().Add(pNode);
			break;

			case PS_FILE:
				lastParsedFile->GetUserData().Add(pNode);
			break;
		}
	}

	lastNode = pNode;
	udText = _T("");
}

void Project::SetDirty()
{
	bool bOld = bDirty;
	bDirty = true;
	
	if(!bOld)
		Folder::notify(pcDirty);
}

ProjectTemplate* Project::GetTemplate() const
{
	return m_template;
}

ProjectViewState* Project::GetViewState()
{
	if(m_viewState == NULL)
	{
		ProjectViewState* vs = new ProjectViewState();
		CFileName fn(GetFileName());
		fn.ChangeExtensionTo(_T(".pnps"));
	
		vs->Load(fn.c_str());
		
		m_viewState = vs;
	}

	return m_viewState;
}

void Project::SaveViewState()
{
	if(m_viewState != NULL)
	{
		CFileName fn(GetFileName());
		fn.ChangeExtensionTo(_T(".pnps"));

		m_viewState->Save(fn.c_str());
	}
}

//////////////////////////////////////////////////////////////////////////////
// Workspace
//////////////////////////////////////////////////////////////////////////////

Workspace::Workspace() : ProjectType(ptWorkspace)
{
	fileName = _T("");
	bDirty = false;
	activeProject = NULL;
	watcher = NULL;
}

Workspace::Workspace(LPCTSTR projectFile) : ProjectType(ptWorkspace)
{
	fileName = projectFile;
	activeProject = NULL;
	watcher = NULL;
	parse();
}

Workspace::~Workspace()
{
	Clear();
}

void Workspace::AddProject(Project* project)
{
	projects.insert(projects.end(), project);
	project->setWorkspace(this);
	bDirty = true;

	if(activeProject == NULL)
		activeProject = project;

	Notify(pcAdd, NULL, project);
}

void Workspace::InsertProject(Project* project, Project* insertAfter)
{
	PROJECT_LIST::iterator iAfter = projects.end();
	PROJECT_LIST::iterator i;
	for(i = projects.begin(); i != projects.end(); ++i)
	{
		if( (*i) == insertAfter )
		{
			iAfter = i;
			++iAfter;
			break;
		}
	}

	projects.insert(iAfter, project);
	project->setWorkspace(this);
    
	bDirty = true;

	Notify(pcAdd, NULL, project);
}

void Workspace::RemoveProject(Project* project)
{
	DetachProject(project);
	project->setWorkspace(NULL);
	delete project;	
}

void Workspace::DetachProject(Project* project)
{
	projects.remove(project);
	project->setWorkspace(NULL);

	if(activeProject == project)
	{
		if(projects.size() > 0)
			activeProject = (*projects.begin());
		else
			activeProject = NULL;
	}
	
	bDirty = true;
}

void Workspace::MoveProject(Project* project, Project* moveAfter)
{
	projects.remove(project);
	InsertProject(project, moveAfter);
}

const PROJECT_LIST& Workspace::GetProjects()
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
	if(fileName.size() == 0)
		return;

	FILE* hFile = _tfopen(fileName.c_str(), _T("wb"));

	if(hFile == NULL)
	{
		UNEXPECTED(_T("Could not open the project file for writing"));
		return;
	}

	genxWriter w = genxNew(NULL, NULL, NULL);

	genxStartDocFile(w, hFile);
	genxStartElementLiteral(w, NULL, u("Workspace"));
	
	Tcs_Utf8 nameconv(name.c_str());
	genxAddAttributeLiteral(w, NULL, u("name"), u(nameconv));

	CFileName wspFN(fileName.c_str());
	tstring wspPath = wspFN.GetPath();

	for(PROJECT_LIST::const_iterator i = projects.begin();
		i != projects.end();
		++i)
	{
		Project* p = (*i);
		
		CFileName pfn(p->GetFileName().c_str());
		Tcs_Utf8 relPath(pfn.GetRelativePath(wspPath.c_str()).c_str());

		genxStartElementLiteral(w, NULL, u("Project"));
		genxAddAttributeLiteral(w, NULL, u("path"), u(relPath));
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

void Workspace::SetDirty()
{
	bDirty = true;
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

	Notify(pcActive, NULL, project);
}

void Workspace::Notify(PROJECT_CHANGE_TYPE changeType, Folder* changeContainer, ProjectType* changeItem)
{
	if(watcher != NULL)
	{
		watcher->OnProjectItemChange(changeType, changeContainer, changeItem);
	}
}

void Workspace::SetWatcher(IProjectWatcher* newWatcher)
{
	watcher = newWatcher;
}

void Workspace::startElement(LPCTSTR name, const XMLAttributes& atts)
{
	if ( IN_STATE(PS_START) )
	{
		if ( MATCH(WORKSPACENODE) )
		{
			SETVALIDATTSTR(this->name, _T("name"));
			STATE(PS_WORKSPACE);
		}
	}
	else if ( IN_STATE(PS_WORKSPACE) )
	{
		if ( MATCH(PROJECTNODE) )
		{
			LPCTSTR path = ATTVAL(_T("path"));
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
		CString err;
		err.Format(_T("Error Parsing Project XML: %s\n (file: %s, line: %d, column %d)"), 
			XML_ErrorString(ex.GetErrorCode()), ex.GetFileName(), ex.GetLine(), ex.GetColumn());

		g_Context.m_frame->SetStatusText(err);
	}

	bDirty = false;
}

//////////////////////////////////////////////////////////////////////////////
// ProjectViewState
//////////////////////////////////////////////////////////////////////////////

#if (_ATL_VER >= 0x0700)
	class ProjectViewState::ExpandCache : public stdext::hash_map<tstring, bool>{};
#else
	class ProjectViewState::ExpandCache : public std::map<tstring, bool>{};
#endif

ProjectViewState::ProjectViewState()
{
	cache = new ExpandCache();
}

ProjectViewState::~ProjectViewState()
{
	delete cache;
}

bool ProjectViewState::ShouldExpand(Folder* folder)
{
	tstring path;
	getFolderPath(folder, path);

	ExpandCache::const_iterator i = cache->find(path);
	if(i != cache->end())
	{
		return (*i).second;
	}
	else
		return OPTIONS->Get(_T("Projects"), _T("ExpandDefault"), false);
}

void ProjectViewState::SetExpand(Folder* folder, bool expand)
{
	tstring path;
	getFolderPath(folder, path);

	cache->insert( ExpandCache::value_type(path, expand) );
}

void ProjectViewState::SetExpand(LPCTSTR folderPath, bool expand)
{
	tstring path(folderPath);
	cache->insert( ExpandCache::value_type(path, expand) );
}

tstring ProjectViewState::GetFolderPath(Folder* folder)
{
	tstring path;
	getFolderPath(folder, path);
	return path;
}

void ProjectViewState::Load(LPCTSTR filename)
{
	if(!FileExists(filename))
		return;

	XMLParser parser;
	parser.SetParseState(this);
	parseState = 0;
	
	try
	{
		parser.LoadFile(filename);
	}
	catch(XMLParserException& ex)
	{
		CString err;
		err.Format(_T("Error Parsing ProjectViewState XML: %s\n (file: %s, line: %d, column %d)"), 
			XML_ErrorString(ex.GetErrorCode()), ex.GetFileName(), ex.GetLine(), ex.GetColumn());

		g_Context.m_frame->SetStatusText(err);
	}
}

void ProjectViewState::Save(LPCTSTR filename)
{
	FILE* hFile = _tfopen(filename, _T("wb"));

	if(hFile == NULL)
	{
		UNEXPECTED(_T("Could not open the project data file for writing"));
		return;
	}

	genxWriter w = genxNew(NULL, NULL, NULL);

	genxStartDocFile(w, hFile);
	genxStartElementLiteral(w, NULL, u("pd"));
	genxStartElementLiteral(w, NULL, u("ViewState"));

	for(ExpandCache::iterator i = cache->begin(); i != cache->end(); ++i)
	{
		genxStartElementLiteral(w, NULL, u("e"));
		Tcs_Utf8 path((*i).first.c_str());
		genxAddAttributeLiteral(w, NULL, u("p"), u(path));
		genxAddAttributeLiteral(w, NULL, u("x"), (*i).second ? u("true") : u("false"));
		genxEndElement(w);
	}

	genxEndElement(w);
	genxEndElement(w);
	genxDispose(w);

	fclose(hFile);
}

void ProjectViewState::Clear()
{
	cache->clear();
}

void ProjectViewState::getFolderPath(Folder* folder, tstring& path)
{
	Folder* parent = folder->GetParent();
	if(parent != NULL)
	{
		getFolderPath(parent, path);
		path += _T('\\');
		path += folder->GetName();
	}
	else
	{
		path = folder->GetName();
	}
}

#define PNPS_START 0
#define PNPS_VIEWSTATE 1

void ProjectViewState::startElement(LPCTSTR name, const XMLAttributes& atts)
{
	if ( IN_STATE(PNPS_START) )
	{
		if ( MATCH(_T("ViewState")) )
		{
			STATE(PNPS_VIEWSTATE);
		}
	}
	else if( IN_STATE(PNPS_VIEWSTATE) )
	{
		if( MATCH(_T("e")) )
		{
			LPCTSTR e = atts.getValue(_T("p"));
			if(e != NULL)
			{
				Xml_Tcs path(e);
				LPCTSTR x = atts.getValue(_T("x"));
				if(x != NULL && *x != NULL)
				{
					if(x[0] == _T('t'))
						SetExpand(path, true);
					else
						SetExpand(path, false);
				}
			}
		}
	}
}

void ProjectViewState::endElement(LPCTSTR name)
{
	if( IN_STATE(PNPS_VIEWSTATE) )
	{
		if( MATCH(_T("ViewState")) )
			STATE(PNPS_START);
	}
}

} // namespace Projects

#if (_MSC_VER >= 1300)
	#pragma warning( pop ) // 4996 - deprecated hash_map.
#endif