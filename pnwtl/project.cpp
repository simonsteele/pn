/**
 * @file project.cpp
 * @brief Projects
 * @author Simon Steele
 * @note Copyright (c) 2003-2004 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "project.h"

#include "include/genx/genx.h"
#include "projectwriter.h"

#include "include/pcreplus.h"
#include "include/filefinder.h"
#include "include/filematcher.h"
#include "include/encoding.h"
#include "folderadder.h"

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
			str = "error"; \
	}

//////////////////////////////////////////////////////////////////////////////
// XmlNode
//////////////////////////////////////////////////////////////////////////////

XmlNode::XmlNode(LPCTSTR lpszNamespace, LPCTSTR lpszName)
{
	sNamespace = lpszNamespace;
	sName = lpszName;
	pParent = NULL;
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
		sNamespace = buf;
		
		delete [] buf;
	}
	else
		sName = qualifiedName;

	pParent = NULL;
}

XmlNode::~XmlNode()
{
	for(XA_IT i = attributes.begin(); i != attributes.end(); ++i)
	{
		delete (*i);
	}
	attributes.clear();

	for(XN_IT j = children.begin(); j != children.end(); ++j)
	{
		delete (*j);
	}
	children.clear();
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

void XmlNode::AddChild(XmlNode* pChild)
{
	children.insert(children.end(), pChild);
	pChild->pParent = this;
}

XmlNode* XmlNode::GetParent()
{
	return pParent;
}

LIST_NODES& XmlNode::GetChildren()
{
	return children;
}

void XmlNode::Write(ProjectWriter writer)
{
	genxStartElementLiteral(writer->w, 
		sNamespace.length() ? (utf8)sNamespace.c_str() : NULL, 
		(utf8)sName.c_str());
	
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

LPCTSTR XmlNode::GetText()
{
	return sText.c_str();
}

void XmlNode::SetText(LPCTSTR text)
{
	sText = text;
}

bool XmlNode::Matches(LPCTSTR ns, LPCTSTR name)
{
	return (_tcscmp(ns, sNamespace.c_str()) == 0 && _tcscmp(name, sName.c_str()) == 0);
}

//////////////////////////////////////////////////////////////////////////////
// XmlAttribute
//////////////////////////////////////////////////////////////////////////////

XmlAttribute::XmlAttribute(LPCTSTR lpszNamespace, LPCTSTR lpszName, LPCTSTR lpszValue)
{
	sNamespace = lpszNamespace != NULL ? lpszNamespace : _T("");
	sName = lpszName;
	sValue = lpszValue;
}

void XmlAttribute::Write(ProjectWriter writer)
{
	genxAddAttributeLiteral(writer->w, 
		sNamespace.length() ? (utf8)sNamespace.c_str() : NULL, 
		(utf8)sName.c_str(), 
		(utf8)sValue.c_str());
}

//////////////////////////////////////////////////////////////////////////////
// UserData
//////////////////////////////////////////////////////////////////////////////

UserData::~UserData()
{
	for(XN_CIT i = nodes.begin(); i != nodes.end(); ++i)
	{
		delete (*i);
	}
	nodes.clear();
}
		
void UserData::Add(XmlNode* node)
{
	nodes.insert(nodes.end(), node);
}

const LIST_NODES& UserData::GetNodes()
{
	return nodes;
}

const int UserData::GetCount()
{
	return nodes.size();
}

void UserData::Write(ProjectWriter writer)
{
	for(XN_CIT i = nodes.begin(); i != nodes.end(); ++i)
	{
		(*i)->Write(writer);
	}
}

bool UserData::Lookup(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, bool defval)
{
	XmlNode* pNode = lookUp(ns, group, category, value);
	if(pNode != NULL)
	{
		LPCTSTR text = pNode->GetText();
		if(text != NULL && _tcslen(text) > 0)
			return _tcsicmp(text, _T("true")) == 0;
	}
	
	return defval;
}

int UserData::Lookup(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, int defval)
{
	XmlNode* pNode = lookUp(ns, group, category, value);
	if(pNode != NULL)
	{
		LPCTSTR text = pNode->GetText();
		if(text != NULL && _tcslen(text) > 0)
			return _ttoi(text);
	}
	
	return defval;
}

LPCTSTR UserData::Lookup(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value, LPCTSTR defval)
{
	XmlNode* pNode = lookUp(ns, group, category, value);
	if(pNode != NULL)
	{
		LPCTSTR text = pNode->GetText();
		if(text != NULL)
			return text;
	}
	
	return defval;
}

XmlNode* UserData::GetCategoryNode(LPCTSTR ns, LPCTSTR group, LPCTSTR category)
{
	XmlNode* pGroupNode = GetGroupNode(ns, group);

	if(!pGroupNode)
		return NULL;

	for(LIST_NODES::const_iterator j = pGroupNode->GetChildren().begin();
		j != pGroupNode->GetChildren().end();
		++j)
	{
		if( (*j)->Matches(ns, category) )
		{
			return *j;
		}
	}

	return NULL;
}

XmlNode* UserData::GetGroupNode(LPCTSTR ns, LPCTSTR group)
{
	for(LIST_NODES::const_iterator i = nodes.begin(); i != nodes.end(); ++i)
	{
		if( (*i)->Matches(ns, group) )
		{
			return *i;
		}
	}

	return NULL;
}

XmlNode* UserData::lookUp(LPCTSTR ns, LPCTSTR group, LPCTSTR category, LPCTSTR value)
{
	
	XmlNode* pCatNode = GetCategoryNode(ns, group, category);

	if(!pCatNode)
		return NULL;

	for(LIST_NODES::const_iterator k = pCatNode->GetChildren().begin();
		k != pCatNode->GetChildren().end();
		++k)
	{
		if( (*k)->Matches(ns, value) )
			return (*k);
	}

	return NULL;
}

XN_CIT UserData::begin()
{
	return nodes.begin();
}

XN_CIT UserData::end()
{
	return nodes.end();
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
	
	genxStatus writeOk = genxAddAttribute(definition->aFilePath, (const utf8)relPath.c_str());
	if(writeOk == GENX_BAD_UTF8)
	{
		Windows1252_Utf8 conv(relPath.c_str());

		writeOk = genxAddAttribute(definition->aFilePath, conv);

		if(writeOk != GENX_SUCCESS)
			UNEXPECTED(_T("Could not write file name."));
	}

	// Save user data...
	userData.Write(definition);

	genxEndElement(definition->w);
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

/*void Folder::AddChild(Folder* folder, Folder* insertBelow)
{
	FOLDER_LIST::iterator i;
	FOLDER_LIST::iterator iAfter = children.end();
	for(i = children.begin(); i != children.end(); ++i)
	{
		if( (*i) == insertBelow )
		{
			iAfter = i;
			++iAfter;
			break;
		}
	}

	folder->SetParent(this);
	children.insert(iAfter, folder);
	setDirty();
}*/

File* Folder::AddFile(LPCTSTR file)
{
	File* pFile = new File(basePath.c_str(), file, this);
	files.insert(files.end(), pFile);
	setDirty();
	return pFile;
}

void Folder::AddFile(File* file)
{
	//TODO: file->SetBasePath(basePath.c_str());
	files.insert(files.end(), file);
	setDirty();
	file->SetFolder(this);
}

Folder* Folder::AddFolder(LPCTSTR path, LPCTSTR filter, bool recursive)
{
	FolderAdder fa;
	Folder* folder = fa.GetFolder(path, filter, basePath.c_str(), recursive);
	AddChild(folder);
	setDirty();
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
	setDirty();
}

void Folder::DetachFile(File* file)
{
	files.remove(file);
	setDirty();
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
	genxStatus writeOk = genxAddAttributeLiteral(definition->w, NULL, u("name"), u(name.c_str()));
	if(writeOk == GENX_BAD_UTF8)
	{
		Windows1252_Utf8 conv(name.c_str());

		writeOk = genxAddAttributeLiteral(definition->w, NULL, u("name"), conv);
		if(writeOk != GENX_SUCCESS)
			UNEXPECTED(_T("Could not write folder name."));
	}
	
	writeContents(definition);

	genxEndElement(definition->w);
}

void Folder::writeContents(SProjectWriter* definition)
{
	// Save user data...
	userData.Write(definition);

	// Save children folders
	for(FOLDER_LIST::const_iterator i = children.begin();
		i != children.end();
		++i)
	{
		(*i)->WriteDefinition(definition);
	}

	// Save files
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

bool Folder::MoveFile(File* file, Folder* into)
{
	Folder* from = file->GetFolder();
	if(into == NULL)
		return false;

	if(from != NULL)
		from->DetachFile(file);
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

	if(hFile == NULL)
	{
		UNEXPECTED(_T("Could not open the project file for writing"));
		return;
	}

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

void Project::characterData(LPCTSTR data, int len)
{
	if( IN_STATE(PS_USERDATA) )
	{
		udText.append(data, len);
	}
}

void Project::writeDefinition(SProjectWriter* definition)
{
	genxStartElementLiteral(definition->w, NULL, u("Project"));
	genxStatus writeOk = genxAddAttributeLiteral(definition->w, NULL, u("name"), u(name.c_str()));
	if(writeOk == GENX_BAD_UTF8)
	{
		Windows1252_Utf8 conv(name.c_str());
		writeOk = genxAddAttributeLiteral(definition->w, NULL, u("name"), conv);

		if(writeOk != GENX_SUCCESS)
			UNEXPECTED(_T("Could not encode project name for writing."));
	}

	writeContents(definition);

	genxEndElement(definition->w);
}

void Project::processProject(XMLAttributes& atts)
{
	if(atts.getValue(_T("name")) != NULL)
	{
		name = Utf8_Windows1252( ATTVAL(_T("name")) );
	}
	else
	{
		// TODO - not valid...
		name = _T("error");
	}
}

void Project::processFolder(XMLAttributes& atts)
{
	Utf8_Windows1252 nm( ATTVAL(_T("name")) );
	Folder* folder = new Folder(nm, basePath.c_str());
	currentFolder->AddChild(folder);
	currentFolder = folder;
}

void Project::processFile(XMLAttributes& atts)
{
	Utf8_Windows1252 path( ATTVAL(_T("path")) );
	lastParsedFile = currentFolder->AddFile(path);
}

void Project::processMagicFolder(XMLAttributes& atts)
{
	Utf8_Windows1252 path( ATTVAL(_T("path")) );
	Utf8_Windows1252 name( ATTVAL(_T("name")) );
	Utf8_Windows1252 filter( ATTVAL(_T("filter")) );
	Utf8_Windows1252 folderFilter( ATTVAL(_T("excludeFolders")) );

	if(!path.IsValid() || !name.IsValid())
		return;

	CPathName mfPath((const char*)path);
	if(mfPath.IsRelativePath())
	{
		mfPath.Root( basePath.c_str() );
	}

	MagicFolder* mf = new MagicFolder(name, mfPath.c_str());
	
	if(filter.IsValid())
		mf->SetFilter( (const char*)filter );

	if(folderFilter.IsValid())
		mf->SetFolderFilter( (const char*)folderFilter );

	currentFolder->AddChild(mf);
	
	// This will pass over the XML Parsing to the MagicFolder
	// until the MagicFolder element finishes, when it will
	// set the handling back to us for the next element.
	mf->HandleReadCache(theParser, this);
}

void Project::processUserData(LPCTSTR name, XMLAttributes& atts)
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
	projects.insert(projects.end(), project);
	bDirty = true;

	if(activeProject == NULL)
		activeProject = project;
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
    
	bDirty = true;
}

void Workspace::RemoveProject(Project* project)
{
	DetachProject(project);
	delete project;	
}

void Workspace::DetachProject(Project* project)
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

//////////////////////////////////////////////////////////////////////////////
// ProjectViewState
//////////////////////////////////////////////////////////////////////////////

#if (_ATL_VER >= 0x0700)
	class ProjectViewState::ExpandCache : public std::hash_map<tstring, bool>{};
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
		return OPTIONS->Get("Projects", "ExpandDefault", false);
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

void ProjectViewState::getFolderPath(Folder* folder, tstring path)
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

} // namespace Projects

#if (_MSC_VER >= 1300)
	#pragma warning( pop ) // 4996 - deprecated hash_map.
#endif