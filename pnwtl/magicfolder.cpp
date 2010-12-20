/**
 * @file magicfolder.cpp
 * @brief Magic Folders in Projects
 * @author Simon Steele
 * @note Copyright (c) 2004-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "project.h"

#include "include/filefinder.h"
#include "include/filematcher.h"
#include "folderadder.h"

#include <algorithm>

#define u(x) (constUtf8)x

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

#define MATCH(ename) \
	(_tcscmp(name, _T(ename)) == 0)

#define IN_STATE(state) \
	(parseState == state)

#define STATE(state) \
	parseState = state

#define ATTVAL(attname) \
	atts.getValue(attname)

#define PS_FOLDER	0
#define PS_FILE		1
#define PS_USERDATA	2

//////////////////////////////////////////////////////////////////////////////
// MagicFolder
//////////////////////////////////////////////////////////////////////////////

MagicFolder::MagicFolder(LPCTSTR name_, /*LPCTSTR path_, */LPCTSTR base_)
{
	type = ptMagicFolder;

	parent = NULL;
	name = name_;
	
	// Using CPathName ensures we get trailing slashes...
	CPathName basepath(base_);
	basePath = basepath.c_str();
	
	read = false;
	
	cache = NULL;

	filter = _T("*");
	excludedFileFilter = FolderAdder::getDefaultExcludedFileFilter();
	folderFilter = FolderAdder::getDefaultExcludedFolderFilter();
}

MagicFolder::~MagicFolder()
{
	if(cache != NULL)
		delete cache;
}

void MagicFolder::HandleReadCache(XMLParser* parser, XMLParseState* parent)
{
	cache = new MagicFolderCache(name.c_str(), parser, parent);
}

const FOLDER_LIST& MagicFolder::GetFolders()
{
	if (!read)
	{
		m_canNotify = false;
		Refresh();
		m_canNotify = true;
	}

	return children;
}

const FILE_LIST& MagicFolder::GetFiles()
{
	if (!read)
	{
		m_canNotify = false;
		Refresh();
		m_canNotify = true;
	}

	return files;
}

void MagicFolder::Refresh()
{
	Clear();

	MagicFolderAdder mfa;
	
	//TODO: remove parameter duplication here...
	mfa.BuildFolder(this, basePath.c_str(), filter.c_str(), basePath.c_str(), excludedFileFilter.c_str(), folderFilter.c_str(), true);

	read = true;
}

//
//void MagicFolder::WriteDefinition(SProjectWriter* definition)
//{
//	genxStartElementLiteral(definition->w, NULL, u("MagicFolder"));
//	genxAddAttributeLiteral(definition->w, NULL, u("name"), u(name.c_str()));
//	
//	CFileName cfn( basePath.c_str() );
//	tstring relPath = cfn.GetRelativePath( GetParent()->GetBasePath() );
//	genxAddAttributeLiteral(definition->w, NULL, u("path"), u(relPath.c_str()));
//
//	genxAddAttributeLiteral(definition->w, NULL, u("filter"), u(filter.c_str()));
//	genxAddAttributeLiteral(definition->w, NULL, u("excludeFolders"), u(folderFilter.c_str()));
//
//	writeContents(definition);
//
//	genxEndElement(definition->w);
//}

void MagicFolder::SetGotContents(bool bGotContents)
{
	read = bGotContents;
}

tstring MagicFolder::getMagicFolderPath(MagicFolder* last)
{
	if(last->GetParent()->GetType() != ptMagicFolder)
	{
		// at the bottom of the magic folder...
		return tstring(_T("\\"));
	}
	else
	{
		MagicFolder* pParent = reinterpret_cast<MagicFolder*>( last->GetParent() );
		tstring s = getMagicFolderPath(pParent);
		s += last->GetName();
		s += _T('\\');
		return s;
	}
}

tstring MagicFolder::GetFolderCachePath()
{
	tstring thePath = getMagicFolderPath(this);

	std::transform (thePath.begin(), thePath.end(),    // source
               thePath.begin(),					// destination
               tolower);

	return thePath;
}

LPCTSTR MagicFolder::GetFullPath() const
{
	return basePath.c_str();
}

void MagicFolder::SetFullPath(LPCTSTR newPath)
{
	basePath = newPath;
	read = false;
}

LPCTSTR MagicFolder::GetFilter() const
{
	return filter.c_str();
}

void MagicFolder::SetFilter(LPCTSTR szFilter)
{
	filter = szFilter;
}

LPCTSTR MagicFolder::GetExcludedFileFilter() const {
	return excludedFileFilter.c_str();
}

void MagicFolder::SetExcludedFileFilter(LPCTSTR value) {
	excludedFileFilter = value;
}

LPCTSTR MagicFolder::GetFolderFilter() const
{
	return folderFilter.c_str();
}

void MagicFolder::SetFolderFilter(LPCTSTR filter)
{
	folderFilter = filter;
}

bool MagicFolder::RenameFolder(LPCTSTR newName)
{
	CPathName fp(basePath);
	fp.ChangeLastElement(newName);
	
	if(::MoveFile(basePath.c_str(), fp.c_str()) != 0)
	{
		basePath = fp.c_str();
		return true;
	}

	return false;
}

//////////////////////////////////////////////////////////////////////////////
// MagicFolderCache::FolderMap
//////////////////////////////////////////////////////////////////////////////

class MagicFolderCache::FolderMap : public stdext::hash_map<tstring, Folder*>
{
	public:
		typedef MagicFolderCache::FolderMap thisClass;
		
		~FolderMap()
		{
			for(thisClass::const_iterator i = begin(); i != end(); ++i)
			{
				delete (*i).second;
			}

			clear();
		}
};

//////////////////////////////////////////////////////////////////////////////
// MagicFolderCache Helper Functions
//////////////////////////////////////////////////////////////////////////////

/**
 * This function helps to build a stack of folder paths which is used as
 * we go up and down the folder tree to store the paths of the folders.
 */
SStringStack* newStringStackItem(SStringStack* last, LPCTSTR newPathElement)
{
	SStringStack* item = new SStringStack;
	
	if(last)
	{
		item->previous = last;
		item->val = last->val;
		
		if (newPathElement)
		{
			item->val += newPathElement;
		}
		else
		{
			item->val += _T("ERROR");
		}
		
		item->val += _T('\\');

		std::transform (item->val.begin(), item->val.end(),    // source
               item->val.begin(),					// destination
               tolower);
	}
	else
	{
		item->val = _T('\\');
		item->previous = NULL;
	}

	return item;
}

//////////////////////////////////////////////////////////////////////////////
// MagicFolderCache
//////////////////////////////////////////////////////////////////////////////

MagicFolderCache::MagicFolderCache(LPCTSTR name, XMLParser* parser, XMLParseState* parent)
{
	_depth = 0;
	_parent = parent;
	_parser = parser;
	_pathStack = newStringStackItem(NULL, NULL);
	_map = new FolderMap;
	
	// Set up the root folder...
	_current = new Folder(name, _T(""));
	_map->insert(MagicFolderCache::FolderMap::value_type(_pathStack->val, _current));

	STATE( PS_FOLDER );
	_parser->SetParseState(this);
}

MagicFolderCache::~MagicFolderCache()
{
	delete _map;
}

Folder* MagicFolderCache::GetCachedFolder(MagicFolder* actual)
{
	tstring path = actual->GetFolderCachePath();
	MagicFolderCache::FolderMap::const_iterator i = _map->find(path);
	if(i != _map->end())
	{
		return (*i).second;
	}
	else
		return NULL;
}

void MagicFolderCache::startElement(XML_CSTR name, const XMLAttributes& atts)
{
	if( IN_STATE( PS_FOLDER ) )
	{
		if( MATCH("MagicFolder") )
		{
			_depth++;
			
			// Push this folder path onto the path stack...
			_pathStack = newStringStackItem(_pathStack, atts.getValue(_T("name")));
			
			// Store a folder object which will hold configuration etc.
			_current = new Folder(_pathStack->val.c_str(), _T(""));
			_map->insert(MagicFolderCache::FolderMap::value_type(_pathStack->val, _current));
		}
		else if( MATCH("File") )
		{
			STATE(PS_FILE);
			processFile(atts);
		}
		else
		{
			processUserData(name, atts);
		}
	}
	else if( IN_STATE( PS_FILE ) || IN_STATE( PS_USERDATA ) )
	{
		processUserData(name, atts);
	}
}

void MagicFolderCache::endElement(XML_CSTR name)
{
	if( IN_STATE(PS_FOLDER) )
	{
		if( MATCH("MagicFolder") )
		{
			if(_depth == 0)
			{
				_parser->SetParseState(_parent);
				
				// Get rid of the last path stack element.
				PNASSERT(_pathStack->previous == NULL);
				delete _pathStack;
				_pathStack = NULL;
			}
			else
			{
				SStringStack* stackOld = _pathStack;
				
				// Update the path stack...
				_pathStack = _pathStack->previous;
				delete stackOld;
				_depth--;
			}
		}
	}
	else if( IN_STATE(PS_FILE) )
	{
		STATE(PS_FOLDER);
	}
	else if( IN_STATE(PS_USERDATA) )
	{
		if(lastNode)
			lastNode = lastNode->GetParent();
		udNestingLevel--;
		if(udNestingLevel == 0)
			STATE(udBase);
	}
}

void MagicFolderCache::characterData(XML_CSTR data, int len)
{
	
}

void MagicFolderCache::processFile(const XMLAttributes& atts)
{
	_currentFile = _current->AddFile(ATTVAL(_T("path")));
}

void MagicFolderCache::processUserData(XML_CSTR name, const XMLAttributes& atts)
{
	if(parseState != PS_USERDATA)
		udBase = parseState;
	STATE(PS_USERDATA);
	udNestingLevel++;
	
	XmlNode* pNode = new XmlNode(name);
	pNode->AddAttributes(atts);

	if(lastNode)
	{
		lastNode->AddChild(pNode);
	}
	else
	{
		switch(udBase)
		{
			case PS_FOLDER:
				_current->GetUserData().Add(pNode);
			break;

			case PS_FILE:
				_currentFile->GetUserData().Add(pNode);
			break;
		}
	}

	lastNode = pNode;
}

} // namespace Projects