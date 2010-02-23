/**
 * @file clipmanager.cpp
 * @brief Text Clips Manager.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "clipmanager.h"
#include "clipparser.h"
#include "clipwriter.h"
#include "../include/filefinder.h"
#include "../include/encoding.h"

namespace TextClips {

//////////////////////////////////////////////////////////////////////////////
// TextClipsManager
//////////////////////////////////////////////////////////////////////////////

TextClipsManager::TextClipsManager() : m_loadingClips(NULL)
{
	//findClips();
}

/**
 * Copy constructor - deep copy
 */
TextClipsManager::TextClipsManager(const TextClipsManager& other)
{
	copy(other);
}

TextClipsManager::~TextClipsManager()
{
	clear();
}

const LIST_CLIPSETS& TextClipsManager::GetClipSets()
{
	return m_clipSets;
}

const LIST_CLIPSETS& TextClipsManager::GetClips(LPCSTR schemeName)
{
	MAP_CLIPSETS::iterator i = m_schemeClipSets.find(std::string(schemeName));
	
	if (i != m_schemeClipSets.end())
	{
		return (*i).second;
	}
	
	// Not loaded yet...
	FileFinder<TextClipsManager> finder(this, &TextClipsManager::OnFound);

	i = m_schemeClipSets.insert(MAP_CLIPSETS::value_type(std::string(schemeName), LIST_CLIPSETS())).first;
	m_loadingClips = &(*i).second;

	tstring path;
	tstring usPath;
	
	// Search distribution clips directory.
	OPTIONS->GetPNPath(path, PNPATH_CLIPS);
	OPTIONS->GetPNPath(usPath, PNPATH_USERCLIPS);

	Utf8_Tcs scheme(schemeName);

	path += (LPCTSTR)scheme;
	path += _T("\\");

	usPath += (LPCTSTR)scheme;
	usPath += _T("\\");

	if (DirExists(path.c_str()))
	{
		finder.Find(path.c_str(), _T("*.clips"));
	}
	
	// Search user clips directory and load those:
	if( DirExists(usPath.c_str()) )
	{
		finder.Find(usPath.c_str(), _T("*.clips"));
	}

	m_loadingClips = NULL;
	return (*i).second;
}

bool SortClipsByShortcut(const Clip* p1, const Clip* p2)
{
	return (p1->Shortcut < p2->Shortcut);
}

std::string TextClipsManager::BuildSortedClipList(LPCSTR schemeName) const
{
	MAP_CLIPSETS::const_iterator i = m_schemeClipSets.find(std::string(schemeName));
	
	if (i != m_schemeClipSets.end())
	{
		return std::string("");
	}

	std::string result;
	
	LIST_CLIPS sortedClips;

	BOOST_FOREACH(TextClipSet* set, (*i).second)
	{
		sortedClips.insert(sortedClips.end(), set->GetClips().begin(), set->GetClips().end());
	}

	sortedClips.sort(SortClipsByShortcut);

	BOOST_FOREACH(Clip* clip, sortedClips)
	{
		if (result.size())
			result += ",";

		result += clip->Shortcut;
		CT2CA name(clip->Name.c_str());
		result += ": ";
		result += name;
	}

	return result;
}

void TextClipsManager::Add(TextClipSet* clips)
{
	if(clips->GetScheme() != NULL)
	{
		MAP_CLIPSETS::iterator i = m_schemeClipSets.find(std::string(clips->GetScheme()));
		if (i == m_schemeClipSets.end())
		{
			i = m_schemeClipSets.insert(MAP_CLIPSETS::value_type(std::string(clips->GetScheme()), LIST_CLIPSETS())).first;
		}

		(*i).second.push_back(clips);
	}
	else
	{
		m_clipSets.push_back(clips);
	}
}

/**
 * First save all cached clip sets, then save those in their
 * own files.
 */
void TextClipsManager::Save(bool ignoreFilenames)
{
	LIST_CLIPSETS toSave;

	tstring usPath;
	OPTIONS->GetPNPath(usPath, PNPATH_USERCLIPS);

	tstring clipcache = usPath + _T("installClipCache.xml");

	TextClipsWriter writer;
	writer.Start(clipcache.c_str());
	writer.BeginClipSets();

	// iterate through and store the clips that need saving into a file other
	// than the cache file in toSave, then save them.

	for (LIST_CLIPSETS::iterator i = m_clipSets.begin(); i != m_clipSets.end(); ++i)
	{
		LPCTSTR filename = (*i)->GetFilename();
		if (!ignoreFilenames && filename != NULL)
		{
			toSave.push_back(*i);
		}
		else
		{
			writer.WriteClipSet(*i);
		}
	}

	throw "FIXME";

	/*for (MAP_CLIPSETS::iterator j = m_schemeClipSets.begin(); j != m_schemeClipSets.end(); ++j)
	{
		LPCTSTR filename = (*j).second->GetFilename();
		if (!ignoreFilenames && filename != NULL)
		{
			toSave.push_back((*j).second);
		}
		else
		{
			writer.WriteClipSet((*j).second);
		}
	}*/

	writer.EndClipSets();
	writer.Close();

	// Save the clip sets that have their own file
	for (LIST_CLIPSETS::iterator k = toSave.begin(); k != toSave.end(); ++k)
	{
		(*k)->Save();
	}
}

void TextClipsManager::Reset(const TextClipsManager& other)
{
	clear();
	copy(other);
}

/**
 * Called by the file finder class, creates a new TextClipSet from
 * the given file path.
 */
void TextClipsManager::OnFound(LPCTSTR path, FileFinderData& file, bool& /*shouldContinue*/)
{
	tstring fullpath(path);
	fullpath += file.GetFilename();
	parse(fullpath.c_str());
}

/**
 * Free all the TextClipSet instances and clear the list.
 */
void TextClipsManager::clear()
{
	for (LIST_CLIPSETS::iterator i = m_clipSets.begin(); i != m_clipSets.end(); ++i)
	{
		delete (*i);
	}

	m_clipSets.clear();

	for (MAP_CLIPSETS::iterator j = m_schemeClipSets.begin(); j != m_schemeClipSets.end(); ++j)
	{
		for (LIST_CLIPSETS::iterator k = (*j).second.begin(); k != (*j).second.end(); ++k)
		{
			delete (*k);
		}
	}

	m_schemeClipSets.clear();
}

void TextClipsManager::copy(const TextClipsManager& copy)
{
	throw "FAIL";

	for (LIST_CLIPSETS::const_iterator i = copy.m_clipSets.begin();
		i != copy.m_clipSets.end();
		++i)
	{
		m_clipSets.push_back( new TextClipSet(**i) );
	}

	/*for (MAP_CLIPSETS::const_iterator j = copy.m_schemeClipSets.begin();
		j != copy.m_schemeClipSets.end();
		++j)
	{
		m_schemeClipSets.insert(MAP_CLIPSETS::value_type( (*j).first, new TextClipSet(*(*j).second)));
	}*/
}

/**
 * Uses FileFinder to find the clips files.
 */
void TextClipsManager::findClips()
{
	FileFinder<TextClipsManager> finder(this, &TextClipsManager::OnFound);

	tstring path;
	tstring usPath;
	
	// Search distribution clips directory.
	OPTIONS->GetPNPath(path, PNPATH_CLIPS);
	OPTIONS->GetPNPath(usPath, PNPATH_USERCLIPS);

	tstring clipcache = usPath + _T("installClipCache.xml");

	if (!FileExists(clipcache.c_str()))
	{
		if (DirExists(path.c_str()))
			finder.Find(path.c_str(), _T("*.clips"));

		// Save the clips we found in the cache
		Save(true);
	}
	else
	{
		// Read the cache
		parse(clipcache.c_str());
	}
	
	// Search user clips directory and load those:
	if( DirExists(usPath.c_str()) )
		finder.Find(usPath.c_str(), _T("*.clips"));
}

void TextClipsManager::parse(LPCTSTR filename)
{
	// Load any sets into the current loading set:
	Parser parseState(*m_loadingClips, filename);
	
	XMLParser parser;
	parser.SetParseState(&parseState);

	try
	{
		parser.LoadFile(filename);
	}
	catch( XMLParserException& ex )
	{
		LOG(ex.GetMessage());
	}
}

} // namespace TextClips