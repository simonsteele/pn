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
}

/**
 * First save all cached clip sets, then save those in their
 * own files.
 */
void TextClipsManager::Save()
{
	// iterate through and store the clips that need saving into a file other
	// than the cache file in toSave, then save them.
	for (MAP_CLIPSETS::iterator j = m_schemeClipSets.begin(); j != m_schemeClipSets.end(); ++j)
	{
		BOOST_FOREACH(TextClipSet* set, (*j).second)
		{
			LPCTSTR filename = set->GetFilename();
			if (filename != NULL)
			{
				set->Save();
			}
		}
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
	for (MAP_CLIPSETS::const_iterator j = copy.m_schemeClipSets.begin();
		j != copy.m_schemeClipSets.end();
		++j)
	{
		MAP_CLIPSETS::iterator k = m_schemeClipSets.insert(MAP_CLIPSETS::value_type((*j).first, LIST_CLIPSETS())).first;
		LIST_CLIPSETS& mine((*k).second);
		BOOST_FOREACH(TextClipSet* copySet, (*j).second)
		{
			mine.push_back(new TextClipSet(*copySet));
		}
	}
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