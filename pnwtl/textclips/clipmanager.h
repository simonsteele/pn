/**
 * @file clipmanager.h
 * @brief text clips manager.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef CLIPMANAGER_H__INCLUDED
#define CLIPMANAGER_H__INCLUDED

#include "../textclips.h"

class FileFinderData;

namespace TextClips {

/**
 * Represents a set of text clip sets.
 */
class TextClipsManager
{
	public:
		TextClipsManager();
		TextClipsManager(const TextClipsManager& copy);
		~TextClipsManager();

		const LIST_CLIPSETS& GetClips(LPCSTR schemeName);

		std::string BuildSortedClipList(LPCSTR schemeName) const;

		void Add(TextClipSet* clips);
		void Delete(TextClipSet* clips);

		void Reset(const TextClipsManager& copy);

	private:
		void clear();
		void copy(const TextClipsManager& copy);
		void loadClips(LPCTSTR path, std::list<tstring>& files);
		void parse(LPCTSTR filename);
		void getAllKnownSetFilenames(const char* scheme, std::vector<tstring>& clipFiles);

		MAP_CLIPSETS	m_schemeClipSets;
		LIST_CLIPSETS*	m_loadingClips;
};

}

#endif