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

namespace TextClips {

typedef std::list<TextClipSet*> LIST_CLIPSETS;
typedef std::map<std::string, TextClipSet*> MAP_CLIPSETS;

/**
 * Represents a set of text clip sets.
 */
class TextClipsManager : public XMLParseState
{
	public:
		TextClipsManager();
		TextClipsManager(const TextClipsManager& copy);
		~TextClipsManager();

		void OnFound(LPCTSTR path, FileFinderData& file, bool& /*shouldContinue*/);

		const LIST_CLIPSETS& GetClipSets();

		TextClipSet* GetClips(LPCSTR schemeName);

		void Add(TextClipSet* clips);

		void Save(bool ignoreFilenames = false);

		void Reset(const TextClipsManager& copy);

		//XMLParseState
	public:
		virtual void startElement(LPCTSTR name, XMLAttributes& atts);
		virtual void endElement(LPCTSTR name);
		virtual void characterData(LPCTSTR data, int len);

	private:
		typedef enum tagEncodings
			{
				eNone,
				eWindows1252,
				eANSI,
			} EEncoding;

		void clear();
		void copy(const TextClipsManager& copy);
		void decodeData();
		void findClips();
		void parse(LPCTSTR filename);

		LIST_CLIPSETS	m_clipSets;
		MAP_CLIPSETS	m_schemeClipSets;

		// Parse state:
		bool decodeNames;
		int	m_parseState;
		std::string m_cData;
		tstring m_curName;
		std::string m_curShortcut;
		tstring m_curFileName;
		TextClipSet* m_pCurSet;
		EEncoding m_curEncoding;
};

}

#endif