/**
 * @file textclips.h
 * @brief Text Clips Classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2007 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef textclips_h__included
#define textclips_h__included

#include <list>

namespace TextClips {

/**
 * Simple class to represent a text clip.
 */
class Clip
{
	public:
		tstring Name;
		tstring Shortcut;
		tstring Text;

		void Insert(CScintilla* scintilla) const;
};

typedef std::list<Clip*>	LIST_CLIPS;

/**
 * Represents a file full of text clips.
 */
class TextClipSet
{
	public:
		TextClipSet(LPCTSTR filename, LPCTSTR name, LPCTSTR scheme);
		~TextClipSet();

		/**
		 * Add a clip
		 */
		void Add(Clip* clip);

		/**
		 * Builds a list of clips formatted for scintilla display
		 */
		tstring BuildSortedClipList() const;

		/**
		 * Find a Clip by its text shortcut
		 */
		const Clip* FindByShortcut(const tstring& shortcut) const;

		/**
		 * Get the clips
		 */
		const LIST_CLIPS& GetClips() const;

		/**
		 * Get the name of this clip set
		 */
		LPCTSTR GetName() const;

		/**
		 * Get the filename that stores these clips, or NULL if in the global store
		 */
		LPCTSTR GetFilename() const;

		/**
		 * Get the scheme name if scheme-tied, NULL otherwise
		 */
		LPCTSTR GetScheme() const;

		/**
		 * Save this clipset to its file
		 */
		void Save();

	private:
		void clear();
		
		LIST_CLIPS	m_clips;

		tstring m_name;
		tstring m_scheme;
		tstring m_filename;
};

typedef std::list<TextClipSet*> LIST_CLIPSETS;
typedef std::map<std::string, TextClipSet*> MAP_CLIPSETS;

/**
 * Represents a set of text clip sets.
 */
class TextClipsManager : public XMLParseState
{
	public:
		TextClipsManager();
		~TextClipsManager();

		void OnFound(LPCTSTR path, LPCTSTR filename);

		const LIST_CLIPSETS& GetClipSets();

		const TextClipSet* GetClips(LPCSTR schemeName);

		void Save(bool ignoreFilenames = false);

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
		void decodeData();
		void findClips();
		void parse(LPCTSTR filename);

		LIST_CLIPSETS	m_clipSets;
		MAP_CLIPSETS	m_schemeClipSets;

		// Parse state:
		bool decodeNames;
		int	m_parseState;
		tstring m_cData;
		tstring m_curName;
		tstring m_curShortcut;
		tstring m_curFileName;
		TextClipSet* m_pCurSet;
		EEncoding m_curEncoding;
};

} // namespace TextClips

#endif //textclips_h__included