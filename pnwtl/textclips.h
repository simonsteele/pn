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
class TextClipSet : public XMLParseState
{
	public:
		TextClipSet(LPCTSTR filename);
		~TextClipSet();

		/**
		 * Builds a list of clips formatted for scintilla display
		 */
		tstring BuildSortedClipList() const;

		const Clip* FindByShortcut(const tstring& shortcut) const;

		const LIST_CLIPS& GetClips();

		LPCTSTR GetName() const;

		/**
		 * Get the scheme name if scheme-tied, NULL otherwise
		 */
		LPCTSTR GetScheme() const;

	//XMLParseState
	public:
		virtual void startElement(LPCTSTR name, XMLAttributes& atts);
		virtual void endElement(LPCTSTR name);
		virtual void characterData(LPCTSTR data, int len);

	protected:
		void clear();
		void parse(LPCTSTR filename);
		void decodeData();

		LIST_CLIPS	clips;

		typedef enum tagEncodings
		{
			eNone,
			eWindows1252,
			eANSI,
		} EEncoding;

	protected:
		EEncoding encoding;
		bool decodeNames;
		int	parseState;
		tstring cData;
		tstring curName;
		tstring name;
		tstring scheme;
		tstring curShortcut;
};

typedef std::list<TextClipSet*> LIST_CLIPSETS;
typedef std::map<std::string, TextClipSet*> MAP_CLIPSETS;

/**
 * Represents a set of text clip sets.
 */
class TextClipsManager
{
public:
	TextClipsManager();
	~TextClipsManager();

	void FindClips();
	void OnFound(LPCTSTR path, LPCTSTR filename);

	const LIST_CLIPSETS& GetClipSets();

	const TextClipSet* GetClips(LPCSTR schemeName);

protected:
	void clear();

protected:
	LIST_CLIPSETS	m_clipSets;
	MAP_CLIPSETS	m_schemeClipSets;
};

} // namespace TextClips

#endif //textclips_h__included