/**
 * @file textclips.h
 * @brief Text Clips Classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
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
		tstring Text;

		void Insert(CScintilla* scintilla);
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

		const LIST_CLIPS& GetClips();

		LPCTSTR GetName();

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
};

typedef std::list<TextClipSet*> LIST_CLIPSETS;

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

protected:
	void clear();

protected:
	LIST_CLIPSETS	clipSets;
};

} // namespace TextClips

#endif //textclips_h__included