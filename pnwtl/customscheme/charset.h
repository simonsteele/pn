/**
 * @file charset.h
 * @brief Character set matching class
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * This code is basically an extraction of regular expression character set
 * matching techniques as seen in the public domain regular expression classes
 * included with Scintilla: http://www.scintilla.org/
 */
#ifndef charset_h__included
#define charset_h__included

#define MAXCHR	256
#define CHRBIT	8
#define BITBLK	MAXCHR/CHRBIT

/**
 * @brief Use to match a character against a regex style character set.
 */
class CharSet
{
	public:
		CharSet();
		CharSet(const CharSet& copy);
		CharSet(const char* pattern);

		const CharSet& operator = (const CharSet& copy);

		bool ParsePattern(const char* pattern, bool caseSensitive = true);
		
		bool Match(const char ch) const;

	protected:
		void ChSet(char c);
		void ChSetWithCase(char c, bool caseSensitive);
		const char escapeValue(char ch);

	protected:
		char bittab[BITBLK];
		char bitset[BITBLK];
		bool matchAll;
};

#endif