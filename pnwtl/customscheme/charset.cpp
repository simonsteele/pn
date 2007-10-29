/**
 * @file charset.cpp
 * @brief Character set matching class
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * This code is basically an extraction of regular expression character set
 * matching techniques as seen in the public domain regular expression classes
 * included with Scintilla: http://www.scintilla.org/
 */
#include "stdafx.h"
#include "charset.h"

// These defines are not meant to be changed, they are
// simply included here to improve readability.
#define BLKIND	0170
#define BITIND	07

const char bitarr[] = {1,2,4,8,16,32,64,'\200'};

#define isinset(x,y) 	((x)[((y)&BLKIND)>>3] & bitarr[(y)&BITIND])

/// Constructor...
CharSet::CharSet()
{
	memset(bitset, 0, sizeof(bitset));
	memset(bittab, 0, sizeof(bittab));
	matchAll = false;
}

/// Copy Constructor...
CharSet::CharSet(const CharSet& copy)
{
	*this = copy;
}

/// Constructor that takes a pattern.
CharSet::CharSet(const char* pattern)
{
	memset(bitset, 0, sizeof(bitset));
	memset(bittab, 0, sizeof(bittab));
	matchAll = false;

	ParsePattern(pattern);
}

const CharSet& CharSet::operator = (const CharSet& copy)
{
	matchAll = copy.matchAll;
	memcpy(bitset, copy.bitset, sizeof(bitset));
	memcpy(bittab, copy.bittab, sizeof(bittab));
	return *this;
}

/**
 * @return true if character matches...
 */
bool CharSet::Match(const char ch) const
{
	return matchAll || (isinset(bitset, ch) != 0);
}

/**
 * @param c Character to add to the matching set.
 */
void CharSet::ChSet(char c)
{
	bittab[((c) & BLKIND) >> 3] |= bitarr[(c) & BITIND];
}

/**
 * @param c Character to add to the matching set.
 * @param caseSensitive true if match should be case sensitive.
 */
void CharSet::ChSetWithCase(char c, bool caseSensitive)
{
	if (caseSensitive)
	{
		ChSet(c);
	}
	else
	{
		if ((c >= 'a') && (c <= 'z'))
		{
			ChSet(c);
			ChSet(static_cast<char>(c - 'a' + 'A'));
		}
		else if ((c >= 'A') && (c <= 'Z'))
		{
			ChSet(c);
			ChSet(static_cast<char>(c - 'A' + 'a'));
		}
		else
		{
			ChSet(c);
		}
	}
}

/**
 * @return the c escaped version of a character.
 */
const char CharSet::escapeValue(char ch) {
	switch (ch)
	{
		case 'a':	return '\a';
		case 'b':	return '\b';
		case 'f':	return '\f';
		case 'n':	return '\n';
		case 'r':	return '\r';
		case 't':	return '\t';
		case 'v':	return '\v';
	}
	return 0;
}

/**
 * @brief Transform a character set pattern into a bit map of matching values.
 */
bool CharSet::ParsePattern(const char* pattern, bool caseSensitive)
{
	const char* p = pattern;
	size_t length = strlen(pattern);
	char mask; // XOR mask
	char c1, c2;
	
	bool bPatternOK = false;

	for(size_t i = 0; i < length; i++, p++)
	{
		if(*p == '*')
		{
			bPatternOK = true;
			matchAll = true;
		}
		else if(*p == '[')               // match char class...
		{
			bPatternOK = true;		// OK, we've got a start....

			i++;
			// Check for cancelling set.
			if (*++p == '^')
			{
				mask = '\377';	
				i++;
				p++;
			}
			else
				mask = 0;

			// dash is valid if the first character in the pattern - otherwise it's a range.
			if (*p == '-')
			{
				i++;
				ChSet(*p++);
			}

			// Can include real brace close character if it's at the start of a pattern.
			if (*p == ']')
			{
				i++;
				ChSet(*p++);
			}

			// Examine the remaining characters...
			while (*p && *p != ']')
			{
				if (*p == '-' && *(p+1) && *(p+1) != ']')
				{
					i++;
					p++;
					c1 = *(p-2) + 1;
					i++;
					c2 = *p++;
					while (c1 <= c2)
					{
						ChSetWithCase(static_cast<char>(c1++), caseSensitive);
					}
				} 
				else if (*p == '\\' && *(p+1))
				{
					i++;
					p++;
					char escape = escapeValue(*p);
					if (escape)
						ChSetWithCase(escape, caseSensitive);
					else
						ChSetWithCase(*p, caseSensitive);
					i++;
					p++;
				}
				else
				{
					i++;
					ChSetWithCase(*p++, caseSensitive);
				}
			}
			
			if (!*p)
				return false; // Missing ]

			char* mp = bitset;
			for (int n = 0; n < BITBLK; bittab[n++] = (char) 0)
				*mp++ = static_cast<char>(mask ^ bittab[n]);
	
		} // if (*p == '[')

		if(bPatternOK)
			 break;
	} // for...

	return bPatternOK;
}