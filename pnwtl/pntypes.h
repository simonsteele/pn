/**
 * @file pntypes.h
 * @brief Define structs etc. used throughout pn2.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef pntypes_h__included
#define pntypes_h__included

#include "scintilla.h"

typedef struct tagFindOptions
{
	CString FindText;
	bool MatchWholeWord;
	bool MatchCase;
	bool UseRegExp;
	bool SearchAll;
	bool Direction;		// true is down.
	bool Loop;
	bool Found;
	bool UseSlashes;
} SFindOptions;

typedef struct tagReplaceOptions : tagFindOptions
{
	CString ReplaceText;
	bool	InSelection;
} SReplaceOptions;

typedef struct tagCloseStruct
{
	void*	pMainFrm;
	bool	bCanClose;
} SCloseStruct;

typedef enum { PNSF_Windows = SC_EOL_CRLF, PNSF_Unix = SC_EOL_LF, PNSF_Mac = SC_EOL_CR, PNSF_NoChange} EPNSaveFormat;

#endif