/**
 * @file ScintillaImpl.cpp
 * @brief Implement further functionality for a scintilla wrapper.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "ScintillaImpl.h"

////////////////////////////////////////////////////////////////////////////////
// Search and Replace Support Code (please move this)
////////////////////////////////////////////////////////////////////////////////

/**
 * Is the character an octal digit?
 */
static bool IsOctalDigit(char ch) {
	return ch >= '0' && ch <= '7';
}

/**
 * If the character is an hexa digit, get its value.
 */
static int GetHexaDigit(char ch) {
	if (ch >= '0' && ch <= '9') {
		return ch - '0';
	}
	if (ch >= 'A' && ch <= 'F') {
		return ch - 'A' + 10;
	}
	if (ch >= 'a' && ch <= 'f') {
		return ch - 'a' + 10;
	}
	return -1;
}

/**
 * Convert C style \a, \b, \f, \n, \r, \t, \v, \ooo and \xhh into their indicated characters.
 */
unsigned int UnSlash(char *s) {
	char *sStart = s;
	char *o = s;

	while (*s) {
		if (*s == '\\') {
			s++;
			if (*s == 'a') {
				*o = '\a';
			} else if (*s == 'b') {
				*o = '\b';
			} else if (*s == 'f') {
				*o = '\f';
			} else if (*s == 'n') {
				*o = '\n';
			} else if (*s == 'r') {
				*o = '\r';
			} else if (*s == 't') {
				*o = '\t';
			} else if (*s == 'v') {
				*o = '\v';
			} else if (IsOctalDigit(*s)) {
				int val = *s - '0';
				if (IsOctalDigit(*(s + 1))) {
					s++;
					val *= 8;
					val += *s - '0';
					if (IsOctalDigit(*(s + 1))) {
						s++;
						val *= 8;
						val += *s - '0';
					}
				}
				*o = static_cast<char>(val);
			} else if (*s == 'x') {
				s++;
				int val = 0;
				int ghd = GetHexaDigit(*s);
				if (ghd >= 0) {
					s++;
					val = ghd;
					ghd = GetHexaDigit(*s);
					if (ghd >= 0) {
						s++;
						val *= 16;
						val += ghd;
					}
				}
				*o = static_cast<char>(val);
			} else {
				*o = *s;
			}
		} else {
			*o = *s;
		}
		o++;
		if (*s) {
			s++;
		}
	}
	*o = '\0';
	return o - sStart;
}

/**
 * Convert C style \0oo into their indicated characters.
 * This is used to get control characters into the regular expresion engine.
 */
unsigned int UnSlashLowOctal(char *s) {
	char *sStart = s;
	char *o = s;
	while (*s) {
		if ((s[0] == '\\') && (s[1] == '0') && IsOctalDigit(s[2]) && IsOctalDigit(s[3])) {
			*o = static_cast<char>(8 * (s[2] - '0') + (s[3] - '0'));
			s += 3;
		} else {
			*o = *s;
		}
		o++;
		if (*s)
			s++;
	}
	*o = '\0';
	return o - sStart;
}

static int UnSlashAsNeeded(CString& s, bool escapes, bool regularExpression)
{
	//StringDup(s.c_str());
	char *sUnslashed = new char[s.GetLength()+1];
	strcpy(sUnslashed, s);

	int len;
	if (escapes) 
	{
		if (regularExpression) 
		{
			// For regular expressions only escape sequences allowed start with \0
			len = UnSlashLowOctal(sUnslashed);
		} else 
		{
			// C style escapes allowed
			len = UnSlash(sUnslashed);
		}
	} 
	else 
	{
		len = strlen(sUnslashed);
	}
	
	s = sUnslashed;
	delete [] sUnslashed;
	return len;
}

////////////////////////////////////////////////////////////
// Manage all of the functionality in this class...
////////////////////////////////////////////////////////////

int CScintillaImpl::HandleNotify(LPARAM lParam)
{
	int msg = CScintilla::HandleNotify(lParam);
	if(msg == SCN_CHARADDED)
	{
		DumbIndent( ((SCNotification*)lParam)->ch );
	}
	else if(msg == SCN_UPDATEUI)
	{
		ManageBraceMatch();
	}

	return msg;
}

////////////////////////////////////////////////////////////
// Indentation Code
////////////////////////////////////////////////////////////

int CScintillaImpl::GetIndentLevel(int line)
{
	return GetLineIndentation(line);
}

void CScintillaImpl::DumbIndent(char ch)
{
	//CharacterRange cr;
	//GetSel(cr);
	int curLine = GetCurrentPos();
	curLine = LineFromPosition(curLine);

	if(ch == '\r' || ch == '\n')
	{
		int previousIndent = GetIndentLevel(curLine-1);
		IndentLine(curLine, previousIndent);
	}
}

void CScintillaImpl::IndentLine(int line, int indent)
{
	if (indent < 0)
		return;
	
	CharacterRange crange;
	GetSel(crange);

	int posBefore = GetLineIndentPosition(line);
	
	SetLineIndentation(line, indent);
	
	int posAfter = GetLineIndentPosition(line);
	int posDifference =  posAfter - posBefore;
	
	if (posAfter > posBefore) 
	{
		// Move selection on
		if (crange.cpMin >= posBefore) 
		{
			crange.cpMin += posDifference; 
		}

		if (crange.cpMax >= posBefore) 
		{
			crange.cpMax += posDifference; 
		}
	} 
	else if (posAfter < posBefore) 
	{
		// Move selection back
		if (crange.cpMin >= posAfter) 
		{
			if (crange.cpMin >= posBefore)
				crange.cpMin += posDifference; 
			else 
				crange.cpMin = posAfter; 
		}
		if (crange.cpMax >= posAfter) 
		{
			if (crange.cpMax >= posBefore)
				crange.cpMax += posDifference; 
			else 
				crange.cpMax = posAfter; 
		}
	}
	SetSel(crange.cpMin, crange.cpMax);
}


////////////////////////////////////////////////////////////
// Brace Matching Code
////////////////////////////////////////////////////////////

/**
 * @return true if we're inside the braces...
 */
bool CScintillaImpl::FindMatchingBraces(int& CaretBrace, int& OtherBrace)
{
	///@todo Move to the properties? And should this be scheme-overridable.
	const char* braceset = "{[()]}";
	
	int Caret = GetCurrentPos();
	bool After = true;	// caret after brackets...
	
	CaretBrace = -1;

	if(Caret > 0)
	{
		// brace could be before the caret.
		char Before = GetCharAt(Caret-1);
		if(Before != NULL && strchr(braceset, Before))
			CaretBrace = Caret-1;
	}

	if(CaretBrace < 0)
	{
		// Look the other side of the caret...
		char After = GetCharAt(Caret);
		if(After != NULL && strchr(braceset, After))
		{
			CaretBrace = Caret;
			After = false;
		}
	}

	OtherBrace = BraceMatch(CaretBrace);

	return (OtherBrace > CaretBrace) ? After : !After;
}

void CScintillaImpl::ManageBraceMatch()
{
	int brace1, brace2 = -1;
	FindMatchingBraces(brace1, brace2);
	
	if(brace1 > -1 && brace2 == -1)
	{
		BraceBadLight(brace1);
		SetHighlightGuide(0);
	}
	else
	{
		BraceHighlight(brace1, brace2);
		int col1 = GetColumn(brace1);
		int col2 = GetColumn(brace2);
		SetHighlightGuide(min(col1, col2));
	}
}

////////////////////////////////////////////////////////////
// Search and Replace code
////////////////////////////////////////////////////////////

bool CScintillaImpl::FindNext(SFindOptions* pOptions)
{
	CharacterRange	cr;
	int				startPosition, endPosition;
	bool			bRet = false;

	int lenFind = UnSlashAsNeeded(pOptions->FindText, pOptions->UseSlashes, pOptions->UseRegExp);
	if(lenFind > 0)
	{

		GetSel(cr);

		LPTSTR ft = pOptions->FindText.LockBuffer();
		USES_CONVERSION;
		const char* findtext = T2A(ft);

		if(!pOptions->Direction)
		{
			startPosition = cr.cpMin - 1;
			endPosition = 0;
		}
		else
		{
			startPosition = cr.cpMax;
			endPosition = GetLength();
		}

		///@todo SCFIND_WORDSTART
		int flags = (pOptions->MatchWholeWord ? SCFIND_WHOLEWORD : 0) |
					(pOptions->MatchCase ? SCFIND_MATCHCASE : 0) |
					(pOptions->UseRegExp ? SCFIND_REGEXP : 0);

		SetTargetStart(startPosition);
		SetTargetEnd(endPosition);
		SetSearchFlags(flags);
		
		int posFind = SearchInTarget(lenFind, findtext);

		if(posFind == -1 && pOptions->Loop)
		{
			if(!pOptions->Direction)
			{
				startPosition = GetLength();
				endPosition = 0;
			}
			else
			{
				startPosition = 0;
				endPosition = GetLength();
			}

			SetTargetStart(startPosition);
			SetTargetEnd(endPosition);
			posFind = SearchInTarget(lenFind, findtext);
		}

		if(posFind != -1)
		{
			int start = GetTargetStart();
			int end = GetTargetEnd();
			EnsureRangeVisible(start, end);
			SetSel(start, end);
			pOptions->Found = true;
			bRet = true;
		}

		// Must un-lock that string before we return...
		pOptions->FindText.UnlockBuffer();
	}

	return bRet;
}

void CScintillaImpl::HighlightAll(SFindOptions* pOptions) 
{
	CString findTarget(pOptions->FindText);

	int findLen = UnSlashAsNeeded(findTarget, /*pOptions->UnSlash*/false, pOptions->UseRegExp);
	if (findLen == 0) 
	{
		/*SString msg = LocaliseMessage(
			inSelection ?
			"Find string must not be empty for 'Replace in Selection' command." :
			"Find string must not be empty for 'Replace All' command.");
		FindMessageBox(msg);*/
		return;
	}

	// Whole document
	int startPosition = 0;
	int endPosition = GetLength();
	
	int flags = (pOptions->MatchWholeWord ? SCFIND_WHOLEWORD : 0) |
	            (pOptions->MatchCase ? SCFIND_MATCHCASE : 0) |
	            (pOptions->UseRegExp ? SCFIND_REGEXP : 0);
	
	SetTarget(startPosition, endPosition);
	SetSearchFlags(flags);

	/////////////////////////////////////
	IndicSetStyle(0, INDIC_SQUIGGLE);
	IndicSetFore(0, RGB(0, 0x80, 0));
	/////////////////////////////////////
	
	int posFind = SearchInTarget(findLen, (LPCTSTR)findTarget);
	
	// Fix from Replace All Code:
	if ((findLen == 1) && pOptions->UseRegExp && (findTarget[0] == '^')) 
	{
		// Special case for find all start of line so it hits the first line
		posFind = startPosition;
		SetTarget(startPosition, startPosition);
	}
	
	if ((posFind != -1) && (posFind <= endPosition)) 
	{
		int lastMatch = posFind;
		//BeginUndoAction();

		while (posFind != -1) 
		{
			int lenTarget = GetTargetEnd() - GetTargetStart();
			
			lastMatch = posFind + findLen;

			StartStyling(GetTargetStart(), INDICS_MASK);
			SetStyling(lenTarget, INDIC0_MASK);
			
			// For the special cases of start of line and end of line
			// Something better could be done but there are too many special cases
			if (lenTarget <= 0)
				lastMatch++;

			SetTarget(lastMatch, endPosition);
			posFind = SearchInTarget(findLen, (LPCTSTR)findTarget);
		}
		
		//EndUndoAction();
	}
}


bool CScintillaImpl::ReplaceOnce(SReplaceOptions* pOptions)
{
	if(pOptions->Found) 
	{
		///@todo this....
		CString replaceTarget(pOptions->ReplaceText);
		int replaceLen = UnSlashAsNeeded(replaceTarget, /*pOptions->UnSlash*/false, pOptions->UseRegExp);
		
		CharacterRange cr;
		GetSel(cr);

		SetTarget(cr.cpMin, cr.cpMax);

		int lenReplaced = replaceLen;
		
		if (pOptions->UseRegExp)
			ReplaceTargetRE(replaceLen, (LPCTSTR)replaceTarget);
		else
			ReplaceTarget(replaceLen, (LPCTSTR)replaceTarget);
		
		SetSel(cr.cpMin + lenReplaced, cr.cpMin);
		
		pOptions->Found = false;
	}

	return FindNext(pOptions);
}

int CScintillaImpl::ReplaceAll(SReplaceOptions* pOptions) 
{
	int repCount = 0;

	CString findTarget(pOptions->FindText);

	int findLen = UnSlashAsNeeded(findTarget, /*pOptions->UnSlash*/false, pOptions->UseRegExp);
	if (findLen == 0) 
	{
		/*SString msg = LocaliseMessage(
			inSelection ?
			"Find string must not be empty for 'Replace in Selection' command." :
			"Find string must not be empty for 'Replace All' command.");
		FindMessageBox(msg);*/
		return 0;
	}

	CharacterRange cr;
	GetSel(cr);

	int startPosition = cr.cpMin;
	int endPosition = cr.cpMax;
	
	if (pOptions->InSelection) 
	{
		if (startPosition == endPosition) 
		{
			/*SString msg = LocaliseMessage("Selection must not be empty for 'Replace in Selection' command.");
			FindMessageBox(msg);*/
			return 0;
		}
	} 
	else 
	{
		endPosition = GetLength();
		if (pOptions->Loop) 
		{
			// Whole document
			startPosition = 0;
		}
		// If not looping, replace all only from caret to end of document
	}

	CString replaceTarget(pOptions->ReplaceText);
	
	int replaceLen = UnSlashAsNeeded(replaceTarget, /*pOptions->UnSlash*/false, pOptions->UseRegExp);
	
	int flags = (pOptions->MatchWholeWord ? SCFIND_WHOLEWORD : 0) |
	            (pOptions->MatchCase ? SCFIND_MATCHCASE : 0) |
	            (pOptions->UseRegExp ? SCFIND_REGEXP : 0);
	
	SetTarget(startPosition, endPosition);
	SetSearchFlags(flags);
	
	int posFind = SearchInTarget(findLen, (LPCTSTR)findTarget);
	
	if ((findLen == 1) && pOptions->UseRegExp && (findTarget[0] == '^')) 
	{
		// Special case for replace all start of line so it hits the first line
		posFind = startPosition;
		SetTarget(startPosition, startPosition);
	}
	
	if ((posFind != -1) && (posFind <= endPosition)) 
	{
		int lastMatch = posFind;
		BeginUndoAction();

		while (posFind != -1) 
		{
			int lenTarget = GetTargetEnd() - GetTargetStart();
			int lenReplaced = replaceLen;
			if (pOptions->UseRegExp)
				lenReplaced = ReplaceTargetRE(replaceLen, (LPCTSTR)replaceTarget);
			else
				ReplaceTarget(replaceLen, (LPCTSTR)replaceTarget);

			repCount++;

			// Modify for change caused by replacement
			endPosition += lenReplaced - lenTarget;
			lastMatch = posFind + lenReplaced;

			// For the special cases of start of line and end of line
			// Something better could be done but there are too many special cases
			if (lenTarget <= 0)
				lastMatch++;

			SetTarget(lastMatch, endPosition);
			posFind = SearchInTarget(findLen, (LPCTSTR)findTarget);
		}

		if (pOptions->InSelection)
			SetSel(startPosition, endPosition);
		else
			SetSel(lastMatch, lastMatch);
		
		EndUndoAction();
	} 
	else 
	{
		/*SString msg = LocaliseMessage(
			"No replacements because string '^0' was not present.", findWhat.c_str());
		FindMessageBox(msg);*/
	}

	return repCount;
}