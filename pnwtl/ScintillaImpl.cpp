#include "stdafx.h"
#include "ScintillaImpl.h"

////////////////////////////////////////////////////////////////////////////////
// Search and Replace Code
// Some of this should maybe be moved into a support file to clean this up 
// a bit.
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

bool CScintillaImpl::FindNext(SFindOptions* pOptions)
{
	TextToFind		ft;
	CharacterRange	cr;
	bool			bRet = false;

	// Init TextToFind struct
	memset(&ft, 0, sizeof(TextToFind));
	
	GetSel(cr);

	if(!pOptions->Direction)
	{
		ft.chrg.cpMin = cr.cpMin - 1;
		ft.chrg.cpMax = 0;
	}
	else
	{
		ft.chrg.cpMin = cr.cpMax;
		ft.chrg.cpMax = GetLength();
	}

	///@todo what to do about unicode here?
	char* lpszTextBuffer = new char[pOptions->FindText.GetLength()+1];
	ft.lpstrText = lpszTextBuffer;
	strcpy(lpszTextBuffer, pOptions->FindText);

	ft.chrgText.cpMin = 0;
	ft.chrgText.cpMax = 0;

	int flags = (pOptions->MatchWholeWord ? SCFIND_WHOLEWORD : 0) |
				(pOptions->MatchCase ? SCFIND_MATCHCASE : 0) |
				(pOptions->UseRegExp ? SCFIND_REGEXP : 0);

	///@todo SCFIND_WORDSTART

	int posFind = FindText(flags, &ft);

	if(posFind == -1 && pOptions->Loop)
	{
		if(!pOptions->Direction)
		{
			ft.chrg.cpMin = GetLength();
			ft.chrg.cpMax = 0;
		}
		else
		{
			ft.chrg.cpMin = 0;
			ft.chrg.cpMax = GetLength();
		}

		posFind = FindText(flags, &ft);
	}

	if(posFind != -1)
	{
		//GetTargetStart(), GetTargetEnd()
		EnsureRangeVisible(ft.chrgText.cpMin, ft.chrgText.cpMax);
		SetSel(ft.chrgText.cpMin, ft.chrgText.cpMax);
		pOptions->Found = true;
		bRet = true;
	}
	
	delete [] lpszTextBuffer;

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
	IndicSetFore(0, PNStringToColor("00,80,00"));
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
		//sci.BeginUndoAction();

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
		
		//sci.EndUndoAction();
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

void CScintillaImpl::ReplaceAll(SReplaceOptions* pOptions) 
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
			return;
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
}