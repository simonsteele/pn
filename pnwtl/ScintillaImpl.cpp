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

CScintillaImpl::CScintillaImpl()
{
	lastFindDetails.findPhrase = _T("");
	lastFindDetails.startPos = 0;
}

////////////////////////////////////////////////////////////////////////////////
// Search and Replace Support Code (please move this)  ///@todo
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
// Folding Code...
////////////////////////////////////////////////////////////

void CScintillaImpl::ToggleFold()
{
	int line = LineFromPosition(GetCurrentPos());
	
	if( !(GetFoldLevel(line) & SC_FOLDLEVELHEADERFLAG) )
	{
        line = GetFoldParent(line);
	}

	CScintilla::ToggleFold(line);
}

void CScintillaImpl::FoldAll()
{
	SPerform(SCI_COLOURISE, 0, -1);
	int maxLine = SPerform(SCI_GETLINECOUNT);
	for (int line = 0; line < maxLine; line++)
	{
		int level = SPerform(SCI_GETFOLDLEVEL, line);
		if ((level & SC_FOLDLEVELHEADERFLAG) &&
		        (SC_FOLDLEVELBASE == (level & SC_FOLDLEVELNUMBERMASK)))
		{
			int lineMaxSubord = SPerform(SCI_GETLASTCHILD, line, -1);
			SPerform(SCI_SETFOLDEXPANDED, line, 0);
			if (lineMaxSubord > line)
				SPerform(SCI_HIDELINES, line + 1, lineMaxSubord);
		}
	}
}

void CScintillaImpl::UnFoldAll()
{
	SPerform(SCI_COLOURISE, 0, -1);
	int maxLine = SPerform(SCI_GETLINECOUNT);
	for (int line = 0; line < maxLine; line++)
	{
		int level = SPerform(SCI_GETFOLDLEVEL, line);
		if ((level & SC_FOLDLEVELHEADERFLAG) &&
		        (SC_FOLDLEVELBASE == (level & SC_FOLDLEVELNUMBERMASK)))
		{
			SPerform(SCI_SETFOLDEXPANDED, line, 1);
			Expand(line, true, false, 0, level);
			line--;
		}
	}
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

int CScintillaImpl::FindNext(SFindOptions* pOptions)
{
	CharacterRange	cr;
	int				startPosition, endPosition;
	int				bRet = fnNotFound;
	bool			checkFoundPos = true;

	int lenFind = UnSlashAsNeeded(pOptions->FindText, pOptions->UseSlashes, pOptions->UseRegExp);
	if(lenFind > 0)
	{
		pOptions->Found = false;

		///@todo set find pos on selchange/edit update
		if( pOptions->FindText.Compare( lastFindDetails.findPhrase.c_str() ) != 0 )
		{
			// Find text has changed, set the startPos value, 
			// and don't check to see if we wrapped this time.
			//lastFindDetails.startPos = GetCurrentPos();
			lastFindDetails.findPhrase = pOptions->FindText;
			checkFoundPos = false;
		}

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
			bRet = fnFound;
		}

		// Must un-lock that string before we return...
		pOptions->FindText.UnlockBuffer();

		if( checkFoundPos && bRet )
		{
			GetSel(cr);

			if( lastFindDetails.startPos == cr.cpMin )
				bRet = fnReachedStart;
		}
		else
		{
			CharacterRange cr;
			GetSel(cr);
			lastFindDetails.startPos = cr.cpMin;
		}
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

	return FindNext(pOptions) != 0;
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

////////////////////////////////////////////////////////////
// Printing Code...
////////////////////////////////////////////////////////////

/**
 * PrintDocument
 * note: This code originally copied from SciTE.
 * @param pOptions - pointer to a struct containing print setup info (inc margins).
 * @param showDialog - Should we show the print dialog?
 */
void CScintillaImpl::PrintDocument(SPrintOptions* pOptions, bool showDialog) ///< false if must print silently (using default settings).
{	
	PRINTDLG pdlg = {
	                    sizeof(PRINTDLG), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	                };
	pdlg.hwndOwner = /*MainHWND()*/ this->m_scihWnd;
	pdlg.hInstance = /*hInstance*/GetModuleHandle(NULL);
	pdlg.Flags = PD_USEDEVMODECOPIES | PD_ALLPAGES | PD_RETURNDC;
	
	// We do not know how many pages in the document until the 
	// printer is selected and the paper size is known.
	pdlg.nFromPage = 1;
	pdlg.nToPage = 1;
	pdlg.nMinPage = 1;
	pdlg.nMaxPage = 0xffffU; 
	
	pdlg.nCopies = 1;
	pdlg.hDC = 0;
	pdlg.hDevMode = pOptions->hDevMode;
	pdlg.hDevNames = pOptions->hDevNames;

	// See if a range has been selected
	CharacterRange crange;
	GetSel(crange);
	int startPos = crange.cpMin;
	int endPos = crange.cpMax;

	if (startPos == endPos) {
		pdlg.Flags |= PD_NOSELECTION;
	} else {
		pdlg.Flags |= PD_SELECTION;
	}
	if (!showDialog) {
		// Don't display dialog box, just use the default printer and options
		pdlg.Flags |= PD_RETURNDEFAULT;
	}
	if (!::PrintDlg(&pdlg)) {
		return;
	}

	pOptions->hDevMode = pdlg.hDevMode;
	pOptions->hDevNames = pdlg.hDevNames;

	HDC hdc = pdlg.hDC;

	CRect rectMargins, rectPhysMargins;
	CPoint ptPage;
	CPoint ptDpi;

	// Get printer resolution
	ptDpi.x = GetDeviceCaps(hdc, LOGPIXELSX);    // dpi in X direction
	ptDpi.y = GetDeviceCaps(hdc, LOGPIXELSY);    // dpi in Y direction

	// Start by getting the physical page size (in device units).
	ptPage.x = GetDeviceCaps(hdc, PHYSICALWIDTH);   // device units
	ptPage.y = GetDeviceCaps(hdc, PHYSICALHEIGHT);  // device units

	// Get the dimensions of the unprintable
	// part of the page (in device units).
	rectPhysMargins.left = GetDeviceCaps(hdc, PHYSICALOFFSETX);
	rectPhysMargins.top = GetDeviceCaps(hdc, PHYSICALOFFSETY);

	// To get the right and lower unprintable area,
	// we take the entire width and height of the paper and
	// subtract everything else.
	rectPhysMargins.right = ptPage.x						// total paper width
	                        - GetDeviceCaps(hdc, HORZRES) // printable width
	                        - rectPhysMargins.left;				// left unprintable margin

	rectPhysMargins.bottom = ptPage.y						// total paper height
	                         - GetDeviceCaps(hdc, VERTRES)	// printable height
	                         - rectPhysMargins.top;				// right unprintable margin

	// At this point, rectPhysMargins contains the widths of the
	// unprintable regions on all four sides of the page in device units.

	// Take in account the page setup given by the user (if one value is not null)
	if (pOptions->rcMargins.left != 0 || pOptions->rcMargins.right != 0 ||
		pOptions->rcMargins.top != 0 || pOptions->rcMargins.bottom != 0) 
	{
		CRect rectSetup;

		// Convert the hundredths of millimeters (HiMetric) or
		// thousandths of inches (HiEnglish) margin values
		// from the Page Setup dialog to device units.
		// (There are 2540 hundredths of a mm in an inch.)

		char localeInfo[3];
		GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IMEASURE, localeInfo, 3);

		if (localeInfo[0] == '0')
		{	// Metric system. '1' is US System
			rectSetup.left		= MulDiv (pOptions->rcMargins.left, ptDpi.x, 2540);
			rectSetup.top		= MulDiv (pOptions->rcMargins.top, ptDpi.y, 2540);
			rectSetup.right		= MulDiv(pOptions->rcMargins.right, ptDpi.x, 2540);
			rectSetup.bottom	= MulDiv(pOptions->rcMargins.bottom, ptDpi.y, 2540);
		} 
		else 
		{
			rectSetup.left		= MulDiv(pOptions->rcMargins.left, ptDpi.x, 1000);
			rectSetup.top		= MulDiv(pOptions->rcMargins.top, ptDpi.y, 1000);
			rectSetup.right		= MulDiv(pOptions->rcMargins.right, ptDpi.x, 1000);
			rectSetup.bottom	= MulDiv(pOptions->rcMargins.bottom, ptDpi.y, 1000);
		}

		// Dont reduce margins below the minimum printable area
		rectMargins.left	= max(rectPhysMargins.left, rectSetup.left);
		rectMargins.top		= max(rectPhysMargins.top, rectSetup.top);
		rectMargins.right	= max(rectPhysMargins.right, rectSetup.right);
		rectMargins.bottom	= max(rectPhysMargins.bottom, rectSetup.bottom);
	} 
	else 
	{
		rectMargins.left	= rectPhysMargins.left;
		rectMargins.top		= rectPhysMargins.top;
		rectMargins.right	= rectPhysMargins.right;
		rectMargins.bottom	= rectPhysMargins.bottom;
	}

	// rectMargins now contains the values used to shrink the printable
	// area of the page.

	// Convert device coordinates into logical coordinates
	DPtoLP(hdc, (LPPOINT) &rectMargins, 2);
	DPtoLP(hdc, (LPPOINT)&rectPhysMargins, 2);

	// Convert page size to logical units and we're done!
	DPtoLP(hdc, (LPPOINT) &ptPage, 1);

	///@todo...
	//SString headerFormat = props.Get("print.header.format");
	//SString footerFormat = props.Get("print.footer.format");
	//SString headerOrFooter;	// Usually the path, date and page number

	TEXTMETRIC tm;
	
	//SString headerStyle = props.Get("print.header.style");
	//StyleDefinition sdHeader(headerStyle.c_str());

	//int headerLineHeight = ::MulDiv(
	//                           (sdHeader.specified & StyleDefinition::sdSize) ? sdHeader.size : 9,
	//                           ptDpi.y, 72);

	int headerLineHeight = ::MulDiv(9, ptDpi.y, 72);

	HFONT fontHeader = ::CreateFont(headerLineHeight,
	                                0, 0, 0,
	                                //sdHeader.bold ? FW_BOLD : FW_NORMAL
									FW_NORMAL,
	                                FALSE /*sdHeader.italics*/,
	                                FALSE /*sdHeader.underlined*/,
	                                0, 0, 0,
	                                0, 0, 0,
	                                /*(sdHeader.specified & StyleDefinition::sdFont) ? sdHeader.font.c_str() :*/ "Arial");
	
	::SelectObject(hdc, fontHeader);
	::GetTextMetrics(hdc, &tm);
	headerLineHeight = tm.tmHeight + tm.tmExternalLeading;

	//SString footerStyle = props.Get("print.footer.style");
	//StyleDefinition sdFooter(footerStyle.c_str());

	//int footerLineHeight = ::MulDiv(
	//                           (sdFooter.specified & StyleDefinition::sdSize) ? sdFooter.size : 9,
	//                           ptDpi.y, 72);
	int footerLineHeight = ::MulDiv(8, ptDpi.y, 72);
	HFONT fontFooter = ::CreateFont(footerLineHeight,
	                                0, 0, 0,
	                                /*sdFooter.bold ? FW_BOLD : FW_NORMAL*/FW_NORMAL,
	                                /*sdFooter.italics*/FALSE,
	                                /*sdFooter.underlined*/FALSE,
	                                0, 0, 0,
	                                0, 0, 0,
	                                /*(sdFooter.specified & StyleDefinition::sdFont) ? sdFooter.font.c_str() :*/ "Arial");
	
	::SelectObject(hdc, fontFooter);
	::GetTextMetrics(hdc, &tm);
	footerLineHeight = tm.tmHeight + tm.tmExternalLeading;

	DOCINFO di = {sizeof(DOCINFO), 0, 0, 0, 0};
	di.lpszDocName = GetDocTitle() /*windowName.c_str()*/;
	di.lpszOutput = 0;
	di.lpszDatatype = 0;
	di.fwType = 0;
	if (::StartDoc(hdc, &di) < 0) {
		//SString msg = LocaliseMessage("Can not start printer document.");
		//MessageBox(MainHWND(), msg.c_str(), 0, MB_OK);
		return;
	}

	LONG lengthDoc = SPerform(SCI_GETLENGTH);
	LONG lengthDocMax = lengthDoc;
	LONG lengthPrinted = 0;

	// Requested to print selection
	if (pdlg.Flags & PD_SELECTION) {
		if (startPos > endPos) {
			lengthPrinted = endPos;
			lengthDoc = startPos;
		} else {
			lengthPrinted = startPos;
			lengthDoc = endPos;
		}

		if (lengthPrinted < 0)
			lengthPrinted = 0;
		if (lengthDoc > lengthDocMax)
			lengthDoc = lengthDocMax;
	}

	// We must substract the physical margins from the printable area
	RangeToFormat frPrint;
	frPrint.hdc = hdc;
	frPrint.hdcTarget = hdc;
	frPrint.rc.left = rectMargins.left - rectPhysMargins.left;
	frPrint.rc.top = rectMargins.top - rectPhysMargins.top;
	frPrint.rc.right = ptPage.x - rectMargins.right - rectPhysMargins.left;
	frPrint.rc.bottom = ptPage.y - rectMargins.bottom - rectPhysMargins.top;
	frPrint.rcPage.left = 0;
	frPrint.rcPage.top = 0;
	frPrint.rcPage.right = ptPage.x - rectPhysMargins.left - rectPhysMargins.right - 1;
	frPrint.rcPage.bottom = ptPage.y - rectPhysMargins.top - rectPhysMargins.bottom - 1;
	
	///@todo header format...
	if (/*headerFormat.size()*/0) {
		frPrint.rc.top += headerLineHeight + headerLineHeight / 2;
	}
	if (/*footerFormat.size()*/0) {
		frPrint.rc.bottom -= footerLineHeight + footerLineHeight / 2;
	}

	// Print each page
	int pageNum = 1;
	bool printPage;
	
	///@todo setup scintilla for printing...
	//PropSet propsPrint;
	//propsPrint.superPS = &props;
	//SetFileProperties(propsPrint);

	while (lengthPrinted < lengthDoc) 
	{
		printPage = (!(pdlg.Flags & PD_PAGENUMS) ||
		             (pageNum >= pdlg.nFromPage) && (pageNum <= pdlg.nToPage));

		char pageString[32];
		sprintf(pageString, "%0d", pageNum);
		
		//propsPrint.Set("CurrentPage", pageString);

		if (printPage) 
		{
			::StartPage(hdc);

			///@todo headerFormat
			if (/*headerFormat.size()*/0) 
			{
				//SString sHeader = propsPrint.GetExpanded("print.header.format");
				string sHeader = _T("Header");
				::SetTextColor(hdc, /*sdHeader.fore.AsLong()*/0);
				::SetBkColor(hdc, /*sdHeader.back.AsLong()*/RGB(255,255,255));
				::SelectObject(hdc, fontHeader);
				UINT ta = ::SetTextAlign(hdc, TA_BOTTOM);
				RECT rcw = {frPrint.rc.left, frPrint.rc.top - headerLineHeight - headerLineHeight / 2,
				            frPrint.rc.right, frPrint.rc.top - headerLineHeight / 2};
				rcw.bottom = rcw.top + headerLineHeight;
				::ExtTextOut(hdc, frPrint.rc.left + 5, frPrint.rc.top - headerLineHeight / 2,
				             ETO_OPAQUE, &rcw, sHeader.c_str(),
				             static_cast<int>(sHeader.length()), NULL);
				::SetTextAlign(hdc, ta);
				HPEN pen = ::CreatePen(0, 1, /*sdHeader.fore.AsLong()*/0);
				HPEN penOld = static_cast<HPEN>(::SelectObject(hdc, pen));
				::MoveToEx(hdc, frPrint.rc.left, frPrint.rc.top - headerLineHeight / 4, NULL);
				::LineTo(hdc, frPrint.rc.right, frPrint.rc.top - headerLineHeight / 4);
				::SelectObject(hdc, penOld);
				::DeleteObject(pen);
			}
		}

		frPrint.chrg.cpMin = lengthPrinted;
		frPrint.chrg.cpMax = lengthDoc;

		lengthPrinted = SPerform(SCI_FORMATRANGE,
		                         printPage,
		                         reinterpret_cast<LPARAM>(&frPrint));

		if (printPage) 
		{
			///@todo footerFormat
			if (/*footerFormat.size()*/0) 
			{
				//SString sFooter = propsPrint.GetExpanded("print.footer.format");
				string sFooter = _T("Footer");
				::SetTextColor(hdc, /*sdFooter.fore.AsLong()*/0);
				::SetBkColor(hdc, /*sdFooter.back.AsLong()*/RGB(255,255,255));
				::SelectObject(hdc, fontFooter);
				UINT ta = ::SetTextAlign(hdc, TA_TOP);
				RECT rcw = {frPrint.rc.left, frPrint.rc.bottom + footerLineHeight / 2,
				            frPrint.rc.right, frPrint.rc.bottom + footerLineHeight + footerLineHeight / 2};
				::ExtTextOut(hdc, frPrint.rc.left + 5, frPrint.rc.bottom + footerLineHeight / 2,
				             ETO_OPAQUE, &rcw, sFooter.c_str(),
				             static_cast<int>(sFooter.length()), NULL);
				::SetTextAlign(hdc, ta);
				HPEN pen = ::CreatePen(0, 1, /*sdFooter.fore.AsLong()*/0);
				HPEN penOld = static_cast<HPEN>(::SelectObject(hdc, pen));
				::SetBkColor(hdc, /*sdFooter.fore.AsLong()*/0);
				::MoveToEx(hdc, frPrint.rc.left, frPrint.rc.bottom + footerLineHeight / 4, NULL);
				::LineTo(hdc, frPrint.rc.right, frPrint.rc.bottom + footerLineHeight / 4);
				::SelectObject(hdc, penOld);
				::DeleteObject(pen);
			}

			::EndPage(hdc);
		}
		pageNum++;

		if ((pdlg.Flags & PD_PAGENUMS) && (pageNum > pdlg.nToPage))
			break;
	}

	SPerform(SCI_FORMATRANGE, FALSE, 0);

	::EndDoc(hdc);
	::DeleteDC(hdc);
	if (fontHeader) {
		::DeleteObject(fontHeader);
	}
	if (fontFooter) {
		::DeleteObject(fontFooter);
	}
}