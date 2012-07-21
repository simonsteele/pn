/**
 * @file ScintillaImpl.cpp
 * @brief Implement further functionality for a scintilla wrapper.
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "ScintillaImpl.h"
#include "include/encoding.h"
#include "scaccessor.h"
#include "autocomplete.h"
#include "autocompletehandler.h"
#include "autocompletemanager.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

typedef boost::xpressive::basic_regex<ScintillaIterator> sciregex;
typedef boost::xpressive::match_results<ScintillaIterator> scimatch;
typedef boost::xpressive::sub_match<ScintillaIterator> scisub_match;


/**
 * Constructor
 */
CScintillaImpl::CScintillaImpl() :
	m_bAutoCompleteIgnoreCase(true)
{
	lastFindDetails.findPhrase = _T("");
	lastFindDetails.startPos = 0;
	lastFindDetails.direction = true;

	// TODO: This needs to be lazy loaded based on the current scheme, and swapped out on 
	// scheme change.
	//m_autoComplete = AutocompleteManager().GetAutocomplete(NULL);
}

CScintillaImpl::~CScintillaImpl()
{
}

////////////////////////////////////////////////////////////////////////////////
// Search and Replace Support Code
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

static int UnSlashAsNeeded(std::string& s, bool escapes, bool regularExpression)
{
	if (!s.size())
	{
		return 0;
	}

	std::vector<char> buf(s.length()+1);
	char *sUnslashed = &buf[0];
	strcpy(sUnslashed, &s[0]);

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

	return len;
}

////////////////////////////////////////////////////////////
// Manage all of the functionality in this class...
////////////////////////////////////////////////////////////

#define SHOWING_LINE_NUMBER (SPerform(SCI_GETMARGINWIDTHN,0)>0)

bool contains(const char *s, char ch) 
{
	return (s && *s) ? strchr(s, ch) != 0 : false;
}

bool iswordcharforsel(char ch) 
{
	return !strchr("\t\n\r !\"#$%&'()*+,-./:;<=>?@[\\]^`{|}~", ch);
}

int CScintillaImpl::HandleNotify(LPARAM lParam)
{
	int msg = CScintilla::HandleNotify(lParam);
	Scintilla::SCNotification* scn = reinterpret_cast<Scintilla::SCNotification*>(lParam);

	switch (msg)
	{
		case SCN_UPDATEUI:
		{
			ManageBraceMatch();
		}
		break;

		case SCN_MODIFIED:
		{
			if (scn->linesAdded && SHOWING_LINE_NUMBER)
			{
				SetLineNumberWidth();
			}

			break;
		}

		case SCN_ZOOM:
		{
			SetLineNumberWidth();
			break;
		}

		case SCN_CALLTIPCLICK: 
		{
			if (scn->position == 1 && m_nCurrentCallTip > 0) 
			{
				m_nCurrentCallTip--;
				FillFunctionDefinition();
			} 
			else if (scn->position == 2 && m_nCurrentCallTip + 1 < m_nMaxCallTips) 
			{
				m_nCurrentCallTip++;
				FillFunctionDefinition();
			}
			break;
		}

		case SCN_DWELLSTART: 
		{
			if (INVALID_POSITION == scn->position) 
			{
				//char message[200];
				//sprintf(message, "%0d (%0d,%0d)", scn->position, scn->x, scn->y);
			} 
			else 
			{
				int selStart = scn->position;
				int selEnd = scn->position;
				char sel[200];
				RangeExtendAndGrab(sel, 200, selStart, selEnd, GetLength(), iswordcharforsel);
				if (*sel)
					CallTipShow(scn->position, sel);				
			}
			break;
		}

		case SCN_DWELLEND:
		{
			CallTipCancel();
			break;
		}

		case SCN_AUTOCSELECTION:
		{
			if( m_autoCompleteHandler.get() )
			{
				if( m_autoCompleteHandler->AutoCSelection(scn) )
				{
					// Handler has done what they wanted to...
					AutoCCancel();
				}

				m_autoCompleteHandler.reset();
			}
		}
		break;

		case SCN_AUTOCCANCELLED:
			{
				m_autoCompleteHandler.reset();
			}
			break;

		case SCN_CHARADDED:
		{
			DumbIndent( ((Scintilla::SCNotification*)lParam)->ch );
			
			// Auto Close Tags:
			if( m_bSmartTag && scn->ch == '>' )
				SmartTag();

			Scintilla::CharacterRange crange = GetSelection();
			int selStart = crange.cpMin;
			int selEnd = crange.cpMax;
			
			if (!((selEnd == selStart) && (selStart > 0)))
				break;
			
			int style = GetStyleAt(selStart - 1);
			if (style == 1)
				break;
			
			if (CallTipActive()) 
			{
				if (contains(m_calltipParametersEnd.c_str(), scn->ch)) 
				{
					m_nBraceCount--;
					if (m_nBraceCount < 1)
						CallTipCancel();							
					else
						StartCallTip();
				} 
				else if (contains(m_calltipParametersStart.c_str(), scn->ch)) 
				{
					m_nBraceCount++;
					StartCallTip();
				} 
				else ContinueCallTip();													
			} 
			else if (AutoCActive()) 
			{
				if (contains(m_calltipParametersStart.c_str(), scn->ch)) 
				{
					m_nBraceCount++;
					StartCallTip();
				} 
				else if (contains(m_calltipParametersEnd.c_str(), scn->ch)) 
				{
					m_nBraceCount--;	
				} 
				else if (!contains(m_strWordCharacters.c_str(), scn->ch)) 
				{
					AutoCCancel();
					if (contains(m_autoCompleteStartCharacters.c_str(), scn->ch))
						StartAutoComplete(false);
				}
			}
			else 
			{
				if (contains(m_calltipParametersStart.c_str(), scn->ch)) 
				{
					m_nBraceCount = 1;
					StartCallTip();
				}
				else
				{
					if (m_bAutoActivate && contains(m_autoCompleteStartCharacters.c_str(), scn->ch))
						StartAutoComplete(false);
				}
			}
			break;
		}	
	}

	return msg;
}

////////////////////////////////////////////////////////////
// Word Count

#include "include/wordcounter.h"

template <int TBlockSize = 131072>
class ScintillaWordCounter : public WordCounter< ScintillaWordCounter<TBlockSize> >
{
	friend class WordCounter<ScintillaWordCounter>;

	public:
		ScintillaWordCounter(CScintilla* sc)
		{
			pScintilla = sc;
			length = pScintilla->GetLength();
			pos = 0;
			actualPos = 0;
		}
		
	protected:
		char getNextChar()
		{
			if(pos == 0)
			{
				int grabSize = length - actualPos;
				if (grabSize > TBlockSize)
					grabSize = TBlockSize;
				Scintilla::TextRange tr;
				tr.chrg.cpMin = actualPos;
				tr.chrg.cpMax = actualPos + grabSize;
				tr.lpstrText = buffer;
				pScintilla->GetTextRange(&tr);
			}
			int oldpos = pos;
			pos = (pos+1) % TBlockSize;
			return buffer[oldpos];
		}

		int getLength()
		{
			return length;
		}

		int length;
		int pos;
		int actualPos;
		CScintilla* pScintilla;
		char buffer[TBlockSize+1];
};

int CScintillaImpl::GetWordCount()
{
	ScintillaWordCounter<> counter(this);
	return counter.count();
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

void CScintillaImpl::DumbIndent(char ch)
{
	//CharacterRange cr;
	//GetSel(cr);

	if(ch == '\r' || ch == '\n')
	{
		int curLine = GetCurrentPos();
		curLine = LineFromPosition(curLine);

		int previousIndent = GetLineIndentation(curLine-1);
		if( previousIndent && GetLineIndentation(curLine) == 0 )
			IndentLine(curLine, previousIndent);
	}
}

void CScintillaImpl::IndentLine(int line, int indent)
{
	if (indent < 0)
		return;
	
	Scintilla::CharacterRange crange;
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
		char chAfter = GetCharAt(Caret);
		if(chAfter != NULL && strchr(braceset, chAfter))
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

int CScintillaImpl::FindNext(extensions::ISearchOptions* pOptions)
{
	Scintilla::CharacterRange	cr;
	int				startPosition, endPosition;
	int				bRet = fnNotFound;
	bool			checkFoundPos = true;

	std::string localFindText;

	// If we're in UTF-8 mode we have a go at making the correct find string
	// so that Windows characters get converted into UTF-8.
	if(GetCodePage() == SC_CP_UTF8)
	{
		Tcs_Utf8 conv(pOptions->GetFindText());
		localFindText = (const char*)(const unsigned char*)conv;
	}
	else
	{
		Tcs_Windows1252 conv(pOptions->GetFindText());
		localFindText = conv;
	}

	int lenFind = UnSlashAsNeeded(localFindText, pOptions->GetUseSlashes(), pOptions->GetUseRegExp());
	
	if(lenFind == 0)
		return fnInvalidSearch;
	
	///@todo Sort out interface accessibility
	pOptions->SetFound(false);

	GetSel(cr);

	///@todo SCFIND_WORDSTART
	int flags = (pOptions->GetMatchWholeWord() ? SCFIND_WHOLEWORD : 0) |
				(pOptions->GetMatchCase() ? SCFIND_MATCHCASE : 0) |
				(pOptions->GetUseRegExp() ? SCFIND_REGEXP : 0);

	///@todo set find pos on selchange/edit update
	if( lastFindDetails.findPhrase != pOptions->GetFindText() ||
		flags != lastFindDetails.flags ||
		!pOptions->GetSearchBackwards() != lastFindDetails.direction )
	{
		// Find text has changed, set the startPos value, 
		// and don't check to see if we wrapped this time.
		//lastFindDetails.startPos = GetCurrentPos();
		lastFindDetails.findPhrase = pOptions->GetFindText();
		lastFindDetails.flags = flags;
		lastFindDetails.result = fnNotFound;
		lastFindDetails.lastPos = -1;
		lastFindDetails.direction = !pOptions->GetSearchBackwards();
		checkFoundPos = false;
	}
	else
	{
		checkFoundPos = (lastFindDetails.lastPos == cr.cpMin);
	}

	const char* findtext = localFindText.c_str();

	if(pOptions->GetSearchBackwards())
	{
		startPosition = cr.cpMin - 1;
		endPosition = 0;
	}
	else
	{
		startPosition = cr.cpMax;
		endPosition = GetLength();
	}

	SetTargetStart(startPosition);
	SetTargetEnd(endPosition);
	SetSearchFlags(flags);
	
	int posFind = SearchInTarget(lenFind, findtext);

	if(posFind == -1 && pOptions->GetLoopOK())
	{
		if(pOptions->GetSearchBackwards())
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

	if (posFind != -1 && posFind != -2)
	{
		int start = GetTargetStart();
		int end = GetTargetEnd();
		if (checkFoundPos && lastFindDetails.result == fnFound &&
			lastFindDetails.startPos == start)
		{
			pOptions->SetFound(true);
			bRet = fnReachedStart;
		}
		else
		{
			EnsureRangeVisible(start, end);
			SetSel(start, end);
 			// ScrollCaret();
			pOptions->SetFound(true);
			bRet = fnFound;
		}
	}
	else if (posFind == -2)
	{
		bRet = fnInvalidRegex;
	}

	lastFindDetails.result = bRet;
	lastFindDetails.lastPos = posFind;
	if (checkFoundPos == false)
	{
		GetSel(cr);
		lastFindDetails.startPos = cr.cpMin;
	}

	return bRet;
}

int CScintillaImpl::FindAll(extensions::ISearchOptions* pOptions, MatchHandlerFn matchHandler)
{
	return FindAll(0, GetLength(), pOptions, matchHandler);
}

int CScintillaImpl::FindAll(int start, int end, extensions::ISearchOptions* pOptions, MatchHandlerFn matchHandler)
{
	std::string localFindText;

	// If we're in UTF-8 mode we have a go at making the correct find string
	// so that Windows characters get converted into UTF-8.
	if(GetCodePage() == SC_CP_UTF8)
	{
		Tcs_Utf8 conv(pOptions->GetFindText());
		localFindText = (const char*)(const unsigned char*)conv;
	}
	else
	{
		CT2CA conv(pOptions->GetFindText());
		localFindText = (const char*)conv;
	}

	int lenFind = UnSlashAsNeeded(localFindText, pOptions->GetUseSlashes(), pOptions->GetUseRegExp());
	
	if(lenFind == 0)
		return fnInvalidSearch;
	
	pOptions->SetFound(false);

	///@todo SCFIND_WORDSTART
	int flags = (pOptions->GetMatchWholeWord() ? SCFIND_WHOLEWORD : 0) |
				(pOptions->GetMatchCase() ? SCFIND_MATCHCASE : 0) |
				(pOptions->GetUseRegExp() ? SCFIND_REGEXP : 0);

	// Whole document
	int startPosition = start;
	int endPosition = end;
	
	SetTarget(startPosition, endPosition);
	SetSearchFlags(flags);

	int posFind = SearchInTarget(lenFind, localFindText.c_str());
	
	// Fix from Replace All Code:
	if ((lenFind == 1) && pOptions->GetUseRegExp() && (localFindText[0] == '^')) 
	{
		// Special case for find all start of line so it hits the first line
		posFind = startPosition;
		SetTarget(startPosition, startPosition);
	}

	if ((posFind != -1) && (posFind <= endPosition))
	{
		int lastMatch = posFind;

		while (posFind != -1) 
		{
			int lenTarget = GetTargetEnd() - GetTargetStart();
			
			lastMatch = posFind + lenTarget;

			matchHandler(GetTargetStart(), GetTargetEnd());
			
			// For the special cases of start of line and end of line
			// Something better could be done but there are too many special cases
			if (lenTarget <= 0)
			{
				lastMatch++;
			}

			if (lastMatch < endPosition)
			{
				SetTarget(lastMatch, endPosition);
				posFind = SearchInTarget(lenFind, localFindText.c_str());
			}
			else
			{
				// we hit document end
				posFind = -1;
			}
		}
	}

	return fnReachedStart;
}

bool CScintillaImpl::ReplaceOnce(extensions::ISearchOptions* pOptions)
{
	if(pOptions->GetFound()) 
	{
		//CT2CA conv(pOptions->GetReplaceText());
		//std::string replaceTarget(conv);
		std::string replaceTarget;
		// If we're in UTF-8 mode we have a go at making the correct find string
		// so that Windows characters get converted into UTF-8.
		if(GetCodePage() == SC_CP_UTF8)
		{
			Tcs_Utf8 conv(pOptions->GetReplaceText());
			replaceTarget = (const char*)(const unsigned char*)conv;
		}
		else
		{
			Tcs_Windows1252 conv(pOptions->GetReplaceText());
			replaceTarget = conv;
		}

		
		int replaceLen = UnSlashAsNeeded(replaceTarget, pOptions->GetUseSlashes(), pOptions->GetUseRegExp());
		
		Scintilla::CharacterRange cr;
		GetSel(cr);

		SetTarget(cr.cpMin, cr.cpMax);

		int lenReplaced = replaceLen;
		
		if (pOptions->GetUseRegExp())
		{
			lenReplaced = ReplaceTargetRE(replaceLen, replaceTarget.c_str());
		}
		else
		{
			ReplaceTarget(replaceLen, replaceTarget.c_str());
		}
		
		SetSel(cr.cpMin + lenReplaced, cr.cpMin);
		
		pOptions->SetFound(false);
	}

	return FindNext(pOptions) != 0;
}

int CScintillaImpl::ReplaceAll(extensions::ISearchOptions* pOptions) 
{
	int repCount = 0;

	//CT2CA findTargetConv(pOptions->GetFindText());
	//std::string findTarget(findTargetConv);
	std::string findTarget;
	
	// If we're in UTF-8 mode we have a go at making the correct find string
	// so that Windows characters get converted into UTF-8.
	if(GetCodePage() == SC_CP_UTF8)
	{
		Tcs_Utf8 conv(pOptions->GetFindText());
		findTarget = (const char*)(const unsigned char*)conv;
	}
	else
	{
		Tcs_Windows1252 conv(pOptions->GetFindText());
		findTarget = conv;
	}


	int findLen = UnSlashAsNeeded(findTarget, pOptions->GetUseSlashes(), pOptions->GetUseRegExp());
	if (findLen == 0)
	{
		/*SString msg = LocaliseMessage(
			inSelection ?
			"Find string must not be empty for 'Replace in Selection' command." :
			"Find string must not be empty for 'Replace All' command.");
		FindMessageBox(msg);*/
		return 0;
	}

	Scintilla::CharacterRange cr;
	GetSel(cr);

	int startPosition = cr.cpMin;
	int endPosition = cr.cpMax;
	
	if (pOptions->GetReplaceInSelection()) 
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
		if (pOptions->GetLoopOK()) 
		{
			// Whole document
			startPosition = 0;
		}
		// If not looping, replace all only from caret to end of document
	}

	//CT2CA replaceTextConv(pOptions->GetReplaceText());
	//std::string replaceTarget(replaceTextConv);
	std::string replaceTarget;

	// If we're in UTF-8 mode we have a go at making the correct find string
	// so that Windows characters get converted into UTF-8.
	if(GetCodePage() == SC_CP_UTF8)
	{
		Tcs_Utf8 conv(pOptions->GetReplaceText());
		replaceTarget = (const char*)(const unsigned char*)conv;
	}
	else
	{
		Tcs_Windows1252 conv(pOptions->GetReplaceText());
		replaceTarget = conv;
	}

	int replaceLen = UnSlashAsNeeded(replaceTarget, pOptions->GetUseSlashes(), pOptions->GetUseRegExp());
	
	int flags = (pOptions->GetMatchWholeWord() ? SCFIND_WHOLEWORD : 0) |
	            (pOptions->GetMatchCase() ? SCFIND_MATCHCASE : 0) |
	            (pOptions->GetUseRegExp() ? SCFIND_REGEXP : 0);
	
	SetTarget(startPosition, endPosition);
	SetSearchFlags(flags);
	
	int posFind = SearchInTarget(findLen, findTarget.c_str());
	
	if ((findLen == 1) && pOptions->GetUseRegExp() && (findTarget[0] == '^')) 
	{
		// Special case for replace all start of line so it hits the first line
		posFind = startPosition;
		SetTarget(startPosition, startPosition);
	}
	
	if ((posFind != -1) && (posFind != -2) && (posFind <= endPosition)) 
	{
		int lastMatch(posFind);
		BeginUndoAction();

		while (posFind != -1) 
		{
			int lenTarget = GetTargetEnd() - GetTargetStart();
			
			// See if the next character is an end-of-line if we have a zero-length replace target.
			int movePastEOL = 0;				
			int lenReplaced = replaceLen;
			if (pOptions->GetUseRegExp())
			{
				lenReplaced = ReplaceTargetRE(replaceLen, replaceTarget.c_str());
			}
			else
			{
				ReplaceTarget(replaceLen, replaceTarget.c_str());
			}

			repCount += 1 * (lenReplaced != 0 || lenTarget != 0);

			if (lenTarget <= 0)
			{
				bool zeroLengthRepeatedFind = (posFind == lastMatch) && (lenReplaced == 0);

				char chNext = static_cast<char>(GetCharAt(GetTargetEnd()));
				if (zeroLengthRepeatedFind || (chNext == '\r' || chNext == '\n'))
				{
					movePastEOL = 1;
				}

				if (pOptions->GetUseRegExp() && findTarget.find('^') == -1 && findTarget.find('$') == -1)
				{
					// Trying to avoid infinite loops here - looks like a zero-length target
					// and it's not start or end of line. This may loop forever so we kill it here.
					g_Context.m_frame->SetStatusText(LS(IDS_AVOIDINFINITESEARCH));
					break;
				}
			}

			// Modify for change caused by replacement
			endPosition += lenReplaced - lenTarget;
			
			// For the special cases of start of line and end of line
			// Something better could be done but there are too many special cases
			lastMatch = posFind + lenReplaced + movePastEOL;
			
			if( lastMatch >= endPosition )
			{
				// With an empty match we might have added one too
				// many to avoid reg-ex replace freezes.
				posFind = -1;
			}
			else
			{
				SetTarget(lastMatch, endPosition);
				posFind = SearchInTarget(findLen, findTarget.c_str());
			}
		}

		if(!pOptions->GetNoCursorMove())
		{
			if (pOptions->GetReplaceInSelection())
				SetSel(startPosition, endPosition);
			else
				SetSel(lastMatch, lastMatch);
		}
		
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

class PrintFormatStringBuilder : public CustomFormatStringBuilder<PrintFormatStringBuilder>
{
	typedef CustomFormatStringBuilder<PrintFormatStringBuilder> baseClass;
public:
	PrintFormatStringBuilder(LPCTSTR fileName) 
		: fi(fileName)
	{
		CFileName fn(fileName);
		filename = fn.GetFileName();
		filepath = fn.GetPath();
	}

	const tstring& Build(LPCTSTR str, int nPage, int nPages)
	{
		page = nPage;
		pages = nPages;
		return baseClass::Build(str);
	}

	void OnFormatChar(TCHAR thechar)
	{
		switch(thechar)
		{
		case _T('f'):
			m_string += filename;
			break;
		case _T('p'):
			// current page
			m_string += IntToTString(page).c_str();
			break;
		case _T('P'):
			// page count
			m_string += IntToTString(pages).c_str();
			break;
		case _T('d'):
			// path
			m_string += filepath;
			break;
		case _T('D'):
			// file date
			m_string += fi.FileDate;
			break;
		case _T('c'):
			// current date
			m_string += dti.CurrentDate;
			break;
		case _T('t'):
			// current time
			m_string += dti.CurrentTime;
			break;
		case _T('T'):
			// file time
			m_string += fi.FileTime;
			break;
		case _T('u'):
			m_string += ui.UserName;
			break;
		}
	}

protected:
	DateTimeInformation dti;
	FileInformation fi;
	UserInformation ui;
	tstring filename;
	tstring filepath;
	int page;
	int pages;
};

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
	Scintilla::CharacterRange crange;
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

		TCHAR localeInfo[3];
		GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IMEASURE, localeInfo, 3);

		if (localeInfo[0] == _T('0'))
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
	tstring headerFormat = pOptions->Header;
	tstring footerFormat = pOptions->Footer;
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
	                                /*(sdHeader.specified & StyleDefinition::sdFont) ? sdHeader.font.c_str() :*/ _T("Arial"));
	
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
	                                /*(sdFooter.specified & StyleDefinition::sdFont) ? sdFooter.font.c_str() :*/ _T("Arial"));
	
	::SelectObject(hdc, fontFooter);
	::GetTextMetrics(hdc, &tm);
	footerLineHeight = tm.tmHeight + tm.tmExternalLeading;

	tstring docTitle(GetDocTitle());

	DOCINFO di = {sizeof(DOCINFO), 0, 0, 0, 0};
	di.lpszDocName = docTitle.c_str();
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
	if (headerFormat.size()) {
		frPrint.rc.top += headerLineHeight + headerLineHeight / 2;
	}
	if (footerFormat.size()) {
		frPrint.rc.bottom -= footerLineHeight + footerLineHeight / 2;
	}

	// Print each page
	int pageNum = 1;
	bool printPage;
	
	PrintFormatStringBuilder fb( pOptions->Filename.c_str() );

	while (lengthPrinted < lengthDoc) 
	{
		printPage = (!(pdlg.Flags & PD_PAGENUMS) ||
		             (pageNum >= pdlg.nFromPage) && (pageNum <= pdlg.nToPage));

		if( printPage )
		{
			::StartPage(hdc);

			if( headerFormat.size() ) 
			{
				tstring sHeader = fb.Build(headerFormat.c_str(), pageNum, 0);
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

		if( printPage )
		{
			if(footerFormat.size())
			{
				tstring sFooter = fb.Build(footerFormat.c_str(), pageNum, 0);
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

bool CScintillaImpl::UnCommentLine(const CommentSpecRec& comments, int line)
{
	int indentPos = GetLineIndentPosition(line);
	ScintillaAccessor sa(this);
	bool match(true);

	for(size_t i(0); i < strlen(comments.CommentLineText); ++i)
	{
		if(sa.SafeGetCharAt(indentPos+i, '\0') != comments.CommentLineText[i])
		{
			match = false;
			break;
		}
	}

	if(match)
	{
		BeginUndoAction();

		// The line starts with a comment line bit, uncomment it...
		SetTargetStart(indentPos);
		SetTargetEnd(indentPos + strlen(comments.CommentLineText));
		ReplaceTarget(0, "");

		EndUndoAction();

		return true;
	}

	return false;
}

bool CScintillaImpl::UnCommentLine(const CommentSpecRec& comments)
{
	Scintilla::CharacterRange cr;
	GetSel(cr);

	return UnCommentLine(comments, LineFromPosition(cr.cpMin));
}

bool CScintillaImpl::UnCommentStream(const CommentSpecRec& comments)
{
	Scintilla::CharacterRange cr;
	GetSel(cr);

	// Sanity checks...
	if(cr.cpMin + (int)strlen(comments.CommentStreamStart) > cr.cpMax)
		return false;
	if(cr.cpMin + (int)strlen(comments.CommentStreamStart) + (int)strlen(comments.CommentStreamEnd) > cr.cpMax)
		return false;

	bool match = true;

	ScintillaAccessor sa(this);
	for(size_t i(0); i < strlen(comments.CommentStreamStart); ++i)
	{
		if(sa.SafeGetCharAt(cr.cpMin+i, '\0') != comments.CommentStreamStart[i])
		{
			match = false;
			break;
		}
	}

	if(!match)
		return false;

	size_t endstart = cr.cpMax - strlen(comments.CommentStreamEnd);
	for(size_t i(0); i < strlen(comments.CommentStreamEnd); ++i)
	{
		if(sa.SafeGetCharAt(endstart + i, '\0') != comments.CommentStreamEnd[i])
		{
			match = false;
			break;
		}
	}

	if(!match)
		return false;

	// We've matched a comment stream...
	BeginUndoAction();

	SetTargetStart(cr.cpMin);
	SetTargetEnd(cr.cpMin + strlen(comments.CommentStreamStart));
	ReplaceTarget(0, NULL);

	cr.cpMax -= strlen(comments.CommentStreamStart);
	SetTargetStart(cr.cpMax-strlen(comments.CommentStreamEnd));
	SetTargetEnd(cr.cpMax);
	ReplaceTarget(0, NULL);

	EndUndoAction();

	return true;
}

//*****************************************************************************
//* Code added by Manuel Sandoval webmailusr-msn@yahoo.com
//* Adapted from anyedit's ScntillaEx.h and ScintillaEx.cpp (www.anyedit.org)
//* Support for autocomplete
//*****************************************************************************

#include "SchemeConfig.h"

inline int CompareNoCase(std::string &a, std::string& b)
{
	return _stricmp(a.c_str(), b.c_str());
}

/**
 * Initialise Autocomplete functionality
 */
void CScintillaImpl::InitAutoComplete(Scheme* sch)
{					
	/*General Settings Initialization*/	
	m_pScheme = sch;
	m_autoComplete = m_autoCompleteManager->GetAutocomplete(m_pScheme->GetName());
	m_bAutoCompletion = OPTIONS->GetCached(Options::OAutoComplete) != FALSE;
	m_bSmartTag = OPTIONS->GetCached(Options::OAutoCompleteTags) != FALSE;
	m_bAutoCompletionUseTags = OPTIONS->GetCached(Options::OAutoCompleteUseTags) != FALSE;
	m_nMinAutoCompleteChars = OPTIONS->GetCached(Options::OAutoCompleteStartChars);
	m_bAutoActivate = OPTIONS->GetCached(Options::OAutoCompleteActivation) == eacTextMatch;
	m_nBraceCount = 0;
	m_nStartCalltipWord = 0;
	m_nCurrentCallTip = 0;
	m_lastPosCallTip = 0;
	m_nMaxCallTips = 256;
	m_currentCallTipWord = "";
	m_functionDefinition = "";
	
	/*These parameters are adjusted to most languages but might be necessary to set them to
	 *Specific languages - I don't know which, but Schema info should be enough!*/
	m_calltipParametersEnd = ")";
	m_calltipParametersStart = "(";	
	m_calltipParametersSeparators = ",;";
	m_strWordCharacters = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
	m_autoCompleteStartCharacters = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
	m_calltipWordCharacters = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";	

	/*These parameters are language specific:*/	
	m_bCallTipIgnoreCase = m_bAutoCompleteIgnoreCase = true;
	AutoCSetIgnoreCase(m_bAutoCompleteIgnoreCase);
			
	ResetAutoComplete();
}

/**
 * This is called when keywords are loaded into scintilla, we
 * intercept it to get the keywords for autocompletion
 */
void CScintillaImpl::SetKeyWords(int keywordSet, const char* keyWords)
{
	if (OPTIONS->GetCached(Options::OAutoComplete) && m_autoComplete.get())
	{
		m_autoComplete->RegisterKeyWords(keywordSet, keyWords);
	}

	// Call base class, send those words to Scintilla!
	CScintilla::SetKeyWords(keywordSet, keyWords);
}

/**
 * User wants to try to autocomplete the current phrase
 */
void CScintillaImpl::AttemptAutoComplete()
{
	if(m_bAutoCompletion)
		StartAutoComplete(true);
}

/**
 * Set a handler for the autocompletion stuff
 */
void CScintillaImpl::SetAutoCompleteHandler(AutoCompleteHandlerPtr& handler)
{
	m_autoCompleteHandler = handler;
}

void CScintillaImpl::SetAutoCompleteManager(AutoCompleteManager* autoComplete)
{
	m_autoCompleteManager = autoComplete;
}

//This function adds to keywords functions from CTags
void CScintillaImpl::AddToAutoComplete(CString FullTag, CString TagName)
{	
	if(!m_bAutoCompletionUseTags)
		return;

	// TODO: This should not use CStrings...
	CT2CA tag(FullTag);
	CT2CA tagName(TagName);
	m_autoComplete->RegisterTag(tag, tagName);
}

//This function cleans CTags Keywords from autocomplete list
void CScintillaImpl::ResetAutoComplete()
{
	if (m_autoComplete.get())
	{
		m_autoComplete->ResetTags();
	}
}

void CScintillaImpl::ClearAutoComplete()
{
	if (m_autoComplete.get())
	{
		m_autoComplete->Reset();
	}
}

std::string CScintillaImpl::GetLineText(int nLine)
{	
	std::string strLine;	
	// Get needed buffer size
	int nLen;
	if (nLine < 0)
		nLen = GetCurLine(0, 0);
	else
		nLen = GetLine(nLine, 0);

	// Allocate buffer
	strLine.resize(nLen + 1);
	char* linebuf = &strLine[0];
	
	// And get the line
	if (nLine < 0)
		GetCurLine(nLen, linebuf);
	else 
		GetLine(nLine, linebuf);
	
	linebuf[nLen] = '\0';
	strLine.resize(nLen);
	
	return strLine;
}

ScintillaIterator CScintillaImpl::begin()
{
	return ScintillaIterator(this, 0);
}

ScintillaIterator CScintillaImpl::end()
{
	return ScintillaIterator(this, GetLength());
}

void CScintillaImpl::SmartTag() //Autocompletes <htmltags> with </htmltags>
{
	long lCurrentPos = GetCurrentPos();
	// If it was an empty tag we don't have a closing tag.
	if ('/' == GetCharAt( lCurrentPos - 2 )) 
		return;
	
	bool html(false);

	if (m_pScheme != NULL)
	{
		LPCSTR lexer = m_pScheme->GetLexer();

		html = strcmp(lexer, "hypertext") == 0;
		if(strcmp(lexer, "xml") != 0 
			&& strcmp(lexer, "hypertext") != 0
			&& strcmp(lexer, "php") != 0
			&& !html)
			return;
	}

	// Only complete in styles 1-8, excluding 5, these are all styles occurring inside a tag:
	int closeTagStyle = GetStyleAt(lCurrentPos - 2);
	if (!(closeTagStyle >= 1 && closeTagStyle <= 8 && closeTagStyle != 5))
	{
		return;
	}

	// Control balanced <> tags: for example:
	// <applet><parameter></parameter></applet> is balanced
	// <applet>><parameter></parameter></applet> is NOT balanced
	// balanced ++ when finding <; balanced-- when finding >
	int balanced = 0;

	long lPos = lCurrentPos - 1;
	long lLastGt(lCurrentPos);
	while (lPos >= 0)
	{
		int cChar = GetCharAt( lPos );
		if ('<' != cChar)
		{		
			if (cChar == '>')
			{
				balanced--; // Balance <>:
				lLastGt = lPos + 1;
			}
			
			lPos--; 
		}
		else 
		{
			balanced++; // Balance <>:
			if (balanced != 0)
			{
				break; // only continue when paired <>
			}
			
			// If this is a closing tag, xml comment, asp tag or processing instruction skip this action.
			char ch1 = GetCharAt( lPos + 1 );
			if ('/' == ch1 || '?' == ch1 || '%' == ch1 || '!' == ch1)
			{
				break;
			}

			if (lCurrentPos - lPos < 1024)
			{
				std::string text = GetTextRange(lPos, lLastGt);
				if (text == "<>")
				{
					break;
				}
				
				size_t pos = text.find(' ');
				if( std::string::npos == pos )
				{
					text.insert(1, "/");
				}
				else
				{
					text = text.substr(0, pos);
					text.insert(1, "/");
					text += ">";
				}

				if (html)
				{
					// Don't close non-close tags in HTML:
					if (stricmp(text.c_str(), "</br>") == 0 ||
						stricmp(text.c_str(), "</img>") == 0 ||
						stricmp(text.c_str(), "</hr>") == 0)
					{
						break;
					}
				}
				
				BeginUndoAction();
				SetTarget(lCurrentPos,lCurrentPos);
				ReplaceTarget(text.size(), text.c_str());
				EndUndoAction();
				break;
			}						
			else break; //Manuel Sandoval: Added this: when the buffer is too long (>1024), don't do insertion.			
		}		
	}
}

void CScintillaImpl::InsertChar(long nPos, char nChar)
{
	char chs[2];
	chs[0] = nChar;
	chs[1] = 0;
	InsertText(nPos, chs);
}

int CScintillaImpl::GetCaretInLine()
{
	int caret = GetCurrentPos();
	int line = LineFromPosition(caret);
	int lineStart = PositionFromLine(line);
	return caret - lineStart;
}

Scintilla::CharacterRange CScintillaImpl::GetSelection()
{
	Scintilla::CharacterRange crange;
	crange.cpMin = GetSelectionStart();
	crange.cpMax = GetSelectionEnd();
	return crange;
}

void CScintillaImpl::SetLineNumberWidth()
{
	if (SHOWING_LINE_NUMBER)
	{
		int lineNumWidth;

		// The margin size will be expanded if the current buffer's maximum
		// line number would overflow the margin.
		int lineCount = GetLineCount();
		lineNumWidth = 1;
		while (lineCount >= 10)
		{
			lineCount /= 10;
			++lineNumWidth;
		}
//		if (lineNumWidth < lineNumbersWidth)lineNumWidth = lineNumbersWidth;
		// Use a min width of 4 chars.
		lineNumWidth = max(lineNumWidth, 4);
		// The 4 here allows for spacing: 1 pixel on left and 3 on right.
		int pixelWidth = 4 + lineNumWidth * TextWidth(STYLE_LINENUMBER, "9");
		SetMarginWidthN(0, pixelWidth);
	}
	else SetMarginWidthN(0, 0);		
}

bool CScintillaImpl::StartCallTip()
{
	m_nCurrentCallTip = 0;
	m_currentCallTipWord.clear();
	std::string strLine = GetLineText();
	int current = GetCaretInLine();
	int pos = GetCurrentPos();
	int braces;
	
	do 
	{
		braces = 0;
		while (current > 0 && (braces || !contains(m_calltipParametersStart.c_str(), strLine[current - 1]))) 
		{
			if (contains(m_calltipParametersStart.c_str(), strLine[current - 1]))
				braces--;
			else if (contains(m_calltipParametersEnd.c_str(), strLine[current - 1]))
				braces++;
			current--;
			pos--;
		}
		
		if (current > 0)
		{
			current--;
			pos--;
		}
		else 
			break;
		
		while (current > 0 && isspace(strLine[current - 1]))
		{
			current--;
			pos--;
		}
	} 
	while (current > 0 && !contains(m_calltipWordCharacters.c_str(), strLine[current - 1]));
	
	if (current <= 0)
		return true;

	m_nStartCalltipWord = current - 1;
	while (m_nStartCalltipWord > 0 &&
	        contains(m_calltipWordCharacters.c_str(), strLine[m_nStartCalltipWord - 1])) 
	{
		m_nStartCalltipWord--;
	}

	strLine.resize(current);
	m_currentCallTipWord = strLine.substr(m_nStartCalltipWord);
	m_functionDefinition.clear();
	
	FillFunctionDefinition(pos);
	
	return true;
}

void CScintillaImpl::ContinueCallTip()
{
	std::string strLine = GetLineText();
	int current = GetCaretInLine();
	const char* functionDefinition = m_functionDefinition.c_str();

	int braces = 0;
	int commas = 0;
	for (int i = m_nStartCalltipWord; i < current; i++) 
	{
		if (contains(m_calltipParametersStart.c_str(), strLine[i]))	
			braces++;
		else if (contains(m_calltipParametersEnd.c_str(), strLine[i]) && braces > 0)
			braces--;
		else if (braces == 1 && contains(m_calltipParametersSeparators.c_str(), strLine[i]))
			commas++;			
	}

	int startHighlight = 0;
	while (functionDefinition[startHighlight] && !contains(m_calltipParametersStart.c_str(), m_functionDefinition[startHighlight]))
		startHighlight++;
		
	if (contains(m_calltipParametersStart.c_str(), functionDefinition[startHighlight]))
		startHighlight++;
		
	while (functionDefinition[startHighlight] && commas > 0) 
	{
		if (contains(m_calltipParametersSeparators.c_str(), functionDefinition[startHighlight]))
			commas--;
		// If it reached the end of the argument list it means that the user typed in more
		// arguments than the ones listed in the calltip
		if (contains(m_calltipParametersEnd.c_str(), functionDefinition[startHighlight]))
			commas = 0;
		else 
			startHighlight++;
	}
	if (contains(m_calltipParametersSeparators.c_str(), functionDefinition[startHighlight]))
		startHighlight++;
	int endHighlight = startHighlight;
	while (functionDefinition[endHighlight] && !contains(m_calltipParametersSeparators.c_str(), functionDefinition[endHighlight]) && !contains(m_calltipParametersEnd.c_str(), functionDefinition[endHighlight]))
		endHighlight++;
		
//	SendEditor(SCI_CALLTIPSETHLT, startHighlight, endHighlight);
	CallTipSetHlt(startHighlight, endHighlight);
_RPT2(_CRT_WARN,"---CallTipSetHlt %i, %i\n",startHighlight,endHighlight);
}

unsigned int LengthWord(const char *word, char otherSeparator) 
{
	// Find a '('. If that fails go to the end of the string.
	const char *endWord = strchr(word, '(');
	if (!endWord && otherSeparator) endWord = strchr(word, otherSeparator);
	if (!endWord) endWord = word + strlen(word);
	// Last case always succeeds so endWord != 0
	// Drop any space characters.
	if (endWord > word) 
	{
		endWord--;	// Back from the '(', otherSeparator, or '\0'
		// Move backwards over any spaces
//		while ((endWord > word) && (IsASpace(*endWord))) {
		while ((endWord > word) && (isspace(*endWord))) endWord--;			
	}
	return endWord - word;
}

void CScintillaImpl::FillFunctionDefinition(int pos)
{
	if (pos > 0)
	{
		m_lastPosCallTip = pos;
	}

	PN::AString callTipList;
	char FNC_SEPARATOR = (char)0x01;
	std::string seps;
	seps += FNC_SEPARATOR;

	m_autoComplete->GetPrototypes(callTipList, FNC_SEPARATOR, m_currentCallTipWord.c_str(), m_currentCallTipWord.length());

	m_Api.clear();
	std::string strCallTipList(callTipList);
	StringTokenise(strCallTipList, m_Api, seps);
	
	if (m_Api.size() > 0 && (static_cast<size_t>(m_nCurrentCallTip) < m_Api.size()))
	{
		m_functionDefinition = "";

		if (m_nMaxCallTips > 1 && m_Api.size() > 1)
		{
			char nofnbuf[100];
			sprintf(nofnbuf, "\001%d of %d\002 ", m_nCurrentCallTip+1, m_Api.size());
			m_functionDefinition = nofnbuf;
		}

		// Should get current api definition
		m_functionDefinition += m_Api[m_nCurrentCallTip];

		CallTipShow(m_lastPosCallTip - m_currentCallTipWord.size(), m_functionDefinition.c_str());
_RPT2(_CRT_WARN,"2---CallTipShow %i, [%s]\n",m_lastPosCallTip - m_currentCallTipWord.size(), m_functionDefinition);
		ContinueCallTip();		
	}
}

bool CScintillaImpl::StartAutoComplete(bool bForcefully)
{
	if( !m_bAutoCompletion )
		return false;

	std::string line = GetLineText();
	int current = GetCaretInLine();
	int startword = current;
	while ((startword > 0) 
		&& (contains(m_calltipWordCharacters.c_str(), line[startword - 1]) 
		  || contains(m_autoCompleteStartCharacters.c_str(), line[startword - 1])))
	{
		startword--;
	}

	if(!bForcefully && m_bAutoActivate && !(current - startword >= m_nMinAutoCompleteChars))
		return false;

	std::string root = line.substr(startword, current - startword);
	PN::AString autoCompleteList;
	m_autoComplete->GetWords(autoCompleteList, root.c_str(), root.length());
	if(autoCompleteList.GetLength())
	{
		AutoCShow(root.length(), autoCompleteList.Get());
	}

	return true;
}

void CScintillaImpl::RangeExtendAndGrab(
    char *sel,  ///< Buffer receiving the result.
    int len,    ///< Size of the buffer.
    int &selStart,
    int &selEnd,
    int lengthDoc,
    bool (*ischarforsel)(char ch),  ///< Function returning @c true if the given char. is part of the selection.
	bool stripEol /*=true*/)
{
	if (selStart == selEnd && ischarforsel)
	{
		// Try and find a word at the caret
		while ((selStart > 0) && (ischarforsel(GetCharAt(selStart - 1))))
			selStart--;
		while ((selEnd < lengthDoc) && (ischarforsel(GetCharAt(selEnd))))
			selEnd++;
	}
	sel[0] = '\0';
	if (selEnd - selStart + 1 > len - 1)
		selEnd = selStart + len - 1;
	
	if (selStart < selEnd)
		GetRange(selStart, selEnd, sel);	
	
	if (stripEol)
	{
		// Change whole line selected but normally end of line characters not wanted.
		// Remove possible terminating \r, \n, or \r\n.
		size_t sellen = strlen(sel);
		if (sellen >= 2 && (sel[sellen - 2] == '\r' && sel[sellen - 1] == '\n'))
			sel[sellen - 2] = '\0';
		else if (sellen >= 1 && (sel[sellen - 1] == '\r' || sel[sellen - 1] == '\n'))
			sel[sellen - 1] = '\0';
	}
}