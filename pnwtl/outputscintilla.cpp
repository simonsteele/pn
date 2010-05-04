/**
 * @file outputscintilla.cpp
 * @brief Simple RegEx based output lexer wrapped in a scintilla.
 * @author Simon Steele
 * @note Copyright (c) 2005-2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "outputscintilla.h"

#include "scaccessor.h"
#include "third_party/scintilla/include/scilexer.h"

using namespace boost::xpressive;

REScintilla::REScintilla()
{
	schemeLoaded = false;
	m_pRE = NULL;
}

REScintilla::~REScintilla()
{
	if(m_pRE)
	{
		delete m_pRE;
	}
}

void REScintilla::SetRE(LPCSTR regex, bool bClearStyling)
{
	// First of all build up the regular expression to use.
	CToolREBuilder builder;
	
	CA2CT regext(regex);
	tstring result = builder.Build(regext);
	CT2CA regexa(result.c_str());
	m_customre = regexa;
	
	/*if(m_pRE)
	{
		delete m_pRE;
		m_pRE = NULL;
	}*/

	if(!m_pRE)
	{
		m_pRE = new sregex;
	}

	try
	{
		// We pass 0 to disable the default UTF-8 matching, we're in ASCII in output window
		*m_pRE = sregex::compile(m_customre);

		g_Context.m_frame->SetStatusText(_T(""));
	}
	catch(boost::xpressive::regex_error& ex)
	{
		size_t len = strlen(ex.what()) + m_customre.size() + 90;
		TCHAR* buf = new TCHAR[len];
		buf[len - 1] = NULL;
		_sntprintf(buf, len - 1, _T("Custom Parser Error (%S): %S"), ex.what(), m_customre.c_str());
		g_Context.m_frame->SetStatusText(buf);
		delete [] buf;

		delete m_pRE;
		m_pRE = NULL;
	}

	if(!schemeLoaded)
	{
		// Now turn Scintilla into custom lex mode, first get styles from the output scheme.
		Scheme* pScheme = SchemeManager::GetInstance()->SchemeByName("output");
		if(pScheme && ::IsWindow(this->m_scihWnd))
		{
			pScheme->Load( *(static_cast<CScintilla*>(this)) );
			
			// Override some nastiness inherited from the default schemes...
			SPerform(SCI_SETCARETLINEVISIBLE, false);
			SPerform(SCI_SETEDGEMODE, EDGE_NONE);
		}

		// Switch to container-based lexing...
		SetLexer(SCLEX_CONTAINER);
	}

	if(bClearStyling)
	{
		// Clear all old styling...
		ClearDocumentStyle();
		
		// Now re-style the whole thing.
		//Colourise(0, -1); - doesn't work for SCLEX_CONTAINER
		ScintillaAccessor styler(this);
		styler.SetCodePage(GetCodePage());
		handleStyleNeeded(styler, 0, GetLength());
		styler.Flush();
	}
}

sregex* REScintilla::GetRE() const
{
	return m_pRE;
}

int REScintilla::HandleNotify(LPARAM lParam)
{
	Scintilla::SCNotification *scn = (Scintilla::SCNotification*)lParam;

	if( scn->nmhdr.code == SCN_STYLENEEDED )
	{
		// Get the length of the entire document
		int lengthDoc = GetLength();
		
		// Get the staring position for styling, and then move it to the start of the line.
		int start = GetEndStyled();
		int lineEndStyled = LineFromPosition(start);
		start = PositionFromLine(lineEndStyled);

		int end = scn->position; // the last character to style.

		if (end == -1)
			end = lengthDoc;
		int length = end - start;

		// The Accessor takes care of managing the lex state for us.
		ScintillaAccessor styler(this);
		styler.SetCodePage(GetCodePage());
		handleStyleNeeded(styler, start, length);
		styler.Flush();

		return 0;
	}
	else
		return baseClass::HandleNotify(lParam);
}

/**
 * Finds the full extent of the text which is styled with "style" and
 * contains the position startPos. This tries to avoid going before the
 * start of the line and after the end of the line because we only do
 * line matches anyway at the moment.
 */
void REScintilla::ExtendStyleRange(int startPos, int style, Scintilla::TextRange* tr)
{
	// Find the extent of this styled text...
	int docLength = GetLength();
	int startRange = startPos - 1;
	int endRange = startPos + 1;
	
	while( startRange >= 0 )
	{
		int c = GetCharAt(startRange);
		if(GetStyleAt(startRange) != style || c == '\n' || c == '\r')
			break;
		startRange--;
	}
	
	while( endRange < docLength )
	{
		if(GetStyleAt(endRange) != style)
			break;
		
		endRange++;
		
		int c = GetCharAt(endRange-1);
		if( c == '\n' || c == '\r' )
			break;
	}

	tr->chrg.cpMin = startRange + 1;
	tr->chrg.cpMax = endRange /* - 1*/;
}

/**
 * @brief Implement container based lexing for custom errors.
 */
void REScintilla::handleStyleNeeded(ScintillaAccessor& styler, int startPos, int length)
{
    char lineBuffer[2048]; // hopefully error lines won't be longer than this!
	styler.StartAt(startPos);
	styler.StartSegment(startPos);
	unsigned int linePos = 0;
	for (unsigned int i = startPos; i < (unsigned int)(startPos + length); i++)
	{
		lineBuffer[linePos++] = styler[i];
		if (styler.AtEOL(i) || (linePos >= sizeof(lineBuffer) - 1))
		{
			// End of line (or of line buffer) met, colourise it
			lineBuffer[linePos] = '\0';
			customColouriseLine(styler, lineBuffer, linePos, i);
			linePos = 0;
		}
	}
	if (linePos > 0)
	{	// Last line does not have ending characters
		customColouriseLine(styler, lineBuffer, length, startPos + length - 1);
	}
}

/**
 * @brief Syntax highlighter using regexs.
 */
void REScintilla::customColouriseLine(ScintillaAccessor& styler, char *lineBuffer, int length, int endLine)
{
	// Check a regex has been constructed...
	if(m_pRE)
	{
		// If the last character is a line end, then we don't continue the styling up to it.
		// This stops the hotspot from line-wrapping.
		bool bHaveLineEnd = (styler[endLine] == '\n' || styler[endLine] == '\r');
		if(bHaveLineEnd)
			endLine--;
		
		std::string line(lineBuffer, length);

		if( regex_search(line, *m_pRE) )
		{
			styler.ColourTo(endLine, SCE_CUSTOM_ERROR);
		}
		else
		{
			styler.ColourTo(endLine, SCE_ERR_DEFAULT);
		}

		if(bHaveLineEnd)
			styler.ColourTo(endLine+1, SCE_ERR_DEFAULT);
	}
}

//////////////////////////////////////////////////////////////////////////////
// CToolREBuilder
//////////////////////////////////////////////////////////////////////////////

/**
 * Replace special symbols in a regex with their regex constructs.
 */
void CToolREBuilder::OnFormatChar(TCHAR thechar)
{
	switch(thechar)
	{
		case _T('f'):
			m_string += _T("(?P<f>.+)");
		break;

		case _T('l'):
			m_string += _T("(?P<l>[0-9]+)");
		break;

		case _T('c'):
			m_string += _T("(?P<c>[0-9]+)");
		break;
	}
}