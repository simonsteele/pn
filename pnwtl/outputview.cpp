/**
 * @file OutputView.cpp
 * @brief View to display output from tool calls.
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "outputview.h"
#include "textview.h"
#include "childfrm.h"
#include "scilexer.h"
#include "scaccessor.h"
#include "include/pcreplus.h"

//////////////////////////////////////////////////////////////////////////////
// COutputView
//////////////////////////////////////////////////////////////////////////////

COutputView::COutputView()
{
	m_bCustom = false;
	m_pRE = NULL;
}

COutputView::~COutputView()
{
	if(m_pRE)
	{
		delete m_pRE;
	}
}

/**
 * Finds the full extent of the text which is styled with "style" and
 * contains the position startPos.
 */
void COutputView::ExtendStyleRange(int startPos, int style, TextRange* tr)
{
	// Find the extent of this styled text...
	int docLength = GetLength();
	int startRange = startPos - 1;
	int endRange = startPos + 1;
	
	while( startRange >= 0 )
	{
		if(GetStyleAt(startRange) != style)
			break;
		startRange--;
	}
	
	while( endRange < docLength )
	{
		if(GetStyleAt(endRange) != style)
			break;
		endRange++;
	}

	tr->chrg.cpMin = startRange + 1;
	tr->chrg.cpMax = endRange - 1;
}

/**
 * @brief Uses any regular expression string to try and match an error.
 * @param style - style that the error is displayed in.
 * @param position - position of the character clicked.
 * @param reDef - Regular Expression pattern definition.
 */
void COutputView::HandleREError(PCRE::RegExp& re, int style, int position)
{
	TextRange tr;
				
	ExtendStyleRange(position, style, &tr);
	char* buf = new char[tr.chrg.cpMax - tr.chrg.cpMin + 1];
	tr.lpstrText = buf;

	GetTextRange(&tr);

	if( re.Match(tr.lpstrText) )
	{
		tstring filename;
		tstring linestr;
		tstring colstr;
        
		// Extract the named matches from the RE, noting if there was a line or column.
		re.GetNamedMatch("f", filename);
		bool bLine = re.GetNamedMatch("l", linestr);
		bool bCol = re.GetNamedMatch("c", colstr);

		int line = atoi(linestr.c_str());

		if(FileExists(filename.c_str()))
		{
			// If the file's already open, just switch to it, otherwise open it.
			if( !g_Context.m_frame->CheckAlreadyOpen(filename.c_str(), eSwitch) )
				g_Context.m_frame->OpenFile(filename.c_str());

			if( bLine )
			{
				CChildFrame* pWnd = CChildFrame::FromHandle(GetCurrentEditor());
				CTextView* pView = pWnd->GetTextView();
				if(pView)
				{
					pView->GotoLine(line-1);
					::SetFocus(pView->m_hWnd);
				}

				if( bCol )
				{
					//@todo Jump to column.
				}
			}
		}
	}
}

void COutputView::BuildAndHandleREError(int style, int position, const char* reDef)
{
	try
	{
		PCRE::RegExp re(reDef);
		HandleREError(re, style, position);
	}
	catch (PCRE::REException& ex)
	{
		::MessageBox(NULL, ex.GetMessage(), "PN2 - Regular Expression Error", MB_OK);
	}
}

/**
 * Parse a simple GCC error string:
 * filename.ext:linenumber: error string/whatever
 */
void COutputView::HandleGCCError(int style, int position)
{
	BuildAndHandleREError(style, position, "(?P<f>.+):(?P<l>[0-9]+): .*");
}

void COutputView::HandleCustomError(int style, int position)
{
	HandleREError(*m_pRE, style, position);
}

LRESULT COutputView::OnHotSpotClicked(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	if(!m_bCustom)
	{
		switch(wParam)
		{
			case /*SCE_ERR_GCC*/ 2:
			{
				HandleGCCError(wParam, lParam);
			}
			break;
		}
	}
	else
	{
		if( wParam == 20 )
		{
			HandleCustomError(wParam, lParam);
		}
	}

	return 0;
}

void COutputView::CustomColouriseLine(ScintillaAccessor& styler, char *lineBuffer, int length, int endLine)
{
	// It is needed to remember the current state to recognize starting
	// comment lines before the first "diff " or "--- ". If a real
	// difference starts then each line starting with ' ' is a whitespace
	// otherwise it is considered a comment (Only in..., Binary file...)
	
	// Check a regex has been constructed...
	if(m_pRE)
	{
		//OpTimer timer; - for measuring performance...
		if( m_pRE->Match(lineBuffer, length, 0) )
		{
			styler.ColourTo(endLine, SCE_CUSTOM_ERROR);
		}
		else
		{
			styler.ColourTo(endLine, SCE_ERR_DEFAULT);
		}
	}
}

/**
 * Implement container based lexing for custom errors.
 */
void COutputView::HandleStyleNeeded(ScintillaAccessor& styler, int startPos, int length)
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
			CustomColouriseLine(styler, lineBuffer, linePos, i);
			linePos = 0;
		}
	}
	if (linePos > 0)
	{	// Last line does not have ending characters
		CustomColouriseLine(styler, lineBuffer, length, startPos + length - 1);
	}
}

/**
 * In this function we look for a hotspot clicked notification, and if we get
 * one then we post a message to ourselves with the details required to handle
 * it. The reason we don't just do it here is that the notify will come in the
 * middle of Scintilla handling a click - therefore we get window focus problems.
 */
int COutputView::HandleNotify(LPARAM lParam)
{
	SCNotification *scn = (SCNotification*)lParam;
	if( scn->nmhdr.code == SCN_HOTSPOTCLICK )
	{
		int style = GetStyleAt(scn->position);
		PostMessage(PN_HANDLEHSCLICK, style, scn->position);
		return 0;
	}
	else if( scn->nmhdr.code == SCN_STYLENEEDED )
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

		ScintillaAccessor styler(this);

		//int styleStart = 0;
		//if(start > 0)
		//	/*styleStart = */styler.StartAt(start - 1);
	
		styler.SetCodePage(GetCodePage());

		//lexCurrent->Lex(start, len, styleStart, keyWordLists, styler);
		HandleStyleNeeded(styler, start, length);
		styler.Flush();

		return 0;
	}
	else
		return baseClass::HandleNotify(lParam);
}

void COutputView::DoContextMenu(CPoint* point)
{
	CSPopupMenu popup(IDR_POPUP_OUTPUT);
	g_Context.m_frame->TrackPopupMenu(popup, 0, point->x, point->y, NULL);
}

void COutputView::SafeAppendText(LPCSTR s, int len)
{
	if(len == -1)
		len = strlen(s);
	SendMessage(SCI_APPENDTEXT, len, reinterpret_cast<LPARAM>(s));

	int line = SendMessage(SCI_GETLENGTH, 0, 0);
	line = SendMessage(SCI_LINEFROMPOSITION, line, 0);
	SendMessage(SCI_ENSUREVISIBLEENFORCEPOLICY, line);
	SendMessage(SCI_GOTOLINE, line);
}

void COutputView::AddToolOutput(LPCTSTR output, int nLength)
{
	SafeAppendText(output, nLength);
}

void COutputView::SetToolBasePath(LPCTSTR path)
{
	m_basepath = path;
}

void COutputView::SetToolParser(bool bBuiltIn, LPCTSTR customExpression)
{
	if(!bBuiltIn && customExpression != NULL)
	{
		// First of all build up the regular expression to use.
		CToolREBuilder builder;
		m_customre = builder.Build(customExpression);
		
		if(m_pRE)
		{
			delete m_pRE;
		}
		
		m_pRE = new PCRE::RegExp(m_customre.c_str());
		m_pRE->Study();

		// Now turn Scintilla into custom lex mode.
		m_bCustom = true;
		SetCustomLexer();
	}
	else
	{
		m_bCustom = false;
		SetOutputLexer();
	}
}

LRESULT COutputView::OnClear(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	ClearAll();

	return 0;
}

void COutputView::OnFirstShow()
{
	if(!m_bCustom)
		SetOutputLexer();
	else
		SetCustomLexer();
}

void COutputView::SetOutputLexer()
{
	CScheme* pScheme = CSchemeManager::GetInstance()->SchemeByName("output");
	if(pScheme && ::IsWindow(m_hWnd))
	{
		pScheme->Load( *(static_cast<CScintilla*>(this)) );
	}
}

void COutputView::SetCustomLexer()
{
	if( ::IsWindow(m_hWnd) )
	{
		// Load the default output lexer styles etc.
		SetOutputLexer();

		// Switch to container-based lexing...
		SetLexer(SCLEX_CONTAINER);
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

//////////////////////////////////////////////////////////////////////////////
// CDockingOutputWindow
//////////////////////////////////////////////////////////////////////////////

LRESULT CDockingOutputWindow::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	//HICON hIconSmall = (HICON)::LoadImage(_Module.GetResourceInstance(), MAKEINTRESOURCE(m_dwIcon), 
	//		IMAGE_ICON, ::GetSystemMetrics(SM_CXSMICON), ::GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);
	//SetIcon(hIconSmall, FALSE);

	m_view.Create(m_hWnd, NULL, _T("GlobalOutput"));

	return 0;
}

LRESULT CDockingOutputWindow::OnSize(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	if(wParam != SIZE_MINIMIZED )
	{
		RECT rc;
		GetClientRect(&rc);
		m_view.SetWindowPos(NULL, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top ,SWP_NOZORDER | SWP_NOACTIVATE);
	}

	bHandled = FALSE;

	return 0;
}

COutputView* CDockingOutputWindow::GetView()
{
	return &m_view;
}
