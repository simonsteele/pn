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
#include "resource.h"
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
 * contains the position startPos. This tries to avoid going before the
 * start of the line and after the end of the line because we only do
 * line matches anyway at the moment.
 */
void COutputView::ExtendStyleRange(int startPos, int style, TextRange* tr)
{
	// Find the extent of this styled text...
	int docLength = GetLength();
	int startRange = startPos - 1;
	int endRange = startPos + 1;
	
	while( startRange >= 0 )
	{
		char c = GetCharAt(startRange);
		if(GetStyleAt(startRange) != style || c == '\n' || c == '\r')
			break;
		startRange--;
	}
	
	while( endRange < docLength )
	{
		if(GetStyleAt(endRange) != style)
			break;
		
		endRange++;
		
		char c = GetCharAt(endRange-1);
		if( c == '\n' || c == '\r' )
			break;
	}

	tr->chrg.cpMin = startRange + 1;
	tr->chrg.cpMax = endRange /* - 1*/;
}

/**
 * @brief Uses any regular expression string to try and match an error.
 * @param style - style that the error is displayed in.
 * @param position - position of the character clicked.
 * @param reDef - Regular Expression pattern definition.
 */
bool COutputView::HandleREError(PCRE::RegExp& re, int style, int position)
{
	bool bRet = false;

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

#ifdef _DEBUG
		tstring dbgout = "Matched file (";
		dbgout += filename;
		dbgout += ") line (";
		dbgout += linestr;
		dbgout += ") col (";
		dbgout += colstr;
		dbgout += ")\n";
		::OutputDebugString(dbgout.c_str());
#endif

		int line = atoi(linestr.c_str());

		//First check if the file exists as is, if it does then we go with that,
		//else we try to resolve it.
		CFileName fn(filename.c_str());

		if(! FileExists(fn.c_str()) )
		{
			if( fn.IsRelativePath() && m_basepath.length() != 0 )
			{
				fn.Root( m_basepath.c_str() );
				filename = fn.c_str();
#ifdef _DEBUG
				dbgout = "Rooted path to: ";
				dbgout += filename;
				dbgout += "\n";
				::OutputDebugString(dbgout.c_str());
#endif
			}
		}
		else
		{
			// Still need to ensure that the filename is fully qualified.
			// If the file is found straight away, and is a relative path 
			// then it must be in the current directory.
			if( fn.IsRelativePath() )
			{
				fn.Root( CFileName::GetCurrentDirectory().c_str() );
			}
		}

		if(FileExists(fn.c_str()))
		{
			// If the file's already open, just switch to it, otherwise open it.
			if( !g_Context.m_frame->CheckAlreadyOpen(fn.c_str(), eSwitch) )
				g_Context.m_frame->OpenFile(fn.c_str());

			if( bLine )
			{
				CChildFrame* pWnd = CChildFrame::FromHandle(GetCurrentEditor());
				CTextView* pView = pWnd->GetTextView();
				if(pView)
				{
					pView->GotoLine(line-1);
					
					if( bCol )
					{
						long lPos = pView->GetCurrentPos();
						long column = atol(colstr.c_str());
						lPos += column;
						pView->SetCurrentPos(lPos);
					}
					
					::SetFocus(pView->m_hWnd);
				}
			}
		}

		bRet = true;
	}

	delete [] tr.lpstrText;

	return bRet;
}

/**
 * @brief Calls HandleREError with a pre-built regular expression handler.
 */
void COutputView::HandleCustomError(int style, int position)
{
	HandleREError(*m_pRE, style, position);
}

/**
 * @brief Builds a regular expression object from reDef and then calls HandleREError.
 */
bool COutputView::BuildAndHandleREError(int style, int position, const char* reDef)
{
	try
	{
		PCRE::RegExp re(reDef);
		return HandleREError(re, style, position);
	}
	catch (PCRE::REException& ex)
	{
		::MessageBox(NULL, ex.GetMessage(), "PN2 - Regular Expression Error", MB_OK);
	}

	return false;
}

/**
 * @brief Parse a simple GCC error string:
 * filename.ext:linenumber: error string/whatever
 */
void COutputView::HandleGCCError(int style, int position)
{
	BuildAndHandleREError(style, position, "(?U)(?P<f>.+):(?P<l>[0-9]+):((?P<c>[0-9]+):)? .*");
}

/**
 * @brief Parse Borland C++ errors, warnings and resource compiler warnings...
 *
 * Also handle lcc-win32 errors.
 *
 * Error E2034 clippert.cpp 207: message...
 * Warning W8070 clippert.cpp 208: message...
 * Error resources.rc 14 18: message (column line:)
 */
void COutputView::HandleBorlandCPPError(int style, int position)
{
	// Explanation of this RE: http://www.pnotepad.org/devlog/archives/000086.html
	
	if( !BuildAndHandleREError(style, position, "(Error|Warning) ((E|W)[0-9]{4} )?(?U)(?P<f>.+) ((?P<c>[0-9]+) )?(?P<l>[0-9]+): [^\\s]") )
	{
		// Didn't match borland C++, try lcc-win32:
		BuildAndHandleREError(style, position, "(Error|Warning) (?P<f>.+): (?P<l>[0-9]+) .");
	}
}

/**
 * @brief Parse perl errors...
 *
 * Perl errors are not anchored to the start of the line, beginning with "at".
 * syntax error at P:\tex\packages\authorindex.pl line 103, near "){"
 * String found where operator expected at P:\tex\packages\authorindex.pl line 110
 * Bareword "AUXFILE" not allowed while "strict subs" in use at P:\tex\packages\authorindex.pl line 100
 *
 * It might be nice in the future to handle the (near "expression") part of 
 * the string - although perhaps un-necessary.
 */
void COutputView::HandlePerlError(int style, int position)
{
	BuildAndHandleREError(style, position, "at (?P<f>.+) line (?P<l>[0-9]+)");
}

/**
 * @brief Parse python errors
 * 
 * File "T:\source\ipconfig\ipconfig.py", line 45
 */
void COutputView::HandlePythonError(int style, int position)
{
	BuildAndHandleREError(style, position, "File \"(?P<f>.+)\", line (?P<l>[0-9]+)");
}

/**
 * Handle when a hotspot is clicked.
 */
LRESULT COutputView::OnHotSpotClicked(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	if(!m_bCustom)
	{
		switch(wParam)
		{
			case SCE_ERR_PYTHON:
			{
				HandlePythonError(wParam, lParam);
			}
			break;

			case SCE_ERR_GCC:
			{
				HandleGCCError(wParam, lParam);
			}
			break;

			case SCE_ERR_BORLAND: // Borland C++
			{
				HandleBorlandCPPError(wParam, lParam);
			}
			break;

			case SCE_ERR_PERL:
			{
				HandlePerlError(wParam, lParam);
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

/**
 * @brief Syntax highlighter using regexs.
 */
void COutputView::CustomColouriseLine(ScintillaAccessor& styler, char *lineBuffer, int length, int endLine)
{
	// Check a regex has been constructed...
	if(m_pRE)
	{
		// If the last character is a line end, then we don't continue the styling up to it.
		// This stops the hotspot from line-wrapping.
		bool bHaveLineEnd = (styler[endLine] == '\n' || styler[endLine] == '\r');
		if(bHaveLineEnd)
			endLine--;
		
		if( m_pRE->Match(lineBuffer, length, 0) )
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

/**
 * @brief Implement container based lexing for custom errors.
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
		// Ignore if Ctrl is pressed...
		if( (((SCNotification*)lParam)->modifiers & SCMOD_CTRL) != 0 )
			return baseClass::HandleNotify(lParam);

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

		// The Accessor takes care of managing the lex state for us.
		ScintillaAccessor styler(this);
		styler.SetCodePage(GetCodePage());
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
	g_Context.m_frame->TrackPopupMenu(popup, 0, point->x, point->y, NULL, m_hWnd);
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

void COutputView::ClearOutput()
{
	ClearAll();
}

LRESULT COutputView::OnClear(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	ClearAll();

	return 0;
}

LRESULT COutputView::OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::SendMessage(GetParent(), WM_COMMAND, ID_OUTPUT_HIDE, NULL);
	return 0;
}

LRESULT COutputView::OnCut(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	SendMessage(WM_CUT, 0, 0);

	return 0;
}

LRESULT COutputView::OnCopy(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	SendMessage(WM_COPY, 0, 0);

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

	RECT rc;
	GetClientRect(&rc);
	m_view.Create(m_hWnd, rc, _T("GlobalOutput"));

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

LRESULT CDockingOutputWindow::OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Hide();

	return 0;
}

COutputView* CDockingOutputWindow::GetView()
{
	return &m_view;
}
