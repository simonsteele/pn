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
#include "include/pcreplus.h"

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
void COutputView::HandleREError(int style, int position, const char* reDef)
{
	TextRange tr;
				
	ExtendStyleRange(position, style, &tr);
	char* buf = new char[tr.chrg.cpMax - tr.chrg.cpMin + 1];
	tr.lpstrText = buf;

	GetTextRange(&tr);

	try
	{
		PCRE::RegExp re(reDef);
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
	HandleREError(style, position, "(?P<f>.+):(?P<l>[0-9]+): .*");
}

LRESULT COutputView::OnHotSpotClicked(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	switch(wParam)
	{
		case /*SCE_ERR_GCC*/ 2:
		{
			HandleGCCError(wParam, lParam);
		}
		break;
	}

	return 0;
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

void COutputView::_AddToolOutput(LPCTSTR output, int nLength)
{
	SafeAppendText(output, nLength);
}

LRESULT COutputView::OnClear(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	ClearAll();

	return 0;
}

void COutputView::OnFirstShow()
{
	CSchemeManager::GetInstance()->SchemeByName("output")->Load(*this);
}



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
