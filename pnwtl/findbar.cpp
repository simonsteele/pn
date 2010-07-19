/**
 * @file findbar.cpp
 * @brief Incremental find bar implementation
 * @author Simon Steele
 * @note Copyright (c) 2005-2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "findbar.h"
#include "resource.h"
#include "childfrm.h"

/////////////////////////////////////////////////////////////////////////////
// CFindBarEdit

CFindBarEdit::CFindBarEdit()
{
	m_brNormalBk.CreateSolidBrush( ::GetSysColor(COLOR_WINDOW) );
	m_brRedBk.CreateSolidBrush( RGB(200,0,0) );
	m_bDoRed = false;
}

void CFindBarEdit::SetDoRed(bool bDoRed)
{
	m_bDoRed = bDoRed;
	Invalidate();
}

LRESULT CFindBarEdit::OnCtlColorEdit(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	CDCHandle dc( (HDC) wParam );
	
	if(m_bDoRed)
	{
		dc.SetTextColor( RGB(255,255,255) );
		dc.SetBkColor( RGB(200, 0, 0) );
		//return (LRESULT) (HBRUSH) m_brRedBk;
		return (LRESULT)GetSysColorBrush(COLOR_3DHILIGHT);
	}
	else
	{
		dc.SetTextColor( ::GetSysColor(COLOR_WINDOWTEXT) );
		dc.SetBkColor( ::GetSysColor(COLOR_WINDOW) );
		return (LRESULT) (HBRUSH) m_brNormalBk;
	}
}

LRESULT CFindBarEdit::OnCtlColorStatic(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	// Microsoft Q130952 explains why we also need this
	CDCHandle dc( (HDC) wParam );
	dc.SetBkMode(TRANSPARENT);

	if (reinterpret_cast<HWND>(lParam) != m_hWnd)
	{
		bHandled = false;
		return 0;
	}

	// According to the docs, this should only be fired when we're disabled,
	// but seems to fire all the time for me when debugging. As the visual effect
	// is right this check is disabled.
	// ATLASSERT(GetStyle() & ES_READONLY);
	dc.SetTextColor( ::GetSysColor(COLOR_GRAYTEXT) );
	dc.SetBkColor( ::GetSysColor(COLOR_WINDOW) );
	return (LRESULT) (HBRUSH) m_brNormalBk;
}

LRESULT CFindBarEdit::OnKeyDown(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	if(wParam == VK_ESCAPE && GetFocus() == m_hWnd)
	{
		::SendMessage(GetParent(), PN_ESCAPEPRESSED, 0, 0);
	}
	else if(wParam == VK_RETURN && GetFocus() == m_hWnd)
	{
		::SendMessage(GetParent(), WM_COMMAND, MAKEWPARAM(CFindBar::IDC_FBFINDNEXTBUTTON, BN_CLICKED), NULL);
	}
	else
	{
		bHandled = FALSE;
	}

	return 0;
}

/////////////////////////////////////////////////////////////////////////////
// CXButton

HWND CXButton::Create(HWND hWndParent, _U_RECT rect, LPCTSTR szWindowName,
			DWORD dwStyle, DWORD dwExStyle, _U_MENUorID MenuOrID, LPVOID lpCreateParam)
{
	HWND hWndResult = CXPButton<CXButton>::Create(hWndParent, rect, szWindowName, dwStyle,
		dwExStyle, MenuOrID, lpCreateParam);
	
	SetWindowTheme(NULL, L"WINDOW");

	return hWndResult;
}

LRESULT CXButton::DrawItem(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL &bHandled)
{
	LPDRAWITEMSTRUCT lpItem = (LPDRAWITEMSTRUCT) lParam;
	CDCHandle dc (lpItem ->hDC);

	// Get data about the request
	UINT uState = lpItem ->itemState;
	CRect rcDraw = lpItem ->rcItem;
	
	// See if we can do nice XP drawing...
	if (m_hTheme != NULL && IsAppThemed())
	{
		// Draw the outer edge
		UINT uFrameState = 0;
		if ((uState & ODS_SELECTED) != 0)
			uFrameState |= CBS_PUSHED;
		if ((uState & ODS_DISABLED) != 0)
			uFrameState |= CBS_DISABLED;
		if ((uState & ODS_HOTLIGHT) != 0 || m_bMouseOver)
			uFrameState |= CBS_HOT;

		DrawThemeBackground(dc, WP_SMALLCLOSEBUTTON, uFrameState, rcDraw, NULL);
	}
	else
	{
		HBITMAP hb = ::LoadBitmap(_Module.m_hInst, MAKEINTRESOURCE(IDB_REDCLOSE));

		CDC bitmapDC;
		bitmapDC.CreateCompatibleDC(dc);
		HBITMAP holdb = bitmapDC.SelectBitmap(hb);
		
		dc.BitBlt(0, 0, 15, 15, bitmapDC, 0, 0, SRCCOPY);

		bitmapDC.SelectBitmap(holdb);
		::DeleteObject(hb);
	}

	return 1;
}

/////////////////////////////////////////////////////////////////////////////
// CFindBar

CFindBar::CFindBar()
{
	m_pLastFrame = NULL;
	
	so.SetLoopOK(true);
	so.SetUseRegExp(false);
	so.SetMatchCase(false);
	so.SetMatchWholeWord(false);
}

void CFindBar::SetControllingHWND(HWND hWnd)
{
	m_controller = hWnd;
}

int CFindBar::GetDesiredHeight()
{
	return 30;
}

LRESULT CFindBar::OnCreate(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	CRect rcCloseButton(5, 6, 20, 21);
	CRect rcTextBox(55, 5, 250, 25);
	CRect rcFindNext(260, 5, 260+80, 24);
	CRect rcFindPrev(345, 5, 345+80, 24);
	CRect rcMatchCase(440, 5, 440+90, 24);
	CRect rcWrapLabel(550, 5, 550+250, 25);
	
	m_xbutton.Create(m_hWnd, rcCloseButton, _T("x"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS, 0, IDCANCEL);
	m_findNext.Create(m_hWnd, rcFindNext, LS(IDS_FINDBAR_FINDNEXT), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS, 0, IDC_FBFINDNEXTBUTTON);
	m_findPrev.Create(m_hWnd, rcFindPrev, LS(IDS_FINDBAR_FINDPREV), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS, 0, IDC_FBFINDPREVBUTTON);
	m_txtbox.Create(m_hWnd, rcTextBox, _T(""), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | ES_AUTOHSCROLL, WS_EX_CLIENTEDGE, IDC_FBTEXT);
	m_matchCase.Create(m_hWnd, rcMatchCase, LS(IDS_FINDBAR_MATCHCASE), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | BS_AUTOCHECKBOX, 0, IDC_FBMATCHCASECHECK);
	m_wrappedLabel.Create(m_hWnd, rcWrapLabel, LS(IDS_FINDLOOPED), WS_CHILD | WS_CLIPSIBLINGS, 0, IDC_FBWRAPLABEL);
	
	HFONT fn = (HFONT)::GetStockObject(DEFAULT_GUI_FONT);
	SetFont(fn);
	m_xbutton.SetFont(fn);
	m_txtbox.SetFont(fn);
	m_findNext.SetFont(fn);
	m_findPrev.SetFont(fn);
	m_matchCase.SetFont(fn);
	m_wrappedLabel.SetFont(fn);

	m_txtbox.SendMessage(EM_SETLIMITTEXT, 0, 0);

	return 0;
}

LRESULT CFindBar::OnEraseBkgnd(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	return 1;
}

LRESULT CFindBar::OnPaint(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	PAINTSTRUCT ps;
	BeginPaint(&ps);
	CRect rc;
	GetClientRect(rc);
	SetBkColor(ps.hdc, GetSysColor(COLOR_BTNFACE));
	FillRect(ps.hdc, &rc, (HBRUSH)::GetSysColorBrush(COLOR_BTNFACE));

	HFONT hOldFont = (HFONT)SelectObject(ps.hdc, GetStockObject(DEFAULT_GUI_FONT));

	CRect rcText(25, 5, 50, 25);
	DrawText(ps.hdc, LS(IDS_FINDBAR_PROMPT), 5, rcText, DT_RIGHT | DT_VCENTER | DT_SINGLELINE);

	SelectObject(ps.hdc, hOldFont);

	EndPaint(&ps);
	return 0;
}

LRESULT CFindBar::OnSetFocus(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	m_txtbox.SetFocus();
	return 0;
}

LRESULT CFindBar::OnShowWindow(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = FALSE;
	if(wParam)
	{
		m_txtbox.SetWindowText(_T(""));
		m_txtbox.SetDoRed(false);
		m_lasttext = _T("");
		so.SetFound(false);
		m_pLastFrame = NULL;
	}

	return 0;
}

LRESULT CFindBar::OnEscapePressed(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	return ::SendMessage(m_controller, PN_ESCAPEPRESSED, 0, 0);
}

LRESULT CFindBar::OnTextChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CWindowText wt(m_txtbox.m_hWnd);
	if((LPCTSTR)wt == NULL)
		return 0;

	findNext(wt, false);
	
	return 0;
}

LRESULT CFindBar::OnCloseClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::SendMessage(m_controller, PN_NOTIFY, 0, PN_HIDEFINDBAR);
	return 0;
}

LRESULT CFindBar::OnFindNextClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	findNext(NULL, false);
	return 0;
}

LRESULT CFindBar::OnFindPrevClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	findNext(NULL, true);
	return 0;
}

LRESULT CFindBar::OnMatchCaseClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	so.SetMatchCase(m_matchCase.GetCheck() == BST_CHECKED);

	// Disabled for now, bugs being raised about searching when this is checked.
	// Could be reintroduced with a check to see if the current selection still matches.
	/*if(!m_lasttext.empty())
	{
		findNext(NULL, false);
	}*/

	return 0;
}

void CFindBar::findNext(LPCTSTR text, bool searchUp)
{
	CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor());
	if(pChild != NULL)
	{
		if(m_pLastFrame != pChild)
			so.SetFound(false);
		m_pLastFrame = pChild;
		
		if(text == NULL)
		{
			so.SetFindText(m_lasttext.c_str());
		}
		else
		{
			so.SetFindText(text);
		}

		so.SetSearchBackwards(searchUp);

		CTextView* pTV = pChild->GetTextView();
		if(!pTV)
			return;

		// Move the text selection around so we search in the
		// right place.
		Scintilla::CharacterRange cr;
		pTV->GetSel(cr);

		if(m_lasttext != so.GetFindText())
		{
			// Kill the selection, so we re-select if we find good stuff,
			// or move further on to find it.
			pTV->SetSel(cr.cpMin, cr.cpMin);
		}
		else
		{
			// Searching for the same thing, it's a find next.
			if(searchUp)
				pTV->SetSel(cr.cpMin, cr.cpMin);
			//else
			//	pTV->SetSel(cr.cpMax, cr.cpMax);
		}

		int result = pTV->FindNext(&so);
		if(result == fnNotFound)
		{
			pTV->SetSel(cr.cpMin, cr.cpMax);
			reinterpret_cast<SearchOptions*>( OPTIONS->GetSearchOptions() )->SetFound(false);
			m_txtbox.SetDoRed(true);
		}
		else
		{
			reinterpret_cast<SearchOptions*>( OPTIONS->GetSearchOptions() )->SetFound(true);
			m_txtbox.SetDoRed(false);

			bool showWrap = (result == fnReachedStart);
			m_wrappedLabel.ShowWindow(showWrap ? SW_SHOW : SW_HIDE);
		}

		// Store text in main search options, and in our stored one.
		OPTIONS->GetSearchOptions()->SetFindText( so.GetFindText() );
		m_lasttext = so.GetFindText();
	}
}