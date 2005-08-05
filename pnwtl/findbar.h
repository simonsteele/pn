#pragma once

#define PN_HIDEFINDBAR 1045

class CFindBarEdit : public CWindowImpl<CFindBarEdit>
{
public:
	DECLARE_WND_SUPERCLASS("PN_FINDBAREDIT", "EDIT");

	CFindBarEdit()
	{
		m_brNormalBk.CreateSolidBrush( ::GetSysColor(COLOR_WINDOW) );
		m_brRedBk.CreateSolidBrush( RGB(200,0,0) );
		m_bDoRed = false;
	}

	BEGIN_MSG_MAP(CFindBarEdit)
		MESSAGE_HANDLER(OCM_CTLCOLOREDIT, OnCtlColorEdit)
		MESSAGE_HANDLER(OCM_CTLCOLORMSGBOX, OnCtlColorEdit)
		MESSAGE_HANDLER(OCM_CTLCOLORSTATIC, OnCtlColorStatic)
		MESSAGE_HANDLER(WM_KEYDOWN, OnKeyDown)
		DEFAULT_REFLECTION_HANDLER()
	END_MSG_MAP()

	LRESULT OnCtlColorEdit(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		CDCHandle dc( (HDC) wParam );
		
		//dc.SetBkMode(TRANSPARENT);
		
		if(m_bDoRed)
		{
			dc.SetTextColor( RGB(255,255,255) );
			dc.SetBkColor( RGB(200, 0, 0) );
			return (LRESULT) (HBRUSH) m_brRedBk;
		}
		else
		{
			dc.SetTextColor( ::GetSysColor(COLOR_WINDOWTEXT) );
			dc.SetBkColor( ::GetSysColor(COLOR_WINDOW) );
			return (LRESULT) (HBRUSH) m_brNormalBk;
		}
	}

	LRESULT OnCtlColorStatic(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		// Microsoft Q130952 explains why we also need this
		CDCHandle dc( (HDC) wParam );
		dc.SetBkMode(TRANSPARENT);

		ATLASSERT(GetStyle() & ES_READONLY);
		dc.SetTextColor( ::GetSysColor(COLOR_GRAYTEXT) );
		dc.SetBkColor( ::GetSysColor(COLOR_WINDOW) );
		return (LRESULT) (HBRUSH) m_brNormalBk;
	}

	void SetDoRed(bool bDoRed)
	{
		m_bDoRed = bDoRed;
		Invalidate();
	}

	LRESULT OnKeyDown(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
	{
		if(wParam == VK_ESCAPE && GetFocus() == m_hWnd)
		{
			::SendMessage(GetParent(), PN_ESCAPEPRESSED, 0, 0);
		}
		else
		{
			bHandled = FALSE;
		}

		return 0;
	}

protected:
	bool m_bDoRed;
	CBrush m_brNormalBk;
	CBrush m_brRedBk;
};

class CXButton : public CXPButton<CXButton>
{
public:
	DECLARE_WND_SUPERCLASS("PN_CLOSEXBUTTON", "BUTTON")

	HWND Create(HWND hWndParent, _U_RECT rect = NULL, LPCTSTR szWindowName = NULL,
			DWORD dwStyle = 0, DWORD dwExStyle = 0,
			_U_MENUorID MenuOrID = 0U, LPVOID lpCreateParam = NULL)
		{
			HWND hWndResult = CXPButton<CXButton>::Create(hWndParent, rect, szWindowName, dwStyle,
				dwExStyle, MenuOrID, lpCreateParam);
			
			SetWindowTheme(NULL, L"WINDOW");

			return hWndResult;
		}
	
	LRESULT DrawItem(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL &bHandled)
	{
		LPDRAWITEMSTRUCT lpItem = (LPDRAWITEMSTRUCT) lParam;
		CDCHandle dc (lpItem ->hDC);

		// Get data about the request
		UINT uState = lpItem ->itemState;
		CRect rcDraw = lpItem ->rcItem;
		
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
			/*else if ((uState & ODS_DEFAULT) != 0)
				uFrameState |= CBS_DEFAULTED;*/
			/*DrawThemeBackground (dc, BP_PUSHBUTTON, 
				uFrameState, &rcDraw, NULL);
			GetThemeBackgroundContentRect (dc, BP_PUSHBUTTON, 
				uFrameState, &rcDraw, &rcDraw);*/

			DrawThemeBackground(dc, WP_SMALLCLOSEBUTTON, uFrameState, rcDraw, NULL);
			//DrawThemeParentBackground(dc, rcDraw);
		}

		return 1;
	}
};

class CFindBar : public CWindowImpl<CFindBar>
{
	typedef CWindowImpl<CFindBar> baseClass;
public:
	enum {
		IDC_FBTEXT = 400,
	};

	BEGIN_MSG_MAP(CFindBar)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_ERASEBKGND, OnEraseBkgnd)
		MESSAGE_HANDLER(WM_PAINT, OnPaint)
		MESSAGE_HANDLER(WM_SETFOCUS, OnSetFocus)
		MESSAGE_HANDLER(PN_ESCAPEPRESSED, OnEscapePressed)

		COMMAND_HANDLER(IDC_FBTEXT, EN_CHANGE, OnTextChanged)
		COMMAND_HANDLER(IDCANCEL, BN_CLICKED, OnCloseClicked)

		REFLECT_NOTIFICATIONS_MSG_FILTERED(WM_DRAWITEM)
		REFLECT_NOTIFICATIONS_MSG_ID_FILTERED(WM_CTLCOLOREDIT, IDC_FBTEXT)
		REFLECT_NOTIFICATIONS_MSG_ID_FILTERED(WM_CTLCOLORMSGBOX, IDC_FBTEXT)
		//REFLECT_NOTIFICATIONS_MSG_ID_FILTERED(WM_CTLCOLORSTATIC, IDC_FBTEXT)

	END_MSG_MAP()

	void SetControllingHWND(HWND hWnd)
	{
		m_controller = hWnd;
	}

	int GetDesiredHeight()
	{
		return 30;
	}

protected:
	LRESULT OnTextChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnCreate(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		CRect rcCloseButton(5, 6, 20, 21);
		CRect rcTextBox(55, 5, 250, 25);
		CRect rcFindNext(260, 5, 260+80, 24);
		CRect rcFindPrev(345, 5, 345+80, 24);
		m_xbutton.Create(m_hWnd, rcCloseButton, "x", WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS, 0, IDCANCEL);
		m_findNext.Create(m_hWnd, rcFindNext, "Find Next", WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS, 0, 200);
		m_findPrev.Create(m_hWnd, rcFindPrev, "Find Previous", WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS, 0, 300);
		m_txtbox.Create(m_hWnd, rcTextBox, "", WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS, WS_EX_CLIENTEDGE, IDC_FBTEXT);
		HFONT fn = (HFONT)::GetStockObject(DEFAULT_GUI_FONT);
		SetFont(fn);
		m_xbutton.SetFont(fn);
		m_txtbox.SetFont(fn);
		m_findNext.SetFont(fn);
		m_findPrev.SetFont(fn);

		return 0;
	}

	LRESULT OnEraseBkgnd(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		return 1;
	}

	LRESULT OnPaint(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		PAINTSTRUCT ps;
		BeginPaint(&ps);
		CRect rc;
		GetClientRect(rc);
		SetBkColor(ps.hdc, GetSysColor(COLOR_BTNFACE));
		FillRect(ps.hdc, &rc, (HBRUSH)::GetSysColorBrush(COLOR_BTNFACE));

		HFONT hOldFont = (HFONT)SelectObject(ps.hdc, GetStockObject(DEFAULT_GUI_FONT));

		CRect rcText(25, 5, 50, 25);
		DrawText(ps.hdc, "Find:", 5, rcText, DT_RIGHT | DT_VCENTER | DT_SINGLELINE);

		SelectObject(ps.hdc, hOldFont);

		EndPaint(&ps);
		return 0;
	}

	LRESULT OnSetFocus(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		m_txtbox.SetFocus();
		return 0;
	}

	LRESULT OnEscapePressed(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		return ::SendMessage(m_controller, PN_ESCAPEPRESSED, 0, 0);
	}

	LRESULT OnCloseClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		::SendMessage(m_controller, PN_NOTIFY, 0, PN_HIDEFINDBAR);
		return 0;
	}


protected:
	CXButton m_xbutton;
	CButton m_findNext;
	CButton m_findPrev;
	CFindBarEdit m_txtbox;
	HWND m_controller;
};