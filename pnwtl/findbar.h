/**
 * @file findbar.h
 * @brief Incremental find bar
 * @author Simon Steele
 * @note Copyright (c) 2005 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#pragma once

#define PN_HIDEFINDBAR 1045

/**
 * Edit control for the find bar, notifies of key presses and
 * highlights whether text was found or not.
 */
class CFindBarEdit : public CWindowImpl<CFindBarEdit>
{
public:
	DECLARE_WND_SUPERCLASS(_T("PN_FINDBAREDIT"), _T("EDIT"));

	CFindBarEdit();

	BEGIN_MSG_MAP(CFindBarEdit)
		MESSAGE_HANDLER(OCM_CTLCOLOREDIT, OnCtlColorEdit)
		MESSAGE_HANDLER(OCM_CTLCOLORMSGBOX, OnCtlColorEdit)
		MESSAGE_HANDLER(OCM_CTLCOLORSTATIC, OnCtlColorStatic)
		
		// Note: Use WM_CHAR instead of WM_KEYDOWN to prevent *beep* on enter/escape
		MESSAGE_HANDLER(WM_CHAR, OnKeyDown)
		
		DEFAULT_REFLECTION_HANDLER()
	END_MSG_MAP()

	void SetDoRed(bool bDoRed);

private:
	LRESULT OnCtlColorEdit(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnCtlColorStatic(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnKeyDown(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled);
	
private:
	bool m_bDoRed;
	CBrush m_brNormalBk;
	CBrush m_brRedBk;
};

/**
 * Simple owner-draw button to look like a sexy XP close button.
 *
 * Looks good on XP, looks average on 2k (lazy owner-draw code)
 */
class CXButton : public CXPButton<CXButton>
{
public:
	DECLARE_WND_SUPERCLASS(_T("PN_CLOSEXBUTTON"), _T("BUTTON"))

	HWND Create(HWND hWndParent, _U_RECT rect = NULL, LPCTSTR szWindowName = NULL,
			DWORD dwStyle = 0, DWORD dwExStyle = 0,
			_U_MENUorID MenuOrID = 0U, LPVOID lpCreateParam = NULL);
	
	LRESULT DrawItem(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL &bHandled);
};

/**
 * Built-in FireFox-esque FindBar implementation
 */
class CFindBar : public CWindowImpl<CFindBar>
{
	typedef CWindowImpl<CFindBar> baseClass;
public:
	enum {
		IDC_FBFINDNEXTBUTTON = 200,
		IDC_FBFINDPREVBUTTON = 300,
		IDC_FBTEXT = 400,
		IDC_FBMATCHCASECHECK = 500,
		IDC_FBWRAPLABEL = 600,
	};

	CFindBar();

	BEGIN_MSG_MAP(CFindBar)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_ERASEBKGND, OnEraseBkgnd)
		MESSAGE_HANDLER(WM_PAINT, OnPaint)
		MESSAGE_HANDLER(WM_SETFOCUS, OnSetFocus)
		MESSAGE_HANDLER(WM_SHOWWINDOW, OnShowWindow)
		MESSAGE_HANDLER(PN_ESCAPEPRESSED, OnEscapePressed)

		COMMAND_HANDLER(IDC_FBTEXT, EN_CHANGE, OnTextChanged)
		COMMAND_HANDLER(IDC_FBFINDNEXTBUTTON, BN_CLICKED, OnFindNextClicked)
		COMMAND_HANDLER(IDC_FBFINDPREVBUTTON, BN_CLICKED, OnFindPrevClicked)
		COMMAND_HANDLER(IDCANCEL, BN_CLICKED, OnCloseClicked)
		COMMAND_HANDLER(IDC_FBMATCHCASECHECK, BN_CLICKED, OnMatchCaseClicked)

		REFLECT_NOTIFICATIONS_MSG_FILTERED(WM_DRAWITEM)
		REFLECT_NOTIFICATIONS_MSG_ID_FILTERED(WM_CTLCOLOREDIT, IDC_FBTEXT)
		REFLECT_NOTIFICATIONS_MSG_ID_FILTERED(WM_CTLCOLORSTATIC, IDC_FBTEXT)
		//REFLECT_NOTIFICATIONS_MSG_ID_FILTERED(WM_CTLCOLORSTATIC, IDC_FBTEXT)

	END_MSG_MAP()

	void SetControllingHWND(HWND hWnd);
	int GetDesiredHeight();

private:
	// Messages:
	LRESULT OnCreate(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
	LRESULT OnEraseBkgnd(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
	LRESULT OnPaint(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
	LRESULT OnSetFocus(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
	LRESULT OnShowWindow(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
	LRESULT OnEscapePressed(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
	
	// Commands:
	LRESULT OnTextChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCloseClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFindNextClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFindPrevClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnMatchCaseClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	void findNext(LPCTSTR text, bool searchUp);

	SearchOptions so;
	CChildFrame* m_pLastFrame;
	CXButton m_xbutton;
	CButton m_findNext;
	CButton m_findPrev;
	CButton m_matchCase;
	CStatic m_wrappedLabel;
	CFindBarEdit m_txtbox;
	HWND m_controller;
	tstring m_lasttext;
};