/**
 * @file finddlg.h
 * @brief Find and Replace dialogs for PN WTL
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef finddlg_h__included
#define finddlg_h__included

#include "CustomAutoComplete.h"
#include "include/accombo.h"

/**
 * @class CSearchDlg
 * @brief template class providing basic functionality for the search and replace dialogs.
 */
template <class T, class TBase = CWindow>
class ATL_NO_VTABLE CSearchDlg : public CDialogImpl<T, TBase>
{
public:
	void Show(LPCTSTR findText = NULL)
	{
		T* pThis = static_cast<T*>(this);
		SetFindText(findText);
		pThis->DoDataExchange(FALSE);

		// Place the dialog so it doesn't hide the text under the caret
		CChildFrame* editor = GetCurrentEditorWnd();
		if(editor)
		{
			// Get the position on screen of the caret.
			CTextView* textView = editor->GetTextView();
			long pos = textView->GetCurrentPos();
			POINT pt = {
				textView->PointXFromPosition(pos),
				textView->PointYFromPosition(pos)
			};
			int lineHeight = textView->TextHeight(0);
			editor->ClientToScreen(&pt);
			
			// Place the window away from the caret.
			pThis->PlaceWindow(pt, lineHeight);
		}

		ShowWindow(SW_SHOW);
		
		pThis->m_FindTextCombo.SetFocus();
	}

	/**
	 * @brief Places the window so that it does not cover the cursor.
	 * @param pt constant reference to the point that should not be covered by the dialog
	 * @param lineHeight line height as specified by Scintilla.
	 */
	void PlaceWindow(const POINT& pt, int lineHeight = 10)
	{
		T* pThis = static_cast<T*>(this);

		// Get current pos and size
		RECT rc;
		pThis->GetWindowRect(&rc);
		POINT winPos = { rc.left, rc.top };
		SIZE winSize = { rc.right - rc.left, rc.bottom - rc.top };

		// Default to place the dialog above the line
		POINT pos = winPos;
		if(rc.bottom >= pt.y && rc.top <= pt.y)
			pos.y = pt.y - winSize.cy - lineHeight;
		if(pos.y < 0)
			pos.y = max(0, pt.y + lineHeight);

		::GetWindowRect(pThis->GetWindow(GW_OWNER), &rc);
		// TODO: 
		pos.x = rc.right - winSize.cx - ::GetSystemMetrics(SM_CXVSCROLL);

		// Ensure that the dialog is inside the work area
		RECT rcWa;
		::SystemParametersInfo(SPI_GETWORKAREA, 0, &rcWa, FALSE);
		if(pos.x + winSize.cx > rcWa.right - rcWa.left)
			pos.x = rcWa.right - rcWa.left - winSize.cx;
		if(pos.x < rcWa.left)
			pos.x = rcWa.left;
		if(pos.y + winSize.cy > rcWa.bottom - rcWa.top)
			pos.y = rcWa.bottom - rcWa.top - winSize.cy;
		if(pos.y < rcWa.top)
			pos.y = rcWa.top;

		// Move when the pos changed only
		if(winPos.x != pos.x || winPos.y != pos.y)
			// note no SWP_SHOWWINDOW - this causes OnShowWindow not to be called on creation.
			pThis->SetWindowPos(NULL, pos.x, pos.y, 0, 0, SWP_NOSIZE | SWP_NOZORDER /*| SWP_SHOWWINDOW*/);
	}

	bool FindNext()
	{
		int found = 0;

		CChildFrame* editor = GetCurrentEditorWnd();

		if(editor != NULL)
		{
			T* pThis = static_cast<T*>(this);
			SFindOptions* pOptions = pThis->GetOptions();
			found = editor->FindNext(pOptions);			
		}
		
		return found != 0;
	}

	BEGIN_MSG_MAP(CSearchDlg)
	END_MSG_MAP()

protected:
	int DoRegExpInsert(BXT::CComboBoxAC* pCB, LPCTSTR insert, CString& str, int offset)
	{
		CEdit cbedit(pCB->m_edit);
		DWORD dwSel = cbedit.GetSel();
		cbedit.ReplaceSel(insert);

		int pos = LOWORD(dwSel);
		if(offset != 0)
			pos += offset;
		else
			pos += _tcslen(insert);
		cbedit.SetFocus();
		cbedit.SetSel(pos, pos);

		// DoDataExchange() seems to take care of it.
//		cbedit.GetWindowText(str);
//		GetFindStartPos();

		return pos;
	}

	int GetRegExpString(int nID, CString& Text)
	{
		int offset = 0;

		switch(nID)
		{
			case ID_REGEXP_ANYCHARACTER:
				Text = _T(".");
				break;
			case ID_REGEXP_CHARACTERINRANGE:
				Text = _T("[]");
				offset = 1;
				break;
			case ID_REGEXP_CHARACTERNOTINRANGE:
				Text = _T("[^]");
				offset = 2;
				break;
			case ID_REGEXP_BEGINNINGOFLINE:
				Text = _T("^");
				break;
			case ID_REGEXP_ENDOFLINE:
				Text = _T("$");
				break;
			case ID_REGEXP_TAGGEDEXPRESSSION:
				Text = _T("\\(\\)");
				offset = 2;
				break;
			case ID_REGEXP_NOT:
				Text = _T("~");
				break;
			case ID_REGEXP_OR:
				Text = _T("\\!");
				break;
			case ID_REGEXP_0ORMOREMATCHES:
				Text = _T("*");
				break;
			case ID_REGEXP_1ORMOREMATCHES:
				Text = _T("+");
				break;
			case ID_REGEXP_GROUP:
				Text = _T("\\{\\}");
				offset = 2;
				break;
		};

		return offset;
	}

	void DoREHelperMenu(LPRECT rc, bool bDoMatches = false)
	{
		HMENU hRegExpMenu;
		if(bDoMatches)
		{
			hRegExpMenu = ::LoadMenu(_Module.m_hInst, MAKEINTRESOURCE(IDR_POPUP_REGEXPM));
		}
		else
		{
			hRegExpMenu = ::LoadMenu(_Module.m_hInst, MAKEINTRESOURCE(IDR_POPUP_REGEXP));
		}

		HMENU hPopup = ::GetSubMenu(hRegExpMenu, 0);

		::TrackPopupMenu(hPopup, 0, rc->right, rc->top, 0, m_hWnd, NULL);

		::DestroyMenu(hRegExpMenu);
	}

	bool EditorChanged()
	{
		return lastEditor != CChildFrame::FromHandle(GetCurrentEditor());
	}

	CChildFrame* GetCurrentEditorWnd()
	{
		lastEditor = CChildFrame::FromHandle(GetCurrentEditor());
		return lastEditor;
	}

	void SetFindText(LPCTSTR findText)
	{
		T* pT = static_cast<T*>(this);
		pT->m_FindText = findText;
	}

	CSize GetGUIFontSize()
	{
		CClientDC dc(m_hWnd);
		dc.SelectFont((HFONT) GetStockObject( DEFAULT_GUI_FONT ));		
		TEXTMETRIC tm;
		dc.GetTextMetrics( &tm );
		//int cxChar = tm.tmAveCharWidth;
		//int cyChar = tm.tmHeight + tm.tmExternalLeading;

		return CSize( tm.tmAveCharWidth, tm.tmHeight + tm.tmExternalLeading);
	}

	CChildFrame* lastEditor;
};

class CFindDlg : public CSearchDlg<CFindDlg>,
					public CWinDataExchange<CFindDlg>,
					public CUpdateUI<CFindDlg>
{
public:
	enum { IDD = IDD_FIND };

	virtual BOOL PreTranslateMessage(MSG* pMsg)
	{
		return IsDialogMessage(pMsg);
	}

	BEGIN_UPDATE_UI_MAP(CFindDlg)
		UPDATE_ELEMENT(IDC_REHELPER_BUTTON, UPDUI_CHILDWINDOW)
	END_UPDATE_UI_MAP()

	BEGIN_MSG_MAP(CFindDlg)
		MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
		COMMAND_ID_HANDLER(IDOK, OnCloseCmd)
		COMMAND_ID_HANDLER(IDCANCEL, OnCloseCmd)
		COMMAND_ID_HANDLER(IDC_FINDNEXT_BUTTON, OnFindNextClicked)
		COMMAND_ID_HANDLER(IDC_REHELPER_BUTTON, OnReHelperClicked)
		COMMAND_RANGE_HANDLER(ID_REGEXP_ANYCHARACTER, ID_REGEXP_GROUP, OnReInsertClicked)
		COMMAND_ID_HANDLER(IDC_REGEXP_CHECK, OnUseRegExpClicked)
		MESSAGE_HANDLER(WM_SHOWWINDOW, OnShowWindow)
		REFLECT_NOTIFICATIONS ()
	END_MSG_MAP()

	BEGIN_DDX_MAP(CFindDlg)
		DDX_TEXT(IDC_FINDTEXT_COMBO, m_FindText)
		DDX_CHECK(IDC_MATCHCASE_CHECK, m_bMatchCase)
		DDX_CHECK(IDC_MATCHWHOLE_CHECK, m_bMatchWhole)
		DDX_CHECK(IDC_SEARCHALL_CHECK, m_bSearchAll)
		DDX_CHECK(IDC_REGEXP_CHECK, m_bRegExp)
		DDX_RADIO(IDC_UP_RADIO, m_Direction)
	END_DDX_MAP()

	LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		m_Direction = 1;

		CRect rc;
		::GetWindowRect(GetDlgItem(IDC_FINDTEXT_DUMMY), rc);
		ScreenToClient(rc);

		CSize size = GetGUIFontSize();
		rc.bottom = rc.top + (size.cy * 10);

		m_FindTextCombo.Create(m_hWnd, rc, _T("FINDTEXTCOMBO"), CBS_DROPDOWN | CBS_AUTOHSCROLL | WS_CHILD | WS_VSCROLL | WS_VISIBLE | WS_TABSTOP, 0, IDC_FINDTEXT_COMBO,
			_T("Software\\Echo Software\\PN2\\AutoComplete\\Find"), IDC_FINDTEXT_DUMMY);

		m_ReHelperBtn.SubclassWindow(GetDlgItem(IDC_REHELPER_BUTTON));

		UIAddChildWindowContainer(m_hWnd);

		CenterWindow(GetParent());

		return TRUE;
	}

	LRESULT OnCloseCmd(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		CChildFrame* pChild = GetCurrentEditorWnd();
		if(pChild != NULL)
			pChild->SetFocus();
		else
			GetWindow(GW_OWNER).SetFocus();

		ShowWindow(SW_HIDE);
		
		return 0;
	}

	LRESULT OnFindNextClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		if(FindNext())
		{
			// Default Visual C++, old PN and others behaviour:
			///@todo implement window stays open behaviour as well...
			ShowWindow(SW_HIDE);
		}
		else
		{
			CString strTextToFind=m_FindText, strMsg;
			if (strTextToFind.IsEmpty())
				strTextToFind = _T("(empty)");

			strMsg.Format(_T("The specified text '%s' was not found."), strTextToFind);
			MessageBox((LPCTSTR)strMsg, _T("Programmers Notepad"), MB_OK | MB_ICONINFORMATION);
		}

		return TRUE;
	}

	LRESULT OnReHelperClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		CRect rc;
		m_ReHelperBtn.GetWindowRect(&rc);
		DoREHelperMenu(rc);

		return TRUE;
	}

	SFindOptions* GetOptions()
	{
		DoDataExchange(TRUE);

		SFindOptions* pOptions = OPTIONS->GetFindOptions();

		// If the user has changed to a differnent scintilla window
		// then Found is no longer necessarily true.
		if(EditorChanged())
			pOptions->Found = false;

		pOptions->FindText			= m_FindText;
		pOptions->Direction			= (m_Direction == 1);
		pOptions->MatchCase			= (m_bMatchCase == TRUE);
		pOptions->MatchWholeWord	= (m_bMatchWhole == TRUE);
		pOptions->SearchAll			= (m_bSearchAll == TRUE);
		pOptions->UseRegExp			= (m_bRegExp == TRUE);
		
		///@todo Add a user interface counterpart for the loop search option.
		pOptions->Loop				= true;

		return pOptions;
	}

	LRESULT OnShowWindow(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		if((BOOL)wParam)
		{
			SFindOptions* pOptions = OPTIONS->GetFindOptions();
			SetFindText(m_FindText);
			if(m_FindText.GetLength() == 0 && pOptions->FindText.GetLength())
				m_FindText = pOptions->FindText;
			m_Direction = pOptions->Direction;
			m_bMatchCase = pOptions->MatchCase;
			m_bMatchWhole = pOptions->MatchWholeWord;
			m_bSearchAll = pOptions->SearchAll;
			m_bRegExp = pOptions->UseRegExp;
			
			// Do the funky DDX thang...
			DoDataExchange(FALSE);

			m_FindTextCombo.SetFocus();

			UIEnable(IDC_REHELPER_BUTTON, m_bRegExp);
			UIUpdateChildWindows();
		}
		return 0;
	}

	LRESULT OnReInsertClicked(WORD /*wNotifyCode*/, WORD nID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		CString Text;
		int offset = GetRegExpString(nID, Text);

		DoDataExchange(TRUE);
		DoRegExpInsert(&m_FindTextCombo, Text, m_FindText, offset);

		return TRUE;
	}

	LRESULT OnUseRegExpClicked(WORD /*wNotifyCode*/, WORD /*nID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		m_bRegExp = !m_bRegExp;
		UIEnable(IDC_REHELPER_BUTTON, m_bRegExp);
		UIUpdateChildWindows();
		return TRUE;
	}

	// Shared properties should be defined in CSearchDlg
	friend CSearchDlg<CFindDlg>;

protected:
	BXT::CComboBoxAC				m_FindTextCombo;
	CArrowButton					m_ReHelperBtn;

	CString	m_FindText;
	int		m_Direction;
	BOOL	m_bMatchCase;
	BOOL	m_bMatchWhole;
	BOOL	m_bSearchAll;
	BOOL	m_bRegExp;
};

class CReplaceDlg : public CSearchDlg<CReplaceDlg>,
					public CWinDataExchange<CReplaceDlg>,
					public CUpdateUI<CReplaceDlg>
{
public:
	enum { IDD = IDD_REPLACE };

	BEGIN_UPDATE_UI_MAP(CReplaceDlg)
		UPDATE_ELEMENT(IDC_REHELPER_BUTTON, UPDUI_CHILDWINDOW)
		UPDATE_ELEMENT(IDC_RHELPER_BUTTON, UPDUI_CHILDWINDOW)
	END_UPDATE_UI_MAP()

	BEGIN_MSG_MAP(CReplaceDlg)
		MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
		COMMAND_ID_HANDLER(IDOK, OnCloseCmd)
		COMMAND_ID_HANDLER(IDCANCEL, OnCloseCmd)
		COMMAND_HANDLER(IDC_REHELPER_BUTTON, BN_CLICKED, OnReHelperClicked)
		COMMAND_HANDLER(IDC_RHELPER_BUTTON, BN_CLICKED, OnReHelper2Clicked)
		COMMAND_RANGE_HANDLER(ID_REGEXP_ANYCHARACTER, ID_REGEXP_GROUP, OnReHelperMenuItemClicked)
		COMMAND_RANGE_HANDLER(ID_REGEXP_TAGGEDEXPRESSION1, ID_REGEXP_TAGGEDEXPRESSION9, OnReMatchesMenuItemClicked)
		MESSAGE_HANDLER(WM_SHOWWINDOW, OnShowWindow)
		MESSAGE_HANDLER(WM_CLOSE, OnCloseWindow)
		COMMAND_HANDLER(IDC_FINDNEXT_BUTTON, BN_CLICKED, OnFindNextClicked)
		COMMAND_HANDLER(IDC_REPLACE_BUTTON, BN_CLICKED, OnReplaceClicked)
		COMMAND_HANDLER(IDC_REPLACEALL_BUTTON, BN_CLICKED, OnReplaceAllClicked)
		COMMAND_HANDLER(IDC_REPLACEINSEL_BUTTON, BN_CLICKED, OnReplaceInSelectionClicked)
		COMMAND_ID_HANDLER(IDC_REGEXP_CHECK, OnUseRegExpClicked)
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	
	BEGIN_DDX_MAP(CReplaceDlg)
		DDX_TEXT(IDC_FINDTEXT_COMBO, m_FindText)
		DDX_TEXT(IDC_REPLACETEXT_COMBO, m_ReplaceText)
		DDX_CHECK(IDC_MATCHCASE_CHECK, m_bMatchCase)
		DDX_CHECK(IDC_MATCHWHOLE_CHECK, m_bMatchWhole)
		DDX_CHECK(IDC_REGEXP_CHECK, m_bRegExp)
		DDX_CHECK(IDC_BACKSLASH_CHECK, m_bUseSlashes)
		DDX_RADIO(IDC_UP_RADIO, m_Direction)
		//DDX_CHECK(IDC_SEARCHALL_CHECK, m_bSearchAll)
	END_DDX_MAP()

	SReplaceOptions* GetOptions()
	{
		DoDataExchange(TRUE);

		SReplaceOptions* pOptions = OPTIONS->GetReplaceOptions();

		// If the user has changed to a differnent scintilla window
		// then Found is no longer necessarily true.
		if(EditorChanged())
			pOptions->Found = false;

		pOptions->FindText			= m_FindText;
		pOptions->ReplaceText		= m_ReplaceText;
		pOptions->Direction			= (m_Direction == 1);
		pOptions->MatchCase			= (m_bMatchCase == TRUE);
		pOptions->MatchWholeWord	= (m_bMatchWhole == TRUE);
		//pOptions->SearchAll			= (m_bSearchAll == TRUE);
		pOptions->UseRegExp			= (m_bRegExp == TRUE);
		pOptions->UseSlashes		= (m_bUseSlashes == TRUE);
		pOptions->InSelection		= false;
		
		///@todo Add a user interface counterpart for the loop search option.
		pOptions->Loop				= true;
		
		return pOptions;
	}

	LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		m_Direction = 1;
		
		m_ReHelperBtn.SubclassWindow(GetDlgItem(IDC_REHELPER_BUTTON));
		m_ReHelperBtn2.SubclassWindow(GetDlgItem(IDC_RHELPER_BUTTON));

		CRect rc;

		::GetWindowRect(GetDlgItem(IDC_FINDTEXT_DUMMY), rc);
		ScreenToClient(rc);

		CSize size = GetGUIFontSize();
		rc.bottom = rc.top + (size.cy * 10);

		m_FindTextCombo.Create(m_hWnd, rc, _T("RFINDTEXTCOMBO"), CBS_DROPDOWN | CBS_AUTOHSCROLL | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 0, IDC_FINDTEXT_COMBO,
			_T("Software\\Echo Software\\PN2\\AutoComplete\\Find"), IDC_FINDTEXT_DUMMY);
		
		::GetWindowRect(GetDlgItem(IDC_REPLACETEXT_DUMMY), rc);
		ScreenToClient(rc);

		rc.bottom = rc.top + (size.cy * 10);

		m_ReplaceTextCombo.Create(m_hWnd, rc, _T("REPLACETEXTCOMBO"), CBS_DROPDOWN | CBS_AUTOHSCROLL | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 0, IDC_REPLACETEXT_COMBO,
			_T("Software\\Echo Software\\PN2\\AutoComplete\\Replace"), IDC_REPLACETEXT_DUMMY);
		
		UIAddChildWindowContainer(m_hWnd);

		CenterWindow(GetParent());
		return TRUE;
	}

	LRESULT OnCloseCmd(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		CChildFrame* pChild = GetCurrentEditorWnd();
		if(pChild != NULL)
			pChild->SetFocus();
		else
			GetWindow(GW_OWNER).SetFocus();

		ShowWindow(SW_HIDE);
		
		return 0;
	}
	
	LRESULT OnReHelperClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		CRect rc;
		m_ReHelperBtn.GetWindowRect(&rc);
		DoREHelperMenu(rc);

		return TRUE;
	}
	
	LRESULT OnReHelper2Clicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		CRect rc;
		m_ReHelperBtn2.GetWindowRect(&rc);
		DoREHelperMenu(rc, true);
		return 0;
	}
	
	LRESULT OnReHelperMenuItemClicked(WORD /*wNotifyCode*/, WORD nID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		CString Text;
		int offset = GetRegExpString(nID, Text);

		DoDataExchange(TRUE);
		DoRegExpInsert(&m_FindTextCombo, Text, m_FindText, offset);
		
		return 0;
	}

	LRESULT OnReMatchesMenuItemClicked(WORD /*wNotifyCode*/, WORD nID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		int matchno = (nID - ID_REGEXP_TAGGEDEXPRESSION1) + 1;
		CString Text;
		Text.Format(_T("\\%d"), matchno);

		DoDataExchange(TRUE);
		DoRegExpInsert(&m_ReplaceTextCombo, Text, m_ReplaceText, 0);

		return 0;
	}

	LRESULT OnUseRegExpClicked(WORD /*wNotifyCode*/, WORD /*nID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		m_bRegExp = !m_bRegExp;
		UIEnable(IDC_REHELPER_BUTTON, m_bRegExp);
		UIEnable(IDC_RHELPER_BUTTON, m_bRegExp);
		UIUpdateChildWindows();
		return TRUE;
	}

	LRESULT OnShowWindow(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		if((BOOL) wParam)
		{
 			SReplaceOptions* pOptions = OPTIONS->GetReplaceOptions();
			
			if(m_FindText.GetLength() == 0 && pOptions->FindText.GetLength())
				m_FindText = pOptions->FindText;
			m_ReplaceText = pOptions->ReplaceText;
			m_Direction = pOptions->Direction;
			m_bMatchCase = pOptions->MatchCase;
			m_bMatchWhole = pOptions->MatchWholeWord;
			m_bRegExp = pOptions->UseRegExp;
			m_bUseSlashes = pOptions->UseSlashes;
			//m_bSearchAll = pOptions->SearchAll;

			DoDataExchange(FALSE);

			m_FindTextCombo.SetFocus();

			UIEnable(IDC_REHELPER_BUTTON, m_bRegExp);
			UIEnable(IDC_RHELPER_BUTTON, m_bRegExp);
			UIUpdateChildWindows();
		}

		return 0;
	}

	LRESULT OnCloseWindow(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		ShowWindow(SW_HIDE);
		return 0;
	}

	LRESULT OnFindNextClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		if (!FindNext())
		{
			CString strTextToFind=m_FindText, strMsg;
			if (strTextToFind.IsEmpty())
				strTextToFind = _T("(empty)");

			strMsg.Format(_T("The specified text '%s' was not found."), strTextToFind);
			MessageBox((LPCTSTR)strMsg, _T("Programmers Notepad"), MB_OK | MB_ICONINFORMATION);
		}

		return 0;
	}

	LRESULT OnReplaceClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		// Call this before GetCurrentEditorWnd - it will check if the editor
		// has changed and if so set Found to false.
		SReplaceOptions* pOptions = GetOptions();

		CChildFrame* pEditor = GetCurrentEditorWnd();		
			
		if(pEditor)
			pEditor->Replace(pOptions);

		return 0;
	}
	
	LRESULT OnReplaceAllClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		SReplaceOptions* pOptions = GetOptions();

		CChildFrame* pEditor = GetCurrentEditorWnd();
			
		if(pEditor)
		{
			int count = pEditor->ReplaceAll(pOptions);
			CString s;
			s.Format(_T("%d occurrance(s) replaced."), count);
			MessageBox((LPCTSTR)s, _T("Programmers Notepad"), MB_OK | MB_ICONINFORMATION);
		}

		return 0;
	}
	
	LRESULT OnReplaceInSelectionClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		SReplaceOptions* pOptions = GetOptions();

		CChildFrame* pEditor = GetCurrentEditorWnd();

		if(pEditor)
		{
			pOptions->InSelection = true;

			pEditor->ReplaceAll(pOptions);
		}

		return 0;
	}

	// Shared properties should be defined in CSearchDlg
	friend CSearchDlg<CReplaceDlg>;

protected:
	BXT::CComboBoxAC		m_FindTextCombo;
	BXT::CComboBoxAC		m_ReplaceTextCombo;
	CArrowButton			m_ReHelperBtn;
	CArrowButton			m_ReHelperBtn2;

	CString	m_FindText;
	CString m_ReplaceText;
	int		m_Direction;
	BOOL	m_bMatchCase;
	BOOL	m_bMatchWhole;
	BOOL	m_bRegExp;
	BOOL	m_bUseSlashes;
	//BOOL	m_bSearchAll;
};

#endif // !finddlg_h_included
