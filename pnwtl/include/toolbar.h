#ifndef toolbar_h__included
#define toolbar_h__included

#include "CustomAutoComplete.h"	// Autocompletion.
#include "include/accombo.h"	// Autocompleting combo box.

const TBBUTTON allButtons[] = 
{
	{ 0, ID_FILE_NEW, TBSTATE_ENABLED, 0, {0}, 0, (INT_PTR)L"New" },
	{ 1, ID_FILE_OPEN, TBSTATE_ENABLED, 0, {0}, 0, (INT_PTR)L"Open" },
	{ 2, ID_FILE_SAVE, TBSTATE_ENABLED, 0, {0}, 0, (INT_PTR)L"Save" },
	{ 3, ID_FILE_SAVEALL, TBSTATE_ENABLED, 0, {0}, 0, (INT_PTR)L"Save All" },
	{ 4, ID_FILE_CLOSE, TBSTATE_ENABLED, 0, {0}, 0, (INT_PTR)L"Close" },
	{ 5, ID_EDIT_UNDO, TBSTATE_ENABLED, 0, {0}, 0, (INT_PTR)L"Undo" },
	{ 6, ID_EDIT_REDO, TBSTATE_ENABLED, 0, {0}, 0, (INT_PTR)L"Redo" },
	{ 7, ID_EDIT_CUT, TBSTATE_ENABLED, 0, {0}, 0, (INT_PTR)L"Cut" },
	{ 8, ID_EDIT_COPY, TBSTATE_ENABLED, 0, {0}, 0, (INT_PTR)L"Copy" },
	{ 9, ID_EDIT_PASTE, TBSTATE_ENABLED, 0, {0}, 0, (INT_PTR)L"Paste" },
	{ -1, ID_PLACEHOLDER_SCHEMECOMBO, TBSTATE_ENABLED, 0, {0}, 0, (INT_PTR)L"Scheme Chooser" },
	{ 11, ID_EDIT_FINDINFILES, TBSTATE_ENABLED, 0, {0}, 0, (INT_PTR)L"Find in Files" },
	{ -1, ID_PLACEHOLDER_FINDCOMBO, TBSTATE_ENABLED, 0, {0}, 0, (INT_PTR)L"Find Text" },
	{ 13, ID_FINDTYPE_BUTTON, TBSTATE_ENABLED, 0, {0}, 0, (INT_PTR)L"Find" },
};

/**
 * Toolbar class to handle customization
 */
class CPNToolBar : public CWindowImpl<CPNToolBar, CToolBarCtrl>
{
	typedef CWindowImpl<CPNToolBar, CToolBarCtrl> baseClass;

public:	
	BEGIN_MSG_MAP(CPNToolBar)
		REFLECTED_NOTIFY_CODE_HANDLER(TBN_GETBUTTONINFO, OnGetButtonInfo)
		REFLECTED_NOTIFY_CODE_HANDLER(TBN_QUERYINSERT, OnQueryInsert)
		REFLECTED_NOTIFY_CODE_HANDLER(TBN_QUERYDELETE, OnQueryDelete)
		REFLECTED_NOTIFY_CODE_HANDLER(TBN_TOOLBARCHANGE, OnToolbarChange)
		DEFAULT_REFLECTION_HANDLER()
	END_MSG_MAP()

	/**
	 * Add a string to the find combo
	 */
	void AddFindText(LPCTSTR text)
	{
		m_FindCombo.AddString(text);
	}

	/**
	 * Toggle the scheme combo
	 */
	void EnableSchemeCombo(bool enable)
	{
		m_SchemeCombo.EnableWindow(enable);
	}

	/**
	 * Get the find text
	 */
	tstring GetFindText() const
	{
		CWindowText wt(m_FindCombo.m_hWnd); //Get find text...
		return tstring((LPCTSTR)wt);
	}

	/**
	 * Get the scheme currently selected in the scheme combo box.
	 */
	Scheme* GetSelectedScheme() const
	{
		int iSel = m_SchemeCombo.GetCurSel();
		return static_cast<Scheme*>(m_SchemeCombo.GetItemDataPtr(iSel));
	}

	/**
	 * Select a scheme
	 */
	void SelectScheme(Scheme* scheme)
	{
		for (int i = 0; i < m_SchemeCombo.GetCount(); i++)
		{
			if (scheme == static_cast<Scheme*>(m_SchemeCombo.GetItemDataPtr(i)))
			{
				m_SchemeCombo.SetCurSel(i);
				break;
			}
		}
	}

	/**
	 * Set the current text in the find combo.
	 */
	void SetFindText(LPCTSTR text)
	{
		m_FindCombo.SetWindowText(text);
	}

	/**
	 * Subclass an existing toolbar window.
	 */
	BOOL SubclassWindow(HWND hWnd)
	{
		ATLASSERT(m_hWnd==NULL);
		ATLASSERT(::IsWindow(hWnd));
		BOOL bRet = baseClass::SubclassWindow(hWnd);
		if (bRet)
		{
			init();
		}

		return bRet;
	}

	/** 
	 * Called by the toolbar customisation window until we return false to 
	 * get information about buttons to display.
	 */
	LRESULT OnGetButtonInfo(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
	{
		LPTBNOTIFY lpTbNotify = reinterpret_cast<LPTBNOTIFY>(pnmh);
		
		int buttonCount = sizeof(allButtons) / sizeof(TBBUTTON);
        if (lpTbNotify->iItem < buttonCount)
        {
            lpTbNotify->tbButton = allButtons[lpTbNotify->iItem];
			return TRUE;
		}

		return FALSE;
	}

	/**
	 * Check whether an item can be inserted - we always say yes.
	 */
	LRESULT OnQueryInsert(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& /*bHandled*/)
	{
		return TRUE;
	}

	/**
	 * Check whether an item can be deleted - we always say yes.
	 */
	LRESULT OnQueryDelete(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& /*bHandled*/)
	{
		return TRUE;
	}

	LRESULT OnToolbarChange(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& /*bHandled*/)
	{
		updateComboPositions();

		return TRUE;
	}

private:

	enum {
		SCHEME_COMBO_SIZE = 24, /* characters */
		FIND_COMBO_SIZE = 30,
		TOOLBAR_COMBO_DROPLINES = 16,
	};

	void init()
	{
		HWND hWndOwner = GetParent();

		// Set TBSTYLE_FLAT here rather than in the create method to do the following:
		// 1. Make split buttons render with a flat drop-down
		// 2. Avoid weird drawing issues where the background shows through the toolbar.
		::SetWindowLong(m_hWnd, GWL_STYLE, ::GetWindowLong(m_hWnd, GWL_STYLE) | TBSTYLE_FLAT);
		SetExtendedStyle(GetExtendedStyle() | /*TBSTYLE_EX_HIDECLIPPEDBUTTONS |*/ TBSTYLE_EX_MIXEDBUTTONS | TBSTYLE_EX_DRAWDDARROWS | TBSTYLE_EX_DOUBLEBUFFER);

		CSize sizeChar = getGUIFontSize();
		int cx = FIND_COMBO_SIZE * sizeChar.cx;

		TBBUTTONINFO tbi;
		RECT rc;
		
		int cxsc = SCHEME_COMBO_SIZE * sizeChar.cx;

		tbi.cbSize = sizeof TBBUTTONINFO;
		tbi.dwMask = TBIF_STYLE | TBIF_SIZE;
		tbi.fsStyle = TBSTYLE_SEP;
		tbi.cx = (unsigned short)cxsc;
		
		SetButtonInfo(ID_PLACEHOLDER_SCHEMECOMBO, &tbi);
		
		int nIndex = CommandToIndex(ID_PLACEHOLDER_SCHEMECOMBO);
		GetItemRect(nIndex, &rc); 

		rc.bottom = rc.top + TOOLBAR_COMBO_DROPLINES * sizeChar.cy;
		
		HWND hWndCombo =  m_SchemeCombo.Create(hWndOwner, rc, NULL, CBS_DROPDOWNLIST | CBS_AUTOHSCROLL | WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL, 0, IDC_SCHEMECOMBO);
		hWndCombo;
		ATLASSERT(hWndCombo != 0);
		
		m_SchemeCombo.SetParent(m_hWnd); 
		m_SchemeCombo.SetFont((HFONT)GetStockObject( DEFAULT_GUI_FONT ));

		SchemeManager* pM = SchemeManager::GetInstance();
		SCHEME_LIST* pSchemes = pM->GetSchemesList();

		int index = m_SchemeCombo.AddString( pM->GetDefaultScheme()->GetTitle() );
		m_SchemeCombo.SetItemDataPtr( index, pM->GetDefaultScheme() );

		for(SCIT i = pSchemes->begin(); i != pSchemes->end(); ++i)
		{
			index = m_SchemeCombo.AddString( (*i).GetTitle() );
			m_SchemeCombo.SetItemDataPtr( index, &(*i) );
		}

		tbi.cbSize = sizeof TBBUTTONINFO;
		tbi.dwMask = TBIF_STYLE | TBIF_SIZE;
		tbi.fsStyle = TBSTYLE_SEP;
		tbi.cx = (unsigned short)cx;

		SetButtonInfo(ID_PLACEHOLDER_FINDCOMBO, &tbi);
		
		nIndex = CommandToIndex(ID_PLACEHOLDER_FINDCOMBO);
		GetItemRect(nIndex, &rc);

		rc.bottom = TOOLBAR_COMBO_DROPLINES * sizeChar.cy;
		rc.left += 1; // slight offset from previous and next buttons.
		rc.right -= 1;

		hWndCombo = m_FindCombo.Create(hWndOwner, rc, _T("FINDTEXTCOMBO"), 
			CBS_DROPDOWN | CBS_AUTOHSCROLL | WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL,
			0, IDC_FINDCOMBO, _T("FindToolbar"));
		hWndCombo;
		ATLASSERT(hWndCombo != 0);

		m_FindCombo.SetParent(m_hWnd);
		m_FindCombo.SetFont((HFONT)GetStockObject( DEFAULT_GUI_FONT ));
		m_FindCombo.SetOwnerHWND(hWndOwner); // Get enter notifications.

		// Set the drop-down button...	
		tstring fs = LS(IDS_FIND);

		// Add drop-down style to the button
		tbi.dwMask = TBIF_STYLE;
		GetButtonInfo(ID_FINDTYPE_BUTTON, &tbi);
		tbi.fsStyle |= BTNS_DROPDOWN;
		tbi.fsStyle |= BTNS_SHOWTEXT | BTNS_AUTOSIZE;
		tbi.dwMask |= TBIF_TEXT;
		tbi.pszText = const_cast<LPSTR>(fs.c_str());
		SetButtonInfo(ID_FINDTYPE_BUTTON, &tbi);
	}

	CSize getGUIFontSize()
	{
		CClientDC dc(m_hWnd);
		dc.SelectFont((HFONT) GetStockObject( DEFAULT_GUI_FONT ));		
		TEXTMETRIC tm;
		dc.GetTextMetrics( &tm );
		//int cxChar = tm.tmAveCharWidth;
		//int cyChar = tm.tmHeight + tm.tmExternalLeading;

		return CSize( tm.tmAveCharWidth, tm.tmHeight + tm.tmExternalLeading);
	}

	void updateComboPositions()
	{
		CRect rc;
		CSize sizeChar = getGUIFontSize();

		int nIndex = CommandToIndex(ID_PLACEHOLDER_FINDCOMBO);
		GetItemRect(nIndex, &rc);

		rc.bottom = TOOLBAR_COMBO_DROPLINES * sizeChar.cy;
		rc.left += 1; // slight offset from previous and next buttons.
		rc.right -= 1;
		m_FindCombo.SetWindowPos(NULL, rc, SWP_NOSIZE | SWP_NOZORDER);

		nIndex = CommandToIndex(ID_PLACEHOLDER_SCHEMECOMBO);
		GetItemRect(nIndex, &rc); 
		rc.left += 1; // slight offset from previous and next buttons.
		rc.right -= 1;
		rc.bottom = rc.top + TOOLBAR_COMBO_DROPLINES * sizeChar.cy;
		m_SchemeCombo.SetWindowPos(NULL, rc, SWP_NOSIZE | SWP_NOZORDER);
	}

	CComboBox				m_SchemeCombo;
	BXT::CComboBoxAC		m_FindCombo;
};

#endif //#ifndef toolbar_h__included