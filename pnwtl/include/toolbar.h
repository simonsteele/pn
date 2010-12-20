/**
 * @file toolbar.h
 * @brief Main Toolbar for Programmer's Notepad
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef toolbar_h__included
#define toolbar_h__included

#include "CustomAutoComplete.h"	// Autocompletion.
#include "include/accombo.h"	// Autocompleting combo box.

#define TBR_FILE 103

namespace toolbar {

const int tbcxSeparator = 8;
const TBBUTTON allButtons[] = 
{
	{ 0, ID_FILE_NEW, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"New" },
	{ 1, ID_FILE_OPEN, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Open" },
	{ tbcxSeparator, 0, TBSTATE_ENABLED, BTNS_SEP, {0}, 0, 0},
	{ 2, ID_FILE_SAVE, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Save" },
	{ 3, ID_FILE_SAVEALL, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Save All" },
	{ tbcxSeparator, 0, TBSTATE_ENABLED, BTNS_SEP, {0}, 0, 0},
	{ 4, ID_FILE_CLOSE, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Close" },
	{ tbcxSeparator, 0, TBSTATE_ENABLED, BTNS_SEP, {0}, 0, 0},
	{ 5, ID_EDIT_UNDO, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Undo" },
	{ 6, ID_EDIT_REDO, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Redo" },
	{ tbcxSeparator, 0, TBSTATE_ENABLED, BTNS_SEP, {0}, 0, 0},
	{ 7, ID_EDIT_CUT, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Cut" },
	{ 8, ID_EDIT_COPY, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Copy" },
	{ 9, ID_EDIT_PASTE, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Paste" },
	{ tbcxSeparator, 0, TBSTATE_ENABLED, BTNS_SEP, {0}, 0, 0},
	{ -1, ID_PLACEHOLDER_SCHEMECOMBO, TBSTATE_ENABLED, BTNS_SEP, {0}, 0, (INT_PTR)L"Scheme Chooser" },
	{ tbcxSeparator, 0, TBSTATE_ENABLED, BTNS_SEP, {0}, 0, 0},
	{ 11, ID_EDIT_FINDINFILES, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Find in Files" },
	{ -1, ID_PLACEHOLDER_FINDCOMBO, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Find Text" },
	{ 13, ID_FINDTYPE_BUTTON, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Find" },
	
	// Not included by default
	{ 14, ID_FILE_NEW_PROJECT, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"New Project" },
	{ 17, ID_FILE_NEW_WORKSPACE, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"New Project Group" },
	{ 15, ID_VIEW_ZOOM_IN, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Zoom In" },
	{ 16, ID_VIEW_ZOOM_OUT, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Zoom Out" },
	{ 18, ID_TOOLS_RECORDSCRIPT, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Record Script" },
	{ 19, ID_TOOLS_STOPRECORDING, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Stop Recording" },
	{ 20, ID_FILE_REVERT, TBSTATE_ENABLED, BTNS_BUTTON, {0}, 0, (INT_PTR)L"Revert" },
	
	// Old Mini toolbar toggles
	{ 21, ID_EDITOR_COLOURISE, TBSTATE_ENABLED, BTNS_CHECK, {0}, 0, (INT_PTR)L"Toggle Syntax Highlighting" },
	{ 22, ID_EDITOR_WORDWRAP, TBSTATE_ENABLED, BTNS_CHECK, {0}, 0, (INT_PTR)L"Toggle Word Wrap" },
	{ 23, ID_EDITOR_LINENOS, TBSTATE_ENABLED, BTNS_CHECK, {0}, 0, (INT_PTR)L"Toggle Line Numbers" },
	{ 24, ID_EDITOR_WHITESPACE, TBSTATE_ENABLED, BTNS_CHECK, {0}, 0, (INT_PTR)L"Toggle Visible Whitespace" },
	{ 25, ID_EDITOR_EOLCHARS, TBSTATE_ENABLED, BTNS_CHECK, {0}, 0, (INT_PTR)L"Toggle Visible Line Endings" },
	{ 26, ID_EDITOR_WRITEPROTECT, TBSTATE_ENABLED, BTNS_CHECK, {0}, 0, (INT_PTR)L"Toggle Read Only" },

};

// Random values we use
enum {
	DEFAULT_TOOLBAR_COUNT = 20,
	TOOLBAR_STATE_VERSION = 1,
	ALL_BUTTONS_COUNT = sizeof(allButtons) / sizeof(TBBUTTON),
};

/**
 * Struct used to head up the saved state file.
 */
typedef struct tagSavedState
{
	int stateVersion;
	int btnInfoSize;
	int buttonCount;
} SavedState;

} // namespace toolbar

/**
 * Toolbar class to handle customization
 */
class CPNToolBar : public CWindowImpl<CPNToolBar, CToolBarCtrl>
{
	typedef CWindowImpl<CPNToolBar, CToolBarCtrl> baseClass;

public:	
	CPNToolBar() : m_LowColor(false) {}

	BEGIN_MSG_MAP(CPNToolBar)
		MESSAGE_HANDLER(PN_NOTIFY, OnNotify)

		// Chained from the main frame:
		ALT_MSG_MAP(1)
		NOTIFY_CODE_HANDLER(TBN_GETBUTTONINFO, OnGetButtonInfo)
		NOTIFY_CODE_HANDLER(TBN_QUERYINSERT, OnQueryInsert)
		NOTIFY_CODE_HANDLER(TBN_QUERYDELETE, OnQueryDelete)
		NOTIFY_CODE_HANDLER(TBN_TOOLBARCHANGE, OnToolbarChange)
		NOTIFY_CODE_HANDLER(TBN_DROPDOWN, OnToolbarDropDown)
		NOTIFY_CODE_HANDLER(TBN_BEGINADJUST, OnToolbarBeginAdjust)
		NOTIFY_CODE_HANDLER(TBN_ENDADJUST, OnToolbarEndAdjust)
		NOTIFY_CODE_HANDLER(TBN_RESET, OnToolbarReset)
		MESSAGE_HANDLER(WM_CTLCOLORLISTBOX, OnCtlColor)
		COMMAND_ID_HANDLER(ID_TOOLS_CUSTOMIZETOOLBAR, OnCustomizeToolbar)
	END_MSG_MAP()

	/**
	 * Set whether to use 24-bit color toolbar images.
	 */
	void SetLowColor(bool lowColor)
	{
		m_LowColor = lowColor;
	}
	
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
		if ((LPCTSTR)wt == NULL)
		{
			return _T("");
		}

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

private:

	enum {
		SCHEME_COMBO_SIZE = 24, /* characters */
		FIND_COMBO_SIZE = 30,
		TOOLBAR_COMBO_DROPLINES = 16,
	};

	/**
	 * Notifications from this class to do UI updates...
	 */
	LRESULT OnNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
	{
		if (lParam == PN_UPDATEDISPLAY)
		{
			fixUpButtons();
		}

		return 0;
	}

	/** 
	 * Called by the toolbar customisation window until we return false to 
	 * get information about buttons to display.
	 */
	LRESULT OnGetButtonInfo(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
	{
		LPTBNOTIFY lpTbNotify = reinterpret_cast<LPTBNOTIFY>(pnmh);
		
		int buttonCount = sizeof(toolbar::allButtons) / sizeof(TBBUTTON);
        if (lpTbNotify->iItem < buttonCount)
        {
            lpTbNotify->tbButton = toolbar::allButtons[lpTbNotify->iItem];
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

	/**
	 * The user has re-arranged toolbar buttons, update the display.
	 */
	LRESULT OnToolbarChange(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& /*bHandled*/)
	{
		fixUpButtons();

		// Make sure the UpdateUI state is correct for the toolbar buttons.
		GetParent().PostMessage(PN_NOTIFY, 0, PN_REFRESHUPDATEUI);

		return TRUE;
	}
	
	/**
	 * Drop down button handler
	 */
	LRESULT OnToolbarDropDown(WPARAM /*wParam*/, LPNMHDR lParam, BOOL& /*bHandled*/)
	{
		LPNMTOOLBAR lpnmTB = reinterpret_cast<LPNMTOOLBAR>(lParam);
		
		switch(lParam->idFrom)
		{
			case TBR_FILE:
			{
				CSPopupMenu popup(IDR_POPUP_FINDBARDD);
				
				CPoint pt(lpnmTB->rcButton.left, lpnmTB->rcButton.bottom);
				::ClientToScreen(lParam->hwndFrom, &pt);

				g_Context.m_frame->TrackPopupMenu(popup, 0, pt.x, pt.y);
			};
			break;
		}

		return 0;
	}

	/**
	 * Toolbar adjustment process beginning.
	 */
	LRESULT OnToolbarBeginAdjust(WPARAM /*wParam*/, LPNMHDR lParam, BOOL& /*bHandled*/)
	{
		//LPNMTOOLBAR lpnmTB = reinterpret_cast<LPNMTOOLBAR>(lParam);

		return 0;
	}

	/**
	 * User wants to reset the toolbar.
	 */
	LRESULT OnToolbarReset(WPARAM /*wParam*/, LPNMHDR lParam, BOOL& /*bHandled*/)
	{
		int buttons = GetButtonCount();
		while (buttons)
		{
			DeleteButton(0);
			buttons--;
		}

		addDefaultButtons();
		resetSpecialButtons();
		fixUpButtons();

		return 0;
	}

	/**
	 * The user has finished modifying the toolbar configuration
	 */
	LRESULT OnToolbarEndAdjust(WPARAM /*wParam*/, LPNMHDR lParam, BOOL& /*bHandled*/)
	{
		// Save...
		saveButtons();

		return 0;
	}

	/**
	 * The user wants to customize the toolbar.
	 */
	LRESULT OnCustomizeToolbar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		Customize();

		return 0;
	}

	/**
	 * For some reason the WM_CTLCOLORLISTBOX message from the combo is not getting
	 * a satisfactory result somewhere resulting in a black background. This fixes that.
	 */
	LRESULT OnCtlColor(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		CDCHandle dc( (HDC) wParam );

		dc.SetTextColor( ::GetSysColor(COLOR_WINDOWTEXT) );
		dc.SetBkColor( ::GetSysColor(COLOR_WINDOW) );
		
		return (LRESULT)::GetSysColorBrush( COLOR_WINDOW );
	}

	/**
	 * Initialise the toolbar - setup extended styles and combo boxes
	 */
	void init()
	{
		HWND hWndOwner = GetParent();

		// Set TBSTYLE_FLAT here rather than in the create method to do the following:
		// 1. Make split buttons render with a flat drop-down
		// 2. Avoid weird drawing issues where the background shows through the toolbar.
		::SetWindowLong(m_hWnd, GWL_STYLE, ::GetWindowLong(m_hWnd, GWL_STYLE) | TBSTYLE_FLAT);
		SetExtendedStyle(GetExtendedStyle() | /*TBSTYLE_EX_HIDECLIPPEDBUTTONS |*/ TBSTYLE_EX_MIXEDBUTTONS | TBSTYLE_EX_DRAWDDARROWS | TBSTYLE_EX_DOUBLEBUFFER);

		::SendMessage(m_hWnd, TB_BUTTONSTRUCTSIZE, sizeof(TBBUTTON), 0L);

		// Add images
		
		
		HBITMAP hBitmap;
		if (m_LowColor)
		{
			m_Images.Create(16, 16, ILC_COLOR24 | ILC_MASK, 20, 4);
			hBitmap = (HBITMAP)::LoadImage(
											_Module.m_hInst,
											MAKEINTRESOURCE(IDB_TBMAIN24),
											IMAGE_BITMAP, 0, 0, LR_SHARED);
		}
		else
		{
			m_Images.Create(16, 16, ILC_COLOR32 | ILC_MASK, 20, 4);
			hBitmap = (HBITMAP)::LoadImage(
											_Module.m_hInst,
											MAKEINTRESOURCE(IDB_TOOLBAR),
											IMAGE_BITMAP, 0, 0, LR_SHARED | LR_CREATEDIBSECTION);
		}

		m_Images.Add(hBitmap, RGB(255,0,255));
		SetImageList(m_Images);

		// check if font is taller than our bitmaps
		CFontHandle font = (HFONT)::SendMessage(m_hWnd, WM_GETFONT, 0, 0L);
		if (font.IsNull())
		{
			font = AtlGetDefaultGuiFont();
		}

		LOGFONT lf = { 0 };
		font.GetLogFont(lf);
		WORD cyFontHeight = (WORD)abs(lf.lfHeight);

		loadButtons();

		// Add them and set button sizes etc.
		::SendMessage(m_hWnd, TB_SETBITMAPSIZE, 0, MAKELONG(16, max(16, cyFontHeight)));
		/*const int cxButtonMargin = 7;
		const int cyButtonMargin = 2;
		::SendMessage(m_hWnd, TB_SETBUTTONSIZE, 0, MAKELONG(16 + cxButtonMargin, max(16, cyFontHeight) + cyButtonMargin));*/

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

		int index = m_SchemeCombo.AddString(pM->GetDefaultScheme()->GetTitle());
		m_SchemeCombo.SetItemDataPtr( index, pM->GetDefaultScheme() );

		for(SCIT i = pSchemes->begin(); i != pSchemes->end(); ++i)
		{
			index = m_SchemeCombo.AddString((*i).GetTitle());
			m_SchemeCombo.SetItemDataPtr(index, &(*i));
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
		
		resetSpecialButtons();
		fixUpButtons();
	}

	/**
	 * Add the default button set.
	 */
	void addDefaultButtons()
	{
		PNASSERT(toolbar::DEFAULT_TOOLBAR_COUNT <= (sizeof(toolbar::allButtons)/sizeof(TBBUTTON)));

#ifndef _UNICODE
		m_nonUnicodeTitles.clear();
#endif

		// Make our buttons:
		std::vector<TBBUTTON> buttons(toolbar::DEFAULT_TOOLBAR_COUNT);
		for (int i = 0; i < toolbar::DEFAULT_TOOLBAR_COUNT; i++)
		{
			buttons[i] = toolbar::allButtons[i];

#ifndef _UNICODE
			// We apply an ASCII version of the title for adding, and use the unicode for customisation
			if (buttons[i].iString != NULL)
			{
				m_nonUnicodeTitles.push_back(std::string(CW2CT((LPCWSTR)toolbar::allButtons[i].iString)));
				buttons[i].iString = (INT_PTR)((*m_nonUnicodeTitles.rbegin()).c_str());
			}
#endif
		}

		AddButtons(toolbar::DEFAULT_TOOLBAR_COUNT, &buttons[0]);
	}

	/**
	 * Get the font metrics for the UI font.
	 */
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

	/**
	 * Move a combo box in the toolbar if it's visible.
	 */
	void moveCombo(CComboBox& combo, DWORD dwComboId, int dropSize)
	{
		CRect rc;
		int nIndex = CommandToIndex(dwComboId);
		
		if (nIndex == -1)
		{
			combo.ShowWindow(SW_HIDE);
		}
		else
		{
			GetItemRect(nIndex, &rc);

			rc.bottom = rc.top + dropSize;
			rc.left += 1; // slight offset from previous and next buttons.
			rc.right -= 1;
			combo.SetWindowPos(NULL, rc, SWP_NOSIZE | SWP_NOZORDER);
			combo.ShowWindow(SW_SHOW);
		}
	}

	/**
	 * Position the combo boxes and update the findtype button
	 */
	void fixUpButtons()
	{
		CSize sizeChar = getGUIFontSize();
		int dropSize = TOOLBAR_COMBO_DROPLINES * sizeChar.cy;

		moveCombo(m_FindCombo, ID_PLACEHOLDER_FINDCOMBO, dropSize);
		moveCombo(m_SchemeCombo, ID_PLACEHOLDER_SCHEMECOMBO, dropSize);
	}

	/**
	 * Set up the buttons on the toolbar that hold the combo boxes
	 */
	void resetSpecialButtons()
	{
		CSize sizeChar = getGUIFontSize();

		TBBUTTONINFO tbi = {0};

		tbi.cbSize = sizeof(TBBUTTONINFO);
		tbi.dwMask = TBIF_STYLE | TBIF_SIZE;
		tbi.fsStyle = TBSTYLE_SEP;
		
		// Scheme Combo Sep:
		tbi.cx = (unsigned short)SCHEME_COMBO_SIZE * sizeChar.cx;
		SetButtonInfo(ID_PLACEHOLDER_SCHEMECOMBO, &tbi);

		// Find Combo Sep:
		tbi.cx = (unsigned short)FIND_COMBO_SIZE * sizeChar.cx;
		SetButtonInfo(ID_PLACEHOLDER_FINDCOMBO, &tbi);

		// Now the FindType drop-down button
		int index = CommandToIndex(ID_FINDTYPE_BUTTON);
		if (index != -1)
		{
			GetButtonInfo(ID_FINDTYPE_BUTTON, &tbi);
			tbi.dwMask = TBIF_STYLE;
			tbi.fsStyle = BTNS_BUTTON | BTNS_AUTOSIZE | BTNS_DROPDOWN | BTNS_SHOWTEXT;
			SetButtonInfo(ID_FINDTYPE_BUTTON, &tbi);
		}
	}

	/**
	 * Try to load the toolbar buttons from the disk, falling back to default buttons.
	 */
	void loadButtons()
	{
		tstring path;
		OPTIONS->GetPNPath(path, PNPATH_USERSETTINGS);

		CFileName fn(_T("toolbar.dat"));
		fn.Root(path.c_str());

		bool bLoaded(false);

		if (FileExists(fn.c_str()))
		{
			FILE* f = _tfopen(fn.c_str(), _T("rb"));
			
			if (f)
			{
				toolbar::SavedState state;
				if ((fread(&state, sizeof(state), 1, f) == 1) && 
					(state.stateVersion == toolbar::TOOLBAR_STATE_VERSION) &&
					state.btnInfoSize == sizeof(TBBUTTON))
				{
					std::vector<TBBUTTON> buttons(state.buttonCount);
					if (fread(&buttons[0], sizeof(TBBUTTON), state.buttonCount, f) == (size_t)state.buttonCount)
					{
						// Fix up the titles:
						for (size_t i = 0; i < buttons.size(); i++)
						{
							for (int j = 0; j < toolbar::ALL_BUTTONS_COUNT; j++)
							{
								if (toolbar::allButtons[j].idCommand == buttons[i].idCommand)
								{
									buttons[i].iString = toolbar::allButtons[j].iString;
									break;
								}
							}

							// Remove styles that break the toolbar height, they'll be re-applied later
							buttons[i].fsStyle &= ~(BTNS_AUTOSIZE | BTNS_DROPDOWN | BTNS_SHOWTEXT);
							buttons[i].fsState = TBSTATE_ENABLED;

#ifndef _UNICODE
							// We apply an ASCII version of the title for adding, and use the unicode for customisation
							if (buttons[i].iString != NULL)
							{
								m_nonUnicodeTitles.push_back(std::string(CW2CT((LPCWSTR)buttons[i].iString)));
								buttons[i].iString = (INT_PTR)((*m_nonUnicodeTitles.rbegin()).c_str());
							}
#endif
						}

						AddButtons(buttons.size(), &buttons[0]);
						bLoaded = true;
					}
				}

				fclose(f);
			}
		}
		
		if (!bLoaded)
		{
			addDefaultButtons();
		}
	}

	/**
	 * Save the toolbar state to user settings.
	 */
	void saveButtons()
	{
		tstring path;
		OPTIONS->GetPNPath(path, PNPATH_USERSETTINGS);

		CFileName fn(_T("toolbar.dat"));
		fn.Root(path.c_str());

		FILE* f = _tfopen(fn.c_str(), _T("wb"));
		if (f)
		{
			toolbar::SavedState state = {toolbar::TOOLBAR_STATE_VERSION, sizeof(TBBUTTON), GetButtonCount()};
			fwrite(&state, sizeof(state), 1, f);
			
			TBBUTTON btn;
			for (int i = 0; i < state.buttonCount; i++)
			{
				GetButton(i, &btn);
				btn.iString = 0;
				fwrite(&btn, sizeof(TBBUTTON), 1, f);
			}

			fclose(f);
		}
	}

	CComboBox				m_SchemeCombo;
	BXT::CComboBoxAC		m_FindCombo;
	CImageList				m_Images;
	std::vector<TBBUTTON>	m_ResetConfig;
	bool					m_LowColor;
	
#ifndef _UNICODE
	std::list<std::string>	m_nonUnicodeTitles;
#endif
};

#endif //#ifndef toolbar_h__included