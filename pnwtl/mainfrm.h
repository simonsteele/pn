/**
 * @file mainfrm.h
 * @brief Main Window for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * Notes: It might be better to try and use a single instance of the child frame
 * menu instead of all the switching work that is done in the current version.
 */

#if !defined(MAINFRM_H__INCLUDED)
#define MAINFRM_H__INCLUDED

#if _MSC_VER >= 1000
	#pragma once
#endif

#include "files.h"

#include "SchemeConfig.h"

BOOL CALLBACK CloseChildEnumProc(HWND hWnd, LPARAM lParam);

/**
 * @class CMainFrame
 * @brief PN (WTL Edition) Main MDI Frame
 */
class CMainFrame : public CTabbedMDIFrameWindowImpl<CMainFrame, CPNMDIClient>, public IMainFrame, public CUpdateUI<CMainFrame>,
		public CMessageFilter, public CIdleHandler, public CSMenuEventHandler
{
public:
	DECLARE_FRAME_WND_CLASS(NULL, IDR_MAINFRAME)

	typedef CTabbedMDIFrameWindowImpl<CMainFrame, CPNMDIClient> baseClass;

	////////////////////////////////////////////////////////////////
	// CMainFrame Implementation

	CMainFrame() : m_RecentFiles(ID_MRUFILE_BASE, 4)
	{
		m_FindDialog = NULL;
		m_ReplaceDialog = NULL;
		hFindWnd = NULL;
		hReplWnd = NULL;

		m_CmdBar.SetCallback(this, OnMDISetMenu);
	}

	~CMainFrame()
	{
		CloseAndFreeDlg(m_FindDialog);
		CloseAndFreeDlg(m_ReplaceDialog);
	}

	virtual BOOL PreTranslateMessage(MSG* pMsg)
	{
		if(baseClass::PreTranslateMessage(pMsg))
			return TRUE;

		// We have to do the following two to get the modeless dialogs
		// to work. An alternative is to spawn a thread for each and to
		// call DoModal from that thread. 
		if(::IsDialogMessage(hFindWnd, pMsg))
			return TRUE;

		if(::IsDialogMessage(hReplWnd, pMsg))
			return TRUE;

		HWND hWnd = MDIGetActive();
		if(hWnd != NULL)
			return (BOOL)::SendMessage(hWnd, WM_FORWARDMSG, 0, (LPARAM)pMsg);

		return FALSE;
	}

	virtual BOOL OnIdle()
	{
		UIUpdateToolBar();
		return FALSE;
	}

	BEGIN_MSG_MAP(CMainFrame)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_INITMENUPOPUP, OnInitMenuPopup)
		MESSAGE_HANDLER(WM_DROPFILES, OnDropFiles)
		MESSAGE_HANDLER(PN_NOTIFY, OnChildNotify)
		MESSAGE_HANDLER(WM_CLOSE, OnClose)
		MESSAGE_HANDLER(WM_ACTIVATE, OnActivate)

		COMMAND_ID_HANDLER(ID_APP_EXIT, OnFileExit)
		COMMAND_ID_HANDLER(ID_FILE_NEW, OnFileNew)
		COMMAND_ID_HANDLER(ID_FILE_OPEN, OnFileOpen)
		COMMAND_ID_HANDLER(ID_VIEW_TOOLBAR, OnViewToolBar)
		COMMAND_ID_HANDLER(ID_VIEW_TOOLBAR_EDIT, OnViewEditBar)
		COMMAND_ID_HANDLER(ID_VIEW_STATUS_BAR, OnViewStatusBar)
		COMMAND_ID_HANDLER(ID_APP_ABOUT, OnAppAbout)
		COMMAND_ID_HANDLER(ID_WINDOW_CASCADE, OnWindowCascade)
		COMMAND_ID_HANDLER(ID_WINDOW_TILE_HORZ, OnWindowTile)
		COMMAND_ID_HANDLER(ID_WINDOW_ARRANGE, OnWindowArrangeIcons)
		COMMAND_ID_HANDLER(ID_EDIT_FIND, OnFind)
		COMMAND_ID_HANDLER(ID_EDIT_REPLACE, OnReplace)
		COMMAND_ID_HANDLER(ID_TOOLS_OPTIONS, OnOptions)
		COMMAND_RANGE_HANDLER(ID_MRUFILE_BASE, (ID_MRUFILE_BASE+15), OnMRUSelected)
		ROUTE_MENUCOMMANDS()
		CHAIN_MDI_CHILD_COMMANDS()
		CHAIN_MSG_MAP(CUpdateUI<CMainFrame>)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	BEGIN_UPDATE_UI_MAP(CMainFrame)
		UPDATE_ELEMENT(ID_VIEW_TOOLBAR, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_STATUS_BAR, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_TOOLBAR_EDIT, UPDUI_MENUPOPUP)
		//UPDATE_ELEMENT(ID_FILE_CLOSE, UPDUI_MENUPOPUP)
	END_UPDATE_UI_MAP()

	BEGIN_MENU_HANDLER_MAP()
		HANDLE_MENU_COMMAND(SCHEMEMANAGER_SELECTSCHEME, OnSchemeNew)
	END_MENU_HANDLER_MAP()

	LRESULT OnActivate(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
	{
		if(LOWORD(wParam) != WA_INACTIVE && !HIWORD(wParam))
		{
			HWND hMDIChild = MDIGetActive();
			if(hMDIChild != 0)
			{
				::PostMessage(hMDIChild, PN_CHECKAGE, 0, 0);
			}
		}
		
		bHandled = FALSE;
		return 1;
	}

	void OnSchemeNew(LPVOID data)
	{	
		CChildFrame* pChild = NewEditor();
		pChild->SetScheme((CScheme*)data);
	}

	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		// create command bar window
		HWND hWndCmdBar = m_CmdBar.Create(m_hWnd, rcDefault, NULL, ATL_SIMPLE_CMDBAR_PANE_STYLE);
		// attach menu
		m_CmdBar.AttachMenu(GetMenu());
		// load command bar images
		m_CmdBar.LoadImages(IDR_MAINFRAME);
		m_CmdBar.LoadImages(IDR_TBR_EDIT);
		//m_CmdBar.LoadImages(IDR_IMAGES);
		// remove old menu
		SetMenu(NULL);

		HWND hWndToolBar = CreateSimpleToolBarCtrl(m_hWnd, IDR_MAINFRAME, FALSE, ATL_SIMPLE_TOOLBAR_PANE_STYLE);
		HWND hWndEdtToolBar = CreateSimpleToolBarCtrl(m_hWnd, IDR_TBR_EDIT, FALSE, ATL_SIMPLE_TOOLBAR_PANE_STYLE);

		CreateSimpleReBar(ATL_SIMPLE_REBAR_NOBORDER_STYLE);
		AddSimpleReBarBand(hWndCmdBar);
		AddSimpleReBarBand(hWndToolBar, NULL, TRUE);
		AddSimpleReBarBand(hWndEdtToolBar, NULL, FALSE);
		SizeSimpleReBarBands();
		
		CreateSimpleStatusBar(_T(""));

		int statusBarPanes[] =
		{
			ID_POS_PANE,
			ID_MOD_PANE,
			ID_INS_PANE,
			ID_DEFAULT_PANE
		};

		m_StatusBar.SubclassWindow(m_hWndStatusBar);
		m_StatusBar.SetPanes(statusBarPanes, sizeof(statusBarPanes) / sizeof(int), false);
		m_StatusBar.SetPaneWidth(ID_POS_PANE, 120);
		m_StatusBar.SetPaneWidth(ID_MOD_PANE, 70);
		m_StatusBar.SetPaneWidth(ID_INS_PANE, 80);
		
		OSVERSIONINFO osvi;
		ZeroMemory(&osvi, sizeof(OSVERSIONINFO));
		osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
		GetVersionEx (&osvi);
		m_bIsXPOrLater = 
			(osvi.dwPlatformId == VER_PLATFORM_WIN32_NT) &&
			( (osvi.dwMajorVersion > 4) && (osvi.dwMinorVersion > 0) );
		
		m_bShowingDefaultStatus = false;
		SetStatusText(NULL);
		
		DragAcceptFiles(TRUE);

		CreateMDIClient();
		m_CmdBar.SetMDIClient(m_hWndMDIClient);

		UIAddToolBar(hWndToolBar);
		UIAddToolBar(hWndEdtToolBar);
		UISetCheck(ID_VIEW_TOOLBAR, 1);
		UISetCheck(ID_VIEW_TOOLBAR_EDIT, 1);
		UISetCheck(ID_VIEW_STATUS_BAR, 1);
		
		// register object for message filtering and idle updates
		CMessageLoop* pLoop = _Module.GetMessageLoop();
		ATLASSERT(pLoop != NULL);
		pLoop->AddMessageFilter(this);
		pLoop->AddIdleHandler(this);

		// Initialise our popup menus.
		m_Switcher.Reset(MENUMESSAGE_CHANGESCHEME);
		CSchemeManager::GetInstance()->BuildMenu((HMENU)m_NewMenu, this);
		
		CString mrukey;
		mrukey = pnregroot;
		mrukey += pnmrukey;
		m_RecentFiles.SetSize(COptionsManager::GetInstance()->Get(PNSK_INTERFACE, _T("MRUSize"), 4));
		m_RecentFiles.SetRegistryKey(mrukey);
		m_RecentFiles.UpdateMenu();
		
		AddMRUMenu(CSMenuHandle(m_hMenu));
		AddNewMenu(CSMenuHandle(m_hMenu));

		return 0;
	}

	LRESULT OnDropFiles(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		TCHAR	buf[MAX_PATH+1];
		HDROP	hDrop = (HDROP)wParam;
		
		int files = DragQueryFile(hDrop, 0xFFFFFFFF, NULL, 0);
		for(int i = 0; i < files; i++)
		{
			DragQueryFile(hDrop, i, buf, MAX_PATH);
			PNOpenFile(buf);
			AddMRUEntry(buf);
		}

		DragFinish(hDrop);
		
		return 0;
	}

	LRESULT OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
	{
		// Check that all of the child windows are ready to close...
		SCloseStruct s;
		s.pMainFrm = (void*)this;
		s.bCanClose = true;

		EnumChildWindows(m_hWndMDIClient, CloseChildEnumProc, (long)&s);

		if(s.bCanClose)
			bHandled = FALSE;
		
		return 0;
	}

	/**
	 * We now update any menus which are placed in the menus of MDI children
	 * (i.e. the MRU and Scheme menus...). This is called back from 
	 * CPNTabbedMDICommandBarCtrl.
	 */
	void OnMDISetMenu(HMENU hOld, HMENU hNew)
	{
		CSMenuHandle r(hOld);
		CSMenuHandle a(hNew);

		MoveMRU(r, a);
		MoveLanguage(r, a);
		MoveNewMenu(r, a);
	}

	/**
	 * This function incorporates a fix from Nenad Stefanovic which 
	 * re-enables double-clicking to close the window when using WTL 7. 
	 * Should probably be un-necessary with future releases.
	 */
	LRESULT OnInitMenuPopup(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
	{
		HMENU hMenu = (HMENU)wParam;
		
		if(hMenu == NULL)
			return 1;
		
		_AtlUpdateUIData* pUIData = m_pUIData;
		
		if(pUIData == NULL)
			return 1;
		
		if(!(BOOL)HIWORD(lParam))
			::SetMenuDefaultItem(hMenu, (UINT)-1, 0);
		
		const _AtlUpdateUIMap* pMap = m_pUIMap;
		
		while(pMap->m_nID != (WORD)-1)
		{
			if(pMap->m_wType & UPDUI_MENUPOPUP)
			UIUpdateMenuBarElement(pMap->m_nID,
			pUIData, hMenu);
			pMap++;
			pUIData++;
		}

		return DefWindowProc(uMsg, wParam, lParam);
	}

	/**
	 * You should always use this function to make a new editor, it sets
	 * up the close callback properly. If you don't, things will crash :)
	 */
	CChildFrame* NewEditor()
	{
		CChildFrame* pChild = new CChildFrame;
		ATLASSERT(pChild != NULL);

		pChild->CreateEx(m_hWndMDIClient);

		pChild->m_onClose = new CallbackClassPtr<CMainFrame, CChildFrame*, bool>(*this, OnEditorClosing);

		return pChild;
	}

	bool OnEditorClosing(CChildFrame* pChild)
	{
		bool bRet = true;

		if(pChild->GetModified())
		{
			CString title;
			title.Format(_T("Would you like to save changes to:\n%s?"), pChild->GetTitle());
			int res = MessageBox(title, "Programmers Notepad", MB_YESNOCANCEL | MB_ICONQUESTION);
			switch (res)
			{
				case IDYES:
				{
					if( pChild->CanSave() )
					{
						pChild->Save();
					}
					else
					{
						return pChild->SaveAs();
					}
				}
				break;
				case IDCANCEL:
				{
					return false;
				}
			} // switch (res)

		}

		return bRet;
	}

	LRESULT OnFileExit(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		PostMessage(WM_CLOSE);
		return 0;
	}

	LRESULT OnFileNew(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		NewEditor();
		return 0;
	}

	void PNOpenFile(LPCTSTR pathname, LPCTSTR filename, CScheme* pScheme = NULL)
	{
		CChildFrame* pChild = NewEditor();
		if(filename)
		{
			pChild->PNOpenFile(pathname, filename, pScheme);
		}
		else
		{
			ctcString buf;
			CFileName(pathname).GetFileName(buf);
			
			pChild->PNOpenFile(pathname, buf.c_str(), pScheme);
		}
	}

	void PNOpenFile(LPCTSTR pathname, CScheme* pScheme)
	{
		PNOpenFile(pathname, NULL, pScheme);
	}

	void PNOpenFile(LPCTSTR pathname)
	{
		PNOpenFile(pathname, NULL, NULL);
	}

	LRESULT OnFileOpen(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		CPNFileDialog dlgOpen(TRUE, NULL, NULL, OFN_HIDEREADONLY, _T("All Files (*.*)|*.*|"), m_hWndClient);
		if (dlgOpen.DoModal() == IDOK)
		{
			PNOpenFile(dlgOpen.m_ofn.lpstrFile, dlgOpen.m_ofn.lpstrFileTitle);
			AddMRUEntry(dlgOpen.m_ofn.lpstrFile);
		}

		return 0;
	}

	LRESULT OnMRUSelected(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		LPCTSTR filename = m_RecentFiles.GetEntry(wID - ID_MRUFILE_BASE);
		PNOpenFile(filename);

		return 0;
	}

	LRESULT OnViewToolBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		static BOOL bVisible = TRUE;	// initially visible
		bVisible = !bVisible;
		CReBarCtrl rebar = m_hWndToolBar;
		int nBandIndex = rebar.IdToIndex(ATL_IDW_BAND_FIRST + 1);	// toolbar is 2nd added band
		rebar.ShowBand(nBandIndex, bVisible);
		UISetCheck(ID_VIEW_TOOLBAR, bVisible);
		UpdateLayout();
		return 0;
	}

	LRESULT OnViewEditBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		static BOOL bVisible = TRUE; // initially visible
		bVisible = !bVisible;
		CReBarCtrl rebar = m_hWndToolBar;
		int index = rebar.IdToIndex(ATL_IDW_BAND_FIRST + 2);
		rebar.ShowBand(index, bVisible);
		UISetCheck(ID_VIEW_TOOLBAR_EDIT, bVisible);
		UpdateLayout();
		return 0;
	}

	LRESULT OnViewStatusBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		BOOL bVisible = !::IsWindowVisible(m_hWndStatusBar);
		::ShowWindow(m_hWndStatusBar, bVisible ? SW_SHOWNOACTIVATE : SW_HIDE);
		UISetCheck(ID_VIEW_STATUS_BAR, bVisible);
		UpdateLayout();
		return 0;
	}

	LRESULT OnAppAbout(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		CAboutDlg dlg;
		dlg.DoModal();
		return 0;
	}

	LRESULT OnWindowCascade(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		MDICascade();
		return 0;
	}

	LRESULT OnWindowTile(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		MDITile();
		return 0;
	}

	LRESULT OnWindowArrangeIcons(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		MDIIconArrange();
		return 0;
	}

	LRESULT OnChildNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
	{
		if(lParam == SCN_UPDATEUI)
		{
			// Update the status bar when Scintilla thinks that we should.
			UpdateStatusBar();
		}
				
		return TRUE;
	}

	LRESULT OnOptions(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		COptionsPageStyle		page;
		
		CSchemeManager* pSM = CSchemeManager::GetInstance();
		SchemeConfigParser		schemeconfig;
		schemeconfig.LoadConfig(pSM->GetPath(), pSM->GetCompiledPath());

		COptionsPageSchemes		page2(&schemeconfig);

		COptionsDialog options;
		options.AddPage(&page);
		options.AddPage(&page2);
		options.DoModal();

		return 0;
	}

	LRESULT OnFind(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		if(m_FindDialog == NULL)
		{
			m_FindDialog = new CFindDlg;
			hFindWnd = m_FindDialog->Create(m_hWnd);
		}
		
		m_FindDialog->ShowWindow(SW_SHOW);

		return 0;
	}

	LRESULT OnReplace(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		if(m_ReplaceDialog == NULL)
		{
			m_ReplaceDialog = new CReplaceDlg;
			hReplWnd = m_ReplaceDialog->Create(m_hWnd);
		}

		m_ReplaceDialog->ShowWindow(SW_SHOW);

		return 0;
	}

	void UpdateStatusBar()
	{
		CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor());
		
		if(pChild)
		{
			m_StatusBar.SetPaneText(ID_MOD_PANE, pChild->GetModified() ? _T("Modified") : _T(""));
			pChild->SetPosStatus(m_StatusBar);
		}
		/* This never gets called, the child is always valid when MDIDestroy happens.
		else
		{
			m_StatusBar.SetPaneText(ID_MOD_PANE, _T(""));
			m_StatusBar.SetPaneText(ID_POS_PANE, _T(""));
			SetStatusText(NULL);	
		}
		*/
	}

	////////////////////////////////////////////////////////////////
	// IMainFrame Implementation

public:
	
	virtual CWindow* GetWindow()
	{
		return static_cast<CWindow*>(this);
	}
	
	virtual void AddMRUEntry(LPCTSTR lpszFile)
	{
		m_RecentFiles.AddEntry(lpszFile);
		m_RecentFiles.UpdateMenu();
	}

	virtual void SetActiveScheme(HWND notifier, LPVOID pScheme)
	{
		if(notifier == MDIGetActive())
		{
			m_Switcher.SetActiveScheme(static_cast<CScheme*>(pScheme));
		}
	}

	virtual BOOL TrackPopupMenu(HMENU hMenu, UINT uFlags, int x, int y, LPTPMPARAMS lpParams = NULL)
	{
		return m_CmdBar.TrackPopupMenu(hMenu, uFlags, x, y, lpParams);
	}

	virtual void SetStatusText(LPCTSTR text)
	{
		if(text)
		{
			m_StatusBar.SetPaneText(ID_DEFAULT_PANE, text, (m_bIsXPOrLater ? SBT_NOBORDERS : 0));
			m_bShowingDefaultStatus = false;
		}
		else
			if(!m_bShowingDefaultStatus)
			{
				m_StatusBar.SetPaneText(ID_DEFAULT_PANE, _T("Ready"), (m_bIsXPOrLater ? SBT_NOBORDERS : 0));
				m_bShowingDefaultStatus = true;
			}
	}

protected:

	void AddNewMenu(CSMenuHandle& menu)
	{
		CSMenuHandle file = menu.GetSubMenu(0);
		::ModifyMenu(file.GetHandle(), 0, MF_BYPOSITION | MF_POPUP, (UINT)(HMENU)m_NewMenu, _T("&New"));
	}

	void AddMRUMenu(CSMenuHandle& menu)
	{
		CSMenuHandle file(menu.GetSubMenu(0));
		::InsertMenu(file.GetHandle(), ID_APP_EXIT, MF_BYCOMMAND | MF_POPUP, (UINT)(HMENU)m_RecentFiles, _T("&Recent Files"));
		::InsertMenu(file.GetHandle(), ID_APP_EXIT, MF_BYCOMMAND | MF_SEPARATOR, 0, NULL);
	}

	void AddLanguageMenu(CSMenuHandle& menu)
	{
		CSMenuHandle view(menu.GetSubMenu(2));
		::ModifyMenu(view.GetHandle(), ID_VIEW_CHANGESCHEME, MF_BYCOMMAND | MF_POPUP, (UINT)(HMENU)m_Switcher, _T("&Change Scheme"));
	}

	void MoveMRU(CSMenuHandle& r, CSMenuHandle& a)
	{
		CSMenuHandle file( r.GetSubMenu(0) );
		int state;
		for(int i = 0; i < file.GetCount(); i++)
		{
			state = ::GetMenuState(file, i, MF_BYPOSITION);
			if(state & MF_POPUP)
			{
				MENUITEMINFO mii;
				memset(&mii, 0, sizeof(mii));
				mii.cbSize = sizeof(MENUITEMINFO);
				mii.fMask = MIIM_SUBMENU;
				file.GetItemInfo(i, &mii);
				
				if(mii.hSubMenu == (HMENU)m_RecentFiles)
				{
					// Recent Files Item found...
					if(::RemoveMenu(file, i, MF_BYPOSITION))
					{
						// We should be able to remove the separator for the MRU too...
						::RemoveMenu(file, i, MF_BYPOSITION);
						AddMRUMenu(a);
						break;
					}
				}
			}
		}
	}

	void MoveNewMenu(CSMenuHandle& remove, CSMenuHandle& add)
	{
		CSMenuHandle file( remove.GetSubMenu(0) );
		::ModifyMenu(file, 0, MF_BYPOSITION | MF_STRING, ID_FILE_NEW, _T("n"));
		AddNewMenu(add);
	}

	void MoveLanguage(CSMenuHandle& remove, CSMenuHandle& add)
	{
		CSMenuHandle view = remove.GetSubMenu(2);
		if((HMENU)remove != m_hMenu)
		{
			int state;
			for(int i = view.GetCount() - 1; i >= 0; i--)
			{
				state = ::GetMenuState(view, i, MF_BYPOSITION);
				if(state & MF_POPUP)
				{
					MENUITEMINFO mii;
					memset(&mii, 0, sizeof(MENUITEMINFO));
					mii.cbSize = sizeof(MENUITEMINFO);
					mii.fMask = MIIM_SUBMENU;
					view.GetItemInfo(i, &mii);

					if(mii.hSubMenu == (HMENU)m_Switcher)
					{
						::ModifyMenu(view, i, MF_BYPOSITION | MF_STRING, ID_VIEW_CHANGESCHEME, _T("s"));
					}
				}
			}
		}
		
		if((HMENU)add != m_hMenu)
			AddLanguageMenu(add);
	}

protected:
	CFindDlg*				m_FindDialog;
	CReplaceDlg*			m_ReplaceDialog;
	CScintilla				m_Dummy;			///< Scintilla often doesn't like unloading and reloading.

	CSPopupMenu				m_NewMenu;
	CMRUMenu				m_RecentFiles;
	CSchemeSwitcher			m_Switcher;

	CMultiPaneStatusBarCtrl	m_StatusBar;
	CPNTabbedMDICommandBarCtrl<CMainFrame> m_CmdBar;

	HWND					hFindWnd;
	HWND					hReplWnd;

	bool					m_bShowingDefaultStatus;
	bool					m_bIsXPOrLater;

	void CloseAndFreeDlg(CDialogImplBase* pD)
	{
		if(pD)
		{
			if(::IsWindow(pD->m_hWnd))
				if(pD->IsWindowVisible())
					pD->PostMessage(WM_CLOSE);
			delete pD;
		}
	}
};

BOOL CALLBACK CloseChildEnumProc(HWND hWnd, LPARAM lParam)
{
	if(GetWindow(hWnd, GW_OWNER))
		return TRUE;

	//if(GetParent(hWnd) == pMF->m_hWndMDIClient) <-- Alternative check...
	CChildFrame* pChild = CChildFrame::FromHandle(hWnd);
	if(pChild != NULL)
	{
		SCloseStruct* s = reinterpret_cast<SCloseStruct*>(lParam);
		CMainFrame *pMF = static_cast<CMainFrame*>(s->pMainFrm);

		if(!pMF->OnEditorClosing(pChild))
			s->bCanClose = false;
	}
	
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(MAINFRM_H__INCLUDED)
