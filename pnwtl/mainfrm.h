/**
 * @file mainfrm.h
 * @brief Main Window for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#if !defined(MAINFRM_H__INCLUDED)
#define MAINFRM_H__INCLUDED

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "files.h"

/**
 * @class CPNMDIClient
 * @brief Add extra MDI plumbing to the TabbedMDIClient Framework.
 *
 * In order to maintain other lists of windows such as one in the
 * windows list control, what better way than to use the framework
 * provided in tabbed MDI code.
 */
class CPNMDIClient : public CTabbedMDIClient< CDotNetTabCtrl >
{
	typedef CTabbedMDIClient<CDotNetTabCtrl> baseClass;

public:
	BEGIN_MSG_MAP(CPNMDIClient)
		MESSAGE_HANDLER(UWM_MDICHILDACTIVATIONCHANGE, OnChildActivationChange)
		MESSAGE_HANDLER(UWM_MDICHILDTABTEXTCHANGE, OnChildTabTextChange)
		MESSAGE_HANDLER(WM_MDIDESTROY, OnMDIDestroy)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	LRESULT OnMDIDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
	{
		bHandled = FALSE;

		return 0;
	}

	LRESULT OnChildActivationChange(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
	{
		SendMessage(GetParent(), PN_NOTIFY, 0, SCN_UPDATEUI);

		bHandled = FALSE;
		
		return 0;
	}

	LRESULT OnChildTabTextChange(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
	{
		SendMessage(GetParent(), PN_NOTIFY, 0, SCN_UPDATEUI);

		bHandled = FALSE;

		return 0;
	}

};

BOOL CALLBACK CloseChildEnumProc(HWND hWnd, LPARAM lParam);

/**
 * @class CMainFrame
 * @brief PN (WTL Edition) Main MDI Frame
 */
class CMainFrame : public CTabbedMDIFrameWindowImpl<CMainFrame, CPNMDIClient>, public CUpdateUI<CMainFrame>,
		public CMessageFilter, public CIdleHandler, public CSMenuEventHandler
{
public:
	DECLARE_FRAME_WND_CLASS(NULL, IDR_MAINFRAME)

	typedef CTabbedMDIFrameWindowImpl<CMainFrame, CPNMDIClient> myClass;

	CMainFrame()
	{
		m_FindDialog = NULL;
		m_ReplaceDialog = NULL;
		hFindWnd = NULL;
		hReplWnd = NULL;
	}

	~CMainFrame()
	{
		if(m_FindDialog)
		{
			///@todo record search box choices...
			if(::IsWindow(m_FindDialog->m_hWnd))
				if(m_FindDialog->IsWindowVisible())
					m_FindDialog->PostMessage(WM_CLOSE);
			delete m_FindDialog;
		}

		if(m_ReplaceDialog)
		{
			if(::IsWindow(m_ReplaceDialog->m_hWnd))
				if(m_ReplaceDialog->IsWindowVisible())
					m_ReplaceDialog->PostMessage(WM_CLOSE);
			delete m_ReplaceDialog;
		}
	}

    CTabbedMDICommandBarCtrl m_CmdBar;

	virtual BOOL PreTranslateMessage(MSG* pMsg)
	{
		if(myClass::PreTranslateMessage(pMsg))
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
		COMMAND_ID_HANDLER(ID_APP_EXIT, OnFileExit)
		COMMAND_ID_HANDLER(ID_FILE_NEW, OnFileNew)
		COMMAND_ID_HANDLER(ID_FILE_OPEN, OnFileOpen)
		COMMAND_ID_HANDLER(ID_VIEW_TOOLBAR, OnViewToolBar)
		COMMAND_ID_HANDLER(ID_VIEW_STATUS_BAR, OnViewStatusBar)
		COMMAND_ID_HANDLER(ID_APP_ABOUT, OnAppAbout)
		COMMAND_ID_HANDLER(ID_WINDOW_CASCADE, OnWindowCascade)
		COMMAND_ID_HANDLER(ID_WINDOW_TILE_HORZ, OnWindowTile)
		COMMAND_ID_HANDLER(ID_WINDOW_ARRANGE, OnWindowArrangeIcons)
		COMMAND_ID_HANDLER(ID_EDIT_FIND, OnFind)
		COMMAND_ID_HANDLER(ID_EDIT_REPLACE, OnReplace)
		CALLBACK_COMMAND_HANDLER()
		CHAIN_MDI_CHILD_COMMANDS()
		CHAIN_MSG_MAP(CUpdateUI<CMainFrame>)
		CHAIN_MSG_MAP(myClass)
	END_MSG_MAP()

	BEGIN_UPDATE_UI_MAP(CMainFrame)
		UPDATE_ELEMENT(ID_VIEW_TOOLBAR, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_STATUS_BAR, UPDUI_MENUPOPUP)
	END_UPDATE_UI_MAP()

	BEGIN_MENU_HANDLER_MAP()
		HANDLE_MENU_COMMAND(SCHEMEMANAGER_SELECTSCHEME, OnSchemeNew)
	END_MENU_HANDLER_MAP()

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
		m_CmdBar.LoadImages(IDR_IMAGES);
		// remove old menu
		SetMenu(NULL);

		HWND hWndToolBar = CreateSimpleToolBarCtrl(m_hWnd, IDR_MAINFRAME, FALSE, ATL_SIMPLE_TOOLBAR_PANE_STYLE);

		CreateSimpleReBar(ATL_SIMPLE_REBAR_NOBORDER_STYLE);
		AddSimpleReBarBand(hWndCmdBar);
		AddSimpleReBarBand(hWndToolBar, NULL, TRUE);
		
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
		m_StatusBar.SetPaneText(ID_DEFAULT_PANE, _T("Ready"), SBT_NOBORDERS);

		DragAcceptFiles(TRUE);

		CreateMDIClient();
		m_CmdBar.SetMDIClient(m_hWndMDIClient);

		UIAddToolBar(hWndToolBar);
		UISetCheck(ID_VIEW_TOOLBAR, 1);
		UISetCheck(ID_VIEW_STATUS_BAR, 1);

		// register object for message filtering and idle updates
		CMessageLoop* pLoop = _Module.GetMessageLoop();
		ATLASSERT(pLoop != NULL);
		pLoop->AddMessageFilter(this);
		pLoop->AddIdleHandler(this);

		ConfigureNewMenu();

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

		pChild->CreateEx(/*m_hWndClient*/m_hWndMDIClient);

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
		CSSFileDialog dlgOpen(TRUE, NULL, NULL, OFN_HIDEREADONLY, "All Files (*.*)|*.*", m_hWndClient);
		if (dlgOpen.DoModal() == IDOK)
		{
			PNOpenFile(dlgOpen.m_ofn.lpstrFile, dlgOpen.m_ofn.lpstrFileTitle);
		}

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

	LRESULT OnFind(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		if(m_FindDialog == NULL)
		{
			m_FindDialog = new CFindDlg(this);
			hFindWnd = m_FindDialog->Create(m_hWnd);
		}
		
		m_FindDialog->ShowWindow(SW_SHOW);

		return 0;
	}

	LRESULT OnReplace(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		if(m_ReplaceDialog == NULL)
		{
			m_ReplaceDialog = new CReplaceDlg(this);
			hReplWnd = m_ReplaceDialog->Create(m_hWnd);
		}

		m_ReplaceDialog->ShowWindow(SW_SHOW);

		return 0;
	}

	void UpdateStatusBar()
	{
		CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor(this));
		
		if(pChild)
		{
			m_StatusBar.SetPaneText(ID_MOD_PANE, pChild->GetModified() ? _T("Modified") : _T(""));
			pChild->SetPosStatus(m_StatusBar);
		}
	}

	protected:

		void ConfigureNewMenu()
		{
			CSMenuHandle cs(m_hMenu);

			CSMenuHandle file = cs.GetSubMenu(0);
			
			CSPopupMenu sm;

			theApp.GetSchemes().BuildMenu(sm.GetHandle(), this);
					
			POINT pt;
			pt.x = 100;
			pt.y = 100;
			
			::ModifyMenu(file.GetHandle(), 0, MF_BYPOSITION | MF_POPUP, (UINT)sm.GetHandle(), _T("&New"));

			cs.Detach();
			sm.Detach();
		}

	protected:
		CFindDlg*				m_FindDialog;
		CReplaceDlg*			m_ReplaceDialog;
		CScintilla				m_Dummy;			///< Scintilla often doesn't like unloading and reloading.

		CMultiPaneStatusBarCtrl	m_StatusBar;

		HWND					hFindWnd;
		HWND					hReplWnd;
};

BOOL CALLBACK CloseChildEnumProc(HWND hWnd, LPARAM lParam)
{
	if(GetWindow(hWnd, GW_OWNER))
		return TRUE;

	//if(GetParent(hWnd) == pMF->m_hWndMDIClient) <-- Alternative check...
	CChildFrame* pChild = CChildFrame::FromHandle(hWnd);
	if(pChild != NULL)
	{
		SCloseStruct* s = (SCloseStruct*)lParam;
		CMainFrame *pMF = (CMainFrame*)s->pMainFrm;

		if(!pMF->OnEditorClosing(pChild))
			s->bCanClose = false;

		//SendMessage(GetParent(hWnd), WM_MDIDESTROY, (WPARAM)hWnd, 0);
	}
	
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(MAINFRM_H__INCLUDED)
