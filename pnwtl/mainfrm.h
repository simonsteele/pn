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
#endif

class CToolCommandString : public CustomFormatStringBuilder<CToolCommandString>
{
	public:
		void OnFormatChar(TCHAR thechar)
		{
			switch(thechar)
			{
				case _T('f'):
					m_string += pChild->GetFileName(FN_FILE);
					break;

				case _T('d'):
					m_string += pChild->GetFileName(FN_PATH);
					break;

				case _T('n'):
					m_string += pChild->GetFileName(FN_FILEPART);
					break;

				case _T('l'):
					_itoa(pChild->GetPosition(EP_LINE), itosbuf, 10);
					m_string += itosbuf;
					break;

				case _T('c'):
					_itoa(pChild->GetPosition(EP_COL), itosbuf, 10);
					m_string += itosbuf;
					break;
			}		
		}

	protected:
		TCHAR itosbuf[100];
		CChildFrame* pChild;
};

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

	CMainFrame();

	~CMainFrame();

	virtual BOOL PreTranslateMessage(MSG* pMsg);

	virtual BOOL OnIdle();

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
		COMMAND_ID_HANDLER(ID_HELP_WEB_PN, OnWebPNHome)
		COMMAND_ID_HANDLER(ID_HELP_WEB_SF, OnWebSFPage)
		COMMAND_ID_HANDLER(ID_HELP_WEB_SB, OnWebSFBug)
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

	CChildFrame* NewEditor();

	void OnSchemeNew(LPVOID data);
	void OnMDISetMenu(HMENU hOld, HMENU hNew);
	bool OnEditorClosing(CChildFrame* pChild);

	LRESULT OnActivate(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled);
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);

	LRESULT OnDropFiles(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnChildNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled);
	LRESULT OnInitMenuPopup(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/);

	LRESULT OnFileExit(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnFileNew(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileOpen(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnMRUSelected(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	// View
	LRESULT OnViewToolBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnViewEditBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnViewStatusBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	// MDI Window Arrangement
	LRESULT OnWindowCascade(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWindowTile(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWindowArrangeIcons(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnAppAbout(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFind(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnReplace(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnOptions(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnWebPNHome(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWebSFPage(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWebSFBug(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	void PNOpenFile(LPCTSTR pathname, LPCTSTR filename, CScheme* pScheme = NULL);
	void PNOpenFile(LPCTSTR pathname, CScheme* pScheme);
	void PNOpenFile(LPCTSTR pathname);

	void UpdateStatusBar();

	static BOOL CALLBACK CloseChildEnumProc(HWND hWnd, LPARAM lParam);

	////////////////////////////////////////////////////////////////
	// IMainFrame Implementation

public:
	virtual CWindow* GetWindow();
	virtual void AddMRUEntry(LPCTSTR lpszFile);
	virtual void SetActiveScheme(HWND notifier, LPVOID pScheme);
	virtual BOOL TrackPopupMenu(HMENU hMenu, UINT uFlags, int x, int y, LPTPMPARAMS lpParams = NULL);
	virtual void SetStatusText(LPCTSTR text);

protected:
	void AddNewMenu(CSMenuHandle& menu);
	void AddMRUMenu(CSMenuHandle& menu);
	void AddLanguageMenu(CSMenuHandle& menu);
	void MoveMRU(CSMenuHandle& r, CSMenuHandle& a);
	void MoveNewMenu(CSMenuHandle& remove, CSMenuHandle& add);
	void MoveLanguage(CSMenuHandle& remove, CSMenuHandle& add);

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

	/* Can't free dialogs via the base class or destructors don't get
	called. Use a template function to free any dialog class */
	template <class T>
	void CloseAndFreeDlg(T* pD)
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

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(MAINFRM_H__INCLUDED)
