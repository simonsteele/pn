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

#pragma once

// Pre-declarations...
class CMainFrame;
class CChildFrame;
class CFindDlg;
class CReplaceDlg;
class CDockingOutputWindow;
class CClipsDocker;
class CProjectDocker;

namespace Projects
{
	class Workspace;
}

struct tagEnumChildrenStruct;

// Auto-complete for the find combo box.
#include "CustomAutoComplete.h"	// Autocompletion.
#include "include/accombo.h"	// Autocompleting combo box.

typedef void(__stdcall CMainFrame::*lpChildEnumFn)(CChildFrame* pFrame, tagEnumChildrenStruct* pStruct);

typedef struct tagEnumChildrenStruct
{
	CMainFrame* pMainFrame;
	lpChildEnumFn pFunction;
} SChildEnumStruct;

typedef struct tagCloseStruct : public tagEnumChildrenStruct
{
	bool	bCanClose;
} SCloseStruct;

typedef struct tagWorkspaceCloseStruct : public tagCloseStruct
{
	Projects::Workspace* pWorkspace;
	std::list<CChildFrame*> FoundWindows;
} SWorkspaceCloseStruct;

typedef struct tagIsOpenStruct : public tagEnumChildrenStruct
{
	bool bFound;
	LPCTSTR pszFilename;
	CChildFrame* pMatch;
} SIsOpen;

/**
 * @class CMainFrame
 * @brief PN (WTL Edition) Main MDI Frame
 */
class CMainFrame : public CPNDockingTabbedMDIFrameWindow<CMainFrame>, public IMainFrame, public CUpdateUI<CMainFrame>,
		public CMessageFilter, public CIdleHandler, public CSMenuEventHandler
{
public:
	DECLARE_FRAME_WND_CLASS(NULL, IDR_MAINFRAME)

	typedef CPNDockingTabbedMDIFrameWindow<CMainFrame>	baseClass;
	typedef CPNTabbedMDICommandBarCtrl<CMainFrame>		commandBarClass;

	////////////////////////////////////////////////////////////////
	// CMainFrame Implementation

	CMainFrame();

	~CMainFrame();

	enum {
		TBR_SCHEME = 100,
		TBR_FIND = 101,
		SCHEME_COMBO_SIZE = 24, /* characters */
		FIND_COMBO_SIZE = 30,
		TOOLBAR_COMBO_DROPLINES = 16,
		REBAR_SAVESTATE_VERSION = 0
	};

	virtual BOOL PreTranslateMessage(MSG* pMsg);

	virtual BOOL OnIdle();

	BEGIN_MSG_MAP(CMainFrame)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_INITMENUPOPUP, OnInitMenuPopup)
		MESSAGE_HANDLER(WM_DROPFILES, OnDropFiles)
		MESSAGE_HANDLER(PN_NOTIFY, OnChildNotify)
		MESSAGE_HANDLER(WM_CLOSE, OnClose)
		MESSAGE_HANDLER(WM_ACTIVATE, OnActivate)
		MESSAGE_HANDLER(WM_LBUTTONDBLCLK, OnDblClick)
		MESSAGE_HANDLER(PN_ESCAPEPRESSED, OnEscapePressed)
		MESSAGE_HANDLER(PN_INITIALISEFRAME, OnInitialiseFrame)
		
		COMMAND_ID_HANDLER(ID_APP_EXIT, OnFileExit)
		COMMAND_ID_HANDLER(ID_FILE_NEW, OnFileNew)
		COMMAND_ID_HANDLER(ID_FILE_NEW_PROJECT, OnFileNewProject)
		COMMAND_ID_HANDLER(ID_FILE_NEW_WORKSPACE, OnFileNewWorkspace)
		COMMAND_ID_HANDLER(ID_FILE_OPEN, OnFileOpen)
		COMMAND_ID_HANDLER(ID_FILE_SAVEALL, OnFileSaveAll)
		COMMAND_ID_HANDLER(ID_FILE_OPENPROJECT, OnFileOpenProject)
		COMMAND_ID_HANDLER(ID_FILE_CLOSEWORKSPACE, OnFileCloseWorkspace)
		
		// Global edit action handlers - simply to map accelerators to action...
		COMMAND_ID_HANDLER(ID_EDIT_CUT, OnCut)
		COMMAND_ID_HANDLER(ID_EDIT_COPY, OnCopy)
		COMMAND_ID_HANDLER(ID_EDIT_PASTE, OnPaste)
		COMMAND_ID_HANDLER(ID_EDIT_UNDO, OnUndo)
		//COMMAND_ID_HANDLER(ID_EDIT_REDO, OnRedo)

		COMMAND_ID_HANDLER(ID_VIEW_TOOLBAR, OnViewToolBar)
		COMMAND_ID_HANDLER(ID_VIEW_TOOLBAR_EDIT, OnViewEditBar)
		COMMAND_ID_HANDLER(ID_VIEW_TOOLBARS_SCHEMES, OnViewSchemesBar)
		COMMAND_ID_HANDLER(ID_VIEW_TOOLBARS_FIND, OnViewFindBar)
		COMMAND_ID_HANDLER(ID_VIEW_STATUS_BAR, OnViewStatusBar)
		COMMAND_ID_HANDLER(ID_EDITOR_OUTPUTWND, OnOutputWindowToggle)
		COMMAND_ID_HANDLER(ID_VIEW_WINDOWS_PROJECT, OnViewProjectWindow)
		COMMAND_ID_HANDLER(ID_VIEW_WINDOWS_TEXTCLIPS, OnViewTextClipsWindow)
		COMMAND_ID_HANDLER(ID_APP_ABOUT, OnAppAbout)
		COMMAND_ID_HANDLER(ID_WINDOW_CASCADE, OnWindowCascade)
		COMMAND_ID_HANDLER(ID_WINDOW_TILE_HORZ, OnWindowTile)
		COMMAND_ID_HANDLER(ID_WINDOW_TILE_VERT, OnWindowTileVert)
		COMMAND_ID_HANDLER(ID_WINDOW_ARRANGE, OnWindowArrangeIcons)
		COMMAND_ID_HANDLER(ID_EDIT_FIND, OnFind)
		COMMAND_ID_HANDLER(ID_EDIT_REPLACE, OnReplace)
		COMMAND_ID_HANDLER(ID_TOOLS_OPTIONS, OnOptions)
		COMMAND_ID_HANDLER(ID_TOOLS_DUMMY, OnOptions)
		COMMAND_ID_HANDLER(ID_HELP_WEB_PN, OnWebPNHome)
		COMMAND_ID_HANDLER(ID_HELP_WEB_SF, OnWebSFPage)
		COMMAND_ID_HANDLER(ID_HELP_WEB_SB, OnWebSFBug)
		COMMAND_ID_HANDLER(ID_HELP_WEB_SR, OnWebSFRFE)
		COMMAND_ID_HANDLER(ID_FINDBAR_SEARCHGOOGLE, OnSearchGoogle)
		COMMAND_ID_HANDLER(ID_FINDBAR_SEARCHGOOGLEGROUPS, OnSearchGoogleGroups)

		COMMAND_HANDLER(IDC_SCHEMECOMBO, CBN_SELCHANGE, OnSchemeComboChange)
		COMMAND_HANDLER(IDC_FINDCOMBO, BXTN_ENTER, OnFindComboEnter)

		NOTIFY_CODE_HANDLER(TBN_DROPDOWN, OnToolbarDropDown)

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
		UPDATE_ELEMENT(ID_VIEW_TOOLBARS_SCHEMES, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_TOOLBARS_FIND, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_EDITOR_OUTPUTWND, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_WINDOWS_TEXTCLIPS, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_WINDOWS_PROJECT, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_FILE_SAVE, UPDUI_MENUPOPUP | UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_FILE_CLOSE, UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_FILE_CLOSEWORKSPACE, UPDUI_MENUPOPUP)
	END_UPDATE_UI_MAP()

	BEGIN_MENU_HANDLER_MAP()
		HANDLE_MENU_COMMAND(SCHEMEMANAGER_SELECTSCHEME, OnSchemeNew)
	END_MENU_HANDLER_MAP()

	CChildFrame* NewEditor();

	void OnSchemeNew(LPVOID data);
	void OnMDISetMenu(HMENU hOld, HMENU hNew);

	LRESULT OnActivate(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled);
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);

	LRESULT OnDropFiles(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnChildNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled);
	LRESULT OnInitMenuPopup(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/);
	LRESULT OnDblClick(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnEscapePressed(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnInitialiseFrame(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	LRESULT OnFileExit(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnFileNew(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileNewProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileNewWorkspace(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileOpen(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileSaveAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnMRUSelected(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileOpenProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileCloseWorkspace(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	// Edit
	LRESULT OnCut(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCopy(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnPaste(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnUndo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	// View
	LRESULT OnViewToolBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnViewEditBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnViewSchemesBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnViewFindBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnViewStatusBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnOutputWindowToggle(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnViewProjectWindow(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnViewTextClipsWindow(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	// MDI Window Arrangement
	LRESULT OnWindowCascade(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWindowTile(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWindowTileVert(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWindowArrangeIcons(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnAppAbout(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFind(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnReplace(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnOptions(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnWebPNHome(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWebSFPage(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWebSFBug(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWebSFRFE(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnSearchGoogle(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnSearchGoogleGroups(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnSchemeComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFindComboEnter(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnToolbarDropDown(WPARAM /*wParam*/, LPNMHDR /*lParam*/, BOOL& /*bHandled*/);

	void OpenFile(LPCTSTR pathname, LPCTSTR filename, CScheme* pScheme = NULL);
	void OpenFile(LPCTSTR pathname, CScheme* pScheme);
	
	void UpdateStatusBar();

	static BOOL CALLBACK ChildEnumProc(HWND hWnd, LPARAM lParam);

	void __stdcall ChildCloseNotify(CChildFrame* pChild, SChildEnumStruct* pES);
	void __stdcall WorkspaceChildCloseNotify(CChildFrame* pChild, SChildEnumStruct* pES);
	void __stdcall ChildOptionsUpdateNotify(CChildFrame* pChild, SChildEnumStruct* pES);
	void __stdcall ChildSaveNotify(CChildFrame* pChild, SChildEnumStruct* pES);
	void __stdcall FileOpenNotify(CChildFrame* pChild, SChildEnumStruct* pES);

	////////////////////////////////////////////////////////////////
	// IMainFrame Implementation

public:
	virtual CWindow* GetWindow();
	virtual ToolWrapper* MakeGlobalOutputWrapper(ToolDefinition* pDefinition);
	virtual void AddMRUEntry(LPCTSTR lpszFile);
	virtual void SetActiveScheme(HWND notifier, LPVOID pScheme);
	virtual BOOL TrackPopupMenu(HMENU hMenu, UINT uFlags, int x, int y, LPTPMPARAMS lpParams = NULL, HWND hWndCaller = NULL);
	virtual void SetStatusText(LPCTSTR text, bool bLongLife = true);
	virtual void SaveAll();
	virtual void OpenFile(LPCTSTR pathname, bool bAddMRU = false);
	virtual bool CheckAlreadyOpen(LPCTSTR filename, EAlreadyOpenAction = COptionsManager::GetInstance()->AlreadyOpenAction);

	////////////////////////////////////////////////////////////////
	// IToolOutputSink Implementation

public:
	void ToggleOutputWindow(bool bSetValue = false, bool bShowing = true);

protected:
	void AddNewMenu(CSMenuHandle& menu);
	void AddMRUMenu(CSMenuHandle& menu);
	void AddLanguageMenu(CSMenuHandle& menu);
	void MoveMRU(CSMenuHandle& r, CSMenuHandle& a);
	void MoveNewMenu(CSMenuHandle& remove, CSMenuHandle& add);
	void MoveLanguage(CSMenuHandle& remove, CSMenuHandle& add);

	CSize GetGUIFontSize();
	HWND CreateFindToolbar();
	HWND CreateSchemeToolbar();

	void InitGUIState();
	void LoadGUIState(LPCTSTR stateName = NULL);
	void SaveGUIState(LPCTSTR stateName = NULL);
	void SetDefaultGUIState();

	void PerformChildEnum(SChildEnumStruct* s);
	void PerformChildEnum(lpChildEnumFn pFunction);

	void _setWindowText(LPCTSTR newText);

	void OpenProject(LPCTSTR project);
	void OpenWorkspace(LPCTSTR workspace);
	bool SaveProjects(Projects::Workspace* pWorkspace);
	DWORD SaveWorkspace(Projects::Workspace* pWorkspace, bool bAsk);
	bool SaveWorkspaceAs(Projects::Workspace* pWorkspace);
	bool CloseWorkspace(bool bAllowCloseFiles = false);
	bool CloseWorkspaceFiles(Projects::Workspace* pWorkspace);

protected:
	CFindDlg*				m_FindDialog;
	CReplaceDlg*			m_ReplaceDialog;
	CDockingOutputWindow*	m_pOutputWnd;
	CClipsDocker*			m_pClipsWnd;
	CProjectDocker*			m_pProjectsWnd;
	
	CScintilla				m_Dummy;			///< Scintilla often doesn't like unloading and reloading.

	CSPopupMenu				m_NewMenu;
	CMRUMenu				m_RecentFiles;
	CSchemeSwitcher			m_Switcher;

	// GUI Stuff:
	CMultiPaneStatusBarCtrl	m_StatusBar;
	commandBarClass			m_CmdBar;
	CPNStateManager			m_GUIState;
	CComboBox				m_SchemeCombo;
	BXT::CComboBoxAC		m_FindCombo;
	CImageList				m_FindImages;

	HWND					hFindWnd;
	HWND					hReplWnd;

	bool					m_bShowingDefaultStatus;
	bool					m_bIsXPOrLater;
	short					m_statusResetCounter;

	/* Can't free dialogs via the base class or destructors don't get
	called. Use a template function to free any dialog class */
	template <class T>
	void CloseAndFreeDlg(T* pD)
	{
		if(pD)
		{
			if(::IsWindow(pD->m_hWnd))
				pD->DestroyWindow();
			delete pD;
		}
	}

};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(MAINFRM_H__INCLUDED)
