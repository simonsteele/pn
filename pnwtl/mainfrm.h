/**
 * @file mainfrm.h
 * @brief Main Window for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-2012 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#if !defined(MAINFRM_H__INCLUDED)
#define MAINFRM_H__INCLUDED

#pragma once

// Pre-declarations...
class CMainFrame;
class CChildFrame;
class CFindExDialog;
class COutputView;
class CClipsDocker;
class CJumpDocker;
class CProjectDocker;
class CFindInFilesSink;
class CMSTreeViewCtrl;
class CFindInFilesView;
class CScriptDocker;
class CBrowseDocker;
class COpenFilesDocker;
class EditorFactory;

namespace Projects
{
	class Workspace;
}

namespace TextClips
{
	class TextClipsManager;
}

namespace Updates
{
	class UpdateAvailableDetails;
}

struct tagEnumChildrenStruct;

// Auto-complete for the find combo box.
#include "CustomAutoComplete.h"	// Autocompletion.
#include "include/accombo.h"	// Autocompleting combo box.
#include "include/toolbar.h"
#include "editorFactory.h"

typedef void(__stdcall CMainFrame::*lpChildEnumFn)(CChildFrame* pFrame, tagEnumChildrenStruct* pStruct);

typedef struct tagEnumChildrenStruct
{
	lpChildEnumFn pFunction;
	CMainFrame* pMainFrame;
} SChildEnumStruct;

typedef struct tagCloseStruct : public tagEnumChildrenStruct
{
	bool	bCanClose;
} SCloseStruct;

typedef struct tagWorkspaceCloseStruct : public tagCloseStruct
{
	Projects::Workspace* pWorkspace;
	std::list<CChildFrame*> FoundWindows;
	bool bInProjectGroupOnly;
} SWorkspaceWindowsStruct;

typedef struct tagIsOpenStruct : public tagEnumChildrenStruct
{
	LPCTSTR pszFilename;
	CChildFrame* pMatch;
	bool bFound;
} SIsOpen;

/**
 * @class CMainFrame
 * @brief PN (WTL Edition) Main MDI Frame
 */
class CMainFrame : public CPNDockingTabbedMDIFrameWindow<CMainFrame>, public IMainFrame, public CUpdateUI<CMainFrame>,
		public CMessageFilter, public CIdleHandler, public CommandEventHandler
{
public:
	DECLARE_FRAME_WND_CLASS(NULL, IDR_MAINFRAME)

	typedef CPNDockingTabbedMDIFrameWindow<CMainFrame>	baseClass;
	typedef CPNTabbedMDICommandBarCtrl<CMainFrame>		commandBarClass;

	////////////////////////////////////////////////////////////////
	// CMainFrame Implementation

	CMainFrame(CommandDispatch* commands, std::list<tstring>* cmdLineArgs);

	~CMainFrame();

	HWND CreateEx(HWND hWndParent = NULL, ATL::_U_RECT rect = NULL, DWORD dwStyle = 0, DWORD dwExStyle = 0, LPVOID lpCreateParam = NULL);

	virtual BOOL PreTranslateMessage(MSG* pMsg);

	virtual BOOL OnIdle();

	BEGIN_MSG_MAP(CMainFrame)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_INITMENUPOPUP, OnInitMenuPopup)
		MESSAGE_HANDLER(WM_DROPFILES, OnDropFiles)
		MESSAGE_HANDLER(PN_NOTIFY, OnChildNotify)
		MESSAGE_HANDLER(PN_PROJECTNOTIFY, OnProjectNotify)
		MESSAGE_HANDLER(WM_CLOSE, OnClose)
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy)
		MESSAGE_HANDLER(WM_ACTIVATE, OnActivate)
		MESSAGE_HANDLER(WM_LBUTTONDBLCLK, OnDblClick)
		MESSAGE_HANDLER(PN_ESCAPEPRESSED, OnEscapePressed)
		MESSAGE_HANDLER(PN_INITIALISEFRAME, OnInitialiseFrame)
		MESSAGE_HANDLER(WM_MENUSELECT, OnMenuSelect)
		MESSAGE_HANDLER(m_uiMIMessageID, OnMultiInstanceMsg)
		MESSAGE_HANDLER(UWM_MDICHILDSAVEMODIFIED, OnSaveModifiedItem)
		MESSAGE_HANDLER(PN_UPDATEFINDTEXT, OnUpdateFindText)
		MESSAGE_HANDLER(PN_MDISETMENU, OnMDISetMenu)
		MESSAGE_HANDLER(PN_UPDATECHILDUI, OnUpdateChildUIState)
		MESSAGE_HANDLER(PN_CLOSEALLOTHER, OnCloseAllOther)
		MESSAGE_HANDLER(WM_QUERYENDSESSION, OnQueryEndSession)
		MESSAGE_HANDLER(WM_ENDSESSION, OnEndSession)
		
		COMMAND_ID_HANDLER(ID_APP_EXIT, OnFileExit)
		COMMAND_ID_HANDLER(ID_FILE_NEW, OnFileNew)
		COMMAND_ID_HANDLER(ID_FILE_NEW_PROJECT, OnFileNewProject)
		COMMAND_ID_HANDLER(ID_FILE_NEW_WORKSPACE, OnFileNewWorkspace)
		COMMAND_ID_HANDLER(ID_FILE_OPEN, OnFileOpen)
		COMMAND_ID_HANDLER(ID_FILE_SAVEALL, OnFileSaveAll)
		COMMAND_ID_HANDLER(ID_FILE_OPENPROJECT, OnFileOpenProject)
		COMMAND_ID_HANDLER(ID_FILE_CLOSEWORKSPACE, OnFileCloseWorkspace)
		COMMAND_ID_HANDLER(ID_FILE_CLOSEALL, OnFileCloseAll)
		COMMAND_ID_HANDLER(ID_FILE_OPENWORKSPACE, OnFileOpenWorkspaceState)
		COMMAND_ID_HANDLER(ID_FILE_SAVEWORKSPACEAS, OnFileSaveWorkspaceState)
		
		// Global edit action handlers - simply to map accelerators to action...
		COMMAND_ID_HANDLER(ID_EDIT_CUT, OnCut)
		COMMAND_ID_HANDLER(ID_EDIT_COPY, OnCopy)
		COMMAND_ID_HANDLER(ID_EDIT_PASTE, OnPaste)
		COMMAND_ID_HANDLER(ID_EDIT_UNDO, OnUndo)

		COMMAND_ID_HANDLER(ID_EDIT_QUICKFIND, OnQuickFind)

		COMMAND_ID_HANDLER(ID_VIEW_TOOLBAR, OnViewToolBar)
		COMMAND_ID_HANDLER(ID_VIEW_STATUS_BAR, OnViewStatusBar)
		COMMAND_ID_HANDLER(ID_OUTPUT_HIDE, OnHideOutput)
		COMMAND_ID_HANDLER(ID_APP_ABOUT, OnAppAbout)
		COMMAND_ID_HANDLER(ID_WINDOW_CASCADE, OnWindowCascade)
		COMMAND_ID_HANDLER(ID_WINDOW_TILE_HORZ, OnWindowTile)
		COMMAND_ID_HANDLER(ID_WINDOW_TILE_VERT, OnWindowTileVert)
		COMMAND_ID_HANDLER(ID_WINDOW_ARRANGE, OnWindowArrangeIcons)
		COMMAND_ID_HANDLER(ID_EDIT_FIND, OnFind)
		COMMAND_ID_HANDLER(ID_EDIT_REPLACE, OnReplace)
		COMMAND_ID_HANDLER(ID_EDIT_FINDINFILES, OnFindInFiles)
		COMMAND_ID_HANDLER(ID_TOOLS_OPTIONS, OnOptions)
		COMMAND_ID_HANDLER(ID_TOOLS_DUMMY, OnOptions)
		COMMAND_ID_HANDLER(ID_TOOLS_STOPTOOLS, OnStopTools)
		COMMAND_ID_HANDLER(ID_TOOLS_RECORDSCRIPT, OnRecordScript)
		COMMAND_ID_HANDLER(ID_TOOLS_STOPRECORDING, OnStopRecording)
		COMMAND_ID_HANDLER(ID_HELP_WEB_PN, OnWebPNHome)
		COMMAND_ID_HANDLER(ID_HELP_WEB_SF, OnWebSFPage)
		COMMAND_ID_HANDLER(ID_HELP_WEB_SB, OnWebSFBug)
		COMMAND_ID_HANDLER(ID_HELP_WEB_DOCS, OnWebPNDoc)
		COMMAND_ID_HANDLER(ID_HELP_CHECKFORUPDATES, OnUpdateCheck)
		COMMAND_ID_HANDLER(ID_FINDTYPE_BUTTON, OnFindBarFind)
		COMMAND_ID_HANDLER(ID_FINDBAR_SEARCHGOOGLE, OnSearchGoogle)
		COMMAND_ID_HANDLER(ID_FINDBAR_SEARCHGOOGLEGROUPS, OnSearchGoogleGroups)
		COMMAND_ID_HANDLER(ID_HELP_CONTENTS, OnHelpContents)

		COMMAND_HANDLER(IDC_SCHEMECOMBO, CBN_SELCHANGE, OnSchemeComboChange)
		COMMAND_HANDLER(IDC_FINDCOMBO, BXTN_ENTER, OnFindComboEnter)

		//NOTIFY_CODE_HANDLER(TBN_DROPDOWN, OnToolbarDropDown)
        NOTIFY_HANDLER(ATL_IDW_STATUS_BAR, NM_DBLCLK, OnStatusBarDblClick)

		COMMAND_RANGE_HANDLER(ID_VIEW_FIRSTDOCKER, ID_VIEW_LASTDOCKER, OnDockerToggle)
		COMMAND_RANGE_HANDLER(ID_MRUFILE_BASE, ID_MRUFILE_MAX, OnMRUSelected)
		COMMAND_RANGE_HANDLER(ID_MRUPROJECT_BASE, ID_MRUPROJECT_MAX, OnMRUProjectSelected)
		
		LOCAL_MENUCOMMAND(TOOLS_RUNTOOL)

		ROUTE_MENUCOMMANDS()

		CHAIN_MSG_MAP_ALT_MEMBER(m_ToolBar, 1)

		CHAIN_MDI_CHILD_COMMANDS()
		CHAIN_MSG_MAP(CUpdateUI<CMainFrame>)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	BEGIN_UPDATE_UI_MAP(CMainFrame)
		/* to UPDATE_UI_MAP of CMainFrame*/
		UPDATE_ELEMENT(ID_EDIT_CUT, UPDUI_MENUPOPUP | UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_EDIT_COPY, UPDUI_MENUPOPUP | UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_EDIT_PASTE, UPDUI_MENUPOPUP | UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_EDIT_UNDO, UPDUI_MENUPOPUP | UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_EDIT_REDO, UPDUI_MENUPOPUP | UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_VIEW_TOOLBAR, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_STATUS_BAR, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_OUTPUT, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_WINDOWS_TEXTCLIPS, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_WINDOWS_CTAGS, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_WINDOWS_PROJECT, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_WINDOWS_FINDRESULTS, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_WINDOWS_SCRIPTS, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_WINDOWS_BROWSER, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_WINDOWS_OPENFILES, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_VIEW_ZOOM_IN, UPDUI_MENUPOPUP | UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_VIEW_ZOOM_OUT, UPDUI_MENUPOPUP | UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_FILE_SAVE, UPDUI_MENUPOPUP | UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_FILE_SAVEALL, UPDUI_MENUPOPUP | UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_FINDTYPE_BUTTON, UPDUI_MENUPOPUP | UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_FILE_CLOSE, UPDUI_TOOLBAR | UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_FILE_CLOSEWORKSPACE, UPDUI_MENUPOPUP)
		UPDATE_ELEMENT(ID_TOOLS_RECORDSCRIPT, UPDUI_MENUPOPUP | UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_TOOLS_STOPRECORDING, UPDUI_MENUPOPUP | UPDUI_TOOLBAR)
		
		// Child toolbar check toggles:
		UPDATE_ELEMENT(ID_EDITOR_COLOURISE, UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_EDITOR_EOLCHARS, UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_EDITOR_LINENOS, UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_EDITOR_WORDWRAP, UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_EDITOR_WRITEPROTECT, UPDUI_TOOLBAR)
		UPDATE_ELEMENT(ID_EDITOR_WHITESPACE, UPDUI_TOOLBAR)
	END_UPDATE_UI_MAP()

	BEGIN_MENU_HANDLER_MAP()
		HANDLE_MENU_COMMAND(SCHEMEMANAGER_SELECTSCHEME, OnSchemeNew)
		HANDLE_MENU_COMMAND(TOOLS_RUNTOOL, OnRunTool)
	END_MENU_HANDLER_MAP()

	EditorFactory& GetFactory();

	bool OnSchemeNew(LPVOID data);
	bool OnRunTool(LPVOID pTool);
	void OnMDISetMenu(HMENU hOld, HMENU hNew);

	LRESULT OnActivate(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled);
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);
	LRESULT OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);

	LRESULT OnDropFiles(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnChildNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled);
	LRESULT OnProjectNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled);
	LRESULT OnDblClick(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnEscapePressed(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnInitialiseFrame(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	LRESULT OnMenuSelect(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& bHandled);

	LRESULT OnMultiInstanceMsg(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/);

	LRESULT OnSaveModifiedItem(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	
	LRESULT OnUpdateFindText(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	LRESULT OnMDISetMenu(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnUpdateChildUIState(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnCloseAllOther(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnQueryEndSession(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnEndSession(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
    
	LRESULT OnStatusBarDblClick(int /*wParam*/, LPNMHDR lParam, BOOL& bHandled);

	LRESULT OnFileExit(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnFileNew(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileNewProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileNewWorkspace(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileOpen(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileSaveAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnMRUSelected(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnDockerToggle(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnMRUProjectSelected(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileOpenProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileCloseWorkspace(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileCloseAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileOpenWorkspaceState(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileSaveWorkspaceState(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	// Edit
	LRESULT OnCut(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCopy(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnPaste(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnUndo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnQuickFind(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	// View
	LRESULT OnViewToolBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnViewStatusBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnHideOutput(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	// MDI Window Arrangement
	LRESULT OnWindowCascade(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWindowTile(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWindowTileVert(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWindowArrangeIcons(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnAppAbout(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFind(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnReplace(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFindInFiles(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnOptions(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnStopTools(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnRecordScript(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnStopRecording(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnWebPNHome(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWebSFPage(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWebSFBug(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWebSFRFE(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWebPNDoc(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWebForums(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnUpdateCheck(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFindBarFind(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnSearchGoogle(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnSearchGoogleGroups(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnHelpContents(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnSchemeComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFindComboEnter(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnToolbarDropDown(WPARAM /*wParam*/, LPNMHDR /*lParam*/, BOOL& /*bHandled*/);

	bool OpenFile(LPCTSTR pathname, Scheme* pScheme = NULL, EPNEncoding encoding = eUnknown);
	
	void UpdateStatusBar();

	static BOOL CALLBACK ChildEnumProc(HWND hWnd, LPARAM lParam);

	void __stdcall ChildCloseNotify(CChildFrame* pChild, SChildEnumStruct* pES);
	void __stdcall WorkspaceChildCloseNotify(CChildFrame* pChild, SChildEnumStruct* pES);
	void __stdcall WorkspaceChildEnumNotify(CChildFrame* pChild, SChildEnumStruct* pES);
	void __stdcall ChildOptionsUpdateNotify(CChildFrame* pChild, SChildEnumStruct* pES);
	void __stdcall ChildSaveNotify(CChildFrame* pChild, SChildEnumStruct* pES);
	void __stdcall FileOpenNotify(CChildFrame* pChild, SChildEnumStruct* pES);
	void __stdcall ChildProjectNotify(CChildFrame* pChild, SChildEnumStruct* pES);

	////////////////////////////////////////////////////////////////
	// IMainFrame Implementation

public:
	virtual CWindow* GetWindow();
	
	virtual void AddMRUEntry(LPCTSTR lpszFile);
	virtual void SetStatusText(LPCTSTR text, bool bLongLife = true);
	virtual BOOL TrackPopupMenu(HMENU hMenu, UINT uFlags, int x, int y, LPTPMPARAMS lpParams = NULL, HWND hWndCaller = NULL);
	virtual void ToggleDockingWindow(EDockingWindow window, bool bSetValue = false, bool bShowing = true);
	
	virtual bool CloseAll();
	virtual bool SaveAll(bool ask = false);
	virtual bool Open(LPCTSTR pathname, bool bAddMRU = false);
	virtual void OpenProject(LPCTSTR project, bool intoExistingGroup = false);
	virtual void OpenProjectGroup(LPCTSTR projectGroup);
	virtual bool CheckAlreadyOpen(LPCTSTR filename, EAlreadyOpenAction action = (EAlreadyOpenAction)OPTIONS->GetCached(Options::OAlreadyOpenAction));
	virtual void SetActiveScheme(HWND notifier, LPVOID pScheme);

	virtual void GetOpenDocuments(DocumentList& list);
	virtual void GetOpenWorkspaceDocuments(DocumentList& list);

	virtual ToolWrapper* MakeGlobalOutputWrapper(ToolDefinition* pDefinition);
	
	virtual Projects::Workspace* GetActiveWorkspace();
	
	virtual void FindInFiles(SearchOptions* options);
	
	virtual void RecordingStopped();
	
	HWND GetJumpViewHandle(){return hCTagsWnd;}

	virtual extensions::ITextOutput* GetGlobalOutputWindow();

	int GetTabIndex(CChildFrame* child);

public:
	void ToggleOutputWindow(bool bSetValue = false, bool bShowing = true);

public:
	typedef enum {
		DW_OUTPUT = ID_VIEW_OUTPUT,
		DW_TEXTCLIPS = ID_VIEW_WINDOWS_TEXTCLIPS,
		DW_PROJECTS = ID_VIEW_WINDOWS_PROJECT,
		DW_CTAGS = ID_VIEW_WINDOWS_CTAGS,
		DW_FINDRESULTS = ID_VIEW_WINDOWS_FINDRESULTS,
		DW_SCRIPTS = ID_VIEW_WINDOWS_SCRIPTS,
		DW_BROWSER = ID_VIEW_WINDOWS_BROWSER,
		DW_OPENFILES = ID_VIEW_WINDOWS_OPENFILES
	} EDocker;

	void ToggleDockingWindow(EDocker window, bool bSetValue = false, bool bShowing = true);

private:
	void AddNewMenu(CSMenuHandle& menu);
	void AddMRUMenu(CSMenuHandle& menu);
	void AddLanguageMenu(CSMenuHandle& menu);
	void MoveMRU(CSMenuHandle& r, CSMenuHandle& a);
	void MoveNewMenu(CSMenuHandle& remove, CSMenuHandle& add);
	void MoveLanguage(CSMenuHandle& remove, CSMenuHandle& add);

	void AddMRUProjectsEntry(LPCTSTR lpszFile);

	HWND CreateToolbar(bool lowColor);
	void CreateDockingWindows();
	BOOL AddReBarBand(HWND hWndBand, LPTSTR lpstrTitle = NULL, BOOL bNewRow = FALSE, bool bUseChevrons = false, int cxWidth = 0, BOOL bFullWidthAlways = FALSE);

	void ToggleToolbar();

	void InitGUIState();
	void LoadGUIState();
	void SaveGUIState();
	void SetDefaultGUIState();

	void PerformChildEnum(SChildEnumStruct* s);
	void PerformChildEnum(lpChildEnumFn pFunction);

	void _setWindowText(LPCTSTR newText);

	void NewProject(LPCTSTR projectPath, LPCTSTR name = NULL, LPCTSTR templateGuid = NULL);
	void OpenWorkspace(LPCTSTR workspace);
	bool SaveProjects(Projects::Workspace* pWorkspace);
	DWORD SaveWorkspace(Projects::Workspace* pWorkspace);
	bool SaveWorkspaceAs(Projects::Workspace* pWorkspace);
	bool CloseWorkspace(bool bAllowCloseFiles = false, bool bAsk = true);
	bool CloseWorkspaceFiles(Projects::Workspace* pWorkspace);
	bool EnumWorkspaceWindows(SWorkspaceWindowsStruct* pWWS);
	bool CheckSaveWorkspace();
	
	bool closeAll(bool shuttingDown);

	bool getProjectsModified(ITabbedMDIChildModifiedList* pModifiedList);

	void launchFind(EFindDialogType findType);
	void launchExternalSearch(LPCTSTR searchString);

	void loadImages(USHORT id, HIMAGELIST* images, USHORT disId = 0, HIMAGELIST* disImages = NULL);

	void openFileCheckType(LPCTSTR filename, EPNEncoding encoding = eUnknown);

	void handleCommandLine(std::list<tstring>& parameters);

	void setupAccelerators(HMENU mainMenu);
	void setupToolsUI();

	inline CPNDockingWindow* getDocker(EDocker window) const;

	void handleUpdate(const Updates::UpdateAvailableDetails* details);
	
	void resetCurrentDir(bool rememberOpenPath);

	CPNDockingWindow*		m_dockingWindows[(ID_VIEW_LASTDOCKER-ID_VIEW_FIRSTDOCKER)+1];

	enum {
		SCHEME_COMBO_SIZE = 24, /* characters */
		FIND_COMBO_SIZE = 30,
		TOOLBAR_COMBO_DROPLINES = 16,
		REBAR_SAVESTATE_VERSION = 0
	};

	CommandDispatch*		m_pCmdDispatch;
	COutputView*			m_pOutputWnd;
	CFindInFilesView*		m_pFindResultsWnd;
	CClipsDocker*			m_pClipsWnd;
	CProjectDocker*			m_pProjectsWnd;
	CJumpDocker*			m_pCtagsWnd; 
	CFindExDialog*			m_pFindEx;
	CFindInFilesSink*		m_pFIFSink;
	CScriptDocker*			m_pScriptsWnd;
	CBrowseDocker*			m_pBrowseWnd;
	COpenFilesDocker*		m_pOpenFilesWnd;
	
	CScintilla				m_Dummy;			///< Scintilla often doesn't like unloading and reloading.

	TextClips::TextClipsManager* m_pTextClips;
	EditorFactory			m_ChildFactory;

	CSPopupMenu				m_NewMenu;
	CMRUMenu				m_RecentFiles;
	CMRUMenu				m_RecentProjects;
	CSchemeSwitcher			m_Switcher;

	// GUI Stuff:
	CMultiPaneStatusBarCtrl	m_StatusBar;
	commandBarClass			m_CmdBar;
	CPNStateManager			m_GUIState;
	CPNToolBar				m_ToolBar;

	HWND					hFindWnd;
	//HWND					hReplWnd;
	HWND					hCTagsWnd;

	UINT					m_uiMIMessageID;

	HACCEL					m_hToolAccel;
	HACCEL					m_hGlobalToolAccel;
	HACCEL					m_hProjAccel;

	HIMAGELIST				m_hILMain;
	HIMAGELIST				m_hILMainD;

	bool					m_bShowingDefaultStatus;
	bool					m_bIsXPOrLater;
	short					m_statusResetCounter;
	int						m_iFirstToolCmd;

	std::list<tstring>*		m_cmdLineArgs;
	tstring					m_lastOpenPath;
	DocumentPtr				m_recordingDoc;

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
