/**
 * @file mainfrm.cpp
 * @brief Main Window for Programmers Notepad 2 (Implementation)
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * Notes: It might be better to try and use a single instance of the child frame
 * menu instead of all the switching work that is done in the current version.
 */

#include "stdafx.h"

// Needed because we derive from it.
#include "tools.h"			// External Tools

// Windows and Dialogs
#include "mainfrm.h"		// This Window
#include "outputview.h"		// Output window
#include "childfrm.h"		// MDI Child
#include "finddlg.h"		// Find Dialogs
#include "OptionsPages.h"	// Options Pages
#include "aboutdlg.h"		// About Dialog

// Other stuff
#include "SchemeConfig.h"	// Scheme Configuration
#include <dbstate.h>		// Docking window state stuff...

#if defined (_DEBUG)
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

CMainFrame::CMainFrame() : m_RecentFiles(ID_MRUFILE_BASE, 4)
{
	m_FindDialog = NULL;
	m_ReplaceDialog = NULL;
	m_pOutputWnd = NULL;
	hFindWnd = NULL;
	hReplWnd = NULL;

	m_statusResetCounter = 0;

	m_CmdBar.SetCallback(this, OnMDISetMenu);
}

CMainFrame::~CMainFrame()
{
	if(m_pOutputWnd)
		delete m_pOutputWnd;
}

/**
 * You should always use this function to make a new editor, it sets
 * up the close callback properly. If you don't, things will crash :)
 */
CChildFrame* CMainFrame::NewEditor()
{
	CChildFrame* pChild = new CChildFrame;
	ATLASSERT(pChild != NULL);

	// Give the user the option to always maximise new windows.
	bool bMax = COptionsManager::GetInstance()->MaximiseNew;
	pChild->CreateEx(m_hWndMDIClient, 0, 0, bMax ? WS_MAXIMIZE : 0);

	return pChild;
}

void CMainFrame::OpenFile(LPCTSTR pathname, LPCTSTR filename, CScheme* pScheme)
{
	CChildFrame* pChild = NewEditor();
	if(filename)
	{
		pChild->PNOpenFile(pathname, filename, pScheme);
	}
	else
	{
		tstring buf;
		CFileName(pathname).GetFileName(buf);
		
		pChild->PNOpenFile(pathname, buf.c_str(), pScheme);
	}
}

void CMainFrame::OpenFile(LPCTSTR pathname)
{
	OpenFile(pathname, NULL, NULL);
}

void CMainFrame::OpenFile(LPCTSTR pathname, CScheme* pScheme)
{
	OpenFile(pathname, NULL, pScheme);
}

void CMainFrame::UpdateStatusBar()
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

void __stdcall CMainFrame::ChildCloseNotify(CChildFrame* pChild, SChildEnumStruct* pES)
{
	SCloseStruct* s = static_cast<SCloseStruct*>(pES);

	if(!pChild->CanClose())
		s->bCanClose = false;
}

void CMainFrame::ChildOptionsUpdateNotify(CChildFrame* pChild, SChildEnumStruct* pES)
{
	pChild->SendMessage(PN_OPTIONSUPDATED);
}

void CMainFrame::ChildSaveNotify(CChildFrame* pChild, SChildEnumStruct* pES)
{
	pChild->Save();
}

void CMainFrame::FileOpenNotify(CChildFrame* pChild, SChildEnumStruct* pES)
{
	SIsOpen* pS = static_cast<SIsOpen*>(pES);
	if(!pS->bFound)
	{
		TCHAR buffer[MAX_PATH+1];
		GetShortPathName(pChild->GetFileName().c_str(), buffer, MAX_PATH+1);
		if(_tcsicmp(buffer, pS->pszFilename) == 0)
		{
			pS->bFound = true;
			pS->pMatch = pChild;
		}
	}
}

void CMainFrame::OnSchemeNew(LPVOID data)
{	
	CChildFrame* pChild = NewEditor();
	pChild->SetScheme((CScheme*)data);
}

/**
 * We now update any menus which are placed in the menus of MDI children
 * (i.e. the MRU and Scheme menus...). This is called back from 
 * CPNTabbedMDICommandBarCtrl.
 */
void CMainFrame::OnMDISetMenu(HMENU hOld, HMENU hNew)
{
	CSMenuHandle r(hOld);
	CSMenuHandle a(hNew);

	MoveMRU(r, a);
	MoveLanguage(r, a);
	MoveNewMenu(r, a);
}

/* Notes: This function could be completely removed by doing the following:
 *  1. Get Find and Replace dialogs to register themselves as message filters.
 *  2. Get child windows to register as message filters.
 * What are the (dis)advantages of doing this I wonder?
 */
BOOL CMainFrame::PreTranslateMessage(MSG* pMsg)
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

BOOL CMainFrame::OnIdle()
{
	UIUpdateToolBar();

	// Because it's complicated to handle the case directly when a docking window
	// is closed by the user by clicking on its X button, we do it in OnIdle() - a bit
	// messy but hey!
	if(m_pOutputWnd != NULL)
		UISetCheck(ID_EDITOR_OUTPUTWND, m_pOutputWnd->IsWindowVisible());

	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// Message Handlers...

LRESULT CMainFrame::OnActivate(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
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

LRESULT CMainFrame::OnChildNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
{
	if(lParam == SCN_UPDATEUI || PN_MDIACTIVATE)
	{
		// Update the status bar when Scintilla thinks that we should.
		UpdateStatusBar();
	}

	/*if(lParam == PN_MDIACTIVATE)
	{
		LPCTSTR childTitle = CChildFrame::FromHandle(GetCurrentEditor())->GetTitle();
		TCHAR* titleBuf = new TCHAR[_tcslen(childTitle) + _tcslen(_T("Programmers Notepad 2"))+5];
		_tcscpy(titleBuf, childTitle);
		_tcscat(titleBuf, _T(" - "));
		_tcscat(titleBuf, _T("Programmers Notepad 2"));
		_setWindowText(titleBuf);
		delete [] titleBuf;
	}*/
			
	return TRUE;
}

LRESULT CMainFrame::OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	// Check that all of the child windows are ready to close...
	SCloseStruct s;
	s.pFunction = ChildCloseNotify; 
	s.bCanClose = true;

	PerformChildEnum(&s);
	
	if(s.bCanClose)
	{
		// We're going to exit.
		bHandled = FALSE;

		SaveGUIState();

		CloseAndFreeDlg(m_FindDialog);
		CloseAndFreeDlg(m_ReplaceDialog);
		m_FindDialog = NULL;
		m_ReplaceDialog = NULL;
	}

	return 0;
}

LRESULT CMainFrame::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
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

	InitializeDockingFrame();
	
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

	// Create docking windows...
	DWORD dwStyle = WS_OVERLAPPEDWINDOW | WS_POPUP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS;
	CRect rcBar(0, 0, 200, 70);
	
	m_pOutputWnd = new CDockingOutputWindow;
	m_pOutputWnd->Create(m_hWnd, rcBar, _T("Output"), dwStyle, WS_EX_TOOLWINDOW);
	DockWindow(*m_pOutputWnd, dockwins::CDockingSide::sBottom, 0, 1, 200, 80);

	InitGUIState();
	PostMessage(PN_INITIALISEFRAME);

	return 0;
}

LRESULT CMainFrame::OnDropFiles(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	TCHAR	buf[MAX_PATH+1];
	HDROP	hDrop = (HDROP)wParam;
	
	int files = DragQueryFile(hDrop, 0xFFFFFFFF, NULL, 0);
	for(int i = 0; i < files; i++)
	{
		DragQueryFile(hDrop, i, buf, MAX_PATH);
		OpenFile(buf);
		AddMRUEntry(buf);
	}

	DragFinish(hDrop);
	
	return 0;
}

/**
 * This function incorporates a fix from Nenad Stefanovic which 
 * re-enables double-clicking to close the window when using WTL 7. 
 * Should probably be un-necessary with future releases.
 */
LRESULT CMainFrame::OnInitMenuPopup(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
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

LRESULT CMainFrame::OnDblClick(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	BOOL b = TRUE;

	switch( COptionsManager::GetInstance()->Get(PNSK_INTERFACE, _T("MDIDoubleClickAction"), 0) )
	{
		case 0:
			OnFileOpen(0, 0, 0, b);
			break;
		case 1:
			OnFileNew(0, 0, 0, b);
			break;
		default:
			OutputDebugString(_T("PN2: Unknown MDI Double-Click Action Code"));
	}
	return 0;
}

LRESULT CMainFrame::OnEscapePressed(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	if( m_pOutputWnd->IsWindowVisible() )
	{
		m_pOutputWnd->Hide();
		return TRUE;
	}

	return FALSE;
}

LRESULT CMainFrame::OnInitialiseFrame(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	// Process cmdline params... __argv and __argc in VC++
	for(int i = 1; i < __argc; i++)
	{
		if(__argv[i][0] == _T('/') || __argv[i][0] == _T('-'))
		{
			// special params, none yet...
		}
		else
		{
			OpenFile(__argv[i]);
		}
	}

	return 0;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// Command Handlers...

LRESULT CMainFrame::OnAppAbout(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CAboutDlg dlg;
	dlg.DoModal();
	return 0;
}

LRESULT CMainFrame::OnFileExit(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	PostMessage(WM_CLOSE);
	return 0;
}

LRESULT CMainFrame::OnFileNew(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CChildFrame* pChild = NewEditor();
	tstring newscheme =	COptionsManager::GetInstance()->Get(PNSK_EDITOR, _T("NewScheme"), _T(""));
	if(newscheme.length() > 0)
        pChild->SetScheme(CSchemeManager::GetInstance()->SchemeByName(newscheme.c_str()));
	else
		pChild->SetScheme(CSchemeManager::GetInstance()->GetDefaultScheme());
		
	
	return 0;
}

LRESULT CMainFrame::OnFileOpen(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CPNOpenDialog dlgOpen(_T("All Files (*.*)|*.*|"));
	dlgOpen.m_ofn.Flags |= OFN_ALLOWMULTISELECT;

	if (dlgOpen.DoModal() == IDOK)
	{
		EAlreadyOpenAction action = COptionsManager::GetInstanceRef().AlreadyOpenAction;
		for(CPNOpenDialog::const_iterator i = dlgOpen.begin(); i != dlgOpen.end(); ++i)
		{
			if( !CheckAlreadyOpen((*i).c_str(), action) )
			{
				OpenFile((*i).c_str());
				AddMRUEntry((*i).c_str());
			}
		}
	}

	return 0;
}

LRESULT CMainFrame::OnFileSaveAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	SaveAll();
	return 0;
}

LRESULT CMainFrame::OnMRUSelected(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	LPCTSTR filename = m_RecentFiles.GetEntry(wID - ID_MRUFILE_BASE);
	OpenFile(filename);

	return 0;
}

LRESULT CMainFrame::OnCut(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::PostMessage(::GetFocus(), WM_CUT, 0, 0);
	return TRUE;
}

LRESULT CMainFrame::OnCopy(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::PostMessage(::GetFocus(), WM_COPY, 0, 0);
	return TRUE;
}

LRESULT CMainFrame::OnPaste(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::PostMessage(::GetFocus(), WM_PASTE, 0, 0);
	return TRUE;
}

LRESULT CMainFrame::OnUndo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::PostMessage(::GetFocus(), WM_UNDO, 0, 0);
	return TRUE;
}

LRESULT CMainFrame::OnViewToolBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
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

LRESULT CMainFrame::OnViewEditBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
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

LRESULT CMainFrame::OnViewStatusBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	BOOL bVisible = !::IsWindowVisible(m_hWndStatusBar);
	::ShowWindow(m_hWndStatusBar, bVisible ? SW_SHOWNOACTIVATE : SW_HIDE);
	UISetCheck(ID_VIEW_STATUS_BAR, bVisible);
	UpdateLayout();
	return 0;
}

LRESULT CMainFrame::OnOutputWindowToggle(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_pOutputWnd->Toggle();
	UISetCheck(ID_EDITOR_OUTPUTWND, m_pOutputWnd->IsWindowVisible());

	return 0;
}

LRESULT CMainFrame::OnWindowCascade(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	MDICascade();
	return 0;
}

LRESULT CMainFrame::OnWindowTile(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	MDITile();
	return 0;
}

LRESULT CMainFrame::OnWindowArrangeIcons(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	MDIIconArrange();
	return 0;
}

LRESULT CMainFrame::OnFind(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_FindDialog == NULL)
	{
		m_FindDialog = new CFindDlg;
		hFindWnd = m_FindDialog->Create(m_hWnd);
	}

	CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor());
	if(pChild)
		m_FindDialog->Show(pChild->GetTextView()->GetCurrentWord().c_str());
	else
		m_FindDialog->Show(NULL);
	

	return 0;
}

LRESULT CMainFrame::OnReplace(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_ReplaceDialog == NULL)
	{
		m_ReplaceDialog = new CReplaceDlg;
		hReplWnd = m_ReplaceDialog->Create(m_hWnd);
	}

	CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor());
	if(pChild)
		m_ReplaceDialog->Show(pChild->GetTextView()->GetCurrentWord().c_str());
	else
		m_ReplaceDialog->Show(NULL);

	return 0;
}

LRESULT CMainFrame::OnOptions(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{		
	CSchemeManager* pSM = CSchemeManager::GetInstance();
	LPCTSTR currentScheme = NULL;

	CChildFrame* pFrame = CChildFrame::FromHandle(GetCurrentEditor());
	if(pFrame)
	{
		currentScheme = pFrame->GetTextView()->GetCurrentScheme()->GetName();
	}

	SchemeConfigParser		schemeconfig(currentScheme);
	
	COptionsPageGeneral		general;

	COptionsPageStyle		pageStyle(&schemeconfig);
	COptionsPageSchemes		pageSchemes(&schemeconfig);
	COptionsPageNewFiles	pageNewFiles(&schemeconfig);
	COptionsPageTools		pageTools(&schemeconfig);

	schemeconfig.LoadConfig(pSM->GetPath(), pSM->GetCompiledPath());

	COptionsDialog options;
	options.AddPage(&general);
	options.AddPage(&pageStyle);
	options.AddPage(&pageSchemes);
	options.AddPage(&pageNewFiles);
	options.AddPage(&pageTools);

	options.SetInitialPage(&general);

	if( options.DoModal() == IDOK )
	{
		///@todo more dirty checking...

		// pass in true to cache menu resources.
		SchemeToolsManager::GetInstance()->ReLoad(true);

		if( pageStyle.IsDirty() || pageSchemes.IsDirty() )
            CSchemeManager::GetInstance()->Compile();

		PerformChildEnum(ChildOptionsUpdateNotify);
	}

	return 0;
}

LRESULT CMainFrame::OnToolsDummy(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	BOOL b;
	MessageBox(_T("You can add external tools to this menu using the Tools\n section of the options dialog.\n\nThis dialog will open now so that you can do so."), _T("Programmers Notepad"), MB_OK);
	OnOptions(0,0,0, b);
	return 0;
}

LRESULT CMainFrame::OnWebPNHome(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::ShellExecute(m_hWnd, _T("open"), _T("http://www.pnotepad.org/"), NULL, NULL, SW_SHOW);
	
	return 0;
}

LRESULT CMainFrame::OnWebSFPage(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::ShellExecute(m_hWnd, _T("open"), _T("http://www.sf.net/projects/pnotepad/"), NULL, NULL, SW_SHOW);

	return 0;
}

LRESULT CMainFrame::OnWebSFBug(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::ShellExecute(m_hWnd, _T("open"), _T("http://sourceforge.net/tracker/?func=add&group_id=45545&atid=443219"), NULL, NULL, SW_SHOW);

	return 0;
}

/**
 * This function first looks to see if a file is already open, and then carries
 * out a specific action if it is.
 * @param filename to find - first gets converted to short form.
 * @param action What to do if the file is found.
 */
bool CMainFrame::CheckAlreadyOpen(LPCTSTR filename, EAlreadyOpenAction action)
{
	TCHAR shortForm[MAX_PATH+1];
	GetShortPathName(filename, shortForm, MAX_PATH+1);
	
	SIsOpen s;
	s.pFunction = FileOpenNotify;
	s.pszFilename = shortForm;
	s.bFound = false; // not found
	s.pMatch = NULL;

	PerformChildEnum(&s);

	if(s.bFound)
	{
		switch( action )
		{
			case eSwitch:
				{
					s.pMatch->BringWindowToTop();
				}
				break;
			case eWarnOpen:
				{
					CString str;
					str.Format(_T("Do you want to open another copy of %s?"), filename);
					if( MessageBox(str, _T("Programmers Notepad 2"), MB_YESNO) == IDYES )
					{
						// Just claim the file wasn't open...
						s.bFound = false;
					}
					else
						s.pMatch->BringWindowToTop();
				}
				break;
			case eOpenAgain:
				{
					// Just claim the file wasn't open...
					s.bFound = false;
				}
				break;
		}
	}

	return s.bFound;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// Menu code...

void CMainFrame::AddNewMenu(CSMenuHandle& menu)
{
	CSMenuHandle file = menu.GetSubMenu(0);
	::ModifyMenu(file.GetHandle(), 0, MF_BYPOSITION | MF_POPUP, (UINT)(HMENU)m_NewMenu, _T("&New"));
}

void CMainFrame::AddMRUMenu(CSMenuHandle& menu)
{
	CSMenuHandle file(menu.GetSubMenu(0));
	::InsertMenu(file.GetHandle(), ID_APP_EXIT, MF_BYCOMMAND | MF_POPUP, (UINT)(HMENU)m_RecentFiles, _T("&Recent Files"));
	::InsertMenu(file.GetHandle(), ID_APP_EXIT, MF_BYCOMMAND | MF_SEPARATOR, 0, NULL);
}

void CMainFrame::AddLanguageMenu(CSMenuHandle& menu)
{
	CSMenuHandle view(menu.GetSubMenu(2));
	::ModifyMenu(view.GetHandle(), ID_VIEW_CHANGESCHEME, MF_BYCOMMAND | MF_POPUP, (UINT)(HMENU)m_Switcher, _T("&Change Scheme"));
}

void CMainFrame::MoveMRU(CSMenuHandle& r, CSMenuHandle& a)
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

void CMainFrame::MoveNewMenu(CSMenuHandle& remove, CSMenuHandle& add)
{
	CSMenuHandle file( remove.GetSubMenu(0) );
	::ModifyMenu(file, 0, MF_BYPOSITION | MF_STRING, ID_FILE_NEW, _T("n"));
	AddNewMenu(add);
}

void CMainFrame::MoveLanguage(CSMenuHandle& remove, CSMenuHandle& add)
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

/**
 * Initialise the GUI state manager with all of the GUI items that it controls.
 */
void CMainFrame::InitGUIState()
{
	// Create a list of the docking windows to manage.
	sstate::CDockWndMgrEx dockers(m_hWnd);
	dockers.Add(sstate::CDockingWindowStateAdapterEx<CDockingOutputWindow>(*m_pOutputWnd));
	
	tstring statekey(pnregroot);
	statekey += PNSK_INTERFACE;
	statekey += PNSK_DEFGUI;

	m_GUIState.Initialize(statekey.c_str(), m_hWnd/*, SW_SHOWMAXIMIZED*/);
	m_GUIState.Add(sstate::CRebarStateAdapter(m_hWndToolBar));
	m_GUIState.Add(dockers);

	LoadGUIState();
}

/**
 * Load a named or default GUI state from the registry.
 */
void CMainFrame::LoadGUIState(LPCTSTR stateName)
{
	tstring* pConfigName = NULL;
	if(stateName)
	{
		pConfigName = new tstring(pnregroot);
		*pConfigName += PNSK_INTERFACE;
		*pConfigName += _T('\\');
		*pConfigName += stateName;
	}

	if( !m_GUIState.Restore(pConfigName) )
	{
		SetDefaultGUIState();
	}

	// Set initial UpdateUI state...
	UISetCheck(ID_EDITOR_OUTPUTWND, m_pOutputWnd->IsWindowVisible());

	UpdateLayout();

	if(pConfigName)
		delete pConfigName;
}

/**
 * Save the default or a named GUI state to the registry.
 */
void CMainFrame::SaveGUIState(LPCTSTR stateName)
{
	tstring* pConfigName = NULL;
	if(stateName)
	{
		pConfigName = new tstring(pnregroot);
		*pConfigName += PNSK_INTERFACE;
		*pConfigName += _T('\\');
		*pConfigName += stateName;
	}

	m_GUIState.Store(pConfigName);
	
	if(pConfigName)
		delete pConfigName;
}

/**
 * Configure docking windows etc. to their default state.
 */
void CMainFrame::SetDefaultGUIState()
{
	// Dock the output window to the bottom of the main frame, hide it.
	m_pOutputWnd->Hide();
}

void CMainFrame::PerformChildEnum(SChildEnumStruct* s)
{
	s->pMainFrame = this;
	EnumChildWindows(m_hWndMDIClient, ChildEnumProc, reinterpret_cast<LPARAM>(s));
}

void CMainFrame::PerformChildEnum(lpChildEnumFn pFunction)
{
	SChildEnumStruct s = {this, pFunction};
	EnumChildWindows(m_hWndMDIClient, ChildEnumProc, reinterpret_cast<LPARAM>(&s));
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// IMainFrame implementation...

CWindow* CMainFrame::GetWindow()
{
	return static_cast<CWindow*>(this);
}

class GlobalOutputWrapper : public ToolWrapperT<CMainFrame, COutputView>
{
typedef ToolWrapperT<CMainFrame, COutputView> baseClass;
public:
	GlobalOutputWrapper(CMainFrame* pOwner, COutputView* pView, CChildFrame* pActiveChild, const ToolDefinition& definition)
		: baseClass(pOwner, pView, pActiveChild, definition)
	{}
};

ToolWrapper* CMainFrame::MakeGlobalOutputWrapper(ToolDefinition* pDefinition)
{
	CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor());
	return new GlobalOutputWrapper(this, m_pOutputWnd->GetView(), pChild, *pDefinition);
}

void CMainFrame::AddMRUEntry(LPCTSTR lpszFile)
{
	m_RecentFiles.AddEntry(lpszFile);
	m_RecentFiles.UpdateMenu();
}

void CMainFrame::SetActiveScheme(HWND notifier, LPVOID pScheme)
{
	if(notifier == MDIGetActive())
	{
		m_Switcher.SetActiveScheme(static_cast<CScheme*>(pScheme));
	}
}

BOOL CMainFrame::TrackPopupMenu(HMENU hMenu, UINT uFlags, int x, int y, LPTPMPARAMS lpParams, HWND hWndCaller)
{
	if( hWndCaller != NULL )
		uFlags |= TPM_RETURNCMD;
	
	BOOL bRet = m_CmdBar.TrackPopupMenu(hMenu, uFlags, x, y, lpParams);

	if( hWndCaller != NULL && bRet != 0 )
		::SendMessage(hWndCaller, WM_COMMAND, bRet, NULL);

	return bRet;
}

void CMainFrame::SetStatusText(LPCTSTR text, bool bLongLife)
{
	if(text)
	{
		m_StatusBar.SetPaneText(ID_DEFAULT_PANE, text, (m_bIsXPOrLater ? SBT_NOBORDERS : 0));
		m_bShowingDefaultStatus = false;
		if(bLongLife)
			m_statusResetCounter = 2;
	}
	else
		if(!m_bShowingDefaultStatus)
		{
			if(m_statusResetCounter != 0)
				m_statusResetCounter--;
			else
			{
				m_StatusBar.SetPaneText(ID_DEFAULT_PANE, _T("Ready"), (m_bIsXPOrLater ? SBT_NOBORDERS : 0));
				m_bShowingDefaultStatus = true;
			}
		}
}

void CMainFrame::SaveAll()
{
	PerformChildEnum(ChildSaveNotify);
}

BOOL CALLBACK CMainFrame::ChildEnumProc(HWND hWnd, LPARAM lParam)
{
	if(::GetWindow(hWnd, GW_OWNER))
		return TRUE;

	CChildFrame* pChild = CChildFrame::FromHandle(hWnd);
	if(pChild != NULL)
	{
		SChildEnumStruct* s = reinterpret_cast<SChildEnumStruct*>(lParam);
		lpChildEnumFn fn = s->pFunction;
		(s->pMainFrame->*fn)(pChild, s);
	}

	return TRUE;
}

void CMainFrame::_setWindowText(LPCTSTR lpszNew)
{
	#define _countof(array) (sizeof(array)/sizeof(array[0]))

	int nNewLen = lstrlen(lpszNew);
	TCHAR szOld[256];
	// fast check to see if text really changes (reduces flash in controls)
	if (nNewLen > _countof(szOld) ||
		GetWindowText(szOld, _countof(szOld)) != nNewLen ||
		lstrcmp(szOld, lpszNew) != 0)
	{
		// change it
		SetWindowText(lpszNew);
	}

	#undef _countof
}

void CMainFrame::ToggleOutputWindow(bool bSetValue, bool bShowing)
{
	if(bSetValue)
	{
		if(bShowing)
		{
			if( !m_pOutputWnd->IsWindowVisible() )
				m_pOutputWnd->Show();
		}
		else
		{
			if( m_pOutputWnd->IsWindowVisible() )
				m_pOutputWnd->Hide();
		}
	}
	else
		m_pOutputWnd->Toggle();
}