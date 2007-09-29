/**
 * @file mainfrm.cpp
 * @brief Main Window for Programmers Notepad 2 (Implementation)
 * @author Simon Steele
 * @note Copyright (c) 2002-2007 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * Notes: It might be better to try and use a single instance of the child frame
 * menu instead of all the switching work that is done in the current version.
 */

#include "stdafx.h"
#include "resource.h"

// Needed because we derive from it.
#include "tools.h"				// External Tools
#include "pndocking.h"			// Docking Window Stuff

// Windows and Dialogs
#include "mainfrm.h"			// This Window
#include "outputview.h"			// Output window
#include "childfrm.h"			// MDI Child
#include "OptionsPages.h"		// Options Pages
#include "aboutdlg.h"			// About Dialog
#include "pndialogs.h"			// Misc Dialogs.
#include "textclipsview.h"		// Text-Clips Docker...
#include "jumpview.h"			// Tags Docker
#include "project.h"			// Projects
#include "projectprops.h"		// Project Properties
#include "projectview.h"		// Projects Docker...
#include "findex.h"				// Find Dialog
#include "findinfiles.h"		// Find in Files
#include "findinfilesview.h"	// Find in Files view...
#include "newprojectdialog.h"	// New Projects Dialog
#include "workspacestate.h"		// Save Workspace State
#include "ScriptRegistry.h"		// Scripts Registry
#include "scriptview.h"			// Scripts Docker
#include "toolsmanager.h"		// Tools Manager
#include "extapp.h"
#include "textclips.h"			// Text Clips

// Other stuff
#include "SchemeConfig.h"		// Scheme Configuration
#include <dbstate.h>			// Docking window state stuff...

#include <htmlhelp.h>

#include "projectholder.h"

using namespace L10N;

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

const DWORD ToolbarCmds[4] = {
	ID_VIEW_TOOLBARS_SCHEMES,
	ID_VIEW_TOOLBARS_FIND,
	ID_VIEW_TOOLBAR_EDIT,
	ID_VIEW_TOOLBAR,
};

const DWORD ToolbarIds[4] = {
	ATL_IDW_BAND_FIRST + 3,
	ATL_IDW_BAND_FIRST + 4,
	ATL_IDW_BAND_FIRST + 2,
	ATL_IDW_BAND_FIRST + 1,
};

CMainFrame::CMainFrame(CommandDispatch* commands, std::list<tstring>* cmdLineArgs) : m_RecentFiles(ID_MRUFILE_BASE, 4), m_RecentProjects(ID_MRUPROJECT_BASE, 4)
{
	m_pCmdDispatch = commands;
	m_pFindEx = NULL;
	m_pOutputWnd = NULL;
	m_pFindResultsWnd = NULL;
	m_pClipsWnd = NULL;
	m_pCtagsWnd = NULL;
	m_pProjectsWnd = NULL;
	hFindWnd = NULL;

	m_statusResetCounter = 0;

	m_CmdBar.SetCallback(this, &CMainFrame::OnMDISetMenu);

	m_hToolAccel = NULL;
	m_hGlobalToolAccel = NULL;

	m_uiMIMessageID = g_Context.m_miManager->GetMessageID();

	m_iFirstToolCmd = ID_TOOLS_DUMMY;

	m_hProjAccel = NULL;

	SchemeTools* pGlobalTools = ToolsManager::GetInstance()->GetGlobalTools();
	pGlobalTools->AllocateMenuResources(m_pCmdDispatch);
	m_hGlobalToolAccel = pGlobalTools->GetAcceleratorTable();

	m_hILMain = NULL;
	m_hILMainD = NULL;
	m_hILEdit = NULL;
	m_hILFind = NULL;

	// If v5 then greater than v5.0 else v6 or better
	m_bIsXPOrLater = 
		(g_Context.OSVersion.dwPlatformId == VER_PLATFORM_WIN32_NT) &&
		(( (g_Context.OSVersion.dwMajorVersion == 5) && (g_Context.OSVersion.dwMinorVersion > 0) ) ||
		 (g_Context.OSVersion.dwMajorVersion >= 6) );

	// Store command-line arguments for use later
	m_cmdLineArgs = cmdLineArgs;

	// This text clip manager will be shared by the text clips view and editors
	m_pTextClips = new TextClips::TextClipsManager;
}

CMainFrame::~CMainFrame()
{
	if(m_pOutputWnd)
		delete m_pOutputWnd;

	if(m_pFindResultsWnd)
		delete m_pFindResultsWnd;

	if(m_pClipsWnd)
		delete m_pClipsWnd;

	if(m_pProjectsWnd)
		delete m_pProjectsWnd;

	if(m_pCtagsWnd)
		delete m_pCtagsWnd;

	if(m_pScriptsWnd)
		delete m_pScriptsWnd;

	if(m_pTextClips)
		delete m_pTextClips;
}

/**
 * You should always use this function to make a new editor, it sets
 * up the close callback properly. If you don't, things will crash :)
 */
CChildFrame* CMainFrame::NewEditor()
{
	DocumentPtr pD(new Document());
	CChildFrame* pChild = new CChildFrame(pD, m_pCmdDispatch, m_pTextClips);
	PNASSERT(pChild != NULL);
	pD->AddChildFrame(pChild);

	// Give the user the option to always maximise new windows.
	bool bMax = OPTIONS->GetCached(Options::OMaximiseNew) != 0;
	pChild->CreateEx(m_hWndMDIClient, 0, 0, bMax ? WS_MAXIMIZE : 0);

	g_Context.ExtApp->OnNewDocument( pD );

	return pChild;
}

bool CMainFrame::OpenFile(LPCTSTR pathname, Scheme* pScheme, EPNEncoding encoding)
{
	bool bRet = false;

	CChildFrame* pChild = NewEditor();
	if(pathname)
	{
		bRet = pChild->PNOpenFile(pathname, pScheme, encoding);
	}

	return bRet;
}

bool CMainFrame::Open(LPCTSTR pathname, bool bAddMRU)
{
	bool bRet = OpenFile(pathname, NULL);
	if(bAddMRU && bRet)
	{
		AddMRUEntry(pathname);
	}

	return bRet;
}

LPCTSTR encodingNames[] = 
{
	_T("ANSI"),
	_T("UTF-16 BE"),
	_T("UTF-16 LE"),
	_T("UTF-8"),
	_T("UTF-8 ?")
};

LPCTSTR lineEndingNames[] = 
{
	_T("CR+LF"),
	_T("CR"),
	_T("LF")
};

void CMainFrame::UpdateStatusBar()
{
	CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor());
	
	if(pChild)
	{
		m_StatusBar.SetPaneText(ID_MOD_PANE, pChild->GetModified() ? LS(IDS_MODIFIED) : _T(""));
		m_StatusBar.SetPaneText(ID_ENC_PANE, encodingNames[pChild->GetTextView()->GetEncoding()]);
		m_StatusBar.SetPaneText(ID_LINEENDS_PANE, lineEndingNames[pChild->GetTextView()->GetEOLMode()]);
		m_StatusBar.SetPaneText(ID_INS_PANE, pChild->GetTextView()->GetOvertype() ? _T("OVR") : _T("INS"));
		pChild->SetPosStatus(m_StatusBar);
	}
	else
	{
		m_StatusBar.SetPaneText(ID_MOD_PANE, _T(""));
		m_StatusBar.SetPaneText(ID_ENC_PANE, _T(""));
		m_StatusBar.SetPaneText(ID_LINEENDS_PANE, _T(""));
		m_StatusBar.SetPaneText(ID_POS_PANE, _T(""));
		m_StatusBar.SetPaneText(ID_INS_PANE, _T(""));
		//SetStatusText(NULL);
	}
}

void __stdcall CMainFrame::ChildCloseNotify(CChildFrame* pChild, SChildEnumStruct* pES)
{
	SCloseStruct* s = static_cast<SCloseStruct*>(pES);

	// check bCanClose first to see if we've already decided not to close.
	if(s->bCanClose && !pChild->CanClose())
		s->bCanClose = false;
}

void __stdcall CMainFrame::WorkspaceChildEnumNotify(CChildFrame* pChild, SChildEnumStruct* pES)
{
	SWorkspaceWindowsStruct* s = static_cast<SWorkspaceWindowsStruct*>(pES);

	if(s->bInProjectGroupOnly)
	{
		tstring filename = pChild->GetFileName().c_str();
		Projects::File* pFile = s->pWorkspace->FindFile(filename.c_str());

		if(pFile)
			s->FoundWindows.push_back(pChild);
	}
	else
	{
		s->FoundWindows.push_back(pChild);
	}
}

void __stdcall CMainFrame::WorkspaceChildCloseNotify(CChildFrame* pChild, SChildEnumStruct* pES)
{
	SWorkspaceWindowsStruct* s = static_cast<SWorkspaceWindowsStruct*>(pES);

	tstring filename = pChild->GetFileName().c_str();
	Projects::File* pFile = s->pWorkspace->FindFile(filename.c_str());

	if(pFile)
	{
		s->FoundWindows.push_back(pChild);

		// check bCanClose first to see if we've already decided not to close.
		if(s->bCanClose && !pChild->CanClose())
			s->bCanClose = false;
	}
}

void __stdcall CMainFrame::ChildOptionsUpdateNotify(CChildFrame* pChild, SChildEnumStruct* pES)
{
	pChild->SendMessage(PN_OPTIONSUPDATED);
}

void __stdcall CMainFrame::ChildSaveNotify(CChildFrame* pChild, SChildEnumStruct* pES)
{
	if(pChild->GetModified())
		pChild->Save();
}

void __stdcall CMainFrame::FileOpenNotify(CChildFrame* pChild, SChildEnumStruct* pES)
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

bool CMainFrame::CloseAll()
{
	return closeAll(false);
}

bool CMainFrame::closeAll(bool shuttingDown)
{
	if(SaveAll(true))
	{
		if(shuttingDown)
		{
			if(OPTIONS->Get(PNSK_INTERFACE, _T("SaveWorkspace"), false))
			{
				WorkspaceState wss;
				wss.Save();
			}
		}

		// Close workspace, don't ask anything...
		CloseWorkspace(false, false);
		
		// Close all files, don't ask anything...
		m_tabbedClient.CloseAll(true);

		return true;
	}
	else
		return false;
}

bool CMainFrame::OnSchemeNew(LPVOID data)
{	
	CChildFrame* pChild = NewEditor();
	pChild->SetScheme((Scheme*)data);

	return true; // we handled it.
}

bool CMainFrame::OnRunTool(LPVOID pTool)
{
	ToolDefinition* pToolDef = static_cast<ToolDefinition*>(pTool);

	// If there's an editor open, and the tool is not for a project,
	// let the child frame handle it...
	HWND curEditor = GetCurrentEditor();
	if(curEditor != NULL && !pToolDef->IsProjectTool())
		return false;

	ToolWrapperPtr pWrapper;
	if(	pToolDef->GlobalOutput() )
	{
		pWrapper.reset( MakeGlobalOutputWrapper(pToolDef) );
	}
	else
	{
		return false;
	}

	pWrapper->SetNotifyWindow(m_hWnd);

	ToolOwner::GetInstance()->RunTool(pWrapper, this);

	return true;
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

	MoveNewMenu(r, a);
	MoveMRU(r, a);
	MoveLanguage(r, a);
}

BOOL CMainFrame::PreTranslateMessage(MSG* pMsg)
{
	if((pMsg->message >= WM_KEYFIRST) && (pMsg->message <= WM_KEYLAST))
	{
		// We need Ctrl-Up to see when to stop monkeying with the tab order.
		if(pMsg->message == WM_KEYUP && pMsg->wParam == VK_CONTROL)
		{
			m_tabbedClient.ControlUp();
		}
		
		if(m_hToolAccel != 0 && ::TranslateAccelerator(m_hWnd, m_hToolAccel, pMsg))
			return TRUE;

		if(m_hGlobalToolAccel != 0 && ::TranslateAccelerator(m_hWnd, m_hGlobalToolAccel, pMsg))
			return TRUE;

		if(m_hProjAccel != 0 && ::TranslateAccelerator(m_hWnd, m_hProjAccel, pMsg))
			return TRUE;
	}

	if(baseClass::PreTranslateMessage(pMsg))
		return TRUE;

	if(::IsWindow(m_hWndMDIClient))
	{
		HWND hWnd = MDIGetActive();
		if(hWnd != NULL)
			return (BOOL)::SendMessage(hWnd, WM_FORWARDMSG, 0, (LPARAM)pMsg);
	}

	return FALSE;
}

BOOL CMainFrame::OnIdle()
{
	// Deal with docking window visible states...
	for(int i = 0; i <= ID_VIEW_LASTDOCKER - ID_VIEW_FIRSTDOCKER; i++)
	{
		UISetCheck( ID_VIEW_FIRSTDOCKER + i, m_dockingWindows[i]->IsWindowVisible() );
	}

	HWND hWnd = MDIGetActive();
	m_SchemeCombo.EnableWindow( hWnd != NULL );

	bool bChild = false;
	bool bCanSave = false;
	
	DWORD wStartPos;
	DWORD wEndPos;
	LRESULT lResult = ::SendMessage(::GetFocus(), EM_GETSEL, (WPARAM)&wStartPos, (LPARAM)&wEndPos);

	bool bHasSel = lResult && (wStartPos != wEndPos);

	if(hWnd != NULL)
	{
		CChildFrame* pChild = CChildFrame::FromHandle(hWnd);
		if(pChild != NULL)
		{
			bChild = true;
			bCanSave = pChild->GetModified();

			CTextView* pTV = pChild->GetTextView();
			if(pTV)
			{
				bHasSel = pTV->GetSelLength() > 0;
			}
		}
	}
	else
	{
		// Clear the status bar if there are no files loaded.
		UpdateStatusBar();
	}
	
	UIEnable(ID_FILE_CLOSE, bChild);
	UIEnable(ID_FILE_SAVE, bCanSave);
	UIEnable(ID_EDIT_CUT, bHasSel);
	UIEnable(ID_EDIT_COPY, bHasSel);

	lResult = ::SendMessage(::GetFocus(), EM_CANUNDO, 0, 0);
	UIEnable(ID_EDIT_UNDO, lResult);
	lResult = ::SendMessage(::GetFocus(), SCI_CANREDO, 0, 0);
	UIEnable(ID_EDIT_REDO, lResult);

	if (::OpenClipboard(NULL))
	{
		UIEnable(ID_EDIT_PASTE, ::IsClipboardFormatAvailable(CF_TEXT) || ::IsClipboardFormatAvailable(CF_UNICODETEXT));
		::CloseClipboard();
	}
	else 
	{
		UIEnable(ID_EDIT_PASTE,FALSE);
	}

	if(m_pProjectsWnd != NULL)
	{
		Projects::Workspace* pWorkspace = m_pProjectsWnd->GetWorkspace();
		UIEnable(ID_FILE_CLOSEWORKSPACE, (pWorkspace != NULL));

		/*if(pWorkspace)
		{
			setupToolsUI();
		}*/
	}

	UIUpdateToolBar();

	bool bToolsRunning = false;
	if( ToolOwner::HasInstance() )
		bToolsRunning = ToolOwner::GetInstance()->HaveRunningTools(this);

	CSMenuHandle menu(m_hMenu);
	menu.EnableMenuItem(ID_TOOLS_STOPTOOLS, bToolsRunning);

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
	if(lParam == SCN_UPDATEUI || lParam == PN_MDIACTIVATE)
	{
		// Update the status bar when Scintilla thinks that we should.
		UpdateStatusBar();
	}

	// Update scheme combo...
	if(lParam == PN_MDIACTIVATE || lParam == PN_SCHEMECHANGED)
	{
		HWND hMDIChild = MDIGetActive();
		if(hMDIChild != NULL)
		{
			CChildFrame* pChild = CChildFrame::FromHandle(hMDIChild);
			if(pChild != NULL)
			{
				Scheme* pScheme = pChild->GetTextView()->GetCurrentScheme();
				for(int i = 0; i < m_SchemeCombo.GetCount(); i++)
				{
					if( pScheme == static_cast<Scheme*>( m_SchemeCombo.GetItemDataPtr(i) ) )
					{
						m_SchemeCombo.SetCurSel(i);
						break;
					}
				}

				m_hToolAccel = pChild->GetToolAccelerators();
			}
			else
				m_hToolAccel = NULL;
		}
		else
			m_hToolAccel = NULL;
	}
	else if(lParam == PN_MDIDESTROY)
	{
		SetStatusText(NULL);
	}
	
	return TRUE;
}

LRESULT CMainFrame::OnProjectNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
{
	bHandled = FALSE;

	setupToolsUI();

	return TRUE;
}

LRESULT CMainFrame::OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	if(closeAll(true)) // close all, and we're shutting down.
	{
		bHandled = FALSE;

		// Still going to close...
		SaveGUIState();

		FindInFiles::GetInstance()->Stop();

		::ImageList_Destroy( m_hILMain );
		::ImageList_Destroy( m_hILMainD );
		::ImageList_Destroy( m_hILEdit );

		CloseAndFreeDlg(m_pFindEx);
		m_pFindEx = NULL;
	}

	return 0;
}

CSize CMainFrame::GetGUIFontSize()
{
	CClientDC dc(m_hWnd);
	dc.SelectFont((HFONT) GetStockObject( DEFAULT_GUI_FONT ));		
	TEXTMETRIC tm;
	dc.GetTextMetrics( &tm );
	//int cxChar = tm.tmAveCharWidth;
	//int cyChar = tm.tmHeight + tm.tmExternalLeading;

	return CSize( tm.tmAveCharWidth, tm.tmHeight + tm.tmExternalLeading);
}

#define PN_BETTER_TOOLBAR_STYLE \
	(WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | CCS_NODIVIDER | CCS_NORESIZE | CCS_NOPARENTALIGN | TBSTYLE_TOOLTIPS | TBSTYLE_FLAT | TBSTYLE_LIST)

HWND CMainFrame::CreateFindToolbar()
{
	HWND hWnd = CreateSimpleToolBarCtrl(m_hWnd, IDR_TBR_FIND, FALSE, PN_BETTER_TOOLBAR_STYLE, TBR_FIND);

	if(!hWnd)
		return 0;

	CToolBarCtrl fToolbar(hWnd);
	fToolbar.SetExtendedStyle(TBSTYLE_EX_DRAWDDARROWS);
	// Only IE 501
	fToolbar.SetExtendedStyle(fToolbar.GetExtendedStyle() | /*TBSTYLE_EX_HIDECLIPPEDBUTTONS |*/ TBSTYLE_EX_MIXEDBUTTONS);

	CSize sizeChar = GetGUIFontSize();
	int cx = FIND_COMBO_SIZE * sizeChar.cx;

	TBBUTTONINFO tbi;
	RECT rc;

	tbi.cbSize = sizeof TBBUTTONINFO;
	tbi.dwMask = TBIF_STYLE | TBIF_SIZE;
	tbi.fsStyle = TBSTYLE_SEP;
	tbi.cx = (unsigned short)cx;

	fToolbar.SetButtonInfo(ID_PLACEHOLDER_FINDCOMBO, &tbi);
	fToolbar.GetItemRect(1, &rc);

	rc.bottom = TOOLBAR_COMBO_DROPLINES * sizeChar.cy;
	rc.left += 1; // slight offset from previous and next buttons.
	rc.right -= 1;

	//m_FindImages.Create(16, 16, ILC_COLOR32 | ILC_MASK, 3, 1);
	//CBitmap bmp;
	//bmp.LoadBitmap(IDB_FINDTOOLBAR);
	//m_FindImages.Add(bmp, RGB(255, 0, 255));

	//fToolbar.SetImageList(m_FindImages.m_hImageList);

	HWND hWndCombo = m_FindCombo.Create(m_hWnd, rc, _T("FINDTEXTCOMBO"), 
		CBS_DROPDOWN | CBS_AUTOHSCROLL | WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL,
		0, IDC_FINDCOMBO, _T("Software\\Echo Software\\PN2\\AutoComplete\\FindToolbar"));
	hWndCombo;
	ATLASSERT(hWndCombo != 0);

	m_FindCombo.SetParent(hWnd);
	m_FindCombo.SetFont((HFONT)GetStockObject( DEFAULT_GUI_FONT ));
	m_FindCombo.SetOwnerHWND(m_hWnd); // Get enter notifications.

	// Set the drop-down button...
	int nIndex = fToolbar.CommandToIndex(ID_FINDTYPE_BUTTON);
	ATLASSERT(nIndex != -1);
	
	tstring fs = LS(IDS_FIND);

	// Add drop-down style to the button
	tbi.dwMask = TBIF_STYLE;
	fToolbar.GetButtonInfo(ID_FINDTYPE_BUTTON, &tbi);
	tbi.fsStyle |= BTNS_DROPDOWN;
	tbi.fsStyle |= BTNS_SHOWTEXT | BTNS_AUTOSIZE;
	tbi.dwMask |= TBIF_TEXT;
	tbi.pszText = const_cast<LPSTR>(fs.c_str());
	fToolbar.SetButtonInfo(ID_FINDTYPE_BUTTON, &tbi);

	return hWnd;
}

HWND CMainFrame::CreateSchemeToolbar()
{
	HWND hWnd = CreateSimpleToolBarCtrl(m_hWnd, IDR_TBR_SCHEME, FALSE, ATL_SIMPLE_TOOLBAR_PANE_STYLE, TBR_SCHEME);

	if (!hWnd) 
		return 0;

	CToolBarCtrl sToolbar(hWnd);

	CSize sizeChar = GetGUIFontSize();
	int cx = SCHEME_COMBO_SIZE * sizeChar.cx;

	RECT rc;

	TBBUTTONINFO tbi;
	tbi.cbSize = sizeof TBBUTTONINFO;		
	tbi.dwMask = TBIF_STYLE | TBIF_SIZE;
	tbi.fsStyle = TBSTYLE_SEP;
	tbi.cx = (unsigned short)cx;
	
	sToolbar.SetButtonInfo(ID_PLACEHOLDER_SCHEMECOMBO, &tbi); 						
	sToolbar.GetItemRect(0, &rc); 

	rc.bottom = rc.top + TOOLBAR_COMBO_DROPLINES * sizeChar.cy;
	
	HWND hWndCombo =  m_SchemeCombo.Create(m_hWnd, rc, NULL, CBS_DROPDOWNLIST | CBS_AUTOHSCROLL | WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_VSCROLL,
		0, IDC_SCHEMECOMBO);
	hWndCombo;
	ATLASSERT(hWndCombo != 0);
	
	m_SchemeCombo.SetParent(hWnd); 
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

	return hWnd;
}

template<class TWnd>
TWnd* CreateDockerWindow(TWnd* pWnd, LPCTSTR title, CRect& rect, CMainFrame* owner, CPNDockingWindow** pArr, int index,
					bool bDock, dockwins::CDockingSide side = dockwins::CDockingSide::sLeft,
					int nBar = 0, float fPctPos = 0)
{
	DWORD dwStyle = WS_OVERLAPPEDWINDOW | WS_POPUP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS;
	CRect rcBar(0, 0, 200, 70);

	CPNDockingWindow* pDocker = new CPNDockingWindow(title);
	pDocker->Create(owner->m_hWnd, rect, title, dwStyle, WS_EX_TOOLWINDOW);

	pWnd->Create(pDocker->m_hWnd, rect, title);

	pDocker->SetClientFlatOutline(true);
	pDocker->SetClient(pWnd->m_hWnd);

	pArr[index] = pDocker;

	if(bDock)
	{
		owner->DockWindow(*pDocker, side, nBar, fPctPos, rect.Width(), rect.Height());
	}

	return pWnd;
}

template<class TWnd>
TWnd* CreateDocker(LPCTSTR title, CRect& rect, CMainFrame* owner, CPNDockingWindow** pArr, int index,
					bool bDock, dockwins::CDockingSide side = dockwins::CDockingSide::sLeft,
					int nBar = 0, float fPctPos = 0)
{
	return CreateDockerWindow(new TWnd, title, rect, owner, pArr, index, bDock, side, nBar, fPctPos);
}


template<class TWnd, class TParam>
TWnd* CreateDocker(TParam* createParam, LPCTSTR title, CRect& rect, CMainFrame* owner, CPNDockingWindow** pArr, int index,
					bool bDock, dockwins::CDockingSide side = dockwins::CDockingSide::sLeft,
					int nBar = 0, float fPctPos = 0)
{
	TWnd* pWnd = new TWnd(createParam);
	return CreateDockerWindow(pWnd, title, rect, owner, pArr, index, bDock, side, nBar, fPctPos);
}

void CMainFrame::CreateDockingWindows()
{
	// Create docking windows...
	CRect rcLeft(0,0,180,400);
	CRect rcBottom(0,0,150,100);

	m_pFindResultsWnd = CreateDocker<CFindInFilesView>(LS(ID_VIEW_WINDOWS_FINDRESULTS), rcBottom, this,
		m_dockingWindows, ID_VIEW_WINDOWS_FINDRESULTS - ID_VIEW_FIRSTDOCKER,
		true, dockwins::CDockingSide::sBottom);

	m_pOutputWnd = CreateDocker<COutputView>(LS(ID_VIEW_OUTPUT), rcBottom, this, 
		m_dockingWindows, ID_VIEW_OUTPUT - ID_VIEW_FIRSTDOCKER,
		true, dockwins::CDockingSide::sBottom);

	m_pClipsWnd = CreateDocker<CClipsDocker, TextClips::TextClipsManager>(m_pTextClips, LS(ID_VIEW_WINDOWS_TEXTCLIPS), rcLeft, this,
		m_dockingWindows, ID_VIEW_WINDOWS_TEXTCLIPS - ID_VIEW_FIRSTDOCKER,
		true, dockwins::CDockingSide::sLeft);

	m_pProjectsWnd = CreateDocker<CProjectDocker>(LS(ID_VIEW_WINDOWS_PROJECT), rcLeft, this, 
		m_dockingWindows, ID_VIEW_WINDOWS_PROJECT - ID_VIEW_FIRSTDOCKER,
		true, dockwins::CDockingSide::sLeft);

	m_pCtagsWnd = CreateDocker<CJumpDocker>(LS(ID_VIEW_WINDOWS_CTAGS), rcLeft, this, 
		m_dockingWindows, ID_VIEW_WINDOWS_CTAGS - ID_VIEW_FIRSTDOCKER,
		true, dockwins::CDockingSide::sLeft);
	
	hCTagsWnd = m_pCtagsWnd->getHandle();

	m_pScriptsWnd = CreateDocker<CScriptDocker>(LS(ID_VIEW_WINDOWS_SCRIPTS), rcLeft, this, 
		m_dockingWindows, ID_VIEW_WINDOWS_SCRIPTS - ID_VIEW_FIRSTDOCKER,
		true, dockwins::CDockingSide::sRight);

	getDocker(DW_FINDRESULTS)->DockTo( getDocker(DW_OUTPUT)->m_hWnd, 0 );
	getDocker(DW_CTAGS)->DockTo( getDocker(DW_PROJECTS)->m_hWnd, 0 );
	getDocker(DW_TEXTCLIPS)->DockTo( getDocker(DW_PROJECTS)->m_hWnd, 0 );

	// Register icons for menu niceness...
	m_CmdBar.AddIcon(getDocker(DW_FINDRESULTS)->GetIcon(FALSE), ID_VIEW_WINDOWS_FINDRESULTS);
	m_CmdBar.AddIcon(getDocker(DW_PROJECTS)->GetIcon(FALSE), ID_VIEW_WINDOWS_PROJECT);
	m_CmdBar.AddIcon(getDocker(DW_SCRIPTS)->GetIcon(FALSE), ID_VIEW_WINDOWS_SCRIPTS);
	m_CmdBar.AddIcon(getDocker(DW_TEXTCLIPS)->GetIcon(FALSE), ID_VIEW_WINDOWS_TEXTCLIPS);
	m_CmdBar.AddIcon(getDocker(DW_OUTPUT)->GetIcon(FALSE), ID_VIEW_OUTPUT);
}

/**
 * This function is mostly identical to the one in WTL but allows the optional
 * use of chevrons.
 */
static BOOL AddReBarBandCtrl(HWND hWndReBar, HWND hWndBand, int nID = 0, LPTSTR lpstrTitle = NULL, BOOL bNewRow = FALSE, int cxWidth = 0, BOOL bFullWidthAlways = FALSE, bool bUseChevrons = false)
{
	ATLASSERT(::IsWindow(hWndReBar));	// must be already created
#ifdef _DEBUG
	// block - check if this is really a rebar
	{
		TCHAR lpszClassName[sizeof(REBARCLASSNAME)];
		::GetClassName(hWndReBar, lpszClassName, sizeof(REBARCLASSNAME));
		ATLASSERT(lstrcmp(lpszClassName, REBARCLASSNAME) == 0);
	}
#endif //_DEBUG
	ATLASSERT(::IsWindow(hWndBand));	// must be already created

	// Get number of buttons on the toolbar
	int nBtnCount = (int)::SendMessage(hWndBand, TB_BUTTONCOUNT, 0, 0L);

	// Set band info structure
	REBARBANDINFO rbBand;
	rbBand.cbSize = sizeof(REBARBANDINFO);
#if (_WIN32_IE >= 0x0400)
	rbBand.fMask = RBBIM_CHILD | RBBIM_CHILDSIZE | RBBIM_STYLE | RBBIM_ID | RBBIM_SIZE | RBBIM_IDEALSIZE;
#else
	rbBand.fMask = RBBIM_CHILD | RBBIM_CHILDSIZE | RBBIM_STYLE | RBBIM_ID | RBBIM_SIZE;
#endif //!(_WIN32_IE >= 0x0400)
	if(lpstrTitle != NULL)
		rbBand.fMask |= RBBIM_TEXT;
	rbBand.fStyle = RBBS_CHILDEDGE;
#if (_WIN32_IE >= 0x0500)
	if(nBtnCount > 0 && bUseChevrons)	// add chevron style for toolbar with buttons
		rbBand.fStyle |= RBBS_USECHEVRON;
#endif //(_WIN32_IE >= 0x0500)
	if(bNewRow)
		rbBand.fStyle |= RBBS_BREAK;

	rbBand.lpText = lpstrTitle;
	rbBand.hwndChild = hWndBand;
	if(nID == 0)	// calc band ID
		nID = ATL_IDW_BAND_FIRST + (int)::SendMessage(hWndReBar, RB_GETBANDCOUNT, 0, 0L);
	rbBand.wID = nID;

	// Calculate the size of the band
	BOOL bRet;
	RECT rcTmp;
	if(nBtnCount > 0)
	{
		bRet = (BOOL)::SendMessage(hWndBand, TB_GETITEMRECT, nBtnCount - 1, (LPARAM)&rcTmp);
		ATLASSERT(bRet);
		rbBand.cx = (cxWidth != 0) ? cxWidth : rcTmp.right;
		rbBand.cyMinChild = rcTmp.bottom - rcTmp.top;
		if(bFullWidthAlways)
		{
			rbBand.cxMinChild = rbBand.cx;
		}
		else if(lpstrTitle == 0)
		{
			bRet = (BOOL)::SendMessage(hWndBand, TB_GETITEMRECT, 0, (LPARAM)&rcTmp);
			ATLASSERT(bRet);
			rbBand.cxMinChild = rcTmp.right;
		}
		else
		{
			rbBand.cxMinChild = 0;
		}
	}
	else	// no buttons, either not a toolbar or really has no buttons
	{
		bRet = ::GetWindowRect(hWndBand, &rcTmp);
		ATLASSERT(bRet);
		rbBand.cx = (cxWidth != 0) ? cxWidth : (rcTmp.right - rcTmp.left);
		rbBand.cxMinChild = bFullWidthAlways ? rbBand.cx : 0;
		rbBand.cyMinChild = rcTmp.bottom - rcTmp.top;
	}

#if (_WIN32_IE >= 0x0400)
	rbBand.cxIdeal = rbBand.cx;
#endif //(_WIN32_IE >= 0x0400)

	// Add the band
	LRESULT lRes = ::SendMessage(hWndReBar, RB_INSERTBAND, (WPARAM)-1, (LPARAM)&rbBand);
	if(lRes == 0)
	{
		ATLTRACE2(atlTraceUI, 0, _T("Failed to add a band to the rebar.\n"));
		return FALSE;
	}

#if (_WIN32_IE >= 0x0501)
	if(bUseChevrons)
	{
		DWORD dwExStyle = (DWORD)::SendMessage(hWndBand, TB_GETEXTENDEDSTYLE, 0, 0L);
		::SendMessage(hWndBand, TB_SETEXTENDEDSTYLE, 0, dwExStyle | TBSTYLE_EX_HIDECLIPPEDBUTTONS);
	}
#endif //(_WIN32_IE >= 0x0501)

	return TRUE;
}

BOOL CMainFrame::AddReBarBand(HWND hWndBand, LPTSTR lpstrTitle, BOOL bNewRow, bool bUseChevrons, int cxWidth, BOOL bFullWidthAlways)
{
	ATLASSERT(::IsWindow(m_hWndToolBar));	// must be an existing rebar
	ATLASSERT(::IsWindow(hWndBand));	// must be created
	return AddReBarBandCtrl(m_hWndToolBar, hWndBand, 0, lpstrTitle, bNewRow, cxWidth, bFullWidthAlways, bUseChevrons);
}

#define PN_REBAR_STYLE \
	(WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | RBS_VARHEIGHT | RBS_BANDBORDERS | RBS_AUTOSIZE | CCS_NODIVIDER | RBS_DBLCLKTOGGLE)

void CMainFrame::loadImages(USHORT id, HIMAGELIST* images, USHORT disId, HIMAGELIST* disImages)
{
	CImageList il;
	CBitmap bmp;
	il.Create(16, 16, ILC_COLOR32 | ILC_MASK, 3, 1);
	bmp.LoadBitmap(id);
	il.Add(bmp, RGB(255, 0, 255));
	*images = il.Detach();
	m_CmdBar.LoadImages(id);

	if(disId != 0)
	{
        CBitmap bmpd;
		il.Create(16, 16, ILC_COLOR32 | ILC_MASK, 3, 1);
		bmpd.LoadBitmap(disId);
		il.Add(bmpd, RGB(255, 0, 255));
		*disImages = il.Detach();
		m_CmdBar.LoadDisabledImages(id, disId, bmpd);
	}
}

HWND CMainFrame::CreateEx(HWND hWndParent, ATL::_U_RECT rect, DWORD dwStyle, DWORD dwExStyle, LPVOID lpCreateParam)
{
	const int cchName = 256;
	TCHAR szWindowName[cchName];
	szWindowName[0] = 0;

	::LoadString(ATL::_AtlBaseModule.GetResourceInstance(), GetWndClassInfo().m_uCommonResourceID, szWindowName, cchName);
	HMENU hMenu = ::LoadMenu(ATL::_AtlBaseModule.GetResourceInstance(), MAKEINTRESOURCE(GetWndClassInfo().m_uCommonResourceID));

	HWND hWnd = Create(hWndParent, rect, szWindowName, dwStyle, dwExStyle, hMenu, lpCreateParam);

	//if(hWnd != NULL)
	//	m_hAccel = ::LoadAccelerators(ATL::_AtlBaseModule.GetResourceInstance(), MAKEINTRESOURCE(T::GetWndClassInfo().m_uCommonResourceID));
	// Load our own accelerator table...
	setupAccelerators(m_hMenu);

	return hWnd;
}

LRESULT CMainFrame::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	//////////////////////////////////////////////////////////////
	// Command Bar and Toolbars:

	// create command bar window
	HWND hWndCmdBar = m_CmdBar.Create(m_hWnd, rcDefault, NULL, ATL_SIMPLE_CMDBAR_PANE_STYLE);
	// attach menu
	m_CmdBar.AttachMenu(GetMenu());
	
	// remove old menu
	SetMenu(NULL);

	HWND hWndToolBar = CreateSimpleToolBarCtrl(m_hWnd, IDR_MAINFRAME, FALSE, ATL_SIMPLE_TOOLBAR_PANE_STYLE);
	HWND hWndEdtToolBar = CreateSimpleToolBarCtrl(m_hWnd, IDR_TBR_EDIT, FALSE, ATL_SIMPLE_TOOLBAR_PANE_STYLE);
	HWND hWndSchemeToolBar = CreateSchemeToolbar();
	HWND hWndFindToolBar = CreateFindToolbar();

	// Sort out toolbar images etc...
	if(m_bIsXPOrLater && !OPTIONS->Get(PNSK_INTERFACE, "LowColourToolbars", false))
	{
		// XP toolbars...
		m_CmdBar.LoadImages(IDR_MAINFRAME);
 		m_CmdBar.LoadImages(IDR_TBR_EDIT);
		m_CmdBar.LoadImages(IDR_TBR_FIND);
	}
	else
	{
		// Allow us to add images with a pink mask...
		COLORREF clrOld = m_CmdBar.m_clrMask;
		m_CmdBar.m_clrMask = RGB(255,0,255);

		// 24-bit tb images, this needs re-instating for Non-XP display.
		loadImages(IDB_TBMAIN24, &m_hILMain, IDB_TBMAINDIS, &m_hILMainD);
		loadImages(IDB_TBEDIT24, &m_hILEdit);
		loadImages(IDB_TBFIND24, &m_hILFind);

		// Again, this code is used for Non-XP images.
		::SendMessage(hWndToolBar, TB_SETIMAGELIST, 0, (LPARAM)m_hILMain);
		::SendMessage(hWndToolBar, TB_SETDISABLEDIMAGELIST, 0, (LPARAM)m_hILMainD);
		::SendMessage(hWndEdtToolBar, TB_SETIMAGELIST, 0, (LPARAM)m_hILEdit);
		::SendMessage(hWndFindToolBar, TB_SETIMAGELIST, 0, (LPARAM)m_hILFind);

		// Set the mask back...
		m_CmdBar.m_clrMask = clrOld;
	}

	// Additional Images:
	{
		COLORREF clrOld = m_CmdBar.m_clrMask;
		m_CmdBar.m_clrMask = RGB(255,0,255);
		
		// Projects menu
		m_CmdBar.LoadImages(IDR_TBR_PROJECTS);
		
		m_CmdBar.m_clrMask = clrOld;
	}

	// Contentious feature time - hidden option to hide SaveAll until 
	// we have toolbar customisation.
	if(OPTIONS->Get(PNSK_INTERFACE, "HideSaveAll", false))
	{
		::SendMessage(hWndToolBar, TB_DELETEBUTTON, 4, 0);
	}

	CreateSimpleReBar(PN_REBAR_STYLE);
	AddReBarBand(hWndCmdBar, NULL, FALSE, true);
	AddReBarBand(hWndToolBar, NULL, TRUE, true);
	AddReBarBand(hWndEdtToolBar, NULL, FALSE, true);
	AddReBarBand(hWndSchemeToolBar, NULL, FALSE, true);
	AddReBarBand(hWndFindToolBar, NULL, FALSE, true);
	SizeSimpleReBarBands();

	// CmdUI
	UIAddToolBar(hWndToolBar);
	UIAddToolBar(hWndEdtToolBar);
	UIAddToolBar(hWndSchemeToolBar);
	UISetCheck(ID_VIEW_TOOLBAR, 1);
	UISetCheck(ID_VIEW_TOOLBAR_EDIT, 1);
	UISetCheck(ID_VIEW_TOOLBARS_FIND, 1);
	UISetCheck(ID_VIEW_TOOLBARS_SCHEMES, 1);
	
	//////////////////////////////////////////////////////////////
	// Status Bar:

	CreateSimpleStatusBar(_T(""));

	int statusBarPanes[] =
	{
		ID_POS_PANE,
		ID_MOD_PANE,
		ID_ENC_PANE,
		ID_LINEENDS_PANE,
		ID_INS_PANE,
		ID_DEFAULT_PANE
	};

	m_StatusBar.SubclassWindow(m_hWndStatusBar);
	m_StatusBar.SetPanes(statusBarPanes, sizeof(statusBarPanes) / sizeof(int), false);
	m_StatusBar.SetPaneWidth(ID_POS_PANE, 120);
	m_StatusBar.SetPaneWidth(ID_MOD_PANE, 70);
	m_StatusBar.SetPaneWidth(ID_ENC_PANE, 70);
	m_StatusBar.SetPaneWidth(ID_LINEENDS_PANE, 50);
	m_StatusBar.SetPaneWidth(ID_INS_PANE, 30);

	bool bStatusBar = OPTIONS->Get(PNSK_INTERFACE, _T("StatusBarVisible"), true);
	UISetCheck(ID_VIEW_STATUS_BAR, bStatusBar);

	if(!bStatusBar)
	{
		m_StatusBar.ShowWindow(SW_HIDE);
		UpdateLayout();
	}
	
	m_bShowingDefaultStatus = false;
	SetStatusText(NULL);

	//////////////////////////////////////////////////////////////
	// Tabs:

	if(!OPTIONS->Get(PNSK_INTERFACE, _T("Tabs"), true))
	{
		m_tabbedClient.GetTabOwner().KeepTabsHidden(true);
	}
	else
	{
		if(OPTIONS->Get(PNSK_INTERFACE, _T("MaximizedTabsOnly"), false))
			m_tabbedClient.HideMDITabsWhenMDIChildNotMaximized(true);

		if(OPTIONS->Get(PNSK_INTERFACE, _T("TabsOnBottom"), false))
			m_tabbedClient.GetTabOwner().ModifyTabStyles(0, CTCS_BOTTOM);
	}

	//////////////////////////////////////////////////////////////
	// Misc:

	
	CreateMDIClient();
	m_CmdBar.SetMDIClient(m_hWndMDIClient);

	InitializeDockingFrame();
	
	// register object for message filtering and idle updates
	CMessageLoop* pLoop = _Module.GetMessageLoop();
	ATLASSERT(pLoop != NULL);
	pLoop->AddMessageFilter(this);
	pLoop->AddIdleHandler(this);

	// Initialise our popup menus.
	m_Switcher.Reset(m_pCmdDispatch, MENUMESSAGE_CHANGESCHEME);
	SchemeManager::GetInstance()->BuildMenu((HMENU)m_NewMenu, m_pCmdDispatch, this);

	tstring mrukey;
	mrukey = PNSK_MRU;
	m_RecentFiles.SetSize(OPTIONS->Get(PNSK_INTERFACE, _T("MRUSize"), 4));
	m_RecentFiles.Load(OPTIONS, mrukey.c_str());
	m_RecentFiles.UpdateMenu();

	mrukey = PNSK_MRUP;
	m_RecentProjects.SetSize(OPTIONS->Get(PNSK_INTERFACE, _T("ProjectMRUSize"), 4));
	m_RecentProjects.Load(OPTIONS, mrukey.c_str());
	m_RecentProjects.UpdateMenu();
	
	AddMRUMenu(CSMenuHandle(m_hMenu));
	AddNewMenu(CSMenuHandle(m_hMenu));

	CreateDockingWindows();
	InitGUIState();

	setupToolsUI();

	// Load extensions...
	g_Context.ExtApp->LoadExtensions();

	DragAcceptFiles(TRUE);

	PostMessage(PN_INITIALISEFRAME);

	// simulate a crash, see if we get a minidump:
	//int a = 5-3;
	//a-=2;
	//int b = 47/a;
	//LOG((LPCTSTR)b);
	//////////////////////////////////////////////

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
		
		if( !CheckAlreadyOpen(buf, (EAlreadyOpenAction)OPTIONS->GetCached(Options::OAlreadyOpenDropAction)) )
		{
			OpenFile(buf);
			AddMRUEntry(buf);
		}
	}

	DragFinish(hDrop);
	
	return 0;
}

LRESULT CMainFrame::OnDblClick(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	BOOL b = TRUE;

	switch( OPTIONS->Get(PNSK_INTERFACE, _T("MDIDoubleClickAction"), 0) )
	{
		case 0:
			OnFileOpen(0, 0, 0, b);
			break;
		case 1:
			OnFileNew(0, 0, 0, b);
			break;
		default:
			LOG(_T("PN2: Unknown MDI Double-Click Action Code"));
	}
	return 0;
}

LRESULT CMainFrame::OnEscapePressed(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	if( getDocker(DW_OUTPUT)->IsWindowVisible() )
	{
		getDocker(DW_OUTPUT)->Hide();

		return TRUE;
	}

	if( getDocker(DW_FINDRESULTS)->IsWindowVisible() )
	{
		getDocker(DW_FINDRESULTS)->Hide();

		return TRUE;
	}

	return FALSE;
}

void CMainFrame::handleCommandLine(std::list<tstring>& parameters)
{
	bool bHaveLine = false;
	bool bHaveCol = false;
	bool bHaveScheme = false;
	int iLine = 0, iCol = 0;
	Scheme* pScheme = NULL;
	
	if(parameters.size() < 1)
		return;

	// Store the last position in the list...
	std::list<tstring>::iterator iLast = parameters.end();
	iLast--;

	for(std::list<tstring>::iterator i = parameters.begin();
		i != parameters.end();
		++i)
	{
		if((*i).length() < 1)
			continue;

		LPCTSTR parm = (*i).c_str();//__argv[i];

		if((parm[0] == _T('/') || parm[0] == _T('-')) && (parm[1] != NULL))
		{
			// special params, check there's another parameter for these ones...
			if(i != iLast)
			{
				std::list<tstring>::iterator next = i;
				next++;
				LPCTSTR nextArg = (*next).c_str();
				bool skip(true);

				if(_tcsicmp(&parm[1], _T("l")) == 0 || _tcsicmp(&parm[1], _T("-line")) == 0)
				{
					iLine = _ttol(nextArg);
					bHaveLine = true;
				}
				else if(_tcsicmp(&parm[1], _T("c")) == 0 || _tcsicmp(&parm[1], _T("-col")) == 0)
				{
					iCol = _ttol(nextArg);
					bHaveCol = true;
				}
				else if(_tcsicmp(&parm[1], _T("s")) == 0 || _tcsicmp(&parm[1], _T("-scheme")) == 0)
				{
					SchemeManager* pSM = SchemeManager::GetInstance();
					pScheme = pSM->SchemeByName(nextArg);
					if(pScheme != NULL && pScheme != pSM->GetDefaultScheme())
					{
						bHaveScheme = true;
					}
					else
						pScheme = NULL;
				}
				else
				{
					skip = false;
				}
				
				if(skip)
				{
					i++;
					continue;
				}
			}
		}
		else
		{
			if(!CheckAlreadyOpen( parm ))
			{
				openFileCheckType(parm);
			}
			
			CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor());
			if(pChild)
			{
				CTextView* pTV = pChild->GetTextView();
				if(bHaveLine)
				{
					pTV->GotoLine(iLine-1);
				}
				if(bHaveCol)
				{
					long pos = pTV->GetCurrentPos();
					pTV->GotoPos(pos + iCol);
				}
				if(bHaveScheme && pScheme)
				{
					pChild->SetScheme( pScheme );
				}
			}
		}

		bHaveCol = bHaveLine = bHaveScheme = false;
	}
}

LRESULT CMainFrame::OnInitialiseFrame(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	LoadGUIState();

	bool bHaveLine = false;
	bool bHaveCol = false;
	bool bHaveScheme = false;
	int iLine = 0, iCol = 0;
	Scheme* pScheme = NULL;

	handleCommandLine(*m_cmdLineArgs);

	if(OPTIONS->Get(PNSK_INTERFACE, _T("CheckAssocsOnStartup"), false))
	{
		FileAssocManager fam;
		if(fam.CheckAssociations())
		{
			fam.UpdateAssociations();
		}
	}

	if(OPTIONS->Get(PNSK_INTERFACE, _T("SaveWorkspace"), false) && m_cmdLineArgs->size() == 0)
	{
		WorkspaceState wss;
		wss.Load();
	}

	HWND hWndEditor = GetCurrentEditor();
	if(hWndEditor == NULL)
	{
		if(OPTIONS->Get(PNSK_INTERFACE, _T("NewOnStart"), true))
		{
			// Bad simon, bad.
			BOOL bHandled;
			OnFileNew(0, 0, 0, bHandled);
		}
	}

	delete m_cmdLineArgs;
	m_cmdLineArgs = NULL;

	return 0;
}

LRESULT CMainFrame::OnMenuSelect(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = false;

	WORD wFlags = HIWORD(wParam);
	if(!(wFlags == 0xFFFF && lParam == NULL))	// menu closing
	{
		if(!(wFlags & MF_POPUP))
		{
			WORD wID = LOWORD(wParam);
			if(wID >= m_RecentFiles.base() && wID <= m_RecentFiles.last() && m_RecentFiles.GetCount() > 0)
			{
				LPCTSTR fn = m_RecentFiles.GetEntry(wID - m_RecentFiles.base());
				::SendMessage(m_hWndStatusBar, SB_SIMPLE, TRUE, 0L);
				::SendMessage(m_hWndStatusBar, SB_SETTEXT, (255 | SBT_NOBORDERS), (LPARAM)fn);
				//::OutputDebugString(fn);
				bHandled = true;
			}
			else if(wID >= m_RecentProjects.base() && wID <= m_RecentProjects.last() && m_RecentProjects.GetCount() > 0)
			{
				LPCTSTR fn = m_RecentProjects.GetEntry(wID - m_RecentProjects.base());
				::SendMessage(m_hWndStatusBar, SB_SIMPLE, TRUE, 0L);
				::SendMessage(m_hWndStatusBar, SB_SETTEXT, (255 | SBT_NOBORDERS), (LPARAM)fn);
				//::OutputDebugString(fn);
				bHandled = true;
			}
		}
	}

	return 1;
}

LRESULT CMainFrame::OnMultiInstanceMsg(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	if(wParam == MultipleInstanceManager::MIM_PARAMETER_ARRAY)
	{
		std::list<tstring> parameters;
		g_Context.m_miManager->GetParameters(parameters, lParam);

		handleCommandLine(parameters);
	}

	if(IsIconic())
		ShowWindow(SW_RESTORE);
	::SetForegroundWindow(m_hWnd);

	return 0;
}

/**
 * This is the handler for the tabbing framework's modified file saving
 * stuff. This handler deals with the project group and any modified projects.
 */
LRESULT CMainFrame::OnSaveModifiedItem(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
{
	ITabbedMDIChildModifiedItem* pMI = reinterpret_cast<ITabbedMDIChildModifiedItem*>( lParam );

	CComPtr<IUnknown> pi;
	CComPtr<IProjectHolder> ph;

	// See if we have any user data in this item. If we do, then
	// it's probably a project group. This is held in an IProjectHolder.
	pMI->get_UserData(&pi);
	if(pi)
		pi->QueryInterface(&ph);

	if(ph)
	{
		// It's gonna be a project group.
		Projects::Workspace* projectGroup;
		ph->get_ProjectGroup(&projectGroup);
		if(projectGroup != NULL)
		{
			if(SaveWorkspace(projectGroup) == IDCANCEL)
				return 1;
		}
	}
	else
	{
		// No project group, we see if there are any sub items of
		// the item - if so they're projects.
		CComPtr<ITabbedMDIChildModifiedList> subItems;
		pMI->get_SubItems(&subItems);

		long count;
		subItems->get_Count(&count);
		
		if(count == 0)
		{
			// nope, carry on.
			bHandled = FALSE;
			return 0;
		}

		// yep, let's iterate 'em and save 'em.
		for(long i = 0; i < count; i++)
		{
			// Reset pointers
			pi.Release();
			ph.Release();

			CComPtr<ITabbedMDIChildModifiedItem> mi;
			if(FAILED(subItems->get_Item(i, &mi)))
				RETURN_UNEXPECTED( _T("A project modified item did not contain an IProjectHolder instance."), 1);

			mi->get_UserData(&pi);
			if(pi)
				pi->QueryInterface(&ph);
			else
				continue;

			if(ph)
			{	
				Projects::Project* project;
				ph->get_Project(&project);
				if(project != NULL)
				{
					project->Save();
				}
			}
		}
	}

	return 0;
}

LRESULT CMainFrame::OnUpdateFindText(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	extensions::ISearchOptions* so = OPTIONS->GetSearchOptions();
	m_FindCombo.SetWindowText(so->GetFindText());
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
	tstring newscheme =	OPTIONS->Get(PNSK_EDITOR, _T("NewScheme"), _T(""));
	if(newscheme.length() > 0)
        pChild->SetScheme(SchemeManager::GetInstance()->SchemeByName(newscheme.c_str()));
	else
		pChild->SetScheme(SchemeManager::GetInstance()->GetDefaultScheme());
		
	
	return 0;
}

LRESULT CMainFrame::OnFileNewProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pProjectsWnd == NULL)
		RETURN_UNEXPECTED(_T("No Projects Window."), 0); // bail.

	CNewProjectDialog dlg;
	
	if( !(dlg.DoModal() == IDOK) )
		return 0; // bail.

	tstring folder = dlg.GetFolder();
	
	CFileName fn(dlg.GetName());
	fn.ChangeExtensionTo(".pnproj");
	fn.Root(folder.c_str());
	
	NewProject(fn.c_str(), dlg.GetName(), dlg.GetTemplateGUID());

	return 0;
}

LRESULT CMainFrame::OnFileNewWorkspace(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pProjectsWnd == NULL)
		RETURN_UNEXPECTED(_T("No Projects Window."), 0);

	if(!CloseWorkspace())
		return 0;

	Projects::Workspace* workspace = new Projects::Workspace;
	workspace->SetName(LS(IDS_NEWPROJECTGROUP));
	m_pProjectsWnd->SetWorkspace(workspace);
	workspace->ClearDirty();

	return 0;
}

LRESULT CMainFrame::OnFileOpen(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CPNOpenDialogEx dlgOpen(LS(IDS_ALLFILES));
	dlgOpen.m_ofn.Flags |= OFN_ALLOWMULTISELECT;

	tstring path;

	if(OPTIONS->Get(PNSK_INTERFACE, _T("OpenInCurrentDir"), true))
	{
		CChildFrame* pChild = CChildFrame::FromHandle( GetCurrentEditor() );
		if(pChild != NULL)
		{
			path = pChild->GetFileName( FN_PATH );
			dlgOpen.m_ofn.lpstrInitialDir = path.c_str();
		}
	}

	if (dlgOpen.DoModal() == IDOK)
	{
		EAlreadyOpenAction action = (EAlreadyOpenAction)OPTIONS->GetCached(Options::OAlreadyOpenAction);
		for(CPNOpenDialog::const_iterator i = dlgOpen.begin(); i != dlgOpen.end(); ++i)
		{
			if( !CheckAlreadyOpen((*i).c_str(), action) )
			{
				if(OpenFile((*i).c_str(), NULL, dlgOpen.GetEncoding()))
				{
					AddMRUEntry((*i).c_str());
				}
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

	if(!::FileExists( filename ))
	{
		tstring err = LS(IDS_CANTOPENFILE);
		int bs = err.length() + _tcslen(filename) + 10;
		TCHAR* buffer = new TCHAR[bs];
		_sntprintf(buffer, bs, err.c_str(), filename);
		::MessageBox(m_hWnd, buffer, LS(IDR_MAINFRAME), MB_ICONWARNING | MB_OK);
		delete [] buffer;

		m_RecentFiles.RemoveEntry(wID - ID_MRUFILE_BASE);
		return 0;
	}

	if( OpenFile(filename) )
	{
		m_RecentFiles.MoveToTop(wID - ID_MRUFILE_BASE);
	}

	return 0;
}

LRESULT CMainFrame::OnDockerToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	PNASSERT(wID >= ID_VIEW_FIRSTDOCKER && wID <= ID_VIEW_LASTDOCKER);
	m_dockingWindows[wID - ID_VIEW_FIRSTDOCKER]->Toggle();

	return 0;
}

LRESULT CMainFrame::OnMRUProjectSelected(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	LPCTSTR filename = m_RecentProjects.GetEntry(wID - ID_MRUPROJECT_BASE);

	CFileName fn(filename);
	fn.ToLower();
	if( fn.GetExtension() == _T(".pnproj") )
	{
		OpenProject(fn.c_str());
	}
	else if( fn.GetExtension() == _T(".ppg") )
	{
		OpenWorkspace(fn.c_str());
	}

	return 0;
}

LRESULT CMainFrame::OnFileOpenProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CPNOpenDialog dlgOpen(LS(IDS_ALLPROJECTFILES));
	
	tstring s = StringLoader::Get(IDS_OPENPROJECTDLGTITLE);
	dlgOpen.m_ofn.lpstrTitle = s.c_str();

	if(dlgOpen.DoModal() == IDOK)
	{
		CFileName fn(dlgOpen.GetSingleFileName());
		fn.ToLower();
		if( fn.GetExtension() == _T(".pnproj") )
		{
			OpenProject(fn.c_str());
		}
		else if( fn.GetExtension() == _T(".ppg") )
		{
			OpenWorkspace(fn.c_str());
		}
	}

	return 0;
}

LRESULT CMainFrame::OnFileCloseWorkspace(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CloseWorkspace(true);

	return 0;
}

LRESULT CMainFrame::OnFileCloseAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CloseAll();

	return 0;
}

LRESULT CMainFrame::OnFileSaveWorkspaceState(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CPNSaveDialog dlgSave(LS(IDS_WORKSPACEFILES), NULL, _T("pnws"));

	if(dlgSave.DoModal() == IDOK)
	{
		WorkspaceState wss;
		wss.Save(dlgSave.m_ofn.lpstrFile);
	}

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

LRESULT CMainFrame::OnQuickFind(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_tabbedClient.ShowFindBar(true);
	return TRUE;
}

LRESULT CMainFrame::OnViewToolBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	ToggleToolbar(TBR_FILE);
	return 0;
}

LRESULT CMainFrame::OnViewEditBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	ToggleToolbar(TBR_EDIT);
	return 0;
}

LRESULT CMainFrame::OnViewSchemesBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	ToggleToolbar(TBR_SCHEME);	
	return 0;
}

LRESULT CMainFrame::OnViewFindBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	ToggleToolbar(TBR_FIND);
	m_tabbedClient.ShowFindBar(true);
	return 0;
}

LRESULT CMainFrame::OnViewStatusBar(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	bool bVisible = !::IsWindowVisible(m_hWndStatusBar);
	::ShowWindow(m_hWndStatusBar, bVisible ? SW_SHOWNOACTIVATE : SW_HIDE);
	OPTIONS->Set(PNSK_INTERFACE, _T("StatusBarVisible"), bVisible);
	UISetCheck(ID_VIEW_STATUS_BAR, bVisible);
	UpdateLayout();
	return 0;
}

LRESULT CMainFrame::OnHideOutput(WORD /*wNotifyCode*/, WORD /*wID*/, HWND hWndCtl, BOOL& /*bHandled*/)
{
	if(hWndCtl == m_pOutputWnd->m_hWnd)
	{
		getDocker(DW_OUTPUT)->Hide();
	}
	else if(hWndCtl == m_pFindResultsWnd->m_hWnd)
	{
		getDocker(DW_FINDRESULTS)->Hide();
	}
	else
	{
		::SendMessage(::GetParent(hWndCtl), WM_COMMAND, ID_OUTPUT_HIDE, (LPARAM)hWndCtl);
	}
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

LRESULT CMainFrame::OnWindowTileVert(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	MDITile(MDITILE_VERTICAL);
	return 0;
}

LRESULT CMainFrame::OnWindowArrangeIcons(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	MDIIconArrange();
	return 0;
}

LRESULT CMainFrame::OnWindowCloseAllOther(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	HWND hWndCurChild = GetCurrentEditor();
	if(!hWndCurChild)
		return 0;

	HWND hWndChild = ::GetTopWindow(m_tabbedClient.m_hWnd);
	while(hWndChild != NULL)
	{
		HWND hWndClose = hWndChild;
		hWndChild = ::GetNextWindow(hWndChild, GW_HWNDNEXT);

		if(hWndClose != hWndCurChild && ::IsWindow(hWndClose))
		{
			// This is not the current window, so send a 
			// close message it should understand.
			::SendMessage(hWndClose, WM_SYSCOMMAND, SC_CLOSE, 0L);
		}
	}

	return 0;
}

LRESULT CMainFrame::OnFind(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	launchFind(eftFind);

	return 0;
}

LRESULT CMainFrame::OnReplace(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	launchFind(eftReplace);

	return 0;
}

LRESULT CMainFrame::OnFindInFiles(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	launchFind(eftFindInFiles);
	
	return 0;
}

/**
 * Show the options dialog
 */
LRESULT CMainFrame::OnOptions(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	SchemeManager* pSM = SchemeManager::GetInstance();
	LPCTSTR currentScheme = NULL;

	CChildFrame* pFrame = CChildFrame::FromHandle(GetCurrentEditor());
	if(pFrame)
	{
		currentScheme = pFrame->GetTextView()->GetCurrentScheme()->GetName();
	}

	SchemeConfigParser		schemeconfig(currentScheme);
	ToolsManager			toolsmanager;
	
	// The pages:
	COptionsPageGeneral			general;
	COptionsPageEditDefaults	editDefs;
	COptionsPageVisual			visual;
	COptionsPageConf			confirmations;
	COptionsPageDialogs			dialogs;
	COptionsPageKeyboard		pageKeyboard(m_pCmdDispatch);
	COptionsPageAutocomplete	pageAutocomplete;
	COptionsPageStyle			pageStyle(&schemeconfig);
	COptionsPageGlobalStyles	pageGlobalStyles(&schemeconfig);
	COptionsPageSchemes			pageSchemes(&schemeconfig);
	COptionsPageNewFiles		pageNewFiles(&schemeconfig);
	COptionsPageTools			pageTools(&schemeconfig, &toolsmanager);
	COptionsPageProjectTools	pageProjectTools(&toolsmanager);
	COptionsPageClips			pageClips(&schemeconfig, m_pTextClips);

	COptionsPageFileTypes		pageFiles(&schemeconfig);
	COptionsPageAFiles			pageAFiles;
	COptionsPageFileAssoc		pageFileAssoc;

	COptionsPageExtensions		pageExtensions;

	// Lots of pages will use the same schemeconfig:
	schemeconfig.LoadConfig(pSM->GetPath(), pSM->GetCompiledPath());

	// The dialog, add the pages:
	COptionsDialog options;
	options.AddPage(&general);
	options.AddPage(&editDefs);
	options.AddPage(&visual);
	options.AddPage(&confirmations);
	options.AddPage(&dialogs);
	options.AddPage(&pageStyle);
	options.AddPage(&pageGlobalStyles);
	options.AddPage(&pageSchemes);
	options.AddPage(&pageFiles);
	options.AddPage(&pageNewFiles);
	options.AddPage(&pageFileAssoc);
	options.AddPage(&pageTools);
	options.AddPage(&pageProjectTools);
	options.AddPage(&pageAFiles);
	options.AddPage(&pageKeyboard);
	options.AddPage(&pageAutocomplete);
	options.AddPage(&pageClips);
	options.AddPage(&pageExtensions);

	// If we were launched from the "Add Tools" menu item, 
	// start on the tools page:
	if( wID != ID_TOOLS_DUMMY )
		options.SetInitialPage(&general);
	else
		options.SetInitialPage(&pageTools);

	// Show the dialog:
	if( options.DoModal() == IDOK )
	{
		DWORD dwTimeNow = ::GetTickCount();

		// Save the modified tool sets...
		toolsmanager.Save();

		// and then re-load them into the main manager...
		ToolsManager* pSTM = ToolsManager::GetInstance();
		pSTM->ReLoad(m_pCmdDispatch); // pass in true to cache menu resources.

#if _DEBUG
		dwTimeNow = ::GetTickCount() - dwTimeNow;
		char buf[55];
		sprintf(buf, "Took %dms to save tools\n", dwTimeNow);
		LOG(buf);
#endif

		if( pageStyle.IsDirty() || pageSchemes.IsDirty() || pageGlobalStyles.IsDirty() )
		{
			CFileName userSettingsPath(_T("UserSettings.xml"));
			userSettingsPath.Root(pSM->GetCompiledPath());

			// Save user settings...
			schemeconfig.SaveConfig(userSettingsPath.c_str());
            
			// Compile those schemes!
			SchemeManager::GetInstance()->Compile();
		}

		m_RecentFiles.SetSize( OPTIONS->Get(PNSK_INTERFACE, _T("MRUSize"), 4) );
		m_RecentFiles.UpdateMenu();

		m_RecentProjects.SetSize( OPTIONS->Get(PNSK_INTERFACE, _T("ProjectMRUSize"), 4) );
		m_RecentProjects.UpdateMenu();

		if(pageKeyboard.IsDirty())
		{			
			tstring kmfile;
			OPTIONS->GetPNPath(kmfile, PNPATH_USERSETTINGS);
			kmfile += _T("keymap.dat");
			m_pCmdDispatch->Save(kmfile.c_str());
			
			setupAccelerators(m_hMenu);
		}

		// If text clips page has changes, then update the text clips view
		// actually not necessary until we actually edit text clips and not just templates!
		if (pageClips.IsDirty())
		{
			m_pClipsWnd->Reset();
		}

		PerformChildEnum(&CMainFrame::ChildOptionsUpdateNotify);

		setupToolsUI();

		pSTM->GetGlobalTools()->AllocateMenuResources(m_pCmdDispatch);
		m_hGlobalToolAccel = pSTM->GetGlobalTools()->GetAcceleratorTable();
	}

	return 0;
}

LRESULT CMainFrame::OnStopTools(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& bHandled)
{
	// Always pass this message on to children.
	bHandled = FALSE;

	// Don't need to wait, we'll assume the user is still using the app.
	ToolOwner::GetInstance()->KillTools(false, this); 

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

LRESULT CMainFrame::OnWebSFRFE(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::ShellExecute(m_hWnd, _T("open"), _T("http://sourceforge.net/tracker/?func=add&group_id=45545&atid=443222"), NULL, NULL, SW_SHOW);

	return 0;
}

void CMainFrame::launchFind(EFindDialogType findType)
{
	HWND hWndCur = GetCurrentEditor();

	if(findType != eftFindInFiles && !hWndCur)
		return;

	if(m_pFindEx == NULL)
	{
		m_pFindEx = new CFindExDialog();
		hFindWnd = m_pFindEx->Create(m_hWnd);
	}

	CChildFrame* pChild = hWndCur ? CChildFrame::FromHandle(hWndCur) : NULL;
	if(pChild)
		m_pFindEx->Show(findType, pChild->GetTextView()->GetCurrentWord().c_str());
	else
		m_pFindEx->Show(findType);
}

void CMainFrame::launchExternalSearch(LPCTSTR searchString)
{
	CWindowText wt(m_FindCombo.m_hWnd);

	tstring s(searchString);

	// avoid empty search strings...
	if( ((LPCTSTR)wt) != NULL )
	{
		LPTSTR searchstr = new TCHAR[_tcslen((LPCTSTR)wt)+1];
		_tcscpy(searchstr, (LPCTSTR)wt);
		for(size_t i = 0; i < _tcslen(searchstr); i++)
		{
			if(searchstr[i] == _T(' '))
				searchstr[i] = _T('+');
		}

		s += (LPCTSTR)searchstr;
		delete [] searchstr;
	}

	::ShellExecute(m_hWnd, _T("open"), s.c_str(), NULL, NULL, SW_SHOW);
}

CPNDockingWindow* CMainFrame::getDocker(EDocker window) const
{
	return m_dockingWindows[(int)window - ID_VIEW_FIRSTDOCKER];
}

LRESULT CMainFrame::OnFindBarFind(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled)
{
	return OnFindComboEnter(wNotifyCode, wID, hWndCtl, bHandled);
}

LRESULT CMainFrame::OnSearchGoogle(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	launchExternalSearch(_T("http://www.google.com/search?q="));

	return 0;
}

LRESULT CMainFrame::OnSearchGoogleGroups(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	launchExternalSearch(_T("http://groups.google.com/groups?q="));

	return 0;
}

LRESULT CMainFrame::OnHelpContents(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	tstring path;
	OPTIONS->GetPNPath(path);
	CFileName fn("pn2.chm");
	fn.Root(path.c_str());
	path = fn.c_str();

	path += "::/htmlhelp/index.html";
	::HtmlHelp(m_hWnd,
         path.c_str(),
         HH_DISPLAY_TOC,
         NULL) ;

	return 0;
}

LRESULT CMainFrame::OnSchemeComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int iSel = m_SchemeCombo.GetCurSel();
	Scheme* pScheme = static_cast<Scheme*>( m_SchemeCombo.GetItemDataPtr( iSel ) );
	if( pScheme != NULL )
	{
		CChildFrame* pEditor = CChildFrame::FromHandle( GetCurrentEditor() );
		if( pEditor != NULL )
			pEditor->SetScheme( pScheme, false );
	}
	return 0;
}

LRESULT CMainFrame::OnFindComboEnter(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CWindowText wt(m_FindCombo.m_hWnd); //Get find text...

	CChildFrame* pEditor = CChildFrame::FromHandle( GetCurrentEditor() );
	if( pEditor != NULL && lstrlen((LPCTSTR)wt) > 0 )
	{
		SearchOptions* pFindOptions = reinterpret_cast<SearchOptions*>( OPTIONS->GetSearchOptions() );
		if(pFindOptions->FindText != (LPCTSTR)wt)
		{
			pFindOptions->Found = false;
			pFindOptions->FindText = (LPCTSTR)wt;
			m_FindCombo.AddString((LPCTSTR)wt);
		}
		pEditor->FindNext(pFindOptions);
	}
	
	return 0;
}

LRESULT CMainFrame::OnToolbarDropDown(WPARAM /*wParam*/, LPNMHDR lParam, BOOL& /*bHandled*/)
{
	#define lpnmTB ((LPNMTOOLBAR)lParam)
	
	switch(lParam->idFrom)
	{
		case TBR_FIND:
		{
			CSPopupMenu popup(IDR_POPUP_FINDBARDD);
			
			CPoint pt(lpnmTB->rcButton.left, lpnmTB->rcButton.bottom);
			::ClientToScreen(lParam->hwndFrom, &pt);

			TrackPopupMenu(popup, 0, pt.x, pt.y);
		};
		break;
	}

	#undef lpnmTB
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
	s.pFunction = &CMainFrame::FileOpenNotify;
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
					str.Format(IDS_OPENANOTHER, filename);
					DWORD dwRes = MessageBox(str, LS(IDR_MAINFRAME), MB_YESNOCANCEL);
					if( dwRes == IDYES )
					{
						// Just claim the file wasn't open...
						s.bFound = false;
					}
					else if(dwRes == IDNO )
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

/**
 * Workspace accessor for the entire system.
 */
Projects::Workspace* CMainFrame::GetActiveWorkspace()
{
	if(m_pProjectsWnd == NULL)
		RETURN_UNEXPECTED(_T("No Projects Window."), NULL); // bail.

	return m_pProjectsWnd->GetWorkspace();
}

void CMainFrame::FindInFiles(SearchOptions* options)
{
	FindInFiles::GetInstance()->Start(
		options->FindText,
		options->Path,
		options->FileExts,
		options->Recurse,
		options->MatchCase,
		options->IncludeHidden,
		m_pFindResultsWnd);
}

void CMainFrame::ToggleDockingWindow(EDockingWindow window, bool bSetValue, bool bShowing)
{
	ToggleDockingWindow((EDocker)(window + ID_VIEW_FIRSTDOCKER), bSetValue, bShowing);
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// Menu code...

void CMainFrame::AddNewMenu(CSMenuHandle& menu)
{
	PNASSERT(::IsMenu(m_NewMenu));
	
	CSMenuHandle file = menu.GetSubMenu(0);
	CString str;
	str.LoadString(IDS_NEW);
	::InsertMenu(file, 0, MF_BYPOSITION | MF_POPUP, (UINT)(HMENU)m_NewMenu, str);
}

void CMainFrame::AddMRUMenu(CSMenuHandle& menu)
{
	CString str;
	CSMenuHandle file(menu.GetSubMenu(0));
	
	PNASSERT(::IsMenu(m_RecentFiles));
	PNASSERT(::IsMenu(m_RecentProjects));

	str.LoadString(IDS_RECENTFILES);
	::InsertMenu(file.GetHandle(), ID_APP_EXIT, MF_BYCOMMAND | MF_POPUP, (UINT)(HMENU)m_RecentFiles, str);
	
	str.LoadString(IDS_RECENTPROJECTS);
	if(m_RecentProjects.GetCount() > 0)
		::InsertMenu(file.GetHandle(), ID_APP_EXIT, MF_BYCOMMAND | MF_POPUP, (UINT)(HMENU)m_RecentProjects, str);
	
	::InsertMenu(file.GetHandle(), ID_APP_EXIT, MF_BYCOMMAND | MF_SEPARATOR, 0, NULL);
}

void CMainFrame::AddLanguageMenu(CSMenuHandle& menu)
{
	CString str;
	CSMenuHandle view(menu.GetSubMenu(2));
	
	PNASSERT(::IsMenu(m_Switcher));
	str.LoadString(IDS_CHANGESCHEME);
	::ModifyMenu(view.GetHandle(), ID_VIEW_CHANGESCHEME, MF_BYCOMMAND | MF_POPUP, (UINT)(HMENU)m_Switcher, str);
}

void CMainFrame::MoveNewMenu(CSMenuHandle& remove, CSMenuHandle& add)
{
	CSMenuHandle file( remove.GetSubMenu(0) );
	::RemoveMenu(file, 0, MF_BYPOSITION);
	
	AddNewMenu(add);
}

void CMainFrame::MoveMRU(CSMenuHandle& r, CSMenuHandle& a)
{
	CSMenuHandle file( r.GetSubMenu(0) );
	int state;
	int count = 0;
	for(int i = file.GetCount() - 1; i >= 0; i--)
	{
		state = ::GetMenuState(file, i, MF_BYPOSITION);
		if(state & MF_POPUP)
		{
			CMenuItemInfo mii;
			mii.fMask = MIIM_SUBMENU;
			file.GetItemInfo(i, &mii);
			
			if(mii.hSubMenu == (HMENU)m_RecentFiles)
			{
				// Recent Files Item found...
				if(::RemoveMenu(file, i, MF_BYPOSITION))
				{
					count++;
					// Remove the separator too.
					if(m_RecentProjects.GetCount() == 0)
					{
						count++;
						::RemoveMenu(file, i, MF_BYPOSITION);
					}
				}
			}
			else if(mii.hSubMenu == (HMENU)m_RecentProjects)
			{
				if(::RemoveMenu(file, i, MF_BYPOSITION))
				{
					// Remove the separator too.
					if(m_RecentProjects.GetCount() > 0)
						::RemoveMenu(file, i, MF_BYPOSITION);
					count++;
				}
			}

			if(count == 2)
				break;
		}
	}

	AddMRUMenu(a);
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
				CMenuItemInfo mii;
				mii.fMask = MIIM_SUBMENU;
				view.GetItemInfo(i, &mii);

				if(mii.hSubMenu == (HMENU)m_Switcher)
				{
					//::ModifyMenu(view, i, MF_BYPOSITION | MF_STRING, ID_VIEW_CHANGESCHEME, _T("s")); - destroys menu.
					::RemoveMenu(view, i, MF_BYPOSITION);
					::InsertMenu(view, i, MF_BYPOSITION | MF_STRING, ID_VIEW_CHANGESCHEME, _T("s"));					
				}
			}
		}
	}
	
	if((HMENU)add != m_hMenu)
		AddLanguageMenu(add);
}

void CMainFrame::AddMRUProjectsEntry(LPCTSTR lpszFile)
{
	CSMenuHandle menu(m_hMenu);
	m_RecentProjects.AddEntry(lpszFile);
	m_RecentProjects.UpdateMenu();

	if(m_RecentProjects.GetCount() == 0)
	{
		CSMenuHandle file(menu.GetSubMenu(0));

		for(int i = file.GetCount() - 1; i >= 0; i--)
		{
			if( ::GetMenuItemID(file, i) == ID_APP_EXIT )
			{
				// Insert one back - before the separator...
				::InsertMenu(file.GetHandle(), i-1, MF_BYPOSITION | MF_POPUP, (UINT)(HMENU)m_RecentProjects, _T("Recent P&rojects"));
			}
		}
	}
}

void CMainFrame::setupAccelerators(HMENU mainMenu)
{
	m_hAccel = m_pCmdDispatch->GetAccelerators();
	m_pCmdDispatch->UpdateMenuShortcuts(mainMenu);
}

void CMainFrame::setupToolsUI()
{
	CSMenuHandle menu(m_hMenu);
	CSMenuHandle tools( menu.GetSubMenu(2) );
	ToolsManager* pTM = ToolsManager::GetInstance();

	tstring projid;

	Projects::Workspace* pAW = g_Context.m_frame->GetActiveWorkspace();
	if(pAW)
	{
		Projects::Project* pAP = pAW->GetActiveProject();
		if(pAP)
		{
			Projects::ProjectTemplate* pT = pAP->GetTemplate();
			if(pT)
			{
				projid = pT->GetID();
			}
		}
	}
	
	m_iFirstToolCmd = pTM->UpdateToolsMenu(
		tools, m_pCmdDispatch, m_iFirstToolCmd, ID_TOOLS_DUMMY, NULL, projid.size() > 0 ? projid.c_str() : NULL
	);

	m_hProjAccel = NULL;

	if(pAW)
	{
		if(projid.size() > 0)
		{
			ProjectTools* pTools = pTM->GetToolsForProject(projid.c_str());
			m_hProjAccel = pTools != NULL ? pTools->GetAcceleratorTable() : NULL;
		}
		
		if(m_hProjAccel == NULL)
		{
			SchemeTools* pTools = pTM->GetGlobalProjectTools();
			pTools->AllocateMenuResources(m_pCmdDispatch);
			m_hProjAccel = pTools->GetAcceleratorTable();
		}
	}
}

/**
 * Get the menu command id associated with the toolbar.
 */
DWORD CMainFrame::GetRebarBarCmd(DWORD toolbarId)
{
	return ToolbarCmds[toolbarId - 100];
}

/**
 * Get the ID of the rebar band a toolbar is on.
 */
DWORD CMainFrame::GetRebarBarId(DWORD toolbarId)
{
	return ToolbarIds[toolbarId - 100];
}

/**
 * Get the rebar band index of a given toolbar.
 */
DWORD CMainFrame::GetRebarBarIndex(DWORD toolbarId)
{
	return ::SendMessage(m_hWndToolBar, RB_IDTOINDEX, GetRebarBarId(toolbarId), 0);
}

bool CMainFrame::GetToolbarShowing(DWORD toolbarId)
{
	REBARBANDINFO rbbi = {sizeof(rbbi), RBBIM_STYLE, 0};
	::SendMessage(m_hWndToolBar, RB_GETBANDINFO, GetRebarBarIndex(toolbarId), (LPARAM)&rbbi);
	return ((rbbi.fStyle & RBBS_HIDDEN) == 0);
}

/**
 * Show/Hide toggle a toolbar (rebar band).
 */
void CMainFrame::ToggleToolbar(DWORD toolbarId)
{
	DWORD cmd = GetRebarBarCmd( toolbarId );
	BOOL bNowVisible = !((UIGetState( cmd ) & UPDUI_CHECKED) != 0);
	::SendMessage(m_hWndToolBar, RB_SHOWBAND, GetRebarBarIndex( toolbarId ), bNowVisible);
	UISetCheck(cmd, bNowVisible);
	UpdateLayout();
}

/**
 * Initialise the GUI state manager with all of the GUI items that it controls.
 */
void CMainFrame::InitGUIState()
{
	// Create a list of the docking windows to manage.
	sstate::CDockWndMgrEx dockers(m_hWnd);
	
	for(int i = 0; i <= ID_VIEW_LASTDOCKER-ID_VIEW_FIRSTDOCKER; ++i)
	{
		dockers.Add(sstate::CDockingWindowStateAdapter<CPNDockingWindow>(*m_dockingWindows[i]));
	}
	
	tstring statekey(pnregroot);
	statekey += PNSK_INTERFACE;
	statekey += PNSK_DEFGUI;

	m_GUIState.Initialize(statekey.c_str(), m_hWnd/*, SW_SHOWMAXIMIZED*/);
	m_GUIState.Add(sstate::CRebarStateAdapter(m_hWndToolBar/*, REBAR_SAVESTATE_VERSION*/));
	m_GUIState.Add(dockers);

	//LoadGUIState();

	UISetCheck( GetRebarBarCmd( TBR_FILE   ), GetToolbarShowing( TBR_FILE   ));
	UISetCheck( GetRebarBarCmd( TBR_EDIT   ), GetToolbarShowing( TBR_EDIT   ));
	UISetCheck( GetRebarBarCmd( TBR_SCHEME ), GetToolbarShowing( TBR_SCHEME ));
	UISetCheck( GetRebarBarCmd( TBR_FIND   ), GetToolbarShowing( TBR_FIND   ));
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

	CPNWindowStateStorage storage;
	if( !m_GUIState.Restore(storage) )
	{
		SetDefaultGUIState();
	}

	// Set initial UpdateUI state...
	UISetCheck(ID_EDITOR_OUTPUTWND, getDocker(DW_OUTPUT)->IsWindowVisible());

	UpdateLayout();

	if(pConfigName)
		delete pConfigName;
}

/**
 * Save the default or a named GUI state to the registry.
 */
void CMainFrame::SaveGUIState(LPCTSTR stateName)
{
	tstring configName = "";
	if(stateName)
	{
		configName = stateName;
	}

	CPNWindowStateStorage storage(configName);
	m_GUIState.Store(storage);

	tstring mrukey;
	mrukey = PNSK_MRU;
	m_RecentFiles.Save(OPTIONS, mrukey.c_str());

	mrukey = PNSK_MRUP;
	m_RecentProjects.Save(OPTIONS, mrukey.c_str());
}

/**
 * Configure docking windows etc. to their default state.
 */
void CMainFrame::SetDefaultGUIState()
{
	// Dock the output window to the bottom of the main frame, hide it.
	getDocker(DW_OUTPUT)->Hide();
	getDocker(DW_FINDRESULTS)->Hide();
	//m_pOutputWnd->Hide();
	//m_pClipsWnd->Hide();
	//m_pProjectsWnd->Hide();
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
	return new GlobalOutputWrapper(this, m_pOutputWnd, pChild, *pDefinition);
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
		m_Switcher.SetActiveScheme(static_cast<Scheme*>(pScheme));
	}
}

void CMainFrame::GetOpenDocuments(DocumentList& list)
{
	SWorkspaceWindowsStruct s;
	s.bInProjectGroupOnly = false;
	s.pFunction = &CMainFrame::WorkspaceChildEnumNotify;
	s.pWorkspace = NULL;

	PerformChildEnum(&s);

	for(std::list<CChildFrame*>::iterator i = s.FoundWindows.begin();
		i != s.FoundWindows.end(); 
		++i)
	{
		list.push_back((*i)->GetDocument());
	}
}

void CMainFrame::GetOpenWorkspaceDocuments(DocumentList& list)
{
	if(GetActiveWorkspace() == NULL)
		return;

	SWorkspaceWindowsStruct s;
	s.bInProjectGroupOnly = true;
	s.pFunction = &CMainFrame::WorkspaceChildEnumNotify;
	s.pWorkspace = GetActiveWorkspace();

	PerformChildEnum(&s);

	for(std::list<CChildFrame*>::iterator i = s.FoundWindows.begin();
		i != s.FoundWindows.end(); 
		++i)
	{
		list.push_back((*i)->GetDocument());
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
				m_StatusBar.SetPaneText(ID_DEFAULT_PANE, LS(IDS_DEFAULTSTATUS), (m_bIsXPOrLater ? SBT_NOBORDERS : 0));
				m_bShowingDefaultStatus = true;
			}
		}
}

bool CMainFrame::SaveAll(bool ask)
{
	bool bRet = false;

	CComPtr<ITabbedMDIChildModifiedList> modifiedItems;
	long modifiedCount = 0;

	// Find any children that report they're modified...
	m_tabbedClient.FindModified(&modifiedItems);
	
	if(modifiedItems == NULL)
	{
		::CreateTabbedMDIChildModifiedList(&modifiedItems);
	}
	
	// Now add any modified projects items...
	getProjectsModified(modifiedItems);

	// Display the "save modified items" dialog with
	// checkboxes for each item if there's anything modified.
	modifiedItems->get_Count(&modifiedCount);
	if(modifiedCount > 0)
	{
		if(ask)
		{
			//  There's at least one modified item
			CSaveModifiedItemsDialog dialog(modifiedItems, true);
			INT_PTR response = dialog.DoModal();

			switch(response)
			{
			case IDYES:
				// The dialog will update the list and remove
				// any items that the user unchecked
				if(m_tabbedClient.SaveModified(modifiedItems) != E_ABORT)
					bRet = true;
				break;

			case IDNO:
				bRet = true;
				break;

			case IDCANCEL:
				// Do nothing...
				break;
			}
		}
		else
		{
			if(m_tabbedClient.SaveModified(modifiedItems) != E_ABORT)
				bRet = true;
		}
	}
	else
		bRet = true;

	return bRet;
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
#ifndef _countof
	#define _countof(array) (sizeof(array)/sizeof(array[0]))
#endif

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
	ToggleDockingWindow(DW_OUTPUT, bSetValue, bShowing);
}

void CMainFrame::ToggleDockingWindow(EDocker window, bool bSetValue, bool bShowing)
{
	CPNDockingWindow* dw = getDocker(window);
	PNASSERT(dw);

	if(bSetValue)
	{
		if(bShowing)
		{
			//if( !dw->IsWindowVisible() )
			dw->Show();
		}
		else
		{
			if( dw->IsWindowVisible() )
				dw->Hide();
		}
	}
	else
		dw->Toggle();
}

void CMainFrame::NewProject(LPCTSTR szProjectFile, LPCTSTR name, LPCTSTR templateGuid)
{
	if(!m_pProjectsWnd)
	{
		UNEXPECTED(_T("No projects window!"));
		return;
	}
	else
	{
		getDocker(DW_PROJECTS)->Show();
		::SetFocus(getDocker(DW_PROJECTS)->GetClient());
	}

	Projects::Workspace* workspace = m_pProjectsWnd->GetWorkspace();

	CFileName fn(szProjectFile);
	tstring projname = fn.GetFileName_NoExt();

	if(name == NULL)
	{
		name = projname.c_str();
	}

	if( !Projects::Project::CreateEmptyProject(projname.c_str(), szProjectFile, templateGuid) )
	{
		UNEXPECTED(_T("Failed to create project template file.")); 
		return; // bail.
	}

	Projects::Project* project = new Projects::Project(szProjectFile);

	AddMRUProjectsEntry(szProjectFile);

	if( workspace == NULL )
	{
		// No workspace currently open, create a blank one to store the project in.
		workspace = new Projects::Workspace;
		workspace->SetName(LS(IDS_NEWPROJECTGROUP));
		workspace->AddProject(project);
		m_pProjectsWnd->SetWorkspace(workspace);
		workspace->ClearDirty();
	}
	else
	{
		// Add this project to the current workspace. Yes this should be an option.
		m_pProjectsWnd->AddProject(project);
	}
}

void CMainFrame::OpenProject(LPCTSTR projectPath, bool intoExistingGroup)
{
	if(!m_pProjectsWnd)
		UNEXPECTED(_T("No projects window!"))
	else
	{
		/*if(!getDocker(DW_PROJECTS)->IsWindowVisible())
			getDocker(DW_PROJECTS)->Toggle();*/
		getDocker(DW_PROJECTS)->Show();
		::SetFocus(getDocker(DW_PROJECTS)->GetClient());
	}

	// Check if a project is already open...
	if(!intoExistingGroup && m_pProjectsWnd->GetWorkspace() != NULL)
	{
		if(!CloseWorkspace(true, true))
			return;
	}

	if(!FileExists(projectPath))
	{
		DWORD dwRes = ::MessageBox(m_hWnd, LS(IDS_PROJECTDOESNOTEXIST), LS(IDR_MAINFRAME), MB_YESNO);
		if(dwRes == IDYES)
		{
			NewProject(projectPath);
		}
		return;
	}

	AddMRUProjectsEntry(projectPath);

	Projects::Workspace* workspace = GetActiveWorkspace();
	bool setActive = false;

	if(!intoExistingGroup || workspace == NULL)
	{
		workspace = new Projects::Workspace;
		workspace->SetName(LS(IDS_NEWPROJECTGROUP));
		setActive = true;
	}
	
	Projects::Project* project = new Projects::Project(projectPath);

	workspace->AddProject(project);

	if(setActive)
	{
		m_pProjectsWnd->SetWorkspace(workspace);
		
		workspace->ClearDirty();
	}
}

void CMainFrame::OpenProjectGroup(LPCTSTR projectGroup)
{
	OpenWorkspace(projectGroup);
}

void CMainFrame::OpenWorkspace(LPCTSTR workspacePath)
{
	if(!m_pProjectsWnd)
		UNEXPECTED(_T("No projects window!"))
	else
	{
		getDocker(DW_PROJECTS)->Show();
		::SetFocus(getDocker(DW_PROJECTS)->GetClient());
	}

	// Check if a project is already open...
	if(m_pProjectsWnd->GetWorkspace() != NULL)
	{
		if(!CloseWorkspace(true, true))
			return;
	}

	Projects::Workspace* workspace = new Projects::Workspace(workspacePath);
	m_pProjectsWnd->SetWorkspace(workspace);
	AddMRUProjectsEntry(workspacePath);
}

bool CMainFrame::SaveWorkspaceAs(Projects::Workspace* pWorkspace)
{
	LPCTSTR path = NULL;
				
	// See if there's a sensible path to save the workspace in.
	const Projects::PROJECT_LIST& projects = pWorkspace->GetProjects();
	if(projects.size() > 0)
		path = (*projects.begin())->GetBasePath();
	
	CPNSaveDialog dlg(LS(IDS_PROJECTGROUPFILES), NULL, _T("ppg"));
	
	tstring st = LS(IDS_SAVESOMETHING);
	st += LS(IDS_PROJECTGROUP);
	dlg.SetTitle(st.c_str());
	
	dlg.SetInitialPath(path);

	if(dlg.DoModal() == IDOK)
	{
		pWorkspace->SetFileName(dlg.m_ofn.lpstrFile);
		AddMRUProjectsEntry(dlg.m_ofn.lpstrFile);
		return true;
	}
	else
	{
		return false;
	}
}

bool CMainFrame::CheckSaveWorkspace()
{
	ITabbedMDIChildModifiedList* modifiedItems;
	::CreateTabbedMDIChildModifiedList(&modifiedItems);
	
	// Now add any modified projects items...
	getProjectsModified(modifiedItems);

	// Display the "save modified items" dialog with
	// checkboxes for each item if there's anything modified.
	long modifiedCount = 0;
	modifiedItems->get_Count(&modifiedCount);
	if(modifiedCount > 0)
	{
		//  There's at least one modified item
		CSaveModifiedItemsDialog dialog(modifiedItems, true);

		INT_PTR response = dialog.DoModal();

		if(response == IDYES)
		{
			// The dialog will update the list and remove
			// any items that the user unchecked
			if( m_tabbedClient.SaveModified(modifiedItems) == E_ABORT )
				return false;
		}

		return response != IDCANCEL;
	}

	return true;
}

DWORD CMainFrame::SaveWorkspace(Projects::Workspace* pWorkspace)
{
	if( pWorkspace->CanSave() )
	{
		pWorkspace->Save();
	}
	else
	{
		if( SaveWorkspaceAs(pWorkspace) )
			pWorkspace->Save();
		else
			return IDCANCEL;
	}

	return IDOK;
}

bool CMainFrame::SaveProjects(Projects::Workspace* pWorkspace)
{
	// It's not a real workspace, just a holder for a 
	// project, so we'll just save projects.
	Projects::PROJECT_LIST projects = pWorkspace->GetProjects();
	for(Projects::PL_CIT i = projects.begin();
		i != projects.end();
		++i)
	{
		if((*i)->IsDirty())
		{
			
			CString str;
			str.Format(IDS_PROJSAVECHANGES, (*i)->GetName());
			DWORD dwRes = ::MessageBox(m_hWnd, str, LS(IDR_MAINFRAME), MB_YESNOCANCEL | MB_ICONQUESTION);
			
			if ( dwRes == IDCANCEL )
			{
				return false;
			}
			else if( dwRes == IDYES )
			{
				(*i)->Save();
			}
		}
	}
	
	return true;
}

/**
 * @return False if the user hits cancel at any point, true otherwise.
 */
bool CMainFrame::CloseWorkspaceFiles(Projects::Workspace* pWorkspace)
{
	SWorkspaceWindowsStruct s;
	s.bInProjectGroupOnly = true;
	s.pFunction = &CMainFrame::WorkspaceChildCloseNotify; 
	s.bCanClose = true;
	s.pWorkspace = pWorkspace;

	PerformChildEnum(&s);

	if(s.bCanClose)
		for(std::list<CChildFrame*>::iterator i = s.FoundWindows.begin();
			i != s.FoundWindows.end(); 
			++i)
		{
			::PostMessage((*i)->m_hWnd, WM_CLOSE, 0, 253);
		}

	return s.bCanClose;
}

/**
 * @return True if there are any windows in the workspace.
 */
bool CMainFrame::EnumWorkspaceWindows(SWorkspaceWindowsStruct* pWWS)
{
	pWWS->pFunction = &CMainFrame::WorkspaceChildEnumNotify;

	PerformChildEnum(pWWS);

	return(pWWS->FoundWindows.size() > 0);
}

/**
 * @return False if the user hits cancel at any point, true otherwise.
 */
bool CMainFrame::CloseWorkspace(bool bAllowCloseFiles, bool bAsk)
{
	if( !m_pProjectsWnd )
		return true;
	
	Projects::Workspace* workspace = m_pProjectsWnd->GetWorkspace();
	if( !workspace )
		return true;

	if( bAsk && workspace->IsDirty() )
	{
		// Something about the workspace has changed, 
			// see if the user wants to save it.
		if(!CheckSaveWorkspace())
			return false;
	}

	if(bAsk && bAllowCloseFiles)
	{
		// No point in asking if there are no files open.
		SWorkspaceWindowsStruct wws;
		wws.bInProjectGroupOnly = true;
		wws.pWorkspace = workspace;
		if(EnumWorkspaceWindows(&wws))
		{
			CString str;
			str.LoadString(_Module.m_hInst, IDS_PROJCLOSEFILES);
			DWORD dwRes = ::MessageBox(m_hWnd, str, LS(IDR_MAINFRAME), MB_YESNOCANCEL | MB_ICONQUESTION);

			if( dwRes == IDCANCEL )
				return false;
			else if( dwRes == IDYES )
				if(!CloseWorkspaceFiles(workspace))
					return false;
		}
	}
	
	m_pProjectsWnd->SetWorkspace(NULL);
	delete workspace;

	return true;
}

extensions::ITextOutput* CMainFrame::GetGlobalOutputWindow()
{
	return m_pOutputWnd;
}

bool CMainFrame::getProjectsModified(ITabbedMDIChildModifiedList* pModifiedList)
{
	USES_CONVERSION;

	bool bRet = false;

	Projects::Workspace* workspace = m_pProjectsWnd->GetWorkspace();

	if( workspace != NULL && workspace->IsDirty() )
	{
		// See if it's just projects that are dirty...
		if(workspace->IsDirty(false))
		{
			// No, the workspace is dirty itself...
			CComPtr<ITabbedMDIChildModifiedItem> wsmod;
			::CreateTabbedMDIChildModifiedItem(m_hWnd, LSW(IDS_PROJECTGROUP), CT2CW(workspace->GetName()), LSW(IDS_PROJECTGROUP), 0, ::LoadIcon(_Module.m_hInst, MAKEINTRESOURCE(IDI_WORKSPACE)), &wsmod);
			
			CComPtr<IProjectHolder> ph;
			::CreateProjectHolder(NULL, workspace, &ph);
			wsmod->putref_UserData(ph);

			pModifiedList->Insert(-1, wsmod);
			bRet = true;
		}

        CComPtr<ITabbedMDIChildModifiedItem> wsitem;
		::CreateTabbedMDIChildModifiedItem(m_hWnd, LSW(ID_VIEW_WINDOWS_PROJECT), CT2CW(workspace->GetName()), LSW(ID_VIEW_WINDOWS_PROJECT), 0, ::LoadIcon(_Module.m_hInst, MAKEINTRESOURCE(IDI_WORKSPACE)), &wsitem);
		
		CComPtr<ITabbedMDIChildModifiedList> subitems;
		wsitem->get_SubItems(&subitems);

		int addCount = 0;
	
		Projects::PROJECT_LIST projects = workspace->GetProjects();
		for(Projects::PL_CIT i = projects.begin();
			i != projects.end();
			++i)
		{
			if((*i)->IsDirty())
			{
				CComPtr<ITabbedMDIChildModifiedItem> pitem;
				//ITabbedMDIChildModifiedItem* pitem;
				::CreateTabbedMDIChildModifiedItem(m_hWnd, LSW(IDS_PROJECT), CT2CW((*i)->GetName()), LSW(IDS_PROJECT), 0, ::LoadIcon(_Module.m_hInst, MAKEINTRESOURCE(IDI_PROJECTFOLDER)), &pitem);
				
				CComPtr<IProjectHolder> ph;
				::CreateProjectHolder((*i), NULL, &ph);
				pitem->putref_UserData(ph);

				subitems->Insert(-1, pitem);
				addCount++;
			}
		}

		CComPtr<ITabbedMDIChildModifiedList> ml;
		wsitem->get_SubItems(&ml);
		if(addCount)
		{
			bRet = true;
			pModifiedList->Insert(-1, wsitem);
		}
	}

	return bRet;
}

void CMainFrame::openFileCheckType(LPCTSTR filename, EPNEncoding encoding)
{
	CFileName fn(filename);
	fn.ToLower();
	if(fn.GetExtension() == _T(".pnproj"))
	{
		OpenProject(filename);
	}
	else if(fn.GetExtension() == _T(".ppg"))
	{
		OpenWorkspace(filename);
	}
	else if(fn.GetExtension() == _T(".pnws"))
	{
		if(CloseAll())
		{
			WorkspaceState wss;
			wss.Load(filename);
		}
	}
	else
	{
		if(!CheckAlreadyOpen(filename))
		{
            OpenFile(filename, NULL, encoding);
			AddMRUEntry(filename);
		}
	}
}