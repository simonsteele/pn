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
#include "resource.h"

// Needed because we derive from it.
#include "tools.h"			// External Tools

// Windows and Dialogs
#include "mainfrm.h"		// This Window
#include "outputview.h"		// Output window
#include "childfrm.h"		// MDI Child
#include "finddlg.h"		// Find Dialogs
#include "OptionsPages.h"	// Options Pages
#include "aboutdlg.h"		// About Dialog
#include "pndialogs.h"		// Misc Dialogs.
#include "textclipsview.h"	// Text-Clips Docker...
#include "project.h"		// Projects
#include "projectview.h"	// Projects Docker...

// Other stuff
#include "SchemeConfig.h"	// Scheme Configuration
#include <dbstate.h>		// Docking window state stuff...

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

CMainFrame::CMainFrame() : m_RecentFiles(ID_MRUFILE_BASE, 4), m_RecentProjects(ID_MRUPROJECT_BASE, 4)
{
	m_FindDialog = NULL;
	m_ReplaceDialog = NULL;
	m_pOutputWnd = NULL;
	m_pClipsWnd = NULL;
	m_pProjectsWnd = NULL;
	hFindWnd = NULL;
	hReplWnd = NULL;

	m_statusResetCounter = 0;

	m_CmdBar.SetCallback(this, OnMDISetMenu);

	m_hToolAccel = NULL;

	m_uiMIMessageID = g_Context.m_miManager->GetMessageID();
}

CMainFrame::~CMainFrame()
{
	if(m_pOutputWnd)
		delete m_pOutputWnd;

	if(m_pClipsWnd)
		delete m_pClipsWnd;

	if(m_pProjectsWnd)
		delete m_pProjectsWnd;
}

/**
 * You should always use this function to make a new editor, it sets
 * up the close callback properly. If you don't, things will crash :)
 */
CChildFrame* CMainFrame::NewEditor()
{
	CChildFrame* pChild = new CChildFrame;
	PNASSERT(pChild != NULL);

	// Give the user the option to always maximise new windows.
	bool bMax = OPTIONS->GetCached(Options::OMaximiseNew) != 0;
	pChild->CreateEx(m_hWndMDIClient, 0, 0, bMax ? WS_MAXIMIZE : 0);

	return pChild;
}

bool CMainFrame::OpenFile(LPCTSTR pathname, CScheme* pScheme)
{
	bool bRet = false;

	CChildFrame* pChild = NewEditor();
	if(pathname)
	{
		bRet = pChild->PNOpenFile(pathname, pScheme);
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

void CMainFrame::UpdateStatusBar()
{
	CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor());
	
	if(pChild)
	{
		m_StatusBar.SetPaneText(ID_MOD_PANE, pChild->GetModified() ? _T("Modified") : _T(""));
		pChild->SetPosStatus(m_StatusBar);
	}
	else
	{
		m_StatusBar.SetPaneText(ID_MOD_PANE, _T(""));
		m_StatusBar.SetPaneText(ID_POS_PANE, _T(""));
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

	tstring filename = pChild->GetFileName().c_str();
	Projects::File* pFile = s->pWorkspace->FindFile(filename.c_str());

	if(pFile)
		s->FoundWindows.push_back(pChild);
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
	if((pMsg->message >= WM_KEYFIRST) && (pMsg->message <= WM_KEYLAST) && m_hToolAccel != 0)
	{
		if(::TranslateAccelerator(m_hWnd, m_hToolAccel, pMsg))
			return TRUE;
	}

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
	// Because it's complicated to handle the case directly when a docking window
	// is closed by the user by clicking on its X button, we do it in OnIdle() - a bit
	// messy but hey!
	if(m_pOutputWnd != NULL)
		UISetCheck(ID_EDITOR_OUTPUTWND, m_pOutputWnd->IsWindowVisible());
	if(m_pProjectsWnd != NULL)
		UISetCheck(ID_VIEW_WINDOWS_PROJECT, m_pProjectsWnd->IsWindowVisible());
	if(m_pClipsWnd != NULL)
		UISetCheck(ID_VIEW_WINDOWS_TEXTCLIPS, m_pClipsWnd->IsWindowVisible());

	HWND hWnd = MDIGetActive();
	m_SchemeCombo.EnableWindow( hWnd != NULL );

	bool bChild = false;
	bool bCanSave = false;

	if(hWnd != NULL)
	{
		CChildFrame* pChild = CChildFrame::FromHandle(hWnd);
		if(pChild != NULL)
		{
			bChild = true;
			bCanSave = pChild->GetModified();
		}
	}
	else
	{
		// Clear the status bar if there are no files loaded.
		UpdateStatusBar();
	}
	
	UIEnable(ID_FILE_CLOSE, bChild);
	UIEnable(ID_FILE_SAVE, bCanSave);

	if(m_pProjectsWnd != NULL)
	{
		Projects::Workspace* pWorkspace = m_pProjectsWnd->GetWorkspace();
		UIEnable(ID_FILE_CLOSEWORKSPACE, (pWorkspace != NULL));
	}

	UIUpdateToolBar();

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
				CScheme* pScheme = pChild->GetTextView()->GetCurrentScheme();
				for(int i = 0; i < m_SchemeCombo.GetCount(); i++)
				{
					if( pScheme == static_cast<CScheme*>( m_SchemeCombo.GetItemDataPtr(i) ) )
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

LRESULT CMainFrame::OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	// Check we want to close the workspace...
	if(!CloseWorkspace())
	{
		return 0;
	}

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

	m_FindImages.Create(16, 16, ILC_COLOR32 | ILC_MASK, 3, 1);
	CBitmap bmp;
	bmp.LoadBitmap(IDB_FINDTOOLBAR);
	m_FindImages.Add(bmp, RGB(255, 0, 255));

	fToolbar.SetImageList(m_FindImages.m_hImageList);

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
		
	// Add drop-down style to the button
	tbi.dwMask = TBIF_STYLE;
	fToolbar.GetButtonInfo(ID_FINDTYPE_BUTTON, &tbi);
	tbi.fsStyle |= BTNS_DROPDOWN;
	tbi.fsStyle |= BTNS_SHOWTEXT | BTNS_AUTOSIZE;
	tbi.dwMask |= TBIF_TEXT;
	tbi.pszText = _T("Find");
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

	CSchemeManager* pM = CSchemeManager::GetInstance();
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

LRESULT CMainFrame::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	// create command bar window
	HWND hWndCmdBar = m_CmdBar.Create(m_hWnd, rcDefault, NULL, ATL_SIMPLE_CMDBAR_PANE_STYLE);
	// attach menu
	m_CmdBar.AttachMenu(GetMenu());
	
	// load command bar images
	m_CmdBar.LoadImages(IDR_MAINFRAME);
 	m_CmdBar.LoadImages(IDR_TBR_EDIT);
	
	// Allow us to add images with a pink mask...
	COLORREF clrOld = m_CmdBar.m_clrMask;
	m_CmdBar.m_clrMask = RGB(255,0,255);
	
	// Load some images from a toolbar that's not used.
	m_CmdBar.LoadImages(IDR_TBR_PROJECTS);
	
	// Set the mask back...
	m_CmdBar.m_clrMask = clrOld;
	
	// remove old menu
	SetMenu(NULL);

	HWND hWndToolBar = CreateSimpleToolBarCtrl(m_hWnd, IDR_MAINFRAME, FALSE, ATL_SIMPLE_TOOLBAR_PANE_STYLE);
	HWND hWndEdtToolBar = CreateSimpleToolBarCtrl(m_hWnd, IDR_TBR_EDIT, FALSE, ATL_SIMPLE_TOOLBAR_PANE_STYLE);
	HWND hWndSchemeToolBar = CreateSchemeToolbar();
	HWND hWndFindToolBar = CreateFindToolbar();

	CreateSimpleReBar(PN_REBAR_STYLE);
	AddReBarBand(hWndCmdBar, NULL, FALSE, true);
	AddReBarBand(hWndToolBar, NULL, TRUE, true);
	AddReBarBand(hWndEdtToolBar, NULL, FALSE, true);
	AddReBarBand(hWndSchemeToolBar, NULL, FALSE, true);
	AddReBarBand(hWndFindToolBar, NULL, FALSE, true);
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

	m_bIsXPOrLater = 
		(g_Context.OSVersion.dwPlatformId == VER_PLATFORM_WIN32_NT) &&
		( (g_Context.OSVersion.dwMajorVersion > 4) && (g_Context.OSVersion.dwMinorVersion > 0) );
	
	m_bShowingDefaultStatus = false;
	SetStatusText(NULL);
	
	DragAcceptFiles(TRUE);

	CreateMDIClient();
	m_CmdBar.SetMDIClient(m_hWndMDIClient);

	UIAddToolBar(hWndToolBar);
	UIAddToolBar(hWndEdtToolBar);
	UIAddToolBar(hWndSchemeToolBar);
	UISetCheck(ID_VIEW_TOOLBAR, 1);
	UISetCheck(ID_VIEW_TOOLBAR_EDIT, 1);
	UISetCheck(ID_VIEW_TOOLBARS_FIND, 1);
	UISetCheck(ID_VIEW_TOOLBARS_SCHEMES, 1);
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
	mrukey += PNSK_MRU;
	m_RecentFiles.SetSize(OPTIONS->Get(PNSK_INTERFACE, _T("MRUSize"), 4));
	m_RecentFiles.SetRegistryKey(mrukey);
	m_RecentFiles.UpdateMenu();

	mrukey = pnregroot;
	mrukey += PNSK_MRUP;
	m_RecentProjects.SetSize(OPTIONS->Get(PNSK_INTERFACE, _T("ProjectMRUSize"), 4));
	m_RecentProjects.SetRegistryKey(mrukey);
	m_RecentProjects.UpdateMenu();

	
	AddMRUMenu(CSMenuHandle(m_hMenu));
	AddNewMenu(CSMenuHandle(m_hMenu));

	// Create docking windows...
	DWORD dwStyle = WS_OVERLAPPEDWINDOW | WS_POPUP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS;
	CRect rcBar(0, 0, 200, 70);
	
	m_pOutputWnd = new CDockingOutputWindow;
	m_pOutputWnd->Create(m_hWnd, rcBar, _T("Output"), dwStyle, WS_EX_TOOLWINDOW);
	DockWindow(*m_pOutputWnd, dockwins::CDockingSide::sBottom, 0, 1, 200, 80);

	m_pClipsWnd = new CClipsDocker;
	m_pClipsWnd->Create(m_hWnd, rcBar, _T("Text-Clips"), dwStyle, WS_EX_TOOLWINDOW);
	DockWindow(*m_pClipsWnd, dockwins::CDockingSide::sLeft, 0, 1, 170, 200);

	m_pProjectsWnd = new CProjectDocker;
	m_pProjectsWnd->Create(m_hWnd, rcBar, _T("Projects"), dwStyle, WS_EX_TOOLWINDOW);
	m_pProjectsWnd->DockTo(m_pClipsWnd->m_hWnd, 0);
	//DockWindow(*m_pProjectsWnd, dockwins::CDockingSide::sLeft, 0, 1, 100, 200);

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
				::OutputDebugString(fn);
				bHandled = true;
			}
			else if(wID >= m_RecentProjects.base() && wID <= m_RecentProjects.last() && m_RecentProjects.GetCount() > 0)
			{
				LPCTSTR fn = m_RecentProjects.GetEntry(wID - m_RecentProjects.base());
				::SendMessage(m_hWndStatusBar, SB_SIMPLE, TRUE, 0L);
				::SendMessage(m_hWndStatusBar, SB_SETTEXT, (255 | SBT_NOBORDERS), (LPARAM)fn);
				::OutputDebugString(fn);
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

		for(std::list<tstring>::iterator i = parameters.begin();
			i != parameters.end();
			++i)
		{
			TCHAR ch = (*i)[0];
			if( ch == _T('/') || ch == _T('-') )
			{
				// handle special param...
			}
			else
			{
				if( !CheckAlreadyOpen( (*i).c_str() ) )
					OpenFile( (*i).c_str() );
			}
		}
	}

	::SetForegroundWindow(m_hWnd);

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
        pChild->SetScheme(CSchemeManager::GetInstance()->SchemeByName(newscheme.c_str()));
	else
		pChild->SetScheme(CSchemeManager::GetInstance()->GetDefaultScheme());
		
	
	return 0;
}

LRESULT CMainFrame::OnFileNewProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pProjectsWnd == NULL)
		RETURN_UNEXPECTED(_T("No Projects Window."), 0); // bail.

	CPNSaveDialog dlg(_T("Project Files (*.pnproj)|*.pnproj|"), NULL, _T("pnproj"));
	dlg.SetTitle(_T("Project Location"));
	
	if( !(dlg.DoModal() == IDOK) )
		return 0; // bail.

	NewProject(dlg.GetSingleFileName());

	return 0;
}

LRESULT CMainFrame::OnFileNewWorkspace(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pProjectsWnd == NULL)
		RETURN_UNEXPECTED(_T("No Projects Window."), 0);

	if(!CloseWorkspace())
		return 0;

	Projects::Workspace* workspace = new Projects::Workspace;
	workspace->SetName(_T("New Project Group"));
	m_pProjectsWnd->SetWorkspace(workspace);
	workspace->ClearDirty();

	return 0;
}

LRESULT CMainFrame::OnFileOpen(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CPNOpenDialog dlgOpen(_T("All Files (*.*)|*.*|"));
	dlgOpen.m_ofn.Flags |= OFN_ALLOWMULTISELECT;

	if (dlgOpen.DoModal() == IDOK)
	{
		EAlreadyOpenAction action = (EAlreadyOpenAction)OPTIONS->GetCached(Options::OAlreadyOpenAction);
		bool bOnlyOne = dlgOpen.GetCount() == 1;
		for(CPNOpenDialog::const_iterator i = dlgOpen.begin(); i != dlgOpen.end(); ++i)
		{
			if(bOnlyOne)
			{
				// If we're only opening one file, check to see if it's a project.
				CFileName fn((*i).c_str());
				fn.ToLower();
				if(fn.GetExtension() == _T(".pnproj"))
				{
					OpenProject((*i).c_str());
					break;
				}
				else if(fn.GetExtension() == _T(".ppg"))
				{
					OpenWorkspace((*i).c_str());
					break;
				}
			}

			if( !CheckAlreadyOpen((*i).c_str(), action) )
			{
				if(OpenFile((*i).c_str()))
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

	if( OpenFile(filename) )
	{
		m_RecentFiles.MoveToTop(wID - ID_MRUFILE_BASE);
	}
	else
	{
		// Ask to create? Remove form list?
		// As we abused OpenFile() for the error message the later might be
		// a bad choice...
		//m_RecentFiles.RemoveFromList(wID - ID_MRUFILE_BASE);
	}

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
	CPNOpenDialog dlgOpen(_T("Projects and Project Groups (*.pnproj, *.ppg)|*.pnproj;*.ppg|"));
	
	dlgOpen.m_ofn.lpstrTitle = _T("Open Project/Project Group");

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
	if(m_pOutputWnd == NULL)
		RETURN_UNEXPECTED(_T("No Projects Window."), 0);

	m_pOutputWnd->Toggle();
	UISetCheck(ID_EDITOR_OUTPUTWND, m_pOutputWnd->IsWindowVisible());

	return 0;
}

LRESULT CMainFrame::OnViewProjectWindow(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pProjectsWnd == NULL)
		RETURN_UNEXPECTED(_T("No Projects Window."), 0);

	m_pProjectsWnd->Toggle();
	UISetCheck(ID_VIEW_WINDOWS_PROJECT, m_pProjectsWnd->IsWindowVisible());

	return 0;
}

LRESULT CMainFrame::OnViewTextClipsWindow(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pClipsWnd == NULL)
		RETURN_UNEXPECTED(_T("No Clips Window."), 0);

	m_pClipsWnd->Toggle();
	UISetCheck(ID_VIEW_WINDOWS_PROJECT, m_pClipsWnd->IsWindowVisible());

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

LRESULT CMainFrame::OnOptions(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CSchemeManager* pSM = CSchemeManager::GetInstance();
	LPCTSTR currentScheme = NULL;

	CChildFrame* pFrame = CChildFrame::FromHandle(GetCurrentEditor());
	if(pFrame)
	{
		currentScheme = pFrame->GetTextView()->GetCurrentScheme()->GetName();
	}

	SchemeConfigParser		schemeconfig(currentScheme);
	
	COptionsPageGeneral			general;
	COptionsPageEditDefaults	editDefs;
	COptionsPageVisual			visual;
	COptionsPageConf			confirmations;

	COptionsPageStyle			pageStyle(&schemeconfig);
	COptionsPageSchemes			pageSchemes(&schemeconfig);
	COptionsPageNewFiles		pageNewFiles(&schemeconfig);
	COptionsPageTools			pageTools(&schemeconfig);

	COptionsPageAFiles			pageAFiles;

	schemeconfig.LoadConfig(pSM->GetPath(), pSM->GetCompiledPath());

	COptionsDialog options;
	options.AddPage(&general);
	options.AddPage(&editDefs);
	options.AddPage(&visual);
	options.AddPage(&confirmations);
	options.AddPage(&pageStyle);
	options.AddPage(&pageSchemes);
	options.AddPage(&pageNewFiles);
	options.AddPage(&pageTools);
	options.AddPage(&pageAFiles);

	if( wID != ID_TOOLS_DUMMY )
		options.SetInitialPage(&general);
	else
		options.SetInitialPage(&pageTools);

	if( options.DoModal() == IDOK )
	{
		///@todo more dirty checking...

		// pass in true to cache menu resources.
		SchemeToolsManager::GetInstance()->ReLoad(true);

		if( pageStyle.IsDirty() || pageSchemes.IsDirty() )
            CSchemeManager::GetInstance()->Compile();

		PerformChildEnum(ChildOptionsUpdateNotify);

		m_RecentFiles.SetSize( OPTIONS->Get(PNSK_INTERFACE, _T("MRUSize"), 4) );
		m_RecentFiles.UpdateMenu();

		m_RecentProjects.SetSize( OPTIONS->Get(PNSK_INTERFACE, _T("ProjectMRUSize"), 4) );
	}

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

LRESULT CMainFrame::OnSchemeComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int iSel = m_SchemeCombo.GetCurSel();
	CScheme* pScheme = static_cast<CScheme*>( m_SchemeCombo.GetItemDataPtr( iSel ) );
	if( pScheme != NULL )
	{
		CChildFrame* pEditor = CChildFrame::FromHandle( GetCurrentEditor() );
		if( pEditor != NULL )
			pEditor->SetScheme( pScheme );
	}
	return 0;
}

LRESULT CMainFrame::OnFindComboEnter(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CWindowText wt(m_FindCombo.m_hWnd); //Get find text...

	CChildFrame* pEditor = CChildFrame::FromHandle( GetCurrentEditor() );
	if( pEditor != NULL )
	{
		SFindOptions* pFindOptions = OPTIONS->GetFindOptions();
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
					DWORD dwRes = MessageBox(str, _T("Programmers Notepad 2"), MB_YESNOCANCEL);
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
	if(m_RecentProjects.GetCount() > 0)
		::InsertMenu(file.GetHandle(), ID_APP_EXIT, MF_BYCOMMAND | MF_POPUP, (UINT)(HMENU)m_RecentProjects, _T("Recent P&rojects"));
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
				CMenuItemInfo mii;
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
	dockers.Add(sstate::CDockingWindowStateAdapterEx<CDockingOutputWindow>(*m_pOutputWnd));
	dockers.Add(sstate::CDockingWindowStateAdapterEx<CProjectDocker>(*m_pProjectsWnd));
	dockers.Add(sstate::CDockingWindowStateAdapterEx<CClipsDocker>(*m_pClipsWnd));
	
	tstring statekey(pnregroot);
	statekey += PNSK_INTERFACE;
	statekey += PNSK_DEFGUI;

	m_GUIState.Initialize(statekey.c_str(), m_hWnd/*, SW_SHOWMAXIMIZED*/);
	m_GUIState.Add(sstate::CRebarStateAdapter(m_hWndToolBar, REBAR_SAVESTATE_VERSION));
	m_GUIState.Add(dockers);

	LoadGUIState();

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

	if( !m_pProjectsWnd )
		return;
	
	Projects::Workspace* workspace = m_pProjectsWnd->GetWorkspace();
	if( !workspace )
		return;

	if( workspace->IsDirty() )
	{
		if(workspace->IsDirty(false))
		{
			// Something about the workspace has changed, 
			// see if the user wants to save it.
			DWORD dwRes = SaveWorkspace(workspace, true);
		}

		SaveProjects(workspace);
	}
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

void CMainFrame::NewProject(LPCTSTR szProjectFile)
{
	if(!m_pProjectsWnd)
	{
		UNEXPECTED(_T("No projects window!"));
		return;
	}
	else
	{
		if(!m_pProjectsWnd->IsWindowVisible())
			m_pProjectsWnd->Toggle();
	}

	Projects::Workspace* workspace = m_pProjectsWnd->GetWorkspace();

	CFileName fn(szProjectFile);
	tstring projname = fn.GetFileName_NoExt();

	if( !Projects::Project::CreateEmptyProject(projname.c_str(), szProjectFile) )
	{
		UNEXPECTED(_T("Failed to create project template file.")); 
		return; // bail.
	}

	Projects::Project* project = new Projects::Project(szProjectFile);

	if( workspace == NULL )
	{
		// No workspace currently open, create a blank one to store the project in.
		workspace = new Projects::Workspace;
		workspace->SetName(_T("New Project Group"));
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

void CMainFrame::OpenProject(LPCTSTR projectPath)
{
	if(!m_pProjectsWnd)
		UNEXPECTED(_T("No projects window!"))
	else
	{
		if(!m_pProjectsWnd->IsWindowVisible())
			m_pProjectsWnd->Toggle();
	}

	if(!FileExists(projectPath))
	{
		DWORD dwRes = ::MessageBox(m_hWnd, _T("The specified project does not exist,\n would you like to create it?"), _T("Project Not Found"), MB_YESNO);
		if(dwRes == IDYES)
		{
			NewProject(projectPath);
		}
		return;
	}

	Projects::Workspace* workspace = new Projects::Workspace;
	workspace->SetName(_T("New Project Group"));
	
	Projects::Project* project = new Projects::Project(projectPath);

	workspace->AddProject(project);

	m_pProjectsWnd->SetWorkspace(workspace);
	
	workspace->ClearDirty();
}

void CMainFrame::OpenWorkspace(LPCTSTR workspacePath)
{
	Projects::Workspace* workspace = new Projects::Workspace(workspacePath);
	m_pProjectsWnd->SetWorkspace(workspace);
}

bool CMainFrame::SaveWorkspaceAs(Projects::Workspace* pWorkspace)
{
	LPCTSTR path = NULL;
				
	// See if there's a sensible path to save the workspace in.
	const Projects::PROJECT_LIST& projects = pWorkspace->GetProjects();
	if(projects.size() > 0)
		path = (*projects.begin())->GetBasePath();
	
	CPNSaveDialog dlg(_T("Project Group Files (*.ppg)|*.ppg|"), NULL, _T("ppg"));
	dlg.SetTitle(_T("Save Project Group"));
	dlg.SetInitialPath(path);

	if(dlg.DoModal() == IDOK)
	{
		pWorkspace->SetFileName(dlg.m_ofn.lpstrFile);
		return true;
	}
	else
	{
		return false;
	}
}

DWORD CMainFrame::SaveWorkspace(Projects::Workspace* pWorkspace, bool bAsk)
{
	if(bAsk)
	{
		DWORD dwRes = ::MessageBox(m_hWnd, _T("Do you want to save your changes to the project group?"), _T("Programmers Notepad"), MB_YESNOCANCEL | MB_ICONQUESTION);
		if ( dwRes == IDCANCEL || dwRes == IDNO )
		{
			return dwRes;
		}
	}

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
			tstring msg = _T("Do you want to save changes to the project: ");
			msg += (*i)->GetName();
			msg += _T("?");
			DWORD dwRes = ::MessageBox(m_hWnd, msg.c_str(), _T("Programmers Notepad"), MB_YESNOCANCEL | MB_ICONQUESTION);
			
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
	s.pFunction = WorkspaceChildCloseNotify; 
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
	pWWS->pFunction = WorkspaceChildEnumNotify;

	PerformChildEnum(pWWS);

	return(pWWS->FoundWindows.size() > 0);
}

/**
 * @return False if the user hits cancel at any point, true otherwise.
 */
bool CMainFrame::CloseWorkspace(bool bAllowCloseFiles)
{
	if( !m_pProjectsWnd )
		return true;
	
	Projects::Workspace* workspace = m_pProjectsWnd->GetWorkspace();
	if( !workspace )
		return true;

	if( workspace->IsDirty() )
	{
		if(workspace->IsDirty(false))
		{
			// Something about the workspace has changed, 
			// see if the user wants to save it.
			DWORD dwRes = SaveWorkspace(workspace, true);
			if( dwRes == IDCANCEL )
				return false;

			if( !SaveProjects(workspace) )
				return false;
		}
		else
		{
			if( !SaveProjects(workspace) )
				return false;
		}
	}

	if(bAllowCloseFiles)
	{
		// No point in asking if there are no files open.
		SWorkspaceWindowsStruct wws;
		wws.pWorkspace = workspace;
		if(EnumWorkspaceWindows(&wws))
		{
			DWORD dwRes = ::MessageBox(m_hWnd, _T("Close all project files?"), _T("Programmers Notepad"), MB_YESNOCANCEL | MB_ICONQUESTION);

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