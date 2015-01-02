/**
 * @file mainfrm.cpp
 * @brief Main Window for Programmer's Notepad 2 (Implementation)
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"

// Needed because we derive from it.
#include "tools.h"				// External Tools
#include "pndocking.h"			// Docking Window Stuff

// Functionality
#include "extapp.h"				// Application
#include "workspacestate.h"		// Save Workspace State
#include "ScriptRegistry.h"		// Scripts Registry
#include "findinfiles.h"		// Find in Files
#include "project.h"			// Projects
#include "projectprops.h"		// Project Properties
#include "textclips.h"			// Text Clips
#include "textclips/clipmanager.h" // Text Clips Manager
#include "SchemeConfig.h"		// Scheme Configuration
#include "projectholder.h"
#include "version.h"
#include "updatecheck.h"
#include "singleinstance.h"

// Windows and Dialogs
#include "mainfrm.h"			// This Window
#include "outputview.h"			// Output window
#include "childfrm.h"			// MDI Child
#include "OptionsPages.h"		// Options Pages
#include "aboutdlg.h"			// About Dialog
#include "pndialogs.h"			// Misc Dialogs
#include "textclipsview.h"		// Text-Clips Docker
#include "jumpview.h"			// Tags Docker
#include "projectview.h"		// Projects Docker
#include "findex.h"				// Find Dialog
#include "findinfilesview.h"	// Find in Files view
#include "newprojectdialog.h"	// New Projects Dialog
#include "scriptview.h"			// Scripts Docker
#include "toolsmanager.h"		// Tools Manager
#include "browseview.h"			// Browse Docker
#include "openfilesview.h"		// Open Files Docker

#include "include/encoding.h"

// Other stuff
#include <dbstate.h>			// Docking window state stuff
#include <htmlhelp.h>

using namespace L10N;

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

#define TOOLS_MENU_INDEX 3

CMainFrame::CMainFrame(CommandDispatch* commands, std::list<tstring>* cmdLineArgs) : 
	m_RecentFiles(ID_MRUFILE_BASE, 4), 
	m_RecentProjects(ID_MRUPROJECT_BASE, 4),
	m_pCmdDispatch(commands),
	m_pFindEx(NULL),
	m_pOutputWnd(NULL),
	m_pFindResultsWnd(NULL),
	m_pClipsWnd(NULL),
	m_pCtagsWnd(NULL),
	m_pProjectsWnd(NULL),
	m_pBrowseWnd(NULL),
	m_pOpenFilesWnd(NULL),
	hFindWnd(NULL),
	m_statusResetCounter(0),
	m_hToolAccel(NULL),
	m_hGlobalToolAccel(NULL),
	m_iFirstToolCmd(ID_TOOLS_DUMMY),
	m_hProjAccel(NULL),
	m_hILMain(NULL),
	m_hILMainD(NULL),

	// Store command-line arguments for use later
	m_cmdLineArgs(cmdLineArgs),

	// This text clip manager will be shared by the text clips view and editors
	m_pTextClips(new TextClips::TextClipsManager),

	m_ChildFactory(commands, m_pTextClips, NULL),

	m_bIsXPOrLater(IsXPOrLater())
{
	m_uiMIMessageID = g_Context.m_miManager->GetMessageID();

	SchemeTools* pGlobalTools = ToolsManager::GetInstance()->GetGlobalTools();
	pGlobalTools->AllocateMenuResources(m_pCmdDispatch);
	m_hGlobalToolAccel = pGlobalTools->GetAcceleratorTable();
}

CMainFrame::~CMainFrame()
{
	if (m_pOutputWnd)
		delete m_pOutputWnd;

	if (m_pFindResultsWnd)
		delete m_pFindResultsWnd;

	if (m_pClipsWnd)
		delete m_pClipsWnd;

	if (m_pProjectsWnd)
		delete m_pProjectsWnd;

	if (m_pCtagsWnd)
		delete m_pCtagsWnd;

	if (m_pScriptsWnd)
		delete m_pScriptsWnd;

	if (m_pTextClips)
		delete m_pTextClips;

	if (m_pBrowseWnd)
		delete m_pBrowseWnd;

	if (m_pOpenFilesWnd)
		delete m_pOpenFilesWnd;
}

EditorFactory& CMainFrame::GetFactory()
{
	return m_ChildFactory;
}

bool CMainFrame::OpenFile(LPCTSTR pathname, Scheme* pScheme, EPNEncoding encoding)
{
	DocumentList docs;
	this->GetOpenDocuments(docs);
	if (docs.size() == 1)
	{
		DocumentPtr& doc = docs.front();
		if (!doc->HasFile() && !doc->GetModified())
		{
			return doc->GetFrame()->PNOpenFile(pathname, pScheme, encoding);
		}
	}

	bool bRet(false);
	m_ChildFactory.FromFile(pathname, pScheme, encoding, bRet);
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
			s->FoundWindows.push_front(pChild); //s->FoundWindows.push_back(pChild);
	}
	else
	{
		s->FoundWindows.push_front(pChild); //s->FoundWindows.push_back(pChild);
	}
}

void __stdcall CMainFrame::WorkspaceChildCloseNotify(CChildFrame* pChild, SChildEnumStruct* pES)
{
	SWorkspaceWindowsStruct* s = static_cast<SWorkspaceWindowsStruct*>(pES);

	tstring filename = pChild->GetFileName().c_str();
	Projects::File* pFile = s->pWorkspace->FindFile(filename.c_str());

	if(pFile)
	{
		s->FoundWindows.push_front(pChild); //s->FoundWindows.push_back(pChild);

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
		pChild->Save(true);// save and notify change
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

void __stdcall CMainFrame::ChildProjectNotify(CChildFrame* pChild, SChildEnumStruct* pES)
{
	pChild->SendMessage(PN_PROJECTNOTIFY);
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
	m_ChildFactory.WithScheme(reinterpret_cast<Scheme*>(data));

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
	m_ToolBar.EnableSchemeCombo(hWnd != NULL);

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
		}
	}
	else
	{
		// Clear the status bar if there are no files loaded.
		UpdateStatusBar();
	}
	
	lResult = ::SendMessage(::GetFocus(), EM_CANUNDO, 0, 0);
	UIEnable(ID_EDIT_UNDO, lResult);
	lResult = ::SendMessage(::GetFocus(), SCI_CANREDO, 0, 0);
	UIEnable(ID_EDIT_REDO, lResult);

	if (bChild && ::OpenClipboard(NULL))
	{
		UIEnable(ID_EDIT_PASTE, ::IsClipboardFormatAvailable(CF_TEXT) || ::IsClipboardFormatAvailable(CF_UNICODETEXT));
		::CloseClipboard();
	}
	else 
	{
		UIEnable(ID_EDIT_PASTE, FALSE);
	}

	bool bHaveProjects = false;

	if(m_pProjectsWnd != NULL)
	{
		Projects::Workspace* pWorkspace = m_pProjectsWnd->GetWorkspace();
		UIEnable(ID_FILE_CLOSEWORKSPACE, (pWorkspace != NULL));
		bHaveProjects = pWorkspace != NULL;

		/*if(pWorkspace)
		{
			setupToolsUI();
		}*/
	}

	UIEnable(ID_FILE_CLOSE, bChild);
	UIEnable(ID_FILE_SAVE, bCanSave);
	UIEnable(ID_EDIT_CUT, bHasSel);
	UIEnable(ID_EDIT_COPY, bHasSel);
	UIEnable(ID_TOOLS_RECORDSCRIPT, bChild && m_recordingDoc.get() == 0 && g_Context.ExtApp->GetRecorder().get() != 0);
	UIEnable(ID_TOOLS_STOPRECORDING, bChild && m_recordingDoc.get() != 0);
	UIEnable(ID_FILE_SAVEALL, bChild || bHaveProjects);
	UIEnable(ID_FINDTYPE_BUTTON, bChild);
	UIEnable(ID_VIEW_ZOOM_IN, bChild);
	UIEnable(ID_VIEW_ZOOM_OUT, bChild);

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

LRESULT CMainFrame::OnChildNotify(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	switch(lParam)
	{
		case SCN_UPDATEUI:
			{
				UpdateStatusBar();
			}
			break;

		case PN_MDIACTIVATE:
		case PN_SCHEMECHANGED:
			{
				UpdateStatusBar();
				
				BOOL temp;
				OnUpdateChildUIState(PN_UPDATECHILDUI, 0, 0, temp);

				HWND hMDIChild = MDIGetActive();
				if(hMDIChild != NULL)
				{
					CChildFrame* pChild = CChildFrame::FromHandle(hMDIChild);
					if(pChild != NULL)
					{
						Scheme* pScheme = pChild->GetTextView()->GetCurrentScheme();
						m_ToolBar.SelectScheme(pScheme);

						m_hToolAccel = pChild->GetToolAccelerators();

						g_Context.ExtApp->OnSelectDocument(pChild->GetDocument());
					}
					else
						m_hToolAccel = NULL;
				}
				else
				{
					m_hToolAccel = NULL;
					g_Context.ExtApp->OnSelectDocument(extensions::IDocumentPtr());
				}
			}
			break;

		case PN_MDIDESTROY:
			{
				SetStatusText(NULL);
			}
			break;

		case PN_UPDATEAVAILABLE:
			{
				handleUpdate(reinterpret_cast<const Updates::UpdateAvailableDetails*>(wParam));
			}
			break;

		case PN_REFRESHUPDATEUI:
			{
				const _AtlUpdateUIMap* pMap = m_pUIMap;

				// Force refresh of all UI data:
				for (; pMap->m_nID != (WORD)-1; pMap++)
				{
					UISetState(pMap->m_nID, UIGetState(pMap->m_nID));
				}

				UIUpdateToolBar(TRUE);
				UIUpdateMenuBar(TRUE);
			}
			break;
	}
	
	return TRUE;
}

LRESULT CMainFrame::OnProjectNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
{
	bHandled = FALSE;

	setupToolsUI();
	PerformChildEnum(&CMainFrame::ChildProjectNotify);

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

		CloseAndFreeDlg(m_pFindEx);
		m_pFindEx = NULL;
	}

	return 0;
}

/**
 * Handles a bug where the autocomplete combo box eats the WM_QUIT message causing PN
 * to never exit on Win7. Hopefully won't be required post beta.
 */
LRESULT CMainFrame::OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	::DestroyWindow(m_ToolBar.m_hWnd);

	bHandled = FALSE;
	return 0;
}

#define PN_BETTER_TOOLBAR_STYLE \
	(WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | TBSTYLE_TOOLTIPS | TBSTYLE_LIST | CCS_ADJUSTABLE)

HWND CMainFrame::CreateToolbar(bool lowColor)
{
	HWND hWnd = ::CreateWindowEx(0, TOOLBARCLASSNAME, NULL, PN_BETTER_TOOLBAR_STYLE, 0, 0, 100, 100, m_hWnd, (HMENU)LongToHandle(TBR_FILE), ModuleHelper::GetModuleInstance(), NULL);

	if(!hWnd)
		return 0;

	m_ToolBar.SetLowColor(lowColor);
	m_ToolBar.SubclassWindow(hWnd);
	
	return hWnd;
}

template<class TWnd>
TWnd* CreateDockerWindow(TWnd* pWnd, LPCTSTR title, CRect& rect, CMainFrame* owner, CPNDockingWindow** pArr, int index,
					bool bDock, dockwins::CDockingSide side = dockwins::CDockingSide::sLeft,
					int nBar = 0, float fPctPos = 0)
{
	DWORD dwStyle = /*WS_OVERLAPPED | */WS_CAPTION | WS_THICKFRAME | WS_POPUP | WS_CLIPCHILDREN /*| WS_CLIPSIBLINGS*/;
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
	
	m_pBrowseWnd = CreateDocker<CBrowseDocker>(LS(ID_VIEW_WINDOWS_BROWSER), rcLeft, this,
		m_dockingWindows, ID_VIEW_WINDOWS_BROWSER - ID_VIEW_FIRSTDOCKER,
		true, dockwins::CDockingSide::sLeft);

	hCTagsWnd = m_pCtagsWnd->getHandle();

	m_pScriptsWnd = CreateDocker<CScriptDocker>(LS(ID_VIEW_WINDOWS_SCRIPTS), rcLeft, this, 
		m_dockingWindows, ID_VIEW_WINDOWS_SCRIPTS - ID_VIEW_FIRSTDOCKER,
		true, dockwins::CDockingSide::sRight);

	m_pOpenFilesWnd = CreateDocker<COpenFilesDocker>(LS(ID_VIEW_WINDOWS_OPENFILES), rcLeft, this,
		m_dockingWindows, ID_VIEW_WINDOWS_OPENFILES - ID_VIEW_FIRSTDOCKER,
		true, dockwins::CDockingSide::sRight);

	getDocker(DW_FINDRESULTS)->DockTo( getDocker(DW_OUTPUT)->m_hWnd, 0 );
	getDocker(DW_CTAGS)->DockTo( getDocker(DW_PROJECTS)->m_hWnd, 0 );
	getDocker(DW_TEXTCLIPS)->DockTo( getDocker(DW_PROJECTS)->m_hWnd, 0 );
	getDocker(DW_BROWSER)->DockTo( getDocker(DW_PROJECTS)->m_hWnd, 0 );
	getDocker(DW_SCRIPTS)->DockTo( getDocker(DW_OPENFILES)->m_hWnd, 0 );

	// Register icons for menu niceness...
	/*m_CmdBar.AddIcon(getDocker(DW_FINDRESULTS)->GetIcon(FALSE), ID_VIEW_WINDOWS_FINDRESULTS);
	m_CmdBar.AddIcon(getDocker(DW_PROJECTS)->GetIcon(FALSE), ID_VIEW_WINDOWS_PROJECT);
	m_CmdBar.AddIcon(getDocker(DW_SCRIPTS)->GetIcon(FALSE), ID_VIEW_WINDOWS_SCRIPTS);
	m_CmdBar.AddIcon(getDocker(DW_TEXTCLIPS)->GetIcon(FALSE), ID_VIEW_WINDOWS_TEXTCLIPS);
	m_CmdBar.AddIcon(getDocker(DW_OUTPUT)->GetIcon(FALSE), ID_VIEW_OUTPUT);
	m_CmdBar.AddIcon(getDocker(DW_BROWSER)->GetIcon(FALSE), ID_VIEW_WINDOWS_BROWSER);
	m_CmdBar.AddIcon(getDocker(DW_CTAGS)->GetIcon(FALSE), ID_VIEW_WINDOWS_CTAGS);
	m_CmdBar.AddIcon(getDocker(DW_OPENFILES)->GetIcon(FALSE), ID_VIEW_WINDOWS_OPENFILES);*/
}

static BOOL AddReBarBandCtrl(HWND hWndReBar, HWND hWndBand, int nID = 0, LPCTSTR lpstrTitle = NULL, BOOL bNewRow = FALSE, int cxWidth = 0, BOOL bFullWidthAlways = FALSE, bool bUseChevrons = true)
{
	ATLASSERT(::IsWindow(hWndReBar));   // must be already created
#ifdef _DEBUG
	// block - check if this is really a rebar
	{
		TCHAR lpszClassName[sizeof(REBARCLASSNAME)] = { 0 };
		::GetClassName(hWndReBar, lpszClassName, sizeof(REBARCLASSNAME));
		ATLASSERT(lstrcmp(lpszClassName, REBARCLASSNAME) == 0);
	}
#endif // _DEBUG
	ATLASSERT(::IsWindow(hWndBand));   // must be already created

	// Get number of buttons on the toolbar
	int nBtnCount = (int)::SendMessage(hWndBand, TB_BUTTONCOUNT, 0, 0L);

	// Set band info structure
	REBARBANDINFO rbBand = { RunTimeHelper::SizeOf_REBARBANDINFO() };
#if (_WIN32_IE >= 0x0400)
	rbBand.fMask = RBBIM_CHILD | RBBIM_CHILDSIZE | RBBIM_STYLE | RBBIM_ID | RBBIM_SIZE | RBBIM_IDEALSIZE;
#else
	rbBand.fMask = RBBIM_CHILD | RBBIM_CHILDSIZE | RBBIM_STYLE | RBBIM_ID | RBBIM_SIZE;
#endif // !(_WIN32_IE >= 0x0400)
	if(lpstrTitle != NULL)
		rbBand.fMask |= RBBIM_TEXT;
	rbBand.fStyle = RBBS_CHILDEDGE;
#if (_WIN32_IE >= 0x0500)
	if(nBtnCount > 0)   // add chevron style for toolbar with buttons
		rbBand.fStyle |= RBBS_USECHEVRON;
#endif // (_WIN32_IE >= 0x0500)
	if(bNewRow)
		rbBand.fStyle |= RBBS_BREAK;

	rbBand.lpText = (LPTSTR)lpstrTitle;
	rbBand.hwndChild = hWndBand;
	if(nID == 0)   // calc band ID
		nID = ATL_IDW_BAND_FIRST + (int)::SendMessage(hWndReBar, RB_GETBANDCOUNT, 0, 0L);
	rbBand.wID = nID;

	// Calculate the size of the band
	BOOL bRet = FALSE;
	RECT rcTmp = { 0 };
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
		else if(lpstrTitle == NULL)
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
#endif // (_WIN32_IE >= 0x0400)

	// Add the band
	LRESULT lRes = ::SendMessage(hWndReBar, RB_INSERTBAND, (WPARAM)-1, (LPARAM)&rbBand);
	if(lRes == 0)
	{
		ATLTRACE2(atlTraceUI, 0, _T("Failed to add a band to the rebar.\n"));
		return FALSE;
	}

#if (_WIN32_IE >= 0x0501)
	if (bUseChevrons)
	{
		DWORD dwExStyle = (DWORD)::SendMessage(hWndBand, TB_GETEXTENDEDSTYLE, 0, 0L);
		::SendMessage(hWndBand, TB_SETEXTENDEDSTYLE, 0, dwExStyle | TBSTYLE_EX_HIDECLIPPEDBUTTONS);
	}
#endif // (_WIN32_IE >= 0x0501)

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

	// Load our accelerator table...
	setupAccelerators(m_hMenu);

	return hWnd;
}

LRESULT CMainFrame::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	//////////////////////////////////////////////////////////////
	// Toolbar:

	bool lowColourToolbars = !m_bIsXPOrLater || OPTIONS->Get(PNSK_INTERFACE, _T("LowColourToolbars"), false);
	m_hWndToolBar = CreateToolbar(lowColourToolbars);

	// CmdUI
	UIAddToolBar(m_hWndToolBar);
	UISetCheck(ID_VIEW_TOOLBAR, 1);
	
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

	UpdateLayout();
	
	m_bShowingDefaultStatus = false;
	SetStatusText(NULL);

	//////////////////////////////////////////////////////////////
	// Tabs:

	if(!OPTIONS->Get(PNSK_INTERFACE, _T("Tabs"), true))
	{
		m_tabbedClient.GetTabOwner().ForceTabsHidden(true);
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
	m_ChildFactory.SetMdiClient(m_hWndMDIClient);

	HICON hIconSmall = (HICON)::LoadImage(_Module.GetResourceInstance(), MAKEINTRESOURCE(IDI_READONLY), 
				IMAGE_ICON, ::GetSystemMetrics(SM_CXSMICON), ::GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);
	m_tabbedClient.GetTabOwner().AddIcon(hIconSmall);

	InitializeDockingFrame();
	
	// register object for message filtering and idle updates
	CMessageLoop* pLoop = _Module.GetMessageLoop();
	ATLASSERT(pLoop != NULL);
	pLoop->AddMessageFilter(this);
	pLoop->AddIdleHandler(this);

	// Initialise our popup menus.
	m_Switcher.Reset(m_pCmdDispatch, MENUMESSAGE_CHANGESCHEME);
	
	// We build the New menu as one whole piece. This is moved between child
	// windows as they are activated to avoid needing to re-insert keyboard
	// shortcuts etc.
	SchemeManager::GetInstance()->BuildMenu(static_cast<HMENU>(m_NewMenu), m_pCmdDispatch, this);

	tstring mrukey;
	mrukey = PNSK_MRU;
	m_RecentFiles.SetSize(OPTIONS->Get(PNSK_INTERFACE, _T("MRUSize"), 4));
	m_RecentFiles.Load(OPTIONS, mrukey.c_str());
	m_RecentFiles.UpdateMenu();

	mrukey = PNSK_MRUP;
	m_RecentProjects.SetSize(OPTIONS->Get(PNSK_INTERFACE, _T("ProjectMRUSize"), 4));
	m_RecentProjects.Load(OPTIONS, mrukey.c_str());
	m_RecentProjects.UpdateMenu();

	CSMenuHandle menu(m_hMenu);
	AddMRUMenu(menu);
	AddNewMenu(menu);

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
	bool bReadOnly = false;
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
					CT2CA schemeconv(nextArg);
					pScheme = pSM->SchemeByName(schemeconv);
					if(pScheme != NULL && pScheme != pSM->GetDefaultScheme())
					{
						bHaveScheme = true;
					}
					else
					{
						pScheme = NULL;
					}
				}
				else if (_tcsicmp(&parm[1], _T("-readonly")) == 0)
				{
					bReadOnly = true;
					skip = false;
					continue;
				}
				else
				{
					skip = false;
				}
				
				if(skip)
				{
					++i;
					continue;
				}
			}
		}
		else
		{
			CFileName fn(parm);
			fn.Sanitise();

			if(!CheckAlreadyOpen(fn.c_str()))
			{
				openFileCheckType(fn.c_str());
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

				if (bReadOnly)
				{
					pChild->SetReadOnly(true);
				}
			}
		}

		bHaveCol = bHaveLine = bHaveScheme = bReadOnly = false;
	}
}

LRESULT CMainFrame::OnInitialiseFrame(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	LoadGUIState();

	handleCommandLine(*m_cmdLineArgs);

	if (OPTIONS->Get(PNSK_INTERFACE, _T("CheckAssocsOnStartup"), false))
	{
		FileAssocManager fam;
		if (fam.CheckAssociations())
		{
			fam.UpdateAssociations();
		}
	}

	HWND hWndEditor = GetCurrentEditor();
	if (OPTIONS->Get(PNSK_INTERFACE, _T("SaveWorkspace"), false))
	{
		WorkspaceState wss;
		wss.Load();

		// If the user selected a file on the command line, re-activate it to
		// avoid workspace files stealing the focus.
		if (hWndEditor != NULL)
		{
			::SetFocus(hWndEditor);
		}
	}

	hWndEditor = GetCurrentEditor();
	if (hWndEditor == NULL)
	{
		if (OPTIONS->Get(PNSK_INTERFACE, _T("NewOnStart"), true))
		{
			BOOL bHandled;
			OnFileNew(0, 0, 0, bHandled);
		}
	}

	delete m_cmdLineArgs;
	m_cmdLineArgs = NULL;

	Updates::CheckForUpdates(m_hWnd);

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

	if (IsIconic())
	{
		ShowWindow(SW_RESTORE);
	}

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
	m_ToolBar.SetFindText(so->GetFindText());
	return 0;
}

LRESULT CMainFrame::OnMDISetMenu(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = FALSE;

	// PN Specific Code
	OnMDISetMenu((HMENU)lParam, (HMENU)wParam);

	return 0;
}

/**
 * PN_UPDATECHILDUI is sent from the child frame to indicate that something about its
 * config has changed and that global UI state might be affected.
 */
LRESULT CMainFrame::OnUpdateChildUIState(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CChildFrame* pChildFrame = CChildFrame::FromHandle(GetCurrentEditor());
	if (pChildFrame != NULL)
	{
		UISetCheck(ID_EDITOR_COLOURISE, pChildFrame->UIGetChecked(ID_EDITOR_COLOURISE));
		UISetCheck(ID_EDITOR_EOLCHARS, pChildFrame->UIGetChecked(ID_EDITOR_EOLCHARS));
		UISetCheck(ID_EDITOR_LINENOS, pChildFrame->UIGetChecked(ID_EDITOR_LINENOS));
		UISetCheck(ID_EDITOR_WORDWRAP, pChildFrame->UIGetChecked(ID_EDITOR_WORDWRAP));
		UISetCheck(ID_EDITOR_WRITEPROTECT, pChildFrame->UIGetChecked(ID_EDITOR_WRITEPROTECT));
		UISetCheck(ID_EDITOR_WHITESPACE, pChildFrame->UIGetChecked(ID_EDITOR_WHITESPACE));
	}
	else
	{
		UISetCheck(ID_EDITOR_COLOURISE, false);
		UISetCheck(ID_EDITOR_EOLCHARS, false);
		UISetCheck(ID_EDITOR_LINENOS, false);
		UISetCheck(ID_EDITOR_WORDWRAP, false);
		UISetCheck(ID_EDITOR_WRITEPROTECT, false);
		UISetCheck(ID_EDITOR_WHITESPACE, false);
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
	Scheme* scheme;
	tstring newscheme =	OPTIONS->Get(PNSK_EDITOR, _T("NewScheme"), _T(""));
	CT2CA schemename(newscheme.c_str());
	if(newscheme.length() > 0)
        scheme = SchemeManager::GetInstance()->SchemeByName(schemename);
	else
		scheme = SchemeManager::GetInstance()->GetDefaultScheme();

	m_ChildFactory.WithScheme(scheme);
		
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
	fn.AddExtension(_T(".pnproj"));
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
	CAutoOpenDialogEx dlgOpen(LS(IDS_ALLFILES));
	dlgOpen.SetAllowMultiSelect(true);

	tstring path;

	if (OPTIONS->Get(PNSK_INTERFACE, _T("OpenInCurrentDir"), true))
	{
		CChildFrame* pChild = CChildFrame::FromHandle( GetCurrentEditor() );
		if(pChild != NULL)
		{
			path = pChild->GetFileName( FN_PATH );
			dlgOpen.SetInitialPath(path.c_str());
		}
	}
	else if (m_lastOpenPath.size())
	{
		dlgOpen.SetInitialPath(m_lastOpenPath.c_str());
	}

	if (dlgOpen.DoModal() == IDOK)
	{
		resetCurrentDir(true);

		EAlreadyOpenAction action = (EAlreadyOpenAction)OPTIONS->GetCached(Options::OAlreadyOpenAction);
		for(IFileOpenDialogBase::const_iterator i = dlgOpen.begin(); i != dlgOpen.end(); ++i)
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
		CString err;
		err.Format(IDS_CANTOPENFILE, filename);
		::MessageBox(m_hWnd, (LPCTSTR)err, LS(IDR_MAINFRAME), MB_ICONWARNING | MB_OK);

		m_RecentFiles.RemoveEntry(wID - ID_MRUFILE_BASE);
		return 0;
	}

	if(!CheckAlreadyOpen(filename))
	{
		if( OpenFile(filename) )
		{
			m_RecentFiles.MoveToTop(wID - ID_MRUFILE_BASE);
		}
	}

	return 0;
}

LRESULT CMainFrame::OnDockerToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	PNASSERT(wID >= ID_VIEW_FIRSTDOCKER && wID <= ID_VIEW_LASTDOCKER);
	m_dockingWindows[wID - ID_VIEW_FIRSTDOCKER]->Toggle();
	HWND hWnd = ::GetFocus();
	TCHAR buf[200];
	_stprintf(buf, L"Focus: %d\n", hWnd);
	LOG(buf);

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
	CAutoOpenDialog dlgOpen(LS(IDS_ALLPROJECTFILES));
	
	tstring s = StringLoader::Get(IDS_OPENPROJECTDLGTITLE);
	dlgOpen.SetTitle(s.c_str());

	if(dlgOpen.DoModal() == IDOK)
	{
		resetCurrentDir(false);

		CFileName fn(dlgOpen.GetSingleFileName());
		fn.ToLower();
		if (fn.GetExtension() == _T(".pnproj"))
		{
			OpenProject(fn.c_str());
		}
		else if (fn.GetExtension() == _T(".ppg"))
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

LRESULT CMainFrame::OnFileOpenWorkspaceState(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CAutoOpenDialog dlgOpen(LS(IDS_WORKSPACEFILES));
	
	if (dlgOpen.DoModal(m_hWnd) == IDOK)
	{
		WorkspaceState wss;
		wss.Load(dlgOpen.GetSingleFileName());
	}

	return 0;
}

LRESULT CMainFrame::OnFileSaveWorkspaceState(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CAutoSaveDialog dlgSave(LS(IDS_WORKSPACEFILES));
	dlgSave.SetDefaultExtension(_T("pnws"));

	Projects::Workspace* pWorkspace = GetActiveWorkspace();
	if (pWorkspace != NULL)
	{
		LPCTSTR path = NULL;

		// See if there's a sensible path to save the workspace in.
		const Projects::PROJECT_LIST& projects = pWorkspace->GetProjects();
		if (projects.size() > 0)
		{
			path = (*projects.begin())->GetBasePath();
		}
		
		dlgSave.SetInitialFilename(pWorkspace->GetName());
		dlgSave.SetInitialPath(path);
	}

	if (dlgSave.DoModal() == IDOK)
	{
		resetCurrentDir(false);

		WorkspaceState wss;
		wss.Save(dlgSave.GetSingleFileName());
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
	ToggleToolbar();
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

LRESULT CMainFrame::OnCloseAllOther(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	HWND staysOpen = reinterpret_cast<HWND>(wParam);

	HWND hWndChild = ::GetTopWindow(m_tabbedClient.m_hWnd);
	while(hWndChild != NULL)
	{
		HWND hWndClose = hWndChild;
		hWndChild = ::GetNextWindow(hWndChild, GW_HWNDNEXT);

		if(hWndClose != staysOpen && ::IsWindow(hWndClose))
		{
			// This is not the current window, so send a 
			// close message it should understand.
			::SendMessage(hWndClose, WM_SYSCOMMAND, SC_CLOSE, 0L);
		}
	}

	return 0;
}

/**
 * Called when the system is shutting down or the user is being logged off. Return
 * zero to prevent shutdown. We ask the user to save any modified files, and only if they
 * cancel do we return 0.
 */
LRESULT CMainFrame::OnQueryEndSession(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	if (SaveAll(true))
	{
		if(OPTIONS->Get(PNSK_INTERFACE, _T("SaveWorkspace"), false))
		{
			WorkspaceState wss;
			wss.Save();
		}
		
		SaveGUIState();

		return 1;
	}

	return 0;
}

/**
 * Called when the system is shutting down and the user has already been asked (or no
 * files were modified. Get out as fast as possible.
 */
LRESULT CMainFrame::OnEndSession(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	if (wParam)
	{
		// Close workspace, don't ask anything...
		CloseWorkspace(false, false);
		
		// Close all files, don't ask anything...
		m_tabbedClient.CloseAll(true);
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
	LPCSTR currentScheme = NULL;

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
	COptionsPageEditing			pageEditing;

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
	options.AddPage(&pageEditing);
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
		// Save the modified tool sets...
		toolsmanager.Save();

		// and then re-load them into the main manager...
		ToolsManager* pSTM = ToolsManager::GetInstance();
		pSTM->ReLoad(m_pCmdDispatch); // pass in true to cache menu resources.

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

/**
 * User wants to record a script/macro, we delegate to any registered instance of extensions::IRecorder
 */
LRESULT CMainFrame::OnRecordScript(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	// Check we can get a recorder
	extensions::IRecorderPtr recorder = g_Context.ExtApp->GetRecorder();
	if (recorder.get() == NULL)
	{
		return 0;
	}

	// Currently only start recording when there's a document open
	CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor());
	if (!pChild)
	{
		return 0;
	}

	m_recordingDoc = pChild->GetDocument();
	pChild->StartRecord(recorder);

	return 0;
}

/**
 * User wants to stop/finalise a script recording session
 */
LRESULT CMainFrame::OnStopRecording(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if (!m_recordingDoc.get())
	{
		// Doesn't look like we're really recording.
		return 0;
	}

	// Asking the view to stop the recording should cause ClearRecordingState() to be called from there, resetting m_recordingDoc.
	m_recordingDoc->GetFrame()->StopRecord();

	return 0;
}

LRESULT CMainFrame::OnWebPNHome(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::ShellExecute(m_hWnd, _T("open"), _T("http://www.pnotepad.org/"), NULL, NULL, SW_SHOW);
	
	return 0;
}

LRESULT CMainFrame::OnWebSFPage(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::ShellExecute(m_hWnd, _T("open"), _T("https://github.com/simonsteele/pn/"), NULL, NULL, SW_SHOW);

	return 0;
}

LRESULT CMainFrame::OnWebSFBug(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::ShellExecute(m_hWnd, _T("open"), _T("https://github.com/simonsteele/pn/issues/new"), NULL, NULL, SW_SHOW);

	return 0;
}

LRESULT CMainFrame::OnWebPNDoc(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::ShellExecute(m_hWnd, _T("open"), _T("http://www.pnotepad.org/docs/"), NULL, NULL, SW_SHOW);

	return 0;
}

/**
 * User wants to forcibly check for updates now.
 */
LRESULT CMainFrame::OnUpdateCheck(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	bool updateAvailable = Updates::CheckForUpdatesSync(m_hWnd);

	// Updates will have been prompted to the user if available
	if (!updateAvailable)
	{
		PNTaskDialog(m_hWnd, LS(IDR_MAINFRAME), IDS_NOUPDATEAVAILABLE, _T(""), TDCBF_OK_BUTTON, TDT_INFORMATION_ICON);
	}

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
	{
		std::string currentWord(pChild->GetTextView()->GetCurrentWord());
		tstring currentWordT;
		if (pChild->GetTextView()->GetEncoding() != eUnknown)
		{
			// Editor is in unicode mode:
			Utf8_Utf16 conv(currentWord.c_str());
			currentWordT = conv;
		}
		else
		{
			Windows1252_Utf16 conv(currentWord.c_str());
			currentWordT = conv;
		}

		m_pFindEx->Show(findType, currentWordT.c_str());
	}
	else
	{
		m_pFindEx->Show(findType);
	}
}

void CMainFrame::launchExternalSearch(LPCTSTR searchString)
{
	tstring findText = m_ToolBar.GetFindText();

	tstring s(searchString);

	// avoid empty search strings...
	if( findText.size() )
	{
		tstring searchstr(findText);
		
		for(size_t i = 0; i < searchstr.size(); i++)
		{
			if(searchstr[i] == _T(' '))
				searchstr[i] = _T('+');
		}

		s += searchstr;
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
	CFileName fn(_T("pn2.chm"));
	fn.Root(path.c_str());
	path = fn.c_str();

	path += _T("::/htmlhelp/index.html");

	// Passing NULL as the caller causes HtmlHelp to load in sibling mode
	::HtmlHelp(NULL,
         path.c_str(),
         HH_DISPLAY_TOC,
         NULL) ;

	return 0;
}

LRESULT CMainFrame::OnSchemeComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Scheme* pScheme = m_ToolBar.GetSelectedScheme();
	if (pScheme != NULL)
	{
		CChildFrame* pEditor = CChildFrame::FromHandle(GetCurrentEditor());
		if (pEditor != NULL)
		{
			pEditor->SetScheme(pScheme, false);
		}
	}

	return 0;
}

LRESULT CMainFrame::OnFindComboEnter(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	tstring findText = m_ToolBar.GetFindText();

	CChildFrame* pEditor = CChildFrame::FromHandle( GetCurrentEditor() );
	if( pEditor != NULL && findText.size() > 0 )
	{
		SearchOptions* pFindOptions = reinterpret_cast<SearchOptions*>( OPTIONS->GetSearchOptions() );
		if(_tcscmp(pFindOptions->GetFindText(), findText.c_str()) != 0)
		{
			pFindOptions->SetFound(false);
			pFindOptions->SetFindText(findText.c_str());
			m_ToolBar.AddFindText(findText.c_str());
		}

		pEditor->FindNext(pFindOptions);
	}
	
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
	switch(options->GetFileSet())
	{
	case extensions::fifSingleFile:
		{
			CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor());
			if(pChild)
			{
				m_pFindResultsWnd->OnBeginSearch(options->GetFindText(), options->GetUseRegExp());
				pChild->GetTextView()->FindAll(options, m_pFindResultsWnd, pChild->GetFileName().c_str());
				m_pFindResultsWnd->OnEndSearch(0, 1);
			}
		}
		break;

	case extensions::fifPath:
		{
			FindInFiles::GetInstance()->Stop();
			FindInFiles::GetInstance()->Start(
				options->GetFindText(),
				options->GetSearchPath(),
				options->GetFileExts(),
				options->GetRecurse(),
				options->GetMatchCase(),
				options->GetMatchWholeWord(),
				options->GetIncludeHidden(),
				m_pFindResultsWnd);
		}
		break;

	case extensions::fifOpenFiles:
		{
			m_pFindResultsWnd->OnBeginSearch(options->GetFindText(), options->GetUseRegExp());

			DocumentList list;
			g_Context.m_frame->GetOpenDocuments(list);
			int documents(0);
			for(DocumentList::const_iterator i = list.begin();
				i != list.end();
				++i)
			{
				documents++;
				(*i)->GetFrame()->GetTextView()->FindAll(options, m_pFindResultsWnd, (*i)->GetFileName(FN_FULL).c_str());
			}

			m_pFindResultsWnd->OnEndSearch(0, documents);
		}
		break;

	case extensions::fifActiveProjectFiles:
		{
			FindInFiles::GetInstance()->Stop();
			
			FileItPtr pIterable(new StringListIterator());
			StringListIterator* files = static_cast<StringListIterator*>(pIterable.get());

			Projects::Workspace* pAW = g_Context.m_frame->GetActiveWorkspace();
			if(pAW)
			{
				Projects::Project* pAP = pAW->GetActiveProject();
				if(pAP)
				{
					pAP->GetAllFiles(files->GetList());
					
					// Remove any modified documents (we need to do this differently)
					DocumentList list;
					g_Context.m_frame->GetOpenDocuments(list);
					for(DocumentList::const_iterator i = list.begin();
						i != list.end();
						++i)
					{ 
						tstring cfn1 = (*i)->GetFileName();
						for(std::vector<tstring>::iterator k = files->GetList().begin();
							k != files->GetList().end();
							++k)
						{
							if (cfn1 == (*k) && (*i)->GetFrame()->GetModified())
							{
								k = files->GetList().erase(k);
								break;
							}
						}
					}

					// Start the search
					FindInFiles::GetInstance()->Start(
						options->GetFindText(),
						pIterable,
						options->GetMatchCase(),
						options->GetMatchWholeWord(),
						m_pFindResultsWnd);
				}
			}
		}
		break;

	}
}

/**
 * Called from an editor to signal script recording has ended
 */
void CMainFrame::RecordingStopped()
{
	m_recordingDoc.reset();
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
	
	CSMenuHandle file = menu.GetSubMenu(LS(IDS_MENU_FILE));
	CString str;
	str.LoadString(IDS_NEW);
	::InsertMenu(file, 0, MF_BYPOSITION | MF_POPUP, (UINT)(HMENU)m_NewMenu, str);
}

void CMainFrame::AddMRUMenu(CSMenuHandle& menu)
{
	CString str;
	CSMenuHandle file(menu.GetSubMenu(LS(IDS_MENU_FILE)));
	
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
	CSMenuHandle view(menu.GetSubMenu(LS(IDS_MENU_VIEW)));
	
	PNASSERT(::IsMenu(m_Switcher));
	str.LoadString(IDS_CHANGESCHEME);
	::ModifyMenu(view.GetHandle(), ID_VIEW_CHANGESCHEME, MF_BYCOMMAND | MF_POPUP, (UINT)(HMENU)m_Switcher, str);
}

void CMainFrame::MoveNewMenu(CSMenuHandle& remove, CSMenuHandle& add)
{
	CSMenuHandle file( remove.GetSubMenu(LS(IDS_MENU_FILE)) );
	::RemoveMenu(file, 0, MF_BYPOSITION);
	
	AddNewMenu(add);
}

void CMainFrame::MoveMRU(CSMenuHandle& r, CSMenuHandle& a)
{
	CSMenuHandle file( r.GetSubMenu(LS(IDS_MENU_FILE)) );
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
	CSMenuHandle view = remove.GetSubMenu(LS(IDS_MENU_VIEW));
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
		CSMenuHandle file(menu.GetSubMenu(LS(IDS_MENU_FILE)));

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
	CSMenuHandle tools(menu.GetSubMenu(LS(IDS_MENU_TOOLS)));
	ToolsManager* pTM = ToolsManager::GetInstance();

	tstring projid;

	Projects::Workspace* activeWorkspace = g_Context.m_frame->GetActiveWorkspace();
	if (activeWorkspace)
	{
		Projects::Project* activeProject = activeWorkspace->GetActiveProject();
		if (activeProject)
		{
			Projects::ProjectTemplate* pT = activeProject->GetTemplate();
			if (pT)
			{
				projid = pT->GetID();
			}
		}
	}
	
	m_iFirstToolCmd = pTM->UpdateToolsMenu(
		tools, m_pCmdDispatch, m_iFirstToolCmd, ID_TOOLS_DUMMY, NULL, projid.size() > 0 ? projid.c_str() : NULL
	);

	m_hProjAccel = NULL;

	if (activeWorkspace)
	{
		if (projid.size() > 0)
		{
			ProjectTools* pTools = pTM->GetToolsForProject(projid.c_str());
			m_hProjAccel = pTools != NULL ? pTools->GetAcceleratorTable() : NULL;
		}
		
		if (m_hProjAccel == NULL)
		{
			SchemeTools* pTools = pTM->GetGlobalProjectTools();
			pTools->AllocateMenuResources(m_pCmdDispatch);
			m_hProjAccel = pTools->GetAcceleratorTable();
		}
	}
}

/**
 * Show/Hide toggle a toolbar (rebar band).
 */
void CMainFrame::ToggleToolbar()
{
	bool bNowVisible = !((UIGetState( ID_VIEW_TOOLBAR ) & UPDUI_CHECKED) != 0);
	::ShowWindow(m_hWndToolBar, bNowVisible ? SW_SHOW : SW_HIDE);
	UISetCheck(ID_VIEW_TOOLBAR, bNowVisible);
	OPTIONS->Set(PNSK_INTERFACE, _T("ShowToolbar"), bNowVisible);
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
	//m_GUIState.Add(sstate::CRebarStateAdapter(m_hWndToolBar/*, REBAR_SAVESTATE_VERSION*/));
	m_GUIState.Add(dockers);
}

/**
 * Load a named or default GUI state from the registry.
 */
void CMainFrame::LoadGUIState()
{
	CPNWindowStateStorage storage;
	if( !m_GUIState.Restore(storage) )
	{
		SetDefaultGUIState();
	}

	// Set initial UpdateUI state...
	UISetCheck(ID_EDITOR_OUTPUTWND, getDocker(DW_OUTPUT)->IsWindowVisible());

	BOOL showToolbar = OPTIONS->Get(PNSK_INTERFACE, _T("ShowToolbar"), true);
	if (!showToolbar)
	{
		::ShowWindow(m_hWndToolBar, SW_HIDE);
	}

	UISetCheck(ID_VIEW_TOOLBAR, showToolbar);

	UpdateLayout();
}

/**
 * Save the default or a named GUI state to the registry.
 */
void CMainFrame::SaveGUIState()
{
	CPNWindowStateStorage storage;
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
	getDocker(DW_BROWSER)->Hide();
	getDocker(DW_PROJECTS)->Hide();
	getDocker(DW_TEXTCLIPS)->Hide();
	getDocker(DW_CTAGS)->Hide();
	getDocker(DW_SCRIPTS)->Hide();
	getDocker(DW_OPENFILES)->Hide();
}

void CMainFrame::PerformChildEnum(SChildEnumStruct* s)
{
	s->pMainFrame = this;
	EnumChildWindows(m_hWndMDIClient, ChildEnumProc, reinterpret_cast<LPARAM>(s));
}

void CMainFrame::PerformChildEnum(lpChildEnumFn pFunction)
{
	SChildEnumStruct s = {pFunction, this};
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

/**
 * Sorter for comparing child windows based on their tab control
 * index.
 */
class SortByTabIndex
{
public:
	SortByTabIndex(CMainFrame* mainframe) : m_frame(mainframe){}
	
	bool operator()(CChildFrame* p1, CChildFrame* p2)
	{
		return m_frame->GetTabIndex(p1) < m_frame->GetTabIndex(p2);
	}

private:
	CMainFrame* m_frame;
};

void CMainFrame::GetOpenDocuments(DocumentList& list)
{
	SWorkspaceWindowsStruct s;
	s.bInProjectGroupOnly = false;
	s.pFunction = &CMainFrame::WorkspaceChildEnumNotify;
	s.pWorkspace = NULL;

	PerformChildEnum(&s);

	s.FoundWindows.sort(SortByTabIndex(this));

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
	
	//BOOL bRet = m_CmdBar.TrackPopupMenu(hMenu, uFlags, x, y, lpParams);
	BOOL bRet = ::TrackPopupMenuEx(hMenu, uFlags, x, y, m_hWnd, lpParams);

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

/**
 * @return true if all files are unmodified/user saves them. False if the user cancels.
 */
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
			HWND hWnd = ::GetFocus();
			TCHAR buf[200];
			_stprintf(buf, L"Focus: %d\n", hWnd);
			LOG(buf);
		}
		else
		{
			if( dw->IsWindowVisible() )
				dw->Hide();
		}
	}
	else
	{
		dw->Toggle();
		HWND hWnd = ::GetFocus();
		TCHAR buf[200];
		_stprintf(buf, L"Focus: %d\n", hWnd);
		LOG(buf);
	}
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
	{
		UNEXPECTED(_T("No projects window!"))
		return;
	}
	
	getDocker(DW_PROJECTS)->Show();
	::SetFocus(getDocker(DW_PROJECTS)->GetClient());

	// Check if a project is already open...
	if (!intoExistingGroup && m_pProjectsWnd->GetWorkspace() != NULL)
	{
		if (!CloseWorkspace(true, true))
			return;
	}

	if (!FileExists(projectPath))
	{
		DWORD dwRes = ::MessageBox(m_hWnd, LS(IDS_PROJECTDOESNOTEXIST), LS(IDR_MAINFRAME), MB_YESNO);
		if (dwRes == IDYES)
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

	if (setActive)
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
	{
		UNEXPECTED(_T("No projects window!"))
		return;
	}
	
	getDocker(DW_PROJECTS)->Show();
	::SetFocus(getDocker(DW_PROJECTS)->GetClient());
	
	// Check if a project is already open...
	if (m_pProjectsWnd->GetWorkspace() != NULL)
	{
		if (!CloseWorkspace(true, true))
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
	
	CAutoSaveDialog dlg(LS(IDS_PROJECTGROUPFILES));
	dlg.SetDefaultExtension(_T("ppg"));
	
	tstring st = LS(IDS_SAVESOMETHING);
	st += LS(IDS_PROJECTGROUP);
	dlg.SetTitle(st.c_str());

	dlg.SetInitialFilename(pWorkspace->GetName());
	dlg.SetInitialPath(path);

	if(dlg.DoModal() == IDOK)
	{
		resetCurrentDir(false);

		pWorkspace->SetFileName(dlg.GetSingleFileName());
		AddMRUProjectsEntry(dlg.GetSingleFileName());
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
			DWORD dwRes = ::MessageBox(m_hWnd, LS(IDS_PROJCLOSEFILES), LS(IDR_MAINFRAME), MB_YESNOCANCEL | MB_ICONQUESTION);

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

/**
 * Get the index of the tab for a child window.
 */
int CMainFrame::GetTabIndex(CChildFrame* child)
{
	return m_tabbedClient.GetTabIndex(child->m_hWnd);
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

void CMainFrame::handleUpdate(const Updates::UpdateAvailableDetails* details)
{
	CStringW msg;
	CStringW ver;
	ver.Format(L"%d.%d.%d Build %d", details->Major, details->Minor, details->Revision, details->Build);
	msg.Format(IDS_UPDATEAVAIL, (LPCWSTR)ver);

	std::wstring title(L10N::StringLoader::GetW(IDR_MAINFRAME));
	std::wstring downloadNow(L10N::StringLoader::GetW(IDS_DOWNLOADNOW));
	std::wstring remindLater(L10N::StringLoader::GetW(IDS_REMINDMELATER));
	std::wstring never(L10N::StringLoader::GetW(IDS_DONOTDOWNLOAD));

	TASKDIALOG_BUTTON buttons[3] = {
		{ IDS_DOWNLOADNOW, downloadNow.c_str() },
		{ IDS_REMINDMELATER, remindLater.c_str() },
		{ IDS_DONOTDOWNLOAD, never.c_str() }};

	TASKDIALOGCONFIG cfg = { 0 };
	cfg.cbSize = sizeof(cfg);
	cfg.hwndParent = m_hWnd;
	cfg.hInstance = _Module.GetResourceInstance();
	cfg.pszWindowTitle = title.c_str();
	cfg.pszMainIcon = MAKEINTRESOURCEW(TDT_INFORMATION_ICON);
	cfg.pszContent = (LPCWSTR)msg;
	cfg.dwCommonButtons = 0;
	cfg.pButtons = buttons;
	cfg.cButtons = 3;
	cfg.nDefaultButton = IDS_DOWNLOADNOW;

	switch(PNTaskDialogIndirect(&cfg))
	{
		// Launch the update...
	case IDS_DOWNLOADNOW:
		{
			::ShellExecute(m_hWnd, _T("open"), details->UpdateUrl.c_str(), NULL, NULL, SW_SHOW);
		}
		break;

	case IDS_REMINDMELATER:
		{
			// Do nothing, we'll try again next time...
		}
		break;

	case IDS_DONOTDOWNLOAD:
		{
			Updates::SetLastOfferedVersion(*details);
		}
		break;
	}
}

void CMainFrame::resetCurrentDir(bool rememberOpenPath)
{
	if (OPTIONS->Get(PNSK_GENERAL, _T("ResetCurrentDir"), true))
	{
		// Store the directory we opened into, and then set current back to PN
		if (rememberOpenPath)
		{
			TCHAR curPath[MAX_PATH+1];
			::GetCurrentDirectory(MAX_PATH+1, curPath);
			m_lastOpenPath = curPath;
		}

		::SetCurrentDirectory(OPTIONS->GetPNPath(PNPATH_PN));
	}
}

LRESULT CMainFrame::OnStatusBarDblClick(int /*wParam*/, LPNMHDR lParam, BOOL& bHandled)
{
    bHandled = FALSE;
    CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor());
	
	if(pChild)
	{
		LPNMMOUSE pnmm = (LPNMMOUSE)lParam;

		PNASSERT(static_cast<int>(pnmm->dwItemSpec) < m_StatusBar.m_nPanes);
		UINT nID = m_StatusBar.m_pPane[pnmm->dwItemSpec];

		switch (nID)
		{
			case ID_POS_PANE:
				SendMessage(m_hWnd, WM_COMMAND, MAKELONG(ID_EDIT_GOTO, 1), 0);

				bHandled = TRUE;
				return TRUE;

			case ID_INS_PANE:
				pChild->GetTextView()->EditToggleOvertype();

				bHandled = TRUE;
				return TRUE;

			default:
				break;
		}
	}

    return FALSE;
}
