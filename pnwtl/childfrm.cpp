/**
 * @file ChildFrm.cpp
 * @brief Implementation of CChildFrame, the MDI Child window.
 * @author Simon Steele
 * @note Copyright (c) 2002-2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "childfrm.h"
#include "tools.h"
#include "toolsmanager.h"
#include "outputview.h"
#include "exporters.h"
#include "pndialogs.h"
#include "docprops.h"
#include "include/pagesetupdialog.h"
#include "jumpto.h"
#include "JumpToDialog.h"
#include "jumpview.h"
#include "afiles.h"
#include "scriptregistry.h"
#include "textclips.h"
#include "autocomplete.h"
#include "autocompletehandler.h"

#include "tabbingframework/TabbedMDISave.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

bool CChildFrame::s_bFirstChild = true;

#define USERLIST_TEXTCLIPS 1

CChildFrame::CChildFrame(DocumentPtr doc, CommandDispatch* commands, TextClips::TextClipsManager* textclips) : 
	m_spDocument(doc), 
	m_view(doc, commands),
	m_pCmdDispatch(commands),
	m_pTextClips(textclips),
	m_hWndOutput(NULL),
	m_hImgList(NULL),
	m_pSplitter(NULL),
	m_pOutputView(NULL),
	m_bClosing(false),
	m_pScript(NULL),
	m_FileAge(-1),
	m_iFirstToolCmd(ID_TOOLS_DUMMY),
	m_bModifiedOverride(false)
{
	m_po.hDevMode = 0;
	m_po.hDevNames = 0;
	memset(&m_po.rcMargins, 0, sizeof(RECT));
	OPTIONS->LoadPrintSettings(&m_po);

	InitUpdateUI();
}

CChildFrame::~CChildFrame()
{
	if( ToolOwner::HasInstance() )
	{
		// Can't afford to wait, no "completed" events will get through...
		// That isn't actually true any more, but we'll leave it as no wait for now.
		ToolOwner::GetInstance()->KillTools(false, this);
	}

	if(m_hImgList)
		::ImageList_Destroy(m_hImgList);

	if(m_pSplitter)
		delete m_pSplitter;

	if(m_pOutputView)
		delete m_pOutputView;

	if(m_pUIData)
		delete [] m_pUIData;
}

void CChildFrame::OnFinalMessage(HWND /*hWnd*/)
{
	delete this;
}

/**
 * We override UpdateLayout in order to call UpdateBarsPosition in this class,
 * instead of in CFrameWindowImplBase. This way we can automagically have a toolbar
 * at the bottom of the window. 
 */
void CChildFrame::UpdateLayout(BOOL bResizeBars)
{
	RECT rect;
	GetClientRect(&rect);

	// position bars and offset their dimensions
	UpdateBarsPosition(rect, bResizeBars);

	// resize client window
	if(m_pSplitter)
	{
		m_pSplitter->UpdateLayout(true);
		return;
	}

	if(m_hWndClient != NULL)
		::SetWindowPos(m_hWndClient, NULL, rect.left, rect.top,
			rect.right - rect.left, rect.bottom - rect.top,
			SWP_NOZORDER | SWP_NOACTIVATE);
}

void CChildFrame::UpdateBarsPosition(RECT& rect, BOOL bResizeBars)
{
	// resize toolbar
	if(m_hWndToolBar != NULL && ((DWORD)::GetWindowLong(m_hWndToolBar, GWL_STYLE) & WS_VISIBLE))
	{
		if(bResizeBars)
		{
			// Size of mini bar controlled here...
			::SetWindowPos(m_hWndToolBar, HWND_TOP, rect.left, rect.bottom - MINI_BAR_HEIGHT, rect.right-rect.left, MINI_BAR_HEIGHT, SWP_NOACTIVATE | SWP_NOZORDER);
		}
		RECT rectTB;
		::GetWindowRect(m_hWndToolBar, &rectTB);
		rect.bottom -= rectTB.bottom - rectTB.top;
	}

	// resize status bar
	if(m_hWndStatusBar != NULL && ((DWORD)::GetWindowLong(m_hWndStatusBar, GWL_STYLE) & WS_VISIBLE))
	{
		if(bResizeBars)
			::SendMessage(m_hWndStatusBar, WM_SIZE, 0, 0);
		RECT rectSB;
		::GetWindowRect(m_hWndStatusBar, &rectSB);
		rect.bottom -= rectSB.bottom - rectSB.top;
	}
}

TBBUTTON MINI_BAR_BUTTONS[5] = 
{
	{ 1, ID_EDITOR_COLOURISE, TBSTATE_ENABLED | TBSTATE_CHECKED, TBSTYLE_CHECK, 0, 0, 0 },
	{ 0, ID_EDITOR_WORDWRAP, TBSTATE_ENABLED, TBSTYLE_CHECK, 0, 0, 0 },
	{ 2, ID_EDITOR_LINENOS, TBSTATE_ENABLED, TBSTYLE_CHECK, 0, 0, 0 },
	{ 3, ID_EDITOR_WHITESPACE, TBSTATE_ENABLED, TBSTYLE_CHECK, 0, 0, 0 },
	{ 4, ID_EDITOR_EOLCHARS, TBSTATE_ENABLED, TBSTYLE_CHECK, 0, 0, 0 }
};

void CChildFrame::SetupToolbar()
{
	CToolBarCtrl toolbar;

	CImageList imglist;
	imglist.Create(IDB_EDITOR, 9, 2, RGB(255,0,255));
	m_hImgList = imglist.Detach();

	CRect rc;
	GetClientRect(rc);
	rc.top = rc.bottom - MINI_BAR_HEIGHT;

	// We fix the size of the mini toolbar to make it suitably small (see MINI_BAR_HEIGHT).
	DWORD dwStyle = WS_CHILD | WS_VISIBLE | /*WS_CLIPCHILDREN | WS_CLIPSIBLINGS |*/ CCS_BOTTOM | 
					CCS_NODIVIDER | TBSTYLE_TOOLTIPS | CCS_NORESIZE | TBSTYLE_FLAT;

	toolbar.Create(m_hWnd, rc, NULL, dwStyle);
	
	toolbar.SetBitmapSize(CSize(9,9));
	toolbar.SetButtonSize(CSize(16, 15));
	toolbar.SetImageList(m_hImgList);
	
	toolbar.AddButtons(5, &MINI_BAR_BUTTONS[0]);

	m_hWndToolBar = toolbar.Detach();
}

void CChildFrame::EnsureOutputWindow()
{
	if(!m_pSplitter)
	{
		m_pSplitter = new CCFSplitter(this);
		
		m_pSplitter->SetHorizontal( OPTIONS->Get(PNSK_EDITOR, _T("OutputSplitHorizontal"), true) );

		CRect rc;
		GetClientRect(rc);
		UpdateBarsPosition(rc, FALSE);

		CRect rc2(rc);
		rc2.top += (rc.Height() / 4) * 3;

		m_pOutputView = new COutputView;
		m_pOutputView->Create(m_hWnd, rc2, _T("Output"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_CLIPCHILDREN, WS_EX_CLIENTEDGE);
		m_hWndOutput = m_pOutputView->m_hWnd;

		m_pSplitter->Create(m_hWnd, rc, _T("Splitter"), 0, 0);
		m_pSplitter->SetPanes((HWND)m_view, m_pOutputView->m_hWnd);
		m_pSplitter->ProportionSplit();
		m_pSplitter->SetSinglePaneMode(SPLITTER_TOP);
	}
}

void CChildFrame::ToggleOutputWindow(bool bSetValue, bool bSetShowing)
{
	bool bShow;
	bool bVisible = ((m_pSplitter != NULL) ? m_pSplitter->GetSinglePaneMode() == SPLITTER_NORMAL : false);
	if(bSetValue)
		bShow = bSetShowing;
	else
		bShow = !bVisible;

	EnsureOutputWindow();

	if(bShow && !bVisible)
	{
		m_pSplitter->DisableSinglePaneMode();
	}
	else if(!bShow && bVisible)
	{
		m_pSplitter->SetSinglePaneMode(SPLITTER_TOP);
	}

	UISetChecked(ID_VIEW_INDIVIDUALOUTPUT, bShow);
}

////////////////////////////////////////////////////
// Autocomplete methods

bool CChildFrame::InsertClipCompleted(SCNotification* notification)
{
	tstring text = notification->text;
	int colon = text.find(':');
	text.resize(colon);

	const TextClips::TextClipSet* set = m_pTextClips->GetClips( m_view.GetCurrentScheme()->GetName() );
	if(set != NULL)
	{
		const TextClips::Clip* clip = set->FindByShortcut(text);
		if(clip != NULL)
		{
			m_view.BeginUndoAction();
			m_view.DelWordLeft();
			clip->Insert(&m_view);
			m_view.EndUndoAction();
		}
	}

	return true;
}

////////////////////////////////////////////////////
// Document Entries

DocumentPtr CChildFrame::GetDocument() const
{
	return m_spDocument;
}

void CChildFrame::SetTitle( bool bModified )
{
	tstring fn = m_spDocument->GetFileName();
	LPCTSTR sFullPath = fn.c_str();

	tstring filepart;
	
	tstring title;
	tstring tabTitle;

	if(_tcschr(sFullPath, _T('<')) != 0)
	{
		// no filename yet...
		title = tabTitle = _T("<new>");
	}
	else
	{
		if( _tcschr(sFullPath, _T('\\')) != NULL )
		{
			CFileName fn(sFullPath);
			filepart = fn.GetFileName();
		}
		else
			filepart = sFullPath;
		
		if( OPTIONS->GetCached(Options::OShowFullPath) )
		{
			title = sFullPath;
			tabTitle = filepart;
		}
		else
		{
			title = filepart;
			tabTitle = filepart;
		}

	}

	if(bModified)
	{
		title += " *";
		tabTitle += " *";
	}

	SetWindowText(title.c_str());
	SetTabText(tabTitle.c_str());
	SetTabToolTip(sFullPath);
	
	m_Title = sFullPath;

	MDIRefreshMenu();
}

tstring CChildFrame::GetFileName(EGFNType type)
{
	return m_spDocument->GetFileName(type);
}

LPCTSTR CChildFrame::GetTitle()
{
	return m_Title;
}

bool CChildFrame::GetModified()
{
	return m_view.GetModified() || m_bModifiedOverride;
}

bool CChildFrame::CanClose()
{
	bool bRet = true;

	if(GetModified())
	{
		CString title;
		title.Format(IDS_SAVE_CHANGES, GetTitle());
		int res = MessageBox(title, LS(IDR_MAINFRAME), MB_YESNOCANCEL | MB_ICONQUESTION);
		switch (res)
		{
			case IDYES:
			{
				if( CanSave() )
				{
					return Save(false);
				}
				else
				{
					return SaveAs(false);
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

void CChildFrame::LoadExternalLexers()
{
	HANDLE hFind;
	WIN32_FIND_DATA FindFileData;

	tstring sPath;
	OPTIONS->GetPNPath(sPath, PNPATH_SCHEMES);

	tstring sPattern(sPath);
	sPattern += "*.lexer";

	hFind = FindFirstFile(sPattern.c_str(), &FindFileData);
	if (hFind != INVALID_HANDLE_VALUE)
	{
		//Found the first file...
		BOOL found = TRUE;
		tstring to_open;

		while (found) 
		{
			to_open = sPath;
			to_open += FindFileData.cFileName;
			m_view.SPerform(SCI_LOADLEXERLIBRARY, 0, reinterpret_cast<LPARAM>( to_open.c_str()));
			found = FindNextFile(hFind, &FindFileData);
		}

		FindClose(hFind);
	}
}


////////////////////////////////////////////////////
// Message Handlers

LRESULT CChildFrame::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	m_hWndClient = m_view.Create(m_hWnd, rcDefault, NULL, WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_CLIPCHILDREN, WS_EX_CLIENTEDGE);

	if(s_bFirstChild)
	{
		LoadExternalLexers();
		s_bFirstChild = false;
	}

	SetTitle();
	SetupToolbar();
	m_pCmdDispatch->UpdateMenuShortcuts(m_hMenu);

	UISetChecked(ID_EDITOR_COLOURISE, true);
	UISetChecked(ID_EDITOR_WORDWRAP, OPTIONS->GetCached(Options::OWordWrap) != 0);
	UISetChecked(ID_EDITOR_LINENOS, OPTIONS->GetCached(Options::OLineNumbers) != 0);
	UISetChecked(ID_TOOLS_LECONVERT, true);

	m_view.ShowLineNumbers(OPTIONS->GetCached(Options::OLineNumbers) != 0);

	UpdateMenu();

	bHandled = FALSE;
	return 1;
}

LRESULT CChildFrame::OnMDIActivate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
{
	if (m_hWnd==(HWND)lParam)
	{
		// Activate
		::PostMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, (WPARAM)JUMPVIEW_FILE_ACTIVATE, (LPARAM)this);
	
	}
	//else // Deactivate
	
	UpdateMenu();
	::PostMessage(m_hWnd, PN_CHECKAGE, 0, 0);
	bHandled = FALSE;
	return 0;
}

LRESULT CChildFrame::OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
{
	bHandled = FALSE;

	// PNID_DONTASKUSER is a special value that means "don't ask the user first". Normally for
	// use if we've already asked the user.
	if((lParam != PNID_DONTASKUSER) && !CanClose())
	{
		bHandled = TRUE;
	}
	else
	{
		handleClose();
	}

	return 0;
}

LRESULT CChildFrame::OnCloseNoPrompt(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = FALSE;

	handleClose();

	return 0;
}

LRESULT CChildFrame::OnPaint(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	// Paint a background for the toolbar.
	PAINTSTRUCT ps;
	CDC cdc(BeginPaint(&ps));
	
	CRect rectTB;
	::GetWindowRect(m_hWndToolBar, &rectTB);
	CBrush brush;
	brush.CreateSysColorBrush(COLOR_3DFACE);

	cdc.FillRect(rectTB, brush);

	EndPaint(&ps);

	return 0;		
}

LRESULT CChildFrame::OnEraseBackground(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	// Need to do this so the toolbar is drawn...
	return 0;
}

LRESULT CChildFrame::OnForwardMsg(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	LPMSG pMsg = reinterpret_cast<LPMSG>(lParam);

	if(CTabbedMDIChildWindowImpl<CChildFrame>::PreTranslateMessage(pMsg))
		return TRUE;

	if(pMsg->message == WM_KEYDOWN && pMsg->wParam == VK_ESCAPE && GetFocus() == m_view.m_hWnd)
		if(OnEscapePressed())
			return TRUE;

	return m_view.PreTranslateMessage(pMsg);
}

LRESULT CChildFrame::OnCheckAge(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	if(!m_bClosing)
		CheckAge();

	return 0;
}

LRESULT CChildFrame::OnViewNotify(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	if(lParam == SCN_SAVEPOINTREACHED || lParam == SCN_SAVEPOINTLEFT)
	{
		SetTitle(GetModified());
		m_spDocument->OnModifiedChanged(GetModified());
	}
	else
	{
		SendMessage(GetMDIFrame(), PN_NOTIFY, wParam, lParam);
	}

	return TRUE;
}

LRESULT CChildFrame::OnOptionsUpdate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	Scheme* pS = m_view.GetCurrentScheme();
	UpdateTools(pS);

	// re-load the compiled scheme...
	if(pS)
		m_view.SetScheme(pS);

	m_view.ShowLineNumbers(OPTIONS->GetCached(Options::OLineNumbers) != 0);

	UpdateMenu();

	SetTitle(GetModified());

	m_pCmdDispatch->UpdateMenuShortcuts(m_hMenu);

	return 0;
}

LRESULT CChildFrame::OnToggleOutput(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	ToggleOutputWindow(wParam != 0, lParam != 0);
	return 0;
}

LRESULT CChildFrame::OnToolFinished(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	UpdateMenu();

	return 0;
}

LRESULT CChildFrame::OnSchemeChanged(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	SchemeChanged(reinterpret_cast<Scheme*>(lParam));
	::SendMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, (WPARAM)JUMPVIEW_FILE_CLOSE, (LPARAM)this);
	::PostMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, (WPARAM)JUMPVIEW_FILE_ADD, (LPARAM)this);
	return 0;
}

LRESULT CChildFrame::OnChildIsModified(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	if( GetModified() )
	{
		ITabbedMDIChildModifiedItem* pMI = (ITabbedMDIChildModifiedItem*)lParam;
		
		USES_CONVERSION;
		
		std::wstring wstr = T2CW(GetFileName(FN_FILE).c_str());
		pMI->put_DisplayName( wstr.c_str() );
		
		if( CanSave() )
		{
			wstr = T2CW( GetFileName().c_str() );
			pMI->put_Description( wstr.c_str() );
		}
		else
		{
			wstr = L"New File: ";
			wstr += T2CW( (LPCTSTR)m_view.GetCurrentScheme()->GetTitle() );
			pMI->put_Description( wstr.c_str() );
		}

		pMI->put_Window( m_hWnd );

		HICON icon = ::LoadIcon(_Module.m_hInst, MAKEINTRESOURCE(IDR_MDICHILD));
		pMI->put_Icon( icon );

		return TRUE;
	}
	else
		return FALSE;
}

LRESULT CChildFrame::OnChildSaveModified(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	if( CanSave() )
	{
		Save(true); 
		return 0;
	}
	else
	{
		// If SaveAs succeeds, we return 0 - we can close. Else return -1, user cancelled.
		return SaveAs(true) ? 0 : -1;
	}
}

LRESULT CChildFrame::OnShowTabContextMenu(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
{
	bHandled = TRUE;

	POINT point = {GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam)};

	CSPopupMenu popup(IDR_POPUP_TABS);
	g_Context.m_frame->TrackPopupMenu(popup, 0, point.x, point.y, NULL, g_Context.m_frame->GetWindow()->m_hWnd);

	return 0;
}

////////////////////////////////////////////////////
// Command Handlers

LRESULT CChildFrame::OnPrint(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_po.Filename = m_spDocument->GetFileName(FN_FULL).c_str();
	m_view.PrintDocument(&m_po, true);

	return TRUE;
}

LRESULT CChildFrame::OnPrintSetup(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	PrintSetup();

	return TRUE;
}

LRESULT CChildFrame::OnOpenContainingFolder(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(CanSave())
	{
		CFileName fn(GetFileName().c_str());
		::ShellExecute(m_hWnd, NULL, fn.GetPath().c_str(), NULL, NULL, SW_SHOWNORMAL);
	}

	return 0;
}

LRESULT CChildFrame::OnShellOpen(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(CanSave())
	{
		::ShellExecute(m_hWnd, _T("open"), GetFileName().c_str(), NULL, NULL, SW_SHOWNORMAL);
	}

	return 0;
}

LRESULT CChildFrame::OnExportRTF(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Export(ExporterFactory::RTF);

	return 0;
}

LRESULT CChildFrame::OnExportHTML(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Export(ExporterFactory::HTML);

	return 0;
}

LRESULT CChildFrame::OnRedo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_view.Redo();
	return TRUE;
}

LRESULT CChildFrame::OnDelete(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_view.DeleteBack();
	return TRUE;
}

LRESULT CChildFrame::OnFindNext(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	SearchOptions* pOptions = reinterpret_cast<SearchOptions*>( OPTIONS->GetSearchOptions() );
	if( pOptions->FindText != _T("") )
	{
		if( !/*m_view.*/FindNext(pOptions) )
		{
			CString cs;
			cs.Format(IDS_FINDNOTFOUND, pOptions->FindText);
			MessageBox(cs, LS(IDR_MAINFRAME), MB_OK);
		}
	}
	return TRUE;
}

LRESULT CChildFrame::OnFindPrevious(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	SearchOptions* pOptions = reinterpret_cast<SearchOptions*>( OPTIONS->GetSearchOptions() );
	if( pOptions->FindText != _T("") )
	{
		pOptions->Direction = !pOptions->Direction;

		if( !/*m_view.*/FindNext(pOptions) )
		{
			CString cs;
			cs.Format(IDS_FINDNOTFOUND, pOptions->FindText);
			MessageBox(cs, LS(IDR_MAINFRAME), MB_OK);
		}

		pOptions->Direction = !pOptions->Direction;
	}
	
	return TRUE;
}

LRESULT CChildFrame::OnCopyRTF(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int selectionLength = m_view.GetSelLength();
	int rtfStringLength;
	//If nothing is selected, copy entire file	
	if(selectionLength == 0)
		rtfStringLength = m_view.GetTextLength() * 2;
	else
		rtfStringLength = m_view.GetSelLength() * 2;

	StringOutput so(rtfStringLength);
	StylesList* pStyles = m_view.GetCurrentScheme()->CreateStylesList();
	RTFExporter rtf(&so, m_view.GetCurrentScheme()->GetName(), pStyles, &m_view);
	//If nothing is selected, copy entire file
	if(selectionLength == 0) 
		rtf.Export(0,m_view.GetTextLength());
	else
		rtf.Export(m_view.GetSelectionStart(), m_view.GetSelectionEnd());
	delete pStyles;
	
	const char* pRTF = so.c_str();
	int len = strlen(pRTF) + 1;

	HGLOBAL hData = ::GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT, len);
	if( hData )
	{
		if( OpenClipboard() )
		{
			EmptyClipboard();
			char* pBuf = static_cast<char*>(::GlobalLock(hData));
			memcpy(pBuf, pRTF, len);
			::GlobalUnlock(hData);
			::SetClipboardData(::RegisterClipboardFormat(CF_RTF), hData);
			CloseClipboard();
		}
	}

	return 0;
}

LRESULT CChildFrame::OnClipboardSwap(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	TextRange tr;

	tr.chrg.cpMin = m_view.GetSelectionStart();
	tr.chrg.cpMax = m_view.GetSelectionEnd();
	if( tr.chrg.cpMax < tr.chrg.cpMin )
		tr.chrg.cpMax = m_view.GetLength();
	int length = tr.chrg.cpMax - tr.chrg.cpMin;
	
	char* pNewBuf = new char[length + 1];
	pNewBuf[length] = '\0';
	tr.lpstrText = pNewBuf;
				
	m_view.GetTextRange(&tr);

	m_view.Paste();

	HGLOBAL hData = ::GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT, length+1);
	if( hData )
	{
		if( OpenClipboard() )
		{
			EmptyClipboard();
			char* pBuf = static_cast<char*>(::GlobalLock(hData));
			memcpy(pBuf, pNewBuf, length + 1);
			::GlobalUnlock(hData);
			::SetClipboardData(CF_TEXT, hData);
			CloseClipboard();
		}
	}

	delete [] pNewBuf;

	return 0;
}

LRESULT CChildFrame::OnDuplicateLine(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_view.LineDuplicate();
	return 0;
}

LRESULT CChildFrame::OnDeleteLine(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_view.LineDelete();
	return 0;
}

LRESULT CChildFrame::OnCutLine(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_view.LineCut();
	return 0;
}

LRESULT CChildFrame::OnCopyLine(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_view.LineCopy();
	return 0;
}

LRESULT CChildFrame::OnTransposeLines(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_view.LineTranspose();
	return 0;
}

LRESULT CChildFrame::OnLowerCase(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_view.LowerCase();
	return 0;
}

LRESULT CChildFrame::OnUpperCase(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_view.UpperCase();
	return 0;
}

LRESULT CChildFrame::OnAutoComplete(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_view.AttemptAutoComplete();
	return 0;
}

LRESULT CChildFrame::OnCopyFilePath(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(::OpenClipboard(m_hWnd))
	{
		::EmptyClipboard();

		tstring fn = GetFileName();

		HGLOBAL mem = ::GlobalAlloc(GMEM_MOVEABLE, (fn.length() + 1) * sizeof(TCHAR));
		if(mem == NULL)
		{
			::CloseClipboard();
			return 0;
		}

		TCHAR* strbuf = static_cast<TCHAR*>( ::GlobalLock(mem) );
		_tcscpy(strbuf, fn.c_str());
		::GlobalUnlock(mem);

		::SetClipboardData(CF_TEXT, mem);

		::CloseClipboard();
	}

	return 0;
}

LRESULT CChildFrame::OnInsertClip(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	tstring word = m_view.GetCurrentWord();	

	const TextClips::TextClipSet* clips = m_pTextClips->GetClips(m_view.GetCurrentScheme()->GetName());
	
	if(clips == NULL)
	{
		return 0;
	}

	const TextClips::Clip* desired = clips->FindByShortcut(word);
	if(desired != NULL)
	{
		m_view.BeginUndoAction();
		m_view.DelWordLeft();
		desired->Insert(&m_view);
		m_view.EndUndoAction();
	}
	else
	{
		// Now we want to autocomplete a list of clips:
		AutoCompleteHandlerPtr p(new AutoCompleteAdaptor<CChildFrame>(this, &CChildFrame::InsertClipCompleted));
		m_view.SetAutoCompleteHandler(p);

		tstring cliptext = clips->BuildSortedClipList();
		int sep = m_view.AutoCGetSeparator();
		m_view.AutoCSetSeparator(',');
		m_view.AutoCShow(word.size(), cliptext.c_str());
		m_view.AutoCSetSeparator(sep);
	}

	return 0;
}

LRESULT CChildFrame::OnJumpTo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CJumpToDialog dlg(this);
	if(dlg.DoModal() == IDOK)
	{
		PostMessage(PN_GOTOLINE, 0, dlg.GetLine());
	}

	return 0;
}

LRESULT CChildFrame::OnRevert(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Revert();

	return 0;
}

LRESULT CChildFrame::OnSave(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Save(true);

	return 0;
}

LRESULT CChildFrame::OnSaveAs(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	SaveAs(true);

	return 0;
}

LRESULT CChildFrame::OnClose(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	PostMessage(WM_CLOSE, 0, 0);
	return 0;
}


LRESULT CChildFrame::OnWordWrapToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_view.GetWrapMode() == SC_WRAP_WORD)
		m_view.SetWrapMode( SC_WRAP_NONE );
	else
		m_view.SetWrapMode( SC_WRAP_WORD );
	//m_view.SetWrapMode( UIInvertCheck(wID) ? SC_WRAP_WORD : SC_WRAP_NONE );
	UpdateMenu();

	return 0;
}

LRESULT CChildFrame::OnColouriseToggle(WORD /*wNotifyCode*/, WORD wID, HWND hWndCtl, BOOL& /*bHandled*/)
{
	m_view.EnableHighlighting(UIInvertCheck(wID));

	UpdateMenu();
	
	return 0;
}

LRESULT CChildFrame::OnLineNoToggle(WORD /*wNotifyCode*/, WORD wID, HWND hWndCtl, BOOL& /*bHandled*/)
{
	m_view.ShowLineNumbers(UIInvertCheck(wID));

	return 0;
}

LRESULT CChildFrame::OnIndividualOutputToggle(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	ToggleOutputWindow();

	return 0;
}

LRESULT CChildFrame::OnMarkWhiteSpaceToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	bool bShow = UIInvertCheck(wID);
	
	m_view.SetViewWS((bShow ? SCWS_VISIBLEALWAYS : SCWS_INVISIBLE));

	return 0;
}

LRESULT CChildFrame::OnEOLMarkerToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_view.SetViewEOL(UIInvertCheck(wID));

	return 0;
}

LRESULT CChildFrame::OnUseAsScript(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pScript != NULL)
		return 0;

	tstring runner;
	if(ScriptRegistry::GetInstanceRef().SchemeScriptsEnabled(m_view.GetCurrentScheme()->GetName(), runner))
	{
		m_pScript = new DocScript(GetFileName(FN_FILE).c_str(), runner.c_str(), m_spDocument);
		ScriptRegistry::GetInstance()->Add("User Scripts", m_pScript);
	}

	return 0;
}

LRESULT CChildFrame::OnHideOutput(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	ToggleOutputWindow(true, false);
	return 0;
}

LRESULT CChildFrame::OnGoto(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CString caption;
	caption.Format(_T("&Line Number (1 - %d):"), m_view.GetLineCount());
	CGotoDialog g(caption);
	if(g.DoModal() == IDOK)
	{
		m_view.GotoLine(g.GetLineNo()-1);
	}

	return 0;
}

/**
 * Provide auto-complete handling combined with Definitions
 * to perform Go To Definition when multiple definitions exist
 */
class DefinitionInsertHandler : 
	public Definitions, 
	public BaseAutoCompleteHandler
{
public:
	/// We're deleted as BaseAutoCompleteHandler*
	virtual ~DefinitionInsertHandler(){}

	/// User made a selection
	virtual bool AutoCSelection(SCNotification* notification)
	{
		for (size_t i = 0; i < Prototypes.size(); ++i)
		{
			if (Prototypes[i] == notification->text)
			{
				Windows[i]->GetTextView()->GotoLine(Lines[i] - 1); //lines based on "0"
				break;
			}
		}

		return true;
	}
};

LRESULT CChildFrame::OnGoToDef(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{    
    // Set up buffer for selected keyword:
	tstring sel = GetTextView()->GetCurrentWord(); //m_view.GetSelText2();
	if (sel.size() == 0)
	{
		return 0;
	}
	
	// Setup stuff for getting all possible definitions:
	boost::shared_ptr<BaseAutoCompleteHandler> insertionHandler(new DefinitionInsertHandler);
	Definitions* defs = static_cast<DefinitionInsertHandler*>(insertionHandler.get());
	defs->SearchTerm = sel;
	
	::SendMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, JUMPVIEW_FIND_DEFINITIONS, reinterpret_cast<LPARAM>(defs));
	
    // Handle definitions: 0: not found, 1: go to source, 2: let user choose.
	if (defs->Lines.size() == 0)
    {
		CString Msg;
		Msg.Format("Definition of '%s' not found", sel.c_str());
		g_Context.m_frame->SetStatusText(Msg);

        return 0;
    }
	else if (defs->Lines.size() == 1)
    {
		defs->Windows[0]->GetTextView()->GotoLine(defs->Lines[0] - 1); //lines based on "0"
    }
    else
    {
		m_view.SetAutoCompleteHandler(insertionHandler);

		std::string autoclist;
		for (std::vector<std::string>::const_iterator i = defs->Prototypes.begin();
			i != defs->Prototypes.end();
			++i)
        {
			autoclist += (*i);
			autoclist += '|';
        }

		if (!autoclist.size())
		{
			return 0;
		}

		// remove stray end separator
		autoclist.resize(autoclist.size()-1);

		int sep = m_view.AutoCGetSeparator();
		m_view.AutoCSetSeparator('|');
		m_view.UserListShow(0, autoclist.c_str());
		m_view.AutoCSetSeparator(sep);
    }

    return 0;
}

LRESULT CChildFrame::OnGotoLine(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam/**/, BOOL& /*bHandled*/)
{
	int line= (int)lParam-1;
	m_view.GotoLine(line);
	m_view.EnsureVisibleEnforcePolicy(line);

	int offset = m_view.GetFirstVisibleLine();
	
	// Put the line we jump to two off the top of the screen...
	offset = (line - offset) - 2;
	m_view.LineScroll(0, offset);
	::SetFocus(m_hWnd);
	return 0;
}

LRESULT CChildFrame::OnLineEndingsToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(wID == ID_TOOLS_LECRLF)
		m_view.SetEOLMode((int)PNSF_Windows);
	else if(wID == ID_TOOLS_LELF)
		m_view.SetEOLMode((int)PNSF_Unix);
	else
		m_view.SetEOLMode((int)PNSF_Mac);
	
	if(UIGetChecked(ID_TOOLS_LECONVERT))
		m_view.ConvertEOLs(m_view.GetEOLMode());

	UpdateMenu();
	return 0;
}

LRESULT CChildFrame::OnLineEndingsConvert(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	UIInvertCheck(ID_TOOLS_LECONVERT);	
	
	return 0;
}

LRESULT CChildFrame::OnStopTools(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	// Don't need to wait, we'll assume the user is still using the document.
	ToolOwner::GetInstance()->KillTools(false, this); 

	return 0;
}

LRESULT CChildFrame::OnUseTabs(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_view.SetUseTabs(!m_view.GetUseTabs());
	UpdateMenu();

	return 0;
}

LRESULT CChildFrame::OnConvertTabsToSpaces(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& bHandled)
{
	SearchOptions options;
	
	m_view.BeginUndoAction();

	// TODO: The options dialog should not allow a tab width of 0
	assert( m_view.GetTabWidth() > 0 );

	std::string replaceText = "\\1";
	for ( int i = 0; i < m_view.GetTabWidth(); i++ )
		replaceText += " ";
	
	// Find all leading groups of spaces, convert to tabs
	options.SetFindText("^\\( *\\)\t");
	options.SetReplaceText(replaceText.c_str());

	options.SetIncludeHidden(true);
	options.SetLoopOK(true);
	options.SetMatchCase(false);
	options.SetMatchWholeWord(false);
	options.SetRecurse(true);
	options.SetReplaceInSelection( m_view.GetSelLength() != 0 );
	options.SetSearchBackwards(false);
	options.SetSearchPath("");
	options.SetUseRegExp(true);
	options.SetUseSlashes(false);
	options.SetNoCursorMove(true);
	
	// Repeat until we've replaced all occurances
	while ( ReplaceAll( &options ) != 0 );

	m_view.EndUndoAction();

	return 0;
}

LRESULT CChildFrame::OnConvertSpacesToTabs(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& bHandled)
{
	SearchOptions options;

	m_view.BeginUndoAction();

	// TODO: The options dialog should not allow a tab width of 0
	assert( m_view.GetTabWidth() > 0 );

	tstring findText = "^\\(\\t*\\)";
	for ( int i = 0; i < m_view.GetTabWidth(); i++ )
		findText += " ";
	
	// Find all leading groups of spaces, convert to tabs
	options.SetFindText(findText.c_str());
	options.SetReplaceText("\\1\\t");

	options.SetIncludeHidden(true);
	options.SetLoopOK(true);
	options.SetMatchCase(false);
	options.SetMatchWholeWord(false);
	options.SetRecurse(true);
	options.SetReplaceInSelection( m_view.GetSelLength() != 0 );
	options.SetSearchBackwards(false);
	options.SetSearchPath("");
	options.SetUseRegExp(true);
	options.SetUseSlashes(false);
	options.SetNoCursorMove(true);

	// Repeat until we've replaced all occurances
	while ( ReplaceAll( &options ) != 0 );

	m_view.EndUndoAction();

	return 0;
}

LRESULT CChildFrame::OnHeaderSwitch(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& bHandled)
{
	// Check there's a valid filename...
	if(CanSave())
	{
		tstring filename = m_spDocument->GetFileName();
		tstring alternateFile;

		AlternateFiles* afiles = AlternateFiles::GetInstance();
		
		if(	afiles->GetAlternate(filename.c_str(), alternateFile) )
		{
				if( !g_Context.m_frame->CheckAlreadyOpen(alternateFile.c_str(), eSwitch) )
					g_Context.m_frame->Open(alternateFile.c_str());
		}
		else
			g_Context.m_frame->SetStatusText(_T("No alternate file found."));
	}

	return 0;
}

LRESULT CChildFrame::OnEncodingSelect(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	EPNEncoding oldEncoding = m_view.GetEncoding();
	EPNEncoding encoding = (EPNEncoding)(wID - ID_ENCODING_8);
	
	if(oldEncoding != encoding)
	{
		m_view.SetEncoding(encoding);

		SetModifiedOverride(true);

		UpdateMenu();
	}

	return 0;
}

LRESULT CChildFrame::OnViewFileProps(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	tstring fn = GetFileName(FN_FILE).c_str();
	CPropertySheet sheet( fn.c_str(), 0, m_hWnd );
	sheet.m_psh.dwFlags |= (PSH_NOAPPLYNOW | PSH_PROPTITLE | PSH_USEICONID);
	sheet.m_psh.pszIcon = MAKEINTRESOURCE(IDR_MDICHILD);
	DocumentPropSheet docPropPage(this, _T("Properties"));

	sheet.AddPage(docPropPage);
	if(sheet.DoModal() == IDOK)
	{
		UpdateMenu();

		if(docPropPage.ModifiedDocument())
			SetModifiedOverride(true);
	}

	return 0;
}

LRESULT CChildFrame::OnProjectAddFile(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int a = 0;
	return 0;
}

class ChildOutputWrapper : public ToolWrapperT<CChildFrame, COutputView>
{
typedef ToolWrapperT<CChildFrame, COutputView> baseClass;
public:
	ChildOutputWrapper(CChildFrame* pOwner, COutputView* pView, CChildFrame* pActiveChild, const ToolDefinition& definition)
		: baseClass(pOwner, pView, pActiveChild, definition)
	{}
};

class TextFilterSink
{
public:
	void AddToolOutput(LPCTSTR output, int nLength = -1)
	{
		if(nLength == -1)
			buffer.append(output);
		else
			buffer.append(output, output+nLength);
	}

	void SetToolBasePath(LPCTSTR path)
	{
	}

	void SetToolParser(bool bBuiltIn, LPCTSTR customExpression = NULL)
	{
	}

	void ClearOutput()
	{
	}

	const char* GetBuffer() const
	{
		return buffer.c_str();
	}

private:
	std::string buffer;
};

class ChildTextFilterWrapper : public ToolWrapperT<CChildFrame, TextFilterSink>
{
	typedef ToolWrapperT<CChildFrame, TextFilterSink> baseClass;
	
public:
	ChildTextFilterWrapper(CChildFrame* pOwner, TextFilterSink* pSink, CChildFrame* pActiveChild, const ToolDefinition& definition)
		: baseClass(pOwner, pSink, pActiveChild, definition)
	{
		m_hFinishedEvent = ::CreateEvent(NULL, FALSE, FALSE, NULL);
	}

	virtual ~ChildTextFilterWrapper()
	{
		::CloseHandle(m_hFinishedEvent);
	}

	HANDLE GetFinishEventHandle() const
	{
		return m_hFinishedEvent;
	}

	virtual void ShowOutputWindow()
	{
	}

	virtual void OnFinished()
	{
		::SetEvent(m_hFinishedEvent);

		baseClass::OnFinished();
	}

private:
	HANDLE m_hFinishedEvent;
};

bool CChildFrame::OnRunTool(LPVOID pTool)
{
	ToolDefinition* pToolDef = static_cast<ToolDefinition*>(pTool);

	ToolWrapperPtr pWrapper;
	boost::shared_ptr<TextFilterSink> filter_sink;
	if(	pToolDef->GlobalOutput() )
	{
		pWrapper.reset( g_Context.m_frame->MakeGlobalOutputWrapper(pToolDef) );
	}
	else if( pToolDef->IsTextFilter() )
	{
		filter_sink.reset(new TextFilterSink());
		pWrapper.reset( new ChildTextFilterWrapper(this, filter_sink.get(), this, *pToolDef) );
	}
	else
	{
		if(pToolDef->CaptureOutput())
			EnsureOutputWindow();
		pWrapper.reset( new ChildOutputWrapper(this, m_pOutputView, this, *pToolDef) );
	}

	pWrapper->SetNotifyWindow(m_hWnd);

	if( pToolDef->WantStdIn() )
	{
		TextRange tr;

		// We want to pass our selection/whole document to StdIn.
		if(m_view.GetSelLength() > 0)
		{
			// Selection...
			tr.chrg.cpMin = m_view.GetSelectionStart();
			tr.chrg.cpMax = m_view.GetSelectionEnd();
		}
		else
		{
			// Whole Document
			tr.chrg.cpMin = 0;
			tr.chrg.cpMax = m_view.GetLength();
		}

		int buflength = (tr.chrg.cpMax - tr.chrg.cpMin) + 1;
		if(buflength)
		{
			tr.lpstrText = (char*)new unsigned char[buflength];
			m_view.GetTextRange(&tr);
			pWrapper->SetStdIOBuffer((unsigned char*)tr.lpstrText, buflength);
		}
	}

	ToolOwner::GetInstance()->RunTool(pWrapper, this);

	if( pToolDef->IsTextFilter() )
	{
		ChildTextFilterWrapper* pWaitWrapper = static_cast<ChildTextFilterWrapper*>(pWrapper.get());
		HANDLE hWait = pWaitWrapper->GetFinishEventHandle();
		bool bAbort = false;
		while(::WaitForSingleObject(hWait, 10000) == WAIT_TIMEOUT)
		{
			if(::MessageBox(m_hWnd, "This text filter is taking a long time to complete,\ndo you want to wait longer?", LS(IDR_MAINFRAME), MB_YESNO) == IDNO)
			{
				bAbort = true;
				break;
			}
		}

		if(!bAbort)
		{
			// We finished running...
			if(!filter_sink.get())
				RETURN_UNEXPECTED("Expected a filter_sink instance here!", false);

			m_view.BeginUndoAction();
			if(m_view.GetSelLength() == 0)
				m_view.ClearAll();
			
			m_view.ReplaceSel( filter_sink->GetBuffer() );
			m_view.EndUndoAction();
		}
	}

	return true;
}

////////////////////////////////////////////////////
// Notify Handlers

LRESULT CChildFrame::OnGetInfoTip(int idCtrl, LPNMHDR pnmh, BOOL& bHandled)
{
	LPNMTBGETINFOTIP pS = (LPNMTBGETINFOTIP)pnmh;

	::LoadString(_Module.m_hInst, pS->iItem, pS->pszText, pS->cchTextMax);

	return 0;
}

////////////////////////////////////////////////////
// File Management Methods

void CChildFrame::CheckAge()
{
	if(CanSave())
	{
		uint64_t age = m_spDocument->GetFileAge();
		if(age != m_FileAge)
		{
			if(m_spDocument->FileExists())
			{
				CString msg;
				msg.Format(IDS_MODIFIEDELSEWHERE, m_spDocument->GetFileName(FN_FULL).c_str());
				if (PNTaskDialog(m_hWnd, _T("File Changed"), _T(""), (LPCTSTR)msg, TDCBF_YES_BUTTON | TDCBF_NO_BUTTON, TDT_WARNING_ICON) == IDYES)
				{
					Revert();
					// Update tags:
					::SendMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, (WPARAM)JUMPVIEW_FILE_CLOSE, (LPARAM)this);
					//Manuel Sandoval: clear ScintillaImpl autocomplete list for adding new methods:
					this->GetTextView()->ResetAutoComplete();
					::PostMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, (WPARAM)JUMPVIEW_FILE_ADD, (LPARAM)this);
				}
				else
				{
					m_FileAge = age;
					SetModifiedOverride(true);
				}
			}
			else
			{
				CString msg;
				msg.Format(IDS_FILENOLONGEREXISTS, m_spDocument->GetFileName(FN_FULL).c_str());
				g_Context.m_frame->SetStatusText((LPCTSTR)msg);
				SetModifiedOverride(true);
			}
		}
	}
}

void CChildFrame::Revert()
{
	// Check that we have a valid filename first...
	if(CanSave())
	{
		m_view.Revert(m_spDocument->GetFileName(FN_FULL).c_str());
		m_FileAge = m_spDocument->GetFileAge();
		SetModifiedOverride(false);
	}
}

bool CChildFrame::PNOpenFile(LPCTSTR pathname, Scheme* pScheme, EPNEncoding encoding)
{
	bool bRet = false;

	if(m_view.Load(pathname, pScheme, encoding))
	{
		m_spDocument->SetFileName(pathname);
		m_FileAge = m_spDocument->GetFileAge();
		SetTitle();
		m_spDocument->OnAfterLoad();
		bRet = true;
	}
	else
	{
		HandleFailedFileOp(pathname, true);
	}
	
	// Loading a file may have changed the line endings/text encoding of the
	// document, so we update the menu...
	UpdateMenu();

	return bRet;
}

bool CChildFrame::SaveFile(LPCTSTR pathname, bool ctagsRefresh, bool bStoreFilename, bool bUpdateMRU)
{
	bool bSuccess = false;

	// If this is a user-relevant save, notify the extensions:
	if(bStoreFilename)
	{
		m_spDocument->OnBeforeSave(pathname);
	}

	// Do the save
	if(m_view.Save(pathname, bStoreFilename))
	{
		bSuccess = true;
	}
	else if (bStoreFilename)
	{
		// Only present UI if we're doing a user-visible save
		switch(HandleFailedFileOp(pathname, false))
		{
		case PNID_SAVEAS:
			bSuccess = SaveAs(ctagsRefresh);
			break;
		case PNID_OVERWRITE:
			if(attemptOverwrite(pathname))
			{
				if(m_view.Save(pathname, bStoreFilename))
				{
					bSuccess = true;
				}
				else
				{
					// If we thought we'd done the overwrite, but then failed to save again...
					CString s;
					s.Format(IDS_SAVEREADONLYFAIL, pathname);
					if(PNTaskDialog(m_hWnd, IDR_MAINFRAME, _T(""), (LPCTSTR)s, TDCBF_YES_BUTTON | TDCBF_NO_BUTTON | TDCBF_CANCEL_BUTTON, TDT_WARNING_ICON) == IDYES)
						bSuccess = SaveAs(ctagsRefresh);
				}
			}
			else
			{
				// If the attempt to overwrite failed.
				CString s;
				s.Format(IDS_SAVEREADONLYFAIL, pathname);
				if(PNTaskDialog(m_hWnd, IDR_MAINFRAME, _T(""), (LPCTSTR)s, TDCBF_YES_BUTTON | TDCBF_NO_BUTTON | TDCBF_CANCEL_BUTTON, TDT_WARNING_ICON) == IDYES)
					bSuccess = SaveAs(ctagsRefresh);
			}
			break;
		}
	}

	// Update state...
	if(bSuccess)
	{
		if(bStoreFilename)
		{
			// If bStoreFilename is *not* true it means we were saving the file for
			// some non-user purpose (saving a copy basically)

			m_FileAge = FileAge(pathname);
			SetModifiedOverride(false);
			m_spDocument->SetFileName(pathname);
			m_spDocument->OnAfterSave();

			SetTitle();
		}

		if(bUpdateMRU)
		{
			g_Context.m_frame->AddMRUEntry(m_spDocument->GetFileName(FN_FULL).c_str());
		}

		if(ctagsRefresh)
		{
			// Update tags:
			::SendMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, (WPARAM)JUMPVIEW_FILE_CLOSE, (LPARAM)this);
			//Manuel Sandoval: clear ScintillaImpl autocomplete list for adding new methods:
			this->GetTextView()->ResetAutoComplete();
			::PostMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, (WPARAM)JUMPVIEW_FILE_ADD, (LPARAM)this);
		}
	}

	return bSuccess;
}

bool CChildFrame::attemptOverwrite(LPCTSTR filename)
{
	DWORD dwFileAtts = ::GetFileAttributes(filename);
	if(dwFileAtts & FILE_ATTRIBUTE_READONLY)
	{
		dwFileAtts &= ~FILE_ATTRIBUTE_READONLY;
		::SetFileAttributes(filename, dwFileAtts);
		return true;
	}
	else return false;
}

void CChildFrame::handleClose()
{
	m_bClosing = true;

	// NO PostMessage !!
	::SendMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, (WPARAM)JUMPVIEW_FILE_CLOSE, (LPARAM)this);

	if( ToolOwner::HasInstance() )
	{
		ToolOwner::GetInstance()->KillTools(true, this);
	}

	if(m_pScript)
	{
		ScriptRegistry::GetInstance()->Remove("User Scripts", m_pScript);
		delete m_pScript;
		m_pScript = NULL;
	}

	m_spDocument->OnDocClosing();

	m_spDocument->SetValid(false);	
}

//TODO Move all the CFILE_ defines into the string resources.
#define PN_CouldNotSaveReadOnly _T("The file \"%s\" could not be saved because it is write-protected.\n\nYou can either save in a different location or PN can attempt to remove the protection and\n overwrite the file in its current location.")
/*const WTL::BXT::MBItem SaveReadOnlyButtons [] = {
	{PNID_SAVEAS, _T("Save &As...")},
	{PNID_OVERWRITE, _T("&Overwrite")},
	{IDCANCEL, 0}
};*/

const int SaveReadOnlyButtonsCount = 2;

TASKDIALOG_BUTTON SaveReadOnlyButtons[] = {
         { PNID_SAVEAS, L"Save &As..." },
         { PNID_OVERWRITE, L"&Overwrite" },
      };

int CChildFrame::HandleFailedFileOp(LPCSTR filename, bool bOpen)
{
	int err = GetLastError();
	
	TCHAR* fstr;

	int MBStyle = bOpen ? TDCBF_OK_BUTTON : TDCBF_CANCEL_BUTTON | TDCBF_YES_BUTTON | TDCBF_NO_BUTTON;
	TASKDIALOG_BUTTON* pItems = NULL;
	int nItems = 0;
	int nDefault = IDCANCEL;

	CFileName cfn(filename);
	tstring fn = cfn.GetFileName();
	
	switch(err)
	{
	case ERROR_ACCESS_DENIED:
		if (bOpen)
		{
			fstr = CFILE_LoadAccessDenied;
		}
		else
		{
			// Special Read-Only handling...
			fstr = PN_CouldNotSaveReadOnly; //IDS_SAVEREADONLY
			MBStyle = TDCBF_CANCEL_BUTTON;
			pItems = &SaveReadOnlyButtons[0];
			nItems = SaveReadOnlyButtonsCount;
			nDefault = PNID_SAVEAS;
		}
		break;

	case ERROR_NOT_DOS_DISK:
	case ERROR_WRITE_PROTECT:
		if (bOpen )
		{
			fstr = CFILE_LoadAccessDenied;
		}
		else
		{
			fstr = CFILE_SaveAccessDenied;
		}
		break;
		
	case ERROR_DISK_FULL:
	case ERROR_HANDLE_DISK_FULL:
		if (bOpen)
		{
			fstr = CFILE_CouldNotLoadError;
		}
		else
		{
			fstr = CFILE_SaveDiskFullError;
		}
		break;
		
	case ERROR_SHARING_VIOLATION:
	case ERROR_LOCK_VIOLATION:
		if (bOpen)
		{
			fstr = CFILE_LoadShareViolation;
		}
		else
		{
			fstr = CFILE_SaveShareViolation;
		}
		break;
		
	case ERROR_DEV_NOT_EXIST:
	case ERROR_BAD_NETPATH:
	case ERROR_NETWORK_BUSY:
		if (bOpen)
		{
			fstr = CFILE_NetLoadError;
		}
		else
		{
			fstr = CFILE_NetSaveError;
		}
		break;
		
	default:
		if (bOpen)
		{
			fstr = CFILE_CouldNotLoadError;
		}
		else
		{
			fstr = CFILE_CouldNotSaveError;
		}
	}

	int bs = _tcslen(fstr) + fn.length() + 10;
	std::string buffer;
	buffer.reserve(bs);
	_sntprintf(&buffer[0], bs, fstr, fn.c_str());

	USES_CONVERSION;

	CT2CW message(buffer.c_str());
	std::wstring title(LSW(IDR_MAINFRAME));

	//ret = AtlCustomMessageBoxNet(m_hWnd, (LPCTSTR)buffer, IDR_MAINFRAME, pItems, nItems, MBStyle);
	TASKDIALOGCONFIG cfg = { 0 };
	cfg.cbSize = sizeof(cfg);
	cfg.hInstance = _Module.GetResourceInstance();
	cfg.pszWindowTitle = title.c_str();
	cfg.pszMainIcon = MAKEINTRESOURCEW(TDT_WARNING_ICON);
	cfg.pszContent = message;
	cfg.dwCommonButtons = MBStyle;
	cfg.pButtons = pItems;
	cfg.cButtons = nItems;
	cfg.nDefaultButton = PNID_SAVEAS;

	int iRes = PNTaskDialogIndirect(&cfg);
	
	return iRes;
}

bool CChildFrame::CanSave()
{
	return m_spDocument->HasFile();
}

bool CChildFrame::SaveAs(bool ctagsRefresh)
{
	tstring saPath;
	if(CanSave())
	{
		saPath = m_spDocument->GetFileName(FN_FULL);
	}

	CPNSaveDialogEx dlgSave(_T("All Files (*.*)|*.*|"), saPath.c_str());
	bool bRet = true;

	if(dlgSave.DoModal() == IDOK)
	{
		EPNSaveFormat format = dlgSave.GetSaveFormat();
		if(format != PNSF_NoChange)
		{
			ChangeFormat(format);
		}
		if(dlgSave.m_ofn.lpstrFile == NULL)
			RETURN_UNEXPECTED(_T("SaveAs lpstrFile == NULL"), false);
		SaveFile(dlgSave.m_ofn.lpstrFile, ctagsRefresh);
	}
	else
	{
		bRet = false;
	}

	// We may have changed the line endings format, so update the menu.
	UpdateMenu();

	return bRet;
}

void CChildFrame::ChangeFormat(EPNSaveFormat format)
{
	m_view.SetEOLMode( (int)format );
	m_view.ConvertEOLs( (int)format );

	UpdateMenu();
}

bool CChildFrame::Save(bool ctagsRefresh)
{
	if(CanSave())
	{
		bool bResult = SaveFile(m_spDocument->GetFileName(FN_FULL).c_str(), ctagsRefresh, true);
		
		m_FileAge = m_spDocument->GetFileAge();
		SetModifiedOverride(false);

		return bResult;
	}
	else
		return SaveAs(ctagsRefresh);
}

////////////////////////////////////////////////////
// Editor Window Methods	

FindNextResult CChildFrame::FindNext(SearchOptions* options)
{
	FindNextResult result = (FindNextResult)m_view.FindNext(options);
	if( result == fnReachedStart )
	{
		CString msg;
		msg.LoadString(IDS_FINDLOOPED);
		MessageBox(msg,	LS(IDR_MAINFRAME), MB_OK | MB_ICONINFORMATION);
	}
	return result;
}

bool CChildFrame::Replace(SearchOptions* options)
{
	if(options->Found)
		return m_view.ReplaceOnce(options);
	else
	{
		m_view.FindNext(options);
		if(options->Found)
			return m_view.ReplaceOnce(options);
		else
			return false;
	}
}

int CChildFrame::ReplaceAll(SearchOptions* options)
{
	return m_view.ReplaceAll(options);
}

int CChildFrame::GetPosition(EGPType type)
{
	if(type == EP_LINE)
		return m_view.LineFromPosition(m_view.GetCurrentPos());
	else
		return m_view.GetColumn(m_view.GetCurrentPos());
}

void CChildFrame::SetPosStatus(CMultiPaneStatusBarCtrl&	stat)
{
	m_view.SetPosStatus(stat);
}

bool CChildFrame::OnSchemeChange(LPVOID pVoid)
{
	SetScheme(static_cast<Scheme*>(pVoid), false);

	return true;
}

void CChildFrame::SetScheme(Scheme* pScheme, bool allSettings)
{
    m_view.SetScheme(pScheme, allSettings);
}

#include "project.h"
#include "projectprops.h"

void CChildFrame::UpdateTools(Scheme* pScheme)
{
	CSMenuHandle menu(m_hMenu);
	CSMenuHandle tools( menu.GetSubMenu(3) );

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
	
	m_iFirstToolCmd = ToolsManager::GetInstance()->UpdateToolsMenu(
		tools, m_pCmdDispatch, m_iFirstToolCmd, ID_TOOLS_DUMMY, pScheme->GetName(), projid.size() > 0 ? projid.c_str() : NULL
	);
}

void CChildFrame::SchemeChanged(Scheme* pScheme)
{
	UpdateTools(pScheme);
	UpdateMenu();
	g_Context.m_frame->SetActiveScheme(m_hWnd, static_cast<LPVOID>(pScheme));

	m_spDocument->OnSchemeChange(pScheme->GetName());
	
	::PostMessage(GetMDIFrame(), PN_NOTIFY, 0, PN_SCHEMECHANGED);
}

void CChildFrame::UpdateMenu()
{
	CSMenuHandle menu(m_hMenu);

	EPNSaveFormat f = (EPNSaveFormat)m_view.GetEOLMode();
	
	menu.CheckMenuItem(ID_TOOLS_LECRLF, f == PNSF_Windows);
	menu.CheckMenuItem(ID_TOOLS_LECR, f == PNSF_Mac);
	menu.CheckMenuItem(ID_TOOLS_LELF, f == PNSF_Unix);
	menu.CheckMenuItem(ID_TOOLS_USETABS, m_view.GetUseTabs());

	EPNEncoding e = m_view.GetEncoding();

	menu.CheckMenuItem(ID_ENCODING_8, e == eUnknown);
	menu.CheckMenuItem(ID_ENCODING_UTF8, e == eUtf8);
	menu.CheckMenuItem(ID_ENCODING_UTF16BE, e == eUtf16BigEndian);
	menu.CheckMenuItem(ID_ENCODING_UTF16LE, e == eUtf16LittleEndian);
	menu.CheckMenuItem(ID_ENCODING_UTF8NOBOM, e == eUtf8NoBOM);

	UISetChecked(ID_EDITOR_WORDWRAP, m_view.GetWrapMode() == SC_WRAP_WORD);
	UISetChecked(ID_EDITOR_EOLCHARS, m_view.GetViewEOL());
	UISetChecked(ID_EDITOR_WHITESPACE, m_view.GetViewWS() == SCWS_VISIBLEALWAYS);
	UISetChecked(ID_EDITOR_LINENOS, m_view.GetMarginWidthN(0) > 0);
	
	bool bToolsRunning = false;
	if( ToolOwner::HasInstance() )
	{
		bToolsRunning = ToolOwner::GetInstance()->HaveRunningTools(this);
		bToolsRunning |= ToolOwner::GetInstance()->HaveRunningTools(g_Context.m_frame);
	}

	menu.EnableMenuItem(ID_TOOLS_STOPTOOLS, bToolsRunning);

	g_Context.m_frame->SetActiveScheme(m_hWnd, m_view.GetCurrentScheme());

	const CommentSpecRec& comments = m_view.GetCurrentScheme()->GetCommentSpec();
	menu.EnableMenuItem(ID_COMMENTS_LINE, comments.CommentBlockLine[0] != NULL);
	menu.EnableMenuItem(ID_COMMENTS_STREAM, (comments.CommentStreamStart[0] != NULL)
		&& (comments.CommentStreamEnd[0] != NULL));
	menu.EnableMenuItem(ID_COMMENTS_BLOCK, (comments.CommentBlockStart[0] != NULL)
		&& (comments.CommentBlockEnd[0] != NULL));

	menu.EnableMenuItem(ID_COMMENTS_UNCOMMENT, (comments.CommentBlockLine[0] != NULL)
		|| (comments.CommentStreamStart[0] != NULL)
		|| (comments.CommentStreamEnd[0] != NULL)
		|| (comments.CommentBlockStart[0] != NULL)
		|| (comments.CommentBlockEnd[0] != NULL));
}

bool CChildFrame::IsOutputVisible()
{
	return ((m_pSplitter != NULL) ? m_pSplitter->GetSinglePaneMode() == SPLITTER_NORMAL : false);
}

/**
 * In here we should kill off any windowy things showing like output windows etc.
 */
BOOL CChildFrame::OnEscapePressed()
{
	if(IsOutputVisible())
	{
        ToggleOutputWindow();
		return TRUE;
	}
	else
		return ::SendMessage(GetTopLevelParent(), PN_ESCAPEPRESSED, 0, 0);
}

/**
 * @brief Generic exporter function
 */
void CChildFrame::Export(int type)
{
	FileOutput fout(NULL);
	StylesList* pStyles = m_view.GetCurrentScheme()->CreateStylesList();
	BaseExporter* pExp = ExporterFactory::GetExporter(
		(ExporterFactory::EExporterType)type, 
		&fout, m_view.GetCurrentScheme()->GetName(), pStyles, &m_view);
	
	if(pExp)
	{
		tstring guessName;

		if(CanSave())
		{
			guessName = m_spDocument->GetFileName(FN_FILEPART);
		}
		else
		{
			guessName = _T("untitled");
		}	

		guessName += _T(".");
		guessName += pExp->GetDefaultExtension();

		tstring fileMask(pExp->GetFileMask());
		fileMask += _T("All Files (*.*)|*.*|");

		CPNFileDialog dlgSave(FALSE, pExp->GetDefaultExtension(), guessName.c_str(), 
			OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
			fileMask.c_str(), m_hWnd);
		
		if(dlgSave.DoModal() == IDOK)
		{
			fout.SetFileName(dlgSave.m_ofn.lpstrFile);
			if(fout.IsValid())
			{
				pExp->Export(0, -1);
			}
		}

		delete pExp;
	}

	delete pStyles;
}

void CChildFrame::SetModifiedOverride(bool bVal)
{
	m_bModifiedOverride = bVal;
	SetTitle(GetModified());
	g_Context.m_frame->GetWindow()->SendMessage(PN_NOTIFY, 0, SCN_UPDATEUI);
}

void CChildFrame::PrintSetup()
{
	SS::CPageSetupDialog psd(PSD_INWININIINTLMEASURE|PSD_ENABLEPAGESETUPTEMPLATE,
		m_hWnd);

	PAGESETUPDLG& pdlg = psd.m_psd;

	// Set margins if valid.
	if (m_po.rcMargins.left != 0 || m_po.rcMargins.right != 0 ||
			m_po.rcMargins.top != 0 || m_po.rcMargins.bottom != 0) 
	{
		psd.SetMargins(&m_po.rcMargins);
	}

	// retrieve cached options from other uses (can't be persisted between sessions)
	pdlg.hDevMode = m_po.hDevMode;
	pdlg.hDevNames = m_po.hDevNames;

	psd.SetHeaderText(m_po.Header);
	psd.SetFooterText(m_po.Footer);

	if ( psd.DoModal() != IDOK )
	{
		return;
	}

	// retrieve margins.
	psd.GetMargins(&m_po.rcMargins, NULL);

	// cache device mode options.
	m_po.hDevMode = pdlg.hDevMode;
	m_po.hDevNames = pdlg.hDevNames;

	m_po.Header = psd.GetHeaderText();
	m_po.Footer = psd.GetFooterText();

	OPTIONS->SavePrintSettings(&m_po);
}

CTextView* CChildFrame::GetTextView()
{
	return &m_view;
}

COutputView* CChildFrame::GetOutputWindow()
{
	EnsureOutputWindow();
	return m_pOutputView;
}

HACCEL CChildFrame::GetToolAccelerators()
{
	SchemeTools* pTools = ToolsManager::GetInstance()->GetToolsFor( m_view.GetCurrentScheme()->GetName() );
	return pTools->GetAcceleratorTable();
}

////////////////////////////////////////////////////
// CChildFrame::CCFSplitter

void CChildFrame::CCFSplitter::GetOwnerClientRect(HWND hOwner, LPRECT lpRect)
{
	m_pFrame->GetClientRect(lpRect);
	m_pFrame->UpdateBarsPosition(*lpRect, FALSE);	
}

////////////////////////////////////////////////////
// PoorMansUI

CChildFrame::_PoorMansUIEntry* CChildFrame::GetDefaultUIMap()
{
	static CChildFrame::_PoorMansUIEntry theMap[] =
	{
		{ID_EDITOR_WORDWRAP, PMUI_MINIBAR | PMUI_MENU},
		{ID_EDITOR_COLOURISE, PMUI_MINIBAR | PMUI_MENU},
		{ID_EDITOR_LINENOS, PMUI_MINIBAR | PMUI_MENU},
		{ID_EDITOR_WHITESPACE, PMUI_MINIBAR | PMUI_MENU},
		{ID_EDITOR_EOLCHARS, PMUI_MINIBAR | PMUI_MENU},
		{ID_VIEW_INDIVIDUALOUTPUT, PMUI_MENU},
		{ID_TOOLS_LECONVERT, PMUI_MENU},
		// note: This one must be at the end.
		{-1, 0}
	};
	return theMap;
}

CChildFrame::_PoorMansUIEntry* CChildFrame::GetPoorMansUIMap()
{
	return m_pUIData;
}

void CChildFrame::UISetChecked(UINT uID, bool bChecked, bool bUpdate)
{
	CChildFrame::_PoorMansUIEntry* pMap = GetPoorMansUIMap();
	while(pMap->nID != -1)
	{
		if(pMap->nID == uID)
		{
			if(bChecked)
				pMap->wState |= PMUI_CHECKED;
			else
				pMap->wState &= ~PMUI_CHECKED;

			if(bUpdate)
			{
				if((pMap->wState & PMUI_MENU) != 0)
					::CheckMenuItem(m_hMenu, uID, MF_BYCOMMAND | mfchecked[bChecked]);
				if((pMap->wState & PMUI_MINIBAR) != 0)
					CToolBarCtrl(m_hWndToolBar).CheckButton(uID, bChecked);
			}
			
			break;
		}

		pMap++;
	}
}

bool CChildFrame::UIGetChecked(UINT uID)
{
	CChildFrame::_PoorMansUIEntry* pMap = GetPoorMansUIMap();
	while(pMap->nID != -1)
	{
		if(pMap->nID == uID)
		{
			return (pMap->wState & PMUI_CHECKED) != 0;
		}
		pMap++;
	}

	return false;
}

bool CChildFrame::UIInvertCheck(UINT uID)
{
	CChildFrame::_PoorMansUIEntry* pMap = GetPoorMansUIMap();
	while(pMap->nID != -1)
	{
		if(pMap->nID == uID)
		{
			bool bChecked = (pMap->wState & PMUI_CHECKED) != 0;
			bChecked = !bChecked;

			if(bChecked)
				pMap->wState |= PMUI_CHECKED;
			else
				pMap->wState &= ~PMUI_CHECKED;

			if((pMap->wState & PMUI_MENU) != 0)
				::CheckMenuItem(m_hMenu, uID, MF_BYCOMMAND | mfchecked[bChecked]);
			if((pMap->wState & PMUI_MINIBAR) != 0)
				CToolBarCtrl(m_hWndToolBar).CheckButton(uID, bChecked);

			return bChecked;
			
			break;
		}
		
		pMap++;
	}
	
	return false;
}

void CChildFrame::InitUpdateUI()
{
	CChildFrame::_PoorMansUIEntry* pMap = GetDefaultUIMap();
	int so = sizeof(*pMap);
	m_pUIData = new CChildFrame::_PoorMansUIEntry[so];
	memcpy(m_pUIData, pMap, sizeof(_PoorMansUIEntry) * so);
}