/**
 * @file ChildFrm.cpp
 * @brief Implementation of CChildFrame, the MDI Child window.
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "childfrm.h"
#include "outputview.h"
#include "tools.h"

#ifdef _DEBUG 
# define LLR_DEBUG_NEW new(_NORMAL_BLOCK, __FILE__, __LINE__) 
# define LLR_DEBUG_MALLOC(size) _malloc_dbg(size, _NORMAL_BLOCK, __FILE__, __LINE__) 
# define new LLR_DEBUG_NEW 
# define malloc LLR_DEBUG_MALLOC 
#endif // _DEBUG 

CChildFrame::CChildFrame()
{
	::InitializeCriticalSection(&m_crRunningTools);

	m_hWndOutput = NULL;
	m_hImgList = NULL;
	m_pSplitter = NULL;
	m_pOutputView = NULL;
	m_pFirstTool = NULL;
	
	m_FileAge = -1;
	
	m_po.hDevMode = 0;
	m_po.hDevNames = 0;
	//memset(&m_po.rcMargins, 0, sizeof(RECT));
	COptionsManager::GetInstance()->LoadPrintSettings(&m_po);

	InitUpdateUI();

	m_iFirstToolCmd = ID_TOOLS_DUMMY;
}

CChildFrame::~CChildFrame()
{
	if(m_pFirstTool)
		KillTools(false); // Can't afford to wait, no "completed" events will get through...

	::DeleteCriticalSection(&m_crRunningTools);

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

	TBBUTTON button;
	memset(&button, 0, sizeof(TBBUTTON));
	button.iBitmap = 1;
	button.fsState = TBSTATE_ENABLED | TBSTATE_CHECKED;
	button.fsStyle = TBSTYLE_CHECK;
	button.idCommand = ID_EDITOR_COLOURISE;

	toolbar.AddButtons(1, &button);

	button.iBitmap = 0;
	button.idCommand = ID_EDITOR_WORDWRAP;
	button.fsState = TBSTATE_ENABLED;

	toolbar.AddButtons(1, &button);

	button.iBitmap = 2;
	button.idCommand = ID_EDITOR_LINENOS;

	toolbar.AddButtons(1, &button);

	m_hWndToolBar = toolbar.Detach();
}

void CChildFrame::ToggleOutputWindow(bool bSetValue, bool bSetShowing)
{
	bool bShow;
	bool bVisible = ((m_pSplitter != NULL) ? m_pSplitter->GetSinglePaneMode() == SPLITTER_NORMAL : false);
	if(bSetValue)
		bShow = bSetShowing;
	else
		bShow = !bVisible;

	if(bShow && !bVisible)
	{
		if(!m_pSplitter)
		{
			m_pSplitter = new CCFSplitter(this);
			
			m_pSplitter->SetHorizontal( COptionsManager::GetInstance()->Get(PNSK_EDITOR, _T("OutputSplitHorizontal"), true) );

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
		}
		else 
		{
			m_pSplitter->DisableSinglePaneMode();
		}
	}
	else if(!bShow && bVisible)
	{
		m_pSplitter->SetSinglePaneMode(SPLITTER_TOP);
	}

	UISetChecked(ID_EDITOR_OUTPUTWND, bShow);
}

////////////////////////////////////////////////////
// Document Entries

void CChildFrame::SetTitle(LPCTSTR sFileName, bool bModified)
{
	CString buf = sFileName;
	
	if(bModified)
		buf += " *";
	
	this->SetWindowText(buf);
	SetTabText(buf);
	
	m_Title = sFileName;
}

tstring CChildFrame::GetFileName(EGFNType type)
{
	CFileName fn(m_FileName);
	tstring s;

	switch(type)
	{
		case FN_FULL:
			return fn;

		case FN_FILE:
			fn.GetFileName(s);
			break;

		case FN_FILEPART:
			fn.GetFileName_NoExt(s);
			break;

		case FN_PATH:
			fn.GetPath(s);
			break;
	};
	
	return s;
}

LPCTSTR CChildFrame::GetTitle()
{
	return m_Title;
}

bool CChildFrame::GetModified()
{
	return m_view.GetModified();
}

bool CChildFrame::CanClose()
{
	bool bRet = true;

	if(GetModified())
	{
		CString title;
		title.Format(_T("Would you like to save changes to:\n%s?"), GetTitle());
		int res = MessageBox(title, "Programmers Notepad", MB_YESNOCANCEL | MB_ICONQUESTION);
		switch (res)
		{
			case IDYES:
			{
				if( CanSave() )
				{
					Save();
				}
				else
				{
					return SaveAs();
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

////////////////////////////////////////////////////
// Message Handlers

LRESULT CChildFrame::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	m_hWndClient = m_view.Create(m_hWnd, rcDefault, NULL, WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_CLIPCHILDREN, WS_EX_CLIENTEDGE);

	SetTitle(_T("<new>"));

	SetupToolbar();

	UISetChecked(ID_EDITOR_COLOURISE, true);
	UISetChecked(ID_EDITOR_WORDWRAP, false);
	UISetChecked(ID_EDITOR_LINENOS, false);
	UISetChecked(ID_TOOLS_LECONVERT, true);

	UpdateMenu();

	bHandled = FALSE;
	return 1;
}

LRESULT CChildFrame::OnMDIActivate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	UpdateMenu();
	CheckAge();
	bHandled = FALSE;
	return 0;
}

LRESULT CChildFrame::OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = FALSE;
	
	if(!CanClose())
	{
		KillTools(true);

		bHandled = TRUE;
	}

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
	CheckAge();

	return 0;
}

LRESULT CChildFrame::OnViewNotify(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	if(lParam == SCN_SAVEPOINTREACHED)
	{
		SetTitle(m_Title, false);
	}
	else if(lParam == SCN_SAVEPOINTLEFT)
	{
		SetTitle(m_Title, true);
	}
	else
	{
		SendMessage(GetMDIFrame(), PN_NOTIFY, wParam, lParam);
	}

	return TRUE;
}

LRESULT CChildFrame::OnOptionsUpdate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CScheme* pS = m_view.GetCurrentScheme();
	UpdateTools(pS);

	// re-load the compiled scheme...
	if(pS)
		m_view.SetScheme(pS);

	UpdateMenu();

	return 0;
}

LRESULT CChildFrame::OnToggleOutput(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	ToggleOutputWindow(wParam != 0, lParam != 0);
	return 0;
}

LRESULT CChildFrame::OnToolFinished(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	ToolFinished(reinterpret_cast<ToolRunner*>(lParam));
	return 0;
}

LRESULT CChildFrame::OnSchemeChanged(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	SchemeChanged(reinterpret_cast<CScheme*>(lParam));
	return 0;
}

////////////////////////////////////////////////////
// Command Handlers

LRESULT CChildFrame::OnPrint(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_view.PrintDocument(&m_po, true);

	return TRUE;
}

LRESULT CChildFrame::OnPrintSetup(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	PrintSetup();

	return TRUE;
}

LRESULT CChildFrame::OnCut(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::PostMessage(::GetFocus(), WM_CUT, 0, 0);
	return TRUE;
}

LRESULT CChildFrame::OnCopy(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::PostMessage(::GetFocus(), WM_COPY, 0, 0);
	return TRUE;
}

LRESULT CChildFrame::OnPaste(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::PostMessage(::GetFocus(), WM_PASTE, 0, 0);
	return TRUE;
}

LRESULT CChildFrame::OnUndo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::PostMessage(::GetFocus(), WM_UNDO, 0, 0);
	return TRUE;
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
	SFindOptions* pOptions = COptionsManager::GetInstanceRef().GetFindOptions();
	if( pOptions->FindText != _T("") )
	{
		if( !m_view.FindNext(pOptions) )
		{
			CString cs;
			cs.Format(_T("Could not find %s."), pOptions->FindText);
			MessageBox(cs, _T("Programmers Notepad"), MB_OK);
		}
	}
	return TRUE;
}

LRESULT CChildFrame::OnSave(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Save();

	return 0;
}

LRESULT CChildFrame::OnSaveAs(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	SaveAs();

	return 0;
}

LRESULT CChildFrame::OnClose(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	PostMessage(WM_CLOSE, 0, 0);
	return 0;
}


LRESULT CChildFrame::OnWordWrapToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_view.SetWrapMode( UIInvertCheck(wID) ? SC_WRAP_WORD : SC_WRAP_NONE );

	return 0;
}

LRESULT CChildFrame::OnColouriseToggle(WORD /*wNotifyCode*/, WORD wID, HWND hWndCtl, BOOL& /*bHandled*/)
{
	m_view.EnableHighlighting(UIInvertCheck(wID));
	
	return 0;
}

LRESULT CChildFrame::OnLineNoToggle(WORD /*wNotifyCode*/, WORD wID, HWND hWndCtl, BOOL& /*bHandled*/)
{
	m_view.ShowLineNumbers(UIInvertCheck(wID));

	return 0;
}

LRESULT CChildFrame::OnOutputWindowToggle(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
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

LRESULT CChildFrame::OnHideOutput(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	ToggleOutputWindow(true, false);
	return 0;
}

LRESULT CChildFrame::OnGoto(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CGotoDialog g;
	if(g.DoModal() == IDOK)
	{
		m_view.GotoLine(g.GetLineNo());
	}

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
	KillTools(false); 

	return 0;
}

LRESULT CChildFrame::OnUseTabs(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_view.SetUseTabs(!m_view.GetUseTabs());
	UpdateMenu();

	return 0;
}

////////////////////////////////////////////////////
// Notify Handlers

LRESULT CChildFrame::OnGetInfoTip(int idCtrl, LPNMHDR pnmh, BOOL& bHandled)
{
	LPNMTBGETINFOTIP pS = (LPNMTBGETINFOTIP)pnmh;

	switch(pS->iItem)
	{
		case ID_EDITOR_COLOURISE:
			_tcsncpy(pS->pszText, _T("Toggle Highlighting"), pS->cchTextMax);
			break;
		case ID_EDITOR_WORDWRAP:
			_tcsncpy(pS->pszText, _T("Toggle Word-Wrap"), pS->cchTextMax);
			break;
		case ID_EDITOR_LINENOS:
			_tcsncpy(pS->pszText, _T("Toggle Line Numbers"), pS->cchTextMax);
			break;
	}

	return 0;
}

////////////////////////////////////////////////////
// File Management Methods

void CChildFrame::CheckAge()
{
	if(CanSave())
	{
		long age = FileAge(m_FileName);
		if(age != m_FileAge)
		{
			CString msg;
			msg.Format(_T("%s\n\nThe above file has been modified outside of Programmers Notepad, do\nyou want to refresh and lose changes?"), (LPCTSTR)m_FileName);
			if ( MessageBox((LPCTSTR)msg, _T("File Changed"), MB_YESNO) == IDYES )
			{
				Revert();
			}
			else
				m_FileAge = age;
		}
	}
}

void CChildFrame::Revert()
{
	// Check that we have a valid filename first...
	if(CanSave())
	{
		//@todo maybe flag this instead of re-applying the scheme (a little un-necessary)
		m_view.Load((LPCTSTR)m_FileName, m_view.GetCurrentScheme());
		m_FileAge = FileAge(m_FileName);
	}
}

void CChildFrame::PNOpenFile(LPCTSTR pathname, LPCTSTR filename, CScheme* pScheme)
{
	if(m_view.Load(pathname, pScheme))
	{
		m_FileAge = FileAge(pathname);
		SetTitle(filename);
		m_FileName = pathname;
	}
	else
	{
		CFile err;
		err.ShowError(pathname);
	}
	
	// Loading a file may have changed the line endings/text encoding of the
	// document, so we update the menu...
	UpdateMenu();
}

void CChildFrame::SaveFile(LPCTSTR pathname, bool bStoreFilename, bool bUpdateMRU)
{
	if(m_view.Save(pathname, bStoreFilename))
	{
		if(bStoreFilename)
		{
			m_FileAge = FileAge(pathname);

			ctcString fn;
			CFileName(pathname).GetFileName(fn);

			SetTitle(fn.c_str());

			m_FileName = pathname;
		}

		if(bUpdateMRU)
		{
			g_Context.m_frame->AddMRUEntry(m_FileName);
		}
	}
	else
	{
		CFile err;
		if ( err.ShowError(pathname, false) == IDYES )
		{
			SaveAs();
		}
	}
}

bool CChildFrame::CanSave()
{
	return ((m_FileName != _T("")) && (m_FileName.Find(_T("<")) == -1));
}

bool CChildFrame::SaveAs()
{
	CPNSaveDialog dlgSave(_T("All Files (*.*)|*.*|"));
	bool bRet = true;

	if(dlgSave.DoModal() == IDOK)
	{
		EPNSaveFormat format = dlgSave.GetSaveFormat();
		if(format != PNSF_NoChange)
		{
			ChangeFormat(format);
		}
		SaveFile(dlgSave.m_ofn.lpstrFile);
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
	//@todo Update menu item...
	UpdateMenu();
}

void CChildFrame::Save()
{
	if(CanSave())
	{
		SaveFile(m_FileName, false);
		m_FileAge = FileAge(m_FileName);
	}
	else
		SaveAs();
}

////////////////////////////////////////////////////
// Editor Window Methods	

bool CChildFrame::FindNext(SFindOptions* options)
{
	return m_view.FindNext(options);
}

bool CChildFrame::Replace(SReplaceOptions* options)
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

int CChildFrame::ReplaceAll(SReplaceOptions* options)
{
	return m_view.ReplaceAll(options);
}

void CChildFrame::HighlightAll(SFindOptions* options)
{
	m_view.HighlightAll(options);
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

void CChildFrame::OnSchemeChange(LPVOID pVoid)
{
	SetScheme(static_cast<CScheme*>(pVoid));
}

void CChildFrame::SetScheme(CScheme* pScheme)
{
    m_view.SetScheme(pScheme);
}

void CChildFrame::UpdateTools(CScheme* pScheme)
{
	//m_iFirstToolCmd

	CSMenuHandle menu(m_hMenu);
	CSMenuHandle tools( menu.GetSubMenu(3) );
	
	m_iFirstToolCmd = SchemeToolsManager::GetInstance()->UpdateToolsMenu(
		tools, m_iFirstToolCmd, ID_TOOLS_DUMMY, pScheme->GetName()
	);
}

void CChildFrame::SchemeChanged(CScheme* pScheme)
{
	UpdateTools(pScheme);
	g_Context.m_frame->SetActiveScheme(m_hWnd, static_cast<LPVOID>(pScheme));
}

void CChildFrame::UpdateMenu()
{
	CSMenuHandle menu(m_hMenu);

	EPNSaveFormat f = (EPNSaveFormat)m_view.GetEOLMode();
	
	menu.CheckMenuItem(ID_TOOLS_LECRLF, f == PNSF_Windows);
	menu.CheckMenuItem(ID_TOOLS_LECR, f == PNSF_Mac);
	menu.CheckMenuItem(ID_TOOLS_LELF, f == PNSF_Unix);
	menu.CheckMenuItem(ID_TOOLS_USETABS, m_view.GetUseTabs());
	
	{
		// Scope to restrict lock scope.
		CSSCritLock lock(&m_crRunningTools);
		menu.EnableMenuItem(ID_TOOLS_STOPTOOLS, m_pFirstTool != NULL);
	}

	g_Context.m_frame->SetActiveScheme(m_hWnd, m_view.GetCurrentScheme());
}

void CChildFrame::OnRunTool(LPVOID pVoid)
{
	SToolDefinition* pTool = reinterpret_cast<SToolDefinition*>(pVoid);
	ToolRunner *r = new ToolRunner(this, pTool);
	
	bool bThreaded = r->GetThreadedExecution();
	if(bThreaded)
		AddRunningTool(r);
	
	if(pTool->bSaveAll)
		g_Context.m_frame->SaveAll();

	r->Execute();
	
	if(!bThreaded)
	{
		if(pTool->bIsFilter)
			Revert();

		delete r;
	}
}

void CChildFrame::AddOutput(LPCSTR outputstring, int nLength)
{
	// We do a sendmessage so that the windows are created in the 
	// window thread, and not in any calling thread.
	SendMessage(PN_TOGGLEOUTPUT, 1, 1);

	m_pOutputView->SafeAppendText(outputstring, nLength);
}

void CChildFrame::AddRunningTool(ToolRunner* pRunner)
{
	CSSCritLock lock(&m_crRunningTools);

    if(m_pFirstTool)
		pRunner->m_pNext = m_pFirstTool;

	m_pFirstTool = pRunner;

	UpdateMenu();
}

void CChildFrame::ToolFinished(ToolRunner* pRunner)
{
	CSSCritLock lock(&m_crRunningTools);

	if(m_pFirstTool)
	{
		ToolRunner* pT = m_pFirstTool;
		ToolRunner* pP = NULL;

		while(pT != NULL && pT != pRunner)
		{
			pP = pT;
			pT = pT->m_pNext;
		}

		if(pT)
		{
			if(pP)
				pP->m_pNext = pT->m_pNext;

			if(m_pFirstTool == pT)
				m_pFirstTool = pT->m_pNext;
		}

		do{
		}
		while(!pRunner->GetStopped(20));

		tstring exitcode(_T("\n> Process Exit Code: "));
		exitcode += IntToTString(pRunner->GetExitCode());
		exitcode += _T("\n");
		AddOutput(exitcode.c_str());
		if(pRunner->GetToolDef())
		{
			if(pRunner->GetToolDef()->bIsFilter)
				Revert();
		}

		delete pRunner;

		UpdateMenu();
	}
}

void CChildFrame::KillTools(bool bWaitForKill)
{
	int iLoopCount = 0;

	// Signal to all tools to exit, scope to enter and exit critical section
	{
		CSSCritLock lock(&m_crRunningTools);

		ToolRunner* pT = m_pFirstTool;
		while(pT)
		{
			pT->SetCanRun(false);
			pT = pT->m_pNext;
		}
	}

	while(bWaitForKill)
	{
		// Normally, we give all the tools a chance to exit before continuing...
		Sleep(100);
		iLoopCount++;

		// Don't tolerate more than 5 seconds of waiting...
		if(iLoopCount > 50)
			break;

		{
			CSSCritLock lock(&m_crRunningTools);
			if(!m_pFirstTool)
				break;
		}
	}
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

	return FALSE;
}

void CChildFrame::PrintSetup()
{
	PAGESETUPDLG pdlg = {
	                sizeof(PAGESETUPDLG), 
					0, 0, 0, 0, 
					{0, 0}, 
					{0, 0, 0, 0}, 
					{0, 0, 0, 0}, 
					0, 0, 0, 0, 0, 0
	                };

	pdlg.hwndOwner = m_hWnd/*MainHWND()*/;
	pdlg.hInstance = ::GetModuleHandle(NULL);

	if (m_po.rcMargins.left != 0 || m_po.rcMargins.right != 0 ||
			m_po.rcMargins.top != 0 || m_po.rcMargins.bottom != 0) {
		pdlg.Flags = PSD_MARGINS;

		pdlg.rtMargin.left = m_po.rcMargins.left;
		pdlg.rtMargin.top = m_po.rcMargins.top;
		pdlg.rtMargin.right = m_po.rcMargins.right;
		pdlg.rtMargin.bottom = m_po.rcMargins.bottom;
	}

	pdlg.hDevMode = m_po.hDevMode;
	pdlg.hDevNames = m_po.hDevNames;

	if (!PageSetupDlg(&pdlg))
		return;

	m_po.rcMargins.left = pdlg.rtMargin.left;
	m_po.rcMargins.top = pdlg.rtMargin.top;
	m_po.rcMargins.right = pdlg.rtMargin.right;
	m_po.rcMargins.bottom = pdlg.rtMargin.bottom;

	m_po.hDevMode = pdlg.hDevMode;
	m_po.hDevNames = pdlg.hDevNames;

	COptionsManager::GetInstance()->SavePrintSettings(&m_po);
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
		{ID_EDITOR_WHITESPACE, PMUI_MENU},
		{ID_EDITOR_EOLCHARS, PMUI_MENU},
		{ID_EDITOR_OUTPUTWND, PMUI_MENU},
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