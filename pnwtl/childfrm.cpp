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
#include "resource.h"
#include "childfrm.h"
#include "tools.h"
#include "outputview.h"
#include "exporters.h"
#include "pndialogs.h"
#include "docprops.h"
#include "include/pagesetupdialog.h"
#include "jumpto.h"
#include "jumptodialog.h"

#include "tabbingframework/TabbedMDISave.h"

#if defined (_DEBUG)
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

bool CChildFrame::s_bFirstChild = true;

CChildFrame::CChildFrame()
{
	m_hWndOutput = NULL;
	m_hImgList = NULL;
	m_pSplitter = NULL;
	m_pOutputView = NULL;
	
	m_FileAge = -1;
	
	m_po.hDevMode = 0;
	m_po.hDevNames = 0;
	memset(&m_po.rcMargins, 0, sizeof(RECT));
	OPTIONS->LoadPrintSettings(&m_po);

	InitUpdateUI();

	m_iFirstToolCmd = ID_TOOLS_DUMMY;

	m_bModifiedOverride = false;
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
// Document Entries

void CChildFrame::SetTitle( bool bModified )
{
	LPCTSTR sFullPath = m_FileName;

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
	CFileName fn(m_FileName);
	//tstring s;

	switch(type)
	{
		case FN_FULL:
			return fn;

		case FN_FILE:
			return fn.GetFileName();

		case FN_FILEPART:
			return fn.GetFileName_NoExt();

		case FN_PATH:
			return fn.GetPath();

		default:
			return fn;
	};
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

void CChildFrame::LoadExternalLexers()
{
	HANDLE hFind;
	WIN32_FIND_DATA FindFileData;

	tstring sPath;
	OPTIONS->GetPNPath(sPath, PNPATH_SCHEMES);

	tstring sPattern(sPath);
	sPattern += "*.lexer";

	hFind = FindFirstFile(sPattern.c_str(), &FindFileData);
	if (hFind != INVALID_HANDLE_VALUE) {
		//Found the first file...
		BOOL found = TRUE;
		tstring to_open;

		while (found) {
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

	m_FileName = _T("<new>");
	SetTitle();

	SetupToolbar();

	UISetChecked(ID_EDITOR_COLOURISE, true);
	UISetChecked(ID_EDITOR_WORDWRAP, false);
	UISetChecked(ID_EDITOR_LINENOS, OPTIONS->GetCached(Options::OLineNumbers) != 0);
	UISetChecked(ID_TOOLS_LECONVERT, true);

	m_view.ShowLineNumbers(OPTIONS->GetCached(Options::OLineNumbers) != 0);

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

LRESULT CChildFrame::OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
{
	bHandled = FALSE;
	
	// 253 is a special value that means "don't ask the user first". Normally for
	// use if we've already asked the user.
	if((lParam != 253) && !CanClose())
	{
		bHandled = TRUE;
	}
	else
	{
		if( ToolOwner::HasInstance() )
		{
			ToolOwner::GetInstance()->KillTools(true, this);
		}
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
	if(lParam == SCN_SAVEPOINTREACHED || lParam == SCN_SAVEPOINTLEFT)
	{
		SetTitle(GetModified());
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

	SetTitle(GetModified());

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
	SchemeChanged(reinterpret_cast<CScheme*>(lParam));
	return 0;
}

LRESULT CChildFrame::OnChildIsModified(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	if( GetModified() )
	{
		ITabbedMDIChildModifiedItem* pMI = (ITabbedMDIChildModifiedItem*)lParam;
		
		USES_CONVERSION;
		
		wstring wstr = T2CW(GetFileName(FN_FILE).c_str());
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

////////////////////////////////////////////////////
// Command Handlers

LRESULT CChildFrame::OnPrint(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_po.Filename = m_FileName;
	m_view.PrintDocument(&m_po, true);

	return TRUE;
}

LRESULT CChildFrame::OnPrintSetup(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	PrintSetup();

	return TRUE;
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
	SFindOptions* pOptions = OPTIONS->GetFindOptions();
	if( pOptions->FindText != _T("") )
	{
		if( !/*m_view.*/FindNext(pOptions) )
		{
			CString cs;
			cs.Format(_T("Could not find %s."), pOptions->FindText);
			MessageBox(cs, _T("Programmers Notepad"), MB_OK);
		}
	}
	return TRUE;
}

LRESULT CChildFrame::OnCopyRTF(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	StringOutput so(m_view.GetSelLength() * 2);
	StylesList* pStyles = m_view.GetCurrentScheme()->CreateStylesList();
	RTFExporter rtf(&so, m_view.GetCurrentScheme()->GetName(), pStyles, &m_view);
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

LRESULT CChildFrame::OnRevert(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Revert();

	return 0;
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

LRESULT CChildFrame::OnJumpTo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CJumpToDialog dlg(this);
	if(dlg.DoModal() == IDOK)
	{
		m_view.GotoLine(dlg.GetLine() - 1);
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
	ToolOwner::GetInstance()->KillTools(false, this); 

	return 0;
}

LRESULT CChildFrame::OnUseTabs(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_view.SetUseTabs(!m_view.GetUseTabs());
	UpdateMenu();

	return 0;
}

#include "afiles.h"

LRESULT CChildFrame::OnHeaderSwitch(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& bHandled)
{
	// Check there's a valid filename...
	if(CanSave())
	{
		tstring alternateFile;

		AlternateFiles* afiles = AlternateFiles::GetInstance();
		
		if(	afiles->GetAlternate(m_FileName, alternateFile) )
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

class ChildOutputWrapper : public ToolWrapperT<CChildFrame, COutputView>
{
typedef ToolWrapperT<CChildFrame, COutputView> baseClass;
public:
	ChildOutputWrapper(CChildFrame* pOwner, COutputView* pView, CChildFrame* pActiveChild, const ToolDefinition& definition)
		: baseClass(pOwner, pView, pActiveChild, definition)
	{}
};

LRESULT CChildFrame::OnRunTool(LPVOID pTool)
{
	ToolDefinition* pToolDef = static_cast<ToolDefinition*>(pTool);
	ToolWrapper* pWrapper = NULL;
	if(	pToolDef->GlobalOutput() )
	{
		pWrapper = g_Context.m_frame->MakeGlobalOutputWrapper(pToolDef);
	}
	else
	{
		if(pToolDef->CaptureOutput())
			EnsureOutputWindow();
		pWrapper = new ChildOutputWrapper(this, m_pOutputView, this, *pToolDef);
	}

	pWrapper->SetNotifyWindow(m_hWnd);

	ToolOwner::GetInstance()->RunTool(pWrapper, this);

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
			if(FileExists(m_FileName))
			{
				CString msg;
				msg.Format(_T("%s\n\nThe above file has been modified outside of Programmers Notepad, do\nyou want to refresh and lose changes?"), (LPCTSTR)m_FileName);
				if ( MessageBox((LPCTSTR)msg, _T("File Changed"), MB_YESNO) == IDYES )
				{
					Revert();
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
				msg.Format(_T("Warning: the file %s does not exist any more."), (LPCTSTR)m_FileName);
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
		m_view.Revert((LPCTSTR)m_FileName);
		m_FileAge = FileAge(m_FileName);
		SetModifiedOverride(false);
	}
}

bool CChildFrame::PNOpenFile(LPCTSTR pathname, CScheme* pScheme)
{
	bool bRet = false;

	if(m_view.Load(pathname, pScheme))
	{
		m_FileAge = FileAge(pathname);
		m_FileName = pathname;
		SetTitle();
		bRet = true;
	}
	else
	{
		CFile err;
		err.ShowError(pathname);
	}
	
	// Loading a file may have changed the line endings/text encoding of the
	// document, so we update the menu...
	UpdateMenu();

	return bRet;
}

void CChildFrame::SaveFile(LPCTSTR pathname, bool bStoreFilename, bool bUpdateMRU)
{
	if(m_view.Save(pathname, bStoreFilename))
	{
		if(bStoreFilename)
		{
			m_FileAge = FileAge(pathname);
			SetModifiedOverride(false);
			m_FileName = pathname;

			SetTitle();			
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
	LPCTSTR saPath = NULL;
	if(CanSave())
	{
		saPath = m_FileName;
	}

	CPNSaveDialogEx dlgSave(_T("All Files (*.*)|*.*|"), saPath);
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

	UpdateMenu();
}

void CChildFrame::Save()
{
	if(CanSave())
	{
		SaveFile(m_FileName, false);
		m_FileAge = FileAge(m_FileName);
		SetModifiedOverride(false);
	}
	else
		SaveAs();
}

////////////////////////////////////////////////////
// Editor Window Methods	

int CChildFrame::FindNext(SFindOptions* options)
{
	int result = m_view.FindNext(options);
	if( result == CScintillaImpl::FindNextResults::fnReachedStart )
		MessageBox(_T("Find reached the starting point of the search."),
							_T("Programmers Notepad"), MB_OK | MB_ICONINFORMATION);
	return result;
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
	
	bool bToolsRunning = false;
	if( ToolOwner::HasInstance() )
		bToolsRunning = ToolOwner::GetInstance()->HaveRunningTools(this);

	menu.EnableMenuItem(ID_TOOLS_STOPTOOLS, bToolsRunning);

	g_Context.m_frame->SetActiveScheme(m_hWnd, m_view.GetCurrentScheme());
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
			CFileName fn(m_FileName);
			fn.GetFileName_NoExt(guessName);
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
	SchemeTools* pTools = SchemeToolsManager::GetInstance()->GetToolsFor( m_view.GetCurrentScheme()->GetName() );
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
		{ID_EDITOR_WHITESPACE, PMUI_MENU},
		{ID_EDITOR_EOLCHARS, PMUI_MENU},
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