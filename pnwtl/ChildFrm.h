/**
 * @file ChildFrm.h
 * @brief Interface Definition for CChildFrame, the MDI Child window.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#if !defined(CHILDFRM_H__INCLUDED)
#define CHILDFRM_H__INCLUDED

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#define MINI_BAR_HEIGHT 15

#include "fromhandle.h"

class CChildFrame : public CTabbedMDIChildWindowImpl<CChildFrame>, public CFromHandle<CChildFrame>
{
public:
	DECLARE_FRAME_WND_CLASS(NULL, IDR_MDICHILD)

	typedef CTabbedMDIChildWindowImpl<CChildFrame> baseClass;

	CTextView m_view;
	CallbackBase2<bool, CChildFrame*>* m_onClose;
	
	CString m_Title;
	CString m_FileName;

	virtual void OnFinalMessage(HWND /*hWnd*/)
	{
		delete this;
	}

	BEGIN_MSG_MAP(CChildFrame)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_FORWARDMSG, OnForwardMsg)
		MESSAGE_HANDLER(WM_CLOSE, OnClose)

		MESSAGE_HANDLER(PN_NOTIFY, OnViewNotify)

		MESSAGE_HANDLER(WM_ERASEBKGND, OnEraseBackground)
		MESSAGE_HANDLER(WM_PAINT, OnPaint)

		// Global Cut, Copy, Paste and Undo handling....
		COMMAND_ID_HANDLER(ID_EDIT_CUT, OnCut)
		COMMAND_ID_HANDLER(ID_EDIT_COPY, OnCopy)
		COMMAND_ID_HANDLER(ID_EDIT_PASTE, OnPaste)
		COMMAND_ID_HANDLER(ID_EDIT_UNDO, OnUndo)
		COMMAND_ID_HANDLER(ID_EDIT_REDO, OnRedo)

		COMMAND_ID_HANDLER(ID_EDIT_FINDNEXT, OnFindNext)
		
		COMMAND_ID_HANDLER(ID_EDITOR_WORDWRAP, OnWordWrapToggle)
		COMMAND_ID_HANDLER(ID_EDITOR_COLOURISE, OnColouriseToggle)

		COMMAND_ID_HANDLER(ID_FILE_SAVE_AS, OnSaveAs)
		COMMAND_ID_HANDLER(ID_FILE_SAVE, OnSave)

		COMMAND_ID_HANDLER(ID_EDIT_GOTO, OnGoto)

		// For ::FromHandle
		IMPLEMENT_FROMHANDLE()
		
		// Chaining
		CHAIN_MSG_MAP(baseClass)
		CHAIN_CLIENT_COMMANDS ()
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	// We override UpdateLayout in order to call UpdateBarsPosition in this class,
	// instead of in CFrameWindowImplBase. This way we can automagically have a toolbar
	// at the bottom of the window.
	void UpdateLayout(BOOL bResizeBars = TRUE)
	{
		RECT rect;
		GetClientRect(&rect);

		// position bars and offset their dimensions
		UpdateBarsPosition(rect, bResizeBars);

		// resize client window
		if(m_hWndClient != NULL)
			::SetWindowPos(m_hWndClient, NULL, rect.left, rect.top,
				rect.right - rect.left, rect.bottom - rect.top,
				SWP_NOZORDER | SWP_NOACTIVATE);
	}

	void UpdateBarsPosition(RECT& rect, BOOL bResizeBars = TRUE)
	{
		// resize toolbar
		if(m_hWndToolBar != NULL && ((DWORD)::GetWindowLong(m_hWndToolBar, GWL_STYLE) & WS_VISIBLE))
		{
			if(bResizeBars)
			{
				// Size of mini bar controlled here...
				::SetWindowPos(m_hWndToolBar, HWND_TOP, rect.left, rect.bottom - MINI_BAR_HEIGHT, rect.right-rect.left, MINI_BAR_HEIGHT, SWP_NOACTIVATE | SWP_NOZORDER);
				//::SendMessage(m_hWndToolBar, WM_SIZE, 0, 0);
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

	void SetupToolbar()
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
		button.iString = NULL;
		button.idCommand = ID_EDITOR_COLOURISE;

		toolbar.AddButtons(1, &button);

		button.iBitmap = 0;
		button.idCommand = ID_EDITOR_WORDWRAP;
		button.fsState = TBSTATE_ENABLED;

		toolbar.AddButtons(1, &button);
		
		//button.fsStyle = TBSTYLE_SEP;

		m_hWndToolBar = toolbar.Detach();
	}

	////////////////////////////////////////////////////
	// Document Entries

	void SetTitle(LPCTSTR sFileName, bool bModified = false)
	{
		CString buf = sFileName;
		
		if(bModified)
			buf += " *";
		
		this->SetWindowText(buf);
		SetTabText(buf);
		
		m_Title = sFileName;
	}

	LPCTSTR GetTitle()
	{
		return m_Title;
	}

	bool GetModified()
	{
		return m_view.GetModified();
	}

	////////////////////////////////////////////////////
	// Message Handlers

	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
	{
		m_hWndClient = m_view.Create(m_hWnd, rcDefault, NULL, WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_CLIPCHILDREN, WS_EX_CLIENTEDGE);

		SetTitle(_T("<new>"));

		SetupToolbar();

		bHandled = FALSE;
		return 1;
	}

	LRESULT OnPaint(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
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

	LRESULT OnEraseBackground(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		// Need to do this so the toolbar is drawn...
		return 0;
	}

	LRESULT OnForwardMsg(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
	{
		LPMSG pMsg = (LPMSG)lParam;

		if(CTabbedMDIChildWindowImpl<CChildFrame>::PreTranslateMessage(pMsg))
			return TRUE;

		return m_view.PreTranslateMessage(pMsg);
	}

	LRESULT OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
	{
		bHandled = FALSE;
		if(m_onClose)
		{
			// Ok, so maybe this OnClose handling should be moved into this class :(
			if( ! (*m_onClose)((CChildFrame*)this) )
				bHandled = TRUE; // cancel close.
		}

		return 0;
	}

	LRESULT OnViewNotify(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
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

	LRESULT OnCut(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		::PostMessage(::GetFocus(), WM_CUT, 0, 0);
		return TRUE;
	}
	
	LRESULT OnCopy(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		::PostMessage(::GetFocus(), WM_COPY, 0, 0);
		return TRUE;
	}

	LRESULT OnPaste(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		::PostMessage(::GetFocus(), WM_PASTE, 0, 0);
		return TRUE;
	}

	LRESULT OnUndo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		::PostMessage(::GetFocus(), WM_UNDO, 0, 0);
		return TRUE;
	}

	LRESULT OnRedo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		m_view.Redo();
		return TRUE;
	}

	LRESULT OnFindNext(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		SFindOptions* pOptions = theApp.GetFindOptions();
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

	////////////////////////////////////////////////////
	// Command Handlers

	LRESULT OnSaveAs(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		SaveAs();

		return 0;
	}

	LRESULT OnSave(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		if(m_Title.Find('<') == -1)
			SaveFile(m_FileName, false);
		else
			SaveAs();

		return 0;
	}

	LRESULT OnWordWrapToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		CToolBarCtrl toolbar(m_hWndToolBar);
		m_view.SetWrapMode( toolbar.IsButtonChecked(wID) ? SC_WRAP_WORD : SC_WRAP_NONE );
		return 0;
	}

	LRESULT OnColouriseToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		CToolBarCtrl toolbar(m_hWndToolBar);
		m_view.EnableHighlighting(toolbar.IsButtonChecked(wID) != 0);
		

		return 0;
	}

	LRESULT OnGoto(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		CGotoDialog g;
		if(g.DoModal() == IDOK)
		{
			m_view.GotoLine(g.GetLineNo());
		}

		return 0;
	}

	////////////////////////////////////////////////////
	// Editor Window Methods

	void PNOpenFile(LPCTSTR pathname, LPCTSTR filename, CScheme* pScheme = NULL)
	{
		m_view.Load(pathname, pScheme);
		SetTitle(filename);
		m_FileName = pathname;
	}

	void SaveFile(LPCTSTR pathname, bool bStoreFilename = true)
	{
		m_view.Save(pathname, bStoreFilename);
		if(bStoreFilename)
		{
			ctcString fn;
			CFileName(pathname).GetFileName(fn);

			SetTitle(fn.c_str());

			m_FileName = pathname;
		}
	}

	bool CanSave()
	{
		return ((m_FileName != _T("")) && (m_FileName.Find(_T("<")) == -1));
	}

	bool SaveAs()
	{
		CSSFileDialog	dlgSave(FALSE, NULL, NULL, OFN_HIDEREADONLY, "All Files (*.*)|*.*", m_hWndClient);
		bool			bRet = true;

		if(dlgSave.DoModal() == IDOK)
		{
			SaveFile(dlgSave.m_ofn.lpstrFile);
		}
		else
		{
			bRet = false;
		}

		return bRet;
	}

	void Save()
	{
		SaveFile(m_FileName, false);
	}

	bool FindNext(SFindOptions* options)
	{
		return m_view.FindNext(options);
	}

	bool Replace(SReplaceOptions* options)
	{
		return m_view.ReplaceOnce(options);
	}

	void ReplaceAll(SReplaceOptions* options)
	{
		m_view.ReplaceAll(options);
	}

	void HighlightAll(SFindOptions* options)
	{
		m_view.HighlightAll(options);
	}

	void SetPosStatus(CMultiPaneStatusBarCtrl&	stat)
	{
		m_view.SetPosStatus(stat);
	}

	void SetScheme(CScheme* pScheme)
	{
		m_view.SetScheme(pScheme);
	}

protected:
	HIMAGELIST m_hImgList;
	int m_lastLexer;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(CHILDFRM_H__INCLUDED)
