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
#endif

#define MINI_BAR_HEIGHT 15

#define MENUMESSAGE_CHANGESCHEME 0xa
#define PN_CHECKAGE WM_USER+32

#define PMUI_MINIBAR	0x0001
#define PMUI_MENU		0x0002
#define PMUI_CHECKED	0x0004

#include "fromhandle.h"

class CChildFrame : public CTabbedMDIChildWindowImpl<CChildFrame>, 
	public CFromHandle<CChildFrame>, public CSMenuEventHandler
{
public:
	DECLARE_FRAME_WND_CLASS(NULL, IDR_MDICHILD)

	typedef CTabbedMDIChildWindowImpl<CChildFrame> baseClass;

	CallbackBase2<bool, CChildFrame*>* m_onClose;

	virtual void OnFinalMessage(HWND /*hWnd*/)
	{
		delete this;
	}

	BEGIN_MSG_MAP(CChildFrame)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_FORWARDMSG, OnForwardMsg)
		MESSAGE_HANDLER(WM_CLOSE, OnClose)
		MESSAGE_HANDLER(WM_MDIACTIVATE, OnMDIActivate)

		MESSAGE_HANDLER(PN_NOTIFY, OnViewNotify)

		MESSAGE_HANDLER(WM_ERASEBKGND, OnEraseBackground)
		MESSAGE_HANDLER(WM_PAINT, OnPaint)

		MESSAGE_HANDLER(PN_CHECKAGE, OnCheckAge)

		// Global Cut, Copy, Paste and Undo handling....
		COMMAND_ID_HANDLER(ID_EDIT_CUT, OnCut)
		COMMAND_ID_HANDLER(ID_EDIT_COPY, OnCopy)
		COMMAND_ID_HANDLER(ID_EDIT_PASTE, OnPaste)
		COMMAND_ID_HANDLER(ID_EDIT_UNDO, OnUndo)
		COMMAND_ID_HANDLER(ID_EDIT_REDO, OnRedo)
		COMMAND_ID_HANDLER(ID_EDIT_DELETE, OnDelete)

		COMMAND_ID_HANDLER(ID_EDIT_FINDNEXT, OnFindNext)
		
		COMMAND_ID_HANDLER(ID_EDITOR_WORDWRAP, OnWordWrapToggle)
		COMMAND_ID_HANDLER(ID_EDITOR_COLOURISE, OnColouriseToggle)
		COMMAND_ID_HANDLER(ID_EDITOR_LINENOS, OnLineNoToggle)

		COMMAND_ID_HANDLER(ID_FILE_SAVE_AS, OnSaveAs)
		COMMAND_ID_HANDLER(ID_FILE_SAVE, OnSave)
		COMMAND_ID_HANDLER(ID_FILE_CLOSE, OnClose)

		COMMAND_ID_HANDLER(ID_EDIT_GOTO, OnGoto)

		COMMAND_ID_HANDLER(ID_TOOLS_LECRLF, OnLineEndingsToggle)
		COMMAND_ID_HANDLER(ID_TOOLS_LELF, OnLineEndingsToggle)
		COMMAND_ID_HANDLER(ID_TOOLS_LECR, OnLineEndingsToggle)
		COMMAND_ID_HANDLER(ID_TOOLS_LECONVERT, OnLineEndingsConvert)

		NOTIFY_CODE_HANDLER(TBN_GETINFOTIP, OnGetInfoTip)

		IMPLEMENT_FROMHANDLE()

		LOCAL_MENUCOMMAND(MENUMESSAGE_CHANGESCHEME)
		//ROUTE_MENUCOMMANDS()
		
		// Chaining
		CHAIN_MSG_MAP(baseClass)
		CHAIN_CLIENT_COMMANDS ()
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	BEGIN_MENU_HANDLER_MAP()
		HANDLE_MENU_COMMAND(MENUMESSAGE_CHANGESCHEME, OnSchemeChange)
	END_MENU_HANDLER_MAP()

	struct _PoorMansUIEntry
	{
		UINT nID;
		WORD wState;
	};

	static _PoorMansUIEntry* GetDefaultUIMap()
	{
		static _PoorMansUIEntry theMap[] =
		{
			{ID_EDITOR_WORDWRAP, PMUI_MINIBAR | PMUI_MENU},
			{ID_EDITOR_COLOURISE, PMUI_MINIBAR | PMUI_MENU},
			{ID_EDITOR_LINENOS, PMUI_MINIBAR | PMUI_MENU},
			// note: This one must be at the end.
			{-1, 0}
		};
		return theMap;
	}

	_PoorMansUIEntry* GetPoorMansUIMap()
	{
		return m_pUIData;
	}

	void UISetChecked(UINT uID, bool bChecked, bool bUpdate = true)
	{
		_PoorMansUIEntry* pMap = GetPoorMansUIMap();
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

	bool UIGetChecked(UINT uID)
	{
		_PoorMansUIEntry* pMap = GetPoorMansUIMap();
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

	bool UIInvertCheck(UINT uID)
	{
		_PoorMansUIEntry* pMap = GetPoorMansUIMap();
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

	void InitUpdateUI()
	{
		_PoorMansUIEntry* pMap = GetDefaultUIMap();
		int so = sizeof(pMap);
		m_pUIData = new _PoorMansUIEntry[so];
		memcpy(m_pUIData, pMap, sizeof(_PoorMansUIEntry)*so);
	}

	CChildFrame()
	{
		m_hImgList = NULL;
		m_FileAge = -1;

		InitUpdateUI();
	}

	~CChildFrame()
	{
		if(m_hImgList)
		{
			::ImageList_Destroy(m_hImgList);
		}

		delete [] m_pUIData;
	}

	/**
	 * We override UpdateLayout in order to call UpdateBarsPosition in this class,
	 * instead of in CFrameWindowImplBase. This way we can automagically have a toolbar
	 * at the bottom of the window. 
	 */
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

		UISetChecked(ID_EDITOR_COLOURISE, true);
		UISetChecked(ID_EDITOR_WORDWRAP, false);
		UISetChecked(ID_EDITOR_LINENOS, false);

		UpdateMenu();

		bHandled = FALSE;
		return 1;
	}
	
	LRESULT OnMDIActivate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
	{
		UpdateMenu();
		CheckAge();
		bHandled = FALSE;
		return 0;
	}

	LRESULT OnCheckAge(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
	{
		CheckAge();

		return 0;
	}

	LRESULT OnPaint(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
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

	LRESULT OnEraseBackground(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
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

	LRESULT OnDelete(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		m_view.DeleteBack();
		return TRUE;
	}

	LRESULT OnFindNext(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
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

	LRESULT OnGetInfoTip(int idCtrl, LPNMHDR pnmh, BOOL& bHandled)
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
	// Command Handlers

	LRESULT OnSaveAs(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		SaveAs();

		return 0;
	}

	LRESULT OnSave(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		Save();

		return 0;
	}

	LRESULT OnClose(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		PostMessage(WM_CLOSE, 0, 0);
		return 0;
	}


	LRESULT OnWordWrapToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		m_view.SetWrapMode( UIInvertCheck(wID) ? SC_WRAP_WORD : SC_WRAP_NONE );

		return 0;
	}

	LRESULT OnColouriseToggle(WORD /*wNotifyCode*/, WORD wID, HWND hWndCtl, BOOL& /*bHandled*/)
	{
		m_view.EnableHighlighting(UIInvertCheck(wID));
		
		return 0;
	}

	LRESULT OnLineNoToggle(WORD /*wNotifyCode*/, WORD wID, HWND hWndCtl, BOOL& /*bHandled*/)
	{
		m_view.ShowLineNumbers(UIInvertCheck(wID));

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

	LRESULT OnLineEndingsToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		if(wID == ID_TOOLS_LECRLF)
			m_view.SetEOLMode((int)PNSF_Windows);
		else if(wID == ID_TOOLS_LELF)
			m_view.SetEOLMode((int)PNSF_Unix);
		else
			m_view.SetEOLMode((int)PNSF_Mac);
		UpdateMenu();
		return 0;
	}

	LRESULT OnLineEndingsConvert(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		if(MessageBox(_T("Are you sure you wish to convert the line endings\nin this file to the currently selected type?"), 
			_T("Programmers Notepad 2"), MB_YESNO | MB_ICONQUESTION) == IDYES)
		{
			m_view.ConvertEOLs(m_view.GetEOLMode());
		}

		return 0;
	}

	void OnSchemeChange(LPVOID pVoid)
	{
		SetScheme(static_cast<CScheme*>(pVoid));
	}

	////////////////////////////////////////////////////
	// File Management Methods
	
	void CheckAge()
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

	void Revert()
	{
		// Check that we have a valid filename first...
		if(CanSave())
		{
			//@todo maybe flag this instead of re-applying the scheme (a little un-necessary)
			m_view.Load((LPCTSTR)m_FileName, m_view.GetCurrentScheme());
			m_FileAge = FileAge(m_FileName);
		}
	}

	void PNOpenFile(LPCTSTR pathname, LPCTSTR filename, CScheme* pScheme = NULL)
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
	}

	void SaveFile(LPCTSTR pathname, bool bStoreFilename = true, bool bUpdateMRU = true)
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

	bool CanSave()
	{
		return ((m_FileName != _T("")) && (m_FileName.Find(_T("<")) == -1));
	}

	bool SaveAs()
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

		return bRet;
	}

	void ChangeFormat(EPNSaveFormat format)
	{
		m_view.SetEOLMode( (int)format );
		m_view.ConvertEOLs( (int)format );
		//@todo Update menu item...
		UpdateMenu();
	}

	void Save()
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

	bool FindNext(SFindOptions* options)
	{
		return m_view.FindNext(options);
	}

	bool Replace(SReplaceOptions* options)
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
		g_Context.m_frame->SetActiveScheme(m_hWnd, static_cast<LPVOID>(pScheme));
	}

	void UpdateMenu()
	{
		CSMenuHandle menu(m_hMenu);

		EPNSaveFormat f = (EPNSaveFormat)m_view.GetEOLMode();
		
		menu.CheckMenuItem(ID_TOOLS_LECRLF, f == PNSF_Windows);
		menu.CheckMenuItem(ID_TOOLS_LECR, f == PNSF_Mac);
		menu.CheckMenuItem(ID_TOOLS_LELF, f == PNSF_Unix);

		g_Context.m_frame->SetActiveScheme(m_hWnd, m_view.GetCurrentScheme());
	}

protected:
	HIMAGELIST m_hImgList;
	CTextView m_view;
	CString m_Title;
	CString m_FileName;
	long m_FileAge;

	_PoorMansUIEntry* m_pUIData;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(CHILDFRM_H__INCLUDED)
