/**
 * @file ChildFrm.h
 * @brief Interface Definition for CChildFrame, the MDI Child window.
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
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
#include "include/wtlsplitter.h"

typedef enum {FN_FULL, FN_FILE, FN_PATH, FN_FILEPART} EGFNType;
typedef enum {EP_LINE, EP_COL} EGPType;

class CChildFrame : public CTabbedMDIChildWindowImpl<CChildFrame>, 
	public CFromHandle<CChildFrame>, public CSMenuEventHandler
{
public:
	DECLARE_FRAME_WND_CLASS(NULL, IDR_MDICHILD)
	typedef CTabbedMDIChildWindowImpl<CChildFrame> baseClass;

	CChildFrame();
	~CChildFrame();
	virtual void OnFinalMessage(HWND /*hWnd*/);

	BEGIN_MSG_MAP(CChildFrame)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_FORWARDMSG, OnForwardMsg)
		MESSAGE_HANDLER(WM_CLOSE, OnClose)
		MESSAGE_HANDLER(WM_MDIACTIVATE, OnMDIActivate)
		MESSAGE_HANDLER(WM_ERASEBKGND, OnEraseBackground)
		MESSAGE_HANDLER(WM_PAINT, OnPaint)
		MESSAGE_HANDLER(PN_NOTIFY, OnViewNotify)
		MESSAGE_HANDLER(PN_CHECKAGE, OnCheckAge)

		COMMAND_ID_HANDLER(ID_EDIT_CUT, OnCut)
		COMMAND_ID_HANDLER(ID_EDIT_COPY, OnCopy)
		COMMAND_ID_HANDLER(ID_EDIT_PASTE, OnPaste)
		COMMAND_ID_HANDLER(ID_EDIT_UNDO, OnUndo)
		COMMAND_ID_HANDLER(ID_EDIT_REDO, OnRedo)
		COMMAND_ID_HANDLER(ID_EDIT_DELETE, OnDelete)
		COMMAND_ID_HANDLER(ID_EDIT_GOTO, OnGoto)
		COMMAND_ID_HANDLER(ID_EDIT_FINDNEXT, OnFindNext)
		
		COMMAND_ID_HANDLER(ID_EDITOR_WORDWRAP, OnWordWrapToggle)
		COMMAND_ID_HANDLER(ID_EDITOR_COLOURISE, OnColouriseToggle)
		COMMAND_ID_HANDLER(ID_EDITOR_LINENOS, OnLineNoToggle)
		COMMAND_ID_HANDLER(ID_EDITOR_OUTPUTWND, OnOutputWindowToggle)

		COMMAND_ID_HANDLER(ID_FILE_SAVE_AS, OnSaveAs)
		COMMAND_ID_HANDLER(ID_FILE_SAVE, OnSave)
		COMMAND_ID_HANDLER(ID_FILE_CLOSE, OnClose)
		COMMAND_ID_HANDLER(ID_FILE_PRINT, OnPrint)
		COMMAND_ID_HANDLER(ID_FILE_PRINT_SETUP, OnPrintSetup)

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

	////////////////////////////////////////////////////
	// Window Layout

	void UpdateLayout(BOOL bResizeBars = TRUE);
	void UpdateBarsPosition(RECT& rect, BOOL bResizeBars = TRUE);
	void SetupToolbar();
	void ToggleOutputWindow(bool bSetValue = false, bool bSetShowing = true);

public:
    
	////////////////////////////////////////////////////
	// PoorMansUI

	struct _PoorMansUIEntry
	{
		UINT nID;
		WORD wState;
	};

	static _PoorMansUIEntry* GetDefaultUIMap();

	_PoorMansUIEntry* GetPoorMansUIMap();

	void UISetChecked(UINT uID, bool bChecked, bool bUpdate = true);
	bool UIGetChecked(UINT uID);
	bool UIInvertCheck(UINT uID);
	void InitUpdateUI();

	////////////////////////////////////////////////////
	// Document Entries

	void SetTitle(LPCTSTR sFileName, bool bModified = false);
	tstring GetFileName(EGFNType type = FN_FULL);
	LPCTSTR GetTitle();
	bool GetModified();

	////////////////////////////////////////////////////
	// Message Handlers

	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);	
	LRESULT OnMDIActivate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);
	LRESULT OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);
	LRESULT OnPaint(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnEraseBackground(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnForwardMsg(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);
	LRESULT OnCheckAge(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);
	LRESULT OnViewNotify(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/);
	
	////////////////////////////////////////////////////
	// Command Handlers

	LRESULT OnPrint(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnPrintSetup(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCut(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCopy(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnPaste(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnUndo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnRedo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnDelete(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFindNext(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnSaveAs(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnSave(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnClose(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWordWrapToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnColouriseToggle(WORD /*wNotifyCode*/, WORD wID, HWND hWndCtl, BOOL& /*bHandled*/);
	LRESULT OnLineNoToggle(WORD /*wNotifyCode*/, WORD wID, HWND hWndCtl, BOOL& /*bHandled*/);
	LRESULT OnOutputWindowToggle(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnGoto(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnLineEndingsToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnLineEndingsConvert(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	////////////////////////////////////////////////////
	// Notify Handlers

	LRESULT OnGetInfoTip(int idCtrl, LPNMHDR pnmh, BOOL& bHandled);

	////////////////////////////////////////////////////
	// File Management Methods
	
	void CheckAge();
	void Revert();
	void PNOpenFile(LPCTSTR pathname, LPCTSTR filename, CScheme* pScheme = NULL);
	void SaveFile(LPCTSTR pathname, bool bStoreFilename = true, bool bUpdateMRU = true);
	bool CanSave();
	bool SaveAs();
	void ChangeFormat(EPNSaveFormat format);
	void Save();

	////////////////////////////////////////////////////
	// Editor Window Methods	

	bool FindNext(SFindOptions* options);
	bool Replace(SReplaceOptions* options);
	int ReplaceAll(SReplaceOptions* options);
	void HighlightAll(SFindOptions* options);
	int GetPosition(EGPType type);
	void SetPosStatus(CMultiPaneStatusBarCtrl&	stat);
	void OnSchemeChange(LPVOID pVoid);
	void SetScheme(CScheme* pScheme);
	void UpdateMenu();

	CallbackBase2<bool, CChildFrame*>* m_onClose;

protected:

	/**
	 * Internal class which adjusts the splitter behaviour to take account
	 * of the mini-bar.
	 */
	class CCFSplitter : public CWTLSplitter<CCFSplitter>
	{
	public:
		CCFSplitter(CChildFrame* pFrame) : m_pFrame(pFrame){}
		void GetOwnerClientRect(HWND hOwner, LPRECT lpRect);
	protected:
		CChildFrame* m_pFrame;
	};

protected:
	void PrintSetup();

protected:
	HIMAGELIST		m_hImgList;
	CTextView		m_view;
	CString			m_Title;
	CString			m_FileName;
	long			m_FileAge;

	CCFSplitter*	m_pSplitter;
	CTextView		m_outputView;

	///@todo move this into COptionsManager
	SPrintOptions	m_po;

	_PoorMansUIEntry* m_pUIData;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(CHILDFRM_H__INCLUDED)
