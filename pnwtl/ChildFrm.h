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

#define PMUI_MINIBAR	0x0001
#define PMUI_MENU		0x0002
#define PMUI_CHECKED	0x0004

#include "fromhandle.h"
#include "include/wtlsplitter.h"
#include "textview.h"
#include "tools.h"

typedef enum {FN_FULL, FN_FILE, FN_PATH, FN_FILEPART} EGFNType;
typedef enum {EP_LINE, EP_COL} EGPType;

class COutputView;

#define CHAIN_OUTPUT_COMMANDS() \
	if(uMsg == WM_COMMAND && m_hWndOutput != NULL) \
		::SendMessage(m_hWndOutput, uMsg, wParam, lParam);

class CChildFrame : public CTabbedMDIChildWindowImpl<CChildFrame>, 
	public CFromHandle<CChildFrame>, public CSMenuEventHandler,
	public ToolOwner<CChildFrame>
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
		MESSAGE_HANDLER(PN_OPTIONSUPDATED, OnOptionsUpdate)
		MESSAGE_HANDLER(PN_TOOLFINISHED, OnToolFinished)
		MESSAGE_HANDLER(PN_SCHEMECHANGED, OnSchemeChanged)

		COMMAND_ID_HANDLER(ID_EDIT_CUT, OnCut)
		COMMAND_ID_HANDLER(ID_EDIT_COPY, OnCopy)
		COMMAND_ID_HANDLER(ID_EDIT_PASTE, OnPaste)
		COMMAND_ID_HANDLER(ID_EDIT_UNDO, OnUndo)
		COMMAND_ID_HANDLER(ID_EDIT_REDO, OnRedo)
		COMMAND_ID_HANDLER(ID_EDIT_DELETE, OnDelete)
		COMMAND_ID_HANDLER(ID_EDIT_GOTO, OnGoto)
		COMMAND_ID_HANDLER(ID_EDIT_FINDNEXT, OnFindNext)
		COMMAND_ID_HANDLER(ID_EDIT_COPYRTF, OnCopyRTF)
		
		COMMAND_ID_HANDLER(ID_EDITOR_WORDWRAP, OnWordWrapToggle)
		COMMAND_ID_HANDLER(ID_EDITOR_COLOURISE, OnColouriseToggle)
		COMMAND_ID_HANDLER(ID_EDITOR_LINENOS, OnLineNoToggle)
		COMMAND_ID_HANDLER(ID_EDITOR_WHITESPACE, OnMarkWhiteSpaceToggle)
		COMMAND_ID_HANDLER(ID_EDITOR_EOLCHARS, OnEOLMarkerToggle)

		COMMAND_ID_HANDLER(ID_OUTPUT_HIDE, OnHideOutput)

		COMMAND_ID_HANDLER(ID_VIEW_INDIVIDUALOUTPUT, OnIndividualOutputToggle);

		COMMAND_ID_HANDLER(ID_FILE_REVERT, OnRevert)
		COMMAND_ID_HANDLER(ID_FILE_SAVE_AS, OnSaveAs)
		COMMAND_ID_HANDLER(ID_FILE_SAVE, OnSave)
		COMMAND_ID_HANDLER(ID_FILE_CLOSE, OnClose)
		COMMAND_ID_HANDLER(ID_FILE_PRINT, OnPrint)
		COMMAND_ID_HANDLER(ID_FILE_PRINT_SETUP, OnPrintSetup)

		COMMAND_ID_HANDLER(ID_EXPORT_RTF, OnExportRTF)

		COMMAND_ID_HANDLER(ID_TOOLS_LECRLF, OnLineEndingsToggle)
		COMMAND_ID_HANDLER(ID_TOOLS_LELF, OnLineEndingsToggle)
		COMMAND_ID_HANDLER(ID_TOOLS_LECR, OnLineEndingsToggle)
		COMMAND_ID_HANDLER(ID_TOOLS_LECONVERT, OnLineEndingsConvert)
		COMMAND_ID_HANDLER(ID_TOOLS_STOPTOOLS, OnStopTools)
		COMMAND_ID_HANDLER(ID_TOOLS_USETABS, OnUseTabs)

		COMMAND_ID_HANDLER(ID_EDIT_HEADERSWITCH, OnHeaderSwitch)

		NOTIFY_CODE_HANDLER(TBN_GETINFOTIP, OnGetInfoTip)

		IMPLEMENT_FROMHANDLE()
		IMPLEMENT_TOOLOWNER()

		LOCAL_MENUCOMMAND(MENUMESSAGE_CHANGESCHEME)
		LOCAL_MENUCOMMAND(TOOLS_RUNTOOL)
		//ROUTE_MENUCOMMANDS()
		
		// Chaining
		CHAIN_MSG_MAP(baseClass)
		CHAIN_CLIENT_COMMANDS()
		CHAIN_OUTPUT_COMMANDS()
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	BEGIN_MENU_HANDLER_MAP()
		HANDLE_MENU_COMMAND(MENUMESSAGE_CHANGESCHEME, OnSchemeChange)
		HANDLE_MENU_COMMAND(TOOLS_RUNTOOL, OnRunTool)
	END_MENU_HANDLER_MAP()

	////////////////////////////////////////////////////
	// Window Layout

	void UpdateLayout(BOOL bResizeBars = TRUE);
	void UpdateBarsPosition(RECT& rect, BOOL bResizeBars = TRUE);
	void SetupToolbar();

public:
    
	////////////////////////////////////////////////////
	// PoorMansUI

	struct _PoorMansUIEntry
	{
		int nID;
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

	bool CanClose();

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
	LRESULT OnOptionsUpdate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnToggleOutput(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnToolFinished(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnSchemeChanged(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	
	////////////////////////////////////////////////////
	// Command Handlers

	LRESULT OnPrint(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnPrintSetup(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnExportRTF(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCut(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCopy(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnPaste(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnUndo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnRedo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnDelete(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFindNext(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCopyRTF(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnRevert(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnSaveAs(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnSave(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnClose(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWordWrapToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnColouriseToggle(WORD /*wNotifyCode*/, WORD wID, HWND hWndCtl, BOOL& /*bHandled*/);
	LRESULT OnLineNoToggle(WORD /*wNotifyCode*/, WORD wID, HWND hWndCtl, BOOL& /*bHandled*/);
	LRESULT OnIndividualOutputToggle(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnMarkWhiteSpaceToggle(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnEOLMarkerToggle(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnHideOutput(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnGoto(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnLineEndingsToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnLineEndingsConvert(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnStopTools(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnUseTabs(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnHeaderSwitch(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

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

	CTextView* GetTextView();

	////////////////////////////////////////////////////
	// ToolOwner needed methods.

	void UpdateRunningTools();
	void ToggleOutputWindow(bool bSetValue = false, bool bSetShowing = true);
	void AddToolOutput(LPCSTR outputstring, int nLength = -1);

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
	void LoadExternalLexers();
	void PrintSetup();
	void SchemeChanged(CScheme* pScheme);
	void UpdateTools(CScheme* pScheme);
	bool IsOutputVisible();
	BOOL OnEscapePressed();
	void Export(int type);

	// Modes Stuff
protected:
	//cpp
	bool				m_bHeaderSwitch;

protected:
	static bool			s_bFirstChild;
	HWND				m_hWndOutput;
	HIMAGELIST			m_hImgList;
	CTextView			m_view;
	CString				m_Title;
	CString				m_FileName;
	long				m_FileAge;
	
	int					m_iFirstToolCmd;

	CCFSplitter*		m_pSplitter;
	COutputView*		m_pOutputView;

	///@todo move this into COptionsManager
	SPrintOptions		m_po;

	_PoorMansUIEntry*	m_pUIData;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(CHILDFRM_H__INCLUDED)
