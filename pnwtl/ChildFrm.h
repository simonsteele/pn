/**
 * @file ChildFrm.h
 * @brief Interface Definition for CChildFrame, the MDI Child window.
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#if !defined(CHILDFRM_H__INCLUDED)
#define CHILDFRM_H__INCLUDED

#pragma once

#define MINI_BAR_HEIGHT 17

#define MENUMESSAGE_CHANGESCHEME 0xa

#define PMUI_MINIBAR	0x0001
#define PMUI_MENU		0x0002
#define PMUI_CHECKED	0x0004

#include "fromhandle.h"
#include "include/wtlsplitter.h"
#include "textview.h"
#include "jumpview.h"
#include "tools.h"
#include "resource.h"
#include "controls/commandbaredit.h"

typedef enum {EP_LINE, EP_COL} EGPType;

class COutputView;
class DocScript;
class AutoCompleteManager;
namespace TextClips { class TextClipsManager; }

#define CHAIN_OUTPUT_COMMANDS() \
	if(uMsg == WM_COMMAND && m_hWndOutput != NULL) \
		::SendMessage(m_hWndOutput, uMsg, wParam, lParam);

#define CHAIN_PN_CLIENT_COMMANDS() \
	if(uMsg == WM_COMMAND && m_hWndClient != NULL) \
		::SendMessage(m_focusView->GetHwnd(), uMsg, wParam, lParam);

/**
 * @brief Programmers Notepad 2 MDI Child window.
 */
class CChildFrame : public CTabbedMDIChildWindowImpl<CChildFrame>, 
	public CFromHandle<CChildFrame>, public CommandEventHandler
{
public:
	DECLARE_FRAME_WND_CLASS(NULL, IDR_MDICHILD)
	typedef CTabbedMDIChildWindowImpl<CChildFrame> baseClass;

	CChildFrame(DocumentPtr doc, CommandDispatch* commands, TextClips::TextClipsManager* textclips, AutoCompleteManager* autoComplete);
	virtual ~CChildFrame();
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
		MESSAGE_HANDLER(PN_TOOLRUNUPDATE, OnToolFinished)
		MESSAGE_HANDLER(PN_SCHEMECHANGED, OnSchemeChanged)
		MESSAGE_HANDLER(PN_GOTOLINE, OnGotoLine)
		MESSAGE_HANDLER(PN_PROJECTNOTIFY, OnProjectNotify)
		MESSAGE_HANDLER(PN_COMPLETECLIP, OnCompleteClip)
		MESSAGE_HANDLER(UWM_MDICHILDISMODIFIED, OnChildIsModified)
		MESSAGE_HANDLER(UWM_MDICHILDSAVEMODIFIED, OnChildSaveModified)
		MESSAGE_HANDLER(UWM_MDICHILDSHOWTABCONTEXTMENU, OnShowTabContextMenu)
		MESSAGE_HANDLER(UWM_MDICHILDCLOSEWITHNOPROMPT, OnCloseNoPrompt)

		//Now handled globally: Cut, Copy, Paste, Undo
		//Still handled locally: Redo...
		COMMAND_ID_HANDLER(ID_EDIT_REDO, OnRedo)
		COMMAND_ID_HANDLER(ID_EDIT_DELETE, OnDelete)
		COMMAND_ID_HANDLER(ID_EDIT_GOTO, OnGoto)
		COMMAND_ID_HANDLER(ID_EDIT_FINDNEXT, OnFindNext)
		COMMAND_ID_HANDLER(ID_EDIT_FINDPREVIOUS, OnFindPrevious)
		COMMAND_ID_HANDLER(ID_SEARCH_FINDNEXTCURRENTWORD, OnFindNextWordUnderCursor)
		COMMAND_ID_HANDLER(ID_SEARCH_FINDPREVIOUSCURRENTWORD, OnFindPrevWordUnderCursor)

		COMMAND_ID_HANDLER(ID_EDIT_COPYRTF, OnCopyRTF)
		COMMAND_ID_HANDLER(ID_EDIT_AUTOCOMPLETE, OnAutoComplete)
		COMMAND_ID_HANDLER(ID_EDIT_COPYFILEPATH, OnCopyFilePath)
		COMMAND_ID_HANDLER(ID_EDIT_INSERTCLIP, OnInsertClip)
		COMMAND_ID_HANDLER(ID_EDIT_JUMPTO, OnJumpTo)
		
		COMMAND_ID_HANDLER(ID_EDITOR_WORDWRAP, OnWordWrapToggle)
		COMMAND_ID_HANDLER(ID_EDITOR_COLOURISE, OnColouriseToggle)
		COMMAND_ID_HANDLER(ID_EDITOR_LINENOS, OnLineNoToggle)
		COMMAND_ID_HANDLER(ID_EDITOR_WHITESPACE, OnMarkWhiteSpaceToggle)
		COMMAND_ID_HANDLER(ID_EDITOR_EOLCHARS, OnEOLMarkerToggle)
		COMMAND_ID_HANDLER(ID_EDITOR_WRITEPROTECT, OnWriteProtectToggle)

		COMMAND_ID_HANDLER(ID_EDITOR_USEASSCRIPT, OnUseAsScript)

		COMMAND_ID_HANDLER(ID_EDITOR_GOTODEFINITION, OnGoToDef)

		COMMAND_ID_HANDLER(ID_OUTPUT_HIDE, OnHideOutput)

		COMMAND_ID_HANDLER(ID_VIEW_INDIVIDUALOUTPUT, OnIndividualOutputToggle);

		COMMAND_ID_HANDLER(ID_FILE_REVERT, OnRevert)
		COMMAND_ID_HANDLER(ID_FILE_SAVE_AS, OnSaveAs)
		COMMAND_ID_HANDLER(ID_FILE_SAVE, OnSave)
		COMMAND_ID_HANDLER(ID_FILE_CLOSE, OnClose)
		COMMAND_ID_HANDLER(ID_FILE_PRINT, OnPrint)
		COMMAND_ID_HANDLER(ID_FILE_PRINT_SETUP, OnPrintSetup)
		COMMAND_ID_HANDLER(ID_FILE_OPENCONTAININGFOLDER, OnOpenContainingFolder)
		COMMAND_ID_HANDLER(ID_FILE_SHELLOPEN, OnShellOpen)

		COMMAND_ID_HANDLER(ID_EXPORT_RTF, OnExportRTF)
		COMMAND_ID_HANDLER(ID_EXPORT_HTML, OnExportHTML)

		COMMAND_ID_HANDLER(ID_TOOLS_LECRLF, OnLineEndingsToggle)
		COMMAND_ID_HANDLER(ID_TOOLS_LELF, OnLineEndingsToggle)
		COMMAND_ID_HANDLER(ID_TOOLS_LECR, OnLineEndingsToggle)
		COMMAND_ID_HANDLER(ID_TOOLS_LECONVERT, OnLineEndingsConvert)
		COMMAND_ID_HANDLER(ID_TOOLS_STOPTOOLS, OnStopTools)
		COMMAND_ID_HANDLER(ID_TOOLS_USETABS, OnUseTabs)        

		COMMAND_ID_HANDLER(ID_EDIT_HEADERSWITCH, OnHeaderSwitch)

		COMMAND_ID_HANDLER(ID_VIEW_FILEPROPERTIES, OnViewFileProps)

		COMMAND_ID_HANDLER(ID_PROJECT_ADDTHISFILE, OnProjectAddFile)

		COMMAND_ID_HANDLER(ID_WINDOW_SPLITHORIZONTAL, OnSplitHorizontal)
		COMMAND_ID_HANDLER(ID_WINDOW_SPLITVERTICAL, OnSplitVertical)
		COMMAND_ID_HANDLER(ID_WINDOW_CLOSESPLIT, OnCloseSplit)

		COMMAND_ID_HANDLER(ID_FILE_CLOSEALL, OnFileCloseAll)
		COMMAND_ID_HANDLER(ID_WINDOW_CLOSEALLOTHER, OnWindowCloseAllOther)

		COMMAND_ID_HANDLER(ID_EDIT_FOCUSCOMMAND, OnFocusCommand)
		COMMAND_ID_HANDLER(ID_WINDOWS_CURRENTEDITOR, OnWindowsCurrentEditor)
		COMMAND_HANDLER(cwCommandWnd, EN_CHANGE, OnCommandNotify)
		COMMAND_HANDLER(cwCommandWnd, BN_CLICKED, OnCommandEnter)
		COMMAND_HANDLER(cwCommandWnd, BN_SETFOCUS, OnCommandGotFocus)
		COMMAND_HANDLER(cwCommandWnd, BN_KILLFOCUS, OnCommandLostFocus)

		COMMAND_RANGE_HANDLER(ID_ENCODING_8, ID_ENCODING_UTF8NOBOM, OnEncodingSelect)

		NOTIFY_CODE_HANDLER(TBN_GETINFOTIP, OnGetInfoTip)

		IMPLEMENT_FROMHANDLE()

		LOCAL_MENUCOMMAND(PN_COMMAND_EDITOR)
		LOCAL_MENUCOMMAND(MENUMESSAGE_CHANGESCHEME)
		LOCAL_MENUCOMMAND(TOOLS_RUNTOOL)
		//ROUTE_MENUCOMMANDS()
		
		// Chaining
		CHAIN_MSG_MAP(baseClass)
		CHAIN_PN_CLIENT_COMMANDS()
		CHAIN_OUTPUT_COMMANDS()
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	BEGIN_MENU_HANDLER_MAP()
		HANDLE_MENU_COMMAND(PN_COMMAND_EDITOR, OnEditorCommand)
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

	DocumentPtr GetDocument() const;

	void SetTitle(bool bModified = false);
	tstring GetFileName(EGFNType type = FN_FULL);
	tstring GetTitle();
	bool GetModified();
	bool GetWriteProtect();

	bool CanClose();

	////////////////////////////////////////////////////
	// Message Handlers

	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);	
	LRESULT OnMDIActivate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);
	LRESULT OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);
	LRESULT OnCloseNoPrompt(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);
	LRESULT OnPaint(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnEraseBackground(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnForwardMsg(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);
	LRESULT OnCheckAge(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);
	LRESULT OnViewNotify(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/);
	LRESULT OnOptionsUpdate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnToggleOutput(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnToolFinished(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnSchemeChanged(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnChildIsModified(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnChildSaveModified(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnShowTabContextMenu(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnGotoLine(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam/**/, BOOL& /*bHandled*/);
	LRESULT OnProjectNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);
	LRESULT OnCompleteClip(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);
	
	////////////////////////////////////////////////////
	// Command Handlers

	LRESULT OnPrint(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnPrintSetup(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnOpenContainingFolder(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnShellOpen(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnExportRTF(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnExportHTML(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnRedo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnDelete(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFindNext(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFindPrevious(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFindNextWordUnderCursor(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFindPrevWordUnderCursor(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCopyRTF(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnAutoComplete(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
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
	LRESULT OnUseAsScript(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnGoToDef(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);    
	LRESULT OnHideOutput(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnGoto(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnLineEndingsToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnLineEndingsConvert(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnStopTools(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnUseTabs(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnHeaderSwitch(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnEncodingSelect(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnViewFileProps(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnProjectAddFile(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnSplitHorizontal(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnSplitVertical(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCloseSplit(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFocusCommand(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCommandNotify(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCommandEnter(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCommandGotFocus(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCommandLostFocus(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCopyFilePath(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnInsertClip(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnJumpTo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWriteProtectToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWindowCloseAllOther(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFileCloseAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWindowsCurrentEditor(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	
	bool OnRunTool(LPVOID pTool);

	void SetReadOnly(bool readonly);

	////////////////////////////////////////////////////
	// Notify Handlers

	LRESULT OnGetInfoTip(int idCtrl, LPNMHDR pnmh, BOOL& bHandled);

	////////////////////////////////////////////////////
	// File Management Methods
	
	void CheckAge();
	void Revert();
	bool PNOpenFile(LPCTSTR pathname, Scheme* pScheme = NULL, EPNEncoding encoding = eUnknown);
	bool SaveFile(LPCTSTR pathname, bool ctagsRefresh = false, bool bStoreFilename = true, bool bUpdateMRU = true);
	bool CanSave();
	bool SaveAs(bool ctagsRefresh);
	void ChangeFormat(EPNSaveFormat format);
	bool Save(bool ctagsRefresh);

	////////////////////////////////////////////////////
	// Editor Window Methods

	FindNextResult FindNext(extensions::ISearchOptions* options);
	void MarkAll(extensions::ISearchOptions* options);
	bool Replace(extensions::ISearchOptions* options);
	int ReplaceAll(extensions::ISearchOptions* options);
	int GetPosition(EGPType type);
	void SetPosStatus(CMultiPaneStatusBarCtrl&	stat);
	bool OnSchemeChange(LPVOID pVoid);
	bool OnEditorCommand(LPVOID pCommand);
	int GetLinePosition(int line);
	
	void SetScheme(Scheme* pScheme, bool allSettings = true);
	void UpdateMenu();

	CTextView* GetTextView();
	COutputView* GetOutputWindow();

	HACCEL GetToolAccelerators();

	////////////////////////////////////////////////////
	// ToolOwner needed methods.

	void EnsureOutputWindow();
	void ToggleOutputWindow(bool bSetValue = false, bool bSetShowing = true);

	////////////////////////////////////////////////////
	// Autocomplete methods
	
	bool InsertClipCompleted(Scintilla::SCNotification* notification);

	////////////////////////////////////////////////////
	// Recording methods

	void StartRecord(extensions::IRecorderPtr recorder);
	void StopRecord();
	bool IsRecording();

	// View Management
	void SetLastView(Views::ViewPtr& view);

private:
	// Window IDs we use, designed to avoid the WM_COMMAND ctrl id range
	enum tagChildWindowIds
	{
		cwScintilla = 0x10000,
		cwMinibar = 0x20000,
		cwOutputView = 0x30000,
		cwSplitter = 0x40000,
		cwViewSplitter = 0x50000,
		
		// This one we want commands from:
		cwCommandWnd = 0x1000
	};

	void LoadExternalLexers();
	void PrintSetup();
	void SchemeChanged(Scheme* pScheme);
	void UpdateTools(Scheme* pScheme);
	bool IsOutputVisible();
	BOOL OnEscapePressed();
	void Export(int type);
	void SetModifiedOverride(bool bVal);
	int HandleFailedFileOp(LPCTSTR filename, bool bOpening);
	IFilePtr attemptOverwrite(LPCTSTR filename);
	void handleClose();
	void resetSaveDir();
	void setReadOnly(bool newValue, bool setAttributes);
	void findNextWordUnderCursor(bool backwards);
	void updateViewKeyBindings();
	void splitSelectedView(bool horizontal);
	void removeSplit(bool closeCurrent);
	void setMDIFrameMenu();
	HMENU getWindowMenu();
	bool insertMatchingClip(const char* word);
	bool canConvertEncoding();
	void insertClip(const TextClips::Clip* clip);

	CommandDispatch*	m_pCmdDispatch;
	DocumentPtr			m_spDocument;
	static bool			s_bFirstChild;
	HIMAGELIST			m_hImgList;
	uint64_t			m_FileAge;
	bool				m_bModifiedOverride;
	bool				m_bClosing;
	bool				m_bReadOnly;
	bool				m_bIgnoreUpdates;
	bool				m_bHandlingCommand;
	bool				m_bReadOnlyOverride;
	HWND				m_hWndOutput;
	CCommandBarEdit		m_cmdTextBox;
	TextClips::TextClipsManager*	m_pTextClips;
	
	int					m_iFirstToolCmd;

	AutoCompleteManager* m_autoComplete;

	DocScript*			m_pScript;

	/**
	 * The prime view at any point is the virtual-root of the view tree, it sits
	 * just above base view, but base view doesn't know anything about it. This gets
	 * changed out when we split the top-level view.
	 */
	Views::ViewPtr		m_primeView;
	
	/**
	 * This is the last focused view, regardless of type. Not really needed with
	 * only one focusable view type, but this will expand in the future.
	 */
	Views::ViewPtr		m_focusView;

	/**
	 * Pointer to the last text view that was focused, this allows us to perform
	 * edit commands where the user would expect them to be done.
	 */
	Views::ViewPtr		m_lastTextView;
	
	/**
	 * Pointer to our base view instance - this is the root of the view
	 * tree, and never changes. It feeds CChildFrame view focus notifications.
	 */
	Views::ViewPtr		m_baseView;

	/**
	 * If you're using individual output windows then this view will get inserted
	 * below the top-level view to split and display the output window.
	 */
	Views::ViewPtr		m_outputView;

	///@todo move this into COptionsManager
	SPrintOptions		m_po;

	_PoorMansUIEntry*	m_pUIData;
};

#endif // !defined(CHILDFRM_H__INCLUDED)
