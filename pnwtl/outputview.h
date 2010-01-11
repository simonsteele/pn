/**
 * @file OutputView.h
 * @brief View to display output from tool calls.
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef outputview_h__included
#define outputview_h__included

#include "outputscintilla.h"
#include "ScintillaWTL.h"
#include "views/view.h"

/**
 * Scintilla window with special output handling.
 */
class COutputView : public CScintillaWindowImpl< COutputView, REScintilla >, public extensions::ITextOutput, public Views::View
{
	typedef CScintillaWindowImpl< COutputView, REScintilla > baseClass;
	friend class baseClass;

public:
	explicit COutputView();
	virtual ~COutputView();

	BEGIN_MSG_MAP(COutputView)
		COMMAND_ID_HANDLER(ID_OUTPUT_CLEAR, OnClear)
		COMMAND_ID_HANDLER(ID_OUTPUT_HIDE, OnHide)
		COMMAND_ID_HANDLER(ID_EDIT_CUT, OnCut)
		COMMAND_ID_HANDLER(ID_EDIT_COPY, OnCopy)
		COMMAND_ID_HANDLER(ID_OUTPUT_WORDWRAP, OnWordWrap)
		MESSAGE_HANDLER(PN_HANDLEHSCLICK, OnHotSpotClicked)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	void DoContextMenu(CPoint* point);

	void SafeAppendText(LPCSTR s, int len = -1, bool bScrollToView = true);

	virtual int HandleNotify(LPARAM lParam);

	// Implement ITextOutput
public:
	virtual void AddToolOutput(LPCTSTR output, int nLength = -1);
	virtual void SetToolBasePath(LPCTSTR path);
	virtual void SetToolParser(bool bBuiltIn, const char* customExpression = NULL);
	virtual void ClearOutput();
	virtual void ShowOutput();
	virtual void HideOutput();

	// Implement Views::View
public:
	virtual HWND GetHwnd() { return m_hWnd; }

protected:
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnHotSpotClicked(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	// Built-in error handlers...
	void HandleGCCError(int style, int position);
	void HandleBorlandCPPError(int style, int position);
	void HandlePerlError(int style, int position);
	void HandlePythonError(int style, int position);
	void HandleMSError(int style, int position);
	
	void HandleCustomError(int style, int position);

	bool ExpandMatchedPath(CFileName& fn);
	bool LocateInProjects(LPCTSTR part, tstring& full);

	bool HandleREError(boost::xpressive::sregex& re, int style, int position);
	bool BuildAndHandleREError(int style, int position, const char* reDef);

	LRESULT OnClear(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCut(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCopy(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnWordWrap(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	void OnFirstShow();

	void SetOutputLexer();
	void SetCustomLexer();

private:
	bool			m_bCustom;
	tstring			m_basepath;
};

#endif