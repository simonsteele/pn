/**
 * @file OutputView.h
 * @brief View to display output from tool calls.
 * @author Simon Steele
 * @note Copyright (c) 2002-2005 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef outputview_h__included
#define outputview_h__included

#include "outputscintilla.h"
#include "ScintillaWTL.h"

/**
 * Scintilla window with special output handling.
 */
class COutputView : public CScintillaWindowImpl< COutputView, REScintilla >
{
typedef CScintillaWindowImpl< COutputView, REScintilla > baseClass;

public:
	COutputView();
	~COutputView();

	BEGIN_MSG_MAP(COutputView)
		COMMAND_ID_HANDLER(ID_OUTPUT_CLEAR, OnClear)
		COMMAND_ID_HANDLER(ID_OUTPUT_HIDE, OnHide)
		COMMAND_ID_HANDLER(ID_EDIT_CUT, OnCut)
		COMMAND_ID_HANDLER(ID_EDIT_COPY, OnCopy)
		MESSAGE_HANDLER(PN_HANDLEHSCLICK, OnHotSpotClicked)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	void DoContextMenu(CPoint* point);

	void SafeAppendText(LPCSTR s, int len = -1, bool bScrollToView = true);

	virtual int HandleNotify(LPARAM lParam);

// Implement IToolOutputSink
public:
	virtual void AddToolOutput(LPCTSTR output, int nLength = -1);
	virtual void SetToolBasePath(LPCTSTR path);
	virtual void SetToolParser(bool bBuiltIn, LPCTSTR customExpression = NULL);
	virtual void ClearOutput();

protected:
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

	bool HandleREError(PCRE::RegExp& re, int style, int position);
	bool BuildAndHandleREError(int style, int position, const char* reDef);

	LRESULT OnClear(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCut(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCopy(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	virtual void OnFirstShow();

	void SetOutputLexer();
	void SetCustomLexer();

protected:
	bool			m_bCustom;
	tstring			m_basepath;
};

#endif