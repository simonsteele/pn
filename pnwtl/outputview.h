/**
 * @file OutputView.h
 * @brief View to display output from tool calls.
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef outputview_h__included
#define outputview_h__included

#include "ScintillaImpl.h"
#include "ScintillaWTL.h"

#define SCE_CUSTOM_ERROR	20

class ScintillaAccessor;

namespace PCRE {
	class RegExp;
}

/**
 * Scintilla window with special output handling.
 */
class COutputView : public CScintillaWindowImpl< COutputView, CScintillaImpl >
{
typedef CScintillaWindowImpl< COutputView, CScintillaImpl > baseClass;

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

	void SafeAppendText(LPCSTR s, int len = -1);

	virtual int HandleNotify(LPARAM lParam);

// Implement IToolOutputSink
public:
	virtual void AddToolOutput(LPCTSTR output, int nLength = -1);
	virtual void SetToolBasePath(LPCTSTR path);
	virtual void SetToolParser(bool bBuiltIn, LPCTSTR customExpression = NULL);
	virtual void ClearOutput();

protected:
	LRESULT OnHotSpotClicked(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	void ExtendStyleRange(int startPos, int style, TextRange* tr);
	
	// Built-in error handlers...
	void HandleGCCError(int style, int position);
	void HandleBorlandCPPError(int style, int position);
	void HandlePerlError(int style, int position);
	void HandlePythonError(int style, int position);
	void HandleMSError(int style, int position);
	
	void HandleCustomError(int style, int position);

	bool LocateInProjects(LPCTSTR part, tstring& full);

	bool HandleREError(PCRE::RegExp& re, int style, int position);
	bool BuildAndHandleREError(int style, int position, const char* reDef);

	void CustomColouriseLine(ScintillaAccessor& styler, char *lineBuffer, int length, int endLine);
	void HandleStyleNeeded(ScintillaAccessor& styler, int startPos, int length);

	

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
	tstring			m_customre;
	PCRE::RegExp*	m_pRE;
};

/**
 * @brief Build regular expressions for tool output matching.
 */
class CToolREBuilder : public CustomFormatStringBuilder<CToolREBuilder>
{
	public:
		void OnFormatChar(TCHAR thechar);
};

#endif