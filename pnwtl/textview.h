/**
 * @file TextView.h
 * @brief Interface Definition for CTextView, the Scintilla based text-editor view.
 * @author Simon Steele
 * @note Copyright (c) 2002-2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef textview_h__included
#define textview_h__included

#pragma once

#include "ScintillaImpl.h"
#include "ScintillaWTL.h"

#include "schemes.h"
#include "pntypes.h"

class FIFSink;

class CTextView : public CScintillaWindowImpl< CTextView, CScintillaImpl >
{
public:
	typedef CScintillaWindowImpl< CTextView, CScintillaImpl > baseClass;

	CTextView(DocumentPtr document, CommandDispatch* commands);
	~CTextView();

	BOOL PreTranslateMessage(MSG* pMsg);

	BEGIN_MSG_MAP(CTextView)
		COMMAND_ID_HANDLER(ID_EDIT_INDENT, OnIndent)
		COMMAND_ID_HANDLER(ID_EDIT_UNINDENT, OnUnindent)
		COMMAND_ID_HANDLER(ID_EDIT_SELECTALL, OnSelectAll)
		COMMAND_ID_HANDLER(ID_BOOKMARKS_NUMBERED_SET, OnSetNumberedBookmark)
		COMMAND_ID_HANDLER(ID_BOOKMARKS_NUMBERED_JUMP, OnNumberedBookmarkJump)
		COMMAND_ID_HANDLER(ID_BOOKMARKS_TOGGLE, OnToggleBookmark)
		COMMAND_ID_HANDLER(ID_BOOKMARKS_NEXT, OnNextBookmark)
		COMMAND_ID_HANDLER(ID_BOOKMARKS_PREVIOUS, OnPrevBookmark)
		COMMAND_ID_HANDLER(ID_VIEW_COLLAPSEALLFOLDS, OnCollapseAll)
		COMMAND_ID_HANDLER(ID_VIEW_EXPANDALLFOLDS, OnExpandAll)
		COMMAND_ID_HANDLER(ID_VIEW_TOGGLEFOLD, OnToggleFold)
		COMMAND_ID_HANDLER(ID_EDIT_GOTOBRACE, OnGotoBrace)
		COMMAND_ID_HANDLER(ID_COMMENTS_LINE, OnCommentLine)
		COMMAND_ID_HANDLER(ID_COMMENTS_STREAM, OnCommentStream)
		COMMAND_ID_HANDLER(ID_COMMENTS_BLOCK, OnCommentBlock)
		COMMAND_ID_HANDLER(ID_COMMENTS_UNCOMMENT, OnUncomment)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	void SetScheme(Scheme* pScheme, bool allSettings = true);
	Scheme* GetCurrentScheme();

	bool Load(LPCTSTR filename, Scheme* pScheme = NULL, EPNEncoding encoding = eUnknown);
	bool Save(LPCTSTR filename, bool bSetScheme = true);

	void Revert(LPCTSTR filename);

	void EnableHighlighting(bool bEnable);
	void SetLineNumberChars(bool bSet = false);
	void ShowLineNumbers(bool bShow);

	void SetPosStatus(CMultiPaneStatusBarCtrl& stat);

	tstring GetCurrentWord();

	EPNEncoding GetEncoding();
	void SetEncoding(EPNEncoding encoding);

	void FindAll(extensions::ISearchOptions* options, FIFSink* sink, LPCTSTR szFilename);
	
	////////////////////////////////////////////////////////////////
	// Overrides from CScintillaImpl / CScintillaWindowImpl
	
	virtual bool OpenFile(LPCTSTR filename, EPNEncoding encoding);
	virtual bool SaveFile(LPCTSTR filename, bool setSavePoint = true);

	void DoContextMenu(CPoint* point);
	virtual int HandleNotify(LPARAM lParam);

	////////////////////////////////////////////////////////////////
	// Command Handlers

	LRESULT OnIndent(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnUnindent(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnSelectAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnSetNumberedBookmark(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnNumberedBookmarkJump(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnToggleBookmark(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnNextBookmark(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnPrevBookmark(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCollapseAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnExpandAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnToggleFold(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnGotoBrace(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCommentLine(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCommentStream(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCommentBlock(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnUncomment(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	void checkLineLength();

protected:
	// Support for document title in printing:
	virtual tstring GetDocTitle();

private:
	bool caretAtBrace(int& posBrace);
	int seekBrace();
	int seekBrace(bool forwards);

	int leastIndentedLine(int startLine, int endLine);
	int lineTextStartPosition(int line);

	void ProcessNumberedBookmark(int n);
	virtual void OnFirstShow();

	CommandDispatch* m_pCmdDispatch;
	BOOL m_waitOnBookmarkNo;
	Scheme* m_pLastScheme;
	EPNEncoding m_encType;
	bool m_bSmartStart;
	bool m_bLineNos;
	DocumentPtr m_pDoc;

	bool m_bMeasureCanRun;
	CRITICAL_SECTION m_csMeasure;
	HANDLE m_hMeasureThread;

	static UINT __stdcall RunMeasureThread(void*);
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif //#ifndef textview_h__included
