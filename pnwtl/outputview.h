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

/**
 * Scintilla window with special output handling.
 */
class COutputView : public CScintillaWindowImpl< COutputView, CScintillaImpl >
{
public:
	typedef CScintillaWindowImpl< COutputView, CScintillaImpl > baseClass;

	BEGIN_MSG_MAP(COutputView)
		COMMAND_ID_HANDLER(ID_OUTPUT_CLEAR, OnClear)
		MESSAGE_HANDLER(PN_HANDLEHSCLICK, OnHotSpotClicked)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	void DoContextMenu(CPoint* point);

	void SafeAppendText(LPCSTR s, int len = -1);

	virtual int HandleNotify(LPARAM lParam);

protected:
	LRESULT OnHotSpotClicked(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	void ExtendStyleRange(int startPos, int style, TextRange* tr);
	void HandleGCCError(int style, int position);

	LRESULT OnClear(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	virtual void OnFirstShow();
};

/**
 * @brief Docking frame for the tool output view.
 */
class CDockingOutputWindow : public CPNDockingWindow<CDockingOutputWindow>
{
	typedef CDockingOutputWindow thisClass;
	typedef CPNDockingWindow<CDockingOutputWindow> baseClass;

public:
	DECLARE_WND_CLASS(_T("CDockingOutputWindow"))

	BEGIN_MSG_MAP(thisClass)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_SIZE, OnSize)
		REFLECT_NOTIFICATIONS()
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	COutputView* GetView();

protected:
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);

protected:
	COutputView m_view;
};

#endif