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

#include "ScintillaWTL.h"

#define PN_HANDLEHSCLICK	(WM_USER+20)

class COutputView : public CScintillaWindowImpl< COutputView, CScintillaImpl >
{
public:
	typedef CScintillaWindowImpl< COutputView, CScintillaImpl > baseClass;

	BEGIN_MSG_MAP(COutputView)
		COMMAND_ID_HANDLER(ID_OUTPUT_CLEAR, OnClear)
		MESSAGE_HANDLER(PN_HANDLEHSCLICK, OnHotSpotClicked)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	void DoContextMenu(CPoint* point)
	{
		CSPopupMenu popup(IDR_POPUP_OUTPUT);
		g_Context.m_frame->TrackPopupMenu(popup, 0, point->x, point->y, NULL);
	}

	void SafeAppendText(LPCSTR s, int len = -1)
	{
		if(len == -1)
			len = strlen(s);
		SendMessage(SCI_APPENDTEXT, len, reinterpret_cast<LPARAM>(s));

		int line = SendMessage(SCI_GETLENGTH, 0, 0);
		line = SendMessage(SCI_LINEFROMPOSITION, line, 0);
		SendMessage(SCI_ENSUREVISIBLEENFORCEPOLICY, line);
		SendMessage(SCI_GOTOLINE, line);
	}

	virtual int HandleNotify(LPARAM lParam);

protected:
	LRESULT OnHotSpotClicked(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	void ExtendStyleRange(int startPos, int style, TextRange* tr);
	void HandleGCCError(int style, int position);

	LRESULT OnClear(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		ClearAll();

		return 0;
	}

	virtual void OnFirstShow()
	{
		CSchemeManager::GetInstance()->SchemeByName("output")->Load(*this);
	}
};

#endif