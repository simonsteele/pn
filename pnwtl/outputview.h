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

class COutputView : public CScintillaWindowImpl< COutputView, CScintillaImpl >
{
public:
	typedef CScintillaWindowImpl< COutputView, CScintillaImpl > baseClass;

	BEGIN_MSG_MAP(COutputView)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	void DoContextMenu(CPoint* point)
	{
		CSPopupMenu popup(IDR_POPUP_EDITOR);
		g_Context.m_frame->TrackPopupMenu(popup, 0, point->x, point->y, NULL);
	}

protected:
	virtual void OnFirstShow()
	{
		CSchemeManager::GetInstance()->SchemeByName("output")->Load(*this);
	}
};

#endif