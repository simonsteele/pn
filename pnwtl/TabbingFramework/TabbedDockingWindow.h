// TabbedDockingWindow.h: interface for the CTabbedDockingWindow class.
//
// NOTE: This class depends on Sergey Klimov's docking window framework
//  and "TabbedFrame.h"
//
//////////////////////////////////////////////////////////////////////

#ifndef __TABBED_DOCKING_WINDOW_H__
#define __TABBED_DOCKING_WINDOW_H__

#include <ExtDockingWindow.h>

#pragma once

class CTabbedDockingWindow :
	public CTabbedFrameImpl<CTabbedDockingWindow, CDotNetTabCtrl, dockwins::CStateKeeper<dockwins::CTitleDockingWindowImpl< CTabbedDockingWindow,CWindow,dockwins::COutlookLikeTitleDockingWindowTraits> > >
{
protected:
	typedef CTabbedDockingWindow thisClass;
	typedef CTabbedFrameImpl<CTabbedDockingWindow, CDotNetTabCtrl, dockwins::CStateKeeper<dockwins::CTitleDockingWindowImpl< CTabbedDockingWindow,CWindow,dockwins::COutlookLikeTitleDockingWindowTraits> > > baseClass;

public:
	DECLARE_WND_CLASS(_T("CTabbedDockingWindow"))

	BEGIN_MSG_MAP(thisClass)
		MESSAGE_HANDLER(WM_SIZE, OnSize)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

public:
	LRESULT OnSize(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
	{
		if(wParam != SIZE_MINIMIZED)
		{
			//T* pT = static_cast<T*>(this);
			//pT->UpdateLayout();
			UpdateLayout();
		}
		bHandled = FALSE;
		return 1;
	}

	void UpdateBarsPosition(RECT& rect, BOOL bResizeBars = TRUE)
	{
	}
};

#endif
