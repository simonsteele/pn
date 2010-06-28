/**
 * @file pntabs.cpp
 * @brief Tab customisation
 * @author Simon Steele
 * @note Copyright (c) 2007-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "pntabs.h"
#include "findbar.h"

CPNMDIClient::CPNMDIClient() : m_bMoving(false), m_findBar(new CFindBar())
{
}

CPNMDIClient::~CPNMDIClient()
{
	delete m_findBar;
}

/**
 * Ask for double-clicks in the class style
 */
BOOL CPNMDIClient::SubclassWindow(HWND hWnd)
{
	BOOL bSuccess = baseClass::SubclassWindow(hWnd);
	
	if(bSuccess)
		SetClassLong(m_hWnd, GCL_STYLE,
			GetClassLong(m_hWnd, GCL_STYLE) | CS_DBLCLKS);	

	if(bSuccess)
	{
		CRect rcFB(0,0,100,30);
		m_findBar->SetControllingHWND(m_hWnd);
		m_findBar->Create(GetParent(), rcFB, _T("FindBar"), /*WS_VISIBLE |*/ WS_CHILD | WS_CLIPSIBLINGS);
		//m_findBar->ShowWindow(SW_HIDE);
	}

	return bSuccess;
}

void CPNMDIClient::ShowFindBar(bool bShow)
{
	RECT rcMDIClient;
	::GetWindowRect(m_hWnd, &rcMDIClient);
	int tah = m_MdiTabOwner.GetTabAreaHeight();
	int fbh = m_findBar->GetDesiredHeight();

	if(bShow)
	{
		// Don't bother doing all this if we're already visible.
		if(m_findBar->IsWindowVisible())
		{
			m_findBar->SetFocus();
			return;
		}

		m_findBar->ShowWindow(SW_SHOW);
		m_findBar->SetFocus();
	}
	else
	{
		if(m_findBar->IsWindowVisible())
			rcMDIClient.bottom += fbh;

		m_findBar->ShowWindow(SW_HIDE);
	}

	::MapWindowPoints(NULL, GetParent(), (LPPOINT)&rcMDIClient, 2); //No Need with SWP_NOMOVE

	// Adjust for size of tab bar (not sure why, just do it!)
	// Need to see if tab bar is visible, and if it's at the top or bottom!
	if(m_MdiTabOwner.IsWindow())
	{
		DWORD dwStyle = m_MdiTabOwner.GetTabStyles();
		bool tabsOnTop = (CTCS_BOTTOM != (dwStyle & CTCS_BOTTOM));
		if(tabsOnTop)
		{
			// Adjust for the fact that the tab bar is at the top
			rcMDIClient.top -= tah;
		}
		else
		{
			// Adjust for the fact that the tab bar is at the bottom...
			rcMDIClient.bottom += tah; // cancel out adjustment in the function...
			if(bShow)
				rcMDIClient.bottom -= fbh;
		}
	}

	/*char buf[200];
	char num[34];
	strcpy(buf, "\nbefore: ");
	_itoa(rcMDIClient.bottom - rcMDIClient.top, num, 10);
	strcat(buf, num);
	LOG(buf);*/

	::SetWindowPos(m_hWnd, NULL, rcMDIClient.left, rcMDIClient.top, rcMDIClient.right - rcMDIClient.left, rcMDIClient.bottom - rcMDIClient.top, SWP_NOZORDER | SWP_NOACTIVATE /*| SWP_NOMOVE*/);
}

int CPNMDIClient::GetTabIndex(HWND hWndChild)
{
	return m_MdiTabOwner.GetTabIndex(hWndChild);
}

LRESULT CPNMDIClient::OnMDISetMenu(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = TRUE;
	
	// Set window menu
	HMENU hOldMenu = GetParent().GetMenu();
	DefWindowProc(uMsg, wParam, lParam);

	// Handle frame window menu
	SendMessage(GetParent(), PN_MDISETMENU, wParam, reinterpret_cast<LPARAM>(hOldMenu));

	return reinterpret_cast<LRESULT>(hOldMenu);
}

LRESULT CPNMDIClient::OnMDINext(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
{
	bHandled = TRUE;

	if(!OPTIONS->GetCached(Options::OManageTabOrder) || m_children.size() < 2)
	{
		return tabOrderMdiSwitch(lParam == 0);
	}

	return mruMdiSwitch(lParam == 0);
}

void CPNMDIClient::ControlUp()
{
	if(m_bMoving)
	{
		// The user has lifted the ctrl key, and we've been moving
		// around the tabs, so we store the new focus window as the
		// top-most.
		m_bMoving = false;
		HWND hWndCursor = *m_moveIt;
		m_children.remove(hWndCursor);
		m_children.push_front(hWndCursor);
	}
}

LRESULT CPNMDIClient::OnMDIActivate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = false;
	return 0;
}

LRESULT CPNMDIClient::OnChildActivationChange(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	// Notify the frame
	SendMessage(GetParent(), PN_NOTIFY, 0, PN_MDIACTIVATE);

	if(!m_bMoving)
	{
		// Put this window up to the front of the stack if we're not
		// already managing a ctrl-tab.
		m_children.remove((HWND)wParam);
		m_children.push_front((HWND)wParam);
	}

	// Cancel the find bar now
	ShowFindBar(false);

	bHandled = FALSE;
	
	return 0;
}

LRESULT CPNMDIClient::OnMDIDestroy(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	// Make sure we don't use a stale iterator...
	m_bMoving = false;

	// Get the HWND out of our stack
	m_children.remove((HWND)wParam);

	SendMessage(GetParent(), PN_NOTIFY, 0, PN_MDIDESTROY);
	bHandled = FALSE;

	return 0;
}

LRESULT CPNMDIClient::OnChildTabTextChange(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	//ss - causing a crash when updates are requested in the middle of a scintilla op.
	//also doesn't seem to have actually served a purpose.
	//SendMessage(GetParent(), PN_NOTIFY, 0, SCN_UPDATEUI);

	bHandled = FALSE;

	return 0;
}

LRESULT CPNMDIClient::OnDblClick(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	// Forward the message
	SendMessage(GetParent(), WM_LBUTTONDBLCLK, wParam, lParam);
	return 0;
}

static int runs = 0;

LRESULT CPNMDIClient::OnWindowPosChanging(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	//char buf[80];
	
	LPWINDOWPOS pWinPos = reinterpret_cast<LPWINDOWPOS>(lParam);
	if(pWinPos)
	{
		if( m_MdiTabOwner.IsWindow() )
		{
			//ATLTRACE(_T("Resizing MDI tab and MDI client\n"));

			int nTabAreaHeight = (m_MdiTabOwner.IsWindowVisible()) ? m_MdiTabOwner.GetTabAreaHeight() : 0;
			int nBottomAreaHeight = (m_findBar->IsWindow() && m_findBar->IsWindowVisible()) ? m_findBar->GetDesiredHeight() : 0;

			//sprintf(buf, "Before OnWindowPosChanging[%d] tabs %d bottom %d height %d, NOMOVE %d NOSIZE %d\n", runs, nTabAreaHeight, nBottomAreaHeight, pWinPos->cy, pWinPos->flags & SWP_NOMOVE, pWinPos->flags & SWP_NOSIZE);
			//LOG(buf);

			TTabCtrl& TabCtrl = m_MdiTabOwner.GetTabCtrl();
			DWORD dwStyle = TabCtrl.GetStyle();
			if(CTCS_BOTTOM == (dwStyle & CTCS_BOTTOM))
			{
				m_MdiTabOwner.SetWindowPos(
					NULL,
					pWinPos->x, pWinPos->y + (pWinPos->cy - nTabAreaHeight),
					pWinPos->cx, nTabAreaHeight,
					(pWinPos->flags & SWP_NOMOVE) | (pWinPos->flags & SWP_NOSIZE) | SWP_NOZORDER | SWP_NOACTIVATE);

				if((pWinPos->flags & SWP_NOSIZE) == 0)
				{
					pWinPos->cy -= nTabAreaHeight;
				}

				if(m_findBar->IsWindow())
					m_findBar->SetWindowPos(NULL, pWinPos->x, pWinPos->y + pWinPos->cy + nTabAreaHeight, pWinPos->cx, nBottomAreaHeight, (pWinPos->flags & SWP_NOMOVE) | (pWinPos->flags & SWP_NOSIZE) | SWP_NOZORDER | SWP_NOACTIVATE);
			}
			else
			{
				// Position the Tab bar
				m_MdiTabOwner.SetWindowPos(
					NULL,
					pWinPos->x, pWinPos->y,
					pWinPos->cx, nTabAreaHeight,
					(pWinPos->flags & SWP_NOMOVE) | (pWinPos->flags & SWP_NOSIZE) | SWP_NOZORDER | SWP_NOACTIVATE);

				// See if we need to adjust for movement?
				if((pWinPos->flags & SWP_NOMOVE) == 0)
				{
					pWinPos->y += nTabAreaHeight;
				}
				
				// Adjust for sizing?
				if((pWinPos->flags & SWP_NOSIZE) == 0)
				{
					pWinPos->cy -= nTabAreaHeight;
					pWinPos->cy -= nBottomAreaHeight; //Leave room for our bottom thing.
				}

				// Move the find bar...
				if(m_findBar->IsWindow())
				{
					m_findBar->SetWindowPos(NULL, pWinPos->x, pWinPos->y + pWinPos->cy, pWinPos->cx, nBottomAreaHeight, (pWinPos->flags & SWP_NOMOVE) | (pWinPos->flags & SWP_NOSIZE) | SWP_NOZORDER | SWP_NOACTIVATE);
				}
			}

			//sprintf(buf, "After OnWindowPosChanging[%d] tabs %d bottom %d height %d\n", runs++, nTabAreaHeight, nBottomAreaHeight, pWinPos->cy);
			//LOG(buf);
		}
	}

	// "base::OnWindowPosChanging()"
	LRESULT lRet = DefWindowProc(uMsg, wParam, lParam);
	bHandled = TRUE;

	return lRet;
}

LRESULT CPNMDIClient::OnEscapePressed(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	ShowFindBar(false);
	::SetFocus(GetCurrentEditor());
	return 0;
}

/**
 * This method returns the rect that the MDIClient would be if no tab bar or
 * find bar were visible, this is the rect that should be passed to OnWindowPosChanging
 */
LRESULT CPNMDIClient::OnGetMdiClientRect(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	RECT* lpRect = reinterpret_cast<RECT*>( lParam );
	GetWindowRect(lpRect);
	::MapWindowPoints(NULL, GetParent(), (LPPOINT)lpRect, 2);
	
	TTabCtrl& TabCtrl = m_MdiTabOwner.GetTabCtrl();
	DWORD dwStyle = TabCtrl.GetStyle();
	
	// Add the height of the tab area
	int nTabAreaHeight = (m_MdiTabOwner.IsWindowVisible()) ? m_MdiTabOwner.GetTabAreaHeight() : 0;
	if(nTabAreaHeight)
	{
		if(CTCS_BOTTOM == (dwStyle & CTCS_BOTTOM))
		{
			lpRect->bottom += nTabAreaHeight;
		}
		else
		{
			lpRect->top -= nTabAreaHeight;
			lpRect->bottom += nTabAreaHeight;
		}
	}
	
	// Add the find bar...
	int nBottomAreaHeight = (m_findBar->IsWindow() && m_findBar->IsWindowVisible()) ? m_findBar->GetDesiredHeight() : 0;
	lpRect->bottom += nBottomAreaHeight;

	return 0;
}

LRESULT CPNMDIClient::OnThemeChanged(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	RECT rcMDIClient;
	SendMessage(PN_GETMDICLIENTRECT, 0, (LPARAM)&rcMDIClient);

	// This doesn't seem to work - perhaps we need to force the child windows to recalculate sizes first?

	::SetWindowPos(m_hWnd, NULL, rcMDIClient.left, rcMDIClient.top, rcMDIClient.right - rcMDIClient.left, rcMDIClient.bottom - rcMDIClient.top, SWP_NOZORDER | SWP_NOACTIVATE);

	bHandled = FALSE;
	return 0;
}

LRESULT CPNMDIClient::OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = FALSE;

	return 0;
}

LRESULT CPNMDIClient::OnPNNotify(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	if(lParam == PN_HIDEFINDBAR)
	{
		ShowFindBar(false);		
	}

	return 0;
}

LRESULT CPNMDIClient::mruMdiSwitch(bool forwards)
{
	if (!m_bMoving)
	{
		// starting to move...
		m_moveIt = m_children.begin();
		m_bMoving = true;
	}

	if (forwards)
	{
		// forwards
		m_moveIt++;
		if (m_moveIt == m_children.end())
		{
			m_moveIt = m_children.begin();
		}
	}
	else
	{
		// backwards
		if (m_moveIt == m_children.begin())
		{
			m_moveIt = m_children.end();
		}
		m_moveIt--;
	}

	// Activate our chosen window
	LockWindowUpdate(TRUE);
	SendMessage(WM_MDIACTIVATE, (WPARAM)(*m_moveIt), 0);
	LockWindowUpdate(FALSE);

	return 0;
}

LRESULT CPNMDIClient::tabOrderMdiSwitch(bool forwards)
{
	int index(0);
	if (forwards)
	{
		index = m_MdiTabOwner.GetTabCtrl().GetCurSel() + 1;
		if (index == m_MdiTabOwner.GetTabCtrl().GetItemCount())
		{
			index = 0;
		}
	}
	else
	{
		index = m_MdiTabOwner.GetTabCtrl().GetCurSel() - 1;
		if (index == -1)
		{
			index = m_MdiTabOwner.GetTabCtrl().GetItemCount() - 1;
		}
	}

	m_MdiTabOwner.GetTabCtrl().SetCurSel(index);

	return 0;
}