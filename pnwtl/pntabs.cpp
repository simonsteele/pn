#include "stdafx.h"
#include "pntabs.h"

CPNMDIClient::CPNMDIClient()
{
	m_bMoving = false;
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

	return bSuccess;
}

LRESULT CPNMDIClient::OnMDINext(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
{
	if(!OPTIONS->GetCached(Options::OManageTabOrder) || m_children.size() < 2)
	{
		bHandled = FALSE;
		return 0;
	}

	if(!m_bMoving)
	{
		// starting to move...
		m_moveIt = m_children.begin();
		m_bMoving = true;
	}

	if(lParam == 0)
	{
		// forwards
		m_moveIt++;
		if(m_moveIt == m_children.end())
			m_moveIt = m_children.begin();
	}
	else
	{
		// backwards
		if(m_moveIt == m_children.begin())
			m_moveIt = m_children.end();
		m_moveIt--;
	}

	// Activate our chosen window
	LockWindowUpdate(TRUE);
	SendMessage(WM_MDIACTIVATE, (WPARAM)(*m_moveIt), 0);
	LockWindowUpdate(FALSE);
	
	
	bHandled = TRUE;

	return 0;
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