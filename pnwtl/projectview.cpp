/**
 * @file projectview.cpp
 * @brief View to display project trees.
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "project.h"
#include "projectview.h"

CProjectDocker::CProjectDocker()
{
	solution = NULL;
}

LRESULT CProjectDocker::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	//HICON hIconSmall = (HICON)::LoadImage(_Module.GetResourceInstance(), MAKEINTRESOURCE(m_dwIcon), 
	//		IMAGE_ICON, ::GetSystemMetrics(SM_CXSMICON), ::GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);
	//SetIcon(hIconSmall, FALSE);

	RECT rc;
	GetClientRect(&rc);
	m_view.Create(m_hWnd, rc, _T("ProjectTree"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS);

	return 0;
}

LRESULT CProjectDocker::OnSize(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	if(wParam != SIZE_MINIMIZED )
	{
		RECT rc;
		GetClientRect(&rc);
		m_view.SetWindowPos(NULL, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top ,SWP_NOZORDER | SWP_NOACTIVATE);
	}

	bHandled = FALSE;

	return 0;
}

LRESULT CProjectDocker::OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Hide();

	return 0;
}