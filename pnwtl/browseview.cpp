/**
 * @file browseview.cpp
 * @brief File Browser View
 * @author Simon Steele
 * @note Copyright (c) 2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "browseview.h"

#include "ExplorerMenu.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

CBrowseDocker::CBrowseDocker() : 
	m_view(NULL), 
	m_menuHandler(new ShellContextMenu()),
	m_rightClickNode(NULL)
{
	
}

CBrowseDocker::~CBrowseDocker()
{
	if (m_view)
	{
		delete m_view;
		m_view = NULL;
	}

	if (m_menuHandler)
	{
		delete m_menuHandler;
		m_menuHandler = NULL;
	}
}

LRESULT CBrowseDocker::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	HICON hIconSmall = (HICON)::LoadImage(_Module.GetResourceInstance(), MAKEINTRESOURCE(IDI_BROWSER), 
			IMAGE_ICON, ::GetSystemMetrics(SM_CXSMICON), ::GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);
	SetIcon(hIconSmall, FALSE);

	RECT rc;
	GetClientRect(&rc);
	
	m_view = new CShellTreeCtrl();
	m_view->SetShellStyle(SCT_EX_FILESYSTEMONLY);
	m_view->Create(m_hWnd, rc, _T("FileSysTree"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | TVS_HASBUTTONS | TVS_HASLINES | /*TVS_EDITLABELS | */TVS_SHOWSELALWAYS, 0, IDC_BROWSETREE);
	m_view->Populate();

	bHandled = FALSE;

	return 0;
}

LRESULT CBrowseDocker::OnSize(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	if(wParam != SIZE_MINIMIZED)
	{
		RECT rc;
		GetClientRect(&rc);
		m_view->SetWindowPos(NULL, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top ,SWP_NOZORDER | SWP_NOACTIVATE);
	}

	bHandled = FALSE;

	return 0;
}

LRESULT CBrowseDocker::OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	//Hide();

	return 0;
}

LRESULT CBrowseDocker::OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = FALSE;

	return 0;
}

LRESULT CBrowseDocker::OnGetMinMaxInfo(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	LPMINMAXINFO mmi = reinterpret_cast<LPMINMAXINFO>(lParam);
	mmi->ptMinTrackSize.x = 80;
	mmi->ptMinTrackSize.y = 100;
	return 0;
}

LRESULT	CBrowseDocker::OnShow(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = FALSE;

	return 0;
}

/**
 * For some reason the WM_CTLCOLOR* messages do not get to the child
 * controls with the docking windows (todo with reflection). This returns
 * the proper result.
 */
LRESULT CBrowseDocker::OnCtlColor(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	return (LRESULT)::GetSysColorBrush( COLOR_WINDOW );
}

LRESULT CBrowseDocker::OnTreeDblClick(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	HTREEITEM hSel = m_view->GetSelectedItem();
	CPidl pidl;
	m_view->GetItemPidl(hSel, &pidl);

	TCHAR path[MAX_PATH+1];
	path[0] = _T('\0');
	if (::SHGetPathFromIDList(pidl, path) && path[0] != NULL)
	{
		SHFILEINFO sfi = {0};
		
		// Check it's not a folder
		::SHGetFileInfo((LPCTSTR)(LPITEMIDLIST)pidl, 0, &sfi, sizeof(SHFILEINFO), SHGFI_PIDL | SHGFI_ATTRIBUTES);
		if ((sfi.dwAttributes & SFGAO_FOLDER) == 0)
		{		
			if(!g_Context.m_frame->CheckAlreadyOpen(path, eSwitch))
			{
				g_Context.m_frame->Open(path, true);
			}
		}
	}

	return 0;
}

LRESULT CBrowseDocker::OnRightClick(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	CPoint pt(GetMessagePos());
	HTREEITEM hItem(NULL);

	// Test for keyboard right-click...
	if(pt.x != -1)
	{
		CPoint pt2(pt);
		m_view->ScreenToClient(&pt2);

		TVHITTESTINFO tvhti = {0};
		tvhti.pt = pt2;

		m_view->HitTest(&tvhti);

		hItem = tvhti.hItem;
	}
	else
	{
		hItem = m_view->GetSelectedItem();
	}

	if (hItem != NULL)
	{
		m_rightClickNode = hItem;

		CPidl pidl;
		m_view->GetItemPidl(hItem, &pidl);

		// m_menuHandler->TrackPopupMenu(pidl, pt.x, pt.y, m_hWnd);
		CSPopupMenu popup(IDR_POPUP_BROWSER);

		int nCmd;
		BOOL result = m_menuHandler->TrackPopupMenu(pidl, pt.x, pt.y, m_hWnd, popup.GetHandle(), popup.GetHandle(), 10000, 11000, nCmd);
		if (result && (nCmd < 10000 || nCmd > 11000))
		{
			::PostMessage(m_hWnd, WM_COMMAND, nCmd, 0L);
		}
	}

	return 0;
}

LRESULT CBrowseDocker::OnRefresh(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if (m_rightClickNode != NULL)
	{
		HTREEITEM hChild = TreeView_GetChild(m_view->m_hWnd, m_rightClickNode);
		if (hChild)
		{
			bool expanded = (TreeView_GetItemState(m_view->m_hWnd, m_rightClickNode, TVIS_EXPANDED) & TVIS_EXPANDED) == TVIS_EXPANDED;
			TreeView_Expand(m_view->m_hWnd, m_rightClickNode, TVE_COLLAPSERESET | TVE_COLLAPSE);

			/*while(hChild)
			{
				TreeView_DeleteItem(m_view->m_hWnd, hChild);
				hChild = TreeView_GetChild(m_view->m_hWnd, m_rightClickNode);
			}*/

			if (expanded)
			{
				TreeView_Expand(m_view->m_hWnd, m_rightClickNode, TVE_EXPAND);
			}
		}
	}

	return 0;
}

/**
 * Chain window message handling to our context menu handler
 */
LRESULT CBrowseDocker::handleSystemContextMenuMessage(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam, LRESULT& lResult)
{
	return m_menuHandler->ProcessWindowMessage(hWnd, uMsg, wParam, lParam, lResult);
}