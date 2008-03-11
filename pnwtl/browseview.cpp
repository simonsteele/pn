#include "stdafx.h"
#include "resource.h"
#include "browseview.h"

#include "include/atlshellext.h"
#include "include/ShellCtrls.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

CBrowseDocker::CBrowseDocker() : m_view(NULL)
{
	
}

CBrowseDocker::~CBrowseDocker()
{
	if (m_view)
	{
		delete m_view;
		m_view = NULL;
	}
}

LRESULT CBrowseDocker::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	/*HICON hIconSmall = (HICON)::LoadImage(_Module.GetResourceInstance(), MAKEINTRESOURCE(IDI_CTAGS), 
			IMAGE_ICON, ::GetSystemMetrics(SM_CXSMICON), ::GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);
	SetIcon(hIconSmall, FALSE);*/

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

	/*if (wParam)
	{
		::OutputDebugString("!!!!!!!!!!!!!!!!!!SHOW");
	}*/

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
			g_Context.m_frame->Open(path, true);
		}
	}

	return 0;
}