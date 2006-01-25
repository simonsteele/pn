#include "stdafx.h"
#include "scriptview.h"
#include "scriptregistry.h"

LRESULT CScriptDocker::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CRect rc;
	GetClientRect(&rc);

	m_view.Create(m_hWnd, rc, _T("ScriptsList"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | TVS_DISABLEDRAGDROP | TVS_HASLINES | TVS_LINESATROOT, 0, IDC_SCRIPTSLIST);
	m_view.ShowWindow(SW_SHOW);

	return 0;
}

LRESULT CScriptDocker::OnSize(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	if(wParam != SIZE_MINIMIZED )
	{
		RECT rc;
		GetClientRect(&rc);
		
		m_view.SetWindowPos(NULL, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top, SWP_NOZORDER | SWP_NOACTIVATE);
	}

	bHandled = FALSE;

	return 0;
}