#include "stdafx.h"
#include "findinfilesview.h"

LRESULT CFindInFilesView::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CRect rc;
	GetClientRect(rc);
	m_list.Create(m_hWnd, rc, _T("Results"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | LVS_REPORT | LVS_NOSORTHEADER | LVS_SHOWSELALWAYS, 0);
	m_list.SetExtendedListViewStyle(LVS_EX_FULLROWSELECT | LVS_EX_HEADERDRAGDROP);

	m_list.AddColumn(_T("File"), 0);
	m_list.AddColumn(_T("Line"), 1);
	m_list.AddColumn(_T("Text"), 2);

	m_list.SetColumnWidth(0, 250);
	

	return TRUE;
}

LRESULT CFindInFilesView::OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CRect rc;
	GetClientRect(rc);
	m_list.SetWindowPos(HWND_TOP, rc, SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE);

	int wC0 = m_list.GetColumnWidth(0);
	int wC1 = m_list.GetColumnWidth(1);

	int wSB = ::GetSystemMetrics(SM_CXVSCROLL);

	m_list.SetColumnWidth(2, rc.Width() - wC0 - wC1 - wSB);

	return TRUE;
}

void CFindInFilesView::AddResult(LPCTSTR file, int line, LPCTSTR str)
{

}