#include "stdafx.h"
#include "resource.h"
#include "findinfiles.h"
#include "findinfilesview.h"
#include "childfrm.h"

CFindInFilesView::CFindInFilesView()
{
	m_nItems = 0;
}

LRESULT CFindInFilesView::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CRect rc;
	GetClientRect(rc);
	m_list.Create(m_hWnd, rc, _T("Results"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | LVS_REPORT | LVS_NOSORTHEADER | LVS_SHOWSELALWAYS, 0, IDC_FIF_LIST);
	m_list.SetExtendedListViewStyle(LVS_EX_FULLROWSELECT | LVS_EX_HEADERDRAGDROP);

	m_list.AddColumn(_T("File"), 0);
	m_list.AddColumn(_T("Line"), 1);
	m_list.AddColumn(_T("Text"), 2);

	m_list.SetColumnWidth(0, 250);
	m_list.SetColumnWidth(1, 50);

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
	int index = m_list.AddItem(m_nItems++, 0, file);
	
	_itoa(line, m_NCBuf, 10);
	m_list.AddItem(index, 1, m_NCBuf);
	m_list.AddItem(index, 2, str);
}

void CFindInFilesView::Clear()
{
	m_list.DeleteAllItems();
	m_nItems = 0;
}

LRESULT CFindInFilesView::OnListDblClk(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMITEMACTIVATE lpnm = (LPNMITEMACTIVATE)pnmh;

	if( lpnm->iItem == -1 )
		return 0;

	CString str;
	m_list.GetItemText(lpnm->iItem, 0, str);

	if( FileExists(str) )
	{
		// If the file's already open, just switch to it, otherwise open it.
		if( !g_Context.m_frame->CheckAlreadyOpen(str, eSwitch) )
			g_Context.m_frame->Open(str);

		m_list.GetItemText(lpnm->iItem, 1, str);

		CChildFrame* pWnd = CChildFrame::FromHandle(GetCurrentEditor());
		CTextView* pView = pWnd->GetTextView();
		
		if(pView)
		{
			int line = atoi(str);

			::SetFocus(pView->m_hWnd);
		
			pView->GotoLine(line-1);
		}
	}
	else
	{
		tstring msg = _T("Could not locate ") + str;
		msg += _T(".");
		g_Context.m_frame->SetStatusText(msg.c_str());
	}

	return 0;
}

void CFindInFilesView::OnBeginSearch(LPCTSTR stringLookingFor, bool bIsRegex)
{
	g_Context.m_frame->ToggleDockingWindow(PNDW_FINDRESULTS, true, true);
	
	m_dwStartTicks = GetTickCount();
	Clear();
}

void CFindInFilesView::OnEndSearch(int nFound, int nFiles)
{
	DWORD dwTicksTaken = GetTickCount() - m_dwStartTicks;
	TCHAR buf[4096];
	_sntprintf(buf, 4096, _T("Search complete: %d matching lines found in %d files, taking %d milliseconds.\n"), nFound, nFiles, dwTicksTaken);
	
	g_Context.m_frame->SetStatusText(buf);
}

void CFindInFilesView::OnFoundString(LPCTSTR stringFound, LPCTSTR szFilename, int line, LPCTSTR buf)
{
	AddResult(szFilename, line, buf);
}