/**
 * @file findinfilesview.cpp
 * @brief Find In Files View
 * @author Simon Steele
 * @note Copyright (c) 2005-2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "findinfiles.h"
#include "findinfilesview.h"
#include "childfrm.h"

#if defined (_DEBUG)
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


CFindInFilesView::CFindInFilesView()
{
	m_nItems = 0;
}

LRESULT CFindInFilesView::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	HICON hIconSmall = (HICON)::LoadImage(_Module.GetResourceInstance(), MAKEINTRESOURCE(IDI_FINDINFILES), 
			IMAGE_ICON, ::GetSystemMetrics(SM_CXSMICON), ::GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);
	SetIcon(hIconSmall, FALSE);

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

int CFindInFilesView::GetResultCount() const
{
	return m_list.GetItemCount();
}

LRESULT CFindInFilesView::OnListDblClk(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMITEMACTIVATE lpnm = (LPNMITEMACTIVATE)pnmh;

	if( lpnm->iItem == -1 )
		return 0;

	handleUserSelection( lpnm->iItem );
	

	return 0;
}

LRESULT CFindInFilesView::OnReturn(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	handleUserSelection( m_list.GetSelectedIndex() );

	return 0;
}

LRESULT CFindInFilesView::OnFIFMatch(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	FIFMatch* pMatch = (FIFMatch*)lParam;
	AddResult(pMatch->FileName, pMatch->Line, pMatch->Buf);
	delete pMatch;
	return 0;
}

LRESULT CFindInFilesView::OnFIFFinish(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	DWORD dwTicksTaken = GetTickCount() - m_dwStartTicks;
	TCHAR buf[4096];
	_sntprintf(buf, 4096, _T("Search complete: %d matching lines found in %d files, taking %d milliseconds.\n"), lParam, wParam, dwTicksTaken);
	g_Context.m_frame->SetStatusText(buf);
	return 0;
}

LRESULT CFindInFilesView::OnFIFStart(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	tstring sstartstr("Searching for: ");
	sstartstr += m_lookingFor;
	g_Context.m_frame->SetStatusText(sstartstr.c_str());
	
	return 0;
}

void CFindInFilesView::OnBeginSearch(LPCTSTR stringLookingFor, bool bIsRegex)
{
	g_Context.m_frame->ToggleDockingWindow(PNDW_FINDRESULTS, true, true);
	
	m_dwStartTicks = GetTickCount();

	m_lookingFor = stringLookingFor;
	::PostMessage(m_hWnd, PN_FIFSTART, 0, 0);

	Clear();
}

void CFindInFilesView::OnEndSearch(int nFound, int nFiles)
{
	::PostMessage(m_hWnd, PN_FIFFINISH, nFiles, nFound);
}

void CFindInFilesView::OnFoundString(LPCTSTR stringFound, LPCTSTR szFilename, int line, LPCTSTR buf)
{
	// Post a match result to ourselves and respond to it on the window thread.
	FIFMatch* match = new FIFMatch(szFilename, line, buf);
	::PostMessage(m_hWnd, PN_FIFMATCH, 0, (LPARAM)match);
}

void CFindInFilesView::handleUserSelection(int index)
{
	CString str;
	m_list.GetItemText(index, 0, str);

	if( FileExists(str) )
	{
		// If the file's already open, just switch to it, otherwise open it.
		if( !g_Context.m_frame->CheckAlreadyOpen(str, eSwitch) )
			g_Context.m_frame->Open(str);

		m_list.GetItemText(index, 1, str);

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
}

//////////////////////////////////////////////////////////////////////////////////
// class CFindInFilesView::FIFMatch

CFindInFilesView::FIFMatch::FIFMatch(LPCTSTR szFileName, int line, LPCTSTR szBuf)
{
	if(szFileName)
	{
		FileName = new TCHAR[_tcslen(szFileName)+1];
		_tcscpy(FileName, szFileName);
	}
	else
	{
		FileName = NULL;
	}

	if(szBuf)
	{
		Buf = new TCHAR[_tcslen(szBuf)+1];
		_tcscpy(Buf, szBuf);
	}
	else
		Buf = NULL;

	Line = line;
}

CFindInFilesView::FIFMatch::~FIFMatch()
{
	if(FileName != NULL)
		delete [] FileName;
	if(Buf != NULL)
		delete [] Buf;
}