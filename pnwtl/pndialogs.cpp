/**
 * @file pndialogs.cpp
 * @brief Assorted Dialogs for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "pndialogs.h"

//////////////////////////////////////////////////////////////////////////////
// CPNSaveDialog
//////////////////////////////////////////////////////////////////////////////

CPNSaveDialog::CPNSaveDialog(LPCTSTR szFilter) : baseClass(FALSE, _T(".txt"), NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT, szFilter)
{
	m_Format = PNSF_NoChange;
	m_ofn.Flags |= OFN_ENABLETEMPLATE;
	m_ofn.lpTemplateName = MAKEINTRESOURCE (IDD_PNSAVE);
}

LRESULT CPNSaveDialog::OnInitDialog (UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled)
{
	m_SaveTypeCombo = GetDlgItem(IDC_PNSAVE_TYPECOMBO);
	m_SaveTypeLabel = GetDlgItem(IDC_PNSAVE_SAVEASSTATIC);

	CRect rectDlg;
	GetWindowRect( &rectDlg );

	CRect rectParent;
	::GetClientRect( GetParent(), &rectParent );
	rectDlg.right = rectDlg.left + rectParent.Width();

	// Make sure there is enough room at the bottom for resize
	CRect rectCombo;
	m_SaveTypeCombo.GetWindowRect( &rectCombo );
	ScreenToClient( &rectCombo );
	int cySize = ::GetSystemMetrics( /*SM_CYSIZE*/ SM_CYSIZEFRAME );
	if (rectDlg.Height() - rectCombo.bottom < cySize)
		rectDlg.bottom = rectDlg.top + rectCombo.bottom + cySize;

	// Reposition
	SetWindowPos( NULL, 0, 0, rectDlg.Width(), rectDlg.Height(),
		SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOZORDER );

	m_SaveTypeCombo.AddString(_T("No change to the file format."));
	m_SaveTypeCombo.AddString(_T("Ensure Windows Format (CR+LF)"));
	m_SaveTypeCombo.AddString(_T("Ensure Unix Format (LF)"));
	m_SaveTypeCombo.AddString(_T("Ensure Macintosh Format (CR)"));

	m_SaveTypeCombo.SetCurSel(0);
	
	RepositionControls();
	return TRUE;
}

LRESULT CPNSaveDialog::OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled)
{
	RepositionControls();
	bHandled = FALSE;
	return FALSE;
}

LRESULT CPNSaveDialog::OnComboSelChange(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	// Store the new selection in here...
	m_Format = (EPNSaveFormat)m_SaveTypeCombo.GetCurSel();

	return TRUE;
}

EPNSaveFormat CPNSaveDialog::GetSaveFormat()
{
	return m_Format;
}

void CPNSaveDialog::RepositionControls()
{
	RepositionControl( m_SaveTypeCombo, cmb1, true );
	RepositionControl( m_SaveTypeLabel, stc2, false );

	RepositionPlacesBar( m_SaveTypeCombo );
}

void CPNSaveDialog::RepositionPlacesBar(CWindow &bottomwnd)
{
	CRect rc, rc2;
	CWindow wndParent = GetParent();
	CWindow wndPB = wndParent.GetDlgItem( ctl1 );
	if( wndPB.IsWindow() )
	{
		wndPB.GetWindowRect( &rc );
		bottomwnd.GetWindowRect( &rc2 );
		wndParent.ScreenToClient( &rc );
		ScreenToClient( &rc2 );
		rc.bottom = rc2.bottom;
		wndPB.SetWindowPos( NULL, &rc, SWP_NOACTIVATE | SWP_NOZORDER );
	}
}

/**
 * @brief Reposition a control
 * @param wnd Control to be reposition
 * @param nID ID of the control used for positioning
 * @param fSize If true, adjust the width of the control
 */
void CPNSaveDialog::RepositionControl(CWindow &wnd, UINT nID, bool fSize)
{
	// Get the window rect in the client area of the 
	// control we are interested in.
	CWindow wndParent = GetParent();
	CWindow wndAnchor = wndParent.GetDlgItem( nID );
	CRect rectAnchor;
	wndAnchor.GetWindowRect( &rectAnchor );
	wndParent.ScreenToClient( &rectAnchor );

	//
	// Reposition the control
	//

	DWORD dwSWFlags = SWP_NOACTIVATE | SWP_NOZORDER | SWP_NOSIZE;
	CRect rectCtrl;
	wnd.GetWindowRect( &rectCtrl );
	ScreenToClient( &rectCtrl );
	rectCtrl.OffsetRect( rectAnchor.left - rectCtrl.left, 0 );
	if( fSize )
	{
		rectCtrl.right = rectCtrl.left + rectAnchor.Width();
		dwSWFlags &= ~SWP_NOSIZE;
	}
	wnd.SetWindowPos( NULL, rectCtrl.left, rectCtrl.top,
		rectCtrl.Width(), rectCtrl.Height(), dwSWFlags );
	return;
}

//////////////////////////////////////////////////////////////////////////////
// CGotoDialog
//////////////////////////////////////////////////////////////////////////////

LRESULT CGotoDialog::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CenterWindow(GetParent());
	return TRUE;
}

LRESULT CGotoDialog::OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	HWND hEdit = GetDlgItem(IDC_GOTOEDIT);
	int i = ::GetWindowTextLength(hEdit);
	i++;
	TCHAR *t = new TCHAR[i];
	::GetWindowText(hEdit, t, i);
	lineno = _ttoi(t);
	delete [] t;

	EndDialog(wID);

	return TRUE;
}

LRESULT CGotoDialog::OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	EndDialog(wID);
	
	return TRUE;
}

int CGotoDialog::GetLineNo()
{
	return lineno;
}

//////////////////////////////////////////////////////////////////////////////
// COptionsDialog
//////////////////////////////////////////////////////////////////////////////

COptionsDialog::COptionsDialog()
{
	m_pCurrentPage = NULL;
}

BOOL COptionsDialog::EndDialog(int nRetCode)
{
	ClosePages();
	return baseClass::EndDialog(nRetCode);
}

LRESULT COptionsDialog::OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	PAGEPTRLIST::iterator i;

	for(i = m_Pages.begin(); i != m_Pages.end(); ++i)
	{
		(*i)->OnOK();
	}

	EndDialog(wID);

	return TRUE;
}

LRESULT COptionsDialog::OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	PAGEPTRLIST::iterator i;

	for(i = m_Pages.begin(); i != m_Pages.end(); ++i)
	{
		(*i)->OnCancel();
	}

	EndDialog(wID);
	
	return TRUE;
}

LRESULT COptionsDialog::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_tree.Attach(GetDlgItem(IDC_TREE));

	InitialisePages();
		
	CenterWindow(GetParent());
	
	return TRUE;
}

void COptionsDialog::AddPage(COptionsPage* pPage)
{
	m_Pages.insert(m_Pages.end(), pPage);
}

HTREEITEM COptionsDialog::FindAtThisLevel(LPCTSTR title, HTREEITEM context)
{
	HTREEITEM i = (context ? m_tree.GetChildItem(context) : m_tree.GetRootItem());

	TCHAR buf[32];
	TVITEM tvi;
	tvi.mask = TVIF_TEXT;
	tvi.pszText = buf;
	tvi.cchTextMax = 30;
	
	while(i)
	{
		tvi.hItem = i;
		m_tree.GetItem(&tvi);

		if(_tcscmp(buf, title) == 0)
			break;

		i = m_tree.GetNextSiblingItem(i);
	}

	return i;
}

HTREEITEM COptionsDialog::AddTreeEntry(LPCTSTR title, HTREEITEM context)
{
	HTREEITEM i = FindAtThisLevel(title, context);

	if(!i)
	{
		i = m_tree.InsertItem(title, context, NULL);
		m_tree.SetItemData(i, NULL);
	}

	return i;
}

void COptionsDialog::InitialisePages()
{
	TCHAR buf[200];
	PAGEPTRLIST::iterator i;

	for(i = m_Pages.begin(); i != m_Pages.end(); ++i)
	{
		LPCTSTR treeloc = (*i)->GetTreePosition();
		PNASSERT(_tcslen(treeloc) < 200);
		_tcscpy(buf, treeloc);

		HTREEITEM ti = NULL;
		TCHAR* pSlash = NULL;
		TCHAR* pNext = buf;

		while((pSlash = _tcschr(pNext, _T('\\'))) != NULL)
		{
			*pSlash = NULL;
			ti = AddTreeEntry(pNext, ti);
			*pSlash = '\\';
			pNext = pSlash + 1;
		}
		// Add Tree Item
		HTREEITEM item = AddTreeEntry(pNext, ti);
		m_tree.SetItemData(item, reinterpret_cast<DWORD_PTR>(*i));
	}
}

void COptionsDialog::ClosePages()
{
	PAGEPTRLIST::iterator i;
	for(i = m_Pages.begin(); i != m_Pages.end(); ++i)
	{
		if((*i)->m_bCreated)
		{
			(*i)->ClosePage();
		}
	}
}

void COptionsDialog::SelectPage(COptionsPage* pPage)
{
	CRect rcPage;
	GetDlgItem(IDC_PLACEHOLDER).GetWindowRect(rcPage);
	ScreenToClient(rcPage);

	if(m_pCurrentPage)
	{
		m_pCurrentPage->ShowPage(SW_HIDE);
	}

	if(!pPage->m_bCreated)
	{
		pPage->CreatePage(m_hWnd, rcPage, m_tree);
		pPage->OnInitialise();
		pPage->m_bCreated = true;
	}
	
	pPage->ShowPage(SW_SHOW);
	m_pCurrentPage = pPage;
}

LRESULT COptionsDialog::OnTreeNotify(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMTREEVIEW pN = reinterpret_cast<LPNMTREEVIEW>(pnmh);
	if(pnmh->code == TVN_SELCHANGED)
	{
		COptionsPage* pPage = reinterpret_cast<COptionsPage*>(m_tree.GetItemData(pN->itemNew.hItem));
		if(pPage)
			SelectPage(pPage);
	}

	return 0;
}
