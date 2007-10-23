/**
 * @file jumptodialog.cpp
 * @brief Jump To Dialog Implementation
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "jumpto.h"
#include "jumptodialog.h"
#include "jumpview.h"

#define COLWIDTH_PARENT 90
#define COLWIDTH_LINE	60
#define COLWIDTH_TAG	153

CJumpToDialog::CJumpToDialog(CChildFrame* pChild) : edtTag(this, 1)
{
	m_pChild = pChild;
}

LRESULT CJumpToDialog::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CenterWindow(GetParent());

	list.Attach(GetDlgItem(IDC_JUMPTOLIST));

	btnOk.Attach(GetDlgItem(IDOK));
	btnCancel.Attach(GetDlgItem(IDCANCEL));
	//edtTag.Attach(GetDlgItem(IDC_JUMPTOTEXT));

	//edtTag.SubclassDlgItem(IDC_JUMPTOTEXT, this);
	//edtTag.SubclassWindow(GetDlgItem(IDC_JUMPTOTEXT));
	edtTag.SubclassWindow(GetDlgItem(IDC_JUMPTOTEXT));

	list.InsertColumn(0, _T("Tag"), LVCFMT_LEFT, COLWIDTH_TAG, 0);
	list.InsertColumn(1, _T("Parent"), LVCFMT_LEFT, COLWIDTH_PARENT, 0);
	list.InsertColumn(2, _T("Line"), LVCFMT_LEFT, COLWIDTH_LINE, 0);

	images.CreateFromImage(IDB_TAGTYPES, 16, 1, RGB(255,0,255), IMAGE_BITMAP);
	list.SetImageList( images.m_hImageList, LVSIL_SMALL );

	int colOrder[3] = {1, 0, 2};
	list.SetColumnOrderArray(3, colOrder);

	list.SetExtendedListViewStyle( LVS_EX_FULLROWSELECT, LVS_EX_FULLROWSELECT );

	CRect rc, rcList, rcBtn, rcCancel;
	GetClientRect(&rc);
	list.GetWindowRect(&rcList);
	ScreenToClient(&rcList);
	listGaps.cx = rc.Width() - rcList.right;
	listGaps.cy = rc.Height() - rcList.bottom;
	
	btnOk.GetWindowRect(&rcBtn);
	btnCancel.GetWindowRect(&rcCancel);
	ScreenToClient(&rcBtn);
	ScreenToClient(&rcCancel);
	buttonGap = rcBtn.top - rcList.bottom;
	buttonWidth = rcBtn.Width();
	buttonGapX = rcCancel.left - rcBtn.right;

	JumpToHandler::GetInstance()->FindTags(m_pChild, this);

	return 0;
}

LRESULT CJumpToDialog::OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	int width = LOWORD(lParam);
	int height = HIWORD(lParam);

    CRect rc, rc2, rc3;
	
	// Size the list control
	list.GetWindowRect(&rc);
	ScreenToClient(&rc);
	rc.right = width - listGaps.cx;
	rc.bottom = height - listGaps.cy;
	list.SetWindowPos(HWND_TOP, &rc, SWP_NOMOVE | SWP_NOZORDER);

	// The edit box
	edtTag.GetWindowRect(&rc2);
	ScreenToClient(&rc2);
	rc2.right = rc.right;
	edtTag.SetWindowPos(HWND_TOP, &rc2, SWP_NOMOVE | SWP_NOZORDER);

	// Cancel button
	btnCancel.GetWindowRect(&rc3);
	ScreenToClient(&rc3);
	rc3.MoveToX(rc.right - buttonWidth);
	rc3.MoveToY(rc.bottom + buttonGap);
	btnCancel.SetWindowPos(HWND_TOP, &rc3, SWP_NOSIZE | SWP_NOZORDER);

	// Ok button
	btnOk.GetWindowRect(&rc2);
	ScreenToClient(&rc2);
	rc2.MoveToX(rc3.left - buttonGapX - buttonWidth);
	rc2.MoveToY(rc.bottom + buttonGap);
	btnOk.SetWindowPos(HWND_TOP, &rc2, SWP_NOSIZE | SWP_NOZORDER);	

	// Now size the tag column to fit.
	list.GetClientRect(&rc);	
	int cw = rc.Width() - list.GetColumnWidth(1) - list.GetColumnWidth(2);
	list.SetColumnWidth(0, cw);

	return 0;
}

LRESULT CJumpToDialog::OnListDblClick(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	transfer();
	if(line != -1)
		EndDialog(IDOK);

	return 0;
}

LRESULT CJumpToDialog::OnEditKeyDown(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	if(wParam == VK_DOWN)
	{
		int index = list.GetSelectedIndex() + 1;
		if(index >= list.GetItemCount())
			index--;
		list.SelectItem(index);
	}
	else if(wParam == VK_UP)
	{
		int index = list.GetSelectedIndex() - 1;
		if(index < 0)
			index = 0;
		list.SelectItem(index);
	}
	else
		bHandled = FALSE;

	return 0;
}

void CJumpToDialog::OnFound(int count, LPMETHODINFO methodInfo)
{
	LVITEM lvi;
	memset(&lvi, 0, sizeof(LVITEM));
	lvi.iItem = list.GetItemCount();
	lvi.mask = LVIF_TEXT | LVIF_IMAGE;
	lvi.pszText = (TCHAR*)methodInfo->methodName;
	if(methodInfo->type <= TAG_MAX)
		lvi.iImage = jumpToTagImages[methodInfo->type].imagesNumber;
	else
		lvi.iImage = 0;
	int index = list.InsertItem(&lvi);

	lvi.mask = LVIF_TEXT;
	lvi.iItem = index;
	lvi.iSubItem = 1;
	lvi.pszText = (TCHAR*)methodInfo->parentName;
	list.SetItem(&lvi);

	_itot(methodInfo->lineNumber, itoabuf, 10);
	lvi.iSubItem = 2;
	lvi.pszText = itoabuf;
	list.SetItem(&lvi);

	list.SetItemData(index, methodInfo->lineNumber);
}

LRESULT CJumpToDialog::OnTextKeyPress(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CWindowText wt(GetDlgItem(IDC_JUMPTOTEXT));

	if((LPCTSTR)wt != NULL)
	{
		filter(wt);
	}

	return 0;
}

LRESULT CJumpToDialog::OnOk(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	transfer();
	EndDialog(wID);
	return 0;
}

LRESULT CJumpToDialog::OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	EndDialog(wID);
	return 0;
}

int CJumpToDialog::GetLine()
{
	return line;
}

/**
 * Select the best match for what the user typed. This could potentially
 * also filter the list but that would make this dialog *much* more
 * expensive.
 */
void CJumpToDialog::filter(LPCTSTR text)
{
	LVFINDINFO lvfi;
	lvfi.flags = LVFI_PARTIAL;
	lvfi.psz = text;

	int index = list.FindItem(&lvfi, 0);
	if(index != -1)
		list.SelectItem(index);
}

/**
 * Transfer the selected item into the line member. 
 * Set to -1 if the selection isn't valid.
 */
void CJumpToDialog::transfer()
{
	int selIndex = list.GetSelectedIndex();
	if(selIndex != -1)
	{
		line = list.GetItemData(selIndex);
	}
	else
		line = -1;
}