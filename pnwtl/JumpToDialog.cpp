#include "stdafx.h"
#include "resource.h"
#include "jumpto.h"
#include "jumptodialog.h"

CJumpToDialog::CJumpToDialog(CChildFrame* pChild)
{
	m_pChild = pChild;
}

LRESULT CJumpToDialog::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CenterWindow(GetParent());

	list.Attach(GetDlgItem(IDC_JUMPTOLIST));
	list.InsertColumn(0, _T("Parent"), LVCFMT_LEFT, 80, 0);
	list.InsertColumn(1, _T("Tag"), LVCFMT_LEFT, 140, 0);
	list.InsertColumn(2, _T("Line"), LVCFMT_LEFT, 60, 0);

	int colOrder[3] = {1, 0, 2};
	list.SetColumnOrderArray(3, colOrder);

	JumpToHandler::GetInstance()->DoJumpTo(m_pChild, this);

	return 0;
}

void CJumpToDialog::OnFound(int count, LPMETHODINFO methodInfo)
{
	LVITEM lvi;
	memset(&lvi, 0, sizeof(LVITEM));
	lvi.mask = LVIF_TEXT;
	lvi.pszText = (TCHAR*)methodInfo->methodName;
	int index = list.InsertItem(&lvi);

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
	int selIndex = list.GetSelectedIndex();
	if(selIndex != -1)
	{
		line = list.GetItemData(selIndex);
	}
	else
		line = -1;

	EndDialog(wID);

	return 0;
}

int CJumpToDialog::GetLine()
{
	return line;
}

void CJumpToDialog::filter(LPCTSTR text)
{
	LVFINDINFO lvfi;
	lvfi.flags = LVFI_PARTIAL;
	lvfi.psz = text;

	int index = list.FindItem(&lvfi, 0);
	if(index != -1)
		list.SelectItem(index);
	//list.FindItem(
}