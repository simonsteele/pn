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
	CListViewCtrl list(GetDlgItem(IDC_JUMPTOLIST));
	list.InsertColumn(0, _T("Tag"), LVCFMT_LEFT, 200, 0);

	JumpToHandler::GetInstance()->DoJumpTo(m_pChild, this);

	return 0;
}

void CJumpToDialog::OnFound(int count, LPMETHODINFO methodInfo)
{
	LVITEM lvi;
	memset(&lvi, 0, sizeof(LVITEM));
	lvi.mask = LVIF_TEXT;
	lvi.pszText = (TCHAR*)methodInfo->methodName;
	ListView_InsertItem(GetDlgItem(IDC_JUMPTOLIST), &lvi);
}