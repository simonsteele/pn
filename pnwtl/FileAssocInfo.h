#ifndef __FILEASSOCINFO_H__
#define __FILEASSOCINFO_H__

class FileAssocInfo : public CDialogImpl<FileAssocInfo>
{
public:
	enum { IDD = IDD_FILEASSOC_INFO };
	enum Columns
	{
		ColExtension,
		ColMethod,
		ColTypeName,
	};

	FileAssocManager::FileAssocs    m_unassociated;
	CListViewCtrl m_list;

	BEGIN_MSG_MAP(FileAssocInfo)
		MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
		COMMAND_ID_HANDLER(IDYES, OnCloseCmd)
		COMMAND_ID_HANDLER(IDNO, OnCloseCmd)
	END_MSG_MAP()

	LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		CenterWindow(GetParent());

		m_list.Attach(GetDlgItem(IDC_LIST));
		m_list.AddColumn(_T("Extension"), ColExtension, ColExtension);
		m_list.AddColumn(_T("Method"), ColMethod, ColMethod);

		RECT rc;
		::GetWindowRect(GetDlgItem(IDC_FILEASSO_LIST), &rc);

		LVCOLUMN lvc = { 0 };
		lvc.mask = LVCF_FMT | LVCF_WIDTH | LVCF_TEXT | LVCF_SUBITEM;
		lvc.fmt = LVCFMT_LEFT;
		lvc.pszText = _T("Type Name");
		lvc.cx = (rc.right - rc.left) / 2;
		lvc.iSubItem = ColTypeName;
		m_list.InsertColumn(ColTypeName, &lvc);

		for(int i = 0; i < m_unassociated.GetSize(); i++)
		{
			const FileAssoc& fa = m_unassociated[i];
			m_list.AddItem(i, ColExtension, fa.GetExtension());
			m_list.SetItemText(i, ColMethod, fa.GetVerbName());
			m_list.SetItemText(i, ColTypeName, fa.GetCurrentTypeName());
		}
		return TRUE;
	}

	LRESULT OnCloseCmd(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		EndDialog(wID);
		return 0;
	}
};

#endif // __FILEASSOCINFO_H__
