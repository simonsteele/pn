/**
 * @file newprojectdialog.h
 * @brief New Project Dialog
 * @author Simon Steele
 * @note Copyright (c) 2005 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef newprojectdialog_h__included_03BDAD52_3803_4f43_8D01_25482603CCF3
#define newprojectdialog_h__included_03BDAD52_3803_4f43_8D01_25482603CCF3

/**
 * @brief New Project Dialog Class
 */
class CNewProjectDialog : public CDialogImpl<CNewProjectDialog>
{
	typedef CDialogImpl<CNewProjectDialog> baseClass;
	public:
		BEGIN_MSG_MAP(CNewProjectDialog)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)

			COMMAND_ID_HANDLER(IDOK, OnOK)
			COMMAND_ID_HANDLER(IDCANCEL, OnCloseCmd)

			COMMAND_HANDLER(IDC_BROWSE, BN_CLICKED, OnBnClickedBrowse)

			ALT_MSG_MAP(1)
			
			MESSAGE_HANDLER(WM_CHAR, OnEditKeyDown)

		END_MSG_MAP()
		
		enum { IDD = IDD_NEWPROJECT };

		CNewProjectDialog();

		LPCTSTR GetName() const;
		LPCTSTR GetFolder() const;
		LPCTSTR GetTemplateGUID() const;

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnOK(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnCloseCmd(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnBnClickedBrowse(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnEditKeyDown(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	protected:
		tstring						m_name;
		tstring						m_folder;
		tstring						m_tguid;
		CListViewCtrl				m_list;
		CImageList					m_images;
		BXT::CComboBoxAC			m_folderCombo;
		std::list<HICON>			m_icons;
		CContainedWindowT<CEdit>	m_editName;
};

#endif // #ifndef newprojectdialog_h__included_03BDAD52_3803_4f43_8D01_25482603CCF3