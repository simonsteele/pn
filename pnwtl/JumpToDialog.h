#ifndef jumptodialog_h__included
#define jumptodialog_h__included

class CJumpToDialog : public CDialogImpl<CJumpToDialog>, IJumpToFindSink
{
	typedef CDialogImpl<CJumpToDialog> baseClass;
	public:
		BEGIN_MSG_MAP(CJumpToDialog)
			COMMAND_HANDLER(IDC_JUMPTOTEXT, EN_CHANGE, OnTextKeyPress)
			COMMAND_ID_HANDLER(IDOK, OnOk)
			COMMAND_ID_HANDLER(IDCANCEL, OnCancel)

			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
		END_MSG_MAP()
		enum { IDD = IDD_JUMPTO };

		CJumpToDialog(CChildFrame* pChild);

		LRESULT OnTextKeyPress(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
		{
			CWindowText wt(GetDlgItem(IDC_JUMPTOTEXT));

			if((LPCTSTR)wt != NULL)
			{
				filter(wt);
			}

			return 0;
		}

		LRESULT OnOk(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
		{
			EndDialog(wID);

			return 0;
		}

		LRESULT OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
		{
			EndDialog(wID);

			return 0;
		}

		virtual void OnFound(int count, LPMETHODINFO methodInfo);

	protected:

		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

		void filter(LPCTSTR text)
		{

		}

		CChildFrame* m_pChild;
};

#endif // #ifndef jumptodialog_h__included