#ifndef pagesetupdialog_h__included
#define pagesetupdialog_h__included

namespace SS
{

class CPageSetupDialog : public CPageSetupDialogImpl<CPageSetupDialog>
{
	typedef CPageSetupDialogImpl<CPageSetupDialog> baseClass;
	public:
		CPageSetupDialog(DWORD dwFlags = PSD_MARGINS | PSD_INWININIINTLMEASURE | PSD_ENABLEPAGESETUPTEMPLATE, HWND hWndParent = NULL)
			: baseClass(dwFlags, hWndParent)
		{
			m_psd.hInstance = ::GetModuleHandle(NULL);
			m_psd.lpPageSetupTemplateName = MAKEINTRESOURCE( IDD_PAGESETUP );
		}

		BEGIN_MSG_MAP(CPageSetupDialog)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			
			COMMAND_ID_HANDLER(IDOK, OnOK)

			CHAIN_MSG_MAP(baseClass)
		END_MSG_MAP()

		static UINT CALLBACK PaintHookProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
		{
			// Built in ATL one doesn't seem to work here...
			return 0;
		}

		void SetMargins(LPRECT lpRectMargins)
		{
			if(lpRectMargins != NULL)
			{
				memcpy(&m_psd.rtMargin, lpRectMargins, sizeof(RECT));
				m_psd.Flags |= PSD_MARGINS;
			}
		}

		LPCTSTR GetHeaderText() const
		{
			return (LPCTSTR)header;
		}

		LPCTSTR GetFooterText() const
		{
			return (LPCTSTR)footer;
		}

		void SetHeaderText(LPCTSTR text)
		{
			header = text;
		}

		void SetFooterText(LPCTSTR text)
		{
			footer = text;
		}

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
		{
			m_HeaderEdit = GetDlgItem(IDC_HEADERTEXT);
			m_FooterEdit = GetDlgItem(IDC_FOOTERTEXT);

			bHandled = FALSE;
			
			return TRUE;
		}

		LRESULT OnOK(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& bHandled)
		{
			bHandled = FALSE;

			m_HeaderEdit.GetWindowText(header);
			m_FooterEdit.GetWindowText(footer);

			return 0;
		}

	protected:
		CEdit m_HeaderEdit;
		CEdit m_FooterEdit;
		CString header;
		CString footer;
};

}

#endif //#ifndef pagesetupdialog_h__included