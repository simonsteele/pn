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
			m_psd.hInstance = _Module.GetResourceInstance();
			m_psd.lpPageSetupTemplateName = MAKEINTRESOURCE( IDD_PAGESETUP );
		}

		BEGIN_MSG_MAP(CPageSetupDialog)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			
			COMMAND_ID_HANDLER(IDOK, OnOK)
			COMMAND_HANDLER(IDC_PSHELPER_BUTTON, BN_CLICKED, OnHelperClicked)
			COMMAND_HANDLER(IDC_PSHELPER2_BUTTON, BN_CLICKED, OnHelperClicked)

			MESSAGE_HANDLER(WM_DRAWITEM, OnDrawItem)

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

			m_HeaderEdit.SetWindowText( header );
			m_FooterEdit.SetWindowText( footer );

			m_helperBtn.SubclassWindow( GetDlgItem(IDC_PSHELPER_BUTTON ) );
			m_helperBtn2.SubclassWindow( GetDlgItem(IDC_PSHELPER2_BUTTON ) );

			bHandled = FALSE;
			
			return TRUE;
		}

		LRESULT OnDrawItem(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
		{
			if( ((LPDRAWITEMSTRUCT)lParam)->hwndItem == m_helperBtn.m_hWnd )
			{
				return m_helperBtn.SendMessage( OCM__BASE + uMsg, wParam, lParam );
			}
			else if( ((LPDRAWITEMSTRUCT)lParam)->hwndItem == m_helperBtn2.m_hWnd )
			{
				return m_helperBtn2.SendMessage( OCM__BASE + uMsg, wParam, lParam );
			}
			else
			{
				bHandled = FALSE;
			}

			return 0;
		}

		LRESULT OnOK(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& bHandled)
		{
			bHandled = FALSE;

			m_HeaderEdit.GetWindowText(header);
			m_FooterEdit.GetWindowText(footer);

			return 0;
		}

		LRESULT OnHelperClicked(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& bHandled)
		{
			CArrowButton& btn = (wID == IDC_PSHELPER_BUTTON ? m_helperBtn : m_helperBtn2);
			
			CRect rc;
			btn.GetWindowRect(rc);

            doHelper(rc, wID == IDC_PSHELPER2_BUTTON);			

			return 0;
		}

		int doHelperInsert(CEdit& cbedit, LPCTSTR insert)
		{
			DWORD dwSel = cbedit.GetSel();
			cbedit.ReplaceSel(insert);

			int pos = LOWORD(dwSel);
			pos += _tcslen(insert);
			cbedit.SetFocus();
			cbedit.SetSel(pos, pos);

			return pos;
		}

		bool getHelperString(DWORD dwId, CString& str)
		{
			switch(dwId)
			{
				case ID_HEADERFOOTERFIELDS_FILENAME:
					str = _T("%f");
					break;
				case ID_HEADERFOOTERFIELDS_FILEPATH:
					str = _T("%d");
					break;
				case ID_HEADERFOOTERFIELDS_FILEDATE:
					str = _T("%D");
					break;
				case ID_HEADERFOOTERFIELDS_FILETIME:
					str = _T("%T");
					break;
				case ID_HEADERFOOTERFIELDS_CURRENTDATE:
					str = _T("%c");
					break;
				case ID_HEADERFOOTERFIELDS_CURRENTTIME:
					str = _T("%t");
					break;
				case ID_HEADERFOOTERFIELDS_USERNAME:
					str = _T("%u");
					break;
				case ID_HEADERFOOTERFIELDS_CURRENTPAGE:
					str = _T("%p");
					break;
				case ID_HEADERFOOTERFIELDS_PAGECOUNT:
					str = _T("%P");
					break;

				default:
					return false;
			}

			return true;
		}

		void doHelper(LPRECT rc, bool bFooter)
		{
			HMENU hPSHelperMenu;

			CEdit& editCtrl = (bFooter ? m_FooterEdit : m_HeaderEdit);
			
			hPSHelperMenu = ::LoadMenu(_Module.GetResourceInstance(), MAKEINTRESOURCE(IDR_POPUP_HEADERFOOTER));
			
			HMENU hPopup = ::GetSubMenu(hPSHelperMenu, 0);

			DWORD dwRes = ::TrackPopupMenu(hPopup, TPM_RETURNCMD, rc->right, rc->top, 0, m_hWnd, NULL);
			if(dwRes != 0)
			{
				CString str;
				if( getHelperString(dwRes, str) )
				{
					doHelperInsert(editCtrl, str);
				}

			}

			::DestroyMenu(hPSHelperMenu);
		}

	protected:
		CEdit m_HeaderEdit;
		CEdit m_FooterEdit;
		CString header;
		CString footer;
		CArrowButton m_helperBtn;
		CArrowButton m_helperBtn2;
};

}

#endif //#ifndef pagesetupdialog_h__included