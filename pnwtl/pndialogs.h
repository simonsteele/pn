/**
 * @file pndialogs.h
 * @brief Assorted Dialogs for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef pndialogs_h__included
#define pndialogs_h__included

class CGotoDialog : public CDialogImpl<CGotoDialog>
{
	public:
		enum { IDD = IDD_GOTO };

		BEGIN_MSG_MAP(CGotoDialog)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_ID_HANDLER(IDOK, OnOK)
			COMMAND_ID_HANDLER(IDCANCEL, OnCancel)
		END_MSG_MAP()

		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
		{
			CenterWindow(GetParent());
			return TRUE;
		}
		
		LRESULT OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
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

		LRESULT OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
		{
			EndDialog(wID);
			
			return TRUE;
		}

		int GetLineNo()
		{
			return lineno;
		}
	
	protected:
		int lineno;
};

#endif