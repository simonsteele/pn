/**
 * @file tools.h
 * @brief Jump To Dialog
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
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

		LRESULT OnTextKeyPress(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnOk(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
		{
			EndDialog(wID);

			return 0;
		}

		virtual void OnFound(int count, LPMETHODINFO methodInfo);

		int GetLine();

	protected:

		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

		void filter(LPCTSTR text);

		CChildFrame*	m_pChild;
		CListViewCtrl	list;
		TCHAR			itoabuf[20];
		int				line;
};

#endif // #ifndef jumptodialog_h__included