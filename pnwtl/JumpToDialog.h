/**
 * @file jumptodialog.h
 * @brief Jump To Dialog
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef jumptodialog_h__included
#define jumptodialog_h__included

/**
 * @brief Jump to dialog class
 */
class CJumpToDialog : public CDialogImpl<CJumpToDialog>, IJumpToFindSink
{
	typedef CDialogImpl<CJumpToDialog> baseClass;
	public:
		BEGIN_MSG_MAP(CJumpToDialog)
			COMMAND_HANDLER(IDC_JUMPTOTEXT, EN_CHANGE, OnTextKeyPress)
			COMMAND_ID_HANDLER(IDOK, OnOk)
			COMMAND_ID_HANDLER(IDCANCEL, OnCancel)

			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			MESSAGE_HANDLER(WM_SIZE, OnSize)

			NOTIFY_HANDLER(IDC_JUMPTOLIST, NM_DBLCLK, OnListDblClick)
		END_MSG_MAP()
		enum { IDD = IDD_JUMPTO };

		CJumpToDialog(CChildFrame* pChild);

		LRESULT OnTextKeyPress(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnOk(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		virtual void OnFound(int count, LPMETHODINFO methodInfo);

		int GetLine();

	protected:

		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnListDblClick(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

		void filter(LPCTSTR text);
		void transfer();

		CChildFrame*	m_pChild;
		CButton			btnOk;
		CButton			btnCancel;
		CEdit			edtTag;
		CListViewCtrl	list;
		CImageList		images;
		int				buttonGap;
		SIZE			listGaps;
		TCHAR			itoabuf[20];
		int				line;
};

#endif // #ifndef jumptodialog_h__included