/**
 * @file jumptodialog.h
 * @brief Jump To Dialog
 * @author Simon Steele
 * @note Copyright (c) 2004-2011 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef JUMPTODIALOG_H_INCLUDED
#define JUMPTODIALOG_H_INCLUDED

/**
 * Target to jump to.
 */
class Target
{
public:
	explicit Target(const char* tag, int line, int image) : Tag(tag), Line(line), Image(image) {}
	explicit Target(const char* tag, const char* parent, int line, int image) : Tag(tag), Parent(parent), Line(line), Image(image) {}
	Target(const Target& other) : Tag(other.Tag), Parent(other.Parent), Line(other.Line), Image(other.Image) {}

	std::string Tag;
	std::string Parent;
	int Line;
	int Image;
};

/**
 * @brief Jump to dialog class
 */
class CJumpToDialog : public CDialogImpl<CJumpToDialog>, extensions::ITagSink
{
	typedef CDialogImpl<CJumpToDialog> baseClass;

	public:
		explicit CJumpToDialog(CChildFrame* pChild);

		BEGIN_MSG_MAP(CJumpToDialog)
			COMMAND_HANDLER(IDC_JUMPTOTEXT, EN_CHANGE, OnTextKeyPress)
			COMMAND_ID_HANDLER(IDOK, OnOk)
			COMMAND_ID_HANDLER(IDCANCEL, OnCancel)

			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			MESSAGE_HANDLER(WM_SIZE, OnSize)

			NOTIFY_HANDLER(IDC_JUMPTOLIST, NM_DBLCLK, OnListDblClick)

			ALT_MSG_MAP(1)
			MESSAGE_HANDLER(WM_KEYDOWN, OnEditKeyDown)

		END_MSG_MAP()
		enum { IDD = IDD_JUMPTO };

		LRESULT OnTextKeyPress(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnOk(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		virtual void OnFound(int count, LPMETHODINFO methodInfo);

		int GetLine();

	private:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnListDblClick(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnEditKeyDown(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

		void filter(LPCTSTR text);
		void display(const std::vector<Target>& targets);
		void transfer();

		std::vector<Target> m_targets;
		std::vector<Target> m_filtered;
		CContainedWindowT<CEdit> edtTag;
		CChildFrame*	m_pChild;
		CButton			btnOk;
		CButton			btnCancel;
		CListViewCtrl	list;
		CImageList		images;
		int				buttonGap;
		int				buttonWidth;
		int				buttonGapX;
		SIZE			listGaps;
		TCHAR			itoabuf[20];
		int				line;
};

#endif // #ifndef JUMPTODIALOG_H_INCLUDED