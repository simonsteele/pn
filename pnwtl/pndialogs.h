/**
 * @file pndialogs.h
 * @brief Assorted Dialogs
 * @author Simon Steele
 * @note Copyright (c) 2002-2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef pndialogs_h__included
#define pndialogs_h__included

#include "filedialogs.h"
#include "autofiledialog.h"

class CPNFolderDialog : public CFolderDialogImpl<CPNFolderDialog>
{
	public:
		CPNFolderDialog(HWND hWndParent = NULL, LPCTSTR lpstrInitial = NULL, LPCTSTR lpstrTitle = NULL, UINT uFlags = BIF_RETURNONLYFSDIRS | BIF_USENEWUI);

		void OnInitialized();

	protected:
		CString		m_csInitialDir;
};

/**
 * Base class for input dialogs - can't believe there's no InputBox() function!!!
 */
template <class T>
class CInputDialogImpl : public CDialogImpl< CInputDialogImpl<T> >
{
	public:
		CInputDialogImpl() : m_title(_T("Input")), m_caption(_T("Input:")){}
		CInputDialogImpl(LPCTSTR title, LPCTSTR caption) : m_title(title), m_caption(caption){}

		enum { IDD = IDD_INPUTBOX };

		BEGIN_MSG_MAP(CInputDialogImpl)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_ID_HANDLER(IDOK, OnOK)
			COMMAND_ID_HANDLER(IDCANCEL, OnCancel)
		END_MSG_MAP()

		LPCTSTR GetInput()
		{
			return (LPCTSTR)m_inputText;
		}

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
		{
			static_cast<T*>(this)->InitDialog();
			return TRUE;
		}

		LRESULT OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
		{
			T* pT = static_cast<T*>(this);
			return pT->OK(wID);
		}

		LRESULT OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
		{
			//return static_cast<T*>(this)->Cancel(wID);
			EndDialog(wID);
			return TRUE;
		}

		LRESULT OK(WORD wID)
		{
			HWND hEdit = GetDlgItem(IDC_THEEDIT);
			int i = ::GetWindowTextLength(hEdit) + 1;
			
			LPTSTR buf = m_inputText.GetBuffer(i);
			::GetWindowText(hEdit, buf, i);
			m_inputText.ReleaseBuffer();

			EndDialog(wID);

			return TRUE;
		}

		LRESULT Cancel(WORD wID)
		{
			EndDialog(wID);
			return TRUE;
		}

		LRESULT InitDialog()
		{
			CenterWindow(GetParent());
			::SetWindowText(GetDlgItem(IDC_TEXTTITLE), m_caption.c_str());
			SetWindowText(m_title.c_str());

			return TRUE;
		}

	protected:
		tstring m_title;
		tstring m_caption;
		CString m_inputText;
};

class CInputDialog : public CInputDialogImpl<CInputDialog>
{
	public:
		CInputDialog(LPCTSTR title, LPCTSTR caption) : 
		  CInputDialogImpl<CInputDialog>(title, caption){}
};

class CGotoDialog : public CInputDialogImpl<CGotoDialog>
{
	typedef CInputDialogImpl<CGotoDialog> baseClass;

	public:
		explicit CGotoDialog(LPCTSTR caption) : CInputDialogImpl<CGotoDialog>(_T("Go To Line"), caption) {}
		LRESULT OK(WORD wID);

		int GetLineNo();

		LRESULT InitDialog();

	private:
		int lineno;
};

#endif