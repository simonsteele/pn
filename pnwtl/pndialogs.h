/**
 * @file pndialogs.h
 * @brief Assorted Dialogs for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef pndialogs_h__included
#define pndialogs_h__included

#include <list>
using std::list;

#include "resource.h"

/**
 * @class CPNFileDialogImpl
 * @brief Very simply adds handling for pipe (|) separated filters.
 */
template <class T>
class CPNFileDialogImpl : public CFileDialogImpl<T>
{
	typedef CFileDialogImpl<T> baseClass;

	public:
		CPNFileDialogImpl(BOOL bOpenFileDialog, // TRUE for FileOpen, FALSE for FileSaveAs
			LPCTSTR lpszDefExt = NULL,
			LPCTSTR lpszFileName = NULL,
			DWORD dwFlags = OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
			LPCTSTR lpszFilter = NULL,
			HWND hWndParent = NULL,
			bool bUsePipeChar = true)
			: CFileDialogImpl<T>(bOpenFileDialog, lpszDefExt, lpszFileName, dwFlags, lpszFilter, hWndParent)
		{
			if(lpszFilter != NULL && bUsePipeChar)
			{
				m_szFilter = new TCHAR[_tcslen(lpszFilter)+1];
				_tcscpy(m_szFilter, lpszFilter);
				
				LPTSTR pch = m_szFilter;
				while ((pch = _tcschr(pch, '|')) != NULL)
					*pch++ = '\0';

				m_ofn.lpstrFilter = m_szFilter;
			}
			else
			{
				m_szFilter = NULL;
				m_ofn.lpstrFilter = lpszFilter;
			}				
		}

		~CPNFileDialogImpl()
		{
			if(m_szFilter != NULL)
				delete [] m_szFilter;
		}

		BEGIN_MSG_MAP(CPNFileDialogImpl)
			CHAIN_MSG_MAP(baseClass)
		END_MSG_MAP()

	protected:
		LPTSTR m_szFilter;
};

/**
 * @brief Empty implementation of CPNFileDialogImpl
 */
class CPNFileDialog : public CPNFileDialogImpl<CPNFileDialog>
{
public:
	typedef CPNFileDialogImpl<CPNFileDialog> baseClass;

	CPNFileDialog(BOOL bOpenFileDialog, // TRUE for FileOpen, FALSE for FileSaveAs
		LPCTSTR lpszDefExt = NULL,
		LPCTSTR lpszFileName = NULL,
		DWORD dwFlags = OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		LPCTSTR lpszFilter = NULL,
		HWND hWndParent = NULL)
		: CPNFileDialogImpl<CPNFileDialog>(bOpenFileDialog, lpszDefExt, lpszFileName, dwFlags, lpszFilter, hWndParent)
	{ }

	// override base class map and references to handlers
	BEGIN_MSG_MAP(CPNFileDialog)
        CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()
};

/**
 * @brief File Dialog with special functionality for opening files.
 * This "special functionality" basically involves being able to open
 * an unlimited number of files with OFN_ALLOWMULTISELECT and also the
 * fact that the class automatically parses any selected filenames 
 * (multiple or single) into a list of tstrings.
 */
class CPNOpenDialog : public CPNFileDialogImpl<CPNOpenDialog>
{
	typedef CPNFileDialogImpl<CPNOpenDialog> baseClass;
	public:
		typedef std::list<tstring>::const_iterator const_iterator;

		CPNOpenDialog(LPCTSTR szFilter, LPCTSTR szPath = NULL);
		~CPNOpenDialog();

		INT_PTR DoModal(HWND hWndParent = ::GetActiveWindow());

		void OnSelChange(LPOFNOTIFY /*lpon*/);

		// List functionality:
		int GetCount();
		const std::list<tstring>& GetFiles();
		const_iterator begin();
		const_iterator end();

	protected:
		TCHAR*	m_szFilesBuffer;
		TCHAR*	m_szFolder;
		bool	m_bParsed;
		bool	m_bIncludePath;
		int		m_bufSizeFiles;
		int		m_bufSizeFolder;

		std::list<tstring>	m_files;

		void Clear();
		void PreProcess();
		inline void EnsureBuffer(TCHAR*& buffer, int size, int& cursize);

	BEGIN_MSG_MAP(CPNOpenDialog)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()
};

/**
 * @class CPNSaveDialog
 * @brief Save dialog with added controls for save type (e.g. LF, CRLF, etc.)
 *
 * Thanks to Tim Smith for his excellent Code Project article which taught me
 * how to do this without tearing my hair out.
 * link: http://www.codeproject.com/useritems/uoth.asp
 */
class CPNSaveDialog : public CPNFileDialogImpl<CPNSaveDialog>
{
	typedef CPNFileDialogImpl<CPNSaveDialog> baseClass;
	public:
		CPNSaveDialog(LPCTSTR szFilter);

		BEGIN_MSG_MAP (CPNSaveDialog)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			MESSAGE_HANDLER(WM_SIZE, OnSize)

			COMMAND_HANDLER(IDC_PNSAVE_TYPECOMBO, CBN_SELCHANGE, OnComboSelChange) 

			CHAIN_MSG_MAP (baseClass);
		END_MSG_MAP ()

		EPNSaveFormat GetSaveFormat();

	protected:
		LRESULT OnComboSelChange(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnSize (UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled);
		LRESULT OnInitDialog (UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled);

		void RepositionControls();
		void RepositionControl(CWindow &wnd, UINT nID, bool fSize);
		void RepositionPlacesBar(CWindow &bottomwnd);

		CComboBox		m_SaveTypeCombo;
		CStatic			m_SaveTypeLabel;
		EPNSaveFormat	m_Format;
};

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
class CInputDialogImpl : public CDialogImpl<CInputDialogImpl>
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
			
			CWindow wnd;
			wnd.Attach(hEdit);
			wnd.GetWindowText(m_inputText);
			wnd.Detach();

			/*LPTSTR buf = m_inputText.GetBuffer(i);
			::GetWindowText(hEdit, buf, i);
			m_inputText.ReleaseBuffer();*/

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
			::SetWindowText(GetDlgItem(IDC_TEXTTITLE), m_caption);
			SetWindowText(m_title);

			return TRUE;
		}

	protected:
		CString m_title;
		CString m_caption;
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
	public:
		CGotoDialog() : CInputDialogImpl<CGotoDialog>(_T("Go To"), _T("Go To:")) {}
		LRESULT OK(WORD wID);

		int GetLineNo();
	
	protected:
		int lineno;
};

#endif