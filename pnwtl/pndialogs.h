/**
 * @file pndialogs.h
 * @brief Assorted Dialogs for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef pndialogs_h__included
#define pndialogs_h__included

#include <list>
using std::list;

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

		LPCTSTR GetSingleFileName()
		{
			return m_ofn.lpstrFile;
		}

		void SetTitle(LPCTSTR title)
		{
			m_ofn.lpstrTitle = title;
		}

		void SetDefaultExtension(LPCTSTR defext)
		{
			m_ofn.lpstrDefExt = defext;
		}

		void SetInitialPath(LPCTSTR path)
		{
			m_ofn.lpstrInitialDir = path;
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

class COpenDialogBase
{
public:
	virtual ~COpenDialogBase(){}
	virtual INT_PTR DoModal(HWND hWndParent = ::GetActiveWindow()) = 0;

	virtual LPCTSTR GetSingleFileName() = 0;
	virtual void SetTitle(LPCTSTR title) = 0;
	virtual void SetAllowMultiSelect(bool allow) = 0;

	typedef std::vector<tstring>::const_iterator const_iterator;

	virtual const_iterator begin() = 0;
	virtual const_iterator end() = 0;
};

/**
 * @brief File Dialog with special functionality for opening files.
 * This "special functionality" basically involves being able to open
 * an unlimited number of files with OFN_ALLOWMULTISELECT and also the
 * fact that the class automatically parses any selected filenames 
 * (multiple or single) into a list of tstrings.
 */
class CPNOpenDialog : public CPNFileDialogImpl<CPNOpenDialog>, public COpenDialogBase
{
	typedef CPNFileDialogImpl<CPNOpenDialog> baseClass;
	public:

		CPNOpenDialog(LPCTSTR szFilter, LPCTSTR szPath = NULL);
		virtual ~CPNOpenDialog();

		virtual INT_PTR DoModal(HWND hWndParent = ::GetActiveWindow());

		virtual LPCTSTR GetSingleFileName()
		{
			return CPNFileDialogImpl<CPNOpenDialog>::GetSingleFileName();
		}

		virtual void SetTitle(LPCTSTR title)
		{
			CPNFileDialogImpl<CPNOpenDialog>::SetTitle(title);
		}

		virtual void SetAllowMultiSelect(bool allow)
		{
			if (allow)
			{
				m_ofn.Flags |= OFN_ALLOWMULTISELECT;
			}
			else
			{
				m_ofn.Flags &= ~OFN_ALLOWMULTISELECT;
			}
		}
	

		void OnSelChange(LPOFNOTIFY /*lpon*/);

		// List functionality:
		int GetCount();
		const std::vector<tstring>& GetFiles();
		virtual const_iterator begin();
		virtual const_iterator end();

	protected:
		TCHAR*	m_szFilesBuffer;
		TCHAR*	m_szFolder;
		bool	m_bParsed;
		bool	m_bIncludePath;
		int		m_bufSizeFiles;
		int		m_bufSizeFolder;

		std::vector<tstring>	m_files;

		void Clear();
		void PreProcess();
		inline void EnsureBuffer(TCHAR*& buffer, int size, int& cursize);

	BEGIN_MSG_MAP(CPNOpenDialog)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()
};

class CVistaOpenDialog : public COpenDialogBase
{
public:
	CVistaOpenDialog(LPCWSTR filter);
	virtual ~CVistaOpenDialog();

	virtual INT_PTR DoModal(HWND hWndParent = ::GetActiveWindow());

	virtual LPCTSTR GetSingleFileName();

	virtual void SetTitle(LPCTSTR title);

	virtual void SetAllowMultiSelect(bool allow);

	virtual const_iterator begin();
	virtual const_iterator end();

private:
	CShellFileOpenDialog* m_dialog;
	std::vector<wchar_t> m_filterbuf;
	std::vector<COMDLG_FILTERSPEC> m_filters;
	std::vector<tstring> m_foundfiles;
	tstring m_title;
};

template <class TClassicDialog, class TVistaDialog>
class CAdvancedOpenDialogImpl
{
public:
	CAdvancedOpenDialogImpl(LPCTSTR filter)
	{
		if (WTL::RunTimeHelper::IsVista())
		{
			CT2CW filterw(filter);
			m_dialog = new TVistaDialog(filterw);
		}
		else
		{
			m_dialog = new TClassicDialog(filter, m_path.size() ? m_path.c_str() : NULL);
		}
	}

	~CAdvancedOpenDialogImpl()
	{
		if (m_dialog != NULL)
		{
			delete m_dialog;
		}
	}

	INT_PTR DoModal(HWND hWndParent = ::GetActiveWindow())
	{
		PNASSERT(m_dialog != NULL);
		return m_dialog->DoModal(hWndParent);
	}

	LPCTSTR GetSingleFileName()
	{
		PNASSERT(m_dialog != NULL);
		return m_dialog->GetSingleFileName();
	}

	void SetTitle(LPCTSTR title)
	{
		PNASSERT(m_dialog != NULL);
		return m_dialog->SetTitle(title);
	}

	void SetAllowMultiSelect(bool allow)
	{
		PNASSERT(m_dialog != NULL);
		return m_dialog->SetAllowMultiSelect(allow);
	}

	COpenDialogBase::const_iterator begin()
	{
		PNASSERT(m_dialog != NULL);
		return m_dialog->begin();
	}

	COpenDialogBase::const_iterator end()
	{
		PNASSERT(m_dialog != NULL);
		return m_dialog->end();
	}


private:
	tstring m_path;
	COpenDialogBase* m_dialog;
};

typedef CAdvancedOpenDialogImpl<CPNOpenDialog, CVistaOpenDialog> CAdvancedOpenDialog;

class CPNSaveDialog : public CPNFileDialogImpl<CPNSaveDialog>
{
	typedef CPNFileDialogImpl<CPNSaveDialog> baseClass;
	public:
		CPNSaveDialog(LPCTSTR szFilter, LPCTSTR szPath = NULL, LPCTSTR szDefaultExt = NULL);

	BEGIN_MSG_MAP(CPNSaveDialog)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()
};

template<class T>
class CCustomDialogImpl
{
protected:
	void RepositionPlacesBar(CWindow &bottomwnd)
	{
		T* pT = static_cast<T*>(this);

		CRect rc, rc2;
		CWindow wndParent = pT->GetParent();
		CWindow wndPB = wndParent.GetDlgItem( ctl1 );
		if( wndPB.IsWindow() )
		{
			wndPB.GetWindowRect( &rc );
			bottomwnd.GetWindowRect( &rc2 );
			wndParent.ScreenToClient( &rc );
			pT->ScreenToClient( &rc2 );
			rc.bottom = rc2.bottom;
			wndPB.SetWindowPos( NULL, &rc, SWP_NOACTIVATE | SWP_NOZORDER );
		}
	}

	/**
	* @brief Reposition a control
	* @param wnd Control to be reposition
	* @param nID ID of the control used for positioning
	* @param fSize If true, adjust the width of the control
	*/
	void RepositionControl(CWindow &wnd, UINT nID, bool fSize)
	{
		T* pT = static_cast<T*>(this);

		// Get the window rect in the client area of the 
		// control we are interested in.
		CWindow wndParent = pT->GetParent();
		CWindow wndAnchor = wndParent.GetDlgItem( nID );
		CRect rectAnchor;
		wndAnchor.GetWindowRect( &rectAnchor );
		wndParent.ScreenToClient( &rectAnchor );

		//
		// Reposition the control
		//

		DWORD dwSWFlags = SWP_NOACTIVATE | SWP_NOZORDER | SWP_NOSIZE;
		CRect rectCtrl;
		wnd.GetWindowRect( &rectCtrl );
		pT->ScreenToClient( &rectCtrl );
		rectCtrl.OffsetRect( rectAnchor.left - rectCtrl.left, 0 );
		if( fSize )
		{
			rectCtrl.right = rectCtrl.left + rectAnchor.Width();
			dwSWFlags &= ~SWP_NOSIZE;
		}
		wnd.SetWindowPos( NULL, rectCtrl.left, rectCtrl.top,
			rectCtrl.Width(), rectCtrl.Height(), dwSWFlags );
	}
};

/**
 * @class CPNSaveDialog
 * @brief Save dialog with added controls for save type (e.g. LF, CRLF, etc.)
 *
 * Thanks to Tim Smith for his excellent Code Project article which taught me
 * how to do this without tearing my hair out.
 * link: http://www.codeproject.com/useritems/uoth.asp
 */
class CPNSaveDialogEx : public CPNFileDialogImpl<CPNSaveDialogEx>, 
	public CCustomDialogImpl<CPNSaveDialogEx>
{
	typedef CPNFileDialogImpl<CPNSaveDialogEx> baseClass;
	public:
		CPNSaveDialogEx(LPCTSTR szFilter, LPCTSTR szPath = NULL);

		BEGIN_MSG_MAP (CPNSaveDialogEx)
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

class CPNOpenDialogEx : public CPNOpenDialog,
	public CCustomDialogImpl<CPNOpenDialogEx>
{
	public:
		CPNOpenDialogEx(LPCTSTR szFilter, LPCTSTR szPath = NULL);

		BEGIN_MSG_MAP(CPNOpenDialogEx)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			MESSAGE_HANDLER(WM_SIZE, OnSize)

			COMMAND_HANDLER(IDC_PNOPEN_ENCODINGCOMBO, CBN_SELCHANGE, OnComboSelChange) 

			CHAIN_MSG_MAP(CPNOpenDialog)
		END_MSG_MAP()

		EPNEncoding GetEncoding();

	protected:
		LRESULT OnComboSelChange(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnSize (UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled);
		LRESULT OnInitDialog (UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled);

		void RepositionControls();

		CComboBox		m_EncodingCombo;
		CStatic			m_EncodingLabel;
		EPNEncoding		m_Encoding;
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
		CGotoDialog(LPCTSTR caption) : CInputDialogImpl<CGotoDialog>(_T("Go To Line"), caption) {}
		LRESULT OK(WORD wID);

		int GetLineNo();
	
	protected:
		int lineno;
};

#endif