/**
 * @file filedialogs.h
 * @brief File Dialogs for Programmer's Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef filedialogs_h__included
#define filedialogs_h__included

#include "filedialogbase.h"

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
 * @brief File Dialog with special functionality for opening files.
 * This "special functionality" basically involves being able to open
 * an unlimited number of files with OFN_ALLOWMULTISELECT and also the
 * fact that the class automatically parses any selected filenames 
 * (multiple or single) into a list of tstrings.
 */
class CPNOpenDialog : public CPNFileDialogImpl<CPNOpenDialog>, public IFileOpenDialogBase
{
	typedef CPNFileDialogImpl<CPNOpenDialog> baseClass;
	public:
		CPNOpenDialog(LPCTSTR szFilter, LPCTSTR szPath = NULL);
		virtual ~CPNOpenDialog();

		virtual INT_PTR DoModal(HWND hWndParent = ::GetActiveWindow());

		virtual LPCTSTR GetSingleFileName();

		virtual void SetTitle(LPCTSTR title);

		virtual void SetAllowMultiSelect(bool allow);

		virtual void SetInitialPath(LPCTSTR initial);

		void OnSelChange(LPOFNOTIFY /*lpon*/);

		// Collection functionality:
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

class CVistaDialogHelper
{
public:
	void InitialiseFilters(LPCWSTR filter);

	const COMDLG_FILTERSPEC* GetFilters() const;

	size_t GetFilterCount() const;

	HRESULT GetPathShellItem(LPCTSTR path, IShellItem** si);
private:
	std::vector<wchar_t> m_filterbuf;
	std::vector<COMDLG_FILTERSPEC> m_filters;
};

class CVistaOpenDialog : public IFileOpenDialogBase
{
public:
	CVistaOpenDialog(LPCWSTR filter);
	virtual ~CVistaOpenDialog();

	virtual INT_PTR DoModal(HWND hWndParent = ::GetActiveWindow());

	virtual LPCTSTR GetSingleFileName();

	virtual void SetTitle(LPCTSTR title);

	virtual void SetAllowMultiSelect(bool allow);

	virtual void SetInitialPath(LPCTSTR initial);

	virtual const_iterator begin();
	virtual const_iterator end();

protected:
	CShellFileOpenDialog* GetDialog();

private:
	CShellFileOpenDialog* m_dialog;
	std::vector<tstring> m_foundfiles;
	CVistaDialogHelper m_helper;
	tstring m_title;
};

class CVistaSaveDialog : public IFileSaveDialogBase
{
public:
	CVistaSaveDialog(LPCWSTR filter);
	virtual ~CVistaSaveDialog();

	virtual INT_PTR DoModal(HWND hWndParent = ::GetActiveWindow());

	virtual LPCTSTR GetSingleFileName();

	virtual void SetTitle(LPCTSTR title);
	virtual void SetInitialPath(LPCTSTR initial);
	virtual void SetDefaultExtension(LPCTSTR ext);
	virtual void SetInitialFilename(LPCTSTR initial);

protected:
	CShellFileSaveDialog* GetDialog();

private:
	CShellFileSaveDialog* m_dialog;
	CVistaDialogHelper m_helper;
	tstring m_title;
	tstring m_result;
};

class CPNSaveDialog : public CPNFileDialogImpl<CPNSaveDialog>, public IFileSaveDialogBase
{
typedef CPNFileDialogImpl<CPNSaveDialog> baseClass;
public:
	CPNSaveDialog(LPCTSTR szFilter, LPCTSTR szPath = NULL, LPCTSTR szDefaultExt = NULL);
	virtual ~CPNSaveDialog();

	virtual INT_PTR DoModal(HWND hWndParent = ::GetActiveWindow());

	virtual LPCTSTR GetSingleFileName();

	virtual void SetTitle(LPCTSTR title);
	virtual void SetInitialPath(LPCTSTR initial);

	virtual void SetDefaultExtension(LPCTSTR ext);
	virtual void SetInitialFilename(LPCTSTR initial);

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
class CPNSaveDialogEx : public CPNSaveDialog, public IHasSaveFormat,
	public CCustomDialogImpl<CPNSaveDialogEx>
{
	typedef CPNSaveDialog baseClass;
	public:
		CPNSaveDialogEx(LPCTSTR szFilter, LPCTSTR szPath = NULL);

		BEGIN_MSG_MAP (CPNSaveDialogEx)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			MESSAGE_HANDLER(WM_SIZE, OnSize)

			COMMAND_HANDLER(IDC_PNSAVE_TYPECOMBO, CBN_SELCHANGE, OnComboSelChange) 

			CHAIN_MSG_MAP (baseClass);
		END_MSG_MAP ()

		virtual EPNSaveFormat GetSaveFormat();

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
	public CCustomDialogImpl<CPNOpenDialogEx>,
	public IHasEncoding
{
	public:
		CPNOpenDialogEx(LPCTSTR szFilter, LPCTSTR szPath = NULL);

		BEGIN_MSG_MAP(CPNOpenDialogEx)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			MESSAGE_HANDLER(WM_SIZE, OnSize)

			COMMAND_HANDLER(IDC_PNOPEN_ENCODINGCOMBO, CBN_SELCHANGE, OnComboSelChange) 

			CHAIN_MSG_MAP(CPNOpenDialog)
		END_MSG_MAP()

		virtual EPNEncoding GetEncoding();

	protected:
		LRESULT OnComboSelChange(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnSize (UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled);
		LRESULT OnInitDialog (UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled);

		void RepositionControls();

		CComboBox		m_EncodingCombo;
		CStatic			m_EncodingLabel;
		EPNEncoding		m_Encoding;
};

class CVistaOpenDialogEx : public CVistaOpenDialog, public IHasEncoding
{
public:
	CVistaOpenDialogEx(LPCWSTR filter);
	virtual ~CVistaOpenDialogEx();

	virtual EPNEncoding GetEncoding();
};

class CVistaSaveDialogEx : public CVistaSaveDialog, public IHasSaveFormat
{
public:
	CVistaSaveDialogEx(LPCWSTR filter);
	virtual ~CVistaSaveDialogEx();

	virtual EPNSaveFormat GetSaveFormat();
};

#endif