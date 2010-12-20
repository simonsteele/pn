/**
 * @file filedialogs.cpp
 * @brief File Dialogs
 * @author Simon Steele
 * @note Copyright (c) 2008-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "filedialogs.h"
#include "include/atlshellext.h"
#include "include/ShellCtrls.h"

//////////////////////////////////////////////////////////////////////////////
// CPNOpenDialog
//////////////////////////////////////////////////////////////////////////////

CPNOpenDialog::CPNOpenDialog(LPCTSTR szFilter, LPCTSTR szPath) :
	baseClass(TRUE, _T("txt"), szPath, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT, szFilter)
{
	m_szFilesBuffer = NULL;
	m_szFolder = NULL;
	m_bParsed = false;
	m_bIncludePath = true;
	m_bufSizeFiles = 0;
	m_bufSizeFolder = 0;
}

CPNOpenDialog::~CPNOpenDialog()
{
	Clear();
}

/**
 * Clean up any internal buffers used.
 */
void CPNOpenDialog::Clear()
{
	if(m_szFilesBuffer)
	{
		delete [] m_szFilesBuffer;
		m_szFilesBuffer = NULL;
	}

	if(m_szFolder)
	{
		delete [] m_szFolder;
		m_szFolder = NULL;
	}

	m_bParsed = false;
	m_bufSizeFiles = 0;
	m_bufSizeFolder = 0;
}

/**
 * Take over the DoModal call to ensure we return OK if the only
 * error is FNERR_BUFFERTOOSMALL.
 */
INT_PTR CPNOpenDialog::DoModal(HWND hWndParent)
{
    Clear();

    INT_PTR ret = baseClass::DoModal();

    if (ret == IDCANCEL)
    {
        DWORD err = CommDlgExtendedError();
        if (err == FNERR_BUFFERTOOSMALL/*0x3003*/ && m_szFilesBuffer)
            ret = IDOK;
    }

	return ret;
}

/**
 * This function basically ensures that a buffer is large enough for "size",
 * and if not then it re-creates the buffer with a size of size+1*2.
 */
inline void CPNOpenDialog::EnsureBuffer(TCHAR*& buffer, int size, int& cursize)
{
	if(!buffer)
	{
		cursize = (size + 1) * 2;
		buffer = new TCHAR[cursize];
	}
	else if(size > cursize)
	{
		cursize = (size + 1) * 2;
		delete [] buffer;
		buffer = new TCHAR[cursize];
	}
}

LPCTSTR CPNOpenDialog::GetSingleFileName()
{
	return CPNFileDialogImpl<CPNOpenDialog>::GetSingleFileName();
}

void CPNOpenDialog::SetTitle(LPCTSTR title)
{
	CPNFileDialogImpl<CPNOpenDialog>::SetTitle(title);
}

void CPNOpenDialog::SetAllowMultiSelect(bool allow)
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

void CPNOpenDialog::SetInitialPath(LPCTSTR initial)
{
	CPNFileDialogImpl<CPNOpenDialog>::SetInitialPath(initial);
}

/**
 * Here we make sure we store any data from the file dialog
 * if the buffer in m_ofn is too small to cope.
 * @see EnsureBuffer
 */
void CPNOpenDialog::OnSelChange(LPOFNOTIFY /*lpon*/)
{
	TCHAR dummy_buffer;
    
    // Get the required size for the 'files' buffer
	UINT nfiles = CommDlg_OpenSave_GetSpec(GetParent(), &dummy_buffer, 1);

    // Get the required size for the 'folder' buffer
	UINT nfolder = CommDlg_OpenSave_GetFolderPath(GetParent(), &dummy_buffer, 1);

    // Check if lpstrFile and nMaxFile are large enough
    if (nfiles + nfolder > m_ofn.nMaxFile)
    {
        //bParsed = FALSE;
		EnsureBuffer(m_szFilesBuffer, nfiles, m_bufSizeFiles);
        
        CommDlg_OpenSave_GetSpec(GetParent(), m_szFilesBuffer, nfiles);

        EnsureBuffer(m_szFolder, nfolder, m_bufSizeFolder);

        CommDlg_OpenSave_GetFolderPath(GetParent(), m_szFolder, nfolder);
    }
    else if (m_szFilesBuffer)
    {
        Clear();
    }
}

int CPNOpenDialog::GetCount()
{
	if(!m_bParsed)
		PreProcess();

	return m_files.size();
}

const std::vector<tstring>& CPNOpenDialog::GetFiles()
{
	if(!m_bParsed)
		PreProcess();

	return m_files;
}

CPNOpenDialog::const_iterator CPNOpenDialog::begin()
{
	if(!m_bParsed)
		PreProcess();

	return m_files.begin();
}

/// @note We don't check PreProcessed here, assuming begin will be called first.
CPNOpenDialog::const_iterator CPNOpenDialog::end()
{
	return m_files.end();
}

/**
 * This function processes either our own buffer (m_szFilesBuffer) or
 * the standard one and adds each filename found to a list<tstring>.
 */
void CPNOpenDialog::PreProcess()
{
	if(m_szFilesBuffer)
	{
		CString temp = m_szFilesBuffer;
		temp.Replace(_T("\" \""), _T("\""));	// replace gaps between files with single quote.
		temp.Delete(0, 1);                      // remove leading quote mark
		temp.Delete(temp.GetLength() - 1, 1);   // remove trailing space

		_tcscpy(m_szFilesBuffer, temp);
		tstring stemp;
		tstring sFolder = m_szFolder;
		if(sFolder[sFolder.size()-1] != _T('\\'))
			sFolder += _T('\\');
		TCHAR *ptr = m_szFilesBuffer;
		TCHAR *pLast = ptr;
		while (*ptr)
		{
			if ('\"' == *ptr) 
			{
				*ptr++ = '\0';
				if(m_bIncludePath) 
				{
					stemp = sFolder;
					stemp += pLast;
					m_files.push_back(stemp);
				}
				else
					m_files.push_back(tstring(pLast));

				pLast = ptr;
			} 
			else
				++ptr;
		}
	}
	else
	{
		if ((m_ofn.Flags & OFN_ALLOWMULTISELECT) == 0)
		{
			m_files.push_back(tstring(m_ofn.lpstrFile));
		}
		else
		{
			// NULL separated...
			tstring stemp;
			tstring sFolder = m_ofn.lpstrFile;
			TCHAR* ptr = m_ofn.lpstrFile + _tcslen(m_ofn.lpstrFile) + 1;
			
			if(!*ptr)
			{
				m_files.push_back(sFolder);	// Must be a single file.
			}
			else
			{
				if(sFolder[sFolder.size()-1] != _T('\\'))
				sFolder += _T('\\');
				TCHAR* pLast = ptr;
				while (*ptr)
				{
					//++ptr;

					// Skip past a single NULL...
					if(!*++ptr)
					{
						if(m_bIncludePath) 
						{
							stemp = sFolder;
							stemp += pLast;
							m_files.push_back(stemp);
						}
						else
							m_files.push_back(tstring(pLast));

						++ptr;
						pLast = ptr;
					}
				}
			}
		}
	}

	m_bParsed = true;
}

//////////////////////////////////////////////////////////////////////////////
// CVistaDialogHelper
//////////////////////////////////////////////////////////////////////////////

void CVistaDialogHelper::InitialiseFilters(LPCWSTR filter)
{
	m_filterbuf.resize(wcslen(filter)+1);
	wcscpy(&m_filterbuf[0], filter);

	bool isFilter(false);
	wchar_t* pch = &m_filterbuf[0];
	wchar_t* pchlast = pch;
	COMDLG_FILTERSPEC spec;
	while ((pch = wcschr(pch, L'|')) != NULL)
	{
		*pch++ = L'\0';
		
		if (!isFilter)
		{
			spec.pszName = pchlast;
		}
		else
		{
			spec.pszSpec = pchlast;
			m_filters.push_back(spec);
		}

		pchlast = pch;
		isFilter = !isFilter;
	}
}

const COMDLG_FILTERSPEC* CVistaDialogHelper::GetFilters() const
{
	return &m_filters[0];
}

size_t CVistaDialogHelper::GetFilterCount() const
{
	return m_filters.size();
}

HRESULT SafeCreateShellItem(PCIDLIST_ABSOLUTE pidlParent, IShellFolder *psfParent, PCUITEMID_CHILD pidl, IShellItem **ppsi)
{
	static HMODULE shell32 = ::LoadLibrary(_T("shell32.dll"));
	if (!shell32) 
	{
		return E_FAIL;
	}

	typedef HRESULT (STDAPICALLTYPE *PFNCREATESHELLITEM)(PCIDLIST_ABSOLUTE pidlParent, IShellFolder *psfParent, PCUITEMID_CHILD pidl, IShellItem **ppsi);
	static PFNCREATESHELLITEM pfnCreateShellItem = reinterpret_cast<PFNCREATESHELLITEM>(::GetProcAddress(shell32, "SHCreateShellItem"));
	if (pfnCreateShellItem == NULL)
	{
		return E_FAIL;
	}

	return pfnCreateShellItem(pidlParent, psfParent, pidl, ppsi);
}

HRESULT CVistaDialogHelper::GetPathShellItem(LPCTSTR path, IShellItem** si)
{
	if (path == NULL)
	{
		return E_FAIL;
	}

	CFileName fn(path);
	tstring strPath = fn.GetPath();

	if (strPath.size() == 0)
	{
		return E_FAIL;
	}

	CT2CW wPath(strPath.c_str());

	CPidl pidl;
	DWORD rgfInOut = 0;

	if (!SUCCEEDED(SHILCreateFromPath(wPath, &pidl, &rgfInOut)))
	{
		return E_FAIL;
	}

	return SafeCreateShellItem(NULL, NULL, pidl, si);
}

//////////////////////////////////////////////////////////////////////////////
// CVistaOpenDialog
//////////////////////////////////////////////////////////////////////////////

CVistaOpenDialog::CVistaOpenDialog(LPCWSTR filter) :
	m_dialog(NULL)
{
	m_helper.InitialiseFilters(filter);

	m_dialog = new CShellFileOpenDialog(
		 /*lpszFileName = */NULL, 
         FOS_FORCEFILESYSTEM | FOS_PATHMUSTEXIST | FOS_FILEMUSTEXIST, 
         /*defaultExtension*/NULL, 
         m_helper.GetFilters(),
         m_helper.GetFilterCount());
}

CVistaOpenDialog::~CVistaOpenDialog()
{
	if (m_dialog)
	{
		delete m_dialog;
	}
}

template <class T> 
class AutoTaskMemory
{
public:
	AutoTaskMemory() : p(NULL)
	{
	}

	~AutoTaskMemory()
	{
		if (p)
		{
			::CoTaskMemFree(p);
		}
	}

	T* p;
};

INT_PTR CVistaOpenDialog::DoModal(HWND hWndParent)
{
	if (m_title.size())
	{
		CT2CW title(m_title.c_str());
		m_dialog->GetPtr()->SetTitle(title);
	}

	INT_PTR res = m_dialog->DoModal(hWndParent);
	if (res != IDOK)
	{
		return res;
	}

	CComPtr<IShellItemArray> resultItems;
	HRESULT hr = m_dialog->GetPtr()->GetResults(&resultItems);
	if (!SUCCEEDED(hr))
	{
		return 0;
	}

	DWORD count = 0;
	hr = resultItems->GetCount(&count);
	if (!SUCCEEDED(hr))
	{
		return 0;
	}
	
	for (DWORD i = 0; i < count; i++)
	{
		CComPtr<IShellItem> shellItem;
		if (SUCCEEDED(resultItems->GetItemAt(i, &shellItem)))
		{
			AutoTaskMemory<wchar_t> name;

			if (SUCCEEDED(shellItem->GetDisplayName(SIGDN_FILESYSPATH, &name.p)))
			{
				CW2CT fn(name.p);
				m_foundfiles.push_back(tstring(fn));
			}
		}
	}
	
	return IDOK;
}

LPCTSTR CVistaOpenDialog::GetSingleFileName()
{
	if (m_foundfiles.size())
	{
		return m_foundfiles[0].c_str();
	}

	return NULL;
}

void CVistaOpenDialog::SetTitle(LPCTSTR title)
{
	m_title = title;
}

void CVistaOpenDialog::SetAllowMultiSelect(bool allow)
{
	DWORD curOptions;
	if (SUCCEEDED(m_dialog->GetPtr()->GetOptions(&curOptions)))
	{
		if (allow)
		{
			curOptions |= FOS_ALLOWMULTISELECT;
		}
		else
		{
			curOptions &= ~FOS_ALLOWMULTISELECT;
		}
		
		m_dialog->GetPtr()->SetOptions(curOptions);
	}
}

void CVistaOpenDialog::SetInitialPath(LPCTSTR initial)
{
	CComPtr<IShellItem> si;
	if (SUCCEEDED(m_helper.GetPathShellItem(initial, &si)))
	{
		m_dialog->GetPtr()->SetFolder(si);
	}
}

IFileOpenDialogBase::const_iterator CVistaOpenDialog::begin()
{
	return m_foundfiles.begin();
}

IFileOpenDialogBase::const_iterator CVistaOpenDialog::end()
{
	return m_foundfiles.end();
}

CShellFileOpenDialog* CVistaOpenDialog::GetDialog()
{
	return m_dialog;
}

//////////////////////////////////////////////////////////////////////////////
// CVistaSaveDialog
//////////////////////////////////////////////////////////////////////////////

CVistaSaveDialog::CVistaSaveDialog(LPCWSTR filter) :
	m_dialog(NULL)
{
	m_helper.InitialiseFilters(filter);

	m_dialog = new CShellFileSaveDialog(
		 /*lpszFileName = */NULL, 
		 FOS_FORCEFILESYSTEM | FOS_PATHMUSTEXIST | FOS_OVERWRITEPROMPT, 
         /*defaultExtension*/NULL, 
         m_helper.GetFilters(),
         m_helper.GetFilterCount());
}

CVistaSaveDialog::~CVistaSaveDialog()
{
	if (m_dialog)
	{
		delete m_dialog;
	}
}

INT_PTR CVistaSaveDialog::DoModal(HWND hWndParent)
{
	if (m_title.size())
	{
		CT2CW title(m_title.c_str());
		m_dialog->GetPtr()->SetTitle(title);
	}

	INT_PTR res = m_dialog->DoModal(hWndParent);
	if (res != IDOK)
	{
		return res;
	}

	CComPtr<IShellItem> si;
	if (SUCCEEDED(m_dialog->GetPtr()->GetResult(&si)))
	{
		AutoTaskMemory<wchar_t> name;
		if (SUCCEEDED(si->GetDisplayName(SIGDN_FILESYSPATH, &name.p)))
		{
			CW2CT fn(name.p);
			m_result = fn;
		}
	}

	return IDOK;
}

LPCTSTR CVistaSaveDialog::GetSingleFileName()
{
	return m_result.c_str();
}

void CVistaSaveDialog::SetTitle(LPCTSTR title)
{
	m_title = title;
}

void CVistaSaveDialog::SetInitialPath(LPCTSTR initial)
{
	CComPtr<IShellItem> si;
	if (SUCCEEDED(m_helper.GetPathShellItem(initial, &si)))
	{
		m_dialog->GetPtr()->SetFolder(si);
	}
}

void CVistaSaveDialog::SetDefaultExtension(LPCTSTR ext)
{
	CT2CW extw(ext);
	m_dialog->GetPtr()->SetDefaultExtension(extw);
}

void CVistaSaveDialog::SetInitialFilename(LPCTSTR initial)
{
	CT2CW fnw(initial);
	m_dialog->GetPtr()->SetFileName(fnw);
}

CShellFileSaveDialog* CVistaSaveDialog::GetDialog()
{
	return m_dialog;
}

//////////////////////////////////////////////////////////////////////////////
// CPNSaveDialog
//////////////////////////////////////////////////////////////////////////////

CPNSaveDialog::CPNSaveDialog(LPCTSTR szFilter, LPCTSTR szPath, LPCTSTR szDefaultExt)
	: baseClass(FALSE, szDefaultExt, szPath, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT, szFilter)
{
}

CPNSaveDialog::~CPNSaveDialog()
{
}

INT_PTR CPNSaveDialog::DoModal(HWND hWndParent)
{
	return CPNFileDialogImpl<CPNSaveDialog>::DoModal(hWndParent);
}

LPCTSTR CPNSaveDialog::GetSingleFileName()
{
	return m_ofn.lpstrFile;
}

void CPNSaveDialog::SetTitle(LPCTSTR title)
{
	CPNFileDialogImpl<CPNSaveDialog>::SetTitle(title);
}

void CPNSaveDialog::SetInitialPath(LPCTSTR initial)
{
	CPNFileDialogImpl<CPNSaveDialog>::SetInitialPath(initial);
}

void CPNSaveDialog::SetDefaultExtension(LPCTSTR ext)
{
	CPNFileDialogImpl<CPNSaveDialog>::SetDefaultExtension(ext);
}

void CPNSaveDialog::SetInitialFilename(LPCTSTR initial)
{
	SecureHelper::strncpy_x(m_szFileName, _countof(m_szFileName), initial, _TRUNCATE);
}

//////////////////////////////////////////////////////////////////////////////
// CPNSaveDialogEx
//////////////////////////////////////////////////////////////////////////////

CPNSaveDialogEx::CPNSaveDialogEx(LPCTSTR szFilter, LPCTSTR szPath) 
	: baseClass(szFilter, szPath, NULL)//OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT)
{
	m_Format = PNSF_NoChange;
	m_ofn.Flags |= OFN_ENABLETEMPLATE | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT;
	m_ofn.lpTemplateName = MAKEINTRESOURCE (IDD_PNSAVE);
}

LRESULT CPNSaveDialogEx::OnInitDialog (UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled)
{
	m_SaveTypeCombo = GetDlgItem(IDC_PNSAVE_TYPECOMBO);
	m_SaveTypeLabel = GetDlgItem(IDC_PNSAVE_SAVEASSTATIC);

	CRect rectDlg;
	GetWindowRect( &rectDlg );

	CRect rectParent;
	::GetClientRect( GetParent(), &rectParent );
	rectDlg.right = rectDlg.left + rectParent.Width();

	// Make sure there is enough room at the bottom for resize
	CRect rectCombo;
	m_SaveTypeCombo.GetWindowRect( &rectCombo );
	ScreenToClient( &rectCombo );
	int cySize = ::GetSystemMetrics( /*SM_CYSIZE*/ SM_CYSIZEFRAME );
	if (rectDlg.Height() - rectCombo.bottom < cySize)
		rectDlg.bottom = rectDlg.top + rectCombo.bottom + cySize;

	// Reposition
	SetWindowPos( NULL, 0, 0, rectDlg.Width(), rectDlg.Height(),
		SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOZORDER );

	int index = m_SaveTypeCombo.AddString(_T("No change to the file format."));
	m_SaveTypeCombo.SetItemData(index, PNSF_NoChange);
	index = m_SaveTypeCombo.AddString(_T("Ensure Windows Format (CR+LF)"));
	m_SaveTypeCombo.SetItemData(index, PNSF_Windows);
	index = m_SaveTypeCombo.AddString(_T("Ensure Unix Format (LF)"));
	m_SaveTypeCombo.SetItemData(index, PNSF_Unix);
	index = m_SaveTypeCombo.AddString(_T("Ensure Macintosh Format (CR)"));
	m_SaveTypeCombo.SetItemData(index, PNSF_Mac);

	m_SaveTypeCombo.SetCurSel(0);
	
	RepositionControls();
	return TRUE;
}

LRESULT CPNSaveDialogEx::OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled)
{
	RepositionControls();
	bHandled = FALSE;
	return FALSE;
}

LRESULT CPNSaveDialogEx::OnComboSelChange(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	// Store the new selection in here...
	m_Format = (EPNSaveFormat)m_SaveTypeCombo.GetItemData( m_SaveTypeCombo.GetCurSel() );

	return TRUE;
}

EPNSaveFormat CPNSaveDialogEx::GetSaveFormat()
{
	return m_Format;
}

void CPNSaveDialogEx::RepositionControls()
{
	RepositionControl( m_SaveTypeCombo, cmb1, true );
	RepositionControl( m_SaveTypeLabel, stc2, false );

	RepositionPlacesBar( m_SaveTypeCombo );
}

void CPNSaveDialogEx::RepositionPlacesBar(CWindow &bottomwnd)
{
	CRect rc, rc2;
	CWindow wndParent = GetParent();
	CWindow wndPB = wndParent.GetDlgItem( ctl1 );
	if( wndPB.IsWindow() )
	{
		wndPB.GetWindowRect( &rc );
		bottomwnd.GetWindowRect( &rc2 );
		wndParent.ScreenToClient( &rc );
		ScreenToClient( &rc2 );
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
void CPNSaveDialogEx::RepositionControl(CWindow &wnd, UINT nID, bool fSize)
{
	// Get the window rect in the client area of the 
	// control we are interested in.
	CWindow wndParent = GetParent();
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
	ScreenToClient( &rectCtrl );
	rectCtrl.OffsetRect( rectAnchor.left - rectCtrl.left, 0 );
	if( fSize )
	{
		rectCtrl.right = rectCtrl.left + rectAnchor.Width();
		dwSWFlags &= ~SWP_NOSIZE;
	}
	wnd.SetWindowPos( NULL, rectCtrl.left, rectCtrl.top,
		rectCtrl.Width(), rectCtrl.Height(), dwSWFlags );
	return;
}

//////////////////////////////////////////////////////////////////////////////
// CPNOpenDialogEx
//////////////////////////////////////////////////////////////////////////////

CPNOpenDialogEx::CPNOpenDialogEx(LPCTSTR szFilter, LPCTSTR szPath) : CPNOpenDialog(szFilter, szPath)
{
	m_Encoding = eUnknown;
	m_ofn.Flags |= OFN_ENABLETEMPLATE;
	m_ofn.lpTemplateName = MAKEINTRESOURCE (IDD_PNOPEN);
}

LRESULT CPNOpenDialogEx::OnInitDialog (UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled)
{
	m_EncodingCombo = GetDlgItem(IDC_PNOPEN_ENCODINGCOMBO);
	m_EncodingLabel = GetDlgItem(IDC_PNOPEN_ENCODINGSTATIC);

	CRect rectDlg;
	GetWindowRect( &rectDlg );

	CRect rectParent;
	::GetClientRect( GetParent(), &rectParent );
	rectDlg.right = rectDlg.left + rectParent.Width();

	// Make sure there is enough room at the bottom for resize
	CRect rectCombo;
	m_EncodingCombo.GetWindowRect( &rectCombo );
	ScreenToClient( &rectCombo );
	int cySize = ::GetSystemMetrics( /*SM_CYSIZE*/ SM_CYSIZEFRAME );
	if (rectDlg.Height() - rectCombo.bottom < cySize)
		rectDlg.bottom = rectDlg.top + rectCombo.bottom + cySize;

	// Reposition
	SetWindowPos( NULL, 0, 0, rectDlg.Width(), rectDlg.Height(),
		SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOZORDER );

	int index = m_EncodingCombo.AddString(_T("Automatic"));
	m_EncodingCombo.SetItemData(index, eUnknown);
	index = m_EncodingCombo.AddString(_T("UTF-8"));
	m_EncodingCombo.SetItemData(index, eUtf8);
	index = m_EncodingCombo.AddString(_T("UTF-16 Big Endian"));
	m_EncodingCombo.SetItemData(index, eUtf16BigEndian);
	index = m_EncodingCombo.AddString(_T("UTF-16 Little Endian"));
	m_EncodingCombo.SetItemData(index, eUtf16LittleEndian);
	index = m_EncodingCombo.AddString(_T("UTF-8 No BOM"));
	m_EncodingCombo.SetItemData(index, eUtf8NoBOM);

	m_EncodingCombo.SetCurSel(0);
	
	RepositionControls();
	return TRUE;
}

LRESULT CPNOpenDialogEx::OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled)
{
	RepositionControls();
	bHandled = FALSE;
	return FALSE;
}

LRESULT CPNOpenDialogEx::OnComboSelChange(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	// Store the new selection in here...
	m_Encoding = (EPNEncoding)m_EncodingCombo.GetItemData( m_EncodingCombo.GetCurSel() );

	return TRUE;
}

EPNEncoding CPNOpenDialogEx::GetEncoding()
{
	return m_Encoding;
}

void CPNOpenDialogEx::RepositionControls()
{
	RepositionControl( m_EncodingCombo, cmb1, true );
	RepositionControl( m_EncodingLabel, stc2, false );

	RepositionPlacesBar( m_EncodingCombo );
}

//////////////////////////////////////////////////////////////////////////////
// CVistaOpenDialogEx
//////////////////////////////////////////////////////////////////////////////

CVistaOpenDialogEx::CVistaOpenDialogEx(LPCWSTR filter) : CVistaOpenDialog(filter)
{
	CComPtr<IFileDialogCustomize> custom;
	if (!SUCCEEDED(GetDialog()->GetPtr()->QueryInterface(&custom)))
	{
		return;
	}

	custom->StartVisualGroup(IDC_PNOPEN_ENCODINGSTATIC, L"Encoding:");
	custom->AddComboBox(IDC_PNOPEN_ENCODINGCOMBO);
	custom->AddControlItem(IDC_PNOPEN_ENCODINGCOMBO, eUnknown, L"Automatic");
	custom->AddControlItem(IDC_PNOPEN_ENCODINGCOMBO, eUtf8, L"UTF-8");
	custom->AddControlItem(IDC_PNOPEN_ENCODINGCOMBO, eUtf16BigEndian, L"UTF-16 Big Endian");
	custom->AddControlItem(IDC_PNOPEN_ENCODINGCOMBO, eUtf16LittleEndian, L"UTF-16 Little Endian");
	custom->AddControlItem(IDC_PNOPEN_ENCODINGCOMBO, eUtf8NoBOM, L"UTF-8 No BOM");
	custom->EndVisualGroup();

	custom->SetSelectedControlItem(IDC_PNOPEN_ENCODINGCOMBO, eUnknown);
}

CVistaOpenDialogEx::~CVistaOpenDialogEx()
{
}

EPNEncoding CVistaOpenDialogEx::GetEncoding()
{
	CComPtr<IFileDialogCustomize> custom;
	if (!SUCCEEDED(GetDialog()->GetPtr()->QueryInterface(&custom)))
	{
		return eUnknown;
	}

	DWORD selectedItem;
	if (SUCCEEDED(custom->GetSelectedControlItem(IDC_PNOPEN_ENCODINGCOMBO, &selectedItem)))
	{
		return (EPNEncoding)selectedItem;
	}

	return eUnknown;
}

//////////////////////////////////////////////////////////////////////////////
// CVistaSaveDialogEx
//////////////////////////////////////////////////////////////////////////////

CVistaSaveDialogEx::CVistaSaveDialogEx(LPCWSTR filter) : CVistaSaveDialog(filter)
{
	CComPtr<IFileDialogCustomize> custom;
	if (!SUCCEEDED(GetDialog()->GetPtr()->QueryInterface(&custom)))
	{
		return;
	}
	
	custom->StartVisualGroup(IDC_PNSAVE_SAVEASSTATIC, L"Format:");
	custom->AddComboBox(IDC_PNSAVE_TYPECOMBO);
	custom->AddControlItem(IDC_PNSAVE_TYPECOMBO, PNSF_NoChange, L"No Change");
	custom->AddControlItem(IDC_PNSAVE_TYPECOMBO, PNSF_Windows, L"Ensure Windows Format (CR+LF)");
	custom->AddControlItem(IDC_PNSAVE_TYPECOMBO, PNSF_Unix, L"Ensure Unix Format (LF)");
	custom->AddControlItem(IDC_PNSAVE_TYPECOMBO, PNSF_Mac, L"Ensure Macintosh Format (CR)");
	custom->EndVisualGroup();

	custom->SetSelectedControlItem(IDC_PNSAVE_TYPECOMBO, PNSF_NoChange);
}

CVistaSaveDialogEx::~CVistaSaveDialogEx()
{
}

EPNSaveFormat CVistaSaveDialogEx::GetSaveFormat()
{
	CComPtr<IFileDialogCustomize> custom;
	if (!SUCCEEDED(GetDialog()->GetPtr()->QueryInterface(&custom)))
	{
		return PNSF_NoChange;
	}

	DWORD selectedItem;
	if (SUCCEEDED(custom->GetSelectedControlItem(IDC_PNSAVE_TYPECOMBO, &selectedItem)))
	{
		return (EPNSaveFormat)selectedItem;
	}

	return PNSF_NoChange;
}