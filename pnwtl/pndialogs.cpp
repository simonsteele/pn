/**
 * @file pndialogs.cpp
 * @brief Assorted Dialogs for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "pndialogs.h"

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

const std::list<tstring>& CPNOpenDialog::GetFiles()
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
// CPNSaveDialog
//////////////////////////////////////////////////////////////////////////////

CPNSaveDialog::CPNSaveDialog(LPCTSTR szFilter) : baseClass(FALSE, _T(".txt"), NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT, szFilter)
{
	m_Format = PNSF_NoChange;
	m_ofn.Flags |= OFN_ENABLETEMPLATE;
	m_ofn.lpTemplateName = MAKEINTRESOURCE (IDD_PNSAVE);
}

LRESULT CPNSaveDialog::OnInitDialog (UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled)
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

	m_SaveTypeCombo.AddString(_T("No change to the file format."));
	m_SaveTypeCombo.AddString(_T("Ensure Windows Format (CR+LF)"));
	m_SaveTypeCombo.AddString(_T("Ensure Unix Format (LF)"));
	m_SaveTypeCombo.AddString(_T("Ensure Macintosh Format (CR)"));

	m_SaveTypeCombo.SetCurSel(0);
	
	RepositionControls();
	return TRUE;
}

LRESULT CPNSaveDialog::OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled)
{
	RepositionControls();
	bHandled = FALSE;
	return FALSE;
}

LRESULT CPNSaveDialog::OnComboSelChange(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	// Store the new selection in here...
	m_Format = (EPNSaveFormat)m_SaveTypeCombo.GetCurSel();

	return TRUE;
}

EPNSaveFormat CPNSaveDialog::GetSaveFormat()
{
	return m_Format;
}

void CPNSaveDialog::RepositionControls()
{
	RepositionControl( m_SaveTypeCombo, cmb1, true );
	RepositionControl( m_SaveTypeLabel, stc2, false );

	RepositionPlacesBar( m_SaveTypeCombo );
}

void CPNSaveDialog::RepositionPlacesBar(CWindow &bottomwnd)
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
void CPNSaveDialog::RepositionControl(CWindow &wnd, UINT nID, bool fSize)
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
// CPNFolderDialog
//////////////////////////////////////////////////////////////////////////////

CPNFolderDialog::CPNFolderDialog(HWND hWndParent, LPCTSTR lpstrInitial, 
								 LPCTSTR lpstrTitle, UINT uFlags)
	: CFolderDialogImpl<CPNFolderDialog>(hWndParent, lpstrTitle, uFlags)
{
	if(lpstrInitial)
		m_csInitialDir = lpstrInitial;
}

void CPNFolderDialog::OnInitialized()
{
	if(m_csInitialDir.GetLength() != 0)
		SetSelection(static_cast<LPCTSTR>(m_csInitialDir));
}

//////////////////////////////////////////////////////////////////////////////
// CGotoDialog
//////////////////////////////////////////////////////////////////////////////

LRESULT CGotoDialog::OK(WORD wID)
{
	CInputDialogImpl<CGotoDialog>::OK(wID);
	lineno = _ttoi(m_inputText);

	return TRUE;
}

int CGotoDialog::GetLineNo()
{
	return lineno;
}