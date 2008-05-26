/**
 * @file pndialogs.cpp
 * @brief Assorted Dialogs for Programmer's Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "pndialogs.h"

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