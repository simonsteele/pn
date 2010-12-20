/**
 * @file magicfolderwiz.cpp
 * @brief Wizard to create magic folders
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "project.h"

#include "include/browsetree.h"
//#include "include/wtltreems.h"

#include "include/atlshellext.h"
#include "include/ShellCtrls.h"
#include "include/filefinder.h"
#include "include/filematcher.h"
#include "folderadder.h"

#include "MagicFolderWiz.h"

MagicFolderWizard1::MagicFolderWizard1() : baseClass(_T("Add Magic Folder"))
{
	shelltree = NULL;
	SetHeaderTitle(_T("Add Magic Folder"));
	SetHeaderSubTitle(_T("Select a folder to include as a magic folder..."));
	selFolder = _T("");
}

MagicFolderWizard1::~MagicFolderWizard1()
{
	if(shelltree)
		delete shelltree;
}

LRESULT MagicFolderWizard1::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	shelltree = new CShellTreeCtrl();
	shelltree->SubclassWindow( GetDlgItem(IDC_SHELLTREE) );
	shelltree->SetShellStyle(SCT_EX_FILESYSTEMONLY | SCT_EX_NOFILES);
	shelltree->Populate();
	
	return 0;
}

int MagicFolderWizard1::OnSetActive()
{
	if(selFolder.GetLength() > 0)
	{
		SetWizardButtons( PSWIZB_NEXT );
	}
	else
	{
		SetWizardButtons ( 0 );
	}

    return 1;
}

LRESULT MagicFolderWizard1::OnSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMTREEVIEW pnmtv = (LPNMTREEVIEW)pnmh;

	CString str;
	
	CPidl pidl;
    shelltree->GetItemPidl(pnmtv->itemNew.hItem, &pidl);
	TCHAR path[MAX_PATH+1];
	path[0] = _T('\0');
	if (!::SHGetPathFromIDList(pidl, path))
	{
		selFolder = _T("");
		SetWizardButtons( 0 );
		return 0;
	}
	
	if (path[0] != NULL)
	{
		SetWizardButtons( PSWIZB_NEXT /*| PSWIZB_DISABLEDFINISH*/ );
		selFolder = path;
	}
	else
		selFolder = _T("");
	
	return 0;
}

LPCTSTR MagicFolderWizard1::GetSelFolder() const
{
	return selFolder;
}

MagicFolderWizard2::MagicFolderWizard2() : baseClass(_T("Add Magic Folder"))
{
	SetHeaderTitle(_T("Add Magic Folder"));
	SetHeaderSubTitle(_T("Set up filters..."));
}

LRESULT MagicFolderWizard2::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CEdit fileFilter(GetDlgItem(IDC_MAGICFOLDER_FILEFILTER));
	CEdit excludedFileFilter(GetDlgItem(IDC_MAGICFOLDER_EXCLUDEDFILEFILTER));
	CEdit folderFilter(GetDlgItem(IDC_MAGICFOLDER_FOLDERFILTER));

	fileFilter.SetWindowText(_T("*"));
	excludedFileFilter.SetWindowText(Projects::FolderAdder::getDefaultExcludedFileFilter());
	folderFilter.SetWindowText(Projects::FolderAdder::getDefaultExcludedFolderFilter());

	return 0;
}

int MagicFolderWizard2::OnSetActive()
{
	SetWizardButtons ( PSWIZB_BACK | PSWIZB_FINISH );

	return 1;
}

int MagicFolderWizard2::OnWizardFinish()
{
	CEdit fileFilter(GetDlgItem(IDC_MAGICFOLDER_FILEFILTER));
	CEdit excludedFileFilter(GetDlgItem(IDC_MAGICFOLDER_EXCLUDEDFILEFILTER));
	CEdit folderFilter(GetDlgItem(IDC_MAGICFOLDER_FOLDERFILTER));

	fileFilter.GetWindowText(strFileFilter);
	excludedFileFilter.GetWindowText(strExcludedFileFilter);
	folderFilter.GetWindowText(strFolderFilter);

	return 1;
}

LPCTSTR MagicFolderWizard2::GetFileFilter() const
{
	return strFileFilter;
}

LPCTSTR MagicFolderWizard2::GetExcludedFileFiler() const {
	return strExcludedFileFilter;
}

LPCTSTR MagicFolderWizard2::GetFolderFilter() const
{
	return strFolderFilter;
}