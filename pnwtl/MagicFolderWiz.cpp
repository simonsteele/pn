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

#include "include/browsetree.h"
#include "include/wtltreems.h"

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
	shelltree = new CBrowseTree();
	shelltree->SubclassWindow( GetDlgItem(IDC_SHELLTREE) );
	shelltree->SetupTree();
	shelltree->ShowFiles(FALSE);
	
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
	CString str;
	shelltree->GetSelectedPath(str);
	if(str.GetLength() > 0)
	{
		SetWizardButtons( PSWIZB_NEXT /*| PSWIZB_DISABLEDFINISH*/ );
		selFolder = str;
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
	CEdit folderFilter(GetDlgItem(IDC_MAGICFOLDER_FOLDERFILTER));

	fileFilter.SetWindowText("*");
	folderFilter.SetWindowText("CVS;.svn");

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
	CEdit folderFilter(GetDlgItem(IDC_MAGICFOLDER_FOLDERFILTER));

	fileFilter.GetWindowText(strFileFilter);
	folderFilter.GetWindowText(strFolderFilter);

	return 1;
}

LPCTSTR MagicFolderWizard2::GetFileFilter() const
{
	return strFileFilter;
}

LPCTSTR MagicFolderWizard2::GetFolderFilter() const
{
	return strFolderFilter;
}