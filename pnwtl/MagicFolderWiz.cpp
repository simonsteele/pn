/**
 * @file magicfolderwiz.cpp
 * @brief Wizard to create magic folders
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele <s.steele@pnotepad.org>
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
	
	return 0;
}

int MagicFolderWizard1::OnSetActive()
{
    SetWizardButtons ( 0 );

    return 1;
}

LRESULT MagicFolderWizard1::OnSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	

	
	TCHAR buf[MAX_PATH+1];
	memset(buf, 0, (MAX_PATH+1)*sizeof(TCHAR));
	//shelltree->GetItemPath(pnmtv->itemNew.hItem, buf);
	if(_tcslen(buf) > 0)
	{
		SetWizardButtons( PSWIZB_NEXT | PSWIZB_DISABLEDFINISH );
	}
	
	return 0;
}