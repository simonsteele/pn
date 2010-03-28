/**
 * @file optionspageextensions.cpp
 * @brief Options Dialog Extensions Page for Programmer's Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2007-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "optionspageextensions.h"
#include "extapp.h"
#include "extension.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

COptionsPageExtensions::COptionsPageExtensions()
{
	m_extensionsLink.SetTarget(L"http://www.pnotepad.org/add-ons/");
}

void COptionsPageExtensions::OnOK()
{
}

void COptionsPageExtensions::OnInitialise()
{
	int count = 0;

	for(App::ExtensionList::iterator i = g_Context.ExtApp->GetExtensions().begin();
		i != g_Context.ExtApp->GetExtensions().end();
		++i)
	{
		PN::AString name;
		PN::AString version;

		std::string unknown(LSA(IDS_EXTENSIONS_UNKNOWN));
		
		if(!(*i)->GetDetails(name, version))
		{
			name = unknown.c_str();
			version = unknown.c_str();
		}
		
		CA2CT nameconv(name.Get());
		CA2CT versionconv(version.Get());

		int index = m_list.InsertItem(count++, nameconv);
		m_list.SetItemText(index, 1, versionconv);
		m_list.SetItemText(index, 2, LS(IDS_EXTENSIONS_ENABLED));
	}
}

tstring COptionsPageExtensions::GetTreePosition()
{
	return L10N::StringLoader::Get(IDS_OPTGROUP_EXTENSIONS);
}

void COptionsPageExtensions::OnCancel()
{

}

LRESULT COptionsPageExtensions::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	// Header:
	m_settingsHeader.SubclassWindow(GetDlgItem(IDC_SETTINGS_STATIC));

	// Link:
	m_extensionsLink.SubclassWindow(GetDlgItem(IDC_EXTLINK));

	// Controls:
	m_list.Attach(GetDlgItem(IDC_EXTENSIONSLIST));
	m_list.InsertColumn(0, LS(IDS_HDR_EXTENSIONS_NAME), LVCFMT_LEFT, 140, 0);
	m_list.InsertColumn(1, LS(IDS_HDR_EXTENSIONS_VERSION), LVCFMT_LEFT, 80, 0);
	m_list.InsertColumn(2, LS(IDS_HDR_EXTENSIONS_ENABLED), LVCFMT_LEFT, 80, 0);
	m_list.SetExtendedListViewStyle(LVS_EX_FULLROWSELECT, LVS_EX_FULLROWSELECT);

	return 0;
}