/**
 * @file optionspages.cpp
 * @brief Options Dialog General Page
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "OptionsPageGeneral.h"

//////////////////////////////////////////////////////////////////////////////
// COptionsPageGeneral
//////////////////////////////////////////////////////////////////////////////

tstring COptionsPageGeneral::GetTreePosition()
{
	return L10N::StringLoader::Get(IDS_OPTGROUP_GENERAL);
}

void COptionsPageGeneral::OnOK()
{
	if(!m_bCreated)
		return;

	DoDataExchange(TRUE);

	Options& options = *OPTIONS;
	options.SetCached(Options::OMaximiseNew, m_bMaximise != FALSE);
	options.SetCached(Options::OShowFullPath, m_bFullPath != FALSE);
	options.SetCached(Options::OManageTabOrder, m_bManageTabOrder);

	// Ensure MRU size <= 50 && >= 1
	m_iMRUSize = ( m_iMRUSize > 50 ? 50 : ( m_iMRUSize < 1 ? 1 : m_iMRUSize ) );
	options.Set(PNSK_INTERFACE, _T("MRUSize"), (int)m_iMRUSize);
	options.Set(PNSK_INTERFACE, _T("AllowMultiInstance"), (bool)(m_bMultiInstanceOk != FALSE));
	options.Set(PNSK_INTERFACE, _T("NewOnStart"), (bool)(m_bNewOnStart != FALSE));

	options.Set(PNSK_INTERFACE, _T("Tabs"), (bool)(m_bShowTabs != FALSE));
	options.Set(PNSK_INTERFACE, _T("TabsOnBottom"), (bool)(m_bTabsOnBottom != FALSE));
	options.Set(PNSK_INTERFACE, _T("MaximizedTabsOnly"), (bool)(m_bTabsOnlyMax != FALSE));
	options.Set(PNSK_INTERFACE, _T("SaveWorkspace"), (bool)(m_bSaveWorkspace != FALSE));

	//options.Set(PNSK_INTERFACE, _T("HideSingleTab"), (bool)(m_bHideSingleTab != FALSE));

	options.Set(PNSK_GENERAL, _T("CheckForUpdates"), m_bCheckForUpdates != FALSE);
	options.Set(PNSK_GENERAL, _T("CheckForUnstableUpdates"), m_bWantTestingUpdates != FALSE);

	options.Set(PNSK_GENERAL, _T("BackupOnSave"), m_bBackupOnSave != FALSE);

	options.Set(PNSK_GENERAL, _T("EditReadOnly"), m_bEditReadOnly != FALSE);
}

void COptionsPageGeneral::OnInitialise()
{
	m_bMaximise = OPTIONS->GetCached(Options::OMaximiseNew);
	m_bFullPath = OPTIONS->GetCached(Options::OShowFullPath);
	m_bManageTabOrder = OPTIONS->GetCached(Options::OManageTabOrder);

	// Interface settings
	OPTIONS->BeginGroupOperation(PNSK_INTERFACE);

	m_iMRUSize = OPTIONS->Get(NULL, _T("MRUSize"), 4);
	m_bMultiInstanceOk = OPTIONS->Get(NULL, _T("AllowMultiInstance"), false);
	m_bNewOnStart = OPTIONS->Get(NULL, _T("NewOnStart"), true);

    m_bShowTabs = OPTIONS->Get(NULL, _T("Tabs"), true);
	m_bTabsOnBottom = OPTIONS->Get(NULL, _T("TabsOnBottom"), false);
	m_bTabsOnlyMax = OPTIONS->Get(NULL, _T("MaximizedTabsOnly"), false);
	m_bSaveWorkspace = OPTIONS->Get(NULL, _T("SaveWorkspace"), false);

	//m_bHideSingleTab = OPTIONS->Get(PNSK_INTERFACE, _T("HideSingleTab"), false);

	OPTIONS->EndGroupOperation();

	m_bCheckForUpdates = OPTIONS->Get(PNSK_GENERAL, _T("CheckForUpdates"), true);
	m_bWantTestingUpdates = OPTIONS->Get(PNSK_GENERAL, _T("CheckForUnstableUpdates"), false);

	m_bBackupOnSave = OPTIONS->Get(PNSK_GENERAL, _T("BackupOnSave"), false);

	m_bEditReadOnly = OPTIONS->Get(PNSK_GENERAL, _T("EditReadOnly"), false);

	DoDataExchange();

	enableControls();
}

LRESULT COptionsPageGeneral::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	// Set up header controls:
	m_settingsHeader.SubclassWindow(GetDlgItem(IDC_SETTINGS_STATIC));
	m_tabsHeader.SubclassWindow(GetDlgItem(IDC_TABS_STATIC));

	return 0;
}

LRESULT COptionsPageGeneral::OnOptionClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	enableControls();

	return 0;
}

void COptionsPageGeneral::enableControls()
{
	DoDataExchange(TRUE);

	GetDlgItem(IDC_OPT_MAXTABSONLY).EnableWindow(m_bShowTabs);
	GetDlgItem(IDC_OPT_TABSBOTTOMCHECK).EnableWindow(m_bShowTabs);
	GetDlgItem(IDC_OPT_WANTTESTINGUPGRADES).EnableWindow(m_bCheckForUpdates);
}