/**
 * @file OptionsPageGeneral.h
 * @brief Options Dialog General Page
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef PNOTEPAD_OPTIONSPAGEGENERAL_H__INCLUDED
#define PNOTEPAD_OPTIONSPAGEGENERAL_H__INCLUDED

#include "include/optionsdialog.h"
#include "controls/OptionsBlockHeader.h"

class COptionsPageGeneral : public COptionsPageImpl<COptionsPageGeneral>,
							public CWinDataExchange<COptionsPageGeneral>
{
	public:
		BEGIN_MSG_MAP(COptionsPageGeneral)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_HANDLER(IDC_OPT_SHOWTABSCHECK, BN_CLICKED, OnOptionClicked)
			COMMAND_HANDLER(IDC_OPT_CHECKUPDATES, BN_CLICKED, OnOptionClicked)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()
		enum { IDD = IDD_PAGE_GENERAL };

		BEGIN_DDX_MAP(COptionsPageGeneral)
			DDX_UINT(IDC_OPT_MRUCOUNT,				m_iMRUSize)
			DDX_CHECK(IDC_OPT_MAXCHECK,				m_bMaximise)
			DDX_CHECK(IDC_OPT_FULLPATHCHECK,		m_bFullPath)
			DDX_CHECK(IDC_OPT_NEWFILEONSTART,		m_bNewOnStart)
			DDX_CHECK(IDC_MULTIINSTANCECHECK,		m_bMultiInstanceOk)
			DDX_CHECK(IDC_OPT_SHOWTABSCHECK,		m_bShowTabs)
			DDX_CHECK(IDC_OPT_TABSBOTTOMCHECK,		m_bTabsOnBottom)
			DDX_CHECK(IDC_OPT_MAXTABSONLY,			m_bTabsOnlyMax)
			DDX_CHECK(IDC_OPT_TABORDERCHECK,		m_bManageTabOrder)
			DDX_CHECK(IDC_OPT_SAVEWORKSPACE,		m_bSaveWorkspace)
			DDX_CHECK(IDC_OPT_CHECKUPDATES,			m_bCheckForUpdates)
			DDX_CHECK(IDC_OPT_WANTTESTINGUPGRADES,	m_bWantTestingUpdates)
			DDX_CHECK(IDC_OPT_BACKUPCHECK,			m_bBackupOnSave)
			DDX_CHECK(IDC_OPT_EDITREADONLY,			m_bEditReadOnly)
		END_DDX_MAP()

		virtual void OnOK();
		virtual void OnInitialise();
		virtual tstring GetTreePosition();

	private:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnOptionClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		void enableControls();
		
		BOOL			m_bNewOnStart;
		BOOL			m_bMaximise;
		BOOL			m_bFullPath;
		BOOL			m_bMultiInstanceOk;
		UINT			m_iMRUSize;
		BOOL			m_bShowTabs;
		BOOL			m_bTabsOnBottom;
		BOOL			m_bTabsOnlyMax;
		BOOL			m_bManageTabOrder;
		BOOL			m_bSaveWorkspace;
		BOOL			m_bCheckForUpdates;
		BOOL			m_bWantTestingUpdates;
		BOOL			m_bBackupOnSave;
		BOOL			m_bEditReadOnly;
		
		COptionsBlockHeader m_tabsHeader;
		COptionsBlockHeader m_settingsHeader;
};

#endif // #ifndef PNOTEPAD_OPTIONSPAGEGENERAL_H__INCLUDED