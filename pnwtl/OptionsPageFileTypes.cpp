/**
 * @file OptionsPageFileTypes.cpp
 * @brief "File" Options Page for Programmer's Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-20010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "OptionsPages.h"
#include "OptionsDialogs.h"

//////////////////////////////////////////////////////////////////////////////
// COptionsPageFileTypes
//////////////////////////////////////////////////////////////////////////////

typedef struct tagFTDetails
{
	bool isFilename;
	Scheme* pScheme;
	tstring ext;
} SFTDetails;

int CALLBACK FileTypeAlphaAscendCompare(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
{
	SFTDetails* p1 = reinterpret_cast<SFTDetails*>(lParam1);
	SFTDetails* p2 = reinterpret_cast<SFTDetails*>(lParam2);

	return (p1->ext < p2->ext) ? -1 : 1;
}

COptionsPageFileTypes::COptionsPageFileTypes(SchemeConfigParser* schemes)
{
	m_schemes = schemes;
	m_bDirty = false;
}

COptionsPageFileTypes::~COptionsPageFileTypes()
{
}

void COptionsPageFileTypes::addItem(int n, LPCTSTR ext, Scheme* pScheme, bool isFilename)
{
	LVITEM lvi;
	lvi.mask = LVIF_TEXT;
	
	if(n == -1)
	{
		n = m_list.GetItemCount();
	}

	int index = m_list.InsertItem(n, ext);
	
	lvi.iItem = index;
	lvi.iSubItem = 1;
	lvi.pszText = const_cast<LPTSTR>( pScheme->GetTitle() );
	m_list.SetItem(&lvi);

	SFTDetails* pDetails = new SFTDetails;
	pDetails->isFilename = isFilename;
	pDetails->pScheme = pScheme;
	pDetails->ext = ext;

	m_list.SetItemData(index, (DWORD_PTR)pDetails);
}

void COptionsPageFileTypes::OnInitialise()
{
	m_pExtMap = SchemeManager::GetInstance()->GetExtensionMap();
	m_pFilenameMap = SchemeManager::GetInstance()->GetFilenameMap();

	int n = 0;
	for(SCHEME_MAP::const_iterator i = m_pExtMap->begin(); i != m_pExtMap->end(); ++i)
	{
		addItem(n++, (*i).first.c_str(), (*i).second, false);
	}

	for(SCHEME_MAP::const_iterator j = m_pFilenameMap->begin(); j != m_pFilenameMap->end(); ++j)
	{
		addItem(n++, (*j).first.c_str(), (*j).second, true);
	}

	m_bStripTrailingOnSave = OPTIONS->Get(PNSK_EDITOR, _T("StripTrailingBeforeSave"), false);
	m_bEnsureBlankLine = OPTIONS->Get(PNSK_EDITOR, _T("EnsureBlankFinalLine"), false);

	DoDataExchange();
}

void COptionsPageFileTypes::OnOK()
{
	if(!m_bCreated)
	{
		return;
	}

	// General options first:
	DoDataExchange(true);

	OPTIONS->Set(PNSK_EDITOR, _T("StripTrailingBeforeSave"), m_bStripTrailingOnSave);
	OPTIONS->Set(PNSK_EDITOR, _T("EnsureBlankFinalLine"), m_bEnsureBlankLine);

	// Dirty set if the list is edited:
	if(!m_bDirty)
	{
		clear();
		return;
	}

	m_pExtMap->clear();
	m_pFilenameMap->clear();

	SchemeManager* pSM = SchemeManager::GetInstance();

	for(int i = 0; i < m_list.GetItemCount(); i++)
	{
		CString sExt;
		m_list.GetItemText(i, 0, sExt);
		
		SFTDetails* pDetails = reinterpret_cast<SFTDetails*>(m_list.GetItemData(i));

		if(!pDetails->isFilename)
		{
			m_pExtMap->insert(SCHEME_MAP::value_type(tstring(sExt), pDetails->pScheme));
		}
		else
		{
			m_pFilenameMap->insert(SCHEME_MAP::value_type(tstring(sExt), pDetails->pScheme));
		}

		delete pDetails;
	}

	pSM->SaveExtMap();
}

void COptionsPageFileTypes::OnCancel()
{
	if(!m_bCreated)
	{
		return;
	}
	
	clear();
}

tstring COptionsPageFileTypes::GetTreePosition()
{
	return L10N::StringLoader::Get(IDS_OPTGROUP_FILES);
}

LRESULT COptionsPageFileTypes::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	// Header:
	m_settingsHeader.SubclassWindow(GetDlgItem(IDC_SETTINGS_STATIC));
	m_optionsHeader.SubclassWindow(GetDlgItem(IDC_SETTINGS_STATIC2));

	// Controls:
	m_list.Attach(GetDlgItem(IDC_LIST));

	m_list.SetExtendedListViewStyle( LVS_EX_FULLROWSELECT, LVS_EX_FULLROWSELECT );

	m_list.InsertColumn(0, LS(IDS_HDR_FILETYPES_MATCH), LVCFMT_LEFT, 120, -1);
	m_list.InsertColumn(1, LS(IDS_HDR_FILETYPES_SCHEME), LVCFMT_LEFT, 200, -1);

	DoDataExchange(false);

	return 0;
}

LRESULT COptionsPageFileTypes::OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CFileTypeEditorDialog dlg(m_schemes);
	if (dlg.DoModal(m_hWnd) == IDOK)
	{
		tstring fn;
		std::string scheme;
		dlg.GetValues(fn, scheme);
		
		Scheme* pScheme;
		pScheme = SchemeManager::GetInstance()->SchemeByName(scheme.c_str());

		if(fn[0] == _T('.'))
		{
			addItem(-1, fn.c_str(), pScheme, false);
		}
		else
		{
			addItem(-1, fn.c_str(), pScheme, true);
		}

		m_bDirty = true;
	}
	return 0;
}

LRESULT COptionsPageFileTypes::OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int i = m_list.GetSelectedIndex();
	if(i == -1)
	{
		return 0;
	}

	SFTDetails* pDetails = reinterpret_cast<SFTDetails*>( m_list.GetItemData( i ) );

	delete pDetails;

    m_list.DeleteItem(i);

	m_bDirty = true;

	return 0;
}

LRESULT COptionsPageFileTypes::OnEditClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int i = m_list.GetSelectedIndex();
	if(i == -1)
	{
		return 0;
	}

	CString ext;
	m_list.GetItemText(i, 0, ext);

	SFTDetails* pDetails = reinterpret_cast<SFTDetails*>(m_list.GetItemData(i));

	CFileTypeEditorDialog dlg(m_schemes);
	dlg.SetValues(ext, pDetails->pScheme->GetName());

	if (dlg.DoModal(m_hWnd) == IDOK)
	{
		tstring match;
		std::string scheme;
		dlg.GetValues(match, scheme);
		
		Scheme* pScheme;
		pScheme = SchemeManager::GetInstance()->SchemeByName(scheme.c_str());

		m_list.SetItemText(i, 0, match.c_str());
		m_list.SetItemText(i, 1, pScheme->GetTitle());
		pDetails->pScheme = pScheme;
		pDetails->isFilename = match[0] != _T('.');
		pDetails->ext = match;

		m_list.SortItems(&FileTypeAlphaAscendCompare, NULL);

		m_bDirty = true;
	}

	return 0;
}


LRESULT COptionsPageFileTypes::OnListDblClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	BOOL b;
	OnEditClicked(0, 0, 0, b);
	return 0;
}

void COptionsPageFileTypes::clear()
{
	for(int i = 0; i < m_list.GetItemCount(); i++)
	{
		SFTDetails* pDetails = reinterpret_cast<SFTDetails*>(m_list.GetItemData(i));
		delete pDetails;
	}
}