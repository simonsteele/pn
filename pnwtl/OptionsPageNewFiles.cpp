/**
 * @file optionspageautocomplete.cpp
 * @brief Options Dialog Autocomplete Page for Programmer's Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2009-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "pnstrings.h"
#include "OptionsPages.h"
#include "OptionsDialogs.h"

//////////////////////////////////////////////////////////////////////////////
// COptionsPageNewFiles
//////////////////////////////////////////////////////////////////////////////

#include "smartstart.h"

COptionsPageNewFiles::COptionsPageNewFiles(SchemeConfigParser* pSchemes)
{
	m_pSchemes = pSchemes;
	m_bDirty = false;
}

void COptionsPageNewFiles::AddItem(LPCSTR key, LPCSTR schemename)
{
	LVITEM lvi;

	Scheme* pScheme = SchemeManager::GetInstance()->SchemeByName(schemename);
	if(pScheme)
	{
		CA2CT keyconv(key);

		char* nameStore = new char[strlen(schemename)+1];
		strcpy(nameStore, schemename);

		lvi.mask = LVIF_IMAGE | LVIF_TEXT | LVIF_PARAM;
		lvi.iItem = m_list.GetItemCount();
		lvi.iSubItem = 0;
		lvi.pszText = const_cast<LPTSTR>((LPCTSTR)keyconv);
		lvi.iImage = 0;
		lvi.lParam = reinterpret_cast<LPARAM>(nameStore);

		int iItem = m_list.InsertItem(&lvi);

		lvi.mask = LVIF_TEXT;
		lvi.iItem = iItem;
		lvi.iSubItem = 1;
		lvi.pszText = const_cast<LPTSTR>( pScheme->GetTitle() );
		lvi.lParam = 0;

		m_list.SetItem(&lvi);
	}
}

/**
 * Manage the enabled state of the controls.
 */
void COptionsPageNewFiles::EnableButtons()
{
	bool bSS = m_ssCheck.GetCheck() == BST_CHECKED;
	int iSI = m_list.GetSelectedIndex();
	m_list.EnableWindow(bSS);
	::EnableWindow(GetDlgItem(IDC_SMARTSTART_ADDBUTTON), bSS);
	::EnableWindow(GetDlgItem(IDC_SMARTSTART_EDITBUTTON), bSS && (iSI != -1));
	::EnableWindow(GetDlgItem(IDC_SMARTSTART_REMOVEBUTTON), bSS && (iSI != -1));
}

void COptionsPageNewFiles::FreeResources()
{
	int count = m_list.GetItemCount();
	for(int i = 0; i < count; i++)
	{
		char* pNameStored = reinterpret_cast<char*>( m_list.GetItemData(i) );
		if(pNameStored)
			delete [] pNameStored;
		m_list.SetItemData(i, NULL);
	}
}

void COptionsPageNewFiles::OnInitialise()
{
	tstring strNewScheme = OPTIONS->Get(PNSK_EDITOR, _T("NewScheme"), LS(IDS_DEFAULTSCHEME));

	// Populate and initialise schemes combo.
	CT2CA newScheme(strNewScheme.c_str());
	m_combo.Load(m_pSchemes, newScheme);

	// Populate SmartStart list.
	string_map& smap = SmartStart::GetInstance()->GetMap();
	
	for(string_map::const_iterator j = smap.begin(); j != smap.end(); ++j)
	{
		AddItem((*j).first.c_str(), (*j).second.c_str());
	}

	m_ssCheck.SetCheck( 
		OPTIONS->Get(PNSK_EDITOR, _T("SmartStart"), true) ? BST_CHECKED : BST_UNCHECKED
	);
	
	EnableButtons();
}

void COptionsPageNewFiles::OnOK()
{
	if(m_bCreated)
	{
		if(m_bDirty)
		{
			// Copy all items into smartstart manager...
			SmartStart* pSS = SmartStart::GetInstance();
			string_map& smap = pSS->GetMap();
			smap.clear();

			CString strBuf;

			int count = m_list.GetItemCount();
			for(int i = 0; i < count; i++)
			{
				m_list.GetItemText(i, 0, strBuf);
				
				CT2CA phrase(strBuf);

				char* pStoredName = reinterpret_cast<char*>(m_list.GetItemData(i));
				smap.insert(string_map::value_type(std::string(phrase), std::string(pStoredName)));
			}

			pSS->Save();

			// Set the default new-scheme.
			int selIndex = m_combo.GetCurSel();
			std::string wt;
			SchemeDetails* pS = m_combo.GetItemScheme(selIndex);
			
			if(pS)
				wt = pS->Name;
			else
				wt = LSAS(IDS_DEFAULTSCHEME);
			
			CA2CT newScheme(wt.c_str());
			OPTIONS->Set(PNSK_EDITOR, _T("NewScheme"), newScheme);

			// Enable SmartStart?
			CButton button(GetDlgItem(IDC_SMARTSTART_ENABLECHECK));
			OPTIONS->Set(PNSK_EDITOR, _T("SmartStart"), button.GetCheck() == BST_CHECKED);
		}

		FreeResources();
	}
}

void COptionsPageNewFiles::OnCancel()
{
	if(m_bCreated)
		FreeResources();
}

tstring COptionsPageNewFiles::GetTreePosition()
{
	return MAKE_OPTIONSTREEPATH(IDS_OPTGROUP_FILES, IDS_OPTPAGE_NEWFILES);
}

LRESULT COptionsPageNewFiles::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	// Headers:
	m_settingsHeader.SubclassWindow(GetDlgItem(IDC_SETTINGS_STATIC));
	m_smartStartHeader.SubclassWindow(GetDlgItem(IDC_SMARTSTART_STATIC));

	// Controls:
	m_list.Attach(GetDlgItem(IDC_SMARTSTART_LIST));
	CRect rc;
	m_list.SetExtendedListViewStyle(LVS_EX_FULLROWSELECT);
	m_list.GetClientRect(rc);
	m_list.InsertColumn(0, LS(IDS_HDR_SMARTSTART_PHRASE), LVCFMT_LEFT, (rc.Width() / 3) * 2, 0);
	m_list.InsertColumn(1, LS(IDS_HDR_FILETYPES_SCHEME), LVCFMT_LEFT, (rc.Width() / 3) - 20, 0);

	m_combo.Attach(GetDlgItem(IDC_NEW_SCHEMECOMBO));

	m_ssCheck.Attach(GetDlgItem(IDC_SMARTSTART_ENABLECHECK));

	return 0;
}

LRESULT COptionsPageNewFiles::OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CSmartStartEditorDialog edit(m_pSchemes);
	
	edit.SetValues("", "");

	if(edit.DoModal() == IDOK)
	{
		std::string startPhrase;
		std::string schemeName;
		edit.GetValues(startPhrase, schemeName);
		AddItem(startPhrase.c_str(), schemeName.c_str());
		m_bDirty = true;
	}

	return 0;
}

LRESULT COptionsPageNewFiles::OnEditClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int iSelIndex = m_list.GetSelectedIndex();
	if(iSelIndex != -1)
	{
		CString strBuf;
		m_list.GetItemText(iSelIndex, 0, strBuf);
		char* pStoredData = reinterpret_cast<char*>( m_list.GetItemData(iSelIndex) );
		if(pStoredData && strBuf.GetLength() > 0)
		{
			CSmartStartEditorDialog edit(m_pSchemes);

			CT2CA phrase(strBuf);
			edit.SetValues(phrase, pStoredData);

			if(edit.DoModal() == IDOK)
			{
				std::string startPhrase;
				std::string schemeName;
				edit.GetValues(startPhrase, schemeName);
				CA2CT startPhraseConv(startPhrase.c_str());
				m_list.SetItemText(iSelIndex, 0, startPhraseConv);
				
				Scheme* pScheme = SchemeManager::GetInstance()->SchemeByName(schemeName.c_str());
				if(pScheme)
					m_list.SetItemText(iSelIndex, 1, pScheme->GetTitle());
				
				delete [] pStoredData;
				pStoredData = new char[schemeName.length()+1];
				strcpy(pStoredData, schemeName.c_str());
				m_list.SetItemData(iSelIndex, reinterpret_cast<DWORD_PTR>(pStoredData));
				m_bDirty = true;
			}
		}
	}
	return 0;
}

LRESULT COptionsPageNewFiles::OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int iSelIndex = m_list.GetSelectedIndex();
	if(iSelIndex != -1)
	{
		char* pStoredName = reinterpret_cast<char*>( m_list.GetItemData(iSelIndex) );
		if(pStoredName)
		{
			delete [] pStoredName;
			m_list.DeleteItem(iSelIndex);
		}
		m_bDirty = true;
	}
	return 0;
}

LRESULT COptionsPageNewFiles::OnComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_bDirty = true;
	return 0;
}

LRESULT COptionsPageNewFiles::OnEnabledChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_bDirty = true;
	EnableButtons();
	return 0;
}

LRESULT COptionsPageNewFiles::OnListKeyDown(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	EnableButtons();

	return 0;
}

LRESULT COptionsPageNewFiles::OnListClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	EnableButtons();

	return 0;
}

LRESULT COptionsPageNewFiles::OnListDblClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	BOOL b;
	OnEditClicked(0, 0, 0, b);

	return 0;
}