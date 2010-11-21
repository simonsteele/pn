/**
 * @file OptionsPageGlobalStyles.cpp
 * @brief Options Dialog Global Styles Page for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2007-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "OptionsDialogs.h"
#include "OptionsPageGlobalStyles.h"
#include "SchemeConfig.h"
#include "pndialogs.h"
#include "include/filefinder.h"

COptionsPageGlobalStyles::COptionsPageGlobalStyles(SchemeConfigParser* pSchemes) : m_pSchemes(pSchemes)
{
	m_dirty = false;
	m_pStyle = NULL;
}

void COptionsPageGlobalStyles::OnOK()
{
}

void COptionsPageGlobalStyles::OnInitialise()
{
	StylePtrMap& classes = m_pSchemes->GetClasses();

	int items(0);

	for(StylePtrMap::const_iterator i = classes.begin();
		i != classes.end();
		++i)
	{
		NamedStyleDetails* style = static_cast<NamedStyleDetails*>((*i).second.get());
		if(style->FriendlyName.size())
		{
			int item = m_list.AddItem(items++, 0, style->FriendlyName.c_str());
			m_list.SetItemData(item, reinterpret_cast<DWORD_PTR>(style));
		}
	}

	// Find Preset Files
	tstring path;
	OPTIONS->GetPNPath(path, PNPATH_PRESETS);
	FileFinder<COptionsPageGlobalStyles> finder(this, &COptionsPageGlobalStyles::OnPresetFound);
	finder.Find(path.c_str(), _T("*.xml"), false, false);
	
	if(m_presets.GetCount() > 0)
	{
		m_presets.SetCurSel(0);
	}
}

tstring COptionsPageGlobalStyles::GetTreePosition()
{
	return MAKE_OPTIONSTREEPATH(IDS_OPTGROUP_STYLES, IDS_OPTPAGE_STYLES);
}

void COptionsPageGlobalStyles::OnCancel()
{
}

bool COptionsPageGlobalStyles::IsDirty()
{
	return m_dirty;
}

LRESULT COptionsPageGlobalStyles::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	// Headers:
	m_settingsHeader.SubclassWindow(GetDlgItem(IDC_SETTINGS_STATIC));
	m_presetsHeader.SubclassWindow(GetDlgItem(IDC_PRESETS_STATIC));

	// Style Controls:
	CRect rc;
	
	m_list.Attach(GetDlgItem(IDC_STYLES_LIST));
	m_list.SetExtendedListViewStyle(LVS_EX_FULLROWSELECT, LVS_EX_FULLROWSELECT);
	m_list.AddColumn(_T("Styles"), 0);
	m_list.GetClientRect(rc);
	m_list.SetColumnWidth(0, rc.Width() - ::GetSystemMetrics(SM_CXVSCROLL));

	CWindow placeholder(GetDlgItem(IDC_STYLE_EXAMPLE));
	placeholder.GetWindowRect(rc);
	ScreenToClient(rc);
	m_sd.Create(m_hWnd, rc, _T("Style Display"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS);

	m_FontCombo.SubclassWindow(GetDlgItem(IDC_STYLE_FONTCOMBO));
	m_SizeCombo.Attach(GetDlgItem(IDC_STYLE_SIZECOMBO));

	m_fore.SubclassWindow(GetDlgItem(IDC_STYLE_FOREBUTTON));
	m_back.SubclassWindow(GetDlgItem(IDC_STYLE_BACKBUTTON));

	m_fore.SetDefaultColor(RGB(0,0,0));
	m_back.SetDefaultColor(RGB(255,255,255));
	
	m_SizeCombo.Add(6);
	m_SizeCombo.Add(8);
	m_SizeCombo.Add(10);
	m_SizeCombo.Add(12);
	m_SizeCombo.Add(14);
	m_SizeCombo.Add(16);
	m_SizeCombo.Add(18);

	m_bold.Attach(GetDlgItem(IDC_STYLE_BOLDCHECK));
	m_italic.Attach(GetDlgItem(IDC_STYLE_ITALICCHECK));
	m_underline.Attach(GetDlgItem(IDC_STYLE_UNDERLINECHECK));
	m_eolfilled.Attach(GetDlgItem(IDC_STYLE_EOLFILLEDCHECK));

	m_presets.Attach(GetDlgItem(IDC_STYLE_PRESETCOMBO));

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnForeChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	NMCOLORBUTTON* pN = reinterpret_cast<NMCOLORBUTTON*>(pnmh);
	COLORREF col = (pN->clr == CLR_DEFAULT ? m_fore.GetDefaultColor() : pN->clr);
	m_style.ForeColor = col;
	onChange();
	return 0;
}

LRESULT COptionsPageGlobalStyles::OnBackChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	NMCOLORBUTTON* pN = reinterpret_cast<NMCOLORBUTTON*>(pnmh);
	COLORREF col = (pN->clr == CLR_DEFAULT ? m_fore.GetDefaultColor() : pN->clr);
	m_style.BackColor = col;
	onChange();
	return 0;
}

LRESULT COptionsPageGlobalStyles::OnFontChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		m_style.FontName = m_FontCombo.GetSelFontName();
		onChange();
	}

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnSizeChanged(WORD wNotifyCode, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		int i = m_SizeCombo.GetSelection((wNotifyCode != CBN_SELCHANGE));//GetDlgItemInt(IDC_STYLE_SIZECOMBO);
		m_style.FontSize = i;
		onChange();
	}

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnBoldClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		bool bC = m_bold.GetCheck() == BST_CHECKED;
		m_style.Bold = bC;
		onChange();
	}

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnItalicClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		bool bC = m_italic.GetCheck() == BST_CHECKED;
		m_style.Italic = bC;
		onChange();
	}

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnUnderlineClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		bool bC = m_underline.GetCheck() == BST_CHECKED;
		m_style.Underline = bC;
		onChange();
	}

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnEOLFilledClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		bool bC = m_eolfilled.GetCheck() == BST_CHECKED;
		m_style.EOLFilled = bC;
		onChange();
	}

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnResetClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		m_pStyle->Reset();
		m_pStyle = NULL;
	}

	m_dirty = true;
	updateSel();

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnResetAllClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_pSchemes->ResetClasses();

	m_dirty = true;
	
	// Re-select whatever we're looking at...
	m_pStyle = NULL;
	updateSel();

	::SendMessage(GetParent(), PN_NOTIFY, 0, PN_UPDATEDISPLAY);

	return 0;
}

/**
 * List selection has changed, save changes if necessary
 */
LRESULT COptionsPageGlobalStyles::OnListSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	updateSel();

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnLoadPresetClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CWindowText wt(m_presets.m_hWnd);
	if((LPCTSTR)wt != NULL)
	{
		loadPreset((LPCTSTR)wt);
	}

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnSavePresetClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	savePreset();

	return 0;
}

void COptionsPageGlobalStyles::OnPresetFound(const wchar_t* path, FileFinderData& file, bool& /*shouldContinue*/)
{
	CFileName fn(file.GetFilename());
	m_presets.AddString(fn.GetFileName_NoExt().c_str());
}

/**
 * User has changed something, store it and update the display
 */
void COptionsPageGlobalStyles::onChange()
{
	m_dirty = true;
	storeChanges();
	updateDisplay();
}

/**
 * Store changes to the style
 */
void COptionsPageGlobalStyles::storeChanges()
{
	if(m_pStyle != NULL)
	{
		m_pStyle->CheckCustomisation( m_pSchemes->GetDefaultStyle(), m_style );
	}
}

/**
 * Store settings for the previous selection and display the new one
 */
void COptionsPageGlobalStyles::updateSel()
{
	storeChanges();

	int sel = m_list.GetSelectedIndex();
	
	if(sel == -1)
	{
		m_pStyle = NULL;
	}
	else
	{
		m_pStyle = reinterpret_cast<NamedStyleDetails*>( m_list.GetItemData( sel ) );
		m_pStyle->Combine( m_pSchemes->GetDefaultStyle(), m_style );
	}
	
	updateDisplay();
}

void COptionsPageGlobalStyles::updateDisplay()
{
	if(!m_pStyle)
	{
		return;
	}

	m_sd.SetStyle(m_style.FontName.c_str(), m_style.FontSize, m_style.ForeColor, m_style.BackColor, m_pStyle->FriendlyName.c_str(), m_style.Bold, m_style.Italic, m_style.Underline);

	m_bold.SetCheck(m_style.Bold ? BST_CHECKED : BST_UNCHECKED);
	m_italic.SetCheck(m_style.Italic ? BST_CHECKED : BST_UNCHECKED);
	m_underline.SetCheck(m_style.Underline ? BST_CHECKED : BST_UNCHECKED);
	m_eolfilled.SetCheck(m_style.EOLFilled ? BST_CHECKED : BST_UNCHECKED);
	m_fore.SetColor(m_style.ForeColor);
	m_back.SetColor(m_style.BackColor);

	m_FontCombo.SelectString(-1, m_style.FontName.c_str());
	m_SizeCombo.Select(m_style.FontSize);
}

void COptionsPageGlobalStyles::loadPreset(LPCTSTR path)
{
	tstring presetPath;
	OPTIONS->GetPNPath(presetPath, PNPATH_PRESETS);

	CFileName fn(path);
	fn.ChangeExtensionTo(_T(".xml"));
	fn.Root( presetPath.c_str() );
	
	m_pSchemes->LoadPresets(fn.c_str());

	m_dirty = true;

	// TODO: Need to notify all styles windows to update their displays
	updateDisplay();

	::SendMessage(GetParent(), PN_NOTIFY, 0, PN_UPDATEDISPLAY);
}

void COptionsPageGlobalStyles::savePreset()
{
	tstring presetPath;
	OPTIONS->GetPNPath(presetPath, PNPATH_PRESETS);

	CAutoSaveDialog sd(_T("Preset Files (*.xml)|*.xml|All Files (*.*)|*.*|"));
	sd.SetTitle(_T("Save Preset As..."));
	sd.SetInitialPath( presetPath.c_str() );
	sd.SetDefaultExtension(_T("xml"));
	
	if(sd.DoModal() == IDOK)
	{
		m_pSchemes->SaveConfig( sd.GetSingleFileName() );
	}
}