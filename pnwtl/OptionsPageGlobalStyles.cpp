/**
 * @file OptionsPageGlobalStyles.h
 * @brief Options Dialog Global Styles Page for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2007 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "OptionsDialogs.h"
#include "OptionsPageGlobalStyles.h"
#include "SchemeConfig.h"

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

	for(StylePtrMap::const_iterator i = classes.begin();
		i != classes.end();
		++i)
	{
		NamedStyleDetails* style = static_cast<NamedStyleDetails*>((*i).second.get());
		if(style->FriendlyName.size())
		{
			int item = m_list.AddItem(0, 0, style->FriendlyName.c_str());
			m_list.SetItemData(item, reinterpret_cast<DWORD_PTR>(style));
		}
	}
}

LPCTSTR COptionsPageGlobalStyles::GetTreePosition()
{
	return _T("Schemes\\Styles");
}

void COptionsPageGlobalStyles::OnCancel()
{
}

LRESULT COptionsPageGlobalStyles::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CRect rc;
	
	m_list.Attach(GetDlgItem(IDC_STYLES_LIST));
	m_list.SetExtendedListViewStyle(LVS_EX_FULLROWSELECT, LVS_EX_FULLROWSELECT);
	m_list.AddColumn("Styles", 0);
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

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnForeChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	NMCOLORBUTTON* pN = reinterpret_cast<NMCOLORBUTTON*>(pnmh);
	COLORREF col = (pN->clr == CLR_DEFAULT ? m_fore.GetDefaultColor() : pN->clr);
	m_style.ForeColor = col;
	m_dirty = true;
	UpdateDisplay();
	return 0;
}

LRESULT COptionsPageGlobalStyles::OnBackChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	NMCOLORBUTTON* pN = reinterpret_cast<NMCOLORBUTTON*>(pnmh);
	COLORREF col = (pN->clr == CLR_DEFAULT ? m_fore.GetDefaultColor() : pN->clr);
	m_style.BackColor = col;
	m_dirty = true;
	UpdateDisplay();
	return 0;
}

LRESULT COptionsPageGlobalStyles::OnFontChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		int i = m_FontCombo.GetCurSel();
		CString str;
		m_FontCombo.GetLBText(i, str);
		m_style.FontName = (LPCTSTR)str;
		m_dirty = true;
		UpdateDisplay();
	}

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnSizeChanged(WORD wNotifyCode, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		int i = m_SizeCombo.GetSelection((wNotifyCode != CBN_SELCHANGE));//GetDlgItemInt(IDC_STYLE_SIZECOMBO);
		m_dirty = true;
		m_style.FontSize = i;
		UpdateDisplay();
	}

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnBoldClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		bool bC = m_bold.GetCheck() == BST_CHECKED;
		m_style.Bold = bC;
		m_dirty = true;
		UpdateDisplay();
	}

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnItalicClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		bool bC = m_italic.GetCheck() == BST_CHECKED;
		m_style.Italic = bC;
		m_dirty = true;
		UpdateDisplay();
	}

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnUnderlineClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		bool bC = m_underline.GetCheck() == BST_CHECKED;
		m_style.Underline = bC;
		m_dirty = true;
		UpdateDisplay();
	}

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnEOLFilledClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		bool bC = m_eolfilled.GetCheck() == BST_CHECKED;
		m_style.EOLFilled = bC;
		m_dirty = true;
		UpdateDisplay();
	}

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnResetClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		if(m_pStyle->CustomStyle != NULL)
		{
			delete m_pStyle->CustomStyle;
			m_pStyle->CustomStyle = NULL;
		}
		
		m_pStyle = NULL;
	}

	m_dirty = true;
	UpdateSel();

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnResetAllClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	for(int i = 0; i < m_list.GetItemCount(); ++i)
	{
		NamedStyleDetails* style = reinterpret_cast<NamedStyleDetails*>( m_list.GetItemData(i) );
		if(style != NULL && style->CustomStyle != NULL)
		{
			delete style->CustomStyle;
			style->CustomStyle = NULL;
		}
	}

	m_dirty = true;
	
	// Re-select whatever we're looking at...
	m_pStyle = NULL;
	UpdateSel();

	return 0;
}

LRESULT COptionsPageGlobalStyles::OnListSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	int sel = m_list.GetSelectedIndex();
	if(sel == -1)
		return 0;

	NamedStyleDetails* style = reinterpret_cast<NamedStyleDetails*>( m_list.GetItemData( sel ) );
	style->Combine( m_pSchemes->GetDefaultStyle(), m_style );
	
	UpdateDisplay();

	return 0;
}

void COptionsPageGlobalStyles::UpdateSel()
{
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
	
	UpdateDisplay();
}

void COptionsPageGlobalStyles::UpdateDisplay()
{
	m_sd.SetStyle(m_style.FontName.c_str(), m_style.FontSize, m_style.ForeColor, m_style.BackColor, m_style.name.c_str(), m_style.Bold, m_style.Italic, m_style.Underline);

	m_bold.SetCheck(m_style.Bold ? BST_CHECKED : BST_UNCHECKED);
	m_italic.SetCheck(m_style.Italic ? BST_CHECKED : BST_UNCHECKED);
	m_underline.SetCheck(m_style.Underline ? BST_CHECKED : BST_UNCHECKED);
	m_eolfilled.SetCheck(m_style.EOLFilled ? BST_CHECKED : BST_UNCHECKED);
	m_fore.SetColor(m_style.ForeColor);
	m_back.SetColor(m_style.BackColor);

	m_FontCombo.SelectString(-1, m_style.FontName.c_str());
	m_SizeCombo.Select(m_style.FontSize);
}