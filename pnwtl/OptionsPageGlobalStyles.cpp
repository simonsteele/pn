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
		m_list.AddItem(0, 0, (*i).second->Style->name.c_str());
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
	return 0;
}

LRESULT COptionsPageGlobalStyles::OnBackChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	return 0;
}

LRESULT COptionsPageGlobalStyles::OnFontChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	return 0;
}

LRESULT COptionsPageGlobalStyles::OnSizeChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	return 0;
}

LRESULT COptionsPageGlobalStyles::OnTreeSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	return 0;
}

LRESULT COptionsPageGlobalStyles::OnBoldClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	return 0;
}

LRESULT COptionsPageGlobalStyles::OnItalicClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	return 0;
}

LRESULT COptionsPageGlobalStyles::OnUnderlineClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	return 0;
}

LRESULT COptionsPageGlobalStyles::OnEOLFilledClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	return 0;
}

LRESULT COptionsPageGlobalStyles::OnResetClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	return 0;
}

LRESULT COptionsPageGlobalStyles::OnResetAllClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	return 0;
}

LRESULT COptionsPageGlobalStyles::OnListSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	return 0;
}