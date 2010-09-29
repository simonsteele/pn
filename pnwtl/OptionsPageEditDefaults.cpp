/**
 * @file optionspages.cpp
 * @brief Edit Defaults Page.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "OptionsPages.h"

//////////////////////////////////////////////////////////////////////////////
// COptionsPageEditDefaults
//////////////////////////////////////////////////////////////////////////////

void COptionsPageEditDefaults::OnOK()
{
	if(!m_bCreated)
		return;

	DoDataExchange(TRUE);

	CComboBox cb(GetDlgItem(IDC_OPT_LECOMBO));
	m_SaveFormat = (EPNSaveFormat)cb.GetItemData(cb.GetCurSel());

	CComboBox cb2(GetDlgItem(IDC_OPT_CPCOMBO));
	m_DefaultEncoding = (EPNEncoding)cb2.GetItemData(cb2.GetCurSel());

	CComboBox cb3(GetDlgItem(IDC_OPT_CSCOMBO));
	m_AnsiCodePage = (ECodePage)cb3.GetItemData(cb3.GetCurSel());

	Options& options = *OPTIONS;
	options.SetCached(Options::OUseTabs, m_bUseTabs != FALSE);
	options.SetCached(Options::OTabWidth, m_iTabWidth);
	options.SetCached(Options::OLineNumbers, m_bLineNos != FALSE);
	options.SetCached(Options::OLineEndings, m_SaveFormat);
	options.SetCached(Options::OWordWrap, m_bWrap != FALSE);
	options.SetCached(Options::OVisibleLineEndings, m_bLineEndings);
	options.SetCached(Options::OVisibleWhiteSpace, m_bWhiteSpace);
	options.SetCached(Options::OMultiByteCodePage, m_AnsiCodePage);
	options.SetCached(Options::ODefaultEncoding, m_DefaultEncoding);
}

void COptionsPageEditDefaults::OnInitialise()
{
	m_bUseTabs		= OPTIONS->GetCached(Options::OUseTabs);
	m_iTabWidth		= OPTIONS->GetCached(Options::OTabWidth);
	m_bLineNos		= OPTIONS->GetCached(Options::OLineNumbers);
	m_bWrap			= OPTIONS->GetCached(Options::OWordWrap);
	m_SaveFormat	= (EPNSaveFormat)OPTIONS->GetCached(Options::OLineEndings);
	m_bLineEndings  = OPTIONS->GetCached(Options::OVisibleLineEndings);
	m_bWhiteSpace	= OPTIONS->GetCached(Options::OVisibleWhiteSpace);
	m_AnsiCodePage  = (ECodePage)OPTIONS->GetCached(Options::OMultiByteCodePage);
	m_DefaultEncoding = (EPNEncoding)OPTIONS->GetCached(Options::ODefaultEncoding);

	CComboBox cb(GetDlgItem(IDC_OPT_LECOMBO));
	for(int i = 0; i < cb.GetCount(); i++)
	{
		if(cb.GetItemData(i) == (DWORD_PTR)m_SaveFormat)
		{
			cb.SetCurSel(i);
			break;
		}
	}

	cb.Detach();

	// Now Default Encoding:
	cb.Attach(GetDlgItem(IDC_OPT_CPCOMBO));
	for(int i = 0; i < cb.GetCount(); i++)
	{
		if(cb.GetItemData(i) == (DWORD_PTR)m_DefaultEncoding)
		{
			cb.SetCurSel(i);
			break;
		}
	}

	// ANSI Code Page:
	cb.Detach();
	cb.Attach(GetDlgItem(IDC_OPT_CSCOMBO));
	for(int i = 0; i < cb.GetCount(); i++)
	{
		if(cb.GetItemData(i) == (DWORD_PTR)m_AnsiCodePage)
		{
			cb.SetCurSel(i);
			break;
		}
	}

	DoDataExchange();
}

tstring COptionsPageEditDefaults::GetTreePosition()
{
	return MAKE_OPTIONSTREEPATH(IDS_OPTGROUP_GENERAL, IDS_OPTPAGE_DEFAULT);
}


LRESULT COptionsPageEditDefaults::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_defaultsHeader.SubclassWindow(GetDlgItem(IDC_DEFAULTS_STATIC));

	//typedef enum { PNSF_Windows = SC_EOL_CRLF, PNSF_Unix = SC_EOL_LF, PNSF_Mac = SC_EOL_CR, PNSF_NoChange} EPNSaveFormat;
	CComboBox cb;
	cb.Attach(GetDlgItem(IDC_OPT_LECOMBO));
	int idx = cb.InsertString(0, _T("Windows (CRLF)"));
	cb.SetItemData(idx, PNSF_Windows);
	idx = cb.InsertString(1, _T("Unix (LF)"));
	cb.SetItemData(idx, PNSF_Unix);
	idx = cb.InsertString(2, _T("Macintosh (CR)"));
	cb.SetItemData(idx, PNSF_Mac);

	cb.Detach();
	cb.Attach(GetDlgItem(IDC_OPT_CPCOMBO));
	idx = cb.InsertString(0, _T("ANSI"));
	cb.SetItemData(idx, eUnknown);
	idx = cb.InsertString(1, _T("UTF-8"));
	cb.SetItemData(idx, eUtf8);
	idx = cb.InsertString(2, _T("UTF-8 No BOM"));
	cb.SetItemData(idx, eUtf8NoBOM);
	idx = cb.InsertString(3, _T("UTF-16 Little Endian"));
	cb.SetItemData(idx, eUtf16LittleEndian);
	idx = cb.InsertString(4, _T("UTF-16 Big Endian"));
	cb.SetItemData(idx, eUtf16BigEndian);
	
	cb.Detach();
	cb.Attach(GetDlgItem(IDC_OPT_CSCOMBO));
	idx = cb.InsertString(0, _T("Default"));
	cb.SetItemData(idx, PNCP_Default);
	idx = cb.InsertString(1, _T("Shift-JIS"));
	cb.SetItemData(idx, PNCP_ShiftJIS);
	idx = cb.InsertString(2, _T("Chinese GBK"));
	cb.SetItemData(idx, PNCP_ChineseGBK);
	idx = cb.InsertString(3, _T("Korean Unified Hangul"));
	cb.SetItemData(idx, PNCP_KoreanHangul);
	idx = cb.InsertString(4, _T("Chinese Big5"));
	cb.SetItemData(idx, PNCP_ChineseBig5);
	idx = cb.InsertString(5, _T("Korean Johab"));
	cb.SetItemData(idx, PNCP_KoreanJohab);

	return 0;
}
