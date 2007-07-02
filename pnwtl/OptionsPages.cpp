/**
 * @file optionspages.cpp
 * @brief Options Dialog Pages (1) for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-2007 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "OptionsPages.h"
#include "OptionsDialogs.h"
#include "toolsmanager.h"

//////////////////////////////////////////////////////////////////////////////
// COptionsPageGeneral
//////////////////////////////////////////////////////////////////////////////

LPCTSTR COptionsPageGeneral::GetTreePosition()
{
	return _T("General");
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

	DoDataExchange();
}

LRESULT COptionsPageGeneral::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	return 0;
}

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
	m_CodePage = (ECodePage)cb2.GetItemData(cb2.GetCurSel());

	CComboBox cb3(GetDlgItem(IDC_OPT_CSCOMBO));
	m_CharSet = (int)cb3.GetItemData(cb3.GetCurSel());

	Options& options = *OPTIONS;
	options.SetCached(Options::OUseTabs, m_bUseTabs != FALSE);
	options.SetCached(Options::OTabWidth, m_iTabWidth);
	options.SetCached(Options::OLineNumbers, m_bLineNos != FALSE);
	options.SetCached(Options::OLineEndings, m_SaveFormat);
	options.SetCached(Options::ODefaultCodePage, m_CodePage);
	options.SetCached(Options::OWordWrap, m_bWrap != FALSE);
	options.SetCached(Options::OVisibleLineEndings, m_bLineEndings);
	options.SetCached(Options::OVisibleWhiteSpace, m_bWhiteSpace);
	options.SetCached(Options::ODefaultCharSet, m_CharSet);
}

void COptionsPageEditDefaults::OnInitialise()
{
	m_bUseTabs		= OPTIONS->GetCached(Options::OUseTabs);
	m_iTabWidth		= OPTIONS->GetCached(Options::OTabWidth);
	m_bLineNos		= OPTIONS->GetCached(Options::OLineNumbers);
	m_bWrap			= OPTIONS->GetCached(Options::OWordWrap);
	m_SaveFormat	= (EPNSaveFormat)OPTIONS->GetCached(Options::OLineEndings);
	m_CodePage		= (ECodePage)OPTIONS->GetCached(Options::ODefaultCodePage);
	m_bLineEndings  = OPTIONS->GetCached(Options::OVisibleLineEndings);
	m_bWhiteSpace	= OPTIONS->GetCached(Options::OVisibleWhiteSpace);
	m_CharSet		= OPTIONS->GetCached(Options::ODefaultCharSet);

	CComboBox cb(GetDlgItem(IDC_OPT_LECOMBO));
	for(int i = 0; i < cb.GetCount(); i++)
	{
		if(cb.GetItemData(i) == m_SaveFormat)
		{
			cb.SetCurSel(i);
			break;
		}
	}

	cb.Detach();
	cb.Attach(GetDlgItem(IDC_OPT_CPCOMBO));
	for(int i = 0; i < cb.GetCount(); i++)
	{
		if(cb.GetItemData(i) == m_CodePage)
		{
			cb.SetCurSel(i);
			break;
		}
	}

	cb.Detach();
	cb.Attach(GetDlgItem(IDC_OPT_CSCOMBO));
	for(int i = 0; i < cb.GetCount(); i++)
	{
		if(cb.GetItemData(i) == m_CharSet)
		{
			cb.SetCurSel(i);
			break;
		}
	}

	DoDataExchange();
}

LPCTSTR COptionsPageEditDefaults::GetTreePosition()
{
	return _T("General\\Defaults");
}


LRESULT COptionsPageEditDefaults::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
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
	idx = cb.InsertString(0, _T("Default"));
	cb.SetItemData(idx, PNCP_Default);
	idx = cb.InsertString(1, _T("Unicode (UTF-8)"));
	cb.SetItemData(idx, PNCP_Unicode);
	idx = cb.InsertString(2, _T("Shift-JIS"));
	cb.SetItemData(idx, PNCP_ShiftJIS);
	idx = cb.InsertString(3, _T("Simple Chinese GBK"));
	cb.SetItemData(idx, PNCP_ChineseGBK);
	idx = cb.InsertString(4, _T("Korean Unified Hangul"));
	cb.SetItemData(idx, PNCP_KoreanHangul);
	idx = cb.InsertString(5, _T("Simple Chinese Big5"));
	cb.SetItemData(idx, PNCP_ChineseBig5);
	idx = cb.InsertString(6, _T("Korean Johab"));
	cb.SetItemData(idx, PNCP_KoreanJohab);

	cb.Detach();
	cb.Attach(GetDlgItem(IDC_OPT_CSCOMBO));
	idx = cb.InsertString(0, _T("Default"));
	cb.SetItemData(idx, SC_CHARSET_DEFAULT);
	//SC_CHARSET_ANSI, SC_CHARSET_ARABIC, SC_CHARSET_BALTIC, SC_CHARSET_CHINESEBIG5, SC_CHARSET_DEFAULT, SC_CHARSET_EASTEUROPE, SC_CHARSET_GB2312, SC_CHARSET_GREEK, SC_CHARSET_HANGUL, SC_CHARSET_HEBREW, SC_CHARSET_JOHAB, SC_CHARSET_MAC, SC_CHARSET_OEM, SC_CHARSET_RUSSIAN (code page 1251), SC_CHARSET_SHIFTJIS, SC_CHARSET_SYMBOL, SC_CHARSET_THAI, SC_CHARSET_TURKISH, and SC_CHARSET_VIETNAMESE
	idx = cb.InsertString(1, _T("ANSI"));
	cb.SetItemData(idx, SC_CHARSET_ANSI);
	idx = cb.InsertString(2, _T("Arabic"));
	cb.SetItemData(idx, SC_CHARSET_ARABIC);
	idx = cb.InsertString(3, _T("Baltic"));
	cb.SetItemData(idx, SC_CHARSET_BALTIC);
	idx = cb.InsertString(4, _T("Chinese Big5"));
	cb.SetItemData(idx, SC_CHARSET_CHINESEBIG5);
	idx = cb.InsertString(5, _T("East European"));
	cb.SetItemData(idx, SC_CHARSET_EASTEUROPE);
	idx = cb.InsertString(6, _T("GB 2312"));
	cb.SetItemData(idx, SC_CHARSET_GB2312);
	idx = cb.InsertString(7, _T("Greek"));
	cb.SetItemData(idx, SC_CHARSET_GREEK);
	idx = cb.InsertString(8, _T("Hangul"));
	cb.SetItemData(idx, SC_CHARSET_HANGUL);
	idx = cb.InsertString(9, _T("Hebrew"));
	cb.SetItemData(idx, SC_CHARSET_HEBREW);
	idx = cb.InsertString(10, _T("Johab"));
	cb.SetItemData(idx, SC_CHARSET_JOHAB);
	idx = cb.InsertString(11, _T("MAC"));
	cb.SetItemData(idx, SC_CHARSET_MAC);
	idx = cb.InsertString(12, _T("OEM"));
	cb.SetItemData(idx, SC_CHARSET_OEM);
	idx = cb.InsertString(13, _T("Russian"));
	cb.SetItemData(idx, SC_CHARSET_RUSSIAN);
	idx = cb.InsertString(14, _T("Shift-JIS"));
	cb.SetItemData(idx, SC_CHARSET_SHIFTJIS);
	idx = cb.InsertString(15, _T("Symbol"));
	cb.SetItemData(idx, SC_CHARSET_SYMBOL);
	idx = cb.InsertString(16, _T("Thai"));
	cb.SetItemData(idx, SC_CHARSET_THAI);
	idx = cb.InsertString(17, _T("Turkish"));
	cb.SetItemData(idx, SC_CHARSET_TURKISH);
	idx = cb.InsertString(18, _T("Vietnamese"));
	cb.SetItemData(idx, SC_CHARSET_VIETNAMESE);

	return 0;
}

//////////////////////////////////////////////////////////////////////////////
// COptionsPageVisual
//////////////////////////////////////////////////////////////////////////////

LPCTSTR COptionsPageVisual::GetTreePosition()
{
	return _T("General\\Visual Help");
}

void COptionsPageVisual::OnOK()
{
	if(!m_bCreated)
		return;

	DoDataExchange(TRUE);

	Options& options = *OPTIONS;
	options.SetCached(Options::OShowIndentGuides, m_bIndentGuides != FALSE);
	options.SetCached(Options::OFoldingEnabled, m_bFolding != FALSE);
	options.SetCached(Options::OLineHighlight, m_bLineHighlight != FALSE);
	options.SetCached(Options::OLineHighlightColour, m_btnLineCol.SafeGetColor());
	options.SetCached(Options::ORightGuide, m_iLongLineHelp);
	options.SetCached(Options::ORightColumn, m_iRightColumn);
	options.SetCached(Options::ORightGuideColour, m_btnLLCol.SafeGetColor());

	OPTIONS->SetCached(Options::OLineHighlightAlpha, m_trackerHighlight.GetPos());
}

void COptionsPageVisual::OnInitialise()
{
	m_bIndentGuides		= OPTIONS->GetCached(Options::OShowIndentGuides);
	m_bFolding			= OPTIONS->GetCached(Options::OFoldingEnabled);
	m_bLineHighlight	= OPTIONS->GetCached(Options::OLineHighlight);
	m_iLongLineHelp		= OPTIONS->GetCached(Options::ORightGuide);
	m_iRightColumn		= OPTIONS->GetCached(Options::ORightColumn);
	
	m_btnLineCol.SetColor( OPTIONS->GetCached(Options::OLineHighlightColour) );
	m_btnLLCol.SetColor( OPTIONS->GetCached(Options::ORightGuideColour) );

	DoDataExchange();

	m_trackerHighlight.SetPos( OPTIONS->GetCached(Options::OLineHighlightAlpha) );
}

LRESULT COptionsPageVisual::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_btnLineCol.SubclassWindow(GetDlgItem(IDC_OPT_LINELIGHTBUTTON));
	m_btnLineCol.SetDefaultColor(RGB(255, 255, 224));
	m_btnLLCol.SubclassWindow(GetDlgItem(IDC_OPT_LLCOLORBUTTON));
	m_btnLLCol.SetDefaultColor(RGB(215,215,215));

	m_trackerHighlight.Attach(GetDlgItem(IDC_LINEHIGHLIGHTSLIDER));
	m_trackerHighlight.SetRange(0, 255);
	m_trackerHighlight.SetPageSize(32);

	m_trackerHighlight.ModifyStyle(0, TBS_FIXEDLENGTH);

	int h = m_trackerHighlight.GetThumbLength();
	h -= 2;

	CRect rc;
	m_trackerHighlight.GetWindowRect(&rc);
	rc.bottom += 2;
	m_trackerHighlight.SetWindowPos(NULL, &rc, SWP_NOZORDER | SWP_NOMOVE);
	m_trackerHighlight.SetThumbLength(h);

	//TODO: Enable/disable IDC_OPT_LLCOLUMNEDIT, IDC_OPT_LLCOLORBUTTON and the associated static text based on the radio selection
	//TODO: Enable/disable IDC_OPT_LINELIGHTBUTTON based on the checkbox selection
	return 0;
}

//////////////////////////////////////////////////////////////////////////////
// COptionsPageConf
//////////////////////////////////////////////////////////////////////////////

void COptionsPageConf::OnOK()
{
	if(!m_bCreated)
		return;

	DoDataExchange(TRUE);

	OPTIONS->SetCached(Options::OAlreadyOpenAction, m_iReOpen);
	OPTIONS->SetCached(Options::OAlreadyOpenDropAction, m_iReDrop);
}

void COptionsPageConf::OnInitialise()
{
	m_iReOpen = OPTIONS->GetCached(Options::OAlreadyOpenAction);
	m_iReDrop = OPTIONS->GetCached(Options::OAlreadyOpenDropAction);

	DoDataExchange();
}

LPCTSTR COptionsPageConf::GetTreePosition()
{
	return _T("General\\Confirmations");
}

LRESULT COptionsPageConf::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
		
	return 0;
}

//////////////////////////////////////////////////////////////////////////////
// COptionsPageDialogs
//////////////////////////////////////////////////////////////////////////////

void COptionsPageDialogs::OnOK()
{
	if(!m_bCreated)
		return;

	DoDataExchange(TRUE);

	// File Dialogs
	OPTIONS->Set(PNSK_INTERFACE, _T("OpenInCurrentDir"), m_bOpenCurFileDir != FALSE);
	
	// Find Dialog
	OPTIONS->Set(PNSK_INTERFACE, _T("FindStaysOpen"), m_bCloseFindNext == FALSE);
	OPTIONS->SetCached(Options::OFindAlphaEnabled, m_bFindAlpha);
}

void COptionsPageDialogs::OnInitialise()
{
	// File Dialogs
	m_bOpenCurFileDir = OPTIONS->Get(PNSK_INTERFACE, _T("OpenInCurrentDir"), true);

	// Find Dialog
	m_bCloseFindNext = !OPTIONS->Get(PNSK_INTERFACE, _T("FindStaysOpen"), false);
	m_bFindAlpha = OPTIONS->GetCached(Options::OFindAlphaEnabled);

	DoDataExchange();
}

LPCTSTR COptionsPageDialogs::GetTreePosition()
{
	return _T("General\\Dialogs");
}

LRESULT COptionsPageDialogs::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	return 0;
}

//////////////////////////////////////////////////////////////////////////////
// COptionsPageStyle
//////////////////////////////////////////////////////////////////////////////

bool COptionsPageStyle::IsDirty()
{
	return m_bDirty;
}

LRESULT COptionsPageStyle::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_FontCombo.SubclassWindow(GetDlgItem(IDC_FONT_COMBO));
	m_SizeCombo.Attach(GetDlgItem(IDC_FONTSIZE_COMBO));

	m_fore.SubclassWindow(GetDlgItem(IDC_STYLE_FOREBUTTON));
	m_back.SubclassWindow(GetDlgItem(IDC_STYLE_BACKBUTTON));

	m_cur.SubclassWindow(GetDlgItem(IDC_STYLE_CURCOLBUTTON));
	m_indentGuides.SubclassWindow(GetDlgItem(IDC_STYLE_IGCOLBUTTON2));
	m_selFore.SubclassWindow(GetDlgItem(IDC_STYLE_SELFOREBUTTON));
	m_selBack.SubclassWindow(GetDlgItem(IDC_STYLE_SELBACKBUTTON2));


	m_bold.Attach(GetDlgItem(IDC_STYLE_BOLDCHECK));
	m_italic.Attach(GetDlgItem(IDC_STYLE_ITALICCHECK));
	m_underline.Attach(GetDlgItem(IDC_STYLE_UNDERLINECHECK));
	
	m_SizeCombo.Add(6);
	m_SizeCombo.Add(8);
	m_SizeCombo.Add(10);
	m_SizeCombo.Add(12);
	m_SizeCombo.Add(14);
	m_SizeCombo.Add(16);
	m_SizeCombo.Add(18);

	return 0;
}

void COptionsPageStyle::OnInitialise()
{
	if(!m_pSchemes)
		return;

	m_defclass = m_pSchemes->GetClass(_T("default"));
	StyleDetails style;
	m_defclass->Combine(NULL, style);
	
	m_FontCombo.SelectString(-1, style.FontName.c_str());
	m_SizeCombo.Select(style.FontSize);
	
	m_fore.SetColor(style.ForeColor);
	m_fore.SetDefaultColor(RGB(0,0,0));
	
	m_back.SetColor(style.BackColor);
	m_back.SetDefaultColor(RGB(255,255,255));

	m_bold.SetCheck(style.Bold ? BST_CHECKED : BST_UNCHECKED);
	m_italic.SetCheck(style.Italic ? BST_CHECKED : BST_UNCHECKED);
	m_underline.SetCheck(style.Underline ? BST_CHECKED : BST_UNCHECKED);

	EditorColours* ec = m_pSchemes->GetDefaultColours();
	
	m_selFore.SetDefaultColor(::GetSysColor(COLOR_HIGHLIGHTTEXT));
	m_selBack.SetDefaultColor(::GetSysColor(COLOR_HIGHLIGHT));
	m_cur.SetDefaultColor(::GetSysColor(COLOR_WINDOWTEXT));
	m_indentGuides.SetDefaultColor(RGB(0,0,0));

	COLORREF c;
	if(ec->GetColour(EditorColours::ecSelFore, c))
	{
		if(c == (COLORREF)-1)
		{
			CButton(GetDlgItem(IDC_STYLE_SELUSEFORE)).SetCheck(BST_CHECKED);
		}
		else
            m_selFore.SetColor( c );
	}
	if(ec->GetColour(EditorColours::ecSelBack, c))
		m_selBack.SetColor( c );
	if(ec->GetColour(EditorColours::ecCaret, c))
		m_cur.SetColor( c );
	if(ec->GetColour(EditorColours::ecIndentG, c))
		m_indentGuides.SetColor( c );

	// Simple dirty checking - if the page is shown we rebuild.
	m_bDirty = true;
}

LRESULT COptionsPageStyle::OnNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	if(lParam == PN_UPDATEDISPLAY)
	{
		// re-initialise display...
		OnInitialise();
	}

	return 0;
}

void COptionsPageStyle::OnOK()
{
	if(!m_bCreated)
		return;

	StyleDetails display;
	StyleDetails current;
	m_defclass->Combine(NULL, current);
		
	int i = m_FontCombo.GetCurSel();
	CString str;
	m_FontCombo.GetLBText(i, str);

	display.FontName	= str;
	display.FontSize	= GetDlgItemInt(IDC_FONTSIZE_COMBO);
	display.ForeColor	= m_fore.SafeGetColor();
	display.BackColor	= m_back.SafeGetColor();
	display.Bold		= (m_bold.GetCheck() == BST_CHECKED);
	display.Italic		= (m_italic.GetCheck() == BST_CHECKED);
	display.Underline	= (m_underline.GetCheck() == BST_CHECKED);
	display.name		= _T("default");

	if(display != current)
	{
		// the new style is not the same as the current style:

		// work out what the differences are
		StyleDetails therealdefault( *m_defclass->Style );
		display.compareTo(therealdefault);

		if(display.values == 0)
		{
			// Not custom any more...
			m_defclass->Reset();
		}
		else
		{
			if(m_defclass->CustomStyle)
				*m_defclass->CustomStyle = display;
			else
				m_defclass->CustomStyle = new StyleDetails(display);
		}
	}
	
	// Clear all existing colour customisations
	EditorColours* ec = m_pSchemes->GetDefaultColours();
	ec->Clear();

	COLORREF c;
	c = m_cur.GetColor();
	
	if( CButton(GetDlgItem(IDC_STYLE_SELUSEFORE)).GetCheck() == BST_CHECKED )
		c = CLR_NONE;
	else
		c = m_selFore.GetColor();

	if(c != CLR_DEFAULT)
		ec->SetColour( EditorColours::ecSelFore, c);
	
	c = m_selBack.GetColor();
	if(c != CLR_DEFAULT)
		ec->SetColour( EditorColours::ecSelBack, c);

	c = m_cur.GetColor();
	if(c != CLR_DEFAULT)
		ec->SetColour( EditorColours::ecCaret, c);

	c = m_indentGuides.GetColor();
	if(c != CLR_DEFAULT)
		ec->SetColour( EditorColours::ecIndentG, c);
}

void COptionsPageStyle::OnCancel()
{
}

LPCTSTR COptionsPageStyle::GetTreePosition()
{
	return _T("Schemes");
}

//////////////////////////////////////////////////////////////////////////////
// COptionsPageSchemes
//////////////////////////////////////////////////////////////////////////////

COptionsPageSchemes::COptionsPageSchemes(SchemeConfigParser* pSchemes) 
	: COptionsPageImpl<COptionsPageSchemes>()
	, m_stylestab(pSchemes)
{
	m_pSchemes = pSchemes;
	m_bDirty = false;
}

bool COptionsPageSchemes::IsDirty()
{
	// In anticipation of a better dirty flag, we should or the IsDirty
	// methods of each tab to see if we're dirty. For now, m_bDirty will
	// always be true if we've shown.
	return m_bDirty || m_misctab.IsDirty();
}

// Dialog Message Hook stuff...
//#include "include/dialogmessagehook.h"
//CDialogMessageHook::InstallHook(m_props.m_hWnd);

LRESULT COptionsPageSchemes::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CWindow label;
	CSize s;
	CRect rc;

	label.Attach(GetDlgItem(IDC_SCHEMELABEL));
	
	CDC dc(label.GetDC());
	dc.GetTextExtent(_T("Scheme:"), 7, &s);
	
	label.GetWindowRect(rc);
	ScreenToClient(rc);
	rc.right = rc.left + s.cx;

	CRect rcCombo;

	m_combo.Attach(GetDlgItem(IDC_SCHEMECOMBO));

	m_combo.GetWindowRect(rcCombo);
	ScreenToClient(rcCombo);
	rcCombo.left = rc.right + 5;

	
	CRect rcPH;
	::GetWindowRect(GetDlgItem(IDC_PS_PLACEHOLDER), rcPH);
	ScreenToClient(rcPH);
	m_stylestab.SetTitle(_T("Styles"));
	m_keywordstab.SetTitle(_T("Keywords"));
	m_misctab.SetTitle(_T("More Options"));
	m_props.AddPage(m_stylestab);
	m_props.AddPage(m_keywordstab);
	m_props.AddPage(m_misctab);

	// Store focus or the property sheet eats it.
	HWND hCurFocus = ::GetFocus();
	m_props.Create(m_hWnd, 0, rcPH);
	::SetFocus(hCurFocus);

	return 0;
}

LRESULT COptionsPageSchemes::OnNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	if(lParam == PN_UPDATEDISPLAY)
	{
		// re-initialise display...
		m_stylestab.UpdateDisplay();
		m_misctab.UpdateDisplay();
	}

	return 0;
}


void COptionsPageSchemes::OnInitialise()
{
	// Load the schemes, we don't want "Plain Text", but we do want internal schemes.
	m_combo.Load(m_pSchemes, NULL, false, true);
	Update();

	m_bDirty = true;
}

void COptionsPageSchemes::OnOK()
{
	m_stylestab.Finalise();
	m_keywordstab.Finalise();
	m_misctab.Finalise();
}

LPCTSTR COptionsPageSchemes::GetTreePosition()
{
	return _T("Schemes\\Advanced");
}

LRESULT COptionsPageSchemes::OnComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Update();
	return 0;
}

void COptionsPageSchemes::Update()
{
	int i = m_combo.GetCurSel();
	SchemeDetails* pScheme = m_combo.GetItemScheme(i);
	m_stylestab.SetScheme(pScheme);
	m_keywordstab.SetScheme(pScheme);
	m_misctab.SetScheme(pScheme);
}

//////////////////////////////////////////////////////////////////////////////
// COptionsPageTools
//////////////////////////////////////////////////////////////////////////////

COptionsPageTools::COptionsPageTools(SchemeConfigParser* pSchemes, ToolsManager* pToolManager)
{
	m_pSchemes = pSchemes;
	m_pScheme = NULL;
	m_pCurrent = NULL;
	m_toolstore = pToolManager;
}

COptionsPageTools::COptionsPageTools(ToolsManager* pToolManager) /*protected*/
{
	m_pSchemes = NULL;
	m_pScheme = NULL;
	m_pCurrent = NULL;
	m_toolstore = pToolManager;
}

COptionsPageTools::~COptionsPageTools()
{
	
}

LRESULT COptionsPageTools::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CWindow label;
	CSize s;
	CRect rc;

	label.Attach(GetDlgItem(IDC_SCHEMELABEL));
	
	CDC dc(label.GetDC());
	dc.GetTextExtent(_T("Scheme:"), 7, &s);
	
	label.GetWindowRect(rc);
	ScreenToClient(rc);
	rc.right = rc.left + s.cx;
	label.SetWindowPos(HWND_TOP, &rc, 0);

	CRect rcCombo;

	m_combo.Attach(GetDlgItem(IDC_SCHEMECOMBO));

	m_combo.GetWindowRect(rcCombo);
	ScreenToClient(rcCombo);
	rcCombo.left = rc.right + 5;
	//m_combo.SetWindowPos(HWND_TOP, &rcCombo, 0);

	m_list.Attach(GetDlgItem(IDC_LIST));
	m_list.SetExtendedListViewStyle( m_list.GetExtendedListViewStyle() | LVS_EX_FULLROWSELECT );
	m_list.GetClientRect(rc);
	m_list.InsertColumn(0, _T("Name"), LVCFMT_LEFT, 130, 0);
	m_list.InsertColumn(1, _T("Command"), LVCFMT_LEFT, rc.Width() - 130 - 100 - 20, 0);
	m_list.InsertColumn(2, _T("Params"), LVCFMT_LEFT, 100, 0);

	m_btnMoveUp.SetDirection(CArrowButton::abdUp);
	m_btnMoveUp.SubclassWindow(GetDlgItem(IDC_TOOLS_MOVEUPBUTTON));
	m_btnMoveDown.SubclassWindow(GetDlgItem(IDC_TOOLS_MOVEDOWNBUTTON));

	EnableButtons();

	return 0;
}

void COptionsPageTools::OnInitialise()
{
	m_combo.AddScheme(_T("(None - Global Tools)"), NULL);
	
	m_combo.Load(m_pSchemes);
	
	if(m_combo.GetCount() > 0)
	{
		Update();
	}
}

void COptionsPageTools::OnOK()
{
	//m_toolstore.Save();
	UpdateIndexes();
}

void COptionsPageTools::UpdateIndexes()
{
	// We re-number the tools here so that they can be displayed properly in the menu.
	if(m_pCurrent != NULL)
	{
		int itemCount = m_list.GetItemCount();
		for(int i = 0; i < itemCount; i++)
		{
			ToolDefinition* pDef = reinterpret_cast<ToolDefinition*>(m_list.GetItemData(i));
			if(pDef)
			{
				pDef->Index = i;
			}
		}
	}
}

void COptionsPageTools::Update()
{
	m_bChanging = true;

	UpdateIndexes();

	m_pCurrent = NULL;
	m_list.DeleteAllItems();

	int iSel = getCombo()->GetCurSel();
	updateFromSel(iSel);

	SchemeTools* pTools = GetTools();
	if(pTools)
	{
		TOOLDEFS_LIST& l = pTools->GetTools();
		for(TOOLDEFS_LIST::const_iterator i = l.begin(); i != l.end(); ++i)
		{
			AddDefinition(*i);
		}
	}

	m_bChanging = false;
	
	EnableButtons();
}

CComboBox* COptionsPageTools::getCombo()
{
	return &m_combo;
}

void COptionsPageTools::updateFromSel(int iSel)
{
	if (iSel != -1)
	{
		if(iSel != 0)
		{
			m_pScheme = m_combo.GetItemScheme(iSel);
		}
		else
		{
			// Global Tools
			m_pScheme = NULL;
			m_pCurrent = m_toolstore->GetGlobalTools();
		}
	}
	else
		m_pScheme = NULL;
}

SchemeTools* COptionsPageTools::GetTools()
{
	if(!m_pCurrent)
	{
		m_pCurrent = m_toolstore->GetToolsFor(m_pScheme->Name.c_str());
	}
		
	return m_pCurrent;
}

bool COptionsPageTools::doToolEditDlg(ToolDefinition* in, ToolDefinition* out)
{
	LPCTSTR title = (in != NULL) ? _T("Edit Tool") : _T("New Tool");
	CPropertySheet sheet( title, 0, m_hWnd );
	sheet.m_psh.dwFlags |= (PSH_NOAPPLYNOW | PSH_PROPTITLE | PSH_USEICONID);
	sheet.m_psh.pszIcon = MAKEINTRESOURCE(IDR_MDICHILD);
	
	CToolSettingsPage toolPage(_T("Properties"));
	CToolConsoleIOPage consolePage(_T("Console I/O"));

	if(in)
	{
		toolPage.SetValues(in);
		consolePage.SetValues(in);
	}

	sheet.AddPage(toolPage);
	sheet.AddPage(consolePage);
	if(sheet.DoModal() == IDOK)
	{
		toolPage.GetValues(out);
		consolePage.GetValues(out);
		
		return true;
	}

	return false;
}

void COptionsPageTools::EnableButtons()
{
	if(m_bChanging)
		return;

	bool bEnable = (m_pScheme != NULL || getCombo()->GetCurSel() == 0);
	enableButtons(bEnable);
}

void COptionsPageTools::enableButtons(bool bEnable)
{
	if(m_bChanging)
		return;

	int iSelIndex = m_list.GetSelectedIndex();

	::EnableWindow(GetDlgItem(IDC_TOOLS_ADDBUTTON), bEnable);
	
	// A scheme or global tools and a selected item...
	bEnable = bEnable && (iSelIndex != -1);

	::EnableWindow(GetDlgItem(IDC_TOOLS_REMOVEBUTTON), bEnable);
	::EnableWindow(GetDlgItem(IDC_TOOLS_EDITBUTTON), bEnable);

	m_btnMoveUp.EnableWindow(bEnable && (iSelIndex != 0));
	m_btnMoveDown.EnableWindow(bEnable && (iSelIndex != (m_list.GetItemCount() - 1)));
}

void COptionsPageTools::AddDefinition(ToolDefinition* pDef)
{
	LVITEM lvi;

	lvi.mask = LVIF_IMAGE | LVIF_TEXT | LVIF_PARAM;
	lvi.iItem = m_list.GetItemCount();
	lvi.iSubItem = 0;
	lvi.pszText = const_cast<LPTSTR>( pDef->Name.c_str() );
	lvi.iImage = 0;
	lvi.lParam = reinterpret_cast<LPARAM>(pDef);

	int iItem = m_list.InsertItem(&lvi);

	lvi.iItem = iItem;
	lvi.mask = LVIF_TEXT;
	lvi.iSubItem = 1;
	lvi.pszText = const_cast<LPTSTR>( pDef->Command.c_str() );
	m_list.SetItem(&lvi);
	lvi.iSubItem = 2;
	lvi.pszText = const_cast<LPTSTR>( pDef->Params.c_str() );
	m_list.SetItem(&lvi);
}

LRESULT COptionsPageTools::OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	SourcedToolDefinition* pDef = new SourcedToolDefinition(m_toolstore->GetDefaultToolStore());

	if (doToolEditDlg(NULL, pDef))
	{
		//@todo check if the name is valid...
		GetTools()->Add(pDef);

		AddDefinition(pDef);
	}
	else
	{
		delete pDef;
	}

	return 0;
}

LRESULT COptionsPageTools::OnEditClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int iSelIndex = m_list.GetSelectedIndex();

	if(iSelIndex != -1)
	{
		ToolDefinition* pDef = reinterpret_cast<ToolDefinition*>(m_list.GetItemData(iSelIndex));
		if(pDef != NULL)
		{
			if(doToolEditDlg(pDef, pDef))
			{
				LVITEM lvi;
				lvi.mask = LVIF_TEXT;
				lvi.iItem = iSelIndex;
				lvi.iSubItem = 0;
				lvi.pszText = const_cast<LPTSTR>( pDef->Name.c_str() );
				m_list.SetItem(&lvi);

				lvi.iSubItem = 1;
				lvi.pszText = const_cast<LPTSTR>( pDef->Command.c_str() );
				m_list.SetItem(&lvi);

				lvi.iSubItem = 2;
				lvi.pszText = const_cast<LPTSTR>( pDef->Params.c_str() );
				m_list.SetItem(&lvi);
			}
		}
	}

	return 0;
}

LRESULT COptionsPageTools::OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int iSelIndex = m_list.GetSelectedIndex();

	if(iSelIndex != -1)
	{
		ToolDefinition* pDef = reinterpret_cast<ToolDefinition*>(m_list.GetItemData(iSelIndex));
		if(pDef != NULL)
			GetTools()->Delete(pDef);
		m_list.DeleteItem(iSelIndex);
	}

	return 0;
}

/*struct ListCtrlMoveOneData
{
	LPARAM ItemData;
    bool bMoveDown;
	bool bDoneOne;
};

int CALLBACK ListCtrlMoveOneCompareFunc(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
{
	ListCtrlMoveOneData* pD = reinterpret_cast<ListCtrlMoveOneData*>(lParamSort);
	LPARAM lpCompare = pD->bMoveDown ? lParam1 : lParam2;
	if( !pD->bDoneOne && lpCompare == pD->ItemData )
	{
		((ListCtrlMoveOneData*)lParamSort)->bDoneOne = true;
		
		return 1;
	}
	return 0;
}*/

LRESULT COptionsPageTools::OnUpClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int iSelIndex = m_list.GetSelectedIndex();
	if( iSelIndex != -1 )
	{
		ToolDefinition* pDef = reinterpret_cast<ToolDefinition*>(m_list.GetItemData(iSelIndex));
		GetTools()->MoveUp(pDef);
		m_list.MoveItemUp(iSelIndex);
	}
	EnableButtons();
	return 0;
}

LRESULT COptionsPageTools::OnDownClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int iSelIndex = m_list.GetSelectedIndex();
	if( iSelIndex != -1 )
	{
		ToolDefinition* pDef = reinterpret_cast<ToolDefinition*>(m_list.GetItemData(iSelIndex));
		GetTools()->MoveDown(pDef);
		m_list.MoveItemDown(iSelIndex);
	}
	EnableButtons();
	return 0;
}

LRESULT COptionsPageTools::OnComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Update();

	return 0;
}

LPCTSTR COptionsPageTools::GetTreePosition()
{
	return _T("Tools");
}

LRESULT COptionsPageTools::OnListKeyDown(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	EnableButtons();

	return 0;
}

LRESULT COptionsPageTools::OnListClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	EnableButtons();

	return 0;
}

LRESULT COptionsPageTools::OnListDblClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	BOOL bHandled;
	OnEditClicked(0, 0, 0, bHandled);
	return 0;
}

//////////////////////////////////////////////////////////////////////////////
// COptionsPageProjectTools
//////////////////////////////////////////////////////////////////////////////

#include "project.h"
#include "projectprops.h"
#include "projectregistry.h"

COptionsPageProjectTools::COptionsPageProjectTools(ToolsManager* pToolManager) : COptionsPageTools(pToolManager)
{
	
}

COptionsPageProjectTools::~COptionsPageProjectTools()
{
	
}

void COptionsPageProjectTools::OnInitialise()
{
	if(m_combo.GetCount() > 0)
	{
		Update();
	}
}

void COptionsPageProjectTools::OnOK()
{
	//m_toolstore.Save();
}

LPCTSTR COptionsPageProjectTools::GetTreePosition()
{
	return _T("Tools\\Project Tools");
}


LRESULT COptionsPageProjectTools::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CWindow label;
	CSize s;
	CRect rc;

	label.Attach(GetDlgItem(IDC_SCHEMELABEL));
	
	CDC dc(label.GetDC());
	label.SetWindowText("Project Template:");
	dc.GetTextExtent(_T("Project Template:"), 17, &s);
	
	label.GetWindowRect(rc);
	ScreenToClient(rc);
	rc.right = rc.left + s.cx;
	label.SetWindowPos(HWND_TOP, &rc, 0);

	CRect rcCombo;

	m_combo.Attach(GetDlgItem(IDC_SCHEMECOMBO));
	m_combo.GetWindowRect(rcCombo);

	int index = m_combo.AddString(_T("(None - All Projects)"));
	m_combo.SetItemDataPtr(index, NULL);

	const Projects::TEMPLATE_MAP& templates = Projects::Registry::GetInstance()->GetTemplates();
	for(Projects::TEMPLATE_MAP::const_iterator i = templates.begin();
		i != templates.end(); 
		++i)
	{
		index = m_combo.AddString((*i).second->GetName());
		m_combo.SetItemDataPtr(index, (*i).second);
	}

	m_combo.SetCurSel(0);
	
	ScreenToClient(rcCombo);
	rcCombo.left = rc.right + 5;
	//m_combo.SetWindowPos(HWND_TOP, &rcCombo, 0);

	m_list.Attach(GetDlgItem(IDC_LIST));
	m_list.SetExtendedListViewStyle( m_list.GetExtendedListViewStyle() | LVS_EX_FULLROWSELECT );
	m_list.GetClientRect(rc);
	m_list.InsertColumn(0, _T("Name"), LVCFMT_LEFT, 130, 0);
	m_list.InsertColumn(1, _T("Command"), LVCFMT_LEFT, rc.Width() - 130 - 100 - 20, 0);
	m_list.InsertColumn(2, _T("Params"), LVCFMT_LEFT, 100, 0);

	m_btnMoveUp.SetDirection(CArrowButton::abdUp);
	m_btnMoveUp.SubclassWindow(GetDlgItem(IDC_TOOLS_MOVEUPBUTTON));
	m_btnMoveDown.SubclassWindow(GetDlgItem(IDC_TOOLS_MOVEDOWNBUTTON));

	EnableButtons();

	return 0;
}

void COptionsPageProjectTools::EnableButtons()
{
	if(m_bChanging)
		return;

	bool bEnable = (m_pCurrent != NULL);
	enableButtons(bEnable);
}

SchemeTools* COptionsPageProjectTools::GetTools()
{
	if(!m_pCurrent)
	{
		if(m_pTemplate)
			m_pCurrent = m_toolstore->GetToolsForProject(m_pTemplate->GetID());
		else
			m_pCurrent = m_toolstore->GetGlobalProjectTools();
	}
		
	return m_pCurrent;
}

CComboBox* COptionsPageProjectTools::getCombo()
{
	return &m_combo;
}

void COptionsPageProjectTools::updateFromSel(int iSel)
{
	if (iSel != -1)
	{
		if(iSel != 0)
		{	
			m_pTemplate = reinterpret_cast<Projects::ProjectTemplate*>(m_combo.GetItemData(iSel));
		}
		else
		{
			// Global Tools
			m_pTemplate = NULL;
			m_pCurrent = m_toolstore->GetGlobalProjectTools();
		}
	}
	else
		m_pTemplate = NULL;
}

//////////////////////////////////////////////////////////////////////////////
// COptionsPageNewFiles
//////////////////////////////////////////////////////////////////////////////

#include "smartstart.h"

COptionsPageNewFiles::COptionsPageNewFiles(SchemeConfigParser* pSchemes)
{
	m_pSchemes = pSchemes;
	m_bDirty = false;
}

void COptionsPageNewFiles::AddItem(LPCTSTR key, LPCTSTR schemename)
{
	LVITEM lvi;

	Scheme* pScheme = SchemeManager::GetInstance()->SchemeByName(schemename);
	if(pScheme)
	{

		TCHAR* nameStore = new TCHAR[_tcslen(schemename)+1];
		_tcscpy(nameStore, schemename);

		lvi.mask = LVIF_IMAGE | LVIF_TEXT | LVIF_PARAM;
		lvi.iItem = m_list.GetItemCount();
		lvi.iSubItem = 0;
		lvi.pszText = const_cast<LPTSTR>( key );
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
		TCHAR* pNameStored = reinterpret_cast<TCHAR*>( m_list.GetItemData(i) );
		if(pNameStored)
			delete [] pNameStored;
		m_list.SetItemData(i, NULL);
	}
}

void COptionsPageNewFiles::OnInitialise()
{
	tstring strNewScheme = OPTIONS->Get(PNSK_EDITOR, _T("NewScheme"), _T("Plain Text"));

	// Populate and initialise schemes combo.
	m_combo.Load(m_pSchemes, strNewScheme.c_str());

	// Populate SmartStart list.
	STRING_MAP& smap = SmartStart::GetInstance()->GetMap();
	///@todo Sort the SmartStart list.
	
	for(SM_IT j = smap.begin(); j != smap.end(); ++j)
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
			STRING_MAP& smap = pSS->GetMap();
			smap.clear();

			CString strBuf;
			TCHAR* pStoredName;

			int count = m_list.GetItemCount();
			for(int i = 0; i < count; i++)
			{
				m_list.GetItemText(i, 0, strBuf);
				pStoredName = reinterpret_cast<TCHAR*>( m_list.GetItemData(i) );
				smap.insert(SM_VT(tstring(strBuf), tstring(pStoredName)));
			}

			pSS->Save();

			// Set the default new-scheme.
			int selIndex = m_combo.GetCurSel();
			LPCTSTR wt = NULL;
			SchemeDetails* pS = m_combo.GetItemScheme(selIndex);
			if(pS)
				wt = pS->Name.c_str();
			else
				wt = _T("Plain Text");
			OPTIONS->Set(PNSK_EDITOR, _T("NewScheme"), wt);

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

LPCTSTR COptionsPageNewFiles::GetTreePosition()
{
	return _T("Files\\New Files");
}

LRESULT COptionsPageNewFiles::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_list.Attach(GetDlgItem(IDC_SMARTSTART_LIST));
	CRect rc;
	m_list.SetExtendedListViewStyle(LVS_EX_FULLROWSELECT);
	m_list.GetClientRect(rc);
	m_list.InsertColumn(0, _T("Starting Phrase"), LVCFMT_LEFT, (rc.Width() / 3) * 2, 0);
	m_list.InsertColumn(1, _T("Scheme"), LVCFMT_LEFT, (rc.Width() / 3) - 20, 0);

	m_combo.Attach(GetDlgItem(IDC_NEW_SCHEMECOMBO));

	m_ssCheck.Attach(GetDlgItem(IDC_SMARTSTART_ENABLECHECK));

	return 0;
}

LRESULT COptionsPageNewFiles::OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CSmartStartEditorDialog edit(m_pSchemes);
	
	edit.SetValues(_T(""), _T(""));

	if(edit.DoModal() == IDOK)
	{
		tstring startPhrase, schemeName;
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
		TCHAR* pStoredData = reinterpret_cast<TCHAR*>( m_list.GetItemData(iSelIndex) );
		if(pStoredData && strBuf.GetLength() > 0)
		{
			CSmartStartEditorDialog edit(m_pSchemes);

			edit.SetValues(strBuf, pStoredData);

			if(edit.DoModal() == IDOK)
			{
				tstring startPhrase, schemeName;
				edit.GetValues(startPhrase, schemeName);
				m_list.SetItemText(iSelIndex, 0, startPhrase.c_str());
				
				Scheme* pScheme = SchemeManager::GetInstance()->SchemeByName(schemeName.c_str());
				if(pScheme)
					m_list.SetItemText(iSelIndex, 1, pScheme->GetTitle());
				
				delete [] pStoredData;
				pStoredData = new TCHAR[schemeName.length()+1];
				_tcscpy(pStoredData, schemeName.c_str());
				m_list.SetItemData(iSelIndex, reinterpret_cast<DWORD_PTR>( pStoredData ));
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
		TCHAR* pStoredName = reinterpret_cast<TCHAR*>( m_list.GetItemData(iSelIndex) );
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

//////////////////////////////////////////////////////////////////////////////
// COptionsPageAFiles
//////////////////////////////////////////////////////////////////////////////

#include "afiles.h"

class SetsList : public AFILES_LIST
{
};

COptionsPageAFiles::COptionsPageAFiles()
{
	sets = NULL;
	m_bDirty = false;
}

COptionsPageAFiles::~COptionsPageAFiles()
{
	if(sets)
	{
		AFILES_IT i;
		for(i = sets->begin(); i != sets->end(); ++i)
		{
			delete (*i);
		}
		sets->clear();
		delete sets;
	}
}

void COptionsPageAFiles::OnInitialise()
{
	sets = new SetsList;

	AlternateFiles* afiles = AlternateFiles::GetInstance();
	const AFILES_LIST& theSets = afiles->GetSets();
	for(AFILES_CIT i = theSets.begin(); i != theSets.end(); ++i)
	{
		AlternateFileSet* aFS = new AlternateFileSet( *(*i) );
		sets->push_back( aFS );
		
		tstring temp1, temp2;
		aFS->GetSet1String(temp1);
		aFS->GetSet2String(temp2);

		addItem(temp1.c_str(), temp2.c_str(), aFS);
	}
}

void COptionsPageAFiles::OnOK()
{
	if(m_bDirty)
	{
		AlternateFiles* afiles = AlternateFiles::GetInstance();
		afiles->Clear();
		
		// We orphan our AlternateFileSet instances into the AlternateFiles container.
		afiles->SetSets(*sets);
		afiles->Save();
		
		// Make sure we don't delete the orphaned items.
		sets->clear();
	}
}

void COptionsPageAFiles::OnCancel()
{

}

LPCTSTR COptionsPageAFiles::GetTreePosition()
{
	return _T("Files\\Alternate Files");
}

LRESULT COptionsPageAFiles::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_list.Attach(GetDlgItem(IDC_AFILES_LIST));
	CRect rc;
	m_list.SetExtendedListViewStyle(LVS_EX_FULLROWSELECT);
	m_list.GetClientRect(rc);
	m_list.InsertColumn(0, _T("Extensions"), LVCFMT_LEFT, (rc.Width() / 2) - 10, 0);
	m_list.InsertColumn(1, _T("Alternate Extensions"), LVCFMT_LEFT, (rc.Width() / 2) - 10, 0);

	//TODO: Enable/disable the add, remove and edit buttons based on the selection
	return 0;
}

LRESULT COptionsPageAFiles::OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CAFileEditorDialog dlg;
	if(dlg.DoModal() == IDOK)
	{
		tstring set1;
		tstring set2;
		dlg.GetValues(set1, set2);
		
		if(set1 != _T("") && set2 != _T(""))
		{
			AlternateFileSet* pSet = new AlternateFileSet(set1.c_str(), set2.c_str());
			sets->push_back(pSet);

			addItem(set1.c_str(), set2.c_str(), pSet);
		}
		m_bDirty = true;
	}

	return 0;
}

LRESULT COptionsPageAFiles::OnEditClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int i = m_list.GetSelectedIndex();
	if(i == -1)
		return 0;

	AlternateFileSet* set = reinterpret_cast<AlternateFileSet*>( m_list.GetItemData( i ) );

	tstring temp1, temp2;

	set->GetSet1String(temp1);
	set->GetSet2String(temp2);

	CAFileEditorDialog dlg;
	dlg.SetValues(temp1.c_str(), temp2.c_str());

	if(dlg.DoModal() == IDOK)
	{
		dlg.GetValues(temp1, temp2);

		LVITEM lvi;
		lvi.mask = LVIF_TEXT;
		lvi.iItem  = i;
		lvi.iSubItem = 0;
		lvi.pszText = const_cast<LPTSTR>(temp1.c_str());
		m_list.SetItem(&lvi);

		lvi.iSubItem = 1;
		lvi.pszText = const_cast<LPTSTR>(temp2.c_str());
		m_list.SetItem(&lvi);

		set->Set(temp1.c_str(), temp2.c_str());

		m_bDirty = true;
	}

	return 0;
}

LRESULT COptionsPageAFiles::OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int i = m_list.GetSelectedIndex();
	if(i == -1)
		return 0;

	AlternateFileSet* set = reinterpret_cast<AlternateFileSet*>( m_list.GetItemData( i ) );

	sets->remove(set);
    
	delete set;

    m_list.DeleteItem(i);

	m_bDirty = true;

	return 0;
}

void COptionsPageAFiles::addItem(LPCTSTR set1, LPCTSTR set2, AlternateFileSet* lpData)
{
	LVITEM lvi;
	lvi.mask = LVIF_TEXT | LVIF_PARAM;
	lvi.pszText = const_cast<LPTSTR>(set1);
	lvi.lParam = reinterpret_cast<LPARAM>(lpData);
	lvi.iItem = m_list.GetItemCount();
	lvi.iSubItem = 0;

	int index = m_list.InsertItem(&lvi);

	lvi.mask = LVIF_TEXT;
	lvi.pszText = const_cast<LPTSTR>(set2);
	lvi.iItem = index;
	lvi.iSubItem = 1;

	m_list.SetItem(&lvi);
}

LRESULT COptionsPageAFiles::OnListDblClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	BOOL b;
	OnEditClicked(0, 0, 0, b);

	return 0;
}

//////////////////////////////////////////////////////////////////////////////
// COptionsPageFileAssoc
//////////////////////////////////////////////////////////////////////////////

COptionsPageFileAssoc::COptionsPageFileAssoc()
	: m_bDirty(false)
	, m_mode(ModeNone)
	, m_fam()
	, m_bKeyChange(false)
{}

LPCTSTR COptionsPageFileAssoc::GetTreePosition()
{
	return _T("Files\\File Associations");
}

void COptionsPageFileAssoc::OnOK()
{
	if(!m_bCreated)
		return;

	CString ext;
	CString method;
	FileAssoc fa;
	for(int i = 0; i < m_list.GetItemCount(); i++)
	{
		ListItemToFileAssoc(i, fa);
		m_fam.SetAssociation(fa);
	}
	m_fam.UpdateAssociations();

	OPTIONS->Set(PNSK_INTERFACE, _T("CheckAssocsOnStartup"),
		IsDlgButtonChecked(IDC_CHECKONSTARTUP) == BST_CHECKED);
}

void COptionsPageFileAssoc::OnInitialise()
{
	CheckDlgButton(IDC_CHECKONSTARTUP,
		OPTIONS->Get(PNSK_INTERFACE, _T("CheckAssocsOnStartup"), false) ? BST_CHECKED : BST_UNCHECKED);

	const FileAssocManager::FileAssocs& fas = m_fam.GetAssociations();
	for(int i = 0; i < fas.GetSize(); i++)
	{
		const FileAssoc& fa = fas[i];
		m_list.AddItem(i, ColConflict, _T(""));
		m_list.SetItemText(i, ColExtension, fa.GetExtension());
		m_list.SetItemText(i, ColMethod, fa.GetVerbName(true));
		m_list.SetItemText(i, ColTypeName, fa.GetCurrentTypeName());
	}
}

void COptionsPageFileAssoc::OnCancel()
{
}

LRESULT COptionsPageFileAssoc::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_combo.Attach(GetDlgItem(IDC_FILEASSO_METHOD));
	m_combo.AddString(_T("Open"));
	m_combo.AddString(_T("Edit"));
	m_combo.AddString(_T("Edit with PN2"));
	m_combo.SetCurSel(0);

	m_buttonAddEdit.Attach(GetDlgItem(IDC_ADD));
	m_buttonRemove.Attach(GetDlgItem(IDC_REMOVE));

	m_list.Attach(GetDlgItem(IDC_FILEASSO_LIST));
	m_list.SetExtendedListViewStyle(LVS_EX_FULLROWSELECT | LVS_EX_INFOTIP);
	m_list.AddColumn(_T("!"), ColConflict, ColConflict, LVCF_FMT | LVCF_WIDTH | LVCF_TEXT | LVCF_SUBITEM, LVCFMT_CENTER);
	m_list.AddColumn(_T("Extension"), ColExtension, ColExtension);
	m_list.AddColumn(_T("Method"), ColMethod, ColMethod);

	RECT rc;
	::GetWindowRect(GetDlgItem(IDC_FILEASSO_LIST), &rc);

	LVCOLUMN lvc = { 0 };
	lvc.mask = LVCF_FMT | LVCF_WIDTH | LVCF_TEXT | LVCF_SUBITEM;
	lvc.fmt = LVCFMT_LEFT;
	lvc.pszText = _T("Type Name");
	lvc.cx = (rc.right - rc.left) / 2;
	lvc.iSubItem = ColTypeName;
	m_list.InsertColumn(ColTypeName, &lvc);
	//m_list.AddColumn(_T("Type Name"), 2, 2);

	// Prepare to draw conflicts highlighted
	m_boldFont;
	HFONT hFont = GetFont();
	LOGFONT lf = {0};
	GetObject(hFont, sizeof(lf), &lf);
	lf.lfWeight = FW_BOLD;
	m_boldFont.CreateFontIndirect(&lf);

	// Ensure there's (some) contrast between the highlight color and the background
	m_colors[0] = ::GetSysColor(COLOR_WINDOWTEXT);
	COLORREF bkgnd = ::GetSysColor(COLOR_WINDOW);
	COLORREF highlight = RGB(255, 0, 0);
	const int tolerance = 0x7F;
	int r = abs(GetRValue(bkgnd) - GetRValue(highlight));
	int g = abs(GetGValue(bkgnd) - GetGValue(highlight));
	int b = abs(GetBValue(bkgnd) - GetBValue(highlight));
	if(r < tolerance && g < tolerance && b < tolerance)
	{
		//TODO: Should a calculate to get an inverse take place here?
		// But this if body will only be reached when the user has a redish
		// window background and we won't be able to use this signal color
		// then...
		highlight = m_colors[0];
	}

	m_colors[1] = highlight;
	return 0;
}

LRESULT COptionsPageFileAssoc::OnExtensionChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CString curExt;
	GetDlgItemText(IDC_EXTENSION, curExt);
	if(curExt.GetLength() == 0)
	{
		SetMode(ModeNone, false);
	}
	else
	{
		// If the extension exists in the list, select the item and set the
		// EditMode. This case is true when the user just selected an item
		// in the list (may be a performace problem on a very low end computer
		// and a very large list of extensions. For this edge case a separate
		// m_list.GetSelectedIndex() != -1 test would perform better).

		// ListViewCtrl::FindItem does not work here because the extension is
		// in a subitem.
		int extIndex;
		for(extIndex = 0; extIndex < m_list.GetItemCount(); extIndex++)
		{
			CString ext;
			m_list.GetItemText(extIndex, 1, ext);
			if(ext == curExt)
			{
				break;
			}
		}

		if(extIndex < m_list.GetItemCount())
		{
			CString method;
			m_list.GetItemText(extIndex, ColMethod, method);
			m_combo.SelectString(0, method);
			m_bKeyChange = true;
			m_list.SelectItem(extIndex);
			m_bKeyChange = false;
			SetMode(ModeEdit, false);
		}
		else
		{
			SetMode(ModeAdd, false);
		}
	}

	return 0;
}

LRESULT COptionsPageFileAssoc::OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CString ext;
	GetDlgItemText(IDC_EXTENSION, ext);
	CString method;
	m_combo.GetLBText(m_combo.GetCurSel(), method);

	FileAssoc fa(ext, method);
	m_fam.SetAssociation(fa);

	int itemIndex;
	if(m_mode == ModeAdd)
	{
		itemIndex = m_list.GetItemCount();
		m_list.AddItem(itemIndex, ColConflict, _T(""));
		m_list.SetItemText(itemIndex, ColExtension, fa.GetExtension());
		m_list.SetItemText(itemIndex, ColTypeName, fa.GetCurrentTypeName());
	}
	else //if(m_mode == ModeEdit)
	{
		itemIndex = m_list.GetSelectedIndex();
	}

	m_list.SetItemText(itemIndex, ColMethod, method);

	if(fa.HasConflict())
	{
		if(m_conflicts.FindKey(ext) != -1)
		{
			m_conflicts.SetAt(ext, fa.GetCurrentAppName());
		}
		else
		{
			m_conflicts.Add(ext, fa.GetCurrentAppName());
		}
		m_list.SetItemText(itemIndex, ColConflict, _T("!"));
	}
	else if(!fa.IsValid())
	{
		// We can ignore the test for VerbNone in FileAssoc::IsValid() here
		// because the user cannot enter VerbNone.
		CString conflictMsg(_T("An extension may not contain any of these characters: "));
		LPCTSTR invalidChars = FileAssoc::GetInvalidChars();
		for(int i = 0; invalidChars[i] != 0; i++)
		{
			conflictMsg += invalidChars[i];
			conflictMsg += _T(' ');
		}

		m_list.SetItemText(itemIndex, ColConflict, _T("!"));
		m_list.SetItemText(itemIndex, ColExtension, ext);
		m_list.SetItemText(itemIndex, ColTypeName, conflictMsg);
	}
	else
	{
		int i = m_conflicts.FindKey(ext);
		if(i != -1)
		{
			m_conflicts.RemoveAt(i);
		}

		m_list.SetItemText(itemIndex, ColConflict, _T(""));
	}

	m_bDirty = true;
	SetMode(ModeNone);
	return 0;
}

LRESULT COptionsPageFileAssoc::OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int selIndex = m_list.GetSelectedIndex();
	if(selIndex != -1)
	{
		RemoveExtension(selIndex);
	}
	return 0;
}

LRESULT COptionsPageFileAssoc::OnCheckNowClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_fam.UpdateAssociations();
	return 0;
}

LRESULT COptionsPageFileAssoc::OnListItemChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	// The item selected state can change throught various ways:
	// 1. the user selected an item in the list
	// 2. the user modifies an extension he just selected which results in a
	//    deselect
	// 3. the user clicks in the list view but not on an item
	// In the first case, we want to switch to edit mode, in the 2nd to add
	// mode and in the last to none.

	NMLISTVIEW* plv = (LPNMLISTVIEW)pnmh;
	if(/*m_mode != ModeAdd && */plv->uChanged == LVIF_STATE)
	{
		if((plv->uNewState & LVIS_SELECTED) != 0)
		{
			CString ext;
			CString method;
			m_list.GetItemText(plv->iItem, ColExtension, ext);
			m_list.GetItemText(plv->iItem, ColMethod, method);
			//NOTE: setting the extension edit after clearing it in SetMode results in the item being deselected again
			SetMode(ModeEdit, !m_bKeyChange, ext);
			m_combo.SelectString(0, method);
		}
		else
		{
			SetMode(ModeNone);
		}
	}
	return 0;
}

LRESULT COptionsPageFileAssoc::OnListSetFocus(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& /*bHandled*/)
{
	if(m_list.GetSelectedIndex() == -1)
	{
		SetMode(ModeNone);
	}
	return 0;
}

LRESULT COptionsPageFileAssoc::OnListGetInfoTip(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	NMLVGETINFOTIP* plgit = (NMLVGETINFOTIP*)pnmh;
	CString ext;
	m_list.GetItemText(plgit->iItem, ColExtension, ext);
	int i = m_conflicts.FindKey(ext);
	if(i != -1)
	{
		CString msg;
		msg.Format(_T("The selected method for this extension is currently associated with '%s'"),
			m_conflicts.GetValueAt(i));
		::lstrcpyn(plgit->pszText, msg, plgit->cchTextMax);
	}
	return 0;
}

LRESULT COptionsPageFileAssoc::OnListKeyDown(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	NMLVKEYDOWN* plkd = (NMLVKEYDOWN*)pnmh;
	int selIndex = m_list.GetSelectedIndex();
	if(plkd->wVKey == VK_DELETE && selIndex != -1)
	{
		RemoveExtension(selIndex);
	}
	return 0;
}

DWORD COptionsPageFileAssoc::OnPrePaint(int idCtrl, LPNMCUSTOMDRAW lpNMCustomDraw)
{
	DWORD ret = CDRF_DODEFAULT;
	if(idCtrl == IDC_LIST)
	{
		ret = CDRF_NOTIFYITEMDRAW;
	}
	return ret;
}

DWORD COptionsPageFileAssoc::OnItemPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW lpNMCustomDraw)
{
	DWORD ret = CDRF_DODEFAULT;
	CString ext;
	m_list.GetItemText(lpNMCustomDraw->dwItemSpec, ColExtension, ext);
	int i = m_conflicts.FindKey(ext);
	if(i != -1)
	{
		CDCHandle dc(lpNMCustomDraw->hdc);
		dc.SelectFont(m_boldFont);
		ret = CDRF_NEWFONT | CDRF_NOTIFYSUBITEMDRAW;
	}
	return ret;
}

DWORD COptionsPageFileAssoc::OnSubItemPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW lpNMCustomDraw)
{
	NMLVCUSTOMDRAW* lvcd = (NMLVCUSTOMDRAW*)lpNMCustomDraw;
	if(lvcd->iSubItem == ColConflict)
		lvcd->clrText = m_colors[1];
	else
		lvcd->clrText = m_colors[0];
	return CDRF_DODEFAULT;
}

void COptionsPageFileAssoc::SetMode(Mode mode, bool setExt /*= true*/, LPCTSTR ext /*= NULL*/)
{
	if(m_mode == ModeChanging)
		return;

	m_mode = ModeChanging;

	if(setExt)
	{
		SetDlgItemText(IDC_EXTENSION, ext);
	}
	m_buttonAddEdit.SetWindowText(mode == ModeAdd ? _T("&Add") : _T("&Edit"));
	m_buttonAddEdit.EnableWindow(mode != ModeNone);
	m_buttonRemove.EnableWindow(m_list.GetSelectedIndex() > -1);

	if(mode != ModeEdit && m_list.GetSelectedIndex() > -1)
	{
		m_list.SetItemState(m_list.GetSelectedIndex(), 0, LVIS_SELECTED);
	}

	m_mode = mode;
}

void COptionsPageFileAssoc::ListItemToFileAssoc(int index, FileAssoc& fa)
{
	CString ext;
	CString method;
	m_list.GetItemText(index, ColExtension, ext);
	m_list.GetItemText(index, ColMethod, method);

	fa.SetExtensionAndVerb(ext, method);
}

void COptionsPageFileAssoc::RemoveExtension(int index)
{
	FileAssoc fa;
	ListItemToFileAssoc(index, fa);
	m_fam.UnsetAssociation(fa);

	m_list.DeleteItem(index);
}

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
		n = m_list.GetItemCount();

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
}

void COptionsPageFileTypes::OnOK()
{
	if(!m_bCreated)
		return;

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
		return;
	
	clear();
}

LPCTSTR COptionsPageFileTypes::GetTreePosition()
{
	return _T("Files");
}

LRESULT COptionsPageFileTypes::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_list.Attach(GetDlgItem(IDC_LIST));

	m_list.SetExtendedListViewStyle( LVS_EX_FULLROWSELECT, LVS_EX_FULLROWSELECT );

	m_list.InsertColumn(0, _T("Match"), LVCFMT_LEFT, 120, -1);
	m_list.InsertColumn(1, _T("Scheme"), LVCFMT_LEFT, 200, -1);

	return 0;
}

LRESULT COptionsPageFileTypes::OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CFileTypeEditorDialog dlg(m_schemes);
	if(dlg.DoModal(m_hWnd) == IDOK)
	{
		tstring fn;
		tstring scheme;
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
		return 0;

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
		return 0;

	CString ext;
	m_list.GetItemText(i, 0, ext);

	SFTDetails* pDetails = reinterpret_cast<SFTDetails*>( m_list.GetItemData( i ) );

	CFileTypeEditorDialog dlg(m_schemes);
	dlg.SetValues(ext, pDetails->pScheme->GetName());

	if(dlg.DoModal(m_hWnd) == IDOK)
	{
		tstring match;
		tstring scheme;
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