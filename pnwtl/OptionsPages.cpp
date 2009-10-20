/**
 * @file optionspages.cpp
 * @brief Options Dialog Pages (1) for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "OptionsPages.h"
#include "OptionsDialogs.h"
#include "toolsmanager.h"
#include "include/filefinder.h"
#include "version.h"

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
		if(cb.GetItemData(i) == (DWORD_PTR)m_SaveFormat)
		{
			cb.SetCurSel(i);
			break;
		}
	}

	cb.Detach();
	cb.Attach(GetDlgItem(IDC_OPT_CPCOMBO));
	for(int i = 0; i < cb.GetCount(); i++)
	{
		if(cb.GetItemData(i) == (DWORD_PTR)m_CodePage)
		{
			cb.SetCurSel(i);
			break;
		}
	}

	cb.Detach();
	cb.Attach(GetDlgItem(IDC_OPT_CSCOMBO));
	for(int i = 0; i < cb.GetCount(); i++)
	{
		if(cb.GetItemData(i) == (DWORD_PTR)m_CharSet)
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

tstring COptionsPageConf::GetTreePosition()
{
	return MAKE_OPTIONSTREEPATH(IDS_OPTGROUP_GENERAL, IDS_OPTPAGE_CONFIRMATIONS);
}

LRESULT COptionsPageConf::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_settingsHeader.SubclassWindow(GetDlgItem(IDC_SETTINGS_STATIC));

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

	// Other UI
	OPTIONS->Set(PNSK_INTERFACE, _T("MiniToolbar"), m_bShowEditorToolbar == TRUE);
	OPTIONS->Set(PNSK_INTERFACE, _T("Commandbar"), m_bEnableCmdBar == TRUE);

	CComboBox langCombo(GetDlgItem(IDC_LANGUAGECOMBO));
	int sel = langCombo.GetCurSel();
	if (sel != 0)
	{
		std::map<int, tstring>::const_iterator i = m_lcid_map.find(langCombo.GetItemData(sel));
		if (i != m_lcid_map.end())
		{
			OPTIONS->Set(PNSK_INTERFACE, _T("Language"), (*i).second.c_str());
		}
	}
	else
	{
		OPTIONS->Set(PNSK_INTERFACE, _T("Language"), _T(""));
	}

}

void COptionsPageDialogs::OnInitialise()
{
	// File Dialogs
	m_bOpenCurFileDir = OPTIONS->Get(PNSK_INTERFACE, _T("OpenInCurrentDir"), true);

	// Find Dialog
	m_bCloseFindNext = !OPTIONS->Get(PNSK_INTERFACE, _T("FindStaysOpen"), false);
	m_bFindAlpha = OPTIONS->GetCached(Options::OFindAlphaEnabled);

	// Other UI
	m_bShowEditorToolbar = OPTIONS->Get(PNSK_INTERFACE, _T("MiniToolbar"), true);
	m_bEnableCmdBar = OPTIONS->Get(PNSK_INTERFACE, _T("Commandbar"), false);

	DoDataExchange();
}

tstring COptionsPageDialogs::GetTreePosition()
{
	return MAKE_OPTIONSTREEPATH(IDS_OPTGROUP_GENERAL, IDS_OPTPAGE_INTERFACE);
}

LRESULT COptionsPageDialogs::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	// Headers:
	m_languageHeader.SubclassWindow(GetDlgItem(IDC_LANGUAGE_STATIC));
	m_dialogsHeader.SubclassWindow(GetDlgItem(IDC_DIALOGS_STATIC));
	m_findHeader.SubclassWindow(GetDlgItem(IDC_FIND_STATIC));
	m_settingsHeader.SubclassWindow(GetDlgItem(IDC_SETTINGS_STATIC));

	// Languages:
	CComboBox langCombo(GetDlgItem(IDC_LANGUAGECOMBO));
	langCombo.AddString(_T("English"));
	langCombo.SetCurSel(0);
	
	tstring currentSetting = OPTIONS->Get(PNSK_INTERFACE, _T("Language"), _T(""));

	FileFinderList finder;
	tstring pnpath;
	OPTIONS->GetPNPath(pnpath);
	const std::list<tstring>& languageDlls = finder.GetFiles(pnpath.c_str(), _T("pnlang*.dll"), false);

	boost::xpressive::tsregex re = boost::xpressive::tsregex::compile(L"^pnlang_(?P<lcid>[0-9]+)_(?P<langcode>[a-zA-Z]+-[a-zA-Z]+)_(?P<ver>([0-9]+\\.){3}[0-9]+).dll$");
	
	BOOST_FOREACH(const tstring& langdll, languageDlls)
	{
		boost::xpressive::tsmatch match;
		if (boost::xpressive::regex_match(langdll, match, re))
		{
			tstring lcidstr(match[_T("lcid")]);
			tstring langcodestr(match[_T("langcode")]);
			tstring ver(match[_T("ver")]);

			if (ver != PN_VERSTRING_T)
			{
				continue;
			}

			int lcid = _ttoi(lcidstr.c_str());

			TCHAR buf[80];
			tstring friendlyname;
			if (::GetLocaleInfo(lcid, LOCALE_SNATIVELANGNAME, buf, (sizeof(buf)/sizeof(TCHAR)) - 1) != 0)
			{
				friendlyname = buf;
				friendlyname += L" (";
				friendlyname += langcodestr;
				friendlyname += L")";
			}
			else
			{
				friendlyname = langcodestr;
			}
			
			int index = langCombo.AddString(friendlyname.c_str());
			langCombo.SetItemData(index, lcid);

			tstring languageIdString(lcidstr + L"_" + langcodestr);
			m_lcid_map.insert(std::map<int, tstring>::value_type(lcid, languageIdString));

			if (languageIdString == currentSetting)
			{
				langCombo.SetCurSel(index);
			}
		}
	}

	return 0;
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

LRESULT COptionsPageSchemes::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CWindow label;
	CSize s;
	CRect rc;

	label.Attach(GetDlgItem(IDC_SCHEMELABEL));
	
	CDC dc(label.GetDC());
	CWindowText hdrText(label.m_hWnd);
	dc.GetTextExtent(hdrText, -1, &s);
	
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

	tstring schemes(LS(IDS_HDR_STYLETAB_STYLES));
	tstring keywords(LS(IDS_HDR_STYLETAB_KEYWORDS));
	tstring misc(LS(IDS_HDR_STYLETAB_MOREOPTIONS));

	m_stylestab.SetTitle(schemes.c_str());
	m_keywordstab.SetTitle(keywords.c_str());
	m_misctab.SetTitle(misc.c_str());
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

tstring COptionsPageSchemes::GetTreePosition()
{
	return MAKE_OPTIONSTREEPATH(IDS_OPTGROUP_STYLES, IDS_OPTPAGE_ADVANCED);
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

COptionsPageTools::COptionsPageTools(ToolsManager* pToolManager)
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
	CWindowText hdrtext(GetDlgItem(IDC_SCHEMELABEL));
	initControls((LPCTSTR)hdrtext);
	
	return 0;
}

void COptionsPageTools::OnInitialise()
{
	m_combo.AddScheme(LS(IDS_TOOLS_GLOBALTOOLS), NULL);
	
	m_combo.Load(m_pSchemes, NULL, false, false);
	
	if(m_combo.GetCount() > 0)
	{
		Update();
	}
}

void COptionsPageTools::OnOK()
{
	UpdateIndexes();
}

void COptionsPageTools::UpdateIndexes()
{
	// We re-number the tools here so that they can be displayed properly in the menu.
	if (m_pCurrent != NULL)
	{
		int itemCount = m_list.GetItemCount();
		for (int i = 0; i < itemCount; i++)
		{
			ToolDefinition* pDef = reinterpret_cast<ToolDefinition*>(m_list.GetItemData(i));
			if (pDef)
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
	tstring title = (in != NULL) ? LS(IDS_HDR_TOOLS_EDITTOOL) : LS(IDS_HDR_TOOLS_NEWTOOL);
	CPropertySheet sheet(title.c_str(), 0, m_hWnd);
	sheet.m_psh.dwFlags |= (PSH_NOAPPLYNOW | PSH_PROPTITLE | PSH_USEICONID);
	sheet.m_psh.pszIcon = MAKEINTRESOURCE(IDR_MDICHILD);
	
	CToolSettingsPage toolPage(LS(IDS_HDR_TOOLS_PROPERTIES));
	CToolConsoleIOPage consolePage(LS(IDS_HDR_TOOLS_CONSOLEIO));

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

tstring COptionsPageTools::GetTreePosition()
{
	return L10N::StringLoader::Get(IDS_OPTGROUP_TOOLS);
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

void COptionsPageTools::initControls(LPCTSTR itemTitle)
{
	CWindow label;
	CSize s;
	CRect rc;

	label.Attach(GetDlgItem(IDC_SCHEMELABEL));
	label.SetWindowText(itemTitle);
	
	CDC dc(label.GetDC());
	dc.GetTextExtent(itemTitle, -1, &s);
	
	label.GetWindowRect(rc);
	ScreenToClient(rc);
	rc.right = rc.left + s.cx;
	label.SetWindowPos(HWND_TOP, &rc, 0);

	getCombo()->Attach(GetDlgItem(IDC_SCHEMECOMBO));

	m_list.Attach(GetDlgItem(IDC_LIST));
	m_list.SetExtendedListViewStyle(m_list.GetExtendedListViewStyle() | LVS_EX_FULLROWSELECT);
	m_list.GetClientRect(rc);
	m_list.InsertColumn(0, LS(IDS_HDR_TOOLS_NAME), LVCFMT_LEFT, 130, 0);
	m_list.InsertColumn(1, LS(IDS_HDR_TOOLS_COMMAND), LVCFMT_LEFT, rc.Width() - 130 - 100 - 20, 0);
	m_list.InsertColumn(2, LS(IDS_HDR_TOOLS_PARAMS), LVCFMT_LEFT, 100, 0);

	m_btnMoveUp.SetDirection(CArrowButton::abdUp);
	m_btnMoveUp.SubclassWindow(GetDlgItem(IDC_TOOLS_MOVEUPBUTTON));
	m_btnMoveDown.SubclassWindow(GetDlgItem(IDC_TOOLS_MOVEDOWNBUTTON));
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

tstring COptionsPageProjectTools::GetTreePosition()
{
	return MAKE_OPTIONSTREEPATH(IDS_OPTGROUP_TOOLS, IDS_OPTPAGE_PROJECTTOOLS);
}

LRESULT COptionsPageProjectTools::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	tstring title(LS(IDS_HDR_PROJECTTOOLS_TEMPLATE));
	initControls(title.c_str());

	int index = m_combo.AddString(LS(IDS_PROJECTTOOLS_ALLPROJECTS));
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

tstring COptionsPageAFiles::GetTreePosition()
{
	return MAKE_OPTIONSTREEPATH(IDS_OPTGROUP_FILES, IDS_OPTPAGE_ALTERNATEFILES);
}

LRESULT COptionsPageAFiles::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	// Header:
	m_settingsHeader.SubclassWindow(GetDlgItem(IDC_SETTINGS_STATIC));

	// Controls:
	m_list.Attach(GetDlgItem(IDC_AFILES_LIST));
	CRect rc;
	m_list.SetExtendedListViewStyle(LVS_EX_FULLROWSELECT);
	m_list.GetClientRect(rc);
	m_list.InsertColumn(0, LS(IDS_HDR_AFILES_EXTENSIONS), LVCFMT_LEFT, (rc.Width() / 2) - 10, 0);
	m_list.InsertColumn(1, LS(IDS_HDR_AFILES_ALTEXTS), LVCFMT_LEFT, (rc.Width() / 2) - 10, 0);

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

tstring COptionsPageFileTypes::GetTreePosition()
{
	return L10N::StringLoader::Get(IDS_OPTGROUP_FILES);
}

LRESULT COptionsPageFileTypes::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	// Header:
	m_settingsHeader.SubclassWindow(GetDlgItem(IDC_SETTINGS_STATIC));

	// Controls:
	m_list.Attach(GetDlgItem(IDC_LIST));

	m_list.SetExtendedListViewStyle( LVS_EX_FULLROWSELECT, LVS_EX_FULLROWSELECT );

	m_list.InsertColumn(0, LS(IDS_HDR_FILETYPES_MATCH), LVCFMT_LEFT, 120, -1);
	m_list.InsertColumn(1, LS(IDS_HDR_FILETYPES_SCHEME), LVCFMT_LEFT, 200, -1);

	return 0;
}

LRESULT COptionsPageFileTypes::OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CFileTypeEditorDialog dlg(m_schemes);
	if(dlg.DoModal(m_hWnd) == IDOK)
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