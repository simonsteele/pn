/**
 * @file optionspages.cpp
 * @brief Options Dialog Pages (1) for Programmer's Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
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
	if (sel != -1)
	{
		DWORD langCode = langCombo.GetItemData(sel);
		std::map<int, tstring>::const_iterator i = m_lcid_map.find(langCode);
		if (langCode != 2057 && i != m_lcid_map.end())
		{
			OPTIONS->Set(PNSK_INTERFACE, _T("Language"), (*i).second.c_str());
		}
		else
		{
			OPTIONS->Set(PNSK_INTERFACE, _T("Language"), _T(""));
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
	
	tstring propertiesTitle(LS(IDS_HDR_TOOLS_PROPERTIES));
	tstring consoleIoTitle(LS(IDS_HDR_TOOLS_CONSOLEIO));
	CToolSettingsPage toolPage(propertiesTitle.c_str());
	CToolConsoleIOPage consolePage(consoleIoTitle.c_str());

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