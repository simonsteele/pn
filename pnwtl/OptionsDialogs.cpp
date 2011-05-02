/**
 * @file OptionsDialogs.cpp
 * @brief Dialogs used to edit settings from the Options dialog.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "OptionsDialogs.h"
#include "SchemeConfig.h"
#include "pndialogs.h"
#include "l10n.h"

using namespace boost::xpressive;

//////////////////////////////////////////////////////////////////////////////
// CToolEditorDialog
//////////////////////////////////////////////////////////////////////////////

CToolSettingsPage::CToolSettingsPage(LPCTSTR title) : 
	CPropertyPageImpl<CToolSettingsPage>(title)
{
	m_csName = _T("");
	m_csCommand = _T("");
	m_csFolder = _T("");
	m_csParams = _T("");

	m_iSaveStyle = 0;

	m_wHotKey = 0;

	m_csDisplayTitle = LS(IDS_TOOLS_NEWTOOL);
}

LRESULT CToolSettingsPage::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CenterWindow(GetParent());

	SetWindowText(m_csDisplayTitle);

	m_saveCombo.Attach(GetDlgItem(IDC_TE_SAVECOMBO));
	m_saveCombo.AddString(LS(IDS_TOOLS_SAVE_NONE));
	m_saveCombo.AddString(LS(IDS_TOOLS_SAVE_CURRENTFILE));
	m_saveCombo.AddString(LS(IDS_TOOLS_SAVE_ALLFILES));
	m_saveCombo.AddString(LS(IDS_TOOLS_SAVE_PROJECTGROUP));

	if(m_iSaveStyle == TOOL_SAVEALL)
		m_saveCombo.SetCurSel(2);
	else if(m_iSaveStyle == TOOL_SAVEONE)
		m_saveCombo.SetCurSel(1);
	else if(m_iSaveStyle == TOOL_SAVEPROJECTGROUP)
		m_saveCombo.SetCurSel(3);
	else
		m_saveCombo.SetCurSel(0);

	m_HotKeyCtrl.SubclassWindow(GetDlgItem(IDC_TE_HOTKEY));
	m_HotKeyCtrl.SetHotKey(LOBYTE(m_wHotKey), HIBYTE(m_wHotKey));

	m_paramHelper.SubclassWindow( GetDlgItem(IDC_OPTHELPER_BUTTON) );

	m_VarList.Attach(GetDlgItem(IDC_VAR_LIST));
	m_VarList.InsertColumn(0, LS(IDS_TOOLS_HEADER_TEXT), LVCFMT_LEFT, 105, -1);
	m_VarList.InsertColumn(1, LS(IDS_TOOLS_HEADER_MEANING), LVCFMT_LEFT, 115, -1);
	m_VarList.InsertColumn(2, LS(IDS_TOOLS_HEADER_EXAMPLE), LVCFMT_LEFT, 100, -1);

	CString str;
	str.LoadString(IDS_TOOLS_VARSTRINGS);

	tstring delimiters = _T(",");
	tstring varstr = str;
	std::vector<tstring> toks;

	StringTokenise(varstr, toks, delimiters);

	int index = 0;
	for(std::vector<tstring>::iterator i = toks.begin(); i != toks.end(); ++i)
	{
		LVITEM lvi = {0};
		lvi.mask = LVIF_TEXT;
		lvi.iItem = index++;
		lvi.pszText = (LPTSTR)(*i).c_str();
		m_VarList.InsertItem(&lvi);
		
		++i;

		lvi.iSubItem = 1;
		lvi.pszText = (LPTSTR)(*i).c_str();
		m_VarList.SetItem(&lvi);

		++i;

		lvi.iSubItem = 2;
		lvi.pszText = (LPTSTR)(*i).c_str();
		m_VarList.SetItem(&lvi);
	}

	DoDataExchange();

	return 0;
}

BOOL CToolSettingsPage::OnApply()
{
	DoDataExchange(TRUE);
	
	m_wHotKey = (WORD)m_HotKeyCtrl.GetHotKey();

	int saveSel = m_saveCombo.GetCurSel();

	switch (saveSel)
	{
		case 3:  m_iSaveStyle = TOOL_SAVEPROJECTGROUP; break;
		case 2:  m_iSaveStyle = TOOL_SAVEALL; break;
		case 1:  m_iSaveStyle = TOOL_SAVEONE; break;
		default: m_iSaveStyle = 0; break;
	}

	return TRUE;
}

LRESULT CToolSettingsPage::OnBrowseDir(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{	
	DoDataExchange(TRUE);

	CString str;
	str.LoadString(IDS_TOOLS_SELWORKINGFOLDER);

	LPCTSTR pInit = ( (m_csFolder.Find(_T('%')) == -1) ? (LPCTSTR)m_csFolder : NULL );
	CPNFolderDialog dlg(NULL, pInit, str);
	
	if( dlg.DoModal() == IDOK )
	{
		m_csFolder = dlg.GetFolderPath();
		DoDataExchange();
	}

	return 0;
}

LRESULT CToolSettingsPage::OnBrowseCommand(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	DoDataExchange(TRUE);

	LPCTSTR pFN = ( (m_csCommand.Find(_T('%')) == -1) ? (LPCTSTR)m_csCommand : NULL );
	tstring filter = LS(IDS_TOOLS_TOOLFILEFILTER);
	CAutoOpenDialog fd(filter.c_str());
	fd.SetTitle(LS(IDS_TOOLS_COMMANDTITLE));

	if (pFN != NULL)
	{
		CFileName fn(pFN);
		fd.SetInitialPath(fn.GetDirectoryName().c_str());
	}

	if( fd.DoModal() == IDOK )
	{
		m_csCommand = fd.GetSingleFileName();
		DoDataExchange();
	}

	return 0;
}

LRESULT CToolSettingsPage::OnClearShortcut(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_HotKeyCtrl.SetHotKey(0,0);

	return 0;
}

LRESULT CToolSettingsPage::OnParamHelper(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{

	return 0;
}

void CToolSettingsPage::GetValues(ToolDefinition* pDefinition)
{
	pDefinition->Name			= m_csName;
	pDefinition->Command		= m_csCommand;
	pDefinition->Folder			= m_csFolder;
	pDefinition->Params			= m_csParams;
	pDefinition->Shortcut		= m_wHotKey;
	
	pDefinition->SetFlags(
		(m_bFilter	? TOOL_ISFILTER : 0) |
		m_iSaveStyle );
}

void CToolSettingsPage::SetValues(ToolDefinition* pDefinition)
{
	m_csName		= pDefinition->Name.c_str();
	m_csCommand		= pDefinition->Command.c_str();
	m_csFolder		= pDefinition->Folder.c_str();
	m_csParams		= pDefinition->Params.c_str();
	m_wHotKey		= pDefinition->Shortcut;
	m_bFilter		= pDefinition->IsFilter();

	m_iSaveStyle = pDefinition->GetFlags() & (TOOL_SAVEALL | TOOL_SAVEONE | TOOL_SAVEPROJECTGROUP);
}

void CToolSettingsPage::SetTitle(LPCTSTR title)
{
	m_csDisplayTitle = title;
}

//////////////////////////////////////////////////////////////////////////////
// CToolConsoleIOPage
//////////////////////////////////////////////////////////////////////////////

CToolConsoleIOPage::CToolConsoleIOPage(LPCTSTR title) : 
	CPropertyPageImpl<CToolConsoleIOPage>(title)
{
	m_csCustomPattern = _T("");

	m_bCapture = true;
	m_bClear = true;
	m_bGlobal = true;
	m_bTextFilter = false;
	m_bWantStdIn = false;
	m_iBuiltIn = 0;
}

BOOL CToolConsoleIOPage::OnApply()
{
	DoDataExchange(TRUE);
	m_bGlobal = (m_outputcombo.GetCurSel() == 0);
	m_bTextFilter = (m_outputcombo.GetCurSel() == 2);
	
	return TRUE;
}

void CToolConsoleIOPage::GetValues(ToolDefinition* pDefinition)
{
	pDefinition->SetFlags( pDefinition->GetFlags() | 
		(m_bCapture	  ? TOOL_CAPTURE	: 0) |
		(m_bGlobal	  ? TOOL_GLOBALOUTPUT : 0) |
		(m_bClear	  ? TOOL_CLEAROUTPUT : 0) |
		(m_bWantStdIn ? TOOL_WANTSTDIN : 0) |
		(m_bTextFilter? TOOL_ISTEXTFILTER : 0) |
		(m_iBuiltIn   ? TOOL_CUSTOMPARSER : 0) );

	if(m_iBuiltIn)
	{
		CT2CA pattern(m_csCustomPattern);
		pDefinition->CustomParsePattern = (const char*)pattern;
	}
}

void CToolConsoleIOPage::SetValues(ToolDefinition* pDefinition)
{
	CA2CT parsePattern(pDefinition->CustomParsePattern.c_str());

	m_bCapture			= pDefinition->CaptureOutput();
	m_bGlobal			= pDefinition->GlobalOutput();
	m_bClear			= pDefinition->ShouldClearOutput();
	m_iBuiltIn			= pDefinition->UseCustomParser() ? 1 : 0;
	m_csCustomPattern	= parsePattern;
	m_bWantStdIn		= pDefinition->WantStdIn();
	m_bTextFilter		= pDefinition->IsTextFilter();
}

LRESULT CToolConsoleIOPage::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_outputcombo.Attach(GetDlgItem(IDC_TE_OUTPUTCOMBO));
	
	m_outputcombo.AddString(LS(IDS_GLOBALOUTPUT));
	m_outputcombo.AddString(LS(IDS_INDIVIDUALOUTPUT));
	m_outputcombo.AddString(LS(IDS_FILTERSELECTION));

	int sel = 1;
	if(m_bGlobal)
		sel = 0;
	else if(m_bTextFilter)
		sel = 2;
	m_outputcombo.SetCurSel(sel);

	m_outputcombo.EnableWindow(m_bCapture);

	CRect rcScintilla;
	::GetWindowRect(GetDlgItem(IDC_PLACEHOLDER), rcScintilla);
	ScreenToClient(rcScintilla);
	m_scintilla.Create(m_hWnd, rcScintilla, _T("Keywords"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_TABSTOP);
	::SetWindowPos(m_scintilla, GetDlgItem(IDC_PLACEHOLDER), 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
	m_scintilla.SetWrapMode(SC_WRAP_WORD);
	m_scintilla.AssignCmdKey(SCK_HOME, SCI_HOMEDISPLAY);
	m_scintilla.AssignCmdKey(SCK_END, SCI_LINEENDDISPLAY);
	
	// Stop scintilla from capturing the escape and tab keys...
	m_scintilla.ClearCmdKey(SCK_ESCAPE);
	m_scintilla.ClearCmdKey(SCK_TAB);

	DoDataExchange();

	BOOL b;
	OnTextChange(0, 0, NULL, b);

	enableButtons();

	return 0;
}

LRESULT CToolConsoleIOPage::OnHandleHSClick(UINT /*uMsg*/, WPARAM style, LPARAM position, BOOL& /*bHandled*/)
{
	Scintilla::TextRange tr;
				
	m_scintilla.ExtendStyleRange(position, style, &tr);
	
	std::string buf;
	buf.resize(tr.chrg.cpMax - tr.chrg.cpMin + 1);
	//char* buf = new char[tr.chrg.cpMax - tr.chrg.cpMin + 1];
	tr.lpstrText = &buf[0];
	m_scintilla.GetTextRange(&tr);
	buf.resize(tr.chrg.cpMax - tr.chrg.cpMin);

	sregex* pRE = m_scintilla.GetRE();

	smatch match;
	if( pRE && regex_search( buf, match, *pRE ))
	{
		std::string filename;
		std::string linestr;
		std::string colstr;

		// Extract the named matches from the RE, noting if there was a line or column.
		bool bFile = safe_get_submatch(match, filename, "f");
		bool bLine = safe_get_submatch(match, linestr, "l");
		bool bCol = safe_get_submatch(match, colstr, "c");
        
		std::string display = "";
		if(bFile)
		{
			display += "f: ";
			display += filename;
		}

		if(bLine)
		{
			if(display.length())
				display += ", ";
			display += "l: ";
			display += linestr;
		}

		if(bCol)
		{
			if(display.length())
				display += ", ";
			display += "c: ";
			display += colstr;
		}

		CA2CT dispconv(display.c_str());
		GetDlgItem(IDC_TE_CLICKRESULTSSTATIC).SetWindowText(dispconv);
	}
	else
	{
		GetDlgItem(IDC_TE_CLICKRESULTSSTATIC).SetWindowText(_T("Error: no match"));
	}

	return 0;
}

LRESULT CToolConsoleIOPage::OnCaptureChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	enableButtons();

	return 0;
}

LRESULT CToolConsoleIOPage::OnAboutBuiltin(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CString str;
	str.LoadString(IDS_OUTPUTSUPPORT);
	MessageBox(	str, _T("Information"),MB_OK | MB_ICONINFORMATION);

	return 0;
}

LRESULT CToolConsoleIOPage::OnWindowStateChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	enableButtons();

	return 0;
}

LRESULT CToolConsoleIOPage::OnTextChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CWindowText wt(GetDlgItem(IDC_TE_CUSTOMTEXT));

	m_scintilla.SetRE(wt.GetA().c_str());
	
	return 0;	
}

void CToolConsoleIOPage::enableButtons()
{
	CButton capture(GetDlgItem(IDC_TE_CAPTURECHECK));
	CButton textFilter(GetDlgItem(IDC_TE_TEXTFILTERCHECK));
	BOOL bCapture = capture.GetCheck();
	BOOL bTextFilter = m_outputcombo.GetCurSel() == 2;

	// These items are always enabled if we are capturing output...
	m_outputcombo.EnableWindow(bCapture);
	::EnableWindow(GetDlgItem(IDC_TE_CLEARCHECK), bCapture);

	::EnableWindow(GetDlgItem(IDC_TE_BUILTIN), bCapture && !bTextFilter);
	::EnableWindow(GetDlgItem(IDC_TE_ABOUTBUILTIN), bCapture && !bTextFilter);

	CButton customParse(GetDlgItem(IDC_TE_CUSTOMPARSE));
	customParse.EnableWindow(bCapture && !bTextFilter);
	BOOL bCustom = customParse.GetCheck() && customParse.IsWindowEnabled();
	
	::EnableWindow(GetDlgItem(IDC_TE_CUSTOMTEXT), bCustom && bCapture);

	m_scintilla.EnableWindow( bCustom && bCapture );
	GetDlgItem( IDC_TE_OUTPUTGROUP ).EnableWindow( bCustom && bCapture );
}

//////////////////////////////////////////////////////////////////////////////
// CSmartStartEditorDialog
//////////////////////////////////////////////////////////////////////////////

CSmartStartEditorDialog::CSmartStartEditorDialog(SchemeConfigParser* pSchemes)
{
	m_pSchemes = pSchemes;
}

LRESULT CSmartStartEditorDialog::OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CComboBox combo(GetDlgItem(IDC_SCHEMECOMBO));
	SchemeDetails* pScheme = reinterpret_cast<SchemeDetails*>( combo.GetItemData(combo.GetCurSel()) );
	if(pScheme)
	{
		m_schemeName = pScheme->Name.c_str();
	}
	else
	{
		m_schemeName = "default";
	}

	CWindowText wt(GetDlgItem(IDC_PHRASEEDIT));
	m_startPhrase = wt.GetA();
	
	EndDialog(wID);

	return 0;
}

LRESULT CSmartStartEditorDialog::OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	EndDialog(wID);

	return 0;
}

LRESULT CSmartStartEditorDialog::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CenterWindow(GetParent());

	CComboBox combo(GetDlgItem(IDC_SCHEMECOMBO));
	
	int selindex = 0;
	int index = combo.AddString(LS(IDS_DEFAULTSCHEME));
	combo.SetItemDataPtr(index, NULL);
	for(SchemeDetailsList::const_iterator i = m_pSchemes->GetSchemes().begin(); i != m_pSchemes->GetSchemes().end(); ++i)
	{
		index = combo.AddString((*i)->Title.c_str());
		if((*i)->Name == m_schemeName)
			selindex = index;
		combo.SetItemDataPtr(index, (*i));
	}

	combo.SetCurSel(selindex);

	CA2CT startPhrase(m_startPhrase.c_str());
	CWindow(GetDlgItem(IDC_PHRASEEDIT)).SetWindowText(startPhrase);

	return 0;
}

void CSmartStartEditorDialog::GetValues(std::string& startPhrase, std::string& schemeName)
{
	startPhrase = m_startPhrase;
	schemeName = m_schemeName;
}

void CSmartStartEditorDialog::SetValues(LPCSTR startPhrase, LPCSTR schemeName)
{
	m_startPhrase = startPhrase;
	m_schemeName = schemeName;
}

//////////////////////////////////////////////////////////////////////////////
// CAFileEditorDialog
//////////////////////////////////////////////////////////////////////////////

LRESULT CAFileEditorDialog::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CenterWindow( GetParent() );

	CEdit edt = GetDlgItem( IDC_AFILE_STARTTEXT );
	edt.SetWindowText( setFrom.c_str() );
	
	edt = GetDlgItem( IDC_AFILE_OTHERTEXT );
	edt.SetWindowText( setTo.c_str() );

	return 0;
}


LRESULT CAFileEditorDialog::OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CWindowText wtFrom( GetDlgItem( IDC_AFILE_STARTTEXT ) );
	if((LPCTSTR)wtFrom != NULL)
		setFrom = (LPCTSTR)wtFrom;

	CWindowText wtTo( GetDlgItem( IDC_AFILE_OTHERTEXT ) );
	if((LPCTSTR)wtTo != NULL)
		setTo = (LPCTSTR)wtTo;

	EndDialog(wID);

	return 0;
}

LRESULT CAFileEditorDialog::OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	EndDialog(wID);

	return 0;
}

void CAFileEditorDialog::GetValues(tstring& set1, tstring& set2)
{
	set1 = setFrom;
	set2 = setTo;
}

void CAFileEditorDialog::SetValues(LPCTSTR set1, LPCTSTR set2)
{
	setFrom = set1;
	setTo = set2;
}

//////////////////////////////////////////////////////////////////////////////
// CFileTypeEditorDialog
//////////////////////////////////////////////////////////////////////////////

CFileTypeEditorDialog::CFileTypeEditorDialog(SchemeConfigParser* schemes)
{
	m_schemes = schemes;
}

void CFileTypeEditorDialog::GetValues(tstring& match, std::string& scheme)
{
	match = m_match;
	scheme = m_sel;
}

void CFileTypeEditorDialog::SetValues(LPCTSTR match, LPCSTR scheme)
{
	m_match = match;
	m_sel = scheme;
}

LRESULT CFileTypeEditorDialog::OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CWindowText wt(GetDlgItem(IDC_FILETYPE_MATCH));
	if(wt == NULL || _tcslen(wt) == 0)
		return 0;

	m_match = (LPCTSTR)wt;
	m_sel = m_combo.GetItemScheme( m_combo.GetCurSel() )->Name;

	EndDialog(wID);

	return 0;
}

LRESULT CFileTypeEditorDialog::OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	EndDialog(wID);

	return 0;
}

LRESULT CFileTypeEditorDialog::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_combo.Attach(GetDlgItem(IDC_SCHEME_COMBO));
	
	m_combo.Load(m_schemes, m_sel.size() ? m_sel.c_str() : NULL, true, false);

	GetDlgItem(IDC_FILETYPE_MATCH).SetWindowText(m_match.c_str());

	CenterWindow(GetParent());

	return 0;
}
