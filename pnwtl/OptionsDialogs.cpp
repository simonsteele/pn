/**
 * @file OptionsDialogs.cpp
 * @brief Dialogs used to edit settings from the Options dialog.
 * @author Simon Steele
 * @note Copyright (c) 2002-2005 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "OptionsDialogs.h"
#include "SchemeConfig.h"
#include "pndialogs.h"
#include "include/pcreplus.h"

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

	m_csDisplayTitle = _T("New Tool");
}

const LPCTSTR toolvarstrs = _T("%f,File Name,mainfrm.cpp,%n,File Name (no ext),mainfrm,%l,Current Line Number,232,%?,Ask for parameters,(?),%p,Current Project File,pn.pnproj,%d,Path of File,c:\\source\\pn\\test\\,%c,Column,12,%%,Percent Symbol,%,%w,Current word,cheese,%g,Project Group File,pn2.ppg,$(ProjectPath),Path of Project,c:\\source\\pn2\\,$(ProjectGroupPath),Path of Project Group,c:\\source\\pn2\\");

LRESULT CToolSettingsPage::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CenterWindow(GetParent());

	SetWindowText(m_csDisplayTitle);

	m_saveCombo.Attach(GetDlgItem(IDC_TE_SAVECOMBO));
	m_saveCombo.AddString(_T("None"));
	m_saveCombo.AddString(_T("Current File"));
	m_saveCombo.AddString(_T("All Files"));

	if(m_iSaveStyle == TOOL_SAVEALL)
		m_saveCombo.SetCurSel(2);
	else if(m_iSaveStyle == TOOL_SAVEONE)
		m_saveCombo.SetCurSel(1);
	else
		m_saveCombo.SetCurSel(0);

	m_HotKeyCtrl = GetDlgItem(IDC_TE_HOTKEY);
	m_HotKeyCtrl.SetHotKey(LOBYTE(m_wHotKey), HIBYTE(m_wHotKey));

	m_paramHelper.SubclassWindow( GetDlgItem(IDC_OPTHELPER_BUTTON) );

	m_VarList.Attach(GetDlgItem(IDC_VAR_LIST));
	m_VarList.InsertColumn(0, _T("Text"), LVCFMT_LEFT, 105, -1);
	m_VarList.InsertColumn(1, _T("Meaning"), LVCFMT_LEFT, 115, -1);
	m_VarList.InsertColumn(2, _T("Example"), LVCFMT_LEFT, 100, -1);

	tstring delimiters = _T(",");
	tstring varstr = toolvarstrs;
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
	m_iSaveStyle = (saveSel == 2 ? TOOL_SAVEALL : (saveSel == 1 ? TOOL_SAVEONE : 0));

	return TRUE;
}

LRESULT CToolSettingsPage::OnBrowseDir(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{	
	DoDataExchange(TRUE);

	LPCTSTR pInit = ( (m_csFolder.Find(_T('%')) == -1) ? (LPCTSTR)m_csFolder : NULL );
	CPNFolderDialog dlg(NULL, pInit, _T("Select the working folder for the tool:"));
	
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
	CFileDialog fd(true, _T("exe"), pFN, OFN_HIDEREADONLY, _T("Executable Files (exe, com, bat, vbs...)\0*.exe;*.com;*.bat;*.vbs;*.cmd\0All Files (*.*)\0*.*\0"), NULL);
	if( fd.DoModal() )
	{
		m_csCommand = fd.m_ofn.lpstrFile;
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
	
	pDefinition->iFlags = 
		(m_bFilter	? TOOL_ISFILTER : 0) |
		m_iSaveStyle;
}

void CToolSettingsPage::SetValues(ToolDefinition* pDefinition)
{
	m_csName		= pDefinition->Name.c_str();
	m_csCommand		= pDefinition->Command.c_str();
	m_csFolder		= pDefinition->Folder.c_str();
	m_csParams		= pDefinition->Params.c_str();
	m_wHotKey		= pDefinition->Shortcut;
	m_bFilter		= pDefinition->IsFilter();

	m_iSaveStyle = pDefinition->iFlags & (TOOL_SAVEALL | TOOL_SAVEONE);
}

void CToolSettingsPage::SetTitle(LPCTSTR title)
{
	m_csDisplayTitle = title;
}

//////////////////////////////////////////////////////////////////////////////
// CToolConsoleIOPage
//////////////////////////////////////////////////////////////////////////////

CToolConsoleIOPage::CToolConsoleIOPage(LPCTSTR title) : 
	CPropertyPageImpl<CToolConsoleIOPage>(title)//,
	//m_infolabel2(_T("Pattern Symbols:"), IDS_PATTERNFORMATSTRINGS)
{
	m_csCustomPattern = _T("");

	m_bCapture = true;
	m_bClear = true;
	m_bGlobal = true;
	m_iBuiltIn = 0;
}

BOOL CToolConsoleIOPage::OnApply()
{
	DoDataExchange(TRUE);
	m_bGlobal = (m_outputcombo.GetCurSel() == 0);
	
	return TRUE;
}

void CToolConsoleIOPage::GetValues(ToolDefinition* pDefinition)
{
	pDefinition->iFlags |= 
		(m_bCapture	? TOOL_CAPTURE	: 0) |
		(m_bGlobal	? TOOL_GLOBALOUTPUT : 0) |
		(m_bClear	? TOOL_CLEAROUTPUT : 0) |
		(m_iBuiltIn * TOOL_CUSTOMPARSER);

	if(m_iBuiltIn)
	{
		pDefinition->CustomParsePattern = m_csCustomPattern;
	}
}

void CToolConsoleIOPage::SetValues(ToolDefinition* pDefinition)
{
	m_bCapture			= pDefinition->CaptureOutput();
	m_bGlobal			= pDefinition->GlobalOutput();
	m_bClear			= pDefinition->ShouldClearOutput();
	m_iBuiltIn			= pDefinition->UseCustomParser() ? 1 : 0;
	m_csCustomPattern	= pDefinition->CustomParsePattern.c_str();
}

LRESULT CToolConsoleIOPage::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	//m_infolabel2.SubclassWindow(GetDlgItem(IDC_TE_CUSTOMINFO));

	m_outputcombo.Attach(GetDlgItem(IDC_TE_OUTPUTCOMBO));
	m_outputcombo.AddString(_T("Use the main output window."));
	m_outputcombo.AddString(_T("Use an individual output window."));

	m_outputcombo.SetCurSel(m_bGlobal ? 0 : 1);

	m_outputcombo.EnableWindow(m_bCapture);

	CRect rcScintilla;
	::GetWindowRect(GetDlgItem(IDC_PLACEHOLDER), rcScintilla);
	ScreenToClient(rcScintilla);
	m_scintilla.Create(m_hWnd, rcScintilla, "Keywords", WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_TABSTOP);
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
	TextRange tr;
				
	m_scintilla.ExtendStyleRange(position, style, &tr);
	char* buf = new char[tr.chrg.cpMax - tr.chrg.cpMin + 1];
	tr.lpstrText = buf;

	m_scintilla.GetTextRange(&tr);

	PCRE::RegExp* pRE = m_scintilla.GetRE();

	if( pRE && pRE->Match(tr.lpstrText) )
	{
		tstring filename;
		tstring linestr;
		tstring colstr;
        
		// Extract the named matches from the RE, noting if there was a line or column.
		bool bFile = pRE->GetNamedMatch("f", filename);
		bool bLine = pRE->GetNamedMatch("l", linestr);
		bool bCol = pRE->GetNamedMatch("c", colstr);

		tstring display = "";
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

		GetDlgItem(IDC_TE_CLICKRESULTSSTATIC).SetWindowText(display.c_str());
	}
	else
	{
		GetDlgItem(IDC_TE_CLICKRESULTSSTATIC).SetWindowText("Error: no match");
	}

	delete [] buf;

	return 0;
}

LRESULT CToolConsoleIOPage::OnCaptureChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	enableButtons();

	return 0;
}

LRESULT CToolConsoleIOPage::OnAboutBuiltin(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	MessageBox(
		_T("Programmers Notepad 2 provides built-in support\nfor parsing output from the following tools:\n\nBorland C++\nMicrosoft Compilers\nGCC\nlcc-win32\nPython\nPerl\nand other similar tools."), 
		_T("Information"),
		MB_OK | MB_ICONINFORMATION);

	return 0;
}

LRESULT CToolConsoleIOPage::OnWindowStateChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	enableButtons();

	return 0;
}

LRESULT CToolConsoleIOPage::OnTextChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CString cs;
	GetDlgItem(IDC_TE_CUSTOMTEXT).GetWindowText(cs);

	try
	{
		m_scintilla.SetRE(cs);
	}
	catch(PCRE::REException& /*ex*/)
	{
		// The RE is invalid...
	}
	
	return 0;	
}

void CToolConsoleIOPage::enableButtons()
{
	CButton capture(GetDlgItem(IDC_TE_CAPTURECHECK));
	BOOL bCapture = capture.GetCheck();

	m_outputcombo.EnableWindow(bCapture);
	::EnableWindow(GetDlgItem(IDC_TE_BUILTIN), bCapture);
	::EnableWindow(GetDlgItem(IDC_TE_ABOUTBUILTIN), bCapture);
	::EnableWindow(GetDlgItem(IDC_TE_CLEARCHECK), bCapture);

	CButton customParse(GetDlgItem(IDC_TE_CUSTOMPARSE));
	customParse.EnableWindow(bCapture);
	BOOL bCustom = customParse.GetCheck();
	
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
	SchemeConfig* pScheme = reinterpret_cast<SchemeConfig*>( combo.GetItemData(combo.GetCurSel()) );
	if(pScheme)
	{
		m_schemeName = (LPCTSTR)pScheme->m_Name;
	}
	else
		m_schemeName = _T("default");

	int i = ::GetWindowTextLength(GetDlgItem(IDC_PHRASEEDIT)) + 1;
			
	TCHAR* buf = new TCHAR[i];
	::GetWindowText(GetDlgItem(IDC_PHRASEEDIT), buf, i);
	m_startPhrase = buf;
	delete [] buf;

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
	int index = combo.AddString(_T("Plain Text"));
	combo.SetItemDataPtr(index, NULL);
	for(SCF_IT i = m_pSchemes->GetSchemes().begin(); i != m_pSchemes->GetSchemes().end(); ++i)
	{
		index = combo.AddString((*i)->m_Title);
		if((*i)->m_Name == m_schemeName.c_str())
			selindex = index;
		combo.SetItemDataPtr(index, (*i));
	}
	combo.SetCurSel(selindex);

	CWindow(GetDlgItem(IDC_PHRASEEDIT)).SetWindowText(m_startPhrase.c_str());

	return 0;
}

void CSmartStartEditorDialog::GetValues(tstring& startPhrase, tstring& schemeName)
{
	startPhrase = m_startPhrase;
	schemeName = m_schemeName;
}

void CSmartStartEditorDialog::SetValues(LPCTSTR startPhrase, LPCTSTR schemeName)
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
