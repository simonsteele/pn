#include "stdafx.h"
#include "OptionsDialogs.h"
#include "SchemeConfig.h"

//////////////////////////////////////////////////////////////////////////////
// CToolEditorDialog
//////////////////////////////////////////////////////////////////////////////

CToolEditorDialog::CInfoLabel::CInfoLabel()
{
	m_pTitleFont = /*m_pBodyFont =*/ NULL;

	memset(strbuf, 0, sizeof(strbuf));
	LoadString(NULL, IDS_TOOLFORMATSTRINGS, strbuf, 200);
}

CToolEditorDialog::CInfoLabel::~CInfoLabel()
{
	if(m_pTitleFont)
	{
		delete m_pTitleFont;
		//delete m_pBodyFont;
	}
}

void CToolEditorDialog::CInfoLabel::MakeFonts(HDC hDC)
{
	if(!m_pTitleFont)
	{
		LOGFONT lf;
		memset(&lf, 0, sizeof(LOGFONT));

		HFONT hDefFont = static_cast<HFONT>( GetStockObject(DEFAULT_GUI_FONT) );
		GetObject(hDefFont, sizeof(LOGFONT), &lf);
		
		lf.lfWeight = FW_BOLD;

		m_pTitleFont = new CFont;
		m_pTitleFont->CreateFontIndirect(&lf);
	}
	
	//lf.lfWeight = FW_NORMAL;
	//m_pBodyFont->CreateFontIndirect(&lf);
}

LRESULT CToolEditorDialog::CInfoLabel::OnPaint(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	PAINTSTRUCT ps;
	::BeginPaint(m_hWnd, &ps);

	CDCHandle dc(ps.hdc);

	MakeFonts(dc);

	CRect rc;

	GetClientRect(rc);
	
	CRect framerc(rc);

	CBrush brush;
	brush.CreateSysColorBrush(COLOR_INFOBK);

	dc.FillRect(rc, brush);

	rc.DeflateRect(3, 3, 2, 2);

	// Draw Text...
	if(m_pTitleFont)
	{
		HFONT hOldFont = dc.SelectFont(m_pTitleFont->m_hFont);
		
		dc.SetBkColor(GetSysColor(COLOR_INFOBK));
		dc.SetTextColor(GetSysColor(COLOR_INFOTEXT));

		int height = dc.DrawText(_T("Special Symbols:"), 16, rc, DT_TOP | DT_LEFT);
		rc.top += height + 2;
		rc.left += 25;

		dc.SelectStockFont(DEFAULT_GUI_FONT);

		/* We draw n columns of text to display the % special chars. 
		This should be modified to draw as many as necessary. 
		Use a while pPipe instead of if...*/

		TCHAR* pStr = strbuf;
		TCHAR* pPipe = _tcschr(pStr, _T('|'));

		while(pPipe)
		{
			CRect rcCol(rc);
			
			*pPipe = '\0';
			
			// Calculate the rect for this column.
			dc.DrawText(pStr, _tcslen(pStr), rcCol, DT_TOP | DT_LEFT | DT_WORDBREAK | DT_CALCRECT);	

			// Actually draw the text.
			dc.DrawText(pStr, _tcslen(pStr), rc, DT_TOP | DT_LEFT | DT_WORDBREAK);

			// Find the next column start.
			rc.left += rcCol.Width() + 20;

			// Replace the pipe, and skip pStr past it.
			*pPipe++ = _T('|');
			pStr = pPipe;

			// Are there any more?
			pPipe = _tcschr(pStr, _T('|'));
		}

		// Draw the remaining text.
		dc.DrawText(pStr, _tcslen(pStr), rc, DT_TOP | DT_LEFT | DT_WORDBREAK);

		dc.SelectFont(hOldFont);
	}

	HBRUSH light = ::GetSysColorBrush(COLOR_3DSHADOW);
	dc.FrameRect(framerc, light);

	::EndPaint(m_hWnd, &ps);
	return 0;
}

CToolEditorDialog::CToolEditorDialog()
{
	m_csName = _T("");
	m_csCommand = _T("");
	m_csFolder = _T("");
	m_csParams = _T("");
	m_csShortcut = _T("");

	m_bCapture = true;

	m_csDisplayTitle = _T("New Tool");
}

LRESULT CToolEditorDialog::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CenterWindow(GetParent());

	SetWindowText(m_csDisplayTitle);

	m_infolabel.SubclassWindow(GetDlgItem(IDC_TE_INFOLABEL));

	DoDataExchange();

	return 0;
}

LRESULT CToolEditorDialog::OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	DoDataExchange(TRUE);
	EndDialog(wID);

	return 0;
}

LRESULT CToolEditorDialog::OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	EndDialog(wID);

	return 0;
}

LRESULT CToolEditorDialog::OnBrowseDir(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
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

LRESULT CToolEditorDialog::OnBrowseCommand(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
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

void CToolEditorDialog::GetValues(ToolDefinition* pDefinition)
{
	pDefinition->Name			= m_csName;
	pDefinition->Command		= m_csCommand;
	pDefinition->Folder			= m_csFolder;
	pDefinition->Params			= m_csParams;
	pDefinition->Shortcut		= m_csShortcut;
	
	pDefinition->iFlags = 
		(m_bCapture	? TOOL_CAPTURE	: 0) |
		(m_bFilter	? TOOL_ISFILTER : 0) |
		(m_bSaveAll	? TOOL_SAVEALL	: 0);
}

void CToolEditorDialog::SetValues(ToolDefinition* pDefinition)
{
	m_csName		= pDefinition->Name.c_str();
	m_csCommand		= pDefinition->Command.c_str();
	m_csFolder		= pDefinition->Folder.c_str();
	m_csParams		= pDefinition->Params.c_str();
	m_csShortcut	= pDefinition->Shortcut.c_str();
	m_bCapture		= pDefinition->CaptureOutput();
	m_bFilter		= pDefinition->IsFilter();
	m_bSaveAll		= pDefinition->SaveAll();
}

void CToolEditorDialog::SetTitle(LPCTSTR title)
{
	m_csDisplayTitle = title;
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