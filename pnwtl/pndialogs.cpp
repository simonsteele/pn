/**
 * @file pndialogs.cpp
 * @brief Assorted Dialogs for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "pndialogs.h"

//////////////////////////////////////////////////////////////////////////////
// CPNSaveDialog
//////////////////////////////////////////////////////////////////////////////

CPNSaveDialog::CPNSaveDialog(LPCTSTR szFilter) : baseClass(FALSE, _T(".txt"), NULL, OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT, szFilter)
{
	m_Format = PNSF_NoChange;
	m_ofn.Flags |= OFN_ENABLETEMPLATE;
	m_ofn.lpTemplateName = MAKEINTRESOURCE (IDD_PNSAVE);
}

LRESULT CPNSaveDialog::OnInitDialog (UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled)
{
	m_SaveTypeCombo = GetDlgItem(IDC_PNSAVE_TYPECOMBO);
	m_SaveTypeLabel = GetDlgItem(IDC_PNSAVE_SAVEASSTATIC);

	CRect rectDlg;
	GetWindowRect( &rectDlg );

	CRect rectParent;
	::GetClientRect( GetParent(), &rectParent );
	rectDlg.right = rectDlg.left + rectParent.Width();

	// Make sure there is enough room at the bottom for resize
	CRect rectCombo;
	m_SaveTypeCombo.GetWindowRect( &rectCombo );
	ScreenToClient( &rectCombo );
	int cySize = ::GetSystemMetrics( /*SM_CYSIZE*/ SM_CYSIZEFRAME );
	if (rectDlg.Height() - rectCombo.bottom < cySize)
		rectDlg.bottom = rectDlg.top + rectCombo.bottom + cySize;

	// Reposition
	SetWindowPos( NULL, 0, 0, rectDlg.Width(), rectDlg.Height(),
		SWP_NOMOVE | SWP_NOACTIVATE | SWP_NOZORDER );

	m_SaveTypeCombo.AddString(_T("No change to the file format."));
	m_SaveTypeCombo.AddString(_T("Ensure Windows Format (CR+LF)"));
	m_SaveTypeCombo.AddString(_T("Ensure Unix Format (LF)"));
	m_SaveTypeCombo.AddString(_T("Ensure Macintosh Format (CR)"));

	m_SaveTypeCombo.SetCurSel(0);
	
	RepositionControls();
	return TRUE;
}

LRESULT CPNSaveDialog::OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled)
{
	RepositionControls();
	bHandled = FALSE;
	return FALSE;
}

LRESULT CPNSaveDialog::OnComboSelChange(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	// Store the new selection in here...
	m_Format = (EPNSaveFormat)m_SaveTypeCombo.GetCurSel();

	return TRUE;
}

EPNSaveFormat CPNSaveDialog::GetSaveFormat()
{
	return m_Format;
}

void CPNSaveDialog::RepositionControls()
{
	RepositionControl( m_SaveTypeCombo, cmb1, true );
	RepositionControl( m_SaveTypeLabel, stc2, false );

	RepositionPlacesBar( m_SaveTypeCombo );
}

void CPNSaveDialog::RepositionPlacesBar(CWindow &bottomwnd)
{
	CRect rc, rc2;
	CWindow wndParent = GetParent();
	CWindow wndPB = wndParent.GetDlgItem( ctl1 );
	if( wndPB.IsWindow() )
	{
		wndPB.GetWindowRect( &rc );
		bottomwnd.GetWindowRect( &rc2 );
		wndParent.ScreenToClient( &rc );
		ScreenToClient( &rc2 );
		rc.bottom = rc2.bottom;
		wndPB.SetWindowPos( NULL, &rc, SWP_NOACTIVATE | SWP_NOZORDER );
	}
}

/**
 * @brief Reposition a control
 * @param wnd Control to be reposition
 * @param nID ID of the control used for positioning
 * @param fSize If true, adjust the width of the control
 */
void CPNSaveDialog::RepositionControl(CWindow &wnd, UINT nID, bool fSize)
{
	// Get the window rect in the client area of the 
	// control we are interested in.
	CWindow wndParent = GetParent();
	CWindow wndAnchor = wndParent.GetDlgItem( nID );
	CRect rectAnchor;
	wndAnchor.GetWindowRect( &rectAnchor );
	wndParent.ScreenToClient( &rectAnchor );

	//
	// Reposition the control
	//

	DWORD dwSWFlags = SWP_NOACTIVATE | SWP_NOZORDER | SWP_NOSIZE;
	CRect rectCtrl;
	wnd.GetWindowRect( &rectCtrl );
	ScreenToClient( &rectCtrl );
	rectCtrl.OffsetRect( rectAnchor.left - rectCtrl.left, 0 );
	if( fSize )
	{
		rectCtrl.right = rectCtrl.left + rectAnchor.Width();
		dwSWFlags &= ~SWP_NOSIZE;
	}
	wnd.SetWindowPos( NULL, rectCtrl.left, rectCtrl.top,
		rectCtrl.Width(), rectCtrl.Height(), dwSWFlags );
	return;
}

//////////////////////////////////////////////////////////////////////////////
// CGotoDialog
//////////////////////////////////////////////////////////////////////////////

LRESULT CGotoDialog::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CenterWindow(GetParent());
	return TRUE;
}

LRESULT CGotoDialog::OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	HWND hEdit = GetDlgItem(IDC_GOTOEDIT);
	int i = ::GetWindowTextLength(hEdit);
	i++;
	TCHAR *t = new TCHAR[i];
	::GetWindowText(hEdit, t, i);
	lineno = _ttoi(t);
	delete [] t;

	EndDialog(wID);

	return TRUE;
}

LRESULT CGotoDialog::OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	EndDialog(wID);
	
	return TRUE;
}

int CGotoDialog::GetLineNo()
{
	return lineno;
}

//////////////////////////////////////////////////////////////////////////////
// CStyleDisplay
//////////////////////////////////////////////////////////////////////////////

CStyleDisplay::CStyleDisplay()
{
	m_Font = NULL;
	memset(&m_lf, 0, sizeof(LOGFONT));
}

CStyleDisplay::~CStyleDisplay()
{
	if(m_Font)
		delete m_Font;
}

void CStyleDisplay::SetBold(bool bold)
{
	m_lf.lfWeight = (bold ? FW_BOLD : FW_NORMAL);
	UpdateFont();
}

void CStyleDisplay::SetItalic(bool italic)
{
	m_lf.lfItalic = italic;
	UpdateFont();
}

void CStyleDisplay::SetUnderline(bool underline)
{
	m_lf.lfUnderline = underline;
	UpdateFont();
}

void CStyleDisplay::SetFontName(LPCTSTR fontname)
{
	_tcscpy(m_lf.lfFaceName, fontname);
	UpdateFont();
}

void CStyleDisplay::SetSize(int size, bool bInvalidate)
{
	HDC dc = GetDC();			
	m_lf.lfHeight = -MulDiv(size, GetDeviceCaps(dc, LOGPIXELSY), 72);
	ReleaseDC(dc);

	if(bInvalidate)
		UpdateFont();
}

void CStyleDisplay::SetFore(COLORREF fore)
{
	m_Fore = fore;
	Invalidate();
}

void CStyleDisplay::SetBack(COLORREF back)
{
	m_Back = back;
	Invalidate();
}

void CStyleDisplay::SetStyle(LPCTSTR fontname, int fontsize, COLORREF fore, COLORREF back, LPCTSTR name, bool bold, bool italic, bool underline)
{
	m_Name = name;
	m_Fore = fore;
	m_Back = back;

	SetSize(fontsize, false);
	
	m_lf.lfWeight = (bold ? FW_BOLD : FW_NORMAL);
	m_lf.lfUnderline = underline;
	m_lf.lfItalic = italic;
	_tcscpy(m_lf.lfFaceName, fontname);

	UpdateFont();
}

LRESULT CStyleDisplay::OnPaint(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	PAINTSTRUCT ps;
	BeginPaint(&ps);

	CDC dc(ps.hdc);
	
	CRect rc;
	GetClientRect(rc);

	dc.FillRect(rc, (HBRUSH)::GetStockObject(WHITE_BRUSH));

	// Draw in the example text.
	if(m_Font)
	{
		HFONT hOldFont = dc.SelectFont(m_Font->m_hFont);
		dc.SetBkColor(m_Back);
		dc.SetTextColor(m_Fore);
		dc.DrawText(m_Name, m_Name.GetLength(), rc, DT_CENTER | DT_VCENTER | DT_SINGLELINE);
		dc.SelectFont(hOldFont);
	}

	// Draw a light border around the control.
	HBRUSH light = ::CreateSolidBrush(::GetSysColor(COLOR_3DSHADOW));
	dc.FrameRect(rc, light);
	::DeleteObject(light);
	
	EndPaint(&ps);
	return 0;
}

LRESULT CStyleDisplay::OnEraseBkgnd(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	return 1;
}

void CStyleDisplay::UpdateFont()
{
	if(m_Font)
		delete m_Font;

	m_Font = new CFont;
	m_Font->CreateFontIndirect(&m_lf);

	Invalidate();
}

//////////////////////////////////////////////////////////////////////////////
// CTabPageKeywords
//////////////////////////////////////////////////////////////////////////////

CTabPageKeywords::CTabPageKeywords()
{
	m_pSet = NULL;
	m_bChanging = false;
}

void CTabPageKeywords::SetScheme(SchemeConfig* pScheme)
{
	m_pSet = NULL;
	m_pScheme = pScheme;

	// Set Keywords
	if( ::IsWindow(m_hWnd) )
	{
		DoSetScheme();
	}
}

void CTabPageKeywords::DoSetScheme()
{
	m_bChanging = true;

	CustomKeywordSet* pSet = m_pScheme->GetFirstKeywordSet();
	
	int iPos = 0;

	while(pSet)
	{
		if(pSet->pName)
		{
			int iItem = m_list.AddItem(iPos++, 0, pSet->pName);
			m_list.SetItemData(iItem, reinterpret_cast<DWORD>(pSet));
		}
		
		pSet = pSet->pNext;
	}

	m_bChanging = false;
}

void CTabPageKeywords::SetItem()
{
	if(m_pSet)
	{
		// compare the keyword sets first, has the current one changed?
		int len = m_scintilla.GetTextLength();
		TCHAR* pCS = new TCHAR[len+1];
		m_scintilla.GetText(len+1, pCS);
		pCS[len] = _T('\0');
		
		if(_tcscmp(m_pSet->pWords, pCS) != 0)
		{
			CustomKeywordSet* pCustomSet = m_pScheme->m_cKeywords.FindKeywordSet(m_pSet->key);

			if(pCustomSet)
			{
				delete [] pCustomSet->pWords;
				pCustomSet->pWords = pCS;
				pCS = NULL;
			}
			else
			{
				CustomKeywordSet* pNewSet = new CustomKeywordSet(*m_pSet);
				pNewSet->pWords = pCS;
				pCS = NULL;
				m_pScheme->m_cKeywords.AddKeywordSet(pNewSet);
			}
		}
		else
		{
			CustomKeywordSet* pCustomSet = m_pScheme->m_cKeywords.FindKeywordSet(m_pSet->key);
			if(pCustomSet)
				m_pScheme->m_cKeywords.DeleteKeywordSet(pCustomSet);
		}

		if(pCS)
			delete [] pCS;
	}
}

void CTabPageKeywords::Finalise()
{
	// Ensure everything is saved that should be...
	SetItem();
}

void CTabPageKeywords::UpdateSel()
{
	if(!m_bChanging)
	{
		SetItem();

		int iSelected = m_list.GetSelectedIndex();
		if(iSelected != -1)
		{
			CustomKeywordSet* pRealSet = reinterpret_cast<CustomKeywordSet*>( m_list.GetItemData(iSelected) );

			if(pRealSet)
			{
				m_pSet = pRealSet;

				CustomKeywordSet* pS = pRealSet;
				CustomKeywordSet* pCustomSet = m_pScheme->m_cKeywords.FindKeywordSet(m_pSet->key);

				if(pCustomSet)
					pS = pCustomSet;

				if(pS->pWords)
				{
					m_scintilla.SetText(pS->pWords);
					EnableControls();
				}
			}
			else
				EnableControls(FALSE);
		}
		else
		{
			EnableControls(FALSE);
		}
	}
}

void CTabPageKeywords::EnableControls(BOOL bEnable)
{
	//m_Text.EnableWindow(bEnable);
	m_ResetBtn.EnableWindow(bEnable);
}

LRESULT CTabPageKeywords::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	//m_Text.Attach(GetDlgItem(IDC_KEYWORDS_KEYWORDTEXT));
	m_ResetBtn.Attach(GetDlgItem(IDC_KEYWORDS_RESETBUTTON));
	m_list.Attach(GetDlgItem(IDC_KEYWORDS_LIST));

	CRect rcScintilla;
	::GetWindowRect(GetDlgItem(IDC_PLACEHOLDER), rcScintilla);
	ScreenToClient(rcScintilla);
	m_scintilla.Create(m_hWnd, rcScintilla, "Keywords", WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS);
	m_scintilla.SetWrapMode(SC_WRAP_WORD);

	CRect rc;
	m_list.GetClientRect(&rc);
	int wCol = rc.right - rc.left - 20;
	m_list.InsertColumn(0, _T(""), LVCFMT_LEFT, wCol, 0);
	
	EnableControls(FALSE);

	if(m_pScheme)
		DoSetScheme();

	m_list.Invalidate(TRUE);

	return 0;
}

LRESULT CTabPageKeywords::OnResetClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pSet)
	{
		m_scintilla.SetText(m_pSet->pWords);
	}
	return 0;
}

LRESULT CTabPageKeywords::OnSortClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{

	return 0;
}

LRESULT CTabPageKeywords::OnListSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	UpdateSel();

	return 0;
}

//////////////////////////////////////////////////////////////////////////////
// CTabPageStyles
//////////////////////////////////////////////////////////////////////////////

CTabPageStyles::CTabPageStyles()
{
	m_pStyle = NULL;
	m_bChanging = false;
}

void CTabPageStyles::SetScheme(SchemeConfig* pScheme)
{
	m_bChanging = true;
	m_pStyle = NULL;
	m_pScheme = pScheme;

	m_tree.DeleteAllItems();

	CustomStyleCollection* pColl = static_cast<CustomStyleCollection*>(pScheme);
	HTREEITEM insertunder = TVI_ROOT;
	
	while(pColl)
	{
		for(SL_IT i = pColl->m_Styles.begin(); i != pColl->m_Styles.end(); ++i)
		{
			HTREEITEM hi = m_tree.InsertItem((*i)->name.c_str(), insertunder, TVI_LAST);
			m_tree.SetItemData(hi, reinterpret_cast<DWORD_PTR>(*i));
		}
		if(insertunder != TVI_ROOT)
			m_tree.Expand(insertunder, TVE_EXPAND);

		pColl = pColl->GetNext();
		if(pColl)
		{
			insertunder = m_tree.InsertItem(pColl->GetName(), NULL, NULL);
			m_tree.SetItemData(insertunder, reinterpret_cast<DWORD_PTR>(pColl));
		}
	}

	m_tree.EnsureVisible(m_tree.GetRootItem());

	m_bChanging = false;
}

void CTabPageStyles::Finalise()
{
	SetItem();
}

LRESULT CTabPageStyles::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CRect rc;
	
	m_tree.Attach(GetDlgItem(IDC_STYLES_TREE));

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

LRESULT CTabPageStyles::OnForeChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	NMCOLORBUTTON* pN = reinterpret_cast<NMCOLORBUTTON*>(pnmh);
	COLORREF col = (pN->clr == CLR_DEFAULT ? m_fore.GetDefaultColor() : pN->clr);
	m_sd.SetFore(col);
	m_Style.ForeColor = col;
	return 0;
}

LRESULT CTabPageStyles::OnBackChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	NMCOLORBUTTON* pN = reinterpret_cast<NMCOLORBUTTON*>(pnmh);
	COLORREF col = (pN->clr == CLR_DEFAULT ? m_fore.GetDefaultColor() : pN->clr);
	m_sd.SetBack(col);
	m_Style.BackColor = col;
	return 0;
}

LRESULT CTabPageStyles::OnFontChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{			
	if(m_pStyle)
	{
		int i = m_FontCombo.GetCurSel();
		CString str;
		m_FontCombo.GetLBText(i, str);
		m_sd.SetFontName(str);
		m_Style.FontName = (LPCTSTR)str;
	}
	return 0;
}

LRESULT CTabPageStyles::OnSizeChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		int i = m_SizeCombo.GetSelection();
		m_sd.SetSize(i);
		m_Style.FontSize = i;
	}
	return 0;
}

LRESULT CTabPageStyles::OnTreeSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	UpdateSel();

	return 0;
}

void CTabPageStyles::UpdateSel()
{
	// If we're not in the middle of changing scheme...
	if(!m_bChanging)
	{
		SetItem();

		HTREEITEM item = m_tree.GetSelectedItem();

		if(item)
		{
			if(m_tree.GetChildItem(item) == NULL)
			{
				StyleDetails* pS = reinterpret_cast<StyleDetails*>(m_tree.GetItemData(item));
				if(pS)
				{
					StyleDetails* existing = m_pScheme->m_customs.GetStyle(pS->Key);
					if(existing)
						m_Style = *existing;
					else
						m_Style = *pS;
					
					m_pStyle = pS;
					m_sd.SetStyle(m_Style.FontName.c_str(), m_Style.FontSize, m_Style.ForeColor, m_Style.BackColor, m_Style.name.c_str(), m_Style.Bold, m_Style.Italic, m_Style.Underline);
					m_bold.SetCheck(m_Style.Bold ? BST_CHECKED : BST_UNCHECKED);
					m_italic.SetCheck(m_Style.Italic ? BST_CHECKED : BST_UNCHECKED);
					m_underline.SetCheck(m_Style.Underline ? BST_CHECKED : BST_UNCHECKED);
					m_eolfilled.SetCheck(m_Style.EOLFilled ? BST_CHECKED : BST_UNCHECKED);
					m_fore.SetColor(m_Style.ForeColor);
					m_back.SetColor(m_Style.BackColor);

					m_FontCombo.SelectString(0, m_Style.FontName.c_str());
					m_SizeCombo.Select(m_Style.FontSize);
				}
			}
			else
			{
				///@todo Disable everything
			}
		}
		else
			m_pStyle = NULL;
	}
}

LRESULT CTabPageStyles::OnBoldClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		bool bC = m_bold.GetCheck() == BST_CHECKED;
		m_Style.Bold = bC;
		m_sd.SetBold(bC);
	}

	return 0;
}

LRESULT CTabPageStyles::OnItalicClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		bool bC = m_italic.GetCheck() == BST_CHECKED;
		m_Style.Italic = bC;
		m_sd.SetItalic(bC);
	}
	return 0;
}

LRESULT CTabPageStyles::OnUnderlineClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		bool bC = m_underline.GetCheck() == BST_CHECKED;
		m_Style.Underline = bC;
		m_sd.SetUnderline(bC);
	}
	return 0;
}

LRESULT CTabPageStyles::OnEOLFilledClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		bool bC = m_eolfilled.GetCheck() == BST_CHECKED;
		m_Style.EOLFilled = bC;
		//m_sd.SetEOLFilled(bC);
	}
	return 0;
}

void CTabPageStyles::SetItem()
{
	if(m_pStyle)
	{
		int mask = 0;

		if(m_Style != *m_pStyle)
		{
			/* The style the user has configured and the original definition version
			   do not match. We need to store the new style in the custom style
			   store. */
			StyleDetails* existing = m_pScheme->m_customs.GetStyle(m_Style.Key);
			if(existing)
			{
				*existing = m_Style;
			}
			else
			{
				existing = new StyleDetails;
				*existing = m_Style;
				m_pScheme->m_customs.AddStyle(existing);
			}
		}
		else
		{
			/* If we have set the style to be like the original, then
			   we can safely remove any custom styles. */
			m_pScheme->m_customs.RemoveStyle(m_Style.Key);
		}
	}
}

LRESULT CTabPageStyles::OnResetClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pScheme)
	{
		if(m_pStyle)
		{
			m_pScheme->m_customs.RemoveStyle(m_pStyle->Key);
			m_pStyle = NULL;
		}
	
		UpdateSel();
	}

	return 0;
}

LRESULT CTabPageStyles::OnResetAllClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pScheme)
	{
		m_pStyle = NULL;
		m_pScheme->m_customs.RemoveAll();

		UpdateSel();
	}
	
	return 0;
}

//////////////////////////////////////////////////////////////////////////////
// COptionsPageStyle
//////////////////////////////////////////////////////////////////////////////

LRESULT COptionsPageStyle::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_FontCombo.SubclassWindow(GetDlgItem(IDC_FONT_COMBO));
	m_SizeCombo.Attach(GetDlgItem(IDC_FONTSIZE_COMBO));

	m_fore.SubclassWindow(GetDlgItem(IDC_STYLE_FOREBUTTON));
	m_back.SubclassWindow(GetDlgItem(IDC_STYLE_BACKBUTTON));

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
	if(m_pSchemes)
	{
		StyleDetails* pStyle = m_pSchemes->GetDefaultStyle();
		
		m_FontCombo.SelectString(0, pStyle->FontName.c_str());
		m_SizeCombo.Select(pStyle->FontSize);
		
		m_fore.SetColor(pStyle->ForeColor);
		m_fore.SetDefaultColor(RGB(0,0,0));
		
		m_back.SetColor(pStyle->BackColor);
		m_back.SetDefaultColor(RGB(255,255,255));

		m_bold.SetCheck(pStyle->Bold ? BST_CHECKED : BST_UNCHECKED);
		m_italic.SetCheck(pStyle->Italic ? BST_CHECKED : BST_UNCHECKED);
		m_underline.SetCheck(pStyle->Underline ? BST_CHECKED : BST_UNCHECKED);
	}
}

void COptionsPageStyle::OnOK()
{
	if(m_bCreated)
	{
		bool bIsCustom;
		StyleDetails* pCurrent = GetDefault(bIsCustom);
		StyleDetails* pS = new StyleDetails(*pCurrent);
		
		int i = m_FontCombo.GetCurSel();
		CString str;
		m_FontCombo.GetLBText(i, str);

		pS->FontName = str;
		pS->FontSize = m_SizeCombo.GetSelection();
		pS->ForeColor = m_fore.SafeGetColor();
		pS->BackColor = m_back.SafeGetColor();
		pS->Bold = (m_bold.GetCheck() == BST_CHECKED);
		pS->Italic = (m_italic.GetCheck() == BST_CHECKED);
		pS->Underline = (m_underline.GetCheck() == BST_CHECKED);

		if(*pS != *pCurrent)
		{
			// the new style is not the same as the current style...
			
			if(bIsCustom)
			{
				// the current style is already a custom one.
				StyleDetails* pOrig = m_pSchemes->GetStyleClasses().GetStyle(_T("default"));
				if(*pOrig == *pS)
				{
					// The user has reverted to the original style.
					m_pSchemes->GetCustomClasses().DeleteStyle(_T("default"));
				}
				else
				{
					// pCurrent is already in the "Custom" classes collection. Update it.
					*pCurrent = *pS;
				}

				/* If there was already a custom version of this style then one
				way or another, there is no need for our temporary one any more. */
				delete pS;
			}
			else
			{
				// There isn't already a custom style for this class, so we add one.
				m_pSchemes->GetCustomClasses().AddStyle(_T("default"), pS);
			}
		}
		else
		{
			delete pS;
		}
	}
}

void COptionsPageStyle::OnCancel()
{
}

LPCTSTR COptionsPageStyle::GetTreePosition()
{
	return _T("Style");
}

StyleDetails* COptionsPageStyle::GetDefault(bool& bIsCustom)
{
	bIsCustom = false;
	
	StyleDetails* pCustom = m_pSchemes->GetCustomClasses().GetStyle(_T("default"));
	if(pCustom)
	{
		bIsCustom = true;
		return pCustom;
	}
	
	return m_pSchemes->GetDefaultStyle();
}


//////////////////////////////////////////////////////////////////////////////
// COptionsPageSchemes
//////////////////////////////////////////////////////////////////////////////

COptionsPageSchemes::COptionsPageSchemes(SchemeConfigParser* pSchemes) : COptionsPageImpl<COptionsPageSchemes>()
{
	m_pSchemes = pSchemes;
}

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
	label.SetWindowPos(HWND_TOP, &rc, 0);

	CRect rcCombo;

	m_combo.Attach(GetDlgItem(IDC_SCHEMECOMBO));

	m_combo.GetWindowRect(rcCombo);
	ScreenToClient(rcCombo);
	rcCombo.left = rc.right + 5;
	m_combo.SetWindowPos(HWND_TOP, &rcCombo, 0);

	
	CRect rcPH;
	::GetWindowRect(GetDlgItem(IDC_PS_PLACEHOLDER), rcPH);
	ScreenToClient(rcPH);
	m_stylestab.SetTitle(_T("Styles"));
	m_keywordstab.SetTitle(_T("Keywords"));
	m_props.AddPage(m_stylestab);
	m_props.AddPage(m_keywordstab);
	
	m_props.Create(m_hWnd, 0, rcPH);

	return 0;
}

void COptionsPageSchemes::OnInitialise()
{
	for(SCF_IT i = m_pSchemes->GetSchemes().begin(); i != m_pSchemes->GetSchemes().end(); ++i)
	{
		int index = m_combo.AddString((*i)->m_Title);
		m_combo.SetItemDataPtr(index, (*i));
	}
	
	if(m_combo.GetCount() > 0)
	{
		m_combo.SetCurSel(0);
		Update();
	}
}

void COptionsPageSchemes::OnOK()
{
	m_stylestab.Finalise();
	m_keywordstab.Finalise();
	m_pSchemes->SaveConfig();
}

LPCTSTR COptionsPageSchemes::GetTreePosition()
{
	return _T("Style\\Schemes");
}

LRESULT COptionsPageSchemes::OnComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Update();
	return 0;
}

void COptionsPageSchemes::Update()
{
	int i = m_combo.GetCurSel();
	SchemeConfig* pScheme = static_cast<SchemeConfig*>(m_combo.GetItemDataPtr(i));
	m_stylestab.SetScheme(pScheme);
	m_keywordstab.SetScheme(pScheme);
}