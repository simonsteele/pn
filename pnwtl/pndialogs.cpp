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
// COptionsDialog
//////////////////////////////////////////////////////////////////////////////

COptionsDialog::COptionsDialog()
{
	m_pCurrentPage = NULL;
}

BOOL COptionsDialog::EndDialog(int nRetCode)
{
	ClosePages();
	return baseClass::EndDialog(nRetCode);
}

LRESULT COptionsDialog::OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	PAGEPTRLIST::iterator i;

	for(i = m_Pages.begin(); i != m_Pages.end(); ++i)
	{
		(*i)->OnOK();
	}

	EndDialog(wID);

	return TRUE;
}

LRESULT COptionsDialog::OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	PAGEPTRLIST::iterator i;

	for(i = m_Pages.begin(); i != m_Pages.end(); ++i)
	{
		(*i)->OnCancel();
	}

	EndDialog(wID);
	
	return TRUE;
}

LRESULT COptionsDialog::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_tree.Attach(GetDlgItem(IDC_TREE));

	InitialisePages();
		
	CenterWindow(GetParent());
	
	return TRUE;
}

void COptionsDialog::AddPage(COptionsPage* pPage)
{
	m_Pages.insert(m_Pages.end(), pPage);
}

HTREEITEM COptionsDialog::FindAtThisLevel(LPCTSTR title, HTREEITEM context)
{
	HTREEITEM i = (context ? m_tree.GetChildItem(context) : m_tree.GetRootItem());

	TCHAR buf[32];
	TVITEM tvi;
	tvi.mask = TVIF_TEXT;
	tvi.pszText = buf;
	tvi.cchTextMax = 30;
	
	while(i)
	{
		tvi.hItem = i;
		m_tree.GetItem(&tvi);

		if(_tcscmp(buf, title) == 0)
			break;

		i = m_tree.GetNextSiblingItem(i);
	}

	return i;
}

HTREEITEM COptionsDialog::AddTreeEntry(LPCTSTR title, HTREEITEM context)
{
	HTREEITEM i = FindAtThisLevel(title, context);

	if(!i)
	{
		i = m_tree.InsertItem(title, context, NULL);
		if(context)
			m_tree.Expand(context, TVE_EXPAND);
		m_tree.SetItemData(i, NULL);
	}

	return i;
}

void COptionsDialog::InitialisePages()
{
	TCHAR buf[200];
	PAGEPTRLIST::iterator i;

	for(i = m_Pages.begin(); i != m_Pages.end(); ++i)
	{
		LPCTSTR treeloc = (*i)->GetTreePosition();
		PNASSERT(_tcslen(treeloc) < 200);
		_tcscpy(buf, treeloc);

		HTREEITEM ti = NULL;
		TCHAR* pSlash = NULL;
		TCHAR* pNext = buf;

		while((pSlash = _tcschr(pNext, _T('\\'))) != NULL)
		{
			*pSlash = NULL;
			ti = AddTreeEntry(pNext, ti);
			*pSlash = '\\';
			pNext = pSlash + 1;
		}
		// Add Tree Item
		HTREEITEM item = AddTreeEntry(pNext, ti);
		m_tree.SetItemData(item, reinterpret_cast<DWORD_PTR>(*i));
	}
}

void COptionsDialog::ClosePages()
{
	PAGEPTRLIST::iterator i;
	for(i = m_Pages.begin(); i != m_Pages.end(); ++i)
	{
		if((*i)->m_bCreated)
		{
			(*i)->ClosePage();
		}
	}
}

void COptionsDialog::SelectPage(COptionsPage* pPage)
{
	CRect rcPage;
	::GetWindowRect(GetDlgItem(IDC_PLACEHOLDER), rcPage);
	ScreenToClient(rcPage);

	if(m_pCurrentPage)
	{
		m_pCurrentPage->ShowPage(SW_HIDE);
	}

	if(!pPage->m_bCreated)
	{
		pPage->CreatePage(m_hWnd, rcPage, m_tree);
		pPage->OnInitialise();
		pPage->m_bCreated = true;
	}
	
	pPage->ShowPage(SW_SHOW);
	m_pCurrentPage = pPage;
}

LRESULT COptionsDialog::OnTreeNotify(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMTREEVIEW pN = reinterpret_cast<LPNMTREEVIEW>(pnmh);
	if(pnmh->code == TVN_SELCHANGED)
	{
		COptionsPage* pPage = reinterpret_cast<COptionsPage*>(m_tree.GetItemData(pN->itemNew.hItem));
		if(pPage)
			SelectPage(pPage);
	}

	return 0;
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

	HBRUSH light = ::CreateSolidBrush(::GetSysColor(COLOR_3DSHADOW));

	dc.FillRect(rc, (HBRUSH)::GetStockObject(WHITE_BRUSH));
	dc.FrameRect(rc, light);

	if(m_Font)
	{
		HFONT hOldFont = dc.SelectFont(m_Font->m_hFont);
		dc.SetBkColor(m_Back);
		dc.SetTextColor(m_Fore);
		dc.DrawText(m_Name, m_Name.GetLength(), rc, DT_CENTER | DT_VCENTER | DT_SINGLELINE);
		dc.SelectFont(hOldFont);
	}

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
// CStylesTabPage
//////////////////////////////////////////////////////////////////////////////

CStylesTabPage::CStylesTabPage()
{
	m_pStyle = NULL;
	m_bChanging = false;
}

void CStylesTabPage::SetScheme(SchemeConfig* pScheme)
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

void CStylesTabPage::Finalise()
{
	SetItem();
}

LRESULT CStylesTabPage::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CRect rc;
	
	m_tree.Attach(GetDlgItem(IDC_STYLES_TREE));

	CWindow placeholder(GetDlgItem(IDC_STYLE_EXAMPLE));
	placeholder.GetWindowRect(rc);
	ScreenToClient(rc);
	m_sd.Create(m_hWnd, rc, _T("Style Display"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS);

	//IDC_FORE_PLACEHOLDER, IDC_BACK_PLACEHOLDER
	//IDC_STYLE_FONTCOMBO, IDC_STYLE_SIZECOMBO

	m_FontCombo.SubclassWindow(GetDlgItem(IDC_STYLE_FONTCOMBO));
	m_SizeCombo.Attach(GetDlgItem(IDC_STYLE_SIZECOMBO));

	m_fore.SubclassWindow(GetDlgItem(IDC_STYLE_FOREBUTTON));
	m_back.SubclassWindow(GetDlgItem(IDC_STYLE_BACKBUTTON));
	
	AddFontSize(6, _T("6"));
	AddFontSize(8, _T("8"));
	AddFontSize(10, _T("10"));
	AddFontSize(12, _T("12"));
	AddFontSize(14, _T("14"));
	AddFontSize(16, _T("16"));
	AddFontSize(18, _T("18"));

	m_bold.Attach(GetDlgItem(IDC_STYLE_BOLDCHECK));
	m_italic.Attach(GetDlgItem(IDC_STYLE_ITALICCHECK));
	m_underline.Attach(GetDlgItem(IDC_STYLE_UNDERLINECHECK));
	m_eolfilled.Attach(GetDlgItem(IDC_STYLE_EOLFILLEDCHECK));

	return 0;
}

LRESULT CStylesTabPage::OnForeChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	NMCOLORBUTTON* pN = reinterpret_cast<NMCOLORBUTTON*>(pnmh);
	m_sd.SetFore(pN->clr);
	m_Style.ForeColor = pN->clr;
	return 0;
}

LRESULT CStylesTabPage::OnBackChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	NMCOLORBUTTON* pN = reinterpret_cast<NMCOLORBUTTON*>(pnmh);
	m_sd.SetBack(pN->clr);
	m_Style.BackColor = pN->clr;
	return 0;
}

LRESULT CStylesTabPage::OnFontChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
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

LRESULT CStylesTabPage::OnSizeChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		int i = m_SizeCombo.GetCurSel();
		i = m_SizeCombo.GetItemData(i);
		m_sd.SetSize(i);
		m_Style.FontSize = i;
	}
	return 0;
}

LRESULT CStylesTabPage::OnTreeSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	UpdateSel();

	return 0;
}

void CStylesTabPage::UpdateSel()
{
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
					TCHAR buf[10];
					_itot(m_Style.FontSize, buf, 10);
					if (m_SizeCombo.SelectString(0, buf) == CB_ERR)
					{
						int idx = m_SizeCombo.AddString(buf);
						m_SizeCombo.SetCurSel(idx);
					}
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

LRESULT CStylesTabPage::OnBoldClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		bool bC = m_bold.GetCheck() == BST_CHECKED;
		m_Style.Bold = bC;
		m_sd.SetBold(bC);
	}

	return 0;
}

LRESULT CStylesTabPage::OnItalicClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		bool bC = m_italic.GetCheck() == BST_CHECKED;
		m_Style.Italic = bC;
		m_sd.SetItalic(bC);
	}
	return 0;
}

LRESULT CStylesTabPage::OnUnderlineClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		bool bC = m_underline.GetCheck() == BST_CHECKED;
		m_Style.Underline = bC;
		m_sd.SetUnderline(bC);
	}
	return 0;
}

LRESULT CStylesTabPage::OnEOLFilledClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		bool bC = m_eolfilled.GetCheck() == BST_CHECKED;
		m_Style.EOLFilled = bC;
		//m_sd.SetEOLFilled(bC);
	}
	return 0;
}

void CStylesTabPage::AddFontSize(int size, LPCTSTR sizestr)
{
	int idx = m_SizeCombo.AddString(sizestr);
	m_SizeCombo.SetItemData(idx, size);
}

void CStylesTabPage::SetItem()
{
	if(m_pStyle)
	{
		int mask = 0;

		if(m_Style != *m_pStyle)
		{
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
	}
}

LRESULT CStylesTabPage::OnResetClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
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

LRESULT CStylesTabPage::OnResetAllClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
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

void COptionsPageStyle::OnOK()
{
	::OutputDebugString(_T("COptionsPageStyle::OnOK\n"));
}

void COptionsPageStyle::OnCancel()
{
	::OutputDebugString(_T("COptionsPageStyle::OnCancel\n"));
}

void COptionsPageStyle::OnInitialise()
{
	::OutputDebugString(_T("COptionsPageStyle::OnInitialise\n"));
	m_FontCombo.SelectString(0, _T("Lucida Console"));
	m_SizeCombo.SelectString(0, _T("10"));
}

LPCTSTR COptionsPageStyle::GetTreePosition()
{
	return _T("Style");
}

LRESULT COptionsPageStyle::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_FontCombo.SubclassWindow(GetDlgItem(IDC_FONT_COMBO));
	m_SizeCombo = GetDlgItem(IDC_FONTSIZE_COMBO);
	
	m_SizeCombo.AddString(_T("6"));
	m_SizeCombo.AddString(_T("8"));
	m_SizeCombo.AddString(_T("10"));
	m_SizeCombo.AddString(_T("12"));
	m_SizeCombo.AddString(_T("14"));
	m_SizeCombo.AddString(_T("16"));
	m_SizeCombo.AddString(_T("18"));

	return 0;
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
}