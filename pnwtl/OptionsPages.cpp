/**
 * @file optionspages.cpp
 * @brief Options Dialog Pages (1) for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "OptionsPages.h"
#include "OptionsDialogs.h"

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

	CComboBox cb(GetDlgItem(IDC_OPT_LECOMBO));
	m_SaveFormat = (EPNSaveFormat)cb.GetItemData(cb.GetCurSel());

	COptionsManager& options = COptionsManager::GetInstanceRef();
	options.UseTabs = m_bUseTabs != FALSE;
	options.TabWidth = m_iTabWidth;
	options.LineNumbers = m_bLineNos != FALSE;
	options.LineEndings = m_SaveFormat;
	options.MaximiseNew = m_bMaximise != FALSE;
	options.ShowFullPath = m_bFullPath != FALSE;
	// Ensure MRU size <= 50 && >= 1
	m_iMRUSize = ( m_iMRUSize > 50 ? 50 : ( m_iMRUSize < 1 ? 1 : m_iMRUSize ) );
	options.Set(PNSK_INTERFACE, _T("MRUSize"), (int)m_iMRUSize);
	options.Set(PNSK_INTERFACE, _T("AllowMultiInstance"), (bool)(m_bMultiInstanceOk  != FALSE));
}

void COptionsPageGeneral::OnInitialise()
{
	COptionsManager& options = COptionsManager::GetInstanceRef();
	m_bUseTabs = options.UseTabs;
	m_iTabWidth = options.TabWidth;
	m_bLineNos = options.LineNumbers;
	m_SaveFormat = options.LineEndings;
	m_bMaximise = options.MaximiseNew;
	m_bFullPath = options.ShowFullPath;
	m_iMRUSize = options.Get(PNSK_INTERFACE, _T("MRUSize"), 4);
	m_bMultiInstanceOk = options.Get(PNSK_INTERFACE, _T("AllowMultiInstance"), false);

	CComboBox cb(GetDlgItem(IDC_OPT_LECOMBO));
	for(int i = 0; i < cb.GetCount(); i++)
	{
		if(cb.GetItemData(i) == m_SaveFormat)
		{
			cb.SetCurSel(i);
			break;
		}
	}

	DoDataExchange();
}

LRESULT COptionsPageGeneral::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
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

	COptionsManager& options = COptionsManager::GetInstanceRef();
	options.ShowIndentGuides = m_bIndentGuides != FALSE;
	options.LineHighlight = m_bLineHighlight != FALSE;
	options.LineHighlightColour = m_btnLineCol.SafeGetColor();
	options.RightGuide = m_iLongLineHelp;
	options.RightColumn = m_iRightColumn;
	options.RightGuideColour = m_btnLLCol.SafeGetColor();
}

void COptionsPageVisual::OnInitialise()
{
	COptionsManager& options = COptionsManager::GetInstanceRef();
	m_bIndentGuides = options.ShowIndentGuides;
	m_bLineHighlight = options.LineHighlight;
	m_btnLineCol.SetColor( options.LineHighlightColour );
	m_iLongLineHelp = options.RightGuide;
	m_iRightColumn = options.RightColumn;
	m_btnLLCol.SetColor(options.RightGuideColour);

	DoDataExchange();
}

LRESULT COptionsPageVisual::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_btnLineCol.SubclassWindow(GetDlgItem(IDC_OPT_LINELIGHTBUTTON));
	m_btnLineCol.SetDefaultColor(RGB(255, 255, 224));
	m_btnLLCol.SubclassWindow(GetDlgItem(IDC_OPT_LLCOLORBUTTON));
	m_btnLLCol.SetDefaultColor(RGB(215,215,215));
	
	return 0;
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

	SetItem();

	m_list.DeleteAllItems();
	m_scintilla.ClearAll();
	m_scintilla.EmptyUndoBuffer();

	CustomKeywordSet* pSet = m_pScheme->GetFirstKeywordSet();
	
	int iPos = 0;

	if(!pSet)
	{
		m_scintilla.EnableWindow(FALSE);
		m_scintilla.SetText("There are no keywords for this scheme");
	}
	else
		m_scintilla.EnableWindow(TRUE);

	while(pSet)
	{
		if(pSet->pName)
		{
			int iItem = m_list.AddItem(iPos++, 0, pSet->pName);
			m_list.SetItemData(iItem, reinterpret_cast<DWORD>(pSet));
		}
		
		pSet = pSet->pNext;
	}

	m_list.SelectItem(0);

	m_bChanging = false;

	UpdateSel();
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
	m_ResetBtn.EnableWindow(bEnable);
	m_SortBtn.EnableWindow(bEnable);
}

LRESULT CTabPageKeywords::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_ResetBtn.Attach(GetDlgItem(IDC_KEYWORDS_RESETBUTTON));
	m_SortBtn.Attach(GetDlgItem(IDC_KEYWORDS_SORTBUTTON));
	m_list.Attach(GetDlgItem(IDC_KEYWORDS_LIST));

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

#include <algorithm>

LRESULT CTabPageKeywords::OnSortClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	string str;

	int len = m_scintilla.GetTextLength();
	TCHAR* pCS = new TCHAR[len+1];
	m_scintilla.GetText(len+1, pCS);
	pCS[len] = _T('\0');
	str = pCS;
	delete [] pCS;

	vector<string> tokens;

	StringTokenise(str, tokens, string(" "));
	
	std::sort(tokens.begin(), tokens.end());

	string strout;
	strout.reserve(len+1);

	for(vector<string>::iterator i = tokens.begin(); i != tokens.end(); ++i)
	{
		if(i != tokens.begin())
			strout += _T(" ");
		strout += (*i);
	}

	m_scintilla.SetText(strout.c_str());

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
		for(SL_CIT i = pColl->StylesBegin(); i != pColl->StylesEnd(); ++i)
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
		int i = m_SizeCombo.GetSelection();//GetDlgItemInt(IDC_STYLE_SIZECOMBO);
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
		m_lastTreeItem = item;

		if(item)
		{
			StyleDetails* pS = NULL;
			m_bGroup = false;

			if(m_tree.GetChildItem(item) == NULL)
			{
				pS = reinterpret_cast<StyleDetails*>(m_tree.GetItemData(item));
				if(pS)
				{
					StyleDetails* existing = m_pScheme->m_customs.GetStyle(pS->Key);
					if(existing)
						m_Style = *existing;
					else
						m_Style = *pS;
				}
			}
			else
			{
				// This is a group item, we see if there is an attached class, and if
				// so then we let the user customise it. Else we disable the controls.
				m_bGroup = true;
				
				CustomStyleCollection* pColl = reinterpret_cast<CustomStyleCollection*>( m_tree.GetItemData(item) );
				if(pColl)
				{
					// CStrings used for indexing the style maps (at the moment)
					CString classname = pColl->GetClassName();
					if(classname.GetLength() != 0)
					{
						pS = m_pScheme->FindStyleClass(classname);

						if(pS)
						{
							StyleDetails* pCustom = m_pScheme->FindCustomStyleClass(classname);

							if(pCustom)
								m_Style = *pCustom;
							else
								m_Style = *pS;
						}
					}
				}
			}

			if(pS)
			{
				m_pStyle = pS;
				
				if(m_bGroup)
				{
					tstring sname = _T("Style Group: ");
					sname += m_Style.name;
					m_sd.SetStyle(m_Style.FontName.c_str(), m_Style.FontSize, m_Style.ForeColor, m_Style.BackColor, sname.c_str(), m_Style.Bold, m_Style.Italic, m_Style.Underline);
				}
				else
					m_sd.SetStyle(m_Style.FontName.c_str(), m_Style.FontSize, m_Style.ForeColor, m_Style.BackColor, m_Style.name.c_str(), m_Style.Bold, m_Style.Italic, m_Style.Underline);

				/*if(!pS->ColourOnly)*/
				{
					m_bold.SetCheck(m_Style.Bold ? BST_CHECKED : BST_UNCHECKED);
					m_italic.SetCheck(m_Style.Italic ? BST_CHECKED : BST_UNCHECKED);
					m_underline.SetCheck(m_Style.Underline ? BST_CHECKED : BST_UNCHECKED);
					m_eolfilled.SetCheck(m_Style.EOLFilled ? BST_CHECKED : BST_UNCHECKED);
					m_fore.SetColor(m_Style.ForeColor);
					m_back.SetColor(m_Style.BackColor);

					m_FontCombo.SelectString(-1, m_Style.FontName.c_str());
					m_SizeCombo.Select(m_Style.FontSize);

					::SetWindowText(GetDlgItem(IDC_STATIC_TC), _T("Text Colour: "));

					EnableButtons(true);
				}
				/*else
				{
					m_fore.SetColor(m_Style.ForeColor);
					::SetWindowText(GetDlgItem(IDC_STATIC_TC), _T("Colour: "));
					DisableNonColourItems();
				}*/
			}
			else
				EnableButtons(false);
		}
		else
			m_pStyle = NULL;
	}
}

void CTabPageStyles::EnableButtons(bool bEnable)
{
	m_FontCombo.EnableWindow(bEnable);
	m_SizeCombo.EnableWindow(bEnable);
	m_bold.EnableWindow(bEnable);
	m_italic.EnableWindow(bEnable);
	m_underline.EnableWindow(bEnable);
	m_eolfilled.EnableWindow(bEnable);
	m_fore.EnableWindow(bEnable);
	m_back.EnableWindow(bEnable);
}

void CTabPageStyles::DisableNonColourItems()
{
	m_FontCombo.EnableWindow(false);
	m_SizeCombo.EnableWindow(false);
	m_bold.EnableWindow(false);
	m_italic.EnableWindow(false);
	m_underline.EnableWindow(false);
	m_eolfilled.EnableWindow(false);
	m_fore.EnableWindow(true);
	m_back.EnableWindow(false);
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

void CTabPageStyles::UpdateStyle()
{
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
		m_pScheme->m_customs.DeleteStyle(m_Style.Key);
	}
}

void CTabPageStyles::UpdateGroupChildren(StyleDetails* pUpdatedClass, CustomStyleCollection* pColl)
{
	// Update child items...
	if(!pColl)
	{
		pColl = reinterpret_cast<CustomStyleCollection*>( m_tree.GetItemData(m_lastTreeItem) );
	}

	m_pScheme->UpdateGroupedStyles(pColl, pUpdatedClass);
}

void CTabPageStyles::UpdateGroup()
{
	if(m_Style != *m_pStyle)
	{
		StyleDetails* pExisting = m_pScheme->FindCustomStyleClass(m_Style.name.c_str());
		if(pExisting)
		{
			*pExisting = m_Style;
		}
		else
		{
			pExisting = new StyleDetails(m_Style);
			m_pScheme->AddCustomStyleClass(CString(m_Style.name.c_str()), pExisting);
		}
	}
	else
	{
		// We're set to the original class...
		m_pScheme->RemoveCustomStyleClass(CString(m_Style.name.c_str()));
	}

	UpdateGroupChildren(&m_Style);
}

void CTabPageStyles::SetItem()
{
	if(m_pStyle)
	{
		if(m_bGroup)
			UpdateGroup();
		else
			UpdateStyle();
	}
}

LRESULT CTabPageStyles::OnResetClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pScheme)
	{
		if(m_pStyle)
		{
			if(m_bGroup)
			{
				m_pScheme->RemoveCustomStyleClass(CString(m_pStyle->name.c_str()));
			}
			else
			{
				m_pScheme->m_customs.DeleteStyle(m_pStyle->Key);
			}
			m_pStyle = NULL;
		}
	
		UpdateSel();

		if(m_bGroup)
			UpdateGroupChildren(&m_Style);
	}

	return 0;
}

LRESULT CTabPageStyles::OnResetAllClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pScheme)
	{
		m_pStyle = NULL;
		
		m_pScheme->ResetAll();

		UpdateSel();
	}
	
	return 0;
}

//////////////////////////////////////////////////////////////////////////////
// CTabPageMisc
//////////////////////////////////////////////////////////////////////////////

CTabPageMisc::CTabPageMisc()
{
	m_bChanging = false;
	m_pScheme = NULL;
	m_bDirty = false;
}

bool CTabPageMisc::IsDirty()
{
	return m_bDirty;
}

void CTabPageMisc::SetScheme(SchemeConfig* pScheme)
{
	SetValues();

	m_pScheme = pScheme;

	UpdateDisplay();
}

void CTabPageMisc::SetValues()
{
	if(m_pScheme != NULL && ::IsWindow(m_hWnd))
	{
		COLORREF theColour;

		// Clear existing customisations.
		m_pScheme->m_editorColours.Clear();
		
		if( m_selUseExistingFore.GetCheck() == BST_CHECKED )
			theColour = -1;
		else
			theColour = m_selFore.GetColor();
		m_pScheme->m_editorColours.SetColour( EditorColours::ecSelFore, theColour);
		
		theColour = m_selBack.GetColor();
		if(theColour != -1)
			m_pScheme->m_editorColours.SetColour( EditorColours::ecSelBack, theColour);

		theColour = m_cursorCol.GetColor();
		if(theColour != -1)
			m_pScheme->m_editorColours.SetColour( EditorColours::ecCaret, theColour);

		theColour = m_igCol.GetColor();
		if(theColour != -1)
			m_pScheme->m_editorColours.SetColour( EditorColours::ecIndentG, theColour);
	}
}

void CTabPageMisc::Finalise()
{
	SetValues();
}

LRESULT CTabPageMisc::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_selFore.SubclassWindow(GetDlgItem(IDC_STYLE_SELFOREBUTTON));
	m_selBack.SubclassWindow(GetDlgItem(IDC_STYLE_SELBACKBUTTON));
	m_cursorCol.SubclassWindow(GetDlgItem(IDC_STYLE_CURCOLBUTTON));
	m_igCol.SubclassWindow(GetDlgItem(IDC_STYLE_IGCOLBUTTON));

	m_selFore.SetDefaultColor(::GetSysColor(COLOR_HIGHLIGHTTEXT));
	m_selBack.SetDefaultColor(::GetSysColor(COLOR_HIGHLIGHT));
	m_cursorCol.SetDefaultColor(::GetSysColor(COLOR_WINDOWTEXT));
	m_igCol.SetDefaultColor(RGB(0,0,0));

	m_selUseExistingFore.Attach( GetDlgItem(IDC_STYLE_SELUSEFORE) );

	if(m_pScheme)
		UpdateDisplay();

	return 0;
}

LRESULT CTabPageMisc::OnValueChanged(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& bHandled)
{
	bHandled = false;

	m_bDirty = true;

	return 0;
}

LRESULT CTabPageMisc::OnSelUseForeClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	EnableButtons();

	m_bDirty = true;

	return 0;
}

void CTabPageMisc::EnableButtons()
{
	m_selFore.EnableWindow( m_selUseExistingFore.GetCheck() != BST_CHECKED );
}

void CTabPageMisc::UpdateDisplay()
{
	if( ::IsWindow(m_hWnd) && (m_pScheme != NULL) )
	{
		COLORREF theColour;
		if(	m_pScheme->m_editorColours.GetColour( EditorColours::ecSelFore, theColour) )
		{
			if(theColour == -1)
				m_selUseExistingFore.SetCheck(BST_CHECKED);
			else
				m_selFore.SetColor( theColour );
		}

		if( m_pScheme->m_editorColours.GetColour( EditorColours::ecSelBack, theColour) )
			m_selBack.SetColor( theColour );

		if( m_pScheme->m_editorColours.GetColour( EditorColours::ecCaret, theColour) )
			m_cursorCol.SetColor( theColour );
	    
		if( m_pScheme->m_editorColours.GetColour( EditorColours::ecIndentG, theColour) )
			m_igCol.SetColor( theColour );

		EnableButtons();
	}
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
		
		m_FontCombo.SelectString(-1, pStyle->FontName.c_str());
		m_SizeCombo.Select(pStyle->FontSize);
		
		m_fore.SetColor(pStyle->ForeColor);
		m_fore.SetDefaultColor(RGB(0,0,0));
		
		m_back.SetColor(pStyle->BackColor);
		m_back.SetDefaultColor(RGB(255,255,255));

		m_bold.SetCheck(pStyle->Bold ? BST_CHECKED : BST_UNCHECKED);
		m_italic.SetCheck(pStyle->Italic ? BST_CHECKED : BST_UNCHECKED);
		m_underline.SetCheck(pStyle->Underline ? BST_CHECKED : BST_UNCHECKED);

		// Simple dirty checking - if the page is shown we rebuild.
		m_bDirty = true;
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
		pS->FontSize = pS->FontSize = GetDlgItemInt(IDC_FONTSIZE_COMBO);
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
	SchemeConfig* pScheme = m_combo.GetItemScheme(i);
	m_stylestab.SetScheme(pScheme);
	m_keywordstab.SetScheme(pScheme);
	m_misctab.SetScheme(pScheme);
}

//////////////////////////////////////////////////////////////////////////////
// COptionsPageTools
//////////////////////////////////////////////////////////////////////////////

COptionsPageTools::COptionsPageTools(SchemeConfigParser* pSchemes)
{
	m_pSchemes = pSchemes;
	m_pScheme = NULL;
	m_pCurrent = NULL;
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
	m_combo.SetWindowPos(HWND_TOP, &rcCombo, 0);

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
	m_toolstore.Save();
}

void COptionsPageTools::Update()
{
	m_bChanging = true;

	m_pCurrent = NULL;
	m_list.DeleteAllItems();

	int iSel = m_combo.GetCurSel();
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
			m_pCurrent = m_toolstore.GetGlobalTools();
		}
	}
	else
		m_pScheme = NULL;

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

SchemeTools* COptionsPageTools::GetTools()
{
	if(!m_pCurrent)
	{
		m_pCurrent = m_toolstore.GetToolsFor(m_pScheme->m_Name);
	}
		
	return m_pCurrent;
}

void COptionsPageTools::EnableButtons()
{
	if(m_bChanging)
		return;

	bool bEnable = (m_pScheme != NULL || m_combo.GetCurSel() == 0);
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
	CToolEditorDialog dlg;
	
	if (dlg.DoModal() == IDOK)
	{
		//@todo check if the name is valid...

		SourcedToolDefinition* pDef = new SourcedToolDefinition(m_toolstore.GetDefaultToolStore());
		GetTools()->Add(pDef);
		dlg.GetValues(pDef);

		AddDefinition(pDef);
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
			CToolEditorDialog dlg;
			dlg.SetValues(pDef);
			dlg.SetTitle(_T("Edit Tool"));

			if(dlg.DoModal())
			{
				dlg.GetValues(pDef);

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

struct ListCtrlMoveOneData
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
}

LRESULT COptionsPageTools::OnUpClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int iSelIndex = m_list.GetSelectedIndex();
	if( iSelIndex != -1 )
	{
		ToolDefinition* pDef = reinterpret_cast<ToolDefinition*>(m_list.GetItemData(iSelIndex));
		GetTools()->MoveUp(pDef);
		//ListCtrlMoveOneData mod = {reinterpret_cast<LPARAM>(pDef), 0, 0};
		//m_list.SortItems(ListCtrlMoveOneCompareFunc, reinterpret_cast<LPARAM>(&mod));
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
		//ListCtrlMoveOneData mod = {reinterpret_cast<LPARAM>(pDef), 1, 0};
		//m_list.SortItems(ListCtrlMoveOneCompareFunc, reinterpret_cast<LPARAM>(&mod));
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

	CScheme* pScheme = CSchemeManager::GetInstance()->SchemeByName(schemename);
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
	COptionsManager* pOM = COptionsManager::GetInstance();
	tstring strNewScheme = pOM->Get(PNSK_EDITOR, _T("NewScheme"), _T("Plain Text"));

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
		pOM->Get(PNSK_EDITOR, _T("SmartStart"), true) ? BST_CHECKED : BST_UNCHECKED
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
			SchemeConfig* pS = m_combo.GetItemScheme(selIndex);
			if(pS)
				wt = pS->m_Name;
			else
				wt = _T("Plain Text");
			COptionsManager::GetInstance()->Set(PNSK_EDITOR, _T("NewScheme"), wt);

			// Enable SmartStart?
			CButton button(GetDlgItem(IDC_SMARTSTART_ENABLECHECK));
			COptionsManager::GetInstance()->Set(PNSK_EDITOR, _T("SmartStart"), button.GetCheck() == BST_CHECKED);
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
		AlternateFileSet* pSet = new AlternateFileSet(set1.c_str(), set2.c_str());
		sets->push_back(pSet);

		addItem(set1.c_str(), set2.c_str(), pSet);

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