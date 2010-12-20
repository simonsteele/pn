/**
 * @file StyleTabPages.cpp
 * @brief Style Tab Pages for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "OptionsDialogs.h"
#include "StyleTabPages.h"

//////////////////////////////////////////////////////////////////////////////
// CTabPageKeywords
//////////////////////////////////////////////////////////////////////////////

CTabPageKeywords::CTabPageKeywords()
{
	m_pSet = NULL;
	m_bChanging = false;
}

void CTabPageKeywords::SetScheme(SchemeDetails* pScheme)
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

	CustomKeywordSet* pSet = m_pScheme->Keywords.GetFirstKeywordSet();
	
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
		char* pCS = new char[len+1];
		m_scintilla.GetText(len+1, pCS);
		pCS[len] = '\0';
		
		if(strcmp(m_pSet->pWords, pCS) != 0)
		{
			CustomKeywordSet* pCustomSet = m_pScheme->CustomKeywords.FindKeywordSet(m_pSet->key);

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
				m_pScheme->CustomKeywords.AddKeywordSet(pNewSet);
			}
		}
		else
		{
			CustomKeywordSet* pCustomSet = m_pScheme->CustomKeywords.FindKeywordSet(m_pSet->key);
			if(pCustomSet)
				m_pScheme->CustomKeywords.DeleteKeywordSet(pCustomSet);
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
				CustomKeywordSet* pCustomSet = m_pScheme->CustomKeywords.FindKeywordSet(m_pSet->key);

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
	m_scintilla.Create(m_hWnd, rcScintilla, _T("Keywords"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_TABSTOP);
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
	std::string str;

	int len = m_scintilla.GetTextLength();
	char* pCS = new char[len+1];
	m_scintilla.GetText(len+1, pCS);
	pCS[len] = _T('\0');
	str = pCS;
	delete [] pCS;

	std::vector<std::string> tokens;

	StringTokenise(str, tokens, std::string(" "));
	
	std::sort(tokens.begin(), tokens.end());

	std::string strout;
	strout.reserve(len+1);

	for(std::vector<std::string>::const_iterator i = tokens.begin(); i != tokens.end(); ++i)
	{
		if(i != tokens.begin())
			strout += " ";
		strout += (*i);
	}

	m_scintilla.SetText(strout.c_str());

	return 0;
}

LRESULT CTabPageKeywords::OnListSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	NMLISTVIEW* plv = (LPNMLISTVIEW)pnmh;
	if(plv->uChanged == LVIF_STATE && (plv->uNewState & LVIS_SELECTED) )
	{
		UpdateSel();
	}

	return 0;
}

//////////////////////////////////////////////////////////////////////////////
// CTabPageStyles
//////////////////////////////////////////////////////////////////////////////

CTabPageStyles::CTabPageStyles(SchemeConfigParser* pSchemes)
{
	m_pStyle = NULL;
	m_bChanging = false;
	m_pSchemes = pSchemes;
}

void CTabPageStyles::SetScheme(SchemeDetails* pScheme)
{
	// Store any changes
	Finalise();

	m_bChanging = true;
	m_pStyle = NULL;
	m_pScheme = pScheme;

	m_tree.DeleteAllItems();

	HTREEITEM insertunder = TVI_ROOT;

	GroupDetailsList::const_iterator iGD = pScheme->GroupDetails.begin();
	
	for(StylePtrList::const_iterator i = pScheme->Styles.begin(); i != pScheme->Styles.end(); ++i)
	{
		if( (*i)->Style->values & edvGroupStart )
		{
			insertunder = m_tree.InsertItem( (*iGD).name.c_str(), NULL, NULL );
			m_tree.SetItemData(insertunder, reinterpret_cast<DWORD_PTR>(&(*iGD)));
			++iGD;
		}
		else if( (*i)->Style->values & edvGroupEnd )
		{
			insertunder = TVI_ROOT;
		}
		else
		{
			HTREEITEM hi = m_tree.InsertItem((*i)->Style->name.c_str(), insertunder, TVI_LAST);
			m_tree.SetItemData(hi, reinterpret_cast<DWORD_PTR>((*i).get()));
		}
		
		if(insertunder != TVI_ROOT)
			m_tree.Expand(insertunder, TVE_EXPAND);
	}

	m_tree.EnsureVisible(m_tree.GetRootItem());

	m_bChanging = false;
}

void CTabPageStyles::Finalise()
{
	SetItem();
}

/**
 * Called because styles have been reset for some reason,
 * drop any pending customisations and re-display
 */
void CTabPageStyles::UpdateDisplay()
{
	m_pStyle = NULL;

	UpdateSel();
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
	if (m_pStyle)
	{
		m_Style.FontName = m_FontCombo.GetSelFontName();
	}

	return 0;
}

LRESULT CTabPageStyles::OnSizeChanged(WORD wNotifyCode, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pStyle)
	{
		int i = m_SizeCombo.GetSelection((wNotifyCode != CBN_SELCHANGE));//GetDlgItemInt(IDC_STYLE_SIZECOMBO);
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

		m_pStyle = NULL;

		HTREEITEM item = m_tree.GetSelectedItem();
		m_lastTreeItem = item;

		if(item)
		{
			FullStyleDetails* pS = NULL;
			m_bGroup = false;

			if(m_tree.GetChildItem(item) == NULL)
			{
				pS = reinterpret_cast<FullStyleDetails*>(m_tree.GetItemData(item));
				if(pS)
				{
					pS->Combine( m_pSchemes->GetDefaultStyle(), m_Style );
				}
			}
			else
			{
				// This is a group item, we see if there is an attached class, and if
				// so then we let the user customise it. Else we disable the controls.
				m_bGroup = true;
				
				GroupDetails_t* pGD = reinterpret_cast<GroupDetails_t*>( m_tree.GetItemData(item) );
				if(pGD)
				{
					if(pGD->classname.length() != 0)
					{
						pS = m_pSchemes->GetClass(pGD->classname.c_str()).get();

						if(pS)
						{
							pS->Combine( m_pSchemes->GetDefaultStyle(), m_Style );
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
				{
					m_sd.SetStyle(m_Style.FontName.c_str(), m_Style.FontSize, m_Style.ForeColor, m_Style.BackColor, m_Style.name.c_str(), m_Style.Bold, m_Style.Italic, m_Style.Underline);
				}

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
			else
			{
				EnableButtons(false);
			}
		}
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
	m_pStyle->CheckCustomisation(m_pSchemes->GetDefaultStyle(), m_Style);
}

void CTabPageStyles::UpdateGroup()
{
	if(m_Style != *m_pStyle->Style)
	{
		if(m_pStyle->CustomStyle)
		{
			*m_pStyle->CustomStyle = m_Style;
		}
		else
		{
			m_pStyle->CustomStyle = new StyleDetails(m_Style);
		}

		m_pStyle->CustomStyle->compareTo(*m_pStyle->Style);
	}
	else
	{
		// We're set to the original class...
		m_pStyle->Reset();
	}
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
			if(m_pStyle->CustomStyle != NULL)
			{
				delete m_pStyle->CustomStyle;
				m_pStyle->CustomStyle = NULL;
			}
			
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

void CTabPageMisc::SetScheme(SchemeDetails* pScheme)
{
	SetValues();

	m_pScheme = pScheme;

	UpdateDisplay();
}

void CTabPageMisc::SetValues()
{
	if(m_pScheme != NULL && ::IsWindow(m_hWnd))
	{
		DoDataExchange(true);

		COLORREF theColour;

		// Clear existing customisations.
		m_pScheme->CustomColours.Clear();
		
		if( m_selUseExistingFore.GetCheck() == BST_CHECKED )
			theColour = CLR_NONE;
		else
			theColour = m_selFore.GetColor();
		
		if(theColour != CLR_DEFAULT)
			m_pScheme->CustomColours.SetColour(EditorColours::ecSelFore, theColour);
		
		theColour = m_selBack.GetColor();
		if(theColour != CLR_DEFAULT)
			m_pScheme->CustomColours.SetColour(EditorColours::ecSelBack, theColour);

		theColour = m_cursorCol.GetColor();
		if(theColour != CLR_DEFAULT)
			m_pScheme->CustomColours.SetColour(EditorColours::ecCaret, theColour);

		theColour = m_igCol.GetColor();
		if(theColour != CLR_DEFAULT)
			m_pScheme->CustomColours.SetColour(EditorColours::ecIndentG, theColour);

		switch(m_iTabOverride)
		{
		case 0:
			{
				m_pScheme->CustomFlags &= ~(schOverrideTabs|schUseTabs);
			}
			break;

		case 1:
			{
				m_pScheme->CustomFlags |= (schOverrideTabs | schUseTabs);
			}
			break;

		case 2:
			{
				m_pScheme->CustomFlags |= schOverrideTabs;
				m_pScheme->CustomFlags &= ~schUseTabs;
			}
			break;
		}
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

LRESULT CTabPageMisc::OnValueChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& bHandled)
{
	bHandled = false;

	m_bDirty = true;

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
		if(	m_pScheme->Colours.GetColour( EditorColours::ecSelFore, theColour) )
		{
			if(theColour == -1)
				m_selUseExistingFore.SetCheck(BST_CHECKED);
			else
				m_selFore.SetColor( theColour );
		}
		else
		{
			m_selFore.SetColor( CLR_DEFAULT );
			m_selUseExistingFore.SetCheck(BST_UNCHECKED);
		}

		if( m_pScheme->CustomColours.GetColour( EditorColours::ecSelBack, theColour) )
			m_selBack.SetColor( theColour );
		else
			m_selBack.SetColor( CLR_DEFAULT );

		if( m_pScheme->CustomColours.GetColour( EditorColours::ecCaret, theColour) )
			m_cursorCol.SetColor( theColour );
		else
			m_cursorCol.SetColor( CLR_DEFAULT );
	    
		if( m_pScheme->CustomColours.GetColour( EditorColours::ecIndentG, theColour) )
			m_igCol.SetColor( theColour );
		else
			m_igCol.SetColor( CLR_DEFAULT );

		switch(m_pScheme->CustomFlags & (schOverrideTabs | schUseTabs))
		{
		case 0:
			m_iTabOverride = 0;
			break;
		case schOverrideTabs:
			m_iTabOverride = 2;
			break;
		default:
			m_iTabOverride = 1;
		}

		DoDataExchange();
		EnableButtons();
	}
}