/**
 * @file OptionsPageStyle.cpp
 * @brief Style Options Page
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "OptionsPageStyle.h"

//////////////////////////////////////////////////////////////////////////////
// COptionsPageStyle
//////////////////////////////////////////////////////////////////////////////

bool COptionsPageStyle::IsDirty()
{
	return m_bDirty;
}

LRESULT COptionsPageStyle::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	// Header:
	m_settingsHeader.SubclassWindow(GetDlgItem(IDC_SETTINGS_STATIC));

	// Style controls:
	m_FontCombo.SubclassWindow(GetDlgItem(IDC_FONT_COMBO));
	m_SizeCombo.Attach(GetDlgItem(IDC_FONTSIZE_COMBO));

	m_fore.SubclassWindow(GetDlgItem(IDC_STYLE_FOREBUTTON));
	m_back.SubclassWindow(GetDlgItem(IDC_STYLE_BACKBUTTON));

	m_cur.SubclassWindow(GetDlgItem(IDC_STYLE_CURCOLBUTTON));
	m_indentGuides.SubclassWindow(GetDlgItem(IDC_STYLE_IGCOLBUTTON2));
	m_selFore.SubclassWindow(GetDlgItem(IDC_STYLE_SELFOREBUTTON));
	m_selBack.SubclassWindow(GetDlgItem(IDC_STYLE_SELBACKBUTTON2));
	m_markAll.SubclassWindow(GetDlgItem(IDC_STYLE_MARKALLCOLBUTTON));
	m_smartHighlight.SubclassWindow(GetDlgItem(IDC_STYLE_SELWORDCOLBUTTON));
	m_templateField.SubclassWindow(GetDlgItem(IDC_STYLE_TFCOLBUTTON));

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

void DisplayIfSet(CPNColorButton& button, EditorColours* colours, EditorColours::Colours colour)
{
	COLORREF c;
	if (colours->GetColour(colour, c))
	{
		button.SetColor(c);
	}
}

void StoreIfSet(CPNColorButton& button, EditorColours* colours, EditorColours::Colours colour)
{
	COLORREF c(button.GetColor());
	if(c != CLR_DEFAULT)
		colours->SetColour(colour, c);
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
	m_indentGuides.SetDefaultColor(RGB(0, 0, 0));
	m_markAll.SetDefaultColor(DEFAULT_MARKALL_COLOUR);
	m_smartHighlight.SetDefaultColor(DEFAULT_SMARTHIGHLIGHT_COLOUR);
	m_templateField.SetDefaultColor(DEFAULT_TEXTCLIPFIELD_COLOUR);

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
	
	DisplayIfSet(m_selBack, ec, EditorColours::ecSelBack);
	DisplayIfSet(m_cur, ec, EditorColours::ecCaret);
	DisplayIfSet(m_indentGuides, ec, EditorColours::ecIndentG);
	DisplayIfSet(m_markAll, ec, EditorColours::ecMarkAll);
	DisplayIfSet(m_smartHighlight, ec, EditorColours::ecSmartHL);
	DisplayIfSet(m_templateField, ec, EditorColours::ecTemplateField);
	
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
		
	display.FontName	= m_FontCombo.GetSelFontName();
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
	
	StoreIfSet(m_selBack, ec, EditorColours::ecSelBack);
	StoreIfSet(m_cur, ec, EditorColours::ecCaret);
	StoreIfSet(m_indentGuides, ec, EditorColours::ecIndentG);
	StoreIfSet(m_markAll, ec, EditorColours::ecMarkAll);
	StoreIfSet(m_smartHighlight, ec, EditorColours::ecSmartHL);
	StoreIfSet(m_templateField, ec, EditorColours::ecTemplateField);
}

void COptionsPageStyle::OnCancel()
{
}

tstring COptionsPageStyle::GetTreePosition()
{
	return L10N::StringLoader::Get(IDS_OPTGROUP_STYLES);
}