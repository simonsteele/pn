/**
 * @file optionscontrols.cpp
 * @brief Controls for options dialogs (and the like).
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "optionscontrols.h"

#include "schemeconfig.h"

#include "outputview.h"
#include "third_party/scintilla/include/scilexer.h"

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
	HBRUSH light = ::GetSysColorBrush(COLOR_3DSHADOW);
	dc.FrameRect(rc, light);
	
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
// CScintillaREDialogWnd
//////////////////////////////////////////////////////////////////////////////

int CScintillaREDialogWnd::HandleNotify(LPARAM lParam)
{
	Scintilla::SCNotification *scn = (Scintilla::SCNotification*)lParam;
	
	if( scn->nmhdr.code == SCN_HOTSPOTCLICK )
	{
		int style = GetStyleAt(scn->position);
		GetParent().PostMessage(PN_HANDLEHSCLICK, style, scn->position);

		return 0;
	}
	else
		return baseClass::HandleNotify(lParam);
}

//////////////////////////////////////////////////////////////////////////////
// CSchemeCombo
//////////////////////////////////////////////////////////////////////////////

int CSchemeCombo::AddScheme(LPCTSTR title, SchemeDetails* pScheme)
{
	int index = AddString(title);
	SetItemDataPtr(index, pScheme);
	return index;
}

void CSchemeCombo::Load(SchemeConfigParser* pConfig, LPCSTR selectScheme, bool bIncludePlainText, bool bIncludeInternal)
{
	LPCSTR schemeToSelect = (selectScheme ? selectScheme : pConfig->GetCurrentScheme());

	Clear();

	tstring schemeText;

	if (bIncludePlainText)
	{
		schemeText = LS(IDS_DEFAULTSCHEME);
		int index = AddString(schemeText.c_str());
		SetItemDataPtr(index, pConfig->GetPlainTextScheme());
	}

	for (SchemeDetailsList::const_iterator i = pConfig->GetSchemes().begin(); i != pConfig->GetSchemes().end(); ++i)
	{
		// Skip internal (special) schemes?
		if (!bIncludeInternal && (*i)->IsInternal())
			continue;

		int index = AddString((*i)->Title.c_str());
		SetItemDataPtr(index, (*i));
		if((*i)->Name == schemeToSelect)
			schemeText = (*i)->Title;
	}
	
	if (schemeText.size())
	{
		SelectString(0, schemeText.c_str());
	}
	else
	{
		// Default: select the first scheme.
		SetCurSel(0);
	}
}

SchemeDetails* CSchemeCombo::GetItemScheme(int index)
{
	return static_cast<SchemeDetails*>(GetItemDataPtr(index));
}

LRESULT CPNHotkeyCtrl::OnKeyDown(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = false;

	if (wParam == VK_DELETE || wParam == VK_SPACE || wParam == VK_ESCAPE)
	{
		bHandled = true;
	}
	
	return 0;
}

LRESULT CPNHotkeyCtrl::OnKeyUp(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = false;

	// For some reason space is still clearing the box before we get to here, so there're no
	// modifiers left.
	if (wParam == VK_DELETE || wParam == VK_SPACE || wParam == VK_ESCAPE)
	{
		int current = SendMessage(HKM_GETHOTKEY, 0, 0);
		if ((current & 0xff) != 0)
		{
			// No low-order byte value means no current key,
			// we don't want to use the current modifiers.
			current = 0;
		}

		if (wParam == VK_DELETE)
		{
			// Set extended to make sure we get DEL and not NUM DECIMAL
			current |= (HOTKEYF_EXT << 8);
		}
		
		int key = wParam | current;
		SendMessage(HKM_SETHOTKEY, key, 0);

		SendMessageW(GetParent(), WM_COMMAND, MAKEWPARAM(GetDlgCtrlID(), EN_CHANGE), (LPARAM)m_hWnd);

		bHandled = true;
	}

	return 0;
}

LRESULT CPNHotkeyCtrl::OnChar(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = false;

	if (wParam == ' ')
	{
		bHandled = true;
	}

	return 0;
}

LRESULT CPNHotkeyCtrl::OnGetDlgCode(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	return DLGC_WANTALLKEYS;
}