#include "stdafx.h"
#include "optionscontrols.h"

#include "schemeconfig.h"

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
// CSchemeCombo
//////////////////////////////////////////////////////////////////////////////

int CSchemeCombo::AddScheme(LPCTSTR title, SchemeConfig* pScheme)
{
	int index = AddString(title);
	SetItemDataPtr(index, pScheme);
	return index;
}

void CSchemeCombo::Load(SchemeConfigParser* pConfig, LPCTSTR selectScheme, bool bIncludePlainText, bool bIncludeInternal)
{
	int index = 0;
	int selIndex = 0;
	LPCTSTR schemeToSelect = (selectScheme ? selectScheme : pConfig->GetCurrentScheme());

	Clear();

	if( bIncludePlainText )
	{
		index = AddString(_T("Plain Text"));
		SetItemDataPtr(index, pConfig->GetPlainTextScheme());
	}

	for(SCF_IT i = pConfig->GetSchemes().begin(); i != pConfig->GetSchemes().end(); ++i)
	{
		// Skip internal (special) schemes?
		if( !bIncludeInternal && (*i)->IsInternal() )
			continue;

		index = AddString((*i)->m_Title);
		SetItemDataPtr(index, (*i));
		if(_tcscmp((*i)->m_Name, schemeToSelect) == 0)
			selIndex = index;
	}
	
	if(GetCount() > 0)
	{
		SetCurSel(selIndex);
	}
}

SchemeConfig* CSchemeCombo::GetItemScheme(int index)
{
	return static_cast<SchemeConfig*>(GetItemDataPtr(index));
}