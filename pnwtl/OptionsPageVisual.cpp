/**
 * @file OptionsPageVisual.cpp
 * @brief Visual Help Options Page
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "OptionsDialogs.h"
#include "OptionsPageVisual.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

//////////////////////////////////////////////////////////////////////////////
// COptionsPageVisual
//////////////////////////////////////////////////////////////////////////////

tstring COptionsPageVisual::GetTreePosition()
{
	return MAKE_OPTIONSTREEPATH(IDS_OPTGROUP_GENERAL, IDS_OPTPAGE_VISUALHELP);
}

void COptionsPageVisual::OnOK()
{
	if(!m_bCreated)
		return;

	DoDataExchange(TRUE);

	Options& options = *OPTIONS;
	options.SetCached(Options::OShowIndentGuides, m_bIndentGuides != FALSE);
	options.SetCached(Options::OFoldingEnabled, m_bFolding != FALSE);
	options.SetCached(Options::OLineHighlight, m_bLineHighlight != FALSE);
	options.SetCached(Options::OLineHighlightColour, m_btnLineCol.SafeGetColor());
	options.SetCached(Options::ORightGuide, m_iLongLineHelp);
	options.SetCached(Options::ORightColumn, m_iRightColumn);
	options.SetCached(Options::ORightGuideColour, m_btnLLCol.SafeGetColor());
	options.SetCached(Options::OSmartHighlight, m_bSmartHighlight);
	options.SetCached(Options::OLinePaddingTop, m_iLinePaddingTop);
	options.SetCached(Options::OLinePaddingBottom, m_iLinePaddingBottom);

	if (m_bLineHighlightAlpha)
	{
		OPTIONS->SetCached(Options::OLineHighlightAlpha, m_trackerHighlight.GetPos());
	}
	else
	{
		OPTIONS->SetCached(Options::OLineHighlightAlpha, SC_ALPHA_NOALPHA);
	}
}

void COptionsPageVisual::OnInitialise()
{
	m_bIndentGuides		= OPTIONS->GetCached(Options::OShowIndentGuides);
	m_bFolding			= OPTIONS->GetCached(Options::OFoldingEnabled);
	m_bLineHighlight	= OPTIONS->GetCached(Options::OLineHighlight);
	m_iLongLineHelp		= OPTIONS->GetCached(Options::ORightGuide);
	m_iRightColumn		= OPTIONS->GetCached(Options::ORightColumn);
	m_bSmartHighlight	= OPTIONS->GetCached(Options::OSmartHighlight);
	m_iLinePaddingTop   = OPTIONS->GetCached(Options::OLinePaddingTop);
	m_iLinePaddingBottom   = OPTIONS->GetCached(Options::OLinePaddingBottom);
	
	int lineHighlightAlpha = OPTIONS->GetCached(Options::OLineHighlightAlpha);
	if (lineHighlightAlpha == SC_ALPHA_NOALPHA)
	{
		m_bLineHighlightAlpha = FALSE;
		lineHighlightAlpha = 30;
	}
	else
	{
		m_bLineHighlightAlpha = TRUE;
	}
	
	m_btnLineCol.SetColor( OPTIONS->GetCached(Options::OLineHighlightColour) );
	m_btnLLCol.SetColor( OPTIONS->GetCached(Options::ORightGuideColour) );
	m_trackerHighlight.SetPos( lineHighlightAlpha );

	DoDataExchange();
}

LRESULT COptionsPageVisual::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_settingsHeader.SubclassWindow(GetDlgItem(IDC_SETTINGS_STATIC));
	m_linesHeader.SubclassWindow(GetDlgItem(IDC_LINEHELPERS_STATIC));
	m_paddingHeader.SubclassWindow(GetDlgItem(IDC_LINEPADDING_STATIC));

	m_btnLineCol.SubclassWindow(GetDlgItem(IDC_OPT_LINELIGHTBUTTON));
	m_btnLineCol.SetDefaultColor(RGB(255, 255, 224));
	m_btnLLCol.SubclassWindow(GetDlgItem(IDC_OPT_LLCOLORBUTTON));
	m_btnLLCol.SetDefaultColor(RGB(215,215,215));

	m_trackerHighlight.Attach(GetDlgItem(IDC_LINEHIGHLIGHTSLIDER));
	m_trackerHighlight.SetRange(0, 255);
	m_trackerHighlight.SetPageSize(32);

	m_trackerHighlight.ModifyStyle(0, TBS_FIXEDLENGTH);

	int h = m_trackerHighlight.GetThumbLength();
	h -= 2;

	CRect rc;
	m_trackerHighlight.GetWindowRect(&rc);
	rc.bottom += 2;
	m_trackerHighlight.SetWindowPos(NULL, &rc, SWP_NOZORDER | SWP_NOMOVE);
	m_trackerHighlight.SetThumbLength(h);

	//TODO: Enable/disable IDC_OPT_LLCOLUMNEDIT, IDC_OPT_LLCOLORBUTTON and the associated static text based on the radio selection
	//TODO: Enable/disable IDC_OPT_LINELIGHTBUTTON based on the checkbox selection
	return 0;
}
