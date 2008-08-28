/**
 * @file OptionsPageEditing.cpp
 * @brief Caret positioning and other editing options
 * @author Simon Steele
 * @note Copyright (c) 2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "OptionsPageEditing.h"

COptionsPageEditing::COptionsPageEditing()
{
}

void COptionsPageEditing::OnOK()
{
	if(!m_bCreated)
		return;

	DoDataExchange(TRUE);

	int xflags = OPTIONS->GetCached(Options::OCaretXFlags);
	int yflags = OPTIONS->GetCached(Options::OCaretYFlags);
	
	if (m_strict)
	{
		xflags |= CARET_STRICT;
		yflags |= CARET_STRICT;
	}
	else
	{
		xflags &= ~CARET_STRICT;
		yflags &= ~CARET_STRICT;
	}

	OPTIONS->SetCached(Options::OCaretXFlags, xflags);
	OPTIONS->SetCached(Options::OCaretYFlags, yflags);
	OPTIONS->SetCached(Options::OCaretXMove, m_slopX);
	OPTIONS->SetCached(Options::OCaretYMove, m_slopY);
}

void COptionsPageEditing::OnInitialise()
{
	m_slopX = OPTIONS->GetCached(Options::OCaretXMove);
	m_slopY = OPTIONS->GetCached(Options::OCaretYMove);
	int xflags = OPTIONS->GetCached(Options::OCaretXFlags);
	int yflags = OPTIONS->GetCached(Options::OCaretYFlags);
	m_strict = (xflags & CARET_STRICT) != 0;

	DoDataExchange();
}

LPCTSTR COptionsPageEditing::GetTreePosition()
{
	return _T("General\\Editing");
}

void COptionsPageEditing::OnCancel()
{
}

LRESULT COptionsPageEditing::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	return 0;
}