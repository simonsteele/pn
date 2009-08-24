/**
 * @file splitview.cpp
 * @brief Split View
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "splitview.h"

using namespace Views;

/**
 * Splitter Constructor
 */
SplitView::SplitView(ESplitType splitType, ViewPtr& parent, HWND view1, HWND view2) : 
	  View(vtSplit, parent), 
	  m_splitType(splitType), 
	  m_w1(view1), 
	  m_w2(view2) 
{
	m_wnd.SetPanes(m_w1, m_w2, false);
	m_wnd.DisableSinglePaneMode(false);
	m_wnd.SetHorizontal(m_splitType == splitHorz);
}

HWND SplitView::Create(HWND hWndOwner, LPRECT rc, int controlId)
{
    HWND wnd = m_wnd.Create(hWndOwner, rc, _T("ViewSplitter"), 0, 0, controlId);
	m_wnd.CentreSplit();
	return wnd;
}

void SplitView::UpdateLayout()
{
	m_wnd.UpdateLayout();
}