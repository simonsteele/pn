/**
 * @file pntabcontrol.h
 * @brief Tab Control for PN
 * @author Simon Steele
 * @note Copyright (c) 2010 Simon Steele - http://untidy.net/
 *
 * Based on the CustomTabControl and modified from the VS tabs implementation there
 * with the intent of creating a cleaner look.
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef PNTABCONTROL_H__INCLUDED
#define PNTABCONTROL_H__INCLUDED

#ifndef __CUSTOMTABCTRL_H__
#include "CustomTabCtrl.h"
#endif

/**
 * PN Main Tab Control.
 */
class CPNTabControl : public CCustomTabCtrl<CPNTabControl, CTabViewTabItem, ATL::CWindow, CCustomTabCtrlWinTraits>
{
protected:
	typedef CCustomTabCtrl<CPNTabControl, CTabViewTabItem, ATL::CWindow, CCustomTabCtrlWinTraits> baseClass;

protected:
	CBrush m_hbrBackground;
	COLORREF m_clrTextInactiveTab, m_clrSelectedTab;

	signed char m_nFontSizeTextTopOffset;

	const signed char m_nMinWidthToDisplayText;

// Constructor
public:
	CPNTabControl() :
	  m_clrTextInactiveTab(::GetSysColor(COLOR_BTNTEXT)),
		  m_clrSelectedTab(::GetSysColor(COLOR_WINDOW)),
		m_nFontSizeTextTopOffset(0),
		m_nMinWidthToDisplayText(12)
	{
	}

// Message Handling
public:
	DECLARE_WND_CLASS_EX(_T("PNTabControl"), CS_DBLCLKS, COLOR_WINDOW)

	BEGIN_MSG_MAP(CPNTabControl)
		MESSAGE_HANDLER(WM_SETTINGCHANGE, OnSettingChange)
		MESSAGE_HANDLER(WM_SYSCOLORCHANGE, OnSettingChange)
		MESSAGE_HANDLER(WM_THEMECHANGED, OnSettingChange)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	LRESULT OnSettingChange(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	// Overrideables
public:
	void DrawBackground(RECT rcClient, LPNMCTCCUSTOMDRAW lpNMCustomDraw);

	void DrawItem_InitBounds(DWORD dwStyle, RECT rcItem, RECT& rcTab, RECT& rcText, int& nIconVerticalCenter);
	void DrawItem_TabSelected(DWORD dwStyle, LPNMCTCCUSTOMDRAW lpNMCustomDraw, RECT& rcTab);
	void DrawItem_TabInactive(DWORD dwStyle, LPNMCTCCUSTOMDRAW lpNMCustomDraw, RECT& rcTab);
	void DrawItem_ImageAndText(DWORD /*dwStyle*/, LPNMCTCCUSTOMDRAW lpNMCustomDraw, int nIconVerticalCenter, RECT& rcTab, RECT& rcText);
	void DrawCloseButton(LPNMCTCCUSTOMDRAW lpNMCustomDraw);
	void DrawScrollButtons(LPNMCTCCUSTOMDRAW lpNMCustomDraw);

// Overrides for painting from CCustomTabCtrl
public:
	void InitializeDrawStruct(LPNMCTCCUSTOMDRAW lpNMCustomDraw);
	void DoPrePaint(RECT rcClient, LPNMCTCCUSTOMDRAW lpNMCustomDraw);
	void DoItemPaint(LPNMCTCCUSTOMDRAW lpNMCustomDraw);
	void DoPostPaint(RECT /*rcClient*/, LPNMCTCCUSTOMDRAW lpNMCustomDraw);

	// Overrides from CCustomTabCtrl
public:
	void CalcSize_NonClient(LPRECT prcTabItemArea);
	void CalcSize_CloseButton(LPRECT prcTabItemArea);
	void CalcSize_ScrollButtons(LPRECT prcTabItemArea);
	void UpdateLayout_Default(RECT rcTabItemArea);
	void UpdateLayout_ScrollToFit(RECT rcTabItemArea);
};

#endif // PNTABCONTROL_H__INCLUDED