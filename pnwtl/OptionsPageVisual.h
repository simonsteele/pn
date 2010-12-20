/**
 * @file OptionsPageVisual.h
 * @brief Visual Help Options Page
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef optionspagevisual_h__included
#define optionspagevisual_h__included

#include "include/optionsdialog.h"
#include "controls/OptionsBlockHeader.h"

/**
 * Visual Help Options Page
 */
class COptionsPageVisual : public COptionsPageImpl<COptionsPageVisual>,
							public CWinDataExchange<COptionsPageVisual>
{
	public:
		BEGIN_MSG_MAP(COptionsPageVisual)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()
		
		enum { IDD = IDD_PAGE_VISUALHELP };

		BEGIN_DDX_MAP(COptionsPageVisual)
			DDX_CHECK(IDC_OPT_INDENTGUIDESCHECK, m_bIndentGuides)
			DDX_CHECK(IDC_OPT_FOLDINGCHECK, m_bFolding)
			DDX_CHECK(IDC_OPT_LINELIGHTCHECK, m_bLineHighlight)
			DDX_RADIO(IDC_OPT_NOLLHELPRADIO, m_iLongLineHelp)
			DDX_UINT(IDC_OPT_LLCOLUMNEDIT, m_iRightColumn)
			DDX_CHECK(IDC_OPT_LINELIGHTALPHACHECK, m_bLineHighlightAlpha)
			DDX_CHECK(IDC_OPT_SMARTHIGHLIGHTCHECK, m_bSmartHighlight)
			DDX_UINT(IDC_OPT_LINEPADDINGTOP_TEXT, m_iLinePaddingTop)
			DDX_UINT(IDC_OPT_LINEPADDINGBOTTOM_TEXT, m_iLinePaddingBottom)
		END_DDX_MAP()

		virtual void OnOK();
		virtual void OnInitialise();
		virtual tstring GetTreePosition();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		
		BOOL m_bIndentGuides;
		BOOL m_bFolding;
		BOOL m_bLineHighlight;
		BOOL m_bLineHighlightAlpha;
		int m_iLongLineHelp;
		UINT m_iRightColumn;
		BOOL m_bSmartHighlight;
		int m_iLinePaddingTop;
		int m_iLinePaddingBottom;

		CPNColorButton	m_btnLineCol;
		CPNColorButton	m_btnLLCol;
		CTrackBarCtrl	m_trackerHighlight;

		COptionsBlockHeader m_settingsHeader;
		COptionsBlockHeader m_linesHeader;
		COptionsBlockHeader m_paddingHeader;
};


#endif //#ifndef optionspagevisual_h__included