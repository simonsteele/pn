/**
 * @file OptionsPageStyle.h
 * @brief Style Options Page
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef optionspagestyle_h__included
#define optionspagestyle_h__included

#include "include/optionsdialog.h"
#include "optionscontrols.h"
#include "SchemeConfig.h"
#include "controls/OptionsBlockHeader.h"

/**
 * Options page for basic text styles
 */
class COptionsPageStyle : public COptionsPageImpl<COptionsPageStyle>
{
	public:
		COptionsPageStyle(SchemeConfigParser* pSchemes) : m_pSchemes(pSchemes), m_bDirty(false) {}

		BEGIN_MSG_MAP(COptionsPageStyle)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			MESSAGE_HANDLER(PN_NOTIFY, OnNotify)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()
		
		enum { IDD = IDD_PAGE_STYLE };

		virtual void OnOK();
		virtual void OnCancel();
		virtual void OnInitialise();
		virtual tstring GetTreePosition();

		bool IsDirty();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	
	protected:
		CFontCombo		m_FontCombo;
		CNumberCombo	m_SizeCombo;

		CPNColorButton	m_fore;
		CPNColorButton	m_back;
		
		CButton			m_bold;
		CButton			m_italic;
		CButton			m_underline;

		CPNColorButton	m_cur;
		CPNColorButton	m_indentGuides;
		CPNColorButton	m_selFore;
		CPNColorButton	m_selBack;
		CPNColorButton	m_markAll;
		CPNColorButton	m_smartHighlight;
		CPNColorButton  m_templateField;

		bool			m_bDirty;

		SchemeConfigParser* m_pSchemes;
		StylePtr		m_defclass;

		COptionsBlockHeader m_settingsHeader;
};

#endif // #ifndef optionspagestyle_h__included