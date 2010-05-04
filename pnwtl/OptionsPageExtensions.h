/**
 * @file optionspages.cpp
 * @brief Extensions Options Dialog Page
 * @author Simon Steele
 * @note Copyright (c) 2008-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef optionspageextensions_h__included
#define optionspageextensions_h__included

#include "include/optionsdialog.h"
#include "controls/OptionsBlockHeader.h"
#include "controls/hyperlink.h"

/**
 * Autocomplete options page
 */
class COptionsPageExtensions : public COptionsPageImpl<COptionsPageExtensions>
{
	public:
		COptionsPageExtensions();

		BEGIN_MSG_MAP(COptionsPageExtensions)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()

		enum { IDD = IDD_PAGE_EXTENSIONS };

		virtual void OnOK();
		virtual void OnInitialise();
		virtual tstring GetTreePosition();
		virtual void OnCancel();

	private:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	private:
		CListViewCtrl m_list;
		CHyperlink m_extensionsLink;

		COptionsBlockHeader m_settingsHeader;
};

#endif // #ifndef optionspageextensions_h__included