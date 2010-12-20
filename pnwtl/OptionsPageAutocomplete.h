/**
 * @file optionspageautocomplete.h
 * @brief Options Dialog Autocomplete Page for Programmer's Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2006-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef optionspageautocomplete_h__included
#define optionspageautocomplete_h__included

#include "include/optionsdialog.h"
#include "controls/OptionsBlockHeader.h"

/**
 * Autocomplete options page
 */
class COptionsPageAutocomplete : public COptionsPageImpl<COptionsPageAutocomplete>,
								public CWinDataExchange<COptionsPageAutocomplete>
{
	public:
		COptionsPageAutocomplete();

		BEGIN_MSG_MAP(COptionsPageAutocomplete)		
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()
		enum { IDD = IDD_PAGE_AUTOCOMPLETE };

		BEGIN_DDX_MAP(COptionsPageAutocomplete)
			DDX_CHECK(IDC_ENABLEAUTOCOMPLETECHECK, m_bEnabled);
			DDX_CHECK(IDC_AUTOCOMPLETEKEYWORDSCHECK, m_bUseKeywords);
			DDX_CHECK(IDC_AUTOCOMPLETETAGSCHECK, m_bUseTags);
			DDX_INT_RANGE(IDC_AUTOCOMPLETECHARSTEXT, m_iStartAt, 1, 10);
			DDX_CHECK(IDC_AUTOCLOSETAGSCHECK, m_bCloseTags);
			DDX_RADIO(IDC_MANUALRADIO, m_iActivation);
		END_DDX_MAP()

		virtual void OnOK();
		virtual void OnInitialise();
		virtual tstring GetTreePosition();
		virtual void OnCancel();

	private:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

		bool m_bInited;
		BOOL m_bEnabled;
		BOOL m_bUseKeywords;
		BOOL m_bUseTags;
		BOOL m_bCloseTags;
		int m_iStartAt;
		int m_iActivation;

		COptionsBlockHeader m_settingsHeader;
};

#endif // #ifndef optionspageautocomplete_h__included