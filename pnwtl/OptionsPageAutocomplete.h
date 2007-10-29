/**
 * @file optionspageautocomplete.h
 * @brief Options Dialog Autocomplete Page for Programmer's Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef optionspageautocomplete_h__included
#define optionspageautocomplete_h__included

#include "include/optionsdialog.h"

/**
 * Autocomplete options page
 */
class COptionsPageAutocomplete : public COptionsPageImpl<COptionsPageAutocomplete>,
								public CWinDataExchange<COptionsPageAutocomplete>
{
	public:
		COptionsPageAutocomplete();

		BEGIN_MSG_MAP(COptionsPageAutocomplete)		
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
		virtual LPCTSTR GetTreePosition();
		virtual void OnCancel();

	private:
		bool m_bInited;
		BOOL m_bEnabled;
		BOOL m_bUseKeywords;
		BOOL m_bUseTags;
		BOOL m_bCloseTags;
		int m_iStartAt;
		int m_iActivation;
};

#endif // #ifndef optionspageautocomplete_h__included