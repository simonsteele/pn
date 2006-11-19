/**
 * @file optionspageautocomplete.cpp
 * @brief Options Dialog Autocomplete Page for Programmer's Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "OptionsPageAutocomplete.h"

COptionsPageAutocomplete::COptionsPageAutocomplete() : m_bInited(false)
{
}

void COptionsPageAutocomplete::OnOK()
{
	if(m_bInited)
	{
		// Save options...
		DoDataExchange(TRUE);

		OPTIONS->SetCached(Options::OAutoComplete, m_bEnabled);
		OPTIONS->SetCached(Options::OAutoCompleteUseKeywords, m_bUseKeywords);
		OPTIONS->SetCached(Options::OAutoCompleteUseTags, m_bUseTags);
		OPTIONS->SetCached(Options::OAutoCompleteStartChars, m_iStartAt);
		OPTIONS->SetCached(Options::OAutoCompleteBraces, m_bCloseBraces);
		OPTIONS->SetCached(Options::OAutoCompleteTags, m_bCloseTags);
	}
}

void COptionsPageAutocomplete::OnInitialise()
{
	m_bEnabled = OPTIONS->GetCached(Options::OAutoComplete);
	m_bUseKeywords = OPTIONS->GetCached(Options::OAutoCompleteUseKeywords);
	m_bUseTags = OPTIONS->GetCached(Options::OAutoCompleteUseTags);
	m_iStartAt = OPTIONS->GetCached(Options::OAutoCompleteStartChars);
	m_bCloseBraces = OPTIONS->GetCached(Options::OAutoCompleteBraces);
	m_bCloseTags = OPTIONS->GetCached(Options::OAutoCompleteTags);

	DoDataExchange(FALSE);

	m_bInited = true;
}

LPCTSTR COptionsPageAutocomplete::GetTreePosition()
{
	return _T("General\\AutoComplete");
}

void COptionsPageAutocomplete::OnCancel()
{
}
