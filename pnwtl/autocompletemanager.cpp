/**
 * @file autocompletemanager.cpp
 * @brief Autocomplete management
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "autocompletemanager.h"
#include "xmlfileautocomplete.h"

/// Shutdown, free all providers.
AutocompleteManager::~AutocompleteManager()
{
}

/// Get an autocomplete implementation for a given scheme.
IWordProviderPtr AutocompleteManager::GetAutocomplete(const char* scheme)
{
	bool m_bAutoCompleteIgnoreCase = true;
	return IWordProviderPtr(new DefaultAutoComplete(m_bAutoCompleteIgnoreCase, OPTIONS->GetCached(extensions::IOptions::OAutoCompleteUseKeywords) == TRUE));
}