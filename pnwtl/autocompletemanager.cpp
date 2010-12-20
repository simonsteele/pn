/**
 * @file AutoCompleteManager.cpp
 * @brief Autocomplete management
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "AutoCompleteManager.h"
#include "xmlfileautocomplete.h"

/// Shutdown, free all providers.
AutoCompleteManager::~AutoCompleteManager()
{
	m_apiProviders.clear();
}

/// Get an autocomplete implementation for a given scheme.
IWordProviderPtr AutoCompleteManager::GetAutocomplete(const char* scheme)
{
	bool m_bAutoCompleteIgnoreCase = true;

	// Currently an API provider trumps the built-in keyword-based intellisense. This will
	// change to use composition in the future.
	IWordProviderPtr api = getApi(scheme);

	if (api.get())
	{
		return api;
	}

	return IWordProviderPtr(new DefaultAutoComplete(m_bAutoCompleteIgnoreCase, OPTIONS->GetCached(extensions::IOptions::OAutoCompleteUseKeywords) == TRUE));
}

/**
 * Go and look for an api file for this scheme, and if we find one then return
 * it. This looks in the schemes directory for [scheme].api. It returns an empty
 * smart pointer if no API file was found.
 */
IWordProviderPtr AutoCompleteManager::getApi(const char* scheme)
{
	IWordProviderPtr api;
	ApiMap::const_iterator existing = m_apiProviders.find(scheme);
	if (existing != m_apiProviders.end())
	{
		return (*existing).second;
	}

	tstring uspath;
	OPTIONS->GetPNPath(uspath, PNPATH_SCHEMES);
	
	CA2W schemeconv(scheme);
	std::wstring apifile(schemeconv);
	apifile += L".api";

	CFileName fn(apifile);
	fn.Root(uspath.c_str());

	if (FileExists(fn.c_str()))
	{
		api.reset(new XmlFileAutocompleteProvider(fn.c_str()));
	}
	
	m_apiProviders.insert(ApiMap::value_type(scheme, api));

	return api;
}