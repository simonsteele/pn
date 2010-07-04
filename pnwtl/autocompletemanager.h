/**
 * @file autocomplete.h
 * @brief Manage autocomplete implementations 
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef AutoCompleteManager_h__included
#define AutoCompleteManager_h__included

#include "autocomplete.h"

typedef std::map<std::string, IWordProviderPtr> ApiMap;

/**
 * Factory / instance manager for Autocomplete providers.
 */
class AutoCompleteManager
{
public:
	/// Shutdown, free all providers.
	~AutoCompleteManager();

	/// Get an autocomplete implementation for a given scheme.
	IWordProviderPtr GetAutocomplete(const char* scheme);

private:
	IWordProviderPtr getApi(const char* scheme);

	ApiMap m_apiProviders;
};

#endif  // #ifndef AutoCompleteManager_h__included