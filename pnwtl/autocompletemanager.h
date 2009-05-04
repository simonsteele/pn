/**
 * @file autocomplete.h
 * @brief Manage autocomplete implementations 
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef autocompletemanager_h__included
#define autocompletemanager_h__included

#include "autocomplete.h"

/**
 * Factory / instance manager for Autocomplete providers.
 */
class AutocompleteManager
{
public:
	/// Shutdown, free all providers.
	~AutocompleteManager();

	/// Get an autocomplete implementation for a given scheme.
	IWordProviderPtr GetAutocomplete(const char* scheme);

private:
	std::map<std::string, IWordProvider*> m_providers;
};

#endif  // #ifndef autocompletemanager_h__included