/**
 * @file autocomplete.h
 * @brief Define autocomplete behaviours
 * @author Simon Steele
 * @note Copyright (c) 2002-2007 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef autocomplete_h__included
#define autocomplete_h__included

#pragma once

/**
 * Implementation of one autocomplete source
 */
class IWordProvider
{
public:
	virtual ~IWordProvider();

	/**
	 * Called as keywords are loaded into a document
	 */
	virtual void RegisterKeyWords(int set, const char* words) = 0;

	/**
	 * Tagger has found a tag
	 */
	virtual void RegisterTag(const char* tag, const char* name) = 0;

	/**
	 * Reset tag based autocomplete
	 */
	virtual void ResetTags() = 0;

	/**
	 * Complete reset
	 */
	virtual void Reset() = 0;

	/**
	 * Get the list of words to use
	 */
	//MSO3: Redefined this function to include also parameters and have a custom token separator
	virtual void GetWords(PN::BaseString& words, const char* root, int rootLength, bool IncludeParameters=false,char TokenSeparator=' ') = 0;

	// New function to return the correct prototypes for a method
	virtual void GetPrototypes(PN::BaseString& prototypes, char TokenSeparator, const char* method, int methodLength) = 0;
};

/**
 * PN default implementation of autocomplete:
 *   keywords and ctags based
 */
class DefaultAutoComplete : public IWordProvider
{
public:
	DefaultAutoComplete(bool ignoreCase, bool useKeywords);
	virtual ~DefaultAutoComplete();

	/**
	 * Get the list of words to use
	 */
	//MSO3: Redefined this function to include also parameters and have a custom token separator
	virtual void GetWords(PN::BaseString& words, const char* root, int rootLength, bool IncludeParameters=false, char TokenSeparator=' ');

	// New function to return the correct prototypes for a method
	virtual void GetPrototypes(PN::BaseString& prototypes, char TokenSeparator, const char* method, int methodLength);

	/**
	 * Called as keywords are loaded into a document
	 */
	virtual void RegisterKeyWords(int set, const char* words);

	/**
	 * Tagger has found a tag
	 */
	virtual void RegisterTag(const char* tag, const char* name);

	/**
	 * Reset any tag-based autocomplete
	 */
	virtual void ResetTags();

	/**
	 * Complete reset
	 */
	virtual void Reset();

private:
	void eliminateDuplicateWords(PN::BaseString& words);
	//MSO3: Redefined this function to have a custom token separator:
	void getNearestWords(PN::BaseString& into, const tstring_array& arr, const char *wordStart, int searchLen, bool ignoreCase, char otherSeparator, bool exactLen, bool IncludeParameters=false, char TokenSeparator=' ');
	unsigned int lengthWord(const char *word, char otherSeparator);

	typedef int (*fnComparer)(const char*, const char*, size_t);
	void BinarySearchFor(PN::BaseString& result, const tstring_array& source, const char* wordStart, int searchLen, fnComparer compare, char otherSeparator, bool includeParameters, bool exactLen, char tokenSeparator);

	tstring_array m_api;
	tstring_array m_keywords;
	bool m_ignoreCase;
	bool m_useKeywords;
};

#endif