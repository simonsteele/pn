/**
 * @file autocomplete.h
 * @brief Define autocomplete behaviours
 * @author Simon Steele
 * @note Copyright (c) 2002-2007 Simon Steele <s.steele@pnotepad.org>
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
	virtual void GetWords(PN::BaseString& words, const char* root, int rootLength) = 0;
};

/**
 * PN default implementation of autocomplete:
 *   keywords and ctags based
 */
class DefaultAutoComplete : public IWordProvider
{
public:
	DefaultAutoComplete(bool ignoreCase);

	/**
	 * Get the list of words to use
	 */
	virtual void GetWords(PN::BaseString& words, const char* root, int rootLength);

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
	void getNearestWords(PN::BaseString& into, const tstring_array& arr, const char *wordStart, int searchLen, bool ignoreCase, char otherSeparator, bool exactLen);
	unsigned int lengthWord(const char *word, char otherSeparator);
	
	tstring_array m_api;
	tstring_array m_keywords;
	bool m_ignoreCase;
};

#endif