/**
 * @file xmlfileautocomplete.h
 * @brief XML-file based autocomplete
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef xmlfileautocomplete_h__included
#define xmlfileautocomplete_h__included

#include "autocomplete.h"

namespace Impl
{
	class Tag;
}

/**
 * Implement autocomplete from xml API files
 */
class XmlFileAutocompleteProvider : public IWordProvider
{
public:
	XmlFileAutocompleteProvider(LPCTSTR apiFile);
	virtual ~XmlFileAutocompleteProvider();

	/**
	 * Called as keywords are loaded into a document
	 */
	virtual void RegisterKeyWords(int set, const char* words);

	/**
	 * Tagger has found a tag
	 */
	virtual void RegisterTag(const char* tag, const char* name);

	/**
	 * Reset tag based autocomplete
	 */
	virtual void ResetTags();

	/**
	 * Complete reset
	 */
	virtual void Reset();

	/**
	 * Get the list of words to use
	 */
	virtual void GetWords(PN::BaseString& words, const char* root, int rootLength, bool includeParameters = false, char tokenSeparator = ' ');

	// New function to return the correct prototypes for a method
	virtual void GetPrototypes(PN::BaseString& prototypes, char TokenSeparator, const char* method, int methodLength);

private:
	std::vector<Impl::Tag> m_tags;
};

#endif // #ifndef xmlfileautocomplete_h__included