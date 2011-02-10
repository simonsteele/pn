/**
 * @file autocomplete.cpp
 * @brief Implement autocomplete behaviours
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "pnstrings.h"
#include "autocomplete.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

// Insert a string into a string list maintaining sorting
void insert_sorted(string_array& arr, const std::string& w)
{
	//TODO: w.Trim();
	//w = w.Trim();
	if(w.empty())
		return;

	if(arr.empty())
	{
		arr.push_back(w);
	}
	else if(_stricmp(w.c_str(), arr.back().c_str()) > 0)
	{
		arr.push_back(w);
	}
	else
	{
		auto insert_point = std::lower_bound(arr.begin(), arr.end(), w, [](const std::string& l, const std::string& r) { return l < r; });
		arr.insert(insert_point, w);
	}
}

/**
 * Virtual destructor for IWordProvider
 */
IWordProvider::~IWordProvider()
{
}

DefaultAutoComplete::DefaultAutoComplete(bool ignoreCase, bool useKeywords) : m_ignoreCase(ignoreCase), m_useKeywords(useKeywords)
{
	m_tagsDirty = false;
	m_keywordsDirty = false;
}

/// virtual destructor
DefaultAutoComplete::~DefaultAutoComplete()
{
}

//MSO3: Redefined this function to include also parameters and have a custom token separator
void DefaultAutoComplete::GetWords(PN::BaseString& nearestWords, const char* root, int rootLength,bool IncludeParameters, char tokenSeparator)
{
	CreateCompleteList();

	if ( m_completelist.size() > 0)
	{
		getNearestWords(nearestWords, m_completelist, root, rootLength, m_ignoreCase, '(', false, IncludeParameters, tokenSeparator);
		if(!nearestWords.Empty())
		{
			eliminateDuplicateWords(nearestWords);
		}
	}
}

void DefaultAutoComplete::GetPrototypes(PN::BaseString& prototypes, char tokenSeparator, const char* method, int methodLength)
{
	CreateCompleteList();

	if ( !m_completelist.size() )
		return;

	getNearestWords(prototypes, m_completelist, method, methodLength, m_ignoreCase, '(', true, true, tokenSeparator);
}

void DefaultAutoComplete::RegisterKeyWords(int set, const char* words)
{
	if(m_useKeywords)
	{
		const char* word(words);
		std::string newWord;
		while(*word)
		{
			newWord.clear();
			while(*word && *word != ' ')
			{
				newWord += *word++;
			}
			
			if(*word /*== ' '*/) // we know it's space if it's not NULL
				word++;

			insert_sorted(m_keywords, newWord);

			m_keywordsDirty = true;
		}
	}
}

/**
 * Tagger has found a tag
 */
void DefaultAutoComplete::RegisterTag(const char* tag, const char* name)
{
	const char* openBrace = strchr(tag, '(');
	if(openBrace != NULL)
	{
		const char* endBrace = strrchr(tag, ')');
		if(endBrace == NULL || endBrace < openBrace)
		{
			endBrace = tag + strlen(tag);
		}
		
		std::string display(name);
		display.append(openBrace, endBrace-openBrace+1);
		//tag += FullTag.Mid(startP, endP-startP+1);
		
		insert_sorted(m_tags, display);
	}	
	else
	{
		insert_sorted(m_tags, std::string(name));
	}

	m_tagsDirty = true;
}

void DefaultAutoComplete::CreateCompleteList()
{
	if(m_tagsDirty || m_keywordsDirty)
	{
		m_completelist.clear();
		
		m_completelist.reserve(m_tags.size() + m_keywords.size());
		m_completelist.insert(m_completelist.end(), m_tags.begin(), m_tags.end());
		m_completelist.insert(m_completelist.end(), m_keywords.begin(), m_keywords.end());

		std::sort(m_completelist.begin(), m_completelist.end(), [](const std::string& a, const std::string& b) { return stricmp(a.c_str(), b.c_str()) < 0; });

		//Turn off the dirty flags
		m_tagsDirty = false;
		m_keywordsDirty = false;
	}
}

void DefaultAutoComplete::ResetTags()
{
	//Make sure the tags are not set to dirty, because the RegisterTags function will do that once it adds a new tag
	m_tags.clear();
	m_tagsDirty = false;
}

/**
 * Complete reset
 */
void DefaultAutoComplete::Reset()
{
	m_completelist.clear();

	//Make sure the keywords are not set to dirty, because the RegisterKeywords function will do that once it adds a new keyword
	m_keywords.clear();
	m_keywordsDirty = false;

	ResetTags();
}

void DefaultAutoComplete::eliminateDuplicateWords(PN::BaseString& words)
{
	char *firstWord = words.LockBuffer(words.GetLength());

	char *firstSpace = strchr(firstWord, ' ');		
	while (firstSpace) 
	{
		int firstLen = firstSpace - firstWord;
		char *secondWord = firstWord + firstLen + 1;
		char *secondSpace = strchr(secondWord, ' ');
		int secondLen = strlen(secondWord);
		if (secondSpace)
			secondLen = secondSpace - secondWord;

		if (firstLen == secondLen && !strncmp(firstWord, secondWord, firstLen)) 
		{
			strcpy(firstWord, secondWord);
			firstSpace = strchr(firstWord, ' ');
		} 
		else 
		{
			firstWord = secondWord;
			firstSpace = secondSpace;
		}
	}
	
	words.UnlockBuffer();
}

void DefaultAutoComplete::BinarySearchFor(PN::BaseString& result, const string_array& source, const char* wordStart, int searchLen, fnComparer compare, char otherSeparator, bool includeParameters, bool exactLen, char tokenSeparator)
{
	unsigned int wordlen;			// length of the word part (before the '(' brace) of the api array element
	int pivot;						// index of api array element just being compared
	int cond;						// comparison result (in the sense of strcmp() result)
	int start = 0;					// lower bound of the api array block to search
	int end = source.size() - 1;	// upper bound of the api array block to search

	while (start <= end) { // Binary searching loop
		pivot = (start + end) / 2;
		cond = compare(wordStart, source[pivot].c_str(), searchLen);
		if (!cond) {
			// Find first match
			while ((pivot > start) &&
				(0 == compare(wordStart, source[pivot-1].c_str(), searchLen))) {
				--pivot;
			}
			// Grab each match
			while ((pivot <= end) &&
				(0 == compare(wordStart, source[pivot].c_str(), searchLen))) 
			{
				++pivot;

				wordlen = lengthWord(source[pivot-1].c_str(), otherSeparator) + 1;
					
				if (exactLen && wordlen != lengthWord(wordStart, otherSeparator) + 1)
					continue;

				if (!result.Empty())
						result += tokenSeparator;

				if(!includeParameters)
				{	
					result += source[pivot-1].substr(0, wordlen).c_str();
				}
				else
				{
					if(strchr(source[pivot-1].c_str(), otherSeparator))
					{	
						result += source[pivot-1].c_str();
					}
				}
			}
			return;
		} else if (cond < 0) {
			end = pivot - 1;
		} else if (cond > 0) {
			start = pivot + 1;
		}
	}
}

void DefaultAutoComplete::getNearestWords(PN::BaseString& wordsNear, const string_array& arr, const char *wordStart, int searchLen, bool ignoreCase, char otherSeparator, bool exactLen, bool IncludeParameters,char TokenSeparator)
{
	if (0 == arr.size())
		return; // is empty

	/*tstring sdebug;
	for(CScintillaImpl::CStringArray::const_iterator i = arr.begin();
		i != arr.end(); ++i)
		sdebug += (*i) + " ";
	LOG(sdebug.c_str());*/

	if (ignoreCase) 
	{
		BinarySearchFor(wordsNear, arr, wordStart, searchLen, _strnicmp, otherSeparator, IncludeParameters, exactLen, TokenSeparator);
	}
	else 
	{
		BinarySearchFor(wordsNear, arr, wordStart, searchLen, strncmp, otherSeparator, IncludeParameters, exactLen, TokenSeparator);
	}
}

unsigned int DefaultAutoComplete::lengthWord(const char *word, char otherSeparator) 
{
	// Find a '('. If that fails go to the end of the string.
	const char *endWord = strchr(word, '(');
	
	if (!endWord && otherSeparator) 
		endWord = strchr(word, otherSeparator);
	
	if (!endWord) 
		endWord = word + strlen(word);
	
	// Last case always succeeds so endWord != 0
	// Drop any space characters.
	if (endWord > word) 
	{
		endWord--;	// Back from the '(', otherSeparator, or '\0'
		// Move backwards over any spaces
//		while ((endWord > word) && (IsASpace(*endWord))) {
		while ((endWord > word) && (isspace(*endWord))) endWord--;			
	}
	return endWord - word;
}