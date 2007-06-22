/**
 * @file autocomplete.cpp
 * @brief Implement autocomplete behaviours
 * @author Simon Steele
 * @note Copyright (c) 2002-2007 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "autocomplete.h"

// Insert a string into a string list maintaining sorting
void insert_sorted(tstring_array& arr, tstring& w)
{
	//TODO: w.Trim();
	//w = w.Trim();
	if(w.empty())
		return;

	if(arr.empty())
	{
		arr.push_back(w);
	}
	else if(_tcsicmp(w.c_str(), arr.back().c_str()) > 0)
	{
		arr.push_back(w);
	}
	else
	{
		tstring_array::iterator i = arr.begin();
		while(i != arr.end())
		{
			if(_tcsicmp((*i).c_str(), w.c_str()) > 0)
			{
				arr.insert(i, w);
				/*if(arr.size()>65) //For debugging
				{				
					for(int i=0;i<arr.size();i++)_RPT1(_CRT_WARN,"%s,",arr[i].c_str());
					_RPT0(_CRT_WARN,"\n");
				}//End debugging*/
				return;
			}

			++i;
		}
	}
}

DefaultAutoComplete::DefaultAutoComplete(bool ignoreCase) : m_ignoreCase(ignoreCase)
{
}

void DefaultAutoComplete::GetWords(PN::BaseString& nearestWords, const char* root, int rootLength)
{
	if ( m_api.size() > 0)
	{
		getNearestWords(nearestWords, m_api, root, rootLength, m_ignoreCase, '(', false);
		if(!nearestWords.Empty())
		{
			eliminateDuplicateWords(nearestWords);
		}
	}
}

void DefaultAutoComplete::RegisterKeyWords(int set, const char* words)
{
	if(OPTIONS->GetCached(Options::OAutoCompleteUseKeywords))
	{
		const char* word(words);
		tstring newWord;
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
		const char* endBrace = strchr(tag, ')');
		if(endBrace == NULL)
		{
			endBrace = tag + strlen(tag);
		}
		
		tstring display(name);
		display.append(openBrace, endBrace-openBrace+1);
		//tag += FullTag.Mid(startP, endP-startP+1);
		
		insert_sorted(m_api, display);
	}	
	else
	{
		insert_sorted(m_api, tstring(name));
	}
}

void DefaultAutoComplete::ResetTags()
{
	m_api.clear();
	m_api = m_keywords;
}

/**
 * Complete reset
 */
void DefaultAutoComplete::Reset()
{
	m_keywords.clear();
	m_api.clear();
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

void DefaultAutoComplete::getNearestWords(PN::BaseString& wordsNear, const tstring_array& arr, const char *wordStart, int searchLen, bool ignoreCase, char otherSeparator, bool exactLen)
{
	unsigned int wordlen;		// length of the word part (before the '(' brace) of the api array element
	int start = 0;				// lower bound of the api array block to search
	int end = arr.size() - 1;	// upper bound of the api array block to search
	int pivot;					// index of api array element just being compared
	int cond;					// comparison result (in the sense of strcmp() result)

	if (0 == arr.size())
		return; // is empty

	/*tstring sdebug;
	for(CScintillaImpl::CStringArray::const_iterator i = arr.begin();
		i != arr.end(); ++i)
		sdebug += (*i) + " ";
	LOG(sdebug.c_str());*/

	if (ignoreCase) {
/*		if (!sortedNoCase) {
			sortedNoCase = true;
			SortWordListNoCase(wordsNoCase, len);
		}
*/
		while (start <= end) { // Binary searching loop
			pivot = (start + end) / 2;
			cond = _strnicmp(wordStart, arr[pivot].c_str(), searchLen);
			if (!cond) {
				// Find first match
				while ((pivot > start) &&
					(0 == _strnicmp(wordStart, arr[pivot-1].c_str(), searchLen))) {
					--pivot;
				}
				// Grab each match
				while ((pivot <= end) &&
					(0 == _strnicmp(wordStart, arr[pivot].c_str(), searchLen))) {
					wordlen = lengthWord(arr[pivot].c_str(), otherSeparator) + 1;
					++pivot;
					if (exactLen && wordlen != lengthWord(wordStart, otherSeparator) + 1)
						continue;
					if (!wordsNear.Empty())
						wordsNear += " ";
					wordsNear += arr[pivot-1].substr(0, wordlen).c_str();// .Left(wordlen);
				}
				return;
			} else if (cond < 0) {
				end = pivot - 1;
			} else if (cond > 0) {
				start = pivot + 1;
			}
		}
	} else {	// Preserve the letter case
/*		if (!sorted) {
			sorted = true;
			SortWordList(words, len);
		}
*/
		while (start <= end) { // Binary searching loop
			pivot = (start + end) / 2;
			cond = strncmp(wordStart, arr[pivot].c_str(), searchLen);
			if (!cond) {
				// Find first match
				while ((pivot > start) &&
					(0 == strncmp(wordStart, arr[pivot-1].c_str(), searchLen))) {
					--pivot;
				}
				// Grab each match
				while ((pivot <= end) &&
					(0 == strncmp(wordStart, arr[pivot].c_str(), searchLen))) {
						wordlen = lengthWord(arr[pivot].c_str(), otherSeparator) + 1;
					++pivot;
					if (exactLen && wordlen != lengthWord(wordStart, otherSeparator) + 1)
						continue;
					if (!wordsNear.Empty())
						wordsNear += " ";
					wordsNear += arr[pivot-1].substr(0, wordlen).c_str();
				}
				return;
			} else if (cond < 0) {
				end = pivot - 1;
			} else if (cond > 0) {
				start = pivot + 1;
			}
		}
	}

	return; // is empty
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