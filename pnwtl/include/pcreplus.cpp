/**
 * @file pcreplus.cpp
 * @brief Wrapper classes for PCRE using C++
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * This code uses PCRE to provide regular expressions support - see pcre_license.txt.
 */

#include "stdafx.h"
#include "pcreplus.h"

/*#ifdef PCRE_STATIC
	#pragma comment(lib, "libpcre.lib") 
	#pragma message("Automatically linking with libpcre.lib")
#endif*/

namespace PCRE
{

RegExp::RegExp()
{
	m_pRE = NULL;
	m_pStudyData = NULL;
	m_pSubStringVector = NULL;
	m_strmatch = NULL;
}

RegExp::RegExp(const char* expression, int flags)
{
	m_pRE = NULL;
	m_pStudyData = NULL;
	m_pSubStringVector = NULL;
	m_strmatch = NULL;

	compile(expression, (flags != -1) ? flags : PCRE_DEFAULT_FLAGS);
}

RegExp::~RegExp()
{
	clear();
}

void RegExp::Compile(const char* expression, int flags /*= -1*/)
{
	compile(expression, (flags != -1) ? flags : PCRE_DEFAULT_FLAGS);
}

/**
 * @brief Study/Analyse the regular expression to see if we can optimise it.
 * Useful if you are going to use the expression multiple times.
 *
 * @note Throws an REException if the study fails.
 * This is from the PCRE manual as of version 4.2: At present, 
 * studying a  pattern  is  useful  only  for  non-anchored  patterns  
 * that do not have a single fixed starting character. A  bitmap  of  
 * possible  starting  characters  is created.
 */
void RegExp::Study()
{
	const char* pError;
	m_pStudyData = pcre_study(m_pRE, 0, &pError);
	
	if(pError != NULL)
	{
		throw REException(pError);
	}
}

/**
 * @brief See if the pattern matches all or part of str.
 * @param str The string to try to match.
 * @param offset Offset into the string to match from. Default 0.
 */
bool RegExp::Match(const char* str, int offset)
{
	return match(str, strlen(str), offset, 0);
}

bool RegExp::Match(const char* str, int length, int offset)
{
	return match(str, length, offset, 0);
}

bool RegExp::GetNamedMatch(const char* name, tstring& str)
{
	const char* buf;
	int result = pcre_get_named_substring(m_pRE, m_strmatch, m_pSubStringVector, m_lastResult, name, &buf);
	if( result >= 0 )
	{
		str = buf;
		pcre_free_substring(buf);
		return true;
	}
	else
	{
		if(result != PCRE_ERROR_NOSUBSTRING)
			throw REException(result);
		
		return false;
	}
}

/**
 * @brief clear down anything held for the current regular expression.
 */
void RegExp::clear()
{
	if(m_pRE)
	{
		pcre_free(m_pRE);
		m_pRE = NULL;
	}
	
	if(m_pStudyData)
	{
		pcre_free(m_pStudyData);
		m_pStudyData = NULL;
	}
	
	if(m_pSubStringVector)
	{
		delete [] m_pSubStringVector;
		m_pSubStringVector = NULL;
	}

	m_nCaptureGroups = 0;

	m_strmatch = 0;
	m_strmatchlen = 0;
	m_lastResult = 0;
}

/**
 * @brief Compile the regular expression string into a PCRE compiled expression.
 * 
 * @note Throws an REException if the compile fails.
 */
void RegExp::compile(const char* expression, int flags)
{
	clear();

	const char* pError = NULL;
	int eoffset = 0;
	m_pRE = pcre_compile(expression, flags, &pError, &eoffset, NULL);
	
	if(!m_pRE)
	{
		throw REException(pError, eoffset);
	}

	// Now we find out how many capture groups there are in the expression.
	int nGroups;
	int result = pcre_fullinfo(m_pRE, m_pStudyData, PCRE_INFO_CAPTURECOUNT, &nGroups );
	
	if(result == 0)
	{
		// The size of the array to hold the captures must be a multiple of 3.
		// There may also be a group before and after the groups, so add 2.
		m_nCaptureGroups = (nGroups + 2) * 3;
	}
	else
	{
		// pcre_fullinfo failed.
		throw REException(result);
	}
}

bool RegExp::match(const char* str, int length, int offset, int flags)
{
	if(m_pSubStringVector == NULL)
	{
		m_pSubStringVector = new int[m_nCaptureGroups];
	}

	int result = 0;

	// Hmmm.... store these for later use - this is possibly bad, but it's fast.
	m_strmatch = str;
	m_strmatchlen = length;

	result = pcre_exec(m_pRE, m_pStudyData, str, length, offset, 
		flags, m_pSubStringVector, m_nCaptureGroups);

	m_lastResult = result;

	return result > 0;
}

} // namespace PCRE