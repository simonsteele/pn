/**
 * @file pcreplus.h
 * @brief Wrapper classes for PCRE using C++
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * This code uses PCRE to provide regular expressions support - see pcre_license.txt.
 */

#ifndef pcreplus_h__included
#define pcreplus_h__included

#define PCRE_STATIC

//extern "C" {
	#include "pcre/pcre.h"
//}

#ifndef PCRE_DEFAULT_FLAGS
	#ifndef PCRE_NO_UTF8
		#define PCRE_DEFAULT_FLAGS	PCRE::RegExp::UTF8
	#else
		#define PCRE_DEFAULT_FLAGS 0
	#endif
#endif

namespace PCRE
{

class RegExp
{
	public:
		RegExp();
		RegExp(const char* expression, int flags = -1);
		~RegExp();

		void Compile(const char* expression, int flags = -1);

		void Study();
		
		bool Match(const char* str, int offset = 0);
		bool Match(const char* str, int length, int offset);

		bool GetNamedMatch(const char* name, tstring& str);

		enum {
			Anchored		= PCRE_ANCHORED,
			CaseInsensitive = PCRE_CASELESS,
			DollarEndOnly	= PCRE_DOLLAR_ENDONLY,	// Dollar matches do not include newlines.
			DotMatchesAll	= PCRE_DOTALL,			// Include newlines in . matches
			Extended		= PCRE_EXTENDED,		// Ignore unescaped whitespace in pattern.
			MultiLine		= PCRE_MULTILINE,
			NoAutoCapture	= PCRE_NO_AUTO_CAPTURE,	// Disable numbered capturing parentheses.
			UnGreedy		= PCRE_UNGREEDY,		// Invert operator greediness.
			UTF8			= PCRE_UTF8,			// Enable UTF-8 Support.
		};

	protected:
		void clear();
		void compile(const char* expression, int flags);
		bool match(const char* str, int length, int offset, int flags);

	protected:
		pcre*		m_pRE;
		pcre_extra*	m_pStudyData;
		const char*	m_strmatch;
		int			m_strmatchlen;
		int			m_lastResult;
		int*		m_pSubStringVector;
		int			m_nCaptureGroups;
};

/**
 * @brief Exception class used by RegExp
 */
class REException
{
	public:
		REException(const char* message, int offset = -1)
		{
			set(message, offset);
		}

		REException(int errCode)
		{
			int acode = abs(errCode);
			if(acode <= 8)
			{
				static const char* errors[8] = 
				{
					"no match",				/*PCRE_ERROR_NOMATCH        (-1)*/
					"null argument",		/*PCRE_ERROR_NULL           (-2)*/
					"bad option",			/*PCRE_ERROR_BADOPTION      (-3)*/
					"bad magic number",		/*PCRE_ERROR_BADMAGIC       (-4)*/
					"unknown node",			/*PCRE_ERROR_UNKNOWN_NODE   (-5)*/
					"no memory",			/*PCRE_ERROR_NOMEMORY       (-6)*/
					"no substring",			/*PCRE_ERROR_NOSUBSTRING    (-7)*/
					"match limit reached"	/*PCRE_ERROR_MATCHLIMIT		(-8)*/
				};
				set(errors[acode-1]);
			}
			else
				set(NULL);
		}

		REException(const REException& copy)
		{
			set(copy.m_pMsg, copy.m_offset);
		}

		~REException()
		{
			if(m_pMsg != NULL)
				delete [] m_pMsg;
		}

		void set(const char* message, int offset = -1)
		{
			if(message)
			{
				m_pMsg = new char[strlen(message)+1];
				strcpy(m_pMsg, message);
			}
			else
				m_pMsg = NULL;

			m_offset = offset;
		}

		const char* GetMessage()
		{
			return m_pMsg;
		}

		int GetOffset()
		{
			return m_offset;
		}

	protected:
		char*	m_pMsg;
		int		m_offset;
};

} // namespace PCRE

#endif