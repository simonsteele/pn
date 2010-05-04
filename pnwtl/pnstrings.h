/**
 * @file pnstrings.h
 * @brief Utility classes and functions for strings.
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef pnstrings_h__included
#define pnstrings_h__included

#if defined(UNICODE)
	typedef std::wostream tstream;
#else
	typedef std::ostream tstream;
#endif

typedef std::map<tstring, tstring> STRING_MAP;
typedef std::list<tstring> tstring_list;
typedef std::vector<tstring> tstring_array;
typedef std::vector<std::string> string_array;
typedef std::list<std::string> string_list;
typedef std::map<tstring, std::string> tstring_string_map;
typedef std::map<std::string, std::string> string_map;

static inline LPARAM StrToLp(const char* str)
{
	return reinterpret_cast<LPARAM>(str);
}

static inline WPARAM StrToWp(const char* str)
{
	return reinterpret_cast<WPARAM>(str);
}

static inline LPARAM WcsToLp(const wchar_t* str)
{
	return reinterpret_cast<LPARAM>(str);
}

static inline WPARAM WcsToWp(const wchar_t* str)
{
	return reinterpret_cast<WPARAM>(str);
}

static TCHAR* tcsnewdup(LPCTSTR strin)
{
	TCHAR* ret = new TCHAR[_tcslen(strin)+1];
	_tcscpy(ret, strin);
	return ret;
}

static char* strnewdup(const char* strin)
{
	char* ret = new char[strlen(strin)+1];
	strcpy(ret, strin);
	return ret;
}

static wchar_t* wcsnewdup(const wchar_t* strin)
{
	wchar_t* ret = new wchar_t[wcslen(strin)+1];
	wcscpy(ret, strin);
	return ret;
}

static tstring IntToTString(int x)
{
	TCHAR buffer[32];
	_sntprintf(buffer, 32, _T("%0d"), x);
	
	return tstring(buffer);
}

static std::string IntToString(int x)
{
	char buffer[32];
	_snprintf(buffer, 32, "%0d", x);
	return std::string(buffer);
}

static int strFirstNonWS(const char* lineBuf)
{
	PNASSERT(AtlIsValidString(lineBuf));

	const char* p = lineBuf;
	int i = 0;
	while(*p != 0 && (*p == _T(' ') || *p == 0x9))
	{
		i++;
		p++;
	}

	return i;
}

static int strLastNonWSChar(const char* lineBuf, int lineLength)
{
	PNASSERT(AtlIsValidString(lineBuf));

	const char* p = &lineBuf[lineLength-1];
	int i = lineLength-1;
	while(p > lineBuf && (*p == _T(' ') || *p == 0x9))
	{
		p--;
		i--;
	}

	return i;
}

template <typename TStringType>
static void StringTokenise(const TStringType& str,
						   std::vector<TStringType>& tokens,
                      const TStringType& delimiters/* = _T(" ")*/)
{
    // Skip delimiters at beginning.
    TStringType::size_type lastPos = str.find_first_not_of(delimiters, 0);
    // Find first "non-delimiter".
    TStringType::size_type pos     = str.find_first_of(delimiters, lastPos);

    while (TStringType::npos != pos || TStringType::npos != lastPos)
    {
        // Found a token, add it to the vector.
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        // Skip delimiters.  Note the "not_of"
        lastPos = str.find_first_not_of(delimiters, pos);
        // Find next "non-delimiter"
        pos = str.find_first_of(delimiters, lastPos);
    }
}

template <typename TStringType>
static void Trim(TStringType& str)
{
	TStringType::size_type pos = str.find_last_not_of(' ');
	if(pos != TStringType::npos) {
		str.erase(pos + 1);
		pos = str.find_first_not_of(' ');
		if(pos != TStringType::npos)
			str.erase(0, pos);
	}
	else 
		str.erase(str.begin(), str.end());
}

/**
 * Exception that can be thrown from a format string builder
 * to abort the build 
 */
class FormatStringBuilderException : public std::exception {};

/**
 * This class builds strings using custom format specifiers. It
 * supports both %x style format strings and also $(var) style
 * strings. The user must implement at least one of OnFormatChar
 * or OnFormatKey and add text to m_string.
 */
template <class T>
class CustomFormatStringBuilder
{
	public:
		const tstring& Build(LPCTSTR str)
		{
			TCHAR next;
			T* pT = static_cast<T*>(this);
			int len = _tcslen(str);

			m_string = _T("");

			for(int i = 0; i < len; i++)
			{
				if(str[i] == _T('%'))
				{
					next = SafeGetNextChar(str, i, len);
					
					if(next == NULL)
					{
						m_string += str[i];
					}
					else if(next == _T('%'))
					{
						m_string += next;
						// Push past the next %
						i++;
					}
					else if(next == _T('('))
					{
						// we matched a %(x) property...
						tstring key;
						i = ExtractProp(key, str, i);
						pT->OnFormatPercentKey(key.c_str());
					}
					else
					{
						pT->OnFormatChar(next);
						i++;
					}
				}
				else if (str[i] == _T('$'))
				{
					next = SafeGetNextChar(str, i, len);

					if (next == NULL)
					{
						m_string += str[i];
					}
					else if (next == _T('$'))
					{
						// If we are seeing a $$ then it means we want the $ sign.
						m_string += str[i];
						// Skip the next dollar sign as well.
						i ++;
						continue;
					}
					else if ( next != _T('(') )
					{
						m_string += str[i];
						continue;
					}

					// we probably matched a $(x) property...
					tstring key;
					i = ExtractProp(key, str, i);

					pT->OnFormatKey(key.c_str());
					
				}
				else if (str[i] == _T('&') && (i != (len-1)))
				{
					if (str[i + 1] == _T('&'))
					{
						// Ignore double-&
						m_string += str[i];
						i++;
					}
					else if (str[i + 1] == _T('{'))
					{
						tstring key;
						i = ExtractScriptRef(key, str, i);
						pT->OnFormatScriptRef(key.c_str());
					}
					else
					{
						m_string += str[i];
					}
				}
				else
				{
					m_string += str[i];
				}				
			}

			return m_string;
		}

		void OnFormatChar(TCHAR thechar){}
		void OnFormatKey(LPCTSTR key){}
		void OnFormatPercentKey(LPCTSTR key){}
		void OnFormatScriptRef(LPCTSTR key){}

	protected:
		TCHAR SafeGetNextChar(LPCTSTR str, int i, int len)
		{
			PNASSERT(i < len);
			PNASSERT(i >= 0);

			if(i == (len-1))
				return NULL;
            
			return str[i+1];
		}

		int ExtractScriptRef(tstring& prop, LPCTSTR source, int pos)
		{
			return ExtractStr(prop, source, pos, _T('}'));
		}

		int ExtractProp(tstring& prop, LPCTSTR source, int pos)
		{
			return ExtractStr(prop, source, pos, _T(')'));
		}

		/**
		 * Extract an expression looking for a specific end character.
		 * @param prop the expression that is extracted
		 * @param source the source text
		 * @param pos index into the source text
		 * @param exprEnd the character that ends the grouped expression.
		 */
		int ExtractStr(tstring& prop, LPCTSTR source, int pos, TCHAR exprEnd)
		{
			LPCTSTR pProp = &source[pos+2];
			LPCTSTR endProp = _tcschr(pProp, exprEnd);
			if(endProp != NULL)
			{
				int keylen = (endProp - pProp);
				prop.assign(pProp, keylen);
				pos += (2 + keylen); // skip ( + len + )
			}
			else
			{
				pos += 1;
			}

			return pos;
		}

		tstring	m_string;
};

void XMLSafeString(LPCTSTR from, tstring& to);
void XMLSafeString(LPCSTR from, std::string& to);
void XMLSafeString(tstring& str);

static std::string MakeIndentText(int indentation, bool useTabs, int tabSize)
{
	std::string theIndent;
	int tabs = useTabs ? (indentation / tabSize) : 0;
	int spaces = useTabs ? (indentation % tabSize) : indentation;

	for(int j = 0; j < tabs; ++j)
	{
		theIndent += "\t";
	}

	for(int j = 0; j < spaces; ++j)
	{
		theIndent += " ";
	}

	return theIndent;
}

#endif // #ifndef pnstrings_h__included