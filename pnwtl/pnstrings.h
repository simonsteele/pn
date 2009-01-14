/**
 * @file pnstrings.h
 * @brief Utility classes and functions for strings.
 * @author Simon Steele
 * @note Copyright (c) 2002-2007 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * Classes in this file:
 *   CustomFormatStringBuilder	- Build up a string based on %x and $(x) format specs.
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
typedef std::list<std::string> string_list;

template <class TChar>
class char_buffer
{
public:
	explicit char_buffer(TChar *buffer) : m_buf(buffer) {}
	~char_buffer() { delete [] m_buf; }

	TChar* get() const { return m_buf; }

	operator TChar* () const { return m_buf; }

private:
	TChar* m_buf;
};

static TCHAR* tcsnewdup(LPCTSTR strin)
{
	TCHAR* ret = new TCHAR[_tcslen(strin)+1];
	_tcscpy(ret, strin);
	return ret;
}

static tstring IntToTString(int x)
{
	TCHAR _buffer[32];
	_sntprintf(_buffer, 32, _T("%0d"), x);
	
	return tstring(_buffer);
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
						std::string key;
						i = ExtractProp(key, str, i);
						pT->OnFormatPercentKey(key.c_str());
					}
					else
					{
						pT->OnFormatChar(next);
						i++;
					}
				}
				else if(str[i] == _T('$') && (i != (len-1)))
				{
					if( str[i+1] == _T('$') )
					{
						// If we are seeing a $$( then it means we want the $ sign.
						if(SafeGetNextChar(str, i+1, len) == _T('('))
						{
							m_string += str[i];
							// Skip the next dollar sign as well.
							i += 1;
							continue;
						}
					}
					else if( str[i+1] != _T('(') )
					{
						m_string += str[i];
						continue;
					}

					// we matched a $(x) property...
					std::string key;
					i = ExtractProp(key, str, i);

					pT->OnFormatKey(key.c_str());
					
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

	protected:
		TCHAR SafeGetNextChar(LPCTSTR str, int i, int len)
		{
			PNASSERT(i < len);
			PNASSERT(i >= 0);

			if(i == (len-1))
				return NULL;
            
			return str[i+1];
		}

		int ExtractProp(tstring& prop, LPCTSTR source, int pos)
		{
			// we matched a $(x) property...
			LPCTSTR pProp = &source[pos+2];
			LPCTSTR endProp = _tcschr(pProp, _T(')'));
			if(endProp != NULL)
			{
				int keylen = (endProp - pProp) / sizeof(TCHAR);
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

///@todo could this be faster at all?
void XMLSafeString(LPCTSTR from, tstring& to);
void XMLSafeString(tstring& str);

struct FormatXML {
   tstring str_;
   explicit FormatXML(const tstring& str) : str_(str) { XMLSafeString(str_); }
   friend tstream& operator<<(tstream& s, const FormatXML& x)
   {
		s << x.str_;
		return s;
   }
};

static std::string MakeIndentText(int indentation, bool useTabs, int tabSize)
{
	tstring theIndent;
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