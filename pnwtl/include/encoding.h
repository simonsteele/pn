/**
 * @file encoding.h
 * @brief Various simple text encoding conversion routines.
 * @author Simon Steele
 * @note Copyright (c) 2003-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef encoding_h__included
#define encoding_h__included

class Utf8_Windows1252
{
	public:
		Utf8_Windows1252(const char* utf8)
		{
			decoded = NULL;
			bValid = convert(utf8);
		}

		~Utf8_Windows1252()
		{
			if(decoded)
			{
				delete [] decoded;
			}
		}

		operator const char* () const
		{
			return decoded;
		}

		bool IsValid() const
		{
			return bValid;
		}

	protected:

		/**
		 * This function is designed to cope only with characters that 
		 * can be represented using windows-1252 encoding. Input strings
		 * with any other values may cause undesirable results.
		 */
		bool convert(const char* utf8)
		{
			int len = strlen(utf8);

#if defined(USE_WINDOWS_ENCODING_FUNCTIONS_ONLY)

			wchar_t* pW = new wchar_t[len+1];
			
			if( ::MultiByteToWideChar( CP_UTF8, 0, utf8, strlen(utf8), pW, ((len+1)*sizeof(wchar_t)) ) == 0 )
			{
				delete [] pW;
				return false;
			}

			int newlen = wcslen(pW);
			char* p1252 = new char[newlen+1];

			if( ::WideCharToMultiByte( 1252, 0, pW, newlen, p1252, newlen+1, 0, NULL ) == 0 )
			{
				delete [] pW;
				delete [] p1252;
				return false;
			}

			decoded = p1252;
			delete [] pW;

#else
			// Home-brew implementation.
			char* p1252 = new char[len+1];
			char* pOut = p1252;
			const unsigned char* pIn = reinterpret_cast<const unsigned char*>(utf8);
			while(*pIn)
			{
				/*
					>0-127 --> none --> 0-127 (==ascii)
					>194 --> 128-191 --> 128-191 (==sec unicode byte)
					>195 --> 128-191 --> 192-255 (==sec byte + 64)
					>192-223 --> 2 byte utf-8 (only 194, 195 indicate iso-8859-1)
					>>223 --> more than 2 bytes utf-8
				*/

				if(*pIn >= 0 && *pIn <= 127)
				{
					*pOut++ = *pIn;
				}
				else if(*pIn == 194 && (*(pIn+1) != NULL))
				{
					pIn++;
					*pOut++ = *pIn;
				}
				else if(*pIn == 195 && (*(pIn+1) != NULL))
				{
					pIn++;
					*pOut++ = *pIn + 64;
				}
				else
				{
					// invalid character for windows-1252. Attempt to replace with a ? but
					// we'll return false if we hit a character of more than 2 bytes.
					if(*pIn < 223)
					{
						// skip second byte.
						*pIn++;
						*pOut++ = '?';
					}
					else
					{
						delete [] p1252;
						return false;
					}
				}

				pIn++;
			}
			*pOut = '\0';

			decoded = p1252;
#endif
			return true;
		}

		const char* decoded;
		bool bValid;
};

class Utf8_ANSI
{
public:
	Utf8_ANSI(const char* utf8)
	{
		decoded = NULL;
		inPlace = false;
		bValid = convert(const_cast<char*>(utf8));
	}

	/**
	 * @param bInPlace if true, then the string is modified in place.
	 */
	Utf8_ANSI(char* utf8, bool bInPlace)
	{
		decoded = NULL;
		inPlace = bInPlace;
		bValid = convert(utf8);
	}

	~Utf8_ANSI()
	{
		if(decoded)
		{
			delete [] decoded;
		}
	}

	operator const char* () const
	{
		return decoded;
	}

	bool IsValid() const
	{
		return bValid;
	}

protected:
	bool convert(char *utf8)
	{
		size_t len = strlen(utf8);
		
		bool bConverted = true;
		//size_t szInputIdx, szOutputIdx = (size_t)0U;

		char* pOut;
		char* pANSI;

		if(!inPlace)
		{
			pANSI = new char[len+1];
			pOut = pANSI;
		}
		else
			pOut = utf8;

		for(size_t i = 0; bConverted && i < len; ++i)
		{
			/* If input_bin(0xxxxxxx) ~ ASCII_bin(0xxxxxxx)
			If input_bin(110000yy 10xxxxxx) ~ ANSI_bin(yyxxxxxx)
			All other UTF-8 encodings don't map to ANSI so we don't
			convert them and fail if we encounter them.
			See: http://www.unicode.org for more information about
			UTF-8 encoding.
			*/

			if( (utf8[i] & 0x80) == 0 )
			{
				// Plain ascii char
				*pOut++ = utf8[i];
			}
			else if( ( (i + 1) < len ) &&
					 ( (utf8[i] & 0xfc) == 0xc0 ) &&
					 ( (utf8[i+1] & 0xc0) == 0x80 ) )
			{
				// UTF-8 encoded char that maps to ANSI.
				*pOut++ = ((utf8[i] & 0x03) << 6) +	(utf8[i+1] & 0x3F);
				// skip the second input char.
				i++;
			}
			else 
			{
				// UTF-8 encoding that doesn't map to ANSI or illegal input.
				bConverted = false;
			}
		}

		if(!inPlace)
		{
			if(!bConverted)
				delete [] pANSI;
			else
				decoded = pANSI;
		}
		else
			decoded = utf8;
		
		return bConverted;
	}

	bool inPlace;
	bool bValid;
	char* decoded;
};

class Windows1252_Utf8
{
public:
	Windows1252_Utf8(const char* winstr)
	{
		if(winstr)
			bValid = convert(winstr);
		else
		{
			bValid = false;
			decoded = NULL;
		}
	}

	~Windows1252_Utf8()
	{
		if(decoded)
		{
			delete [] decoded;
		}
	}

	operator const unsigned char* () const
	{
		return decoded;
	}

	bool IsValid()
	{
		return bValid;
	}

protected:
	bool convert(const char* instring)
	{
		const unsigned char* instr = (const unsigned char*)instring;

		/*All characters in the range of 0-127 (hex 00 through 7F), are represented 
		identically in both encodings.  This covers the entire range of the original 
		ASCII characters. 
		All iso-8859-1 characters in the range of 128-191 (hex 80 through BF) need to 
		be preceeded by a byte with the value of 194 (hex C2) in utf-8, but otherwise 
		are left intact. 
		All iso-8859-1 characters in the range of 192-255 (hex C0 through FF) not only 
		need to be preceede by a byte with the value of 195 (hex C3) in utf-8, but 
		also need to have 64 (hex 40) subtracted from the iso-8859-1 character value. 
		Thanks to: http://intertwingly.net/stories/2004/04/14/i18n.html*/

		size_t length = strlen(instring);
		decoded = new unsigned char[(length*2)+1];
		unsigned char* pD = decoded;

		for(size_t i = 0; i < length; i++)
		{
			if(instr[i] > 0 && instr[i] < 0x80)
			{
				*pD++ = instr[i];
			}
			else if(instr[i] > 0x7f && instr[i] < 0xc0)
			{
				*pD++ = 0xc2;
				*pD++ = instr[i];
			}
			else //if(instr[i] > 0xbf && instr[i] <= 0xff)
			{
				*pD++ = 0xc3;
				*pD++ = instr[i] - 0x40;
			}
		}

		*pD = NULL;

		return true;
	}

	unsigned char* decoded;
	bool bValid;
};

template <int ICodePage>
class Mbs_Utf16
{
public:
	explicit Mbs_Utf16(const char* str)
	{
		if (str == NULL)
		{
			return;
		}

		size_t lenstr(strlen(str));

		// UTF-16 should be less chars than UTF-8:
		store.resize(lenstr+1);

		int length = MultiByteToWideChar(ICodePage, 0, str, lenstr, &store[0], lenstr+1);
		if (length == 0)
		{
			length = MultiByteToWideChar(ICodePage, 0, str, -1, NULL, 0);
			store.resize(length);
			length = MultiByteToWideChar(ICodePage, 0, str, -1, &store[0], length);
		}

		if (length != 0)
		{
			m_valid = true;
			store.resize(length);
		}
	}
	
	operator const wchar_t* () const
	{
		return store.c_str();
	}

private:
	std::wstring store;
	bool m_valid;
};

typedef Mbs_Utf16<CP_ACP> Windows1252_Utf16;
typedef Mbs_Utf16<CP_UTF8> Utf8_Utf16;

/**
 * Convert UTF-16 to UTF-8
 * Currently using Windows transformation.
 */
template <int ICodePage, typename TResult>
class Utf16_Mbs
{
public:
	explicit Utf16_Mbs(const wchar_t* str)
	{
		if (str == NULL)
		{
			return;
		}

		int length = WideCharToMultiByte(ICodePage, 0, str, -1, NULL, 0, NULL, NULL);
		m_store.resize(length);
		length = WideCharToMultiByte(ICodePage, 0, str, -1, &m_store[0], length, NULL, NULL);
		m_valid = length != 0;
		
		if (m_valid)
		{
			m_store.resize(length);
		}
		else
		{
			m_store.resize(0);
		}
	}

	operator TResult () const
	{
		return reinterpret_cast<TResult>(m_store.c_str());
	}

	const char* c_str() const
	{
		return m_store.c_str();
	}

private:
	bool m_valid;
	std::string m_store;
};

typedef Utf16_Mbs<CP_UTF8, const unsigned char*> Utf16_Utf8;
typedef Utf16_Mbs<CP_ACP, const char*> Utf16_Windows1252;

class TcsIdentity
{
public:
	explicit TcsIdentity(const TCHAR* str) : m_store(str)
	{
	}

	operator const TCHAR* () const
	{
		return m_store;
	}

	bool IsValid() const
	{
		return m_store != NULL;
	}

private:
	const TCHAR* m_store;
};

#ifdef _UNICODE
	typedef Utf16_Utf8 Tcs_Utf8;
	typedef Utf8_Utf16 Utf8_Tcs;
	typedef TcsIdentity Xml_Tcs;
	typedef Utf16_Windows1252 Tcs_Windows1252;
	typedef Utf16_Windows1252 Xml_Windows1252;
	typedef Windows1252_Utf16 Windows1252_Tcs;
#else
	typedef Windows1252_Utf8 Tcs_Utf8;
	typedef Utf8_Windows1252 Utf8_Tcs;
	typedef Utf8_Windows1252 Xml_Tcs;
	typedef TcsIdentity Tcs_Windows1252;
	typedef TcsIdentity Windows1252_Tcs;
	typedef Utf8_Windows1252 Xml_Windows1252;
#endif

#endif // #ifndef encoding_h__included