/**
 * @file encoding.h
 * @brief Find files according to a spec.
 * @author Simon Steele
 * @note Copyright (c) 2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
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

#endif // #ifndef encoding_h__included