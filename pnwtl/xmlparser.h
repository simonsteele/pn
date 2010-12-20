/**
 * @file xmlparser.h
 * @brief Define an XML parser framework using expat (http://expat.sourceforge.net/).
 * @author Simon Steele
 * @note Copyright (c) 2002-2008 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * Unicode Status: Unicode Ready (tested).
 */

#ifndef xmlparser_h__included
#define xmlparser_h__included

#ifdef _UNICODE
	#define XML_UNICODE
	#define XML_UNICODE_WCHAR_T
	#define XML_CSTR LPCTSTR
#else
	#define XML_CSTR LPCSTR
#endif

#include "expat.h"

static const TCHAR* tszXMLParserDefaultException = _T("Exception while parsing XML.");

/**
 * @class XMLAttributes
 * @brief Simple wrapper class for a char** based set of attributes.
 */
class XMLAttributes
{
	public:
		XMLAttributes(XML_CSTR * atts);

		XML_CSTR getName(int index) const;
		
		XML_CSTR getValue(int index) const;
		XML_CSTR getValue(XML_CSTR name) const;
		XML_CSTR operator [] (int index) const;

		int getCount() const;

	private:
		XML_CSTR * m_atts;
		int m_count;
};

/**
 * @class XMLParseState
 * @brief Abstract base class for implementing XML parser callbacks.
 *
 * You should either inherit from this class or use one of the callback
 * implementations like XMLParserCallback.
 */
class XMLParseState
{
	public:
		virtual void startElement(XML_CSTR name, const XMLAttributes& atts) = 0;
		virtual void endElement(XML_CSTR name) = 0;
		virtual void characterData(XML_CSTR data, int len) = 0;
};

template <typename T>
class XMLParserCallback : public XMLParseState
{
	public:
		typedef void (T::*SF)(void *userData, XML_CSTR name, const XMLAttributes& atts);
		typedef void (T::*EF)(void *userData, XML_CSTR name);
		typedef void (T::*CD)(void *userData, XML_CSTR data, int len);

		XMLParserCallback(T& t, SF elementStartFn, EF elementEndFn) :
			m_t(&t), m_sf(elementStartFn), m_ef(elementEndFn), m_cd(NULL) {}

		XMLParserCallback(T& t, SF elementStartFn, EF elementEndFn, CD characterDataFn) :
			m_t(&t), m_sf(elementStartFn), m_ef(elementEndFn), m_cd(characterDataFn) {}

		void SetCharacterDataCallback(CD characterDataFn)
		{
			m_cd = characterDataFn;
		}

		void SetUserData(void* data)
		{
			m_userData = data;
		}

		virtual void startElement(XML_CSTR name, const XMLAttributes& atts)
		{
			(m_t->*m_sf)(m_userData, name, atts);
		}

		virtual void endElement(XML_CSTR name)
		{
			(m_t->*m_ef)(m_userData, name);
		}

		virtual void characterData(XML_CSTR data, int len)
		{
			if(m_cd)
			{
				(m_t->*m_cd)(m_userData, data, len);
			}
		}

	protected:
		T*	m_t;
		SF	m_sf;
		EF	m_ef;
		CD	m_cd;

		void* m_userData;
};

/**
 * @class XMLParser
 * @author Simon Steele - http://untidy.net/
 * @brief Wrapper for an expat XML parser.
 *
 * This class wraps the operations involved in using
 * expat. It also allows for the use of callbacks and
 * multiple inheritance through the XMLParseState
 * class. A parse state must be specified before load
 * file is called.
 */
class XMLParser
{
	public:
		XMLParser(bool namespaceAware = false);
		~XMLParser();
		bool LoadFile(LPCTSTR filename);
		bool ParseBuffer(const char* buffer, DWORD dwRead, bool final);
		void SetParseState(XMLParseState* pState);
		void Reset();

		XML_Parser	GetParser();
		LPCTSTR		GetFileName();

	protected:
		XML_Parser		m_parser;
		XMLParseState*	m_pState;
		TCHAR*			m_szFilename;
};

/**
 * @class XMLParserException
 * @author Simon Steele - http://untidy.net/
 * @brief Exception object to be thrown by an XML parser
 * 
 * When constructed, this class stores the line and column
 * on which an error occured. Optionally, an error code can
 * also be stored.
 */
class XMLParserException
{
	public:
		XMLParserException(XMLParser* pParser, LPCTSTR msg = NULL)
		{
			m_errcode = XML_ERROR_NONE;
			m_filename = NULL;
			set(pParser, msg);
		}

		XMLParserException(XMLParser* pParser, XML_Error ErrorCode = XML_ERROR_NONE, LPCTSTR msg = NULL)
		{
			m_errcode = ErrorCode;
			m_filename = NULL;
			set(pParser, msg);
		}

		XMLParserException(const XMLParserException& copy)
		{
			m_errcode = copy.m_errcode;
			if(copy.m_filename)
			{
				m_filename = new TCHAR[_tcslen(copy.m_filename)+1];
				_tcscpy(m_filename, copy.m_filename);
			}
			else 
			{
				if(m_filename)
					delete [] m_filename;
				m_filename = NULL;
			}
			m_msg = copy.m_msg;
			m_line = copy.m_line;
			m_column = copy.m_column;			
		}
		
		virtual ~XMLParserException()
		{
			if(m_filename)
			{
				delete [] m_filename;
				m_filename = NULL;
			}
		}

		LPCTSTR GetFileName() { return m_filename; }
		LPCTSTR	GetMessage() { return m_msg; }
		int		GetLine() { return m_line; }
		int		GetColumn()	{ return m_column; }
		XML_Error GetErrorCode() { return m_errcode; }

	protected:
		void set(XMLParser* pParser, LPCTSTR msg)
		{
			m_line = XML_GetCurrentLineNumber(pParser->GetParser());
			m_column = XML_GetCurrentColumnNumber(pParser->GetParser());

			LPCTSTR f = pParser->GetFileName();
			m_filename = new TCHAR[_tcslen(f)+1];
			_tcscpy(m_filename, f);

			if(msg != NULL)
				m_msg = msg;
			else
				m_msg = tszXMLParserDefaultException;
		}

	protected:
		LPCTSTR			m_msg;
		int				m_line;
		int				m_column;
		XML_Error		m_errcode;
		TCHAR*			m_filename;
};

/* These are definitions for the global scope functions used to
   call operations on an XMLParseState instance. */
void XMLParserStartElement(void *userData, const char *name, const char **atts);
void XMLParserEndElement(void *userData, const char *name);
void XMLParserCharacterData(void *userData, const char *s, int len);

#endif //xmlparser_h__included