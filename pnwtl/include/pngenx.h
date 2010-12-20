/**
 * @file pngenx.h
 * @brief Bits and pieces for using genx.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef pngenx_h__included
#define pngenx_h__included

#include "encoding.h"

/**
 * Wraps up the details of starting and stopping writing an XML file using genx.
 */
class GenxXMLWriter
{
public:
	GenxXMLWriter() : m_hFile(NULL), m_bInited(false)
	{
		m_writer = genxNew(NULL, NULL, NULL);
	}

	~GenxXMLWriter()
	{
		Close();
		genxDispose(m_writer);
	}

	/**
	 * Open a file and prepare to write xml.
	 * @return True if file opened successfully, false otherwise.
	 */
	bool Start(LPCTSTR filename)
	{
		if(!m_bInited)
		{
			initXmlBits();
			m_bInited = true;
		}

		m_hFile = _tfopen(filename, _T("wb"));

		if(m_hFile == NULL)
		{
			return false;
		}

		genxStartDocFile(m_writer, m_hFile);

		return true;
	}

	/**
	 * @return true if this writer currently has a file open for writing.
	 */
	bool IsValid()
	{
		return (m_hFile != NULL) && (m_writer != NULL);
	}

	/**
	 * Close the current file.
	 */
	void Close()
	{
		if (!IsValid())
		{
			return;
		}

		if (genxEndDocument(m_writer))
		{
			// error...
		}

		fclose(m_hFile);
		m_hFile = NULL;
	}

	operator genxWriter ()
	{
		return m_writer;
	}

	/**
	 * Add a string attribute, converting the passed value to UTF-8 along the way.
	 */
	void addAttributeConvertUTF8(genxAttribute a, LPCWSTR str)
	{
		Utf16_Utf8 conv(str);
		genxAddAttribute(a, conv);
	}

	/**
	 * Add a string attribute, converting the passed value to UTF-8 along the way.
	 */
	void addAttributeConvertUTF8(genxAttribute a, LPCSTR str)
	{
		Windows1252_Utf8 conv(str);
		genxAddAttribute(a, conv);
	}

protected:

	/**
	 * Use this to initialize all your elements that you'll use over and
	 * over.
	 */
	virtual void initXmlBits()
	{
		//genxStatus s;
	}

	void pop()
	{
		genxEndElement(m_writer);
	}

protected:
	genxWriter	m_writer;
	FILE*		m_hFile;
	bool		m_bInited;
};

#define PREDECLARE_ATTRIBUTES() \
	{
#define ATT(name, member) \
	member = genxDeclareAttribute(m_writer, NULL, u(name), &s)
#define END_ATTRIBUTES() \
	}

#define u(x) (constUtf8)x

#endif