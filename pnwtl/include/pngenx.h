/**
 * @file pngenx.h
 * @brief Bits and pieces for using genx.
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef pngenx_h__included
#define pngenx_h__included

/**
 * Wraps up the details of starting and stopping writing an XML file using genx.
 */
class GenxXMLWriter
{
public:
	GenxXMLWriter()
	{
		m_hFile = NULL;
		
		m_writer = genxNew(NULL, NULL, NULL);

		initXmlBits();
	}

	~GenxXMLWriter()
	{
		if(m_hFile != NULL)
			Close();

		genxDispose(m_writer);
	}

	void Start(LPCTSTR filename)
	{
		m_hFile = _tfopen(filename, "wb");

		if(m_hFile == NULL)
		{
			UNEXPECTED(_T("Could not open an XML file for writing"));
			return;
		}

		genxStartDocFile(m_writer, m_hFile);
	}

	bool IsValid()
	{
		return (m_hFile != NULL) && (m_writer != NULL);
	}

	void Close()
	{
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
};

#define PREDECLARE_ATTRIBUTES() \
	{
#define ATT(name, member) \
	member = genxDeclareAttribute(m_writer, NULL, u(name), &s)
#define END_ATTRIBUTES() \
	}

#define u(x) (constUtf8)x

/*class PreDecAtt
{
public:
	PreDecAtt(genxWriter writer, constUtf8 name) 
	{ 
		genxStatus s; 
		att = genxDeclareAttribute(writer, NULL, name, &s); 
	}
protected:
	genxAttribute att;
};*/


#endif