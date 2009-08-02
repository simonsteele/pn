/**
 * @file xmlparser.cpp
 * @brief Implement the XML parser framework.
 * @author Simon Steele
 * @note Copyright (c) 2002-2008 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * Unicode Status: Unicode Ready (tested).
 */

#include "stdafx.h"
#include "xmlparser.h"

#ifdef XML_UNICODE
	#pragma comment(lib,"libexpatw.lib") 
	#pragma message("Automatically linking with libexpatw.lib")
#else
	#pragma comment(lib,"libexpat.lib") 
	#pragma message("Automatically linking with libexpat.lib")
#endif


// Get CFile for XMLParser::LoadFile
#include "Files.h"

//////////////////////////////////////////////////////////
// Global scope functions
//////////////////////////////////////////////////////////

void XMLParserStartElement(void *userData, XML_CSTR name, LPCTSTR *atts)
{
	XMLParseState* pState = static_cast<XMLParseState*>(userData);
	pState->startElement(name, XMLAttributes(atts));
}

void XMLParserEndElement(void *userData, XML_CSTR name)
{
	XMLParseState* pState = static_cast<XMLParseState*>(userData);
	pState->endElement(name);
}

void XMLParserCharacterData(void *userData, XML_CSTR s, int len)
{
	XMLParseState* pState = static_cast<XMLParseState*>(userData);
	pState->characterData(s, len);
}

//////////////////////////////////////////////////////////
// XMLAttributes - expat attributes wrapper...
//////////////////////////////////////////////////////////

XMLAttributes::XMLAttributes(XML_CSTR * atts)
{
	m_count = 0;
	m_atts = atts;
	
	for(int i = 0; atts[i] != 0; i += 2)
		m_count++;
}

XML_CSTR XMLAttributes::operator [] (int index) const
{
	return getValue(index);
}

XML_CSTR XMLAttributes::getName(int index) const
{
	ATLASSERT(index < m_count);

	return m_atts[index*2];
}

XML_CSTR XMLAttributes::getValue(int index) const
{
	ATLASSERT(index < m_count);
	
	return m_atts[(index*2)+1];
}

int XMLAttributes::getCount() const
{
	return m_count;
}

XML_CSTR XMLAttributes::getValue(XML_CSTR name) const
{
	XML_CSTR key = NULL;
	XML_CSTR val = NULL;
	
	for(int i = 0; i < m_count; i++)
	{
		key = getName(i);
		if(_tcscmp(key, name)==0)
		{
			val = getValue(i);
			break;
		}
	}

	return val;
}

//////////////////////////////////////////////////////////
// XMLParser - expat parser wrapper...
//////////////////////////////////////////////////////////

XMLParser::XMLParser(bool namespaceAware)
{
	m_pState = NULL;
	m_szFilename = NULL;
	
	if(namespaceAware)
	{
		m_parser = XML_ParserCreateNS(NULL, _T(':'));
	}
	else
	{
		m_parser = XML_ParserCreate(NULL);
	}
	
	XML_SetElementHandler(m_parser, XMLParserStartElement, XMLParserEndElement);
	XML_SetCharacterDataHandler(m_parser, XMLParserCharacterData);
}

XMLParser::~XMLParser()
{
	XML_ParserFree(m_parser);
	if(m_szFilename)
		delete [] m_szFilename;
}

void XMLParser::Reset()
{
	XML_ParserReset(m_parser, NULL);
	
	XML_SetElementHandler(m_parser, XMLParserStartElement, XMLParserEndElement);
	XML_SetCharacterDataHandler(m_parser, XMLParserCharacterData);

	SetParseState(m_pState);
}

void XMLParser::SetParseState(XMLParseState* pState)
{
	m_pState = pState;
	XML_SetUserData(m_parser, m_pState);
}

bool XMLParser::LoadFile(LPCTSTR filename)
{
	ATLASSERT(m_pState != NULL);

	bool bRet = true;

	if(m_szFilename != NULL)
		delete [] m_szFilename;
	m_szFilename = new TCHAR[_tcslen(filename)+1];
	_tcscpy(m_szFilename, filename);

	CFile file;
	if(!file.Open(m_szFilename, 0))
		return false;

	char buf[4096];
	int done = 0;
	size_t len;

	do
	{
		len = file.Read(buf, sizeof(buf));
		done = len < sizeof(buf);
		if (!XML_Parse(m_parser, buf, len, done)) 
		{
			bRet = false;
			
			throw XMLParserException(this, XML_GetErrorCode(m_parser));
			
			break;
		};

	} while (!done);

	return bRet;
}

bool XMLParser::ParseBuffer(const char* buffer, DWORD dwRead, bool final)
{
	return XML_Parse(m_parser, buffer, dwRead, final) != 0;
}

XML_Parser XMLParser::GetParser()
{
	return m_parser;
}

LPCTSTR	XMLParser::GetFileName()
{
	return m_szFilename;
}