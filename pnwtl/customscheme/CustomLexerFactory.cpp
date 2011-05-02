/**
 * @file CustomLexerFactory.cpp
 * @brief Custom Scheme XML Parser.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "CustomScheme.h"
#include "../include/encoding.h"

#define STATE_DEFAULT		0
#define STATE_INSCHEME		1
#define STATE_INSTRINGS		2
#define STATE_INLANG		3
#define STATE_INUSEKW		4
#define STATE_INCOMMENTS	5

#define CT_LINE		1
#define CT_BLOCK	2

#define SBOOL(x) ((x[0] == _T('t')) || (x[0] == _T('T')))

//////////////////////////////////////////////////////////////////////////
// CustomLexerFactory
//////////////////////////////////////////////////////////////////////////

CustomLexerFactory::CustomLexerFactory(const TCHAR* path, Lexers& lexers) : m_lexers(lexers)
{
	m_parser.SetParseState(this);

	//Load and parse customlexers
	tstring sPath(path);
	if(sPath[sPath.size()-1] != _T('/') && sPath[sPath.size()-1] != _T('\\'))
		sPath += _T('\\');

	tstring sPattern(sPath);
	sPattern += _T("*.schemedef");

	WIN32_FIND_DATA FindFileData;
	HANDLE hFind = INVALID_HANDLE_VALUE;
	hFind = FindFirstFile(sPattern.c_str(), &FindFileData);
	if (hFind != INVALID_HANDLE_VALUE) {
		//Found the first file...
		BOOL found = TRUE;
		tstring to_open;

		while (found) {
			to_open = sPath;
			to_open += FindFileData.cFileName;
			
			// Now make the CustomLexer object.
			m_pCurrent = new LexerConfig;
			if (Parse(to_open.c_str()))
			{
				m_lexers.push_back(m_pCurrent);
			}
			else
			{
				delete m_pCurrent;
				m_pCurrent = NULL;
			}

			found = FindNextFile(hFind, &FindFileData);
		}

		FindClose(hFind);

	}
}

bool CustomLexerFactory::Parse(LPCTSTR file)
{
	m_bFileOK = true;
	m_state = STATE_DEFAULT;

	try
	{
		m_parser.LoadFile(file);
		m_parser.Reset();
	}
	catch (XMLParserException& ex)
	{
		::OutputDebugString(ex.GetMessage());
		m_bFileOK = false;
	}

	return m_bFileOK;
}

void CustomLexerFactory::doScheme(const XMLAttributes& atts)
{
	// ??? braces="{[()]}

	LPCTSTR pName;
	LPCTSTR pVal;

	for(int i = 0; i < atts.getCount(); i++)
	{
		pName = atts.getName(i);
		pVal = atts.getValue(i);

		if(_tcscmp(pName, _T("casesensitive")) == 0)
		{
			m_pCurrent->bCaseSensitive = SBOOL(pVal);
		}
		else if(_tcscmp(pName, _T("name")) == 0)
		{
			m_pCurrent->tsName = Tcs_Windows1252(pVal);
		}
	}

	m_state = STATE_INSCHEME;
}

void CustomLexerFactory::doStringType(const XMLAttributes& atts)
{
	int id;
	
	LPCTSTR szID = atts.getValue(_T("id"));
	if(!szID)
		return;

	id = _ttoi(szID);

	if(id < 0 || id >= MAX_STRINGTYPES)
		return;
	
	StringType_t& st = m_pCurrent->stringTypes[id];

	LPCTSTR pName, pVal;

	for(int i = 0; i < atts.getCount(); i++)
	{
		pName = atts.getName(i);
		pVal = atts.getValue(i);

		if(_tcscmp(pName, _T("start")) == 0)
		{
			st.start = static_cast<char>(pVal[0]);
		}
		else if(_tcscmp(pName, _T("end")) == 0)
		{
			st.end = static_cast<char>(pVal[0]);
		}
		else if(_tcscmp(pName, _T("multiline")) == 0)
		{
			st.multiLine = SBOOL(pVal);
		}
		else if(_tcscmp(pName, _T("continuation")) == 0)
		{
			st.bContinuation = true;
			st.continuation = static_cast<char>(pVal[0]);
		}
		else if(_tcscmp(pName, _T("escape")) == 0)
		{
			st.bEscape = true;
			st.escape = static_cast<char>(pVal[0]);
		}
	}

	if(st.start != 0 && st.end != 0)
		st.bValid = true;
}

void CustomLexerFactory::doKeyword(const XMLAttributes& atts)
{
	LPCTSTR szKey = atts.getValue(_T("key"));
	if(!szKey)
		return;

	int key = _ttoi(szKey);

	if(key < 0 || key >= MAX_KEYWORDS)
		return;

	m_pCurrent->kwEnable[key] = true;
}

void CustomLexerFactory::doPreProcessor(const XMLAttributes& atts)
{
	LPCTSTR pszStart = atts.getValue(_T("start"));
	if(!pszStart)
		return;
	m_pCurrent->bPreProc = true;
	m_pCurrent->preProcStart = static_cast<char>(pszStart[0]);

	LPCTSTR pszCont = atts.getValue(_T("continuation"));
	if(!pszCont)
		return;
	m_pCurrent->bPreProcContinuation = true;
	m_pCurrent->preProcContinue = static_cast<char>(pszCont[0]);
}

void CustomLexerFactory::doNumbers(const XMLAttributes& atts)
{
	LPCTSTR pszStart = atts.getValue(_T("start"));
	if(!pszStart)
		return;

	// start will be something like [a-z]. This needs parsing into a character set.
	m_pCurrent->numberStartSet.ParsePattern(Tcs_Windows1252(pszStart));

	LPCTSTR pszContent = atts.getValue(_T("content"));
	if(!pszContent)
		return;

	m_pCurrent->numberContentSet.ParsePattern(Tcs_Windows1252(pszContent));
}

void CustomLexerFactory::doKeywords(const XMLAttributes& atts)
{
	LPCTSTR pszStart = atts.getValue(_T("start"));
	if( pszStart )
	{
		CharSet chSet;
		if( chSet.ParsePattern(Tcs_Windows1252(pszStart)) )
			m_pCurrent->wordStartSet = chSet;
	}
	
	LPCTSTR pszContent = atts.getValue(_T("content"));
	if( pszContent )
	{
		CharSet chSet;
		if( chSet.ParsePattern(Tcs_Windows1252(pszContent)) )
            m_pCurrent->wordContentSet = chSet;
	}
}

void CustomLexerFactory::doIdentifiers(const XMLAttributes& atts)
{
	LPCTSTR pszStart = atts.getValue(_T("start"));
	if( pszStart )
	{
		CharSet chSet;
		if( chSet.ParsePattern(Tcs_Windows1252(pszStart)) )
            m_pCurrent->identStartSet = chSet;
	}
	
	LPCTSTR pszContent = atts.getValue(_T("content"));
	if( pszContent )
	{
		CharSet chSet;
		if( chSet.ParsePattern(Tcs_Windows1252(pszContent)) )
            m_pCurrent->identContentSet = chSet;
	}
}

void CustomLexerFactory::doIdentifiers2(const XMLAttributes& atts)
{
	LPCTSTR pszStart = atts.getValue(_T("start"));
	if(!pszStart)
		return;

	// start will be something like [a-z]. This needs parsing into a character set.
	m_pCurrent->identStartSet2.ParsePattern(Tcs_Windows1252(pszStart));

	LPCTSTR pszContent = atts.getValue(_T("content"));
	if(!pszContent)
		return;

	m_pCurrent->identContentSet2.ParsePattern(Tcs_Windows1252(pszContent));
}

void CustomLexerFactory::SetCommentTypeCode(LPCSTR pVal, ECodeLength& length, char* code, char*& pCode, CommentType_t* type)
{
	if(pVal && pVal[0] != NULL)
	{
		code[0] = pVal[0];
		if(pVal[1] == NULL)
		{
			length = eSingle;
		}
		else if(pVal[2] == NULL)
		{
			length = eDouble;
			code[1] = pVal[1];
		}
		else
		{
			length = eMore;
			pCode = new char[strlen(pVal)+1];
			strcpy(pCode, pVal);
		}
	}
	else
	{
		// Not sure what to do here...
		length = eSingle;
		code[0] = '\0';
	}
}

void CustomLexerFactory::doCommentType(int commentType, const XMLAttributes& atts)
{
	CommentType_t* type(NULL);
	switch(commentType)
	{
		case CT_LINE:
			type = &m_pCurrent->singleLineComment;
			break;
		
		case CT_BLOCK:
			{
				for (int i = 0; i < _countof(m_pCurrent->blockComment); i++)
				{
					if (!m_pCurrent->blockComment[i].bValid)
					{
						type = &m_pCurrent->blockComment[i];
						break;
					}
				}

				if (type == NULL)
				{
					return;
				}

				break;
			}

		default:
			return; // unknown type.
	}

	type->bValid = true;

	LPCTSTR pVal = atts.getValue(_T("start"));
	SetCommentTypeCode(Tcs_Windows1252(pVal), type->scLength, type->scode, type->pSCode, type);
	pVal = atts.getValue(_T("end"));
	SetCommentTypeCode(Tcs_Windows1252(pVal), type->ecLength, type->ecode, type->pECode, type);

	if(commentType == CT_LINE)
	{
		pVal = atts.getValue(_T("continuation"));
		if(pVal)
		{
			type->bContinuation = true;
			type->continuation = static_cast<char>(pVal[0]);
		}
	}
}

void CustomLexerFactory::startElement(XML_CSTR name, const XMLAttributes& atts)
{
	if(_tcscmp(name, _T("schemedef")) == 0 && m_state == STATE_DEFAULT)
	{
		doScheme(atts);
	}
	else if( m_state == STATE_INSCHEME )
	{
		if(_tcscmp(name, _T("strings")) == 0 )
		{
			m_state = STATE_INSTRINGS;
		}
		else if(_tcscmp(name, _T("language")) == 0 )
		{
			m_state = STATE_INLANG;
		}
		else if( _tcscmp(name, _T("comments")) == 0 )
		{
			m_state = STATE_INCOMMENTS;
		}
		else if( _tcscmp(name, _T("preprocessor")) == 0 )
		{
			doPreProcessor(atts);
		}
		else if( _tcscmp(name, _T("numbers")) == 0 )
		{
			doNumbers(atts);
		}
		else if (_tcscmp(name, _T("keywords")) == 0)
		{
			doKeywords(atts);
		}
		else if( _tcscmp(name, _T("identifiers")) == 0 )
		{
			doIdentifiers(atts);
		}
		else if( _tcscmp(name, _T("identifiers2")) == 0 )
		{
			doIdentifiers2(atts);
		}
	}
	else if( m_state == STATE_INSTRINGS )
	{
		if( _tcscmp(name, _T("stringtype")) == 0 )
		{
			doStringType(atts);
		}
	}
	else if( m_state == STATE_INLANG )
	{
		if( _tcscmp(name, _T("use-keywords")) == 0 )
			m_state = STATE_INUSEKW;
	}
	else if( m_state == STATE_INUSEKW )
	{
		if( _tcscmp(name, _T("keyword")) == 0 )
		{
			doKeyword(atts);
		}
	}
	else if( m_state == STATE_INCOMMENTS )
	{
		if( _tcscmp(name, _T("line")) == 0 )
		{
			doCommentType(CT_LINE, atts);
		}
		else if( _tcscmp(name, _T("block")) == 0 )
		{
			doCommentType(CT_BLOCK, atts);
		}
	}
}

void CustomLexerFactory::endElement(LPCTSTR name)
{
	if( m_state == STATE_INSTRINGS && _tcscmp(name, _T("strings")) == 0 )
	{
		m_state = STATE_INSCHEME;
	}
	else if( m_state == STATE_INUSEKW && _tcscmp(name, _T("use-keywords")) == 0 )
	{
		m_state = STATE_INLANG;
	}
	else if( m_state == STATE_INLANG && _tcscmp(name, _T("language")) == 0 )
	{
		m_state = STATE_INSCHEME;
	}
	else if( m_state == STATE_INCOMMENTS && _tcscmp(name, _T("comments")) == 0 )
	{
		m_state = STATE_INSCHEME;
	}
	else if( m_state == STATE_INSCHEME && _tcscmp(name, _T("schemedef")) == 0 )
	{
		m_state = STATE_DEFAULT;
	}
}

void CustomLexerFactory::characterData(LPCTSTR data, int len)
{

}