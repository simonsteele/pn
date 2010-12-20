/**
 * @file clipmanager.cpp
 * @brief Text Clips Parser.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "clipparser.h"

using namespace TextClips;

#define TCPS_START	0
#define TCPS_CLIPS	1
#define TCPS_CLIP	2
#define TCPS_CHAR	3

#define MATCH_ELEMENT(ename) \
	if(_tcscmp(name, ename) == 0)

#define IN_STATE(state) \
	(m_parseState == state)

#define SET_STATE(state) \
	m_parseState = state

void Parser::startElement(LPCTSTR name, const XMLAttributes& atts)
{
	if ( IN_STATE(TCPS_START) )
	{
		MATCH_ELEMENT(_T("clips"))
		{
			decodeNames = false;
			m_curShortcut = "";
			m_cData = "";
			m_curEncoding = eNone;

			// get name attribute...
			SET_STATE(TCPS_CLIPS);

			LPCTSTR szName = atts.getValue(_T("name"));
			if(szName == NULL)
				szName = _T("(unknown)");

			LPCTSTR szScheme = atts.getValue(_T("scheme"));
			
			LPCTSTR szEncoding = atts.getValue(_T("encoding"));
			if(szEncoding != NULL)
			{
				if(_tcscmp(szEncoding, _T("windows-1252")) == 0)
					m_curEncoding = eWindows1252;
				else if(_tcscmp(szEncoding, _T("ansi")) == 0)
					m_curEncoding = eANSI;
			}

			szEncoding = atts.getValue(_T("decodeNames"));
			if(szEncoding != NULL && (szEncoding[0] == _T('t') || szEncoding[0] == _T('T')))
					decodeNames = true;

			CT2CA schemeconv(szScheme);
			
			m_pCurSet = new TextClipSet(m_curFileName.c_str(), szName, schemeconv, decodeNames);
		}
	}
	else if (IN_STATE(TCPS_CLIPS))
	{
		MATCH_ELEMENT(_T("clip"))
		{
			LPCTSTR pName = atts.getValue(_T("name"));
			m_curName = (pName != NULL ? pName : _T("error"));
			pName = atts.getValue(_T("shortcut"));
			CT2CA shortcut(pName);
			m_curShortcut = (shortcut != NULL ? shortcut : "");
			SET_STATE(TCPS_CLIP);
		}
	}
	else if (IN_STATE(TCPS_CLIP))
	{
		MATCH_ELEMENT(_T("char"))
		{
			LPCTSTR charValue = atts.getValue(_T("value"));
			if (charValue != NULL && charValue[0] != NULL)
			{
				char val = static_cast<char>(_ttoi(charValue));
				m_cData += val;
			}

			SET_STATE(TCPS_CHAR);
		}
	}
}

void Parser::endElement(LPCTSTR name)
{
	if (IN_STATE(TCPS_CLIP))
	{
		// Create new clip - name = m_curName, content = m_cData;
		if (m_curEncoding != eNone)
		{
			decodeData();
		}

#ifndef _UNICODE
		if(decodeNames)
		{
			m_curName = Utf8_Windows1252(m_curName.c_str());
		}
#endif

		Clip* clip = new Clip(m_curName, m_curShortcut, m_cData);

		m_pCurSet->Add( clip );

		m_cData = "";

		SET_STATE(TCPS_CLIPS);
	}
	else if (IN_STATE(TCPS_CLIPS))
	{
		m_clipSets.push_back(m_pCurSet);
		m_pCurSet = NULL;

		SET_STATE(TCPS_START);
	}
	else if (IN_STATE(TCPS_CHAR))
	{
		SET_STATE(TCPS_CLIP);
	}
}

void Parser::characterData(LPCTSTR data, int len)
{
	if ( IN_STATE(TCPS_CLIP) )
	{
		tstring buf(data, len);
		CT2CA conv(buf.c_str());
		m_cData += conv;
	}
}

void Parser::decodeData()
{
	// Until we have UTF-16 to 'x' conversion routines these won't work.
	#ifndef _UNICODE
	switch (m_curEncoding)
	{
		case eWindows1252:
		{
			Utf8_Windows1252 conv( m_cData.c_str() );
			if(conv.IsValid())
				m_cData = (const char*)conv;
		}
		break;

		case eANSI:
		{
			Utf8_ANSI conv( m_cData.c_str() );
			if(conv.IsValid())
				m_cData = (const char*)conv;
		}
		break;
	}
	#endif
}
