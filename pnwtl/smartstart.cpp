/**
 * @file smartstart.cpp
 * @brief Implementation of SmartStart
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "textview.h"
#include "xmlparser.h"
#include "smartstart.h"

//SmartStart* SmartStart::s_pTheInstance = NULL;

/// protected singleton constructor...
SmartStart::SmartStart()
{
	tstring path;
	OPTIONS->GetPNPath(path, PNPATH_USERSETTINGS);
	path += _T("UserSmartStart.xml");

	if(!FileExists(path.c_str()))
	{
		// Some simple defaults...
		m_Map.insert(SM_VT(tstring(_T("#ifndef")), tstring(_T("cpp"))));
		m_Map.insert(SM_VT(tstring(_T("#include")), tstring(_T("cpp"))));
		m_Map.insert(SM_VT(tstring(_T("/*")), tstring(_T("cpp"))));
		m_Map.insert(SM_VT(tstring(_T("unit")), tstring(_T("pascal"))));
		m_Map.insert(SM_VT(tstring(_T("public class")), tstring(_T("csharp"))));
		m_Map.insert(SM_VT(tstring(_T("<?")), tstring(_T("xml"))));
		m_Map.insert(SM_VT(tstring(_T("<html")), tstring(_T("web"))));
	}
	else
	{
		XMLParser parser;
		parser.SetParseState(this);
		try
		{
			parser.LoadFile(path.c_str());
		}
		catch (XMLParserException& ex)
		{
			tstring stat = _T("Error loading UserSmartStart.xml: ");
			stat += ex.GetMessage();
			g_Context.m_frame->SetStatusText(stat.c_str());
		}
	}

	m_max = 0;

	for(SM_IT i = m_Map.begin(); i != m_Map.end(); ++i)
	{
		m_max = ((*i).first.size() > m_max ? (*i).first.size() : m_max);
	}

	m_max++;

	m_buffer = new TCHAR[m_max];
	memset(m_buffer, 0, m_max * sizeof(TCHAR));
}

SmartStart::~SmartStart()
{
	delete [] m_buffer;
}

void SmartStart::Save()
{
	tstring path;
	OPTIONS->GetPNPath(path, PNPATH_USERSETTINGS);
	path += _T("UserSmartStart.xml");

	CFile f;
	if( f.Open(path.c_str(), CFile::modeWrite | CFile::modeBinary) )
	{
		// <?xml version="1.0"?>
		// <SmartStart>
		// <ssv from="using " to="csharp" />
		// </SmartStart>
		
		f.Write(_T("<?xml version=\"1.0\"?>\r\n<SmartStart>\r\n"), 37 * sizeof(TCHAR));

		tstring sout;
		for(SM_IT i = m_Map.begin(); i != m_Map.end(); ++i)
		{
			sout = _T("<ssv from=\"");
			XMLSafeString((*i).first.c_str(), sout);
			//sout += (*i).first;
			sout += _T("\" to=\"");
			XMLSafeString((*i).second.c_str(), sout);
			//sout += (*i).second;
			sout += _T("\" />\r\n");
			f.Write((void*)sout.c_str(), sout.length() * sizeof(TCHAR));
		}

		f.Write(_T("</SmartStart>"), 13 * sizeof(TCHAR));

		f.Close();
	}
}

/// Called by CTextView on character addition until it returns !eContinue
SmartStart::EContinueState SmartStart::OnChar(CTextView* pView)
{
	// See if we can find the first m_max characters in the view in our map.
	pView->GetText(m_max, m_buffer);
	SM_IT found = m_Map.find(tstring(m_buffer));
	
	if(found != m_Map.end())
	{
		// We did find that text, so we can map it to a scheme to select:
		CScheme* pScheme = CSchemeManager::GetInstance()->SchemeByName((*found).second.c_str());
		if(pScheme)
		{
			// Apply the scheme, and notify the user by setting the status...
			pView->SetScheme(pScheme);
			tstring stat = _T("SmartStart selected the scheme: ");
			stat += pScheme->GetTitle();
			g_Context.m_frame->SetStatusText(stat.c_str());
		}
		return eMatched;
	}

	if(_tcslen(m_buffer) == (m_max - 1))
	{
		//buffer is full and we haven't matched. Give up trying...
		return eGiveUp;
	}
	else
		return eContinue;
}

STRING_MAP& SmartStart::GetMap()
{
	return m_Map;
}

// XMLParseState Implementation:

/// Called on each XML element start - looks for tags: <ssv from="keyphrase" to="lexer" />
void SmartStart::startElement(LPCTSTR name, XMLAttributes& atts)
{
	if(_tcscmp(name, "ssv") == 0)
	{
		LPCTSTR tcf, tct;
		tcf = atts.getValue("from");
		tct = atts.getValue("to");
		if(tcf == NULL || tct == NULL)
			return;
		m_Map.insert(SM_VT(tstring(tcf), tstring(tct)));
	}
}