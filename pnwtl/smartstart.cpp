/**
 * @file smartstart.cpp
 * @brief Implementation of SmartStart
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "textview.h"
#include "xmlparser.h"
#include "pnstrings.h"
#include "smartstart.h"

typedef string_map::value_type SM_VT;
typedef string_map::iterator SM_IT;

/// protected singleton constructor...
SmartStart::SmartStart() : m_buffer(NULL)
{
	tstring path;
	OPTIONS->GetPNPath(path, PNPATH_USERSETTINGS);
	path += _T("UserSmartStart.xml");

	if(!FileExists(path.c_str()))
	{
		// Some simple defaults...
		m_Map.insert(SM_VT(std::string("#ifndef"), std::string("cpp")));
		m_Map.insert(SM_VT(std::string("#include"), std::string("cpp")));
		m_Map.insert(SM_VT(std::string("/*"), std::string("cpp")));
		m_Map.insert(SM_VT(std::string("unit"), std::string("pascal")));
		m_Map.insert(SM_VT(std::string("public class"), std::string("csharp")));
		m_Map.insert(SM_VT(std::string("<?php"), std::string("php")));
		m_Map.insert(SM_VT(std::string("<?xml"), std::string("xml")));
		m_Map.insert(SM_VT(std::string("<html"), std::string("web")));
		m_Map.insert(SM_VT(std::string("<!--"), std::string("xml")));
		m_Map.insert(SM_VT(std::string("import"), std::string("python")));
		m_Map.insert(SM_VT(std::string("#!/usr/bin/perl"), std::string("perl")));
		m_Map.insert(SM_VT(std::string("#!/usr/local/bin/perl"), std::string("perl")));
		m_Map.insert(SM_VT(std::string("#!/usr/bin/env python"), std::string("python")));
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

	update();
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
		
		f.Write("<?xml version=\"1.0\"?>\r\n<SmartStart>\r\n", 37 * sizeof(char));

		std::string sout;
		for(string_map::const_iterator i = m_Map.begin(); i != m_Map.end(); ++i)
		{
			sout = "<ssv from=\"";
			XMLSafeString((*i).first.c_str(), sout);
			//sout += (*i).first;
			sout += "\" to=\"";
			XMLSafeString((*i).second.c_str(), sout);
			//sout += (*i).second;
			sout += "\" />\r\n";
			f.Write((void*)sout.c_str(), sout.length() * sizeof(char));
		}

		f.Write(_T("</SmartStart>"), 13 * sizeof(TCHAR));

		f.Close();
	}

	// When save is called we've probably changed our word list so
	// reconfigure...
	update();
}

/// Called by CTextView on character addition until it returns !eContinue
SmartStart::EContinueState SmartStart::OnChar(CTextView* pView)
{
	// See if we can find the first m_max characters in the view in our map.
	pView->GetText(m_max, m_buffer);
	SM_IT found = m_Map.find(std::string(m_buffer));
	
	if(found != m_Map.end())
	{
		// We did find that text, so we can map it to a scheme to select:
		Scheme* pScheme = SchemeManager::GetInstance()->SchemeByName((*found).second.c_str());
		if(pScheme)
		{
			applyScheme(pView, pScheme);
		}

		return eMatched;
	}

	if(strlen(m_buffer) == (m_max - 1))
	{
		//buffer is full and we haven't matched. Give up trying...
		return eGiveUp;
	}
	else
		return eContinue;
}

void SmartStart::Scan(CTextView* pView)
{
	pView->GetText(m_max, m_buffer);
	
	int length = strlen(m_buffer);
	
	while (length > 0)
	{
		std::string str(m_buffer, 0, length);

		SM_IT found = m_Map.find(str);
		if(found != m_Map.end())
		{
			Scheme* pScheme = SchemeManager::GetInstance()->SchemeByName((*found).second.c_str());
			if(pScheme)
			{
				applyScheme(pView, pScheme);
				break;
			}
		}

		length--;
	}
}

string_map& SmartStart::GetMap()
{
	return m_Map;
}

void SmartStart::update()
{
	m_max = 0;

	for(SM_IT i = m_Map.begin(); i != m_Map.end(); ++i)
	{
		m_max = ((*i).first.size() > m_max ? (*i).first.size() : m_max);
	}

	m_max++;

	if(m_buffer)
		delete [] m_buffer;

	m_buffer = new char[m_max];
	memset(m_buffer, 0, m_max * sizeof(char));
}

/// Apply the scheme, and notify the user by setting the status...
void SmartStart::applyScheme(CTextView* pView, Scheme* pScheme)
{
	pView->SetScheme(pScheme);
	tstring stat = _T("SmartStart selected the scheme: ");
	stat += pScheme->GetTitle();
	g_Context.m_frame->SetStatusText(stat.c_str());
}

// XMLParseState Implementation:

/// Called on each XML element start - looks for tags: <ssv from="keyphrase" to="lexer" />
void SmartStart::startElement(LPCTSTR name, const XMLAttributes& atts)
{
	if(_tcscmp(name, _T("ssv")) == 0)
	{
		LPCTSTR tcf, tct;
		tcf = atts.getValue(_T("from"));
		tct = atts.getValue(_T("to"));
		if(tcf == NULL || tct == NULL)
			return;
		
		CT2CA phrase(tcf);
		CT2CA scheme(tct);
		m_Map.insert(SM_VT(std::string(phrase), std::string(scheme)));
	}
}