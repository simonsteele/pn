/**
 * @file smartstart.cpp
 * @brief Implementation of SmartStart
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
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

#include "third_party/genx/genx.h"
#include "include\pngenx.h"

#define u(x) (constUtf8)x

typedef string_map::value_type SM_VT;
typedef string_map::iterator SM_IT;

/////////////////////////////////////////////////////////////////////////////////////////////////
// Format:

// <SmartStart>
// <ssv from="using " to="csharp" />
// </SmartStart>

/////////////////////////////////////////////////////////////////////////////////////////////////
// SmartStartWriter

class SmartStartWriter : public GenxXMLWriter
{
public:
	void WriteMappings(const string_map& map)
	{
		genxStartElement(m_eContainer);
		for(string_map::const_iterator i = map.begin(); i != map.end(); ++i)
		{
			writeMapping(i->first, i->second);
		}
		genxEndElement(m_writer);
	}

protected:
	/**
	 * Use this to initialize all your elements that you'll use over and
	 * over.
	 */
	virtual void initXmlBits()
	{
		genxStatus s;

		m_eContainer = genxDeclareElement(m_writer, NULL, u("SmartStart"), &s);
		m_eMapping = genxDeclareElement(m_writer, NULL, u("ssv"), &s);
		
		PREDECLARE_ATTRIBUTES()
			ATT("from", m_aTrigger);
			ATT("to", m_aScheme);
		END_ATTRIBUTES();
	}

private:
	void writeMapping(const std::string& trigger, const std::string& scheme)
	{
		genxStartElement(m_eMapping);
		addAttributeConvertUTF8(m_aTrigger, trigger.c_str());
		addAttributeConvertUTF8(m_aScheme, scheme.c_str());
		genxEndElement(m_writer);
	}

	genxAttribute m_aTrigger;
	genxAttribute m_aScheme;
	genxElement m_eContainer;
	genxElement m_eMapping;
};

/////////////////////////////////////////////////////////////////////////////////////////////////
// SmartStart

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
	
	{
		SmartStartWriter writer;
		writer.Start(path.c_str());
		writer.WriteMappings(m_Map);
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