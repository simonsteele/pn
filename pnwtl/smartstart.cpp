#include "stdafx.h"
#include "textview.h"
#include "xmlparser.h"
#include "smartstart.h"

SmartStart* SmartStart::s_pTheInstance = NULL;

/// protected singleton constructor...
SmartStart::SmartStart()
{
	tstring path;
	COptionsManager::GetInstance()->GetPNPath(path, PNPATH_USERSETTINGS);
	path += _T("UserSmartStart.xml");

	if(!FileExists(path.c_str()))
	{
		// Some simple defaults...
		m_Map.insert(SM_VT(tstring(_T("#ifndef")), tstring(_T("cpp"))));
		m_Map.insert(SM_VT(tstring(_T("#include")), tstring(_T("cpp"))));
		m_Map.insert(SM_VT(tstring(_T("unit")), tstring(_T("pascal"))));
		m_Map.insert(SM_VT(tstring(_T("public class")), tstring(_T("csharp"))));
		m_Map.insert(SM_VT(tstring(_T("<")), tstring(_T("web"))));
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
			tstring stat = _T("Error loading SmartStart.xml: ");
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