/**
 * @file StylesUserSettings.cpp
 * @brief Implement user customised scheme reading
 * @author Simon Steele
 * @note Copyright (c) 2002-2006 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "SchemeCompiler.h"
#include "ssreg.h"

#define US_SCHEMES				1
#define US_SCHEME				2
#define US_KEYWORD_OVERRIDES	3
#define US_STYLE_OVERRIDES		4
#define US_KEYWORDS				5
#define US_CLASSES				6
#define US_CLASS				7

////////////////////////////////////////////////////////////
// UserSettingsParser Implementation
////////////////////////////////////////////////////////////

UserSettingsParser::UserSettingsParser()
{
	//pScheme = NULL;
	m_pCurScheme = NULL;
}

void UserSettingsParser::Parse(LPCTSTR path, SchemeLoaderState* pState)
{
	ssreg::CSRegistry reg;
	reg.OpenKey(_T("Software\\Echo Software\\PN2\\SchemeDates"), true);
	
	if(FileExists(path))
	{
		reg.WriteInt(_T("UserSettings"),  FileAge(path));
	}
	else
	{
		reg.DeleteValue(_T("UserSettings"));
		return;
	}

	XMLParserCallback<UserSettingsParser> callback(*this, &UserSettingsParser::startElement, &UserSettingsParser::endElement, &UserSettingsParser::characterData);

	XMLParser parser;
	parser.SetParseState(&callback);
	
	callback.SetUserData((void*)pState);

	pState->m_pParser = &parser;

	pState->m_State = 0;

	try
	{
		parser.LoadFile(path);
	}
	catch (SchemeParserException& E)
	{
		CString err;
		err.Format(_T("Error Parsing Scheme UserSettings XML: %s\n (file: %s, line: %d, column %d)"), 
			E.GetMessage(), E.GetFileName(), E.GetLine(), E.GetColumn());
		
		LOG(err);
	}
	catch (XMLParserException& E)
	{
		CString err;
		err.Format(_T("Error Parsing Scheme UserSettings XML: %s\n (file: %s, line: %d, column %d)"), 
			XML_ErrorString(E.GetErrorCode()), E.GetFileName(), E.GetLine(), E.GetColumn());
		
		LOG(err);
	}
}

void UserSettingsParser::characterData(void* userData, LPCTSTR data, int len)
{
	SchemeLoaderState* pState = static_cast<SchemeLoaderState*>(userData);

	if(pState->m_State == US_KEYWORDS)
	{
		CString cdata;
		TCHAR* buf = cdata.GetBuffer(len+1);
		_tcsncpy(buf, data, len);
		buf[len] = 0;
		cdata.ReleaseBuffer();

		pState->m_CDATA += cdata;
	}
}

void UserSettingsParser::startElement(void *userData, LPCTSTR name, XMLAttributes& atts)
{
	SchemeLoaderState* pState = static_cast<SchemeLoaderState*>(userData);
	int state = pState->m_State;

	if(state == US_SCHEMES && (_tcscmp(name, _T("scheme")) == 0))
	{
		processScheme(pState, atts);
	}
	else if(state == US_SCHEME || state == US_KEYWORD_OVERRIDES || state == US_STYLE_OVERRIDES)
	{
		processSchemeElement(pState, name, atts);
	}
	else if(state == US_CLASSES)
	{
		processClassElement(pState, name, atts);
	}
	else if(_tcscmp(name, _T("schemes")) == 0)
	{
		pState->m_State = US_SCHEMES;
	}
	else if(_tcscmp(name, _T("override-classes")) == 0)
	{
		pState->m_State = US_CLASSES;
	}
	else if(_tcscmp(name, _T("override-colours")) == 0)
	{
		processGlobalColours(pState, atts);
	}
}

void UserSettingsParser::endElement(void *userData, LPCTSTR name)
{
	SchemeLoaderState* pState = static_cast<SchemeLoaderState*>(userData);
	int state = pState->m_State;

	if(state == US_KEYWORDS && (_tcscmp(name, _T("keywords")) == 0))
	{
		tstring kw = NormaliseKeywords( tstring(pState->m_CDATA) );
		
		if(kw.length() > 0)
		{
			CustomKeywordSet* pSet = new CustomKeywordSet;
			pSet->key = m_idval;
			pSet->pName = NULL;
			pSet->pWords = new TCHAR[kw.length()+1];
			_tcscpy(pSet->pWords, kw.c_str());
			m_pCurScheme->CustomKeywords.AddKeywordSet(pSet);
		}

		pState->m_State = US_KEYWORD_OVERRIDES;
	}
	else if(state == US_SCHEME && (_tcscmp(name, _T("scheme")) == 0))
	{
		pState->m_SchemeDetails.insert(SchemeDetailsMap::value_type(m_pCurScheme->Name, m_pCurScheme));
		//pScheme = NULL;
		m_pCurScheme = NULL;

		pState->m_State = US_SCHEMES;
	}
	else if(state == US_SCHEMES && (_tcscmp(name, _T("schemes")) == 0))
	{
		pState->m_State = 0;
	}
	else if(state == US_KEYWORD_OVERRIDES && (_tcscmp(name, _T("override-keywords")) == 0))
	{
		pState->m_State = US_SCHEME;
	}
	else if(state == US_STYLE_OVERRIDES && (_tcscmp(name, _T("override-styles")) == 0))
	{
		pState->m_State = US_SCHEME;
	}
	else if(state == US_CLASSES && (_tcscmp(name, _T("override-classes")) == 0))
	{
		pState->m_State = 0;
	}

	pState->m_CDATA = _T("");
}

void UserSettingsParser::processClassElement(SchemeLoaderState* pState, LPCTSTR name, XMLAttributes& atts)
{
	if(_tcscmp(name, _T("style-class")) == 0)
	{
		StylePtr p( new FullStyleDetails(-1) );
		p->CustomStyle = new StyleDetails;
		SchemeParser::parseStyle(pState, atts, p->CustomStyle);
		
		pState->m_Classes.insert( StylePtrMap::value_type(p->CustomStyle->name, p) );
	}
}

void UserSettingsParser::processSchemeElement(SchemeLoaderState* pState, LPCTSTR name, XMLAttributes& atts)
{
	if(pState->m_State == US_STYLE_OVERRIDES)
	{
		if(_tcscmp(name, _T("style")) == 0)
		{
			StyleDetails* pStyle = new StyleDetails;
			SchemeParser::parseStyle(pState, atts, pStyle);

			// Hand off the style to the master style
			StylePtr p( new FullStyleDetails(pStyle->Key) );
			p->CustomStyle = pStyle;
			m_pCurScheme->PreLoadCustomisedStyle( p );
		}
	}
	else if (pState->m_State == US_KEYWORD_OVERRIDES)
	{
		if(_tcscmp(name, _T("keywords")) == 0)
		{
			LPCTSTR key = atts.getValue(_T("key"));
			m_idval = _ttoi(key);

			pState->m_CDATA = _T("");

			pState->m_State = US_KEYWORDS;
		}
	}
	else if(pState->m_State == US_SCHEME)
	{
		if(_tcscmp(name, _T("override-keywords")) == 0)
		{
			pState->m_State = US_KEYWORD_OVERRIDES;
		}
		else if(_tcscmp(name, _T("override-styles")) == 0)
		{
			pState->m_State = US_STYLE_OVERRIDES;
		}
		else if(_tcscmp(name, _T("colours")) == 0)
		{
			m_pCurScheme->CustomColours.SetFromXml(atts);
			//pScheme->m_editorColours.SetFromXml(atts);
		}
	}
}

#define SZTRUE(s) \
	(s[0] == 't')

void UserSettingsParser::processScheme(SchemeLoaderState* pState, XMLAttributes& atts)
{
	LPCTSTR pName =  atts.getValue(_T("name"));

	if(pName && ((int)_tcslen(pName) > 0))
	{
		m_pCurScheme = new SchemeDetails(pName);

		//pScheme = new CustomisedScheme;
		m_SchemeName = pName;
		pState->m_State = US_SCHEME;

		LPCTSTR temp = atts.getValue("ovtabs");
		if(temp != NULL && _tcslen(temp) > 0)
		{
			m_pCurScheme->CustomFlagFlags |= schOverrideTabs;

			if(SZTRUE(temp))
				// Signal that we definitely want to override the tab use.
				m_pCurScheme->CustomFlags |= schOverrideTabs;
		
			temp = atts.getValue("usetabs");
			if(temp != NULL && _tcslen(temp) > 0)
			{
				m_pCurScheme->CustomFlagFlags |= schUseTabs;
				if(SZTRUE(temp))
					m_pCurScheme->CustomFlags |= schUseTabs;
			}

		}

		temp = atts.getValue("tabwidth");
		if(temp != NULL && _tcslen(temp) > 0)
		{
			m_pCurScheme->CustomFlagFlags |= schOverrideTabSize;
			m_pCurScheme->CustomFlags |= schOverrideTabSize;

			m_pCurScheme->CustomTabWidth = _ttoi(temp);
		}
	}
#ifdef _DEBUG
	else
	{
		LOG(_T("UserSettingsParser::processScheme(): Scheme section without name attribute.\n"));
	}
#endif
}

void UserSettingsParser::processGlobalColours(SchemeLoaderState* pState, XMLAttributes& atts)
{
	pState->m_DefaultColours.SetFromXml(atts);
}