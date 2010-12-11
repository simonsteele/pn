/**
 * @file StylesUserSettings.cpp
 * @brief Implement user customised scheme reading
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
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
	m_loadingPreset = false;
}

void UserSettingsParser::SetPresetLoadMode()
{
	m_loadingPreset = true;
}

void UserSettingsParser::Parse(LPCTSTR path, SchemeLoaderState* pState)
{
	if(!FileExists(path))
	{
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
		tstring buf(data, len);
		CT2CA bufconv(buf.c_str());
		
		pState->m_CDATA += bufconv;
	}
}

void UserSettingsParser::startElement(void *userData, LPCTSTR name, const XMLAttributes& atts)
{
	SchemeLoaderState* pState = static_cast<SchemeLoaderState*>(userData);
	int state = pState->m_State;

	if(state == US_SCHEMES && (_tcscmp(name, _T("scheme")) == 0))
	{
		processScheme(pState, atts);
	}
	else if(state == US_SCHEME || state == US_KEYWORD_OVERRIDES || state == US_STYLE_OVERRIDES)
	{
		if(m_pCurScheme != NULL)
		{
			processSchemeElement(pState, name, atts);
		}
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
		if(!m_loadingPreset)
		{
			std::string kw = NormaliseKeywords( std::string(pState->m_CDATA) );
			
			if(kw.length() > 0)
			{
				CustomKeywordSet* pSet = new CustomKeywordSet;
				pSet->key = m_idval;
				pSet->pName = NULL;
				pSet->pWords = new char[kw.length()+1];
				strcpy(pSet->pWords, kw.c_str());
				m_pCurScheme->CustomKeywords.AddKeywordSet(pSet);
			}
		}

		pState->m_State = US_KEYWORD_OVERRIDES;
	}
	else if(state == US_SCHEME && (_tcscmp(name, _T("scheme")) == 0))
	{
		if(m_pCurScheme != NULL && !m_loadingPreset)
			pState->m_SchemeDetails.insert(SchemeDetailsMap::value_type(m_pCurScheme->Name, m_pCurScheme));

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

	pState->m_CDATA = "";
}

void UserSettingsParser::processClassElement(SchemeLoaderState* pState, LPCTSTR name, const XMLAttributes& atts)
{
	if(_tcscmp(name, _T("style-class")) == 0)
	{
		// Get the style details:
		StyleDetails* style = new StyleDetails;
		SchemeParser::parseStyle(pState, atts, style);

		if(!m_loadingPreset)
		{
			StylePtr p( new NamedStyleDetails(-1) );
			p->CustomStyle = style;
			pState->m_Classes.insert( StylePtrMap::value_type(p->CustomStyle->name, p) );
		}
		else
		{
			//if(style->name == "default")
			//{
			//	// Default style:
			//	pState->m_Default = *style;
			//	delete style;
			//}
			//else
			{
				StylePtrMap::iterator is = pState->m_Classes.find(style->name);
				if(is == pState->m_Classes.end())
				{
					StylePtr p( new NamedStyleDetails(-1) );
					p->CustomStyle = style;
					pState->m_Classes.insert( StylePtrMap::value_type(style->name, p));
				}
				else
				{
					(*is).second->CustomStyle = style;
				}
			}
		}
	}
}

void UserSettingsParser::processSchemeElement(SchemeLoaderState* pState, LPCTSTR name, const XMLAttributes& atts)
{
	if(pState->m_State == US_STYLE_OVERRIDES)
	{
		if(_tcscmp(name, _T("style")) == 0)
		{
			StyleDetails* pStyle = new StyleDetails;
			SchemeParser::parseStyle(pState, atts, pStyle);

			if(!m_loadingPreset)
			{
				// Hand off the style to the master style
				StylePtr p( new FullStyleDetails(pStyle->Key) );
				p->CustomStyle = pStyle;
				m_pCurScheme->PreLoadCustomisedStyle( p );
			}
			else
			{
				// Apply customisation to existing style...
				StylePtr p = m_pCurScheme->GetStyle(pStyle->Key);
				if(p != NULL)
				{
					p->CustomStyle = pStyle;
				}
			}
		}
	}
	else if (pState->m_State == US_KEYWORD_OVERRIDES)
	{
		if(_tcscmp(name, _T("keywords")) == 0)
		{
			LPCTSTR key = atts.getValue(_T("key"));
			m_idval = _ttoi(key);

			pState->m_CDATA = "";

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
		}
	}
}

#define SZTRUE(s) \
	(s[0] == 't')

void UserSettingsParser::processScheme(SchemeLoaderState* pState, const XMLAttributes& atts)
{
	LPCTSTR pName =  atts.getValue(_T("name"));

	if(pName && ((int)_tcslen(pName) > 0))
	{
		if(!m_loadingPreset)
		{
			CT2CA schemeName(pName);
			m_pCurScheme = new SchemeDetails(schemeName);
		}
		else
		{
			CT2CA schemeName(pName);
			SchemeDetailsMap::iterator iScheme = pState->m_SchemeDetails.find(std::string(schemeName));
			if(iScheme != pState->m_SchemeDetails.end())
			{
				m_pCurScheme = (*iScheme).second;
			}
			else
			{
				m_pCurScheme = NULL;
			}
		}

		if(m_pCurScheme == NULL)
			return;

		//pScheme = new CustomisedScheme;
		m_SchemeName = pName;
		pState->m_State = US_SCHEME;

		LPCTSTR temp = atts.getValue(_T("ovtabs"));
		if(temp != NULL && _tcslen(temp) > 0)
		{
			m_pCurScheme->CustomFlagFlags |= schOverrideTabs;

			if(SZTRUE(temp))
				// Signal that we definitely want to override the tab use.
				m_pCurScheme->CustomFlags |= schOverrideTabs;
		
			temp = atts.getValue(_T("usetabs"));
			if(temp != NULL && _tcslen(temp) > 0)
			{
				m_pCurScheme->CustomFlagFlags |= schUseTabs;
				if(SZTRUE(temp))
					m_pCurScheme->CustomFlags |= schUseTabs;
			}

		}

		temp = atts.getValue(_T("tabwidth"));
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

void UserSettingsParser::processGlobalColours(SchemeLoaderState* pState, const XMLAttributes& atts)
{
	pState->m_DefaultColours.SetFromXml(atts);
}