/**
 * @file SchemeConfig.cpp
 * @brief Scheme configuration classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2006 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "schemeconfig.h"

#include "include/genx/genx.h"
#include "include/pngenx.h"
#include "usersettingswriter.h"

/////////////////////////////////////////////////////////
// SchemeConfigParser
/////////////////////////////////////////////////////////

SchemeConfigParser::SchemeConfigParser(LPCTSTR currentScheme)
	: m_DefaultScheme("default")
{
	m_pCurrent = NULL;

	if(currentScheme)
		m_CurrentScheme = currentScheme;

	m_DefaultScheme.Title = _T("Plain Text");
}

SchemeConfigParser::~SchemeConfigParser()
{
	SchemeDetailsList::iterator i;
	for(i = m_Schemes.begin(); i != m_Schemes.end(); ++i)
	{
		delete (*i);
	}
	m_Schemes.clear();
}

SchemeDetailsList& SchemeConfigParser::GetSchemes()
{
	return m_Schemes;
}

StylesMap& SchemeConfigParser::GetStyleClasses()
{
	return m_originalclasses;
}

StylesMap& SchemeConfigParser::GetCustomClasses()
{
	return m_customclasses;
}

StyleDetails* SchemeConfigParser::GetDefaultStyle()
{
	return &m_LoadState.m_Default;
}

EditorColours* SchemeConfigParser::GetDefaultColours()
{
	return &m_LoadState.m_DefaultColours;
}

SchemeDetails* SchemeConfigParser::GetPlainTextScheme()
{
	return &m_DefaultScheme;
}

void SchemeConfigParser::LoadConfig(LPCTSTR path, LPCTSTR compiledpath)
{
	m_Path = compiledpath;
	m_Path += _T("UserSettings.xml");

	Parse(path, _T("master.scheme"), m_Path.c_str());
	
	/* The classes used while building up the styles are not the original
	classes as seen in the config files. Those are stored in our own 
	m_originalclasses member. We don't need the LoadState ones any more. */
	//m_LoadState.m_StyleClasses.Clear();
	//m_LoadState.m_CustomClasses.Clear();

	Sort();
}

void SchemeConfigParser::SaveConfig()
{
	Save(m_Path.c_str());
}

LPCTSTR SchemeConfigParser::GetCurrentScheme()
{
	return m_CurrentScheme.c_str();
}

void SchemeConfigParser::Save(LPCTSTR filename)
{
	Schemes::Writer writer;

	StyleDetails* pStyle = NULL;
	StyleDetails* pOrig = NULL;
	
	writer.Start(filename);
	writer.beginDoc();

	// Colour Overrides
	writer.writeOverrideColours( GetDefaultColours() );

	// Style Classes
	if(GetCustomClasses().GetCount() != 0)
	{
		writer.beginOverrideClasses();

		tstring name;
		STYLEDETAILS_NAMEMAP& map = GetCustomClasses().GetMap();
		for(SDNM_IT j = map.begin(); j != map.end(); ++j)
		{
			pStyle = (*j).second;
			{
				pOrig = GetStyleClasses().GetStyle(pStyle->name.c_str());
				if(pOrig)
				{					
					pStyle->compareTo(*pOrig);
				}
				else
					(*j).second->values = ~ 0;
			}

			writer.writeStyleClass(* (*j).second);
		}

		writer.endOverrideClasses();
	}

	// Schemes
	if(m_Schemes.size() != 0)
	{
		writer.beginSchemes();

		for(SchemeDetailsList::const_iterator i = m_Schemes.begin(); i != m_Schemes.end(); ++i)
		{
			SchemeDetails* pScheme = (*i);

			if(!pScheme->IsCustomised())
				continue;

			// Write out scheme block...
			writer.beginScheme( pScheme );

			writer.writeEditorColours( &(pScheme->CustomColours) );

			CustomKeywordSet* pKeywordSet = pScheme->CustomKeywords.GetFirstKeywordSet();
			if(pKeywordSet != NULL)
			{
				// Keywords
				writer.beginOverrideKeywords();
				while(pKeywordSet)
				{
					writer.writeKeywords(pKeywordSet->key, pKeywordSet->pWords);
					pKeywordSet = pKeywordSet->pNext;
				}
				writer.endOverrideKeywords();
			}

			bool begun(false);
			for(StylePtrList::const_iterator iS = pScheme->Styles.begin(); iS != pScheme->Styles.end(); ++iS)
			{
				if( (*iS)->CustomStyle )
				{
					if(!begun)
					{
						begun = true;
						writer.beginOverrideStyles();
					}

					writer.writeStyle( *(*iS)->CustomStyle );
				}
			}

			if(begun)
				writer.endOverrideStyles();

			// End of Scheme
			writer.endScheme();
		}

		// End of Schemes...
		writer.endSchemes();
	}
	
	// End of file.
	writer.endDoc();
	writer.Close();
}

void SchemeConfigParser::onLexer(LPCTSTR name, int styleBits)
{

}

void SchemeConfigParser::onLanguage(LPCTSTR name, LPCTSTR title, int foldflags, int ncfoldflags)
{
	PNASSERT(m_pCurrent == NULL);

	m_pCurrent = ensureSchemeDetails( m_LoadState.m_SchemeDetails, tstring(name) );

	m_pCurrent->Title = title;
	m_pCurrent->Flags = ncfoldflags;
	
	m_Schemes.push_back(m_pCurrent);
}

void SchemeConfigParser::onLanguageEnd()
{
	m_pCurrent = NULL;
}

void SchemeConfigParser::onStyleGroup(XMLAttributes& att, const StylePtr& pClass)
{
	PNASSERT(m_pCurrent != NULL);

	LPCTSTR name = att.getValue(_T("name"));
	if(name)
	{
		m_pCurrent->BeginStyleGroup(name, att.getValue(_T("description")), (pClass.get() ? pClass->Style->name.c_str() : NULL) );
	}
}

void SchemeConfigParser::onStyle(const StylePtr& style)
{
	PNASSERT(m_pCurrent != NULL);

	m_pCurrent->Styles.push_back(style);
}

void SchemeConfigParser::onStyleClass(const StylePtr& style)
{
}

void SchemeConfigParser::onStyleGroupEnd()
{
	m_pCurrent->EndStyleGroup();
}

void SchemeConfigParser::onKeywords(int key, LPCTSTR keywords, LPCTSTR name, LPCTSTR custom)
{
	PNASSERT(m_pCurrent != NULL);

	CustomKeywordSet* pSet = new CustomKeywordSet;
	pSet->key = key;
	pSet->pWords = new TCHAR[_tcslen(keywords)+1];
	_tcscpy(pSet->pWords, keywords);
	pSet->pName = new TCHAR[_tcslen(name)+1];
	_tcscpy(pSet->pName, name);
	m_pCurrent->Keywords.AddKeywordSet(pSet);

	if(custom)
	{
		CustomKeywordSet* pCustomSet = new CustomKeywordSet(*pSet);
		pCustomSet->pWords = new TCHAR[_tcslen(custom)+1];
		_tcscpy(pCustomSet->pWords, custom);
		m_pCurrent->CustomKeywords.AddKeywordSet(pCustomSet);
	}
}

void SchemeConfigParser::onFile(LPCTSTR /*filename*/)
{

}

void SchemeConfigParser::onColours(const EditorColours* defCols, const EditorColours* colours)
{
	// We ignore the defaults here, we're only interested in any differences...
	if(colours)
		m_pCurrent->Colours = *colours;
}

bool SortSchemes(SchemeDetails* p1, SchemeDetails* p2)
{
	return (p1->Title < p2->Title);
}

void SchemeConfigParser::Sort()
{
///@todo find a way to do this in VC6
#if (_ATL_VER >= 0x0700)
	m_Schemes.sort(SortSchemes);
#endif
}