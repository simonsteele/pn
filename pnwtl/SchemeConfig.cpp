#include "stdafx.h"
#include "schemeconfig.h"

/////////////////////////////////////////////////////////
// CustomStyleCollection
/////////////////////////////////////////////////////////

CustomStyleCollection::CustomStyleCollection()
{
	m_pNext = NULL;
}

CustomStyleCollection::~CustomStyleCollection()
{
	for(SL_IT i = m_Styles.begin(); i != m_Styles.end(); ++i)
	{
		delete (*i);
	}
	m_Styles.clear();

	if(m_pNext)
		delete m_pNext;
}

CustomStyleCollection* CustomStyleCollection::GetNext()
{
	return m_pNext;
}

void CustomStyleCollection::SetNext(CustomStyleCollection* pNext)
{
	m_pNext = pNext;
}

void CustomStyleCollection::AddStyle(StyleDetails* pStyle)
{
	m_Styles.insert(m_Styles.end(), pStyle);
}

StyleDetails* CustomStyleCollection::GetStyle(int key)
{
	for(SL_IT i = m_Styles.begin(); i != m_Styles.end(); ++i)
	{
		if( (*i)->Key == key )
			return (*i);
	}

	return NULL;
}

void CustomStyleCollection::SetName(LPCTSTR name)
{
	m_name = name;
}

void CustomStyleCollection::SetDescription(LPCTSTR description)
{
	m_description = description;
}

LPCTSTR CustomStyleCollection::GetName()
{
	return m_name.c_str();
}

LPCTSTR CustomStyleCollection::GetDescription()
{
	return m_description.c_str();
}

/////////////////////////////////////////////////////////
// CustomStyleHolder
/////////////////////////////////////////////////////////

CustomStyleHolder::CustomStyleHolder() : CustomStyleCollection()
{
	m_pCurrent = NULL;
}

void CustomStyleHolder::BeginGroup(LPCTSTR name, LPCTSTR description)
{
	CustomStyleCollection* pColl = new CustomStyleCollection;
	pColl->SetName(name);
	if(description)
		pColl->SetDescription(description);

	pColl->SetNext(m_pNext);
	m_pNext = pColl;
	m_pCurrent = pColl;
}

void CustomStyleHolder::EndGroup()
{
	m_pCurrent = NULL;
}

void CustomStyleHolder::AddStyle(StyleDetails* pStyle)
{
	if(m_pCurrent)
		m_pCurrent->AddStyle(pStyle);
	else
		CustomStyleCollection::AddStyle(pStyle);
}

/////////////////////////////////////////////////////////
// SchemeConfigParser
/////////////////////////////////////////////////////////

bool SortSchemes(SchemeConfig* p1, SchemeConfig* p2)
{
	return (p1->m_Title < p2->m_Title);
}

SchemeConfigParser::SchemeConfigParser()
{
	m_pCurrent = NULL;
}

SchemeConfigParser::~SchemeConfigParser()
{
	LIST_SCHEMECONFIGS::iterator i;
	for(i = m_Schemes.begin(); i != m_Schemes.end(); ++i)
	{
		delete (*i);
	}
	m_Schemes.clear();
}

LIST_SCHEMECONFIGS& SchemeConfigParser::GetSchemes()
{
	return m_Schemes;
}

void SchemeConfigParser::LoadConfig(LPCTSTR path, LPCTSTR compiledpath)
{
	CString UserSettingsFile = compiledpath;
	UserSettingsFile += _T("UserSettings.xml");

	Parse(path, _T("master.scheme"), (LPCTSTR)UserSettingsFile);
	Sort();
}

void SchemeConfigParser::onLexer(LPCTSTR name, int styleBits)
{

}

void SchemeConfigParser::onLanguage(LPCTSTR name, LPCTSTR title, int foldflags)
{
	PNASSERT(m_pCurrent == NULL);

	m_pCurrent = new SchemeConfig;
	m_pCurrent->m_Name = name;
	m_pCurrent->m_Title = title;
	m_pCurrent->m_foldflags = foldflags;
	m_Schemes.insert(m_Schemes.begin(), m_pCurrent);
}

void SchemeConfigParser::onLanguageEnd()
{
	m_pCurrent = NULL;
}

void SchemeConfigParser::onStyleGroup(XMLAttributes& att)
{
	PNASSERT(m_pCurrent != NULL);

	LPCTSTR name = att.getValue(_T("name"));
	if(name)
	{
		m_pCurrent->BeginGroup(name, att.getValue(_T("description")));
	}
}

void SchemeConfigParser::onStyle(StyleDetails* pStyle, StyleDetails* pCustom)
{
	PNASSERT(m_pCurrent != NULL);

	StyleDetails* pCopy = new StyleDetails(*pStyle);
	m_pCurrent->AddStyle(pCopy);

	if(pCustom)
	{
		StyleDetails* pC = new StyleDetails(*pStyle);
		customiseStyle(pC, pCustom);
		m_pCurrent->m_customs.AddStyle(pC);
	}
}

void SchemeConfigParser::onStyleGroupEnd()
{
	m_pCurrent->EndGroup();
}

void SchemeConfigParser::onKeywords(int key, LPCTSTR keywords)
{
	PNASSERT(m_pCurrent != NULL);

	CustomKeywordSet* pSet = new CustomKeywordSet;
	pSet->key = key;
	pSet->pWords = new TCHAR[_tcslen(keywords)+1];
	_tcscpy(pSet->pWords, keywords);
	m_pCurrent->AddKeywordSet(pSet);
}

void SchemeConfigParser::onFile(LPCTSTR filename)
{

}

void SchemeConfigParser::Sort()
{
///@todo find a way to do this in VC6
#if (_ATL_VER >= 0x0700)
	m_Schemes.sort(SortSchemes);
#endif
}