#include "stdafx.h"
#include "schemeconfig.h"

//#include <algorithm>
//using std::sort;

/////////////////////////////////////////////////////////
// SchemeConfig
/////////////////////////////////////////////////////////

SchemeConfig::SchemeConfig()
{

}

SchemeConfig::SchemeConfig(const SchemeConfig& copy)
{
	*this = copy;
}

SchemeConfig& SchemeConfig::operator = (const SchemeConfig& copy)
{
	m_Name = copy.m_Name;
	m_Title = copy.m_Title;

	// Copy Keyword sets...
	CustomKeywordSet* pS = copy.pKeywordSets;
	while(pS)
	{
		CustomKeywordSet* pSet = new CustomKeywordSet;
		pSet->key = pS->key;
		pSet->pWords = new TCHAR[_tcslen(pS->pWords)+1];
		_tcscpy(pSet->pWords, pS->pWords);
		AddKeywordSet(pSet);

		pS = pS->pNext;
	}

	// Copy styles...
	STYLES_LIST::const_iterator i;
	for(i = copy.m_Styles.begin(); i != copy.m_Styles.end(); ++i)
	{
		StyleDetails* pStyle = *i;
		StyleDetails* pNew = new StyleDetails;
		*pNew = *(*i);
		m_Styles.insert(m_Styles.end(), pNew);
	}

	return *this;
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

void SchemeConfigParser::onLexer(LPCTSTR name, int styleBits)
{

}

void SchemeConfigParser::onLanguage(LPCTSTR name, LPCTSTR title, int foldflags)
{
	PNASSERT(m_pCurrent == NULL);

	m_pCurrent = new SchemeConfig;
	m_Schemes.insert(m_Schemes.begin(), m_pCurrent);
}

void SchemeConfigParser::onLanguageEnd()
{
	m_pCurrent = NULL;
}

void SchemeConfigParser::onStyle(StyleDetails* pStyle)
{
	PNASSERT(m_pCurrent != NULL);

	m_pCurrent->m_Styles.insert(m_pCurrent->m_Styles.begin(), pStyle);
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
	m_Schemes.sort(SortSchemes);
}