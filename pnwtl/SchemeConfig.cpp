/**
 * @file SchemeConfig.cpp
 * @brief Scheme configuration classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2005 Simon Steele <s.steele@pnotepad.org>
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
// CustomStyleCollection
/////////////////////////////////////////////////////////

CustomStyleCollection::CustomStyleCollection()
{
	m_pNext = NULL;
}

CustomStyleCollection::~CustomStyleCollection()
{
	ClearStyles();

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

/**
 * This function will find a style if it exists in this, or any
 * related CustomStyleCollection instance.
 */
StyleDetails* CustomStyleCollection::FindStyle(int key)
{
	StyleDetails* pS = GetStyle(key);
	if(!pS && m_pNext)
	{
		pS = m_pNext->FindStyle(key);
	}
	
	return pS;
}

void CustomStyleCollection::SetName(LPCTSTR name)
{
	m_name = name;
}

void CustomStyleCollection::SetDescription(LPCTSTR description)
{
	m_description = description;
}

void CustomStyleCollection::SetClassName(LPCTSTR classname)
{
	m_classname = classname;
}

LPCTSTR CustomStyleCollection::GetName()
{
	return m_name.c_str();
}

LPCTSTR CustomStyleCollection::GetDescription()
{
	return m_description.c_str();
}

LPCTSTR CustomStyleCollection::GetClassName()
{
	return m_classname.c_str();
}

/////////////////////////////////////////////////////////
// CustomStyleHolder
/////////////////////////////////////////////////////////

CustomStyleHolder::CustomStyleHolder() : CustomStyleCollection()
{
	m_pCurrent = NULL;
}

void CustomStyleHolder::BeginGroup(LPCTSTR name, LPCTSTR description, LPCTSTR classname)
{
	CustomStyleCollection* pColl = new CustomStyleCollection;
	pColl->SetName(name);
	if(description)
		pColl->SetDescription(description);
	if(classname)
		pColl->SetClassName(classname);

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
// SchemeConfig
/////////////////////////////////////////////////////////

SchemeConfig::SchemeConfig(SchemeConfigParser* pOwner)
{
	m_pOwner = pOwner; 
	m_tabwidth = 4; 
	m_foldflags = 0;
	m_origfoldflags = 0;
}

StyleDetails* SchemeConfig::FindStyleClass(LPCTSTR name)
{
	return FindStyleClass(CString(name));
}

StyleDetails* SchemeConfig::FindStyleClass(const CString& name)
{
	return m_pOwner->GetStyleClasses().GetStyle(name);
}

StyleDetails* SchemeConfig::FindCustomStyleClass(LPCTSTR name)
{
	return FindCustomStyleClass(CString(name));
}

StyleDetails* SchemeConfig::FindCustomStyleClass(const CString& name)
{
	return m_pOwner->GetCustomClasses().GetStyle(name);
}

void SchemeConfig::AddCustomStyleClass(const CString& name, StyleDetails* pCustom)
{
	m_pOwner->GetCustomClasses().AddStyle(name, pCustom);
}

void SchemeConfig::RemoveCustomStyleClass(const CString& name)
{
	m_pOwner->GetCustomClasses().DeleteStyle(name);
}

void SchemeConfig::UpdateGroupedStyles(CustomStyleCollection* pColl, StyleDetails* pUpdatedClass)
{
	for(SL_CIT i = pColl->StylesBegin(); i != pColl->StylesEnd(); ++i)
	{
		StyleDetails* pStyle = (*i);

		StyleDetails* pCustom = m_customs.GetStyle(pStyle->Key);
		if(pCustom)
		{
			// set the ->values property with the differences mask.
			pCustom->compareTo(*pStyle);
			
			// We don't want to change anything that the original style 
			// or the custom style changes, combine their mask.
			pCustom->values |= pStyle->values;

			pCustom->updateUnmasked(*pUpdatedClass);
		}
		
		pStyle->updateUnmasked(*pUpdatedClass);
	}

}

void SchemeConfig::ResetAll()
{
	// Remove all custom styles
	m_customs.DeleteAllStyles();

	// Remove all custom style class explicitly linked to groups, assuming we don't have one.
	CustomStyleCollection* pColl = m_pNext;
	while(pColl)
	{
		CString classname = pColl->GetClassName();
		if(classname.GetLength() != 0)
		{
			RemoveCustomStyleClass(classname);
			StyleDetails* pStyleClass = FindStyleClass(classname);
			if(pStyleClass) //safety chex
				UpdateGroupedStyles(pColl, pStyleClass);
		}

		pColl = pColl->GetNext();
	}
}

bool SchemeConfig::IsInternal() const
{
	return (m_foldflags & schInternal) != 0;
}

bool SchemeConfig::IsCustomised() const
{
	CustomKeywordSet* pKeywordSet = m_cKeywords.GetFirstKeywordSet();

	return	(m_customs.StylesCount() != 0) ||
			(pKeywordSet != NULL) || 
			m_editorColours.HasColours();
}

/////////////////////////////////////////////////////////
// SchemeConfigParser
/////////////////////////////////////////////////////////

SchemeConfigParser::SchemeConfigParser(LPCTSTR currentScheme)
	: m_DefaultScheme(this)
{
	m_pCurrent = NULL;

	if(currentScheme)
		m_CurrentScheme = currentScheme;

	m_DefaultScheme.m_Name = _T("default");
	m_DefaultScheme.m_Title = _T("Plain Text");
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
SchemeConfig* SchemeConfigParser::GetPlainTextScheme()
{
	return &m_DefaultScheme;
}

void SchemeConfigParser::LoadConfig(LPCTSTR path, LPCTSTR compiledpath)
{
	m_Path = compiledpath;
	m_Path += _T("UserSettings.xml");

	Parse(path, _T("master.scheme"), (LPCTSTR)m_Path);
	
	/* The classes used while building up the styles are not the original
	classes as seen in the config files. Those are stored in our own 
	m_originalclasses member. We don't need the LoadState ones any more. */
	m_LoadState.m_StyleClasses.Clear();
	m_LoadState.m_CustomClasses.Clear();

	Sort();
}

void SchemeConfigParser::SaveConfig()
{
	Save((LPCTSTR)m_Path);
}

LPCTSTR SchemeConfigParser::GetCurrentScheme()
{
	return (LPCTSTR)m_CurrentScheme;
}

/*void SchemeConfigParser::WriteStyle(Schemes::Writer& writer, StyleDetails& style, bool bIsClass)
{
	if(bIsClass)
		writer.beginStyleClass(style.name.c_str());
	else
		writer.beginStyle(style.Key);

	if(style.values & edvFontName)
		writer.setFont(style.FontName.c_str());
	if(style.values & edvFontSize)
		writer.setSize(style.FontSize);
	if(style.values & edvForeColor)
		writer.setFore(style.ForeColor);
	if(style.values & edvBackColor)
		writer.setBack(style.BackColor);
	if(style.values & edvBold)
		writer.setBold(style.Bold);
	if(style.values & edvItalic)
		writer.setItalic(style.Italic);
	if(style.values & edvUnderline)
		writer.setUnderline(style.Underline);
	if(style.values & edvEOLFilled)
		writer.setEolFilled(style.EOLFilled);
	// @todo
	if(style.values & edvClass && !bIsClass)
		writer.setClass(style.classname.c_str());

	if(bIsClass)
		writer.endStyleClass();
	else
		writer.endStyle();
}*/

void SchemeConfigParser::Save(LPCTSTR filename)
{
	Schemes::Writer writer;

	StyleDetails* pStyle = NULL;
	StyleDetails* pOrig = NULL;
	
	writer.Start(filename);
	writer.beginDoc();

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

		for(SCF_IT i = m_Schemes.begin(); i != m_Schemes.end(); ++i)
		{
			CustomKeywordSet* pKeywordSet = (*i)->m_cKeywords.GetFirstKeywordSet();

			bool bCustomised = ((*i)->m_customs.StylesCount() != 0) ||
				(pKeywordSet != NULL) || (*i)->m_editorColours.HasColours();

			if(!bCustomised)
				continue;

			// Write out scheme block...
			writer.beginScheme( (*i) );

			writer.writeEditorColours( &((*i)->m_editorColours) );

			if(pKeywordSet != NULL)
			{
				// Keywords
				writer.beginOverrideKeywords();
				while(pKeywordSet)
				{
					writer.writeKeywords(pKeywordSet->key, pKeywordSet->pWords);
				}
				writer.endOverrideKeywords();
			}

			if((*i)->m_customs.StylesCount() != 0)
			{
				// Styles
				writer.beginOverrideStyles();
				for(SL_CIT j = (*i)->m_customs.StylesBegin();
					j != (*i)->m_customs.StylesEnd();
					++j)
				{
					StyleDetails* pOriginal = (*i)->FindStyle( (*j)->Key );
					if(pOriginal)
					{
						(*j)->compareTo(*pOriginal);
						writer.writeStyle(*(*j));
					}
					else
					{
						ATLTRACE(_T("PN2 Warning: Custom style with no original...\n"));
					}
				}
				writer.endOverrideStyles();
			}

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

	m_pCurrent = new SchemeConfig(this);
	m_pCurrent->m_Name = name;
	m_pCurrent->m_Title = title;
	m_pCurrent->m_foldflags = foldflags;
	m_pCurrent->m_origfoldflags = ncfoldflags;
	m_Schemes.insert(m_Schemes.begin(), m_pCurrent);
}

void SchemeConfigParser::onLanguageEnd()
{
	m_pCurrent = NULL;
}

void SchemeConfigParser::onStyleGroup(XMLAttributes& att, StyleDetails* pClass)
{
	PNASSERT(m_pCurrent != NULL);

	LPCTSTR name = att.getValue(_T("name"));
	if(name)
	{
		m_pCurrent->BeginGroup(name, att.getValue(_T("description")), (pClass ? pClass->name.c_str() : NULL) );
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

void SchemeConfigParser::onStyleClass(StyleDetails* pClass, StyleDetails* pCustom)
{
	StyleDetails* pStore = new StyleDetails(*pClass);
	
	m_originalclasses.AddStyle(pStore->name.c_str(), pStore);

    if(pCustom)
	{
		customiseStyle(pClass, pCustom);

		StyleDetails* pC = new StyleDetails(*pClass);
		m_customclasses.AddStyle(pC->name.c_str(), pC);
	}
}

void SchemeConfigParser::onStyleGroupEnd()
{
	m_pCurrent->EndGroup();
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
	m_pCurrent->AddKeywordSet(pSet);

	if(custom)
	{
		CustomKeywordSet* pCustomSet = new CustomKeywordSet(*pSet);
		pCustomSet->pWords = new TCHAR[_tcslen(custom)+1];
		_tcscpy(pCustomSet->pWords, custom);
		m_pCurrent->m_cKeywords.AddKeywordSet(pCustomSet);
	}
}

void SchemeConfigParser::onFile(LPCTSTR /*filename*/)
{

}

void SchemeConfigParser::onColours(const EditorColours* colours)
{
	m_pCurrent->m_editorColours = *colours;
}

bool SortSchemes(SchemeConfig* p1, SchemeConfig* p2)
{
	return (p1->m_Title < p2->m_Title);
}

void SchemeConfigParser::Sort()
{
///@todo find a way to do this in VC6
#if (_ATL_VER >= 0x0700)
	m_Schemes.sort(SortSchemes);
#endif
}