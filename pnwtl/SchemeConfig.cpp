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
	RemoveAll();

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

/**
 * This function will find a style if it exists in this
 * CustomStyleCollection instance.
 */
StyleDetails* CustomStyleCollection::GetStyle(int key)
{
	for(SL_IT i = m_Styles.begin(); i != m_Styles.end(); ++i)
	{
		if( (*i)->Key == key )
			return (*i);
	}

	return NULL;
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

void CustomStyleCollection::RemoveStyle(int key)
{
	StyleDetails* pS = GetStyle(key);
	if(pS)
		m_Styles.remove(pS);
	delete pS;
}

void CustomStyleCollection::RemoveAll()
{
	for(SL_IT i = m_Styles.begin(); i != m_Styles.end(); ++i)
	{
		delete (*i);
	}
	m_Styles.clear();
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

#define USERSETTINGS_START			"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<UserSettings>\n"
#define USERSETTINGS_END			"</UserSettings>"
#define USERSETTINGS_SCHEMES_START	"\t<schemes>\n"
#define USERSETTINGS_SCHEMES_END	"\t</schemes>\n"
#define USERSETTINGS_SCHEME_START	_T("\t\t<scheme name=\"%s\">\n")
#define USERSETTINGS_SCHEME_END		"\t\t</scheme>\n"
#define USERSETTINGS_STYLES_START	"\t\t\t<override-styles>\n"
#define USERSETTINGS_STYLES_END		"\t\t\t</override-styles>\n"
#define USERSETTINGS_CLASSES_START	"\t<override-classes>\n"
#define USERSETTINGS_CLASSES_END	"\t</override-classes>\n"

#define USERSETTINGS_STYLE_START	_T("\t\t\t\t<style key=\"%d\" ")
#define USERSETTINGS_CLASS_START	_T("\t\t<style-class name=\"%s\" ")
#define USERSETTINGS_STYLE_FN		_T("font=\"%s\" ")
#define USERSETTINGS_STYLE_FS		_T("size=\"%d\" ")
#define USERSETTINGS_STYLE_FC		_T("fore")
#define USERSETTINGS_STYLE_BC		_T("back")
#define USERSETTINGS_STYLE_SB		_T("bold")
#define USERSETTINGS_STYLE_SI		_T("italic")
#define USERSETTINGS_STYLE_SU		_T("underline")
#define USERSETTINGS_STYLE_SE		_T("eolfilled")
#define USERSETTINGS_STYLE_CL		_T("class=\"%s\" ")

void SchemeConfigParser::AddBoolParam(CString& buf, LPCTSTR name, bool bVal)
{
	buf += name;
	buf += _T("=\"");
	buf += bVal ? _T("true\" ") : _T("false\" ");
}

void SchemeConfigParser::AddColourParam(CString& buf, LPCTSTR name, COLORREF colour)
{
	buf += name;
	buf += _T("=\"");
	
	char colbuf[12];
	colbuf[11] = NULL;
	_sntprintf(colbuf, 11, _T("%.2x%.2x%.2x\" "), GetRValue(colour), GetGValue(colour), GetBValue(colour));
	buf += colbuf;
}

void SchemeConfigParser::WriteStyle(CFile& file, StyleDetails& style, bool bIsClass)
{
	USES_CONVERSION;

	CString stylestr;
	CString tempstr;

	if(bIsClass)
	{
		stylestr.Format(USERSETTINGS_CLASS_START, A2CT(style.name.c_str()));
	}
	else
	{
		stylestr.Format(USERSETTINGS_STYLE_START, style.Key);
	}
	
	if(style.values & edvFontName)
	{
		tempstr.Format(USERSETTINGS_STYLE_FN, A2CT(style.FontName.c_str()));
		stylestr += tempstr;
	}
	if(style.values & edvFontSize)
	{
		tempstr.Format(USERSETTINGS_STYLE_FS, style.FontSize);
		stylestr += tempstr;
	}
	if(style.values & edvForeColor)
		AddColourParam(stylestr, USERSETTINGS_STYLE_FC, style.ForeColor);

	if(style.values & edvBackColor)
		AddColourParam(stylestr, USERSETTINGS_STYLE_BC, style.BackColor);

	if(style.values & edvBold)
		AddBoolParam(stylestr, USERSETTINGS_STYLE_SB, style.Bold);

	if(style.values & edvItalic)
		AddBoolParam(stylestr, USERSETTINGS_STYLE_SI, style.Italic);

	if(style.values & edvUnderline)
		AddBoolParam(stylestr, USERSETTINGS_STYLE_SU, style.Underline);

	if(style.values & edvEOLFilled)
		AddBoolParam(stylestr, USERSETTINGS_STYLE_SE, style.EOLFilled);

	// @todo
	if(style.values & edvClass && !bIsClass)
	{
		tempstr.Format(USERSETTINGS_STYLE_CL, A2CT(style.classname.c_str()));
		stylestr += tempstr;
	}

	stylestr += _T("/>\n");
	const char* pSS = T2CA((LPCTSTR)stylestr);
	file.Write((void*)pSS, strlen(pSS));
}

void SchemeConfigParser::Save(LPCTSTR filename)
{
	USES_CONVERSION;

	CFile file;
	CString s;
	StyleDetails* pStyle = NULL;
	StyleDetails* pOrig = NULL;
	
	file.Open(filename, CFile::modeWrite | CFile::modeBinary);

	// Write file header, and begin Schemes section.
	file.Write(USERSETTINGS_START, strlen(USERSETTINGS_START));

	// Style Classes
	if(GetCustomClasses().GetCount() != 0)
	{
        file.Write(USERSETTINGS_CLASSES_START, strlen(USERSETTINGS_CLASSES_START));

		ctcString name;
		STYLEDETAILS_NAMEMAP& map = GetCustomClasses().GetMap();
		for(SDNM_IT j = map.begin(); j != map.end(); ++j)
		{
			pStyle = (*j).second;
			name = pStyle->name;
			{
				pOrig = GetStyleClasses().GetStyle(name.c_str());
				if(pOrig)
				{					
					pStyle->compareTo(*pOrig);
				}
				else
					(*j).second->values = ~ 0;
			}

			WriteStyle(file, * (*j).second, true);
		}

		file.Write(USERSETTINGS_CLASSES_END, strlen(USERSETTINGS_CLASSES_END));
	}

	// Schemes
	if(m_Schemes.size() != 0)
	{
		file.Write(USERSETTINGS_SCHEMES_START, strlen(USERSETTINGS_SCHEMES_START));

		for(SCF_IT i = m_Schemes.begin(); i != m_Schemes.end(); ++i)
		{
			// Scheme
			if((*i)->m_customs.m_Styles.size() != 0)
			{
				s.Format(USERSETTINGS_SCHEME_START, (LPCTSTR)(*i)->m_Name);
				const char* pS = T2CA((LPCTSTR)s);
				file.Write((void*)pS, strlen(pS));

				// Styles
				file.Write(USERSETTINGS_STYLES_START, strlen(USERSETTINGS_STYLES_START));
				for(SL_IT j = (*i)->m_customs.m_Styles.begin();
					j != (*i)->m_customs.m_Styles.end();
					++j)
				{
					StyleDetails* pOriginal = (*i)->FindStyle( (*j)->Key );
					if(pOriginal)
					{
						(*j)->compareTo(*pOriginal);
						WriteStyle(file, *(*j));
					}
					else
					{
						ATLTRACE(_T("PN2 Warning: Custom style with no original...\n"));
					}
				}
				file.Write(USERSETTINGS_STYLES_END, strlen(USERSETTINGS_STYLES_END));
		        		
				// End of Scheme
				file.Write(USERSETTINGS_SCHEME_END, strlen(USERSETTINGS_SCHEME_END));
			}
		}

		// End of Schemes...
		file.Write(USERSETTINGS_SCHEMES_END, strlen(USERSETTINGS_SCHEMES_END));
	}
	
	// End of file.
	file.Write(USERSETTINGS_END, strlen(USERSETTINGS_END));
	file.Close();
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