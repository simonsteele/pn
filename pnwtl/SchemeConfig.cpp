/**
 * @file SchemeConfig.cpp
 * @brief Scheme configuration classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "schemeconfig.h"

#include "third_party/genx/genx.h"
#include "include/pngenx.h"
#include "usersettingswriter.h"

/////////////////////////////////////////////////////////
// SchemeConfigParser
/////////////////////////////////////////////////////////

SchemeConfigParser::SchemeConfigParser(LPCSTR currentScheme)
	: m_DefaultScheme("default")
{
	m_pCurrent = NULL;

	if(currentScheme)
		m_CurrentScheme = currentScheme;

	m_DefaultScheme.Title = LS(IDS_DEFAULTSCHEME);
}

SchemeConfigParser::~SchemeConfigParser()
{
	// The schemes are owned by the SchemeLoaderState object, don't free them.
	m_Schemes.clear();
}

SchemeDetailsList& SchemeConfigParser::GetSchemes()
{
	return m_Schemes;
}

StylePtr SchemeConfigParser::GetClass(LPCTSTR name)
{
	return m_LoadState.GetClass(name);
}

StylePtrMap& SchemeConfigParser::GetClasses()
{
	return m_LoadState.m_Classes;
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

void SchemeConfigParser::LoadPresets(LPCTSTR path)
{
	// Reset the current settings:
	m_LoadState.m_DefaultColours.Clear();

	for(SchemeDetailsList::iterator i = m_Schemes.begin();
		i != m_Schemes.end();
		++i)
	{
		(*i)->ResetAll();
	}

	for(StylePtrMap::iterator j = m_LoadState.m_Classes.begin();
		j != m_LoadState.m_Classes.end();
		++j)
	{
		(*j).second->Reset();
	}

	// Now load the new...
	UserSettingsParser usp;
	usp.SetPresetLoadMode();
	usp.Parse(path, &m_LoadState);

	StylePtr defcls = GetClass(_T("default"));

	// Minor validation (want our default font to look good):
	if(defcls->CustomStyle != NULL && (defcls->CustomStyle->values & edvFontName))
	{
		if(!validateFont(defcls->CustomStyle->FontName.c_str()))
		{
			defcls->CustomStyle->FontName = _T("");
			defcls->CustomStyle->values ^= edvFontName;
		}
	}

	// Now re-set the default style:
	defcls->Combine(NULL, m_LoadState.m_Default);
}

void SchemeConfigParser::SaveConfig(LPCTSTR userSettingsPath)
{
	Save(userSettingsPath);
}

LPCSTR SchemeConfigParser::GetCurrentScheme()
{
	return m_CurrentScheme.c_str();
}

void SchemeConfigParser::ResetClasses()
{
	for(StylePtrMap::iterator i = m_LoadState.m_Classes.begin(); 
		i != m_LoadState.m_Classes.end();
		++i)
	{
		(*i).second->Reset();
	}

	// Also going to reset the default colours here:
	m_LoadState.m_DefaultColours.Clear();

	// Now re-set the default style:
	StylePtr defcls = GetClass(_T("default"));
	defcls->Combine(NULL, m_LoadState.m_Default);
}

void SchemeConfigParser::Save(LPCTSTR filename)
{
	Schemes::Writer writer;

	if (!writer.Start(filename))
	{
		UNEXPECTED(_T("Could not open usersettings.xml for writing."));
		return;
	}

	writer.beginDoc();

	// Colour Overrides
	writer.writeOverrideColours( GetDefaultColours() );

	// Style Classes
	bool beganClasses(false);
	
	for(StylePtrMap::const_iterator i = m_LoadState.m_Classes.begin();
		i != m_LoadState.m_Classes.end();
		++i)
	{
		const StylePtr& s = (*i).second;

		if(!s->CustomStyle)
			continue;

		if(!beganClasses)
		{
			beganClasses = true;
			writer.beginOverrideClasses();
		}
		writer.writeStyleClass(*s->CustomStyle);
	}
	
	if(beganClasses)
		writer.endOverrideClasses();

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

void SchemeConfigParser::onLanguage(LPCSTR name, LPCTSTR title, int foldflags, int ncfoldflags)
{
	PNASSERT(m_pCurrent == NULL);

	m_pCurrent = ensureSchemeDetails( m_LoadState.m_SchemeDetails, std::string(name) );

	m_pCurrent->Title = title;
	m_pCurrent->Flags = ncfoldflags;
	
	m_Schemes.push_back(m_pCurrent);
}

void SchemeConfigParser::onLanguageEnd()
{
	m_pCurrent = NULL;
}

void SchemeConfigParser::onStyleGroup(const XMLAttributes& att, const StylePtr& pClass)
{
	PNASSERT(m_pCurrent != NULL);

	LPCTSTR name = att.getValue(_T("name"));
	if(name)
	{
		m_pCurrent->BeginStyleGroup(name, att.getValue(_T("description")), (pClass.get() ? pClass->Style->name.c_str() : NULL) );
	}
}

void SchemeConfigParser::onStyle(const StylePtr& style, bool isBaseStyle)
{
	PNASSERT(m_pCurrent != NULL);
	
	if(isBaseStyle)
	{
		// Not already stored in the current one...
        StylePtr p(new FullStyleDetails(*style.get()));
		m_pCurrent->Styles.push_back(p);
	}
}

void SchemeConfigParser::onStyleClass(const StylePtr& style)
{
}

void SchemeConfigParser::onStyleGroupEnd()
{
	m_pCurrent->EndStyleGroup();
}

void SchemeConfigParser::onKeywords(int key, LPCSTR keywords, LPCTSTR name, LPCSTR custom)
{
	PNASSERT(m_pCurrent != NULL);

	CustomKeywordSet* pSet = new CustomKeywordSet;
	pSet->key = key;
	pSet->pWords = new char[strlen(keywords)+1];
	strcpy(pSet->pWords, keywords);
	pSet->pName = new TCHAR[_tcslen(name)+1];
	_tcscpy(pSet->pName, name);
	m_pCurrent->Keywords.AddKeywordSet(pSet);

	/* - Custom already stored by the user settings loader now
	if(custom)
	{
		CustomKeywordSet* pCustomSet = new CustomKeywordSet(*pSet);
		pCustomSet->pWords = new TCHAR[_tcslen(custom)+1];
		_tcscpy(pCustomSet->pWords, custom);
		m_pCurrent->CustomKeywords.AddKeywordSet(pCustomSet);
	}*/
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

class FontValidationInfo
{
public:
	FontValidationInfo() : Found(false) {}
	
	CString Name;
	bool Found;
};

int CALLBACK ValidateFontCB( 
	ENUMLOGFONT FAR *lpelf,    // pointer to logical-font data 
	NEWTEXTMETRIC FAR *lpntm,  // pointer to physical-font data 
	int FontType,              // type of font 
	LPARAM lParam              // pointer to application-defined data 
	) 
{
	FontValidationInfo* info = reinterpret_cast<FontValidationInfo*>(lParam);
	if( info->Name == lpelf->elfLogFont.lfFaceName )
	{
		info->Found = true;
		return 0;
	}
 
    return 1; 
} 

bool SchemeConfigParser::validateFont(LPCTSTR fontName)
{
	FontValidationInfo fvi;
	fvi.Name = fontName;
	EnumFonts(GetDC(NULL), NULL, (FONTENUMPROC)ValidateFontCB, reinterpret_cast<LPARAM>(&fvi));
	return fvi.Found;
}