/**
 * @file SchemeConfig.h
 * @brief Scheme configuration classes.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef schemeconfig_h__included
#define schemeconfig_h__included

#include "SchemeCompiler.h"
#include "files.h"

/**
 * @brief collection of StyleDetails objects
 */
class CustomStyleCollection : public StylesList
{
	_NO_COPY(CustomStyleCollection)
	public:
		CustomStyleCollection();
		~CustomStyleCollection();

		//virtual void AddStyle(StyleDetails* pStyle);
		//void RemoveStyle(int key);
		//void RemoveAll();
		//StyleDetails* GetStyle(int key);

		CustomStyleCollection* GetNext();
		void SetNext(CustomStyleCollection* pNext);
		void SetName(LPCTSTR name);
		void SetDescription(LPCTSTR description);
		void SetClassName(LPCTSTR classname);

		LPCTSTR GetName();
		LPCTSTR GetDescription();
		LPCTSTR GetClassName();

		StyleDetails* FindStyle(int key);

		//StylesList m_Styles;

	protected:
		tstring	m_name;
		tstring	m_description;
		tstring	m_classname; // for groups with classes.

		CustomStyleCollection* m_pNext;
};

/**
 * @brief Subclass of CustomStyleCollection providing linked-list based grouping.
 */
class CustomStyleHolder : public CustomStyleCollection
{
	_NO_COPY(CustomStyleHolder)
	public:
		CustomStyleHolder();
		virtual void AddStyle(StyleDetails* pStyle);

		void BeginGroup(LPCTSTR name, LPCTSTR description = NULL, LPCTSTR classname = NULL);
		void EndGroup();
	protected:
        CustomStyleCollection* m_pCurrent;
};

class SchemeConfigParser;

/**
 * @brief Class representing the configuration of one scheme.
 *
 * Stores custom styles and keywords, and also stores the existing
 * styles and keywords for a single scheme.
 */
class SchemeConfig : public CustomKeywordHolder, public CustomStyleHolder
{
	_NO_COPY(SchemeConfig)
	public:
		SchemeConfig(SchemeConfigParser* pOwner){m_pOwner = pOwner;}

		// Style class customisation support
		void AddCustomStyleClass(const CString& name, StyleDetails* pCustom);
		void RemoveCustomStyleClass(const CString& name);
		StyleDetails* FindStyleClass(LPCTSTR name);
		StyleDetails* FindStyleClass(const CString& name);
		StyleDetails* FindCustomStyleClass(LPCTSTR name);
		StyleDetails* FindCustomStyleClass(const CString& name);

		// Configuration functions
		void ResetAll();
		void UpdateGroupedStyles(CustomStyleCollection* pColl, StyleDetails* pUpdatedClass);

		bool IsInternal();

		CString m_Name;
		CString m_Title;
		int m_foldflags;

		CustomStyleCollection	m_customs;
		CustomKeywordHolder		m_cKeywords;

		EditorColours			m_editorColours;

	protected:
		SchemeConfigParser*		m_pOwner;
};

typedef list<SchemeConfig*>	LIST_SCHEMECONFIGS;
typedef LIST_SCHEMECONFIGS::iterator SCF_IT;

/**
 * @brief Sub-Class of the Scheme file parser, specifically for configuration...
 */
class SchemeConfigParser : public SchemeParser
{
	public:
		SchemeConfigParser(LPCTSTR currentScheme = NULL);
		~SchemeConfigParser();

		void LoadConfig(LPCTSTR path, LPCTSTR compiledpath);
		void SaveConfig();

		LPCTSTR GetCurrentScheme();

		LIST_SCHEMECONFIGS&		GetSchemes();
		StylesMap&				GetStyleClasses();
		StylesMap&				GetCustomClasses();
		StyleDetails*			GetDefaultStyle();

		SchemeConfig*			GetPlainTextScheme();

		StylesMap				m_originalclasses;
		StylesMap				m_customclasses;

	protected:
		void Sort();
		void Save(LPCTSTR filename);
		void WriteStyle(CFile& file, StyleDetails& style, bool bIsClass = false);

		inline void AddBoolParam(CString& buf, LPCTSTR name, bool bVal);
		inline void AddColourParam(CString& buf, LPCTSTR name, COLORREF colour);

		LIST_SCHEMECONFIGS	m_Schemes;
		SchemeConfig*		m_pCurrent;
		CString				m_Path;
		CString				m_CurrentScheme;
		SchemeConfig		m_DefaultScheme;

	// SchemeParser
	protected:
		virtual void onLexer(LPCTSTR name, int styleBits);
		virtual void onLanguage(LPCTSTR name, LPCTSTR title, int foldflags);
		virtual void onLanguageEnd();
		virtual void onStyleGroup(XMLAttributes& att, StyleDetails* pClass);
		virtual void onStyle(StyleDetails* pStyle, StyleDetails* pCustom);
		virtual void onStyleGroupEnd();
		virtual void onStyleClass(StyleDetails* pClass, StyleDetails* pCustom);
		virtual void onProperty(LPCTSTR name, LPCTSTR value){}
		virtual void onKeywords(int key, LPCTSTR keywords, LPCTSTR name, LPCTSTR custom);
		virtual void onFile(LPCTSTR filename);
		virtual void onColours(const EditorColours* colours);
		virtual void onError(XMLParserException& ex){}
};


#endif