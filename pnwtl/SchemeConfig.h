#ifndef schemeconfig_h__included
#define schemeconfig_h__included

#include "SchemeCompiler.h"
#include "files.h"

/**
 * @brief collection of StyleDetails objects
 */
class CustomStyleCollection
{
	_NO_COPY(CustomStyleCollection)
	public:
		CustomStyleCollection();
		~CustomStyleCollection();

		virtual void AddStyle(StyleDetails* pStyle);
		void RemoveStyle(int key);
		void RemoveAll();

		CustomStyleCollection* GetNext();
		void SetNext(CustomStyleCollection* pNext);
		void SetName(LPCTSTR name);
		void SetDescription(LPCTSTR description);

		LPCTSTR GetName();
		LPCTSTR GetDescription();

		StyleDetails* GetStyle(int key);
		StyleDetails* FindStyle(int key);

		STYLES_LIST	m_Styles;

	protected:
		ctcString m_name;
		ctcString m_description;

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

		void BeginGroup(LPCTSTR name, LPCTSTR description = NULL);
		void EndGroup();
	protected:
        CustomStyleCollection* m_pCurrent;
};

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
		SchemeConfig(){}

		CString m_Name;
		CString m_Title;
		int m_foldflags;

		CustomStyleCollection	m_customs;
		CustomKeywordHolder		m_cKeywords;
};

typedef list<SchemeConfig*>	LIST_SCHEMECONFIGS;
typedef LIST_SCHEMECONFIGS::iterator SCF_IT;

/**
 * @brief Sub-Class of the Scheme file parser, specifically for configuration...
 */
class SchemeConfigParser : public SchemeParser
{
	_NO_COPY(SchemeConfigParser)
	public:
		SchemeConfigParser();
		~SchemeConfigParser();

		void LoadConfig(LPCTSTR path, LPCTSTR compiledpath);
		void SaveConfig();

		LIST_SCHEMECONFIGS&		GetSchemes();
		StylesMap&				GetStyleClasses();
		StylesMap&				GetCustomClasses();
		StyleDetails*			GetDefaultStyle();

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

	// SchemeParser
	protected:
		virtual void onLexer(LPCTSTR name, int styleBits);
		virtual void onLanguage(LPCTSTR name, LPCTSTR title, int foldflags);
		virtual void onLanguageEnd();
		virtual void onStyleGroup(XMLAttributes& att);
		virtual void onStyle(StyleDetails* pStyle, StyleDetails* pCustom);
		virtual void onStyleGroupEnd();
		virtual void onStyleClass(StyleDetails* pClass, StyleDetails* pCustom);
		virtual void onKeywords(int key, LPCTSTR keywords, LPCTSTR name, LPCTSTR custom);
		virtual void onFile(LPCTSTR filename);	
};


#endif