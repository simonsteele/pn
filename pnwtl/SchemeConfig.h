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
		void SetClassName(LPCTSTR classname);

		LPCTSTR GetName();
		LPCTSTR GetDescription();
		LPCTSTR GetClassName();

		StyleDetails* GetStyle(int key);
		StyleDetails* FindStyle(int key);

		STYLES_LIST	m_Styles;

	protected:
		ctcString m_name;
		ctcString m_description;
		ctcString m_classname; // for groups with classes.

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

		CString m_Name;
		CString m_Title;
		int m_foldflags;

		CustomStyleCollection	m_customs;
		CustomKeywordHolder		m_cKeywords;

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
		virtual void onStyleGroup(XMLAttributes& att, StyleDetails* pClass);
		virtual void onStyle(StyleDetails* pStyle, StyleDetails* pCustom);
		virtual void onStyleGroupEnd();
		virtual void onStyleClass(StyleDetails* pClass, StyleDetails* pCustom);
		virtual void onKeywords(int key, LPCTSTR keywords, LPCTSTR name, LPCTSTR custom);
		virtual void onFile(LPCTSTR filename);	
};


#endif