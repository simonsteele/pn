#ifndef schemeconfig_h__included
#define schemeconfig_h__included

#include "SchemeCompiler.h"

class SchemeConfig : public CustomisedScheme
{
	public:
		SchemeConfig();
		SchemeConfig(const SchemeConfig& copy);

		SchemeConfig& operator = (const SchemeConfig& copy);

		CString m_Name;
		CString m_Title;

		
};

typedef list<SchemeConfig*>	LIST_SCHEMECONFIGS;

class SchemeConfigParser : public SchemeParser
{
	_NO_COPY(SchemeConfigParser)
	public:
		SchemeConfigParser();

	protected:
		void Sort();
		LIST_SCHEMECONFIGS	m_Schemes;
		SchemeConfig*		m_pCurrent;

	// SchemeParser
	protected:
		virtual void onLexer(LPCTSTR name, int styleBits);
		virtual void onLanguage(LPCTSTR name, LPCTSTR title, int foldflags);
		virtual void onLanguageEnd();
		virtual void onStyle(StyleDetails* pStyle);
		virtual void onKeywords(int key, LPCTSTR keywords);
		virtual void onFile(LPCTSTR filename);
};


#endif