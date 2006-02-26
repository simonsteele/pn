/**
 * @file SchemeConfig.h
 * @brief Scheme configuration classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2006 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef schemeconfig_h__included
#define schemeconfig_h__included

#include "SchemeCompiler.h"
#include "files.h"

class SchemeConfigParser;

namespace Schemes {	class Writer; }

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

		SchemeDetailsList&	GetSchemes();
		StyleDetails*		GetDefaultStyle();
		EditorColours*		GetDefaultColours();

		SchemeDetails*		GetPlainTextScheme();

		StylePtr			GetClass(LPCTSTR name);

	protected:
		void Sort();
		void Save(LPCTSTR filename);

		SchemeDetailsList	m_Schemes;
		SchemeDetails*		m_pCurrent;
		tstring				m_Path;
		tstring				m_CurrentScheme;
		SchemeDetails		m_DefaultScheme;

	// SchemeParser
	protected:
		virtual void onLexer(LPCTSTR name, int styleBits);
		virtual void onLanguage(LPCTSTR name, LPCTSTR title, int foldflags, int ncfoldflags);
		virtual void onLanguageEnd();
		virtual void onStyleGroup(XMLAttributes& att, const StylePtr& pClass);
		virtual void onStyle(const StylePtr& style);
		virtual void onStyleGroupEnd();
		virtual void onStyleClass(const StylePtr& style);
		virtual void onProperty(LPCTSTR name, LPCTSTR value){}
		virtual void onKeywords(int key, LPCTSTR keywords, LPCTSTR name, LPCTSTR custom);
		virtual void onFile(LPCTSTR filename);
		virtual void onColours(const EditorColours* defCols, const EditorColours* colours);
		virtual void onError(XMLParserException& ex){}
};


#endif