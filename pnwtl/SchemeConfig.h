/**
 * @file SchemeConfig.h
 * @brief Scheme configuration classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
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
		SchemeConfigParser(LPCSTR currentScheme = NULL);
		~SchemeConfigParser();

		void LoadConfig(LPCTSTR path, LPCTSTR compiledpath);
		void SaveConfig(LPCTSTR userSettingsPath);

		void LoadPresets(LPCTSTR path);

		LPCSTR GetCurrentScheme();

		void ResetClasses();

		SchemeDetailsList&	GetSchemes();
		StyleDetails*		GetDefaultStyle();
		EditorColours*		GetDefaultColours();

		SchemeDetails*		GetPlainTextScheme();

		StylePtr			GetClass(LPCTSTR name);

		StylePtrMap&		GetClasses();

	protected:
		void Sort();
		void Save(LPCTSTR filename);
		bool validateFont(LPCTSTR fontName);

		SchemeDetailsList	m_Schemes;
		SchemeDetails*		m_pCurrent;
		tstring				m_Path;
		std::string			m_CurrentScheme;
		SchemeDetails		m_DefaultScheme;

	// SchemeParser
	protected:
		virtual void onLexer(LPCTSTR name, int styleBits);
		virtual void onLanguage(LPCSTR name, LPCTSTR title, int foldflags, int ncfoldflags);
		virtual void onLanguageEnd();
		virtual void onStyleGroup(const XMLAttributes& att, const StylePtr& pClass);
		virtual void onStyle(const StylePtr& style, bool isBaseStyle);
		virtual void onStyleGroupEnd();
		virtual void onStyleClass(const StylePtr& style);
		virtual void onProperty(LPCTSTR name, LPCTSTR value){}
		virtual void onKeywords(int key, LPCSTR keywords, LPCTSTR name, LPCSTR custom);
		virtual void onFile(LPCTSTR filename);
		virtual void onColours(const EditorColours* defCols, const EditorColours* colours);
		virtual void onError(XMLParserException& ex){}
		virtual void onCommentSpec(const char *,const char *,const char *,const char *,const char *,const char *){}
		virtual void onWordChars(const char* charset){}
};


#endif