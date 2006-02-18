/**
 * @file SchemeCompiler.h
 * @brief Define scheme reader and compiler classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2005 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * These classes make use of expat through the XMLParser wrapper class.
 *
 * Unicode Status: Unicode Ready (untested).
 */

#ifndef schemecompiler_h__included
#define schemecompiler_h__included

class CSchemeLoaderState
{
	public:
		~CSchemeLoaderState();
		CSTRING_MAP				m_Globals;
		CSTRING_MAP				m_Keywords;
		StylesMap				m_StyleClasses;
		StyleDetails			m_Default;
		EditorColours			m_DefaultColours;

		StylesList				m_BaseStyles;

		CUSTOMISED_NAMEMAP		m_CustomSchemes;
		StylesMap				m_CustomClasses;
		CustomisedScheme*		m_pCustom;
		StyleDetails*			m_pGroupClass;
		BaseScheme*				m_pBase;

		CUSTOMISED_NAMEMAP		m_BaseSchemes;

		XMLParser*				m_pParser;

		int						m_State;
		bool					m_bBaseParse;
		
		DWORD					m_StartLoad;
		DWORD					m_StartLang;
		
		CString m_csGName;
		CString m_csLangName;

		// Character Data Caching...
		CString m_csCData;

		CString m_csBasePath;
		CString m_csOutPath;
		CSTRING_LIST m_IncludeFiles;
};

// Empty class for exception source identification purposes...
class CSchemeParserException : public XMLParserException
{
	public:
		CSchemeParserException(XMLParser* pParser, LPCTSTR msg = NULL)
			: XMLParserException(pParser, msg) {}
		
		CSchemeParserException(XMLParser* pParser, XML_Error ErrorCode = XML_ERROR_NONE, LPCTSTR msg = NULL)
			: XMLParserException(pParser, ErrorCode, msg) {}
};

class SchemeRecorder : public CScintilla
{
	public:
		SchemeRecorder();
	
		bool StartRecording(LPCTSTR scheme, LPCTSTR title, LPCTSTR outfile, int FoldFlags);
		bool EndRecording();
		bool IsRecording(){return m_out != NULL;}

		virtual void Record(long Msg, WPARAM wParam, LPARAM lParam);

		void SetDefStyle(StyleDetails* defaults);

		virtual long SPerform(long Msg, WPARAM wParam=0, LPARAM lParam=0);

	protected:
		bool CheckNecessary(long Msg, WPARAM wParam, LPARAM lParam);

		void WriteHeader(LPCTSTR schemename, LPCTSTR schemetitle, int FoldFlags);

		StyleDetails	m_DefStyle;
		eNextRec		m_next;
		FILE*			m_out;
		eTextType		m_tType;
};

class UserSettingsParser
{
	public:
		UserSettingsParser();
		void Parse(LPCTSTR path, CSchemeLoaderState*	pState);

	protected:
		CustomisedScheme*	pScheme;
		CString				m_SchemeName;
		int					m_idval;

	protected:
		void characterData(void* userData, LPCTSTR data, int len);
		void endElement(void *userData, LPCTSTR name);
		void startElement(void *userData, LPCTSTR name, XMLAttributes& atts);

		void processScheme(CSchemeLoaderState* pState, XMLAttributes& atts);
		void processSchemeElement(CSchemeLoaderState* pState, LPCTSTR name, XMLAttributes& atts);
		void processClassElement(CSchemeLoaderState* pState, LPCTSTR name, XMLAttributes& atts);
		void processGlobalColours(CSchemeLoaderState* pState, XMLAttributes& atts);

		//void DefineStyle(StyleDetails* pStyle, XMLAttributes atts);
};

/**
 * XML Scheme Parser
 * Uses James Clark's XML parser expat.
 */
class SchemeParser
{
	public:
		void Parse(LPCTSTR path, LPCTSTR mainfile, LPCTSTR userfile);

		static void parseStyle(CSchemeLoaderState* pState, XMLAttributes& atts, StyleDetails* pStyle, bool bExpandGlobals = true);

	protected:
		CSchemeLoaderState	m_LoadState;

	protected:
		void characterData(void* userData, LPCTSTR data, int len);
		void endElement(void *userData, LPCTSTR name);
		void startElement(void *userData, LPCTSTR name, XMLAttributes& atts);
		void processKeywordCombine(CSchemeLoaderState* pState, XMLAttributes& atts);
		void specifyImportSet(CSchemeLoaderState* pState, XMLAttributes& atts);
		void specifyImportFile(CSchemeLoaderState* pState, XMLAttributes& atts);
		void processBaseStyle(CSchemeLoaderState* pState, XMLAttributes& atts);
		void processLanguageElement(CSchemeLoaderState* pState, LPCTSTR name, XMLAttributes& atts);
		void processLanguageKeywords(CSchemeLoaderState* pState, XMLAttributes& atts);
		void processLanguageStyle(CSchemeLoaderState* pState, XMLAttributes& atts);
		void processLanguageStyleGroup(CSchemeLoaderState* pState, XMLAttributes& atts);
		void processStyleClass(CSchemeLoaderState* pState, XMLAttributes& atts);
		void sendStyle(StyleDetails* s, SchemeRecorder* compiler);
		void processKeywordClass(CSchemeLoaderState* pState, XMLAttributes& atts);
		void processGlobal(CSchemeLoaderState* pState, XMLAttributes& atts);
		void processProperty(CSchemeLoaderState* pState, XMLAttributes& atts);
		void customiseStyle(StyleDetails* style, StyleDetails* custom);
		void sendBaseScheme(CSchemeLoaderState* pState, BaseScheme* pBase);
		void sendBaseStyle(CSchemeLoaderState* pState, StyleDetails* pS);
		void sendBaseStyles(CSchemeLoaderState* pState);

	protected:
		virtual void onLexer(LPCTSTR name, int styleBits) = 0;
		virtual void onLanguage(LPCTSTR name, LPCTSTR title, int foldflags, int ncfoldflags) = 0;
		virtual void onLanguageEnd() = 0;
		virtual void onStyleGroup(XMLAttributes& atts, StyleDetails* pClass) = 0;
		virtual void onStyle(StyleDetails* pStyle, StyleDetails* pCustom) = 0;
		virtual void onStyleGroupEnd() = 0;
		virtual void onStyleClass(StyleDetails* pClass, StyleDetails* pCustom) = 0;
		virtual void onProperty(LPCTSTR name, LPCTSTR value) = 0;
		virtual void onKeywords(int key, LPCTSTR keywords, LPCTSTR name, LPCTSTR custom) = 0;
		virtual void onFile(LPCTSTR filename) = 0;
		virtual void onColours(const EditorColours* defCols, const EditorColours* colours) = 0;
		virtual void onError(XMLParserException& ex) = 0;
};

/**
 * XML Scheme Compiler
 */
class SchemeCompiler : public SchemeParser
{
	public:
		void Compile(LPCTSTR path, LPCTSTR output, LPCTSTR mainfile);

	protected:
		SchemeRecorder m_Recorder;
		void sendStyle(StyleDetails* s, SchemeRecorder* compiler);
	
	// Implement SchemeParser
	protected:
		virtual void onLanguage(LPCTSTR name, LPCTSTR title, int foldflags, int ncfoldflags);
		virtual void onLanguageEnd();
		virtual void onStyleGroup(XMLAttributes& atts, StyleDetails* pClass){}
		virtual void onStyle(StyleDetails* pStyle, StyleDetails* pCustom);
		virtual void onStyleGroupEnd(){}
		virtual void onStyleClass(StyleDetails* pClass, StyleDetails* pCustom);
		virtual void onProperty(LPCTSTR name, LPCTSTR value);
		virtual void onFile(LPCTSTR filename);
		virtual void onKeywords(int key, LPCTSTR keywords, LPCTSTR name, LPCTSTR custom);
		virtual void onLexer(LPCTSTR name, int styleBits);
		virtual void onColours(const EditorColours* defCols, const EditorColours* colours);
		virtual void onError(XMLParserException& ex);
};

#endif //#ifndef schemecompiler_h__included