/**
 * @file SchemeCompiler.h
 * @brief Define scheme reader and compiler classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef schemecompiler_h__included
#define schemecompiler_h__included

class SchemeLoaderState
{
	public:
		~SchemeLoaderState();
		
		StylePtr				GetClass(LPCTSTR name);

		XMLParser*				m_pParser;

		tstring					m_basePath;
		tstring					m_outputPath;
		std::list<tstring>		m_IncludeFiles;

		// Defaults:
		StyleDetails			m_Default;
		EditorColours			m_DefaultColours;

		// Style Classes:
		StylePtrMap				m_Classes;

		// Styles:
		//StylesList				m_BaseStyles;
		StylePtrList			m_BaseStyles;

		// Schemes:
		SchemeDetailsMap		m_BaseSchemeDetails;
		SchemeDetailsMap		m_SchemeDetails;
		
		// Keyword Definitions:
		STRING_MAP				m_Keywords;

		// State:
		StylePtr				m_pGroupClass;
		BaseScheme*				m_pBase;
		SchemeDetails*			m_pCurScheme;

		int						m_State;
		bool					m_bBaseParse;
		
		DWORD					m_StartLoad;
		DWORD					m_StartLang;
		
		tstring					m_langName;

		// Character Data Caching...
		tstring					m_CDATA;
		tstring					m_storedName;
};

// Empty class for exception source identification purposes...
class SchemeParserException : public XMLParserException
{
	public:
		SchemeParserException(XMLParser* pParser, LPCTSTR msg = NULL)
			: XMLParserException(pParser, msg) {}
		
		SchemeParserException(XMLParser* pParser, XML_Error ErrorCode = XML_ERROR_NONE, LPCTSTR msg = NULL)
			: XMLParserException(pParser, ErrorCode, msg) {}
};

class SchemeRecorder : public CScintilla
{
	public:
		SchemeRecorder();
	
		bool StartRecording(LPCTSTR scheme, LPCTSTR title, LPCTSTR outfile, int FoldFlags);
		void WriteCommentBlock(const char* linecomment, const char* streamcommentstart, const char* streamcommentend, 
			const char* commentblockstart, const char* commentblockend, const char* commentblockline);
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

/**
 * Class to parse UserSettings.xml
 */
class UserSettingsParser
{
	public:
		UserSettingsParser();
		void SetPresetLoadMode();
		void Parse(LPCTSTR path, SchemeLoaderState*	pState);

	private:
		void characterData(void* userData, LPCTSTR data, int len);
		void endElement(void *userData, LPCTSTR name);
		void startElement(void *userData, LPCTSTR name, XMLAttributes& atts);

		void processScheme(SchemeLoaderState* pState, XMLAttributes& atts);
		void processSchemeElement(SchemeLoaderState* pState, LPCTSTR name, XMLAttributes& atts);
		void processClassElement(SchemeLoaderState* pState, LPCTSTR name, XMLAttributes& atts);
		void processGlobalColours(SchemeLoaderState* pState, XMLAttributes& atts);

	private:
		SchemeDetails*		m_pCurScheme;
		tstring				m_SchemeName;
		int					m_idval;
		bool				m_loadingPreset;
};

/**
 * XML Scheme Parser
 * Uses James Clark's XML parser expat.
 */
class SchemeParser
{
	public:
		void Parse(LPCTSTR path, LPCTSTR mainfile, LPCTSTR userfile);

		static void parseStyle(SchemeLoaderState* pState, XMLAttributes& atts, StyleDetails* pStyle);

	protected:
		SchemeLoaderState	m_LoadState;

	protected:
		void characterData(void* userData, LPCTSTR data, int len);
		void endElement(void *userData, LPCTSTR name);
		void startElement(void *userData, LPCTSTR name, XMLAttributes& atts);
		void processKeywordCombine(SchemeLoaderState* pState, XMLAttributes& atts);
		void specifyImportSet(SchemeLoaderState* pState, XMLAttributes& atts);
		void specifyImportFile(SchemeLoaderState* pState, XMLAttributes& atts);
		void processBaseStyle(SchemeLoaderState* pState, XMLAttributes& atts);
		void processLanguageElement(SchemeLoaderState* pState, LPCTSTR name, XMLAttributes& atts);
		void processLanguageKeywords(SchemeLoaderState* pState, XMLAttributes& atts);
		void processLanguageStyle(SchemeLoaderState* pState, XMLAttributes& atts);
		void processLanguageStyleGroup(SchemeLoaderState* pState, XMLAttributes& atts);
		void processStyleClass(SchemeLoaderState* pState, XMLAttributes& atts);
		void sendStyle(StyleDetails* s, SchemeRecorder* compiler);
		void processKeywordClass(SchemeLoaderState* pState, XMLAttributes& atts);
		void processGlobal(SchemeLoaderState* pState, XMLAttributes& atts);
		void processProperty(SchemeLoaderState* pState, XMLAttributes& atts);
		void customiseStyle(StyleDetails* style, StyleDetails* custom);
		void sendBaseScheme(SchemeLoaderState* pState, BaseScheme* pBase, LPCTSTR baseName);
		void sendBaseStyles(SchemeLoaderState* pState);
		void processComments(SchemeLoaderState* pState, XMLAttributes& atts);

	protected:
		virtual void onLexer(LPCTSTR name, int styleBits) = 0;
		virtual void onLanguage(LPCTSTR name, LPCTSTR title, int foldflags, int ncfoldflags) = 0;
		virtual void onCommentSpec(const char* linecomment, const char* streamcommentstart, const char* streamcommentend, 
			const char* commentblockstart, const char* commentblockend, const char* commentblockline) = 0;
		virtual void onLanguageEnd() = 0;
		virtual void onStyleGroup(XMLAttributes& atts, const StylePtr& pClass) = 0;
		virtual void onStyle(const StylePtr& details, bool isBaseStyle) = 0;
		virtual void onStyleGroupEnd() = 0;
		virtual void onStyleClass(const StylePtr& details) = 0;
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

		uint64_t GetNewestFileTime() const;

	protected:
		SchemeRecorder m_Recorder;
		void sendStyle(StyleDetails* s, SchemeRecorder* compiler);
	
	// Implement SchemeParser
	protected:
		virtual void onLanguage(LPCTSTR name, LPCTSTR title, int foldflags, int ncfoldflags);
		virtual void onCommentSpec(const char* linecomment, const char* streamcommentstart, const char* streamcommentend, 
			const char* commentblockstart, const char* commentblockend, const char* commentblockline);
		virtual void onLanguageEnd();
		virtual void onStyleGroup(XMLAttributes& atts, const StylePtr& pClass){}
		virtual void onStyle(const StylePtr& details, bool isBaseStyle);
		virtual void onStyleGroupEnd(){}
		virtual void onStyleClass(const StylePtr& details);
		virtual void onProperty(LPCTSTR name, LPCTSTR value);
		virtual void onFile(LPCTSTR filename);
		virtual void onKeywords(int key, LPCTSTR keywords, LPCTSTR name, LPCTSTR custom);
		virtual void onLexer(LPCTSTR name, int styleBits);
		virtual void onColours(const EditorColours* defCols, const EditorColours* colours);
		virtual void onError(XMLParserException& ex);

	private:
		uint64_t	m_newestFile;
};

SchemeDetails* ensureSchemeDetails(SchemeDetailsMap& map, tstring& name);

#endif //#ifndef schemecompiler_h__included