/**
 * @file SchemeCompiler.h
 * @brief Define scheme reader and compiler classes.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
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

#include "scintillaif.h"
#include <string>
#include <list>
#include <map>

using std::list;
using std::map;

typedef map<CString, CString> CSTRING_MAP;
typedef list<CString> CSTRING_LIST;

/**********************************************
 * Structs for compiled scheme file reading...
 **********************************************/

typedef struct
{
	char Magic[16];
	int	Version;
} CompiledHdrRec;

#define SC_HDR_NAMESIZE 11
#define SC_HDR_TITLESIZE 40

typedef struct
{
	char Name[SC_HDR_NAMESIZE];
	char Title[SC_HDR_TITLESIZE];
	int Folding;
} SchemeHdrRec;

typedef struct
{
	unsigned long	TextLength;
	long			MsgNum;
	long			lParam;
	long			wParam;
	char			TextType;
} TextRec;

typedef struct
{
	int MsgNum;
	long lParam;
	long wParam;
} MsgRec;

typedef enum {ttFontName, ttKeywords, ttLexerLanguage} eTextType;
typedef enum {nrMsgRec, nrTextRec} eNextRec;
typedef enum {fldEnabled = 1, fldCompact = 2, fldComments = 4, fldPreProc = 8} eFoldFlags;
//typedef enum {ovrTabWidth = 1, ovrIndentGuides = 2} eOverrideFlags;

typedef enum {edvFontName = 0x0001,	edvFontSize = 0x0002, edvForeColor = 0x0004, edvBackColor = 0x0008,
				edvBold = 0x0010, edvItalic = 0x0020, edvUnderline = 0x0040, edvEOLFilled = 0x0080, 
				edvClass = 0x0100} EValuesSet;

// Parser State Defines
#define DOING_GLOBALS			1
#define DOING_GLOBAL			2
#define DOING_KEYWORDC			3
#define DOING_KEYWORDS			4
#define DOING_STYLECS			5	//style-classes
#define DOING_STYLEC			6	//style-class
#define DOING_LANGUAGE			7	//language and children...
#define DOING_LANGUAGE_DETAILS	8
#define DOING_LANGUAGE_KW		9
#define DOING_LANGUAGE_STYLES	10
#define	DOING_IMPORTS			11
#define DOING_KEYWORDCOMBINE	12

#define US_SCHEMES				1
#define US_SCHEME				2
#define US_KEYWORD_OVERRIDES	3
#define US_STYLE_OVERRIDES		4
#define US_KEYWORDS				5

// File Content Defines
#define CompileVersion 0x03
#define FileID "Caffeine.Scheme"

class StyleDetails
{
	public:
		StyleDetails()
		{
			Key = 0;
			FontName = "Courier New";
			FontSize = 10;
			ForeColor = RGB(0,0,0);
			BackColor = RGB(255,255,255);
			Bold = false;
			Italic = false;
			Underline = false;
			EOLFilled = false;
		}

		StyleDetails(const StyleDetails& copy)
		{
			*this = copy;
		}

		StyleDetails& operator = (const StyleDetails& copy)
		{
			Key = copy.Key;
			FontName = copy.FontName;
			FontSize = copy.FontSize;
			ForeColor = copy.ForeColor;
			BackColor = copy.BackColor;
			Bold = copy.Bold;
			Italic = copy.Italic;
			Underline = copy.Underline;
			EOLFilled = copy.EOLFilled;
			values = copy.values;
			classname = copy.classname;
			name = copy.name;
			return *this;
		}

		int Key;
		
		std::string FontName;
		int FontSize;
		COLORREF ForeColor;
		COLORREF BackColor;
		bool Bold;
		bool Italic;
		bool Underline;
		bool EOLFilled;

		std::string name;
		std::string classname;
		int values;
};

typedef list<StyleDetails*>	STYLES_LIST;
typedef STYLES_LIST::iterator SL_IT;

struct CustomKeywordSet
{
	int		key;
	TCHAR*	pWords;
	CustomKeywordSet* pNext;
};

class CustomKeywordHolder
{
	public:
		CustomKeywordHolder()
		{
			pKeywordSets = NULL;
			pLast = NULL;
		}

		~CustomKeywordHolder()
		{
			CustomKeywordSet* pSet = pKeywordSets;
			CustomKeywordSet* pDel;
			while(pSet)
			{
				pDel = pSet;
				pSet = pSet->pNext;
				if(pDel->pWords)
					delete [] pDel->pWords;
				delete pDel;
			}

			pKeywordSets = NULL;
		}

		void AddKeywordSet(CustomKeywordSet* pSet)
		{
			if(pLast)
			{
				pLast->pNext = pSet;
				pLast = pSet;
			}
			else
			{
				pKeywordSets = pLast = pSet;
			}
			pLast->pNext = NULL;
		}

		CustomKeywordSet* FindKeywordSet(int key)
		{
			CustomKeywordSet* pSet = pKeywordSets;
			while(pSet)
			{
				if(pSet->key == key)
					break;
				pSet = pSet->pNext;
			}
			return pSet;
		}

	protected:
		CustomKeywordSet* pKeywordSets;
		CustomKeywordSet* pLast;
};

class CustomisedScheme : public CustomKeywordHolder
{
public:
	~CustomisedScheme()
	{
		for(SL_IT i = m_Styles.begin(); i != m_Styles.end(); ++i)
		{
			delete (*i);
		}
		m_Styles.clear();
	}

	StyleDetails* FindStyle(int key)
	{
		for(SL_IT i = m_Styles.begin(); i != m_Styles.end(); ++i)
		{
			if((*i)->Key == key)
				return *i;
		}
		return NULL;
	}

	STYLES_LIST	m_Styles;
};

typedef map<CString, CustomisedScheme*> CUSTOMISED_NAMEMAP;
typedef CUSTOMISED_NAMEMAP::iterator CNM_IT;
typedef map<CString, StyleDetails*> STYLEDETAILS_NAMEMAP;
typedef STYLEDETAILS_NAMEMAP::iterator SDNM_IT;

class CSchemeLoaderState
{
	public:
		~CSchemeLoaderState();
		CSTRING_MAP				m_Globals;
		CSTRING_MAP				m_Keywords;
		STYLEDETAILS_NAMEMAP	m_StyleClasses;
		StyleDetails			m_Default;

		CUSTOMISED_NAMEMAP		m_CustomSchemes;
		CustomisedScheme*		m_pCustom;

		XMLParser*				m_pParser;

		int m_State;
		
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
		
		CSchemeParserException(XMLParser* pParser, int ErrorCode = 0, LPCTSTR msg = NULL)
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
};

/**
 * XML Scheme Parser
 * Uses James Clark's XML parser expat.
 */
class SchemeParser
{
	public:
		void Parse(LPCTSTR path, LPCTSTR mainfile, LPCTSTR userfile);

	protected:
		CSchemeLoaderState	m_LoadState;

	protected:
		void characterData(void* userData, LPCTSTR data, int len);
		void endElement(void *userData, LPCTSTR name);
		void startElement(void *userData, LPCTSTR name, XMLAttributes& atts);
		void processKeywordCombine(CSchemeLoaderState* pState, XMLAttributes& atts);
		void specifyImportSet(CSchemeLoaderState* pState, XMLAttributes& atts);
		void specifyImportFile(CSchemeLoaderState* pState, XMLAttributes& atts);
		void processLanguageElement(CSchemeLoaderState* pState, LPCTSTR name, XMLAttributes& atts);
		void processLanguageKeywords(CSchemeLoaderState* pState, XMLAttributes& atts);
		void processLanguageStyle(CSchemeLoaderState* pState, XMLAttributes& atts);
		void processLanguageStyleGroup(CSchemeLoaderState* pState, XMLAttributes& atts);
		void processStyleClass(CSchemeLoaderState* pState, XMLAttributes& atts);
		void sendStyle(StyleDetails* s, SchemeRecorder* compiler);
		void parseStyle(CSchemeLoaderState* pState, XMLAttributes& atts, StyleDetails* pStyle);
		void processKeywordClass(CSchemeLoaderState* pState, XMLAttributes& atts);
		void processGlobal(CSchemeLoaderState* pState, XMLAttributes& atts);
		void customiseStyle(StyleDetails* style, StyleDetails* custom);

	protected:
		virtual void onLexer(LPCTSTR name, int styleBits) = 0;
		virtual void onLanguage(LPCTSTR name, LPCTSTR title, int foldflags) = 0;
		virtual void onLanguageEnd() = 0;
		virtual void onStyleGroup(XMLAttributes& atts) = 0;
		virtual void onStyle(StyleDetails* pStyle) = 0;
		virtual void onStyleGroupEnd() = 0;
		virtual void onKeywords(int key, LPCTSTR keywords) = 0;
		virtual void onFile(LPCTSTR filename) = 0;
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
		virtual void onLanguage(LPCTSTR name, LPCTSTR title, int foldflags);
		virtual void onLanguageEnd();
		virtual void onStyleGroup(XMLAttributes& atts){}
		virtual void onStyle(StyleDetails* pStyle);
		virtual void onStyleGroupEnd(){}
		virtual void onFile(LPCTSTR filename);
		virtual void onKeywords(int key, LPCTSTR keywords);
		virtual void onLexer(LPCTSTR name, int styleBits);
};

#endif //#ifndef schemecompiler_h__included