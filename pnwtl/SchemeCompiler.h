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

using namespace std;

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
			ForeColor = -1;
			BackColor = -1;
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
};

typedef map<CString, StyleDetails*> STYLEDETAILS_NAMEMAP;
typedef STYLEDETAILS_NAMEMAP::iterator SDNM_IT;

class CSchemeLoaderState
{
	public:
		CSTRING_MAP				m_Globals;
		CSTRING_MAP				m_Keywords;
		STYLEDETAILS_NAMEMAP	m_StyleClasses;
		StyleDetails			m_Default;

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
class CSchemeCompilerException : public XMLParserException
{
	public:
		CSchemeCompilerException(XMLParser* pParser, LPCTSTR msg = NULL)
			: XMLParserException(pParser, msg) {}
		
		CSchemeCompilerException(XMLParser* pParser, int ErrorCode = 0, LPCTSTR msg = NULL)
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

/**
 * XML Scheme Compiler
 * Uses James Clark's XML parser expat.
 */
class SchemeCompiler
{
	public:
		void Compile(LPCTSTR path, LPCTSTR outpath, LPCTSTR mainfile);

	protected:
		CSchemeLoaderState	m_LoadState;
		SchemeRecorder		m_Recorder;

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
		void processStyleClass(CSchemeLoaderState* pState, XMLAttributes& atts);
		void sendStyle(StyleDetails* s, SchemeRecorder* compiler);
		void parseStyle(CSchemeLoaderState* pState, XMLAttributes& atts, StyleDetails* pStyle);
		void processKeywordClass(CSchemeLoaderState* pState, XMLAttributes& atts);
		void processGlobal(CSchemeLoaderState* pState, XMLAttributes& atts);
};

#endif //#ifndef schemecompiler_h__included