/**
 * @file SchemeCompiler.h
 * @brief Interface of the CSchemeCompiler class, and definitions for scheme files.
 * @author Simon Steele
 * @note copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 * 
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef __scheme_compiler_h__
#define __scheme_compiler_h__

#include "scintillaif.h"
#include "scintilla.h"
#include <string>
#include "inifile.h"

/**********************************************
 * Structs for compiled scheme file reading...
 **********************************************/

typedef struct
{
	char Magic[16];
	int	Version;
} CompiledHdrRec;

#define SC_HDR_NAMESIZE 40

typedef struct
{
	char Name[SC_HDR_NAMESIZE];
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
typedef enum {fldEnabled = 1, fldCompact = 2, fldComments = 4} eFoldFlags;
typedef enum {ovrTabWidth = 1, ovrIndentGuides = 2} eOverrideFlags;

#define CompileVersion 0x02
#define FileID "Caffeine.Scheme"

/**********************************************
 * Scheme Classes
 **********************************************/

#define DefaultStyleBits 5
#define sGroup _T("Group")
#define sBase _T("Base")
#define sNewBase _T("NewBase")
#define sStyles _T("Styles.")
#define sKeywords _T("Keywords.")
#define scLexer _T("PNLexer")
#define scBraces _T("Braces")
#define sDefStyle _T("Styles.Default")
//#define BraceHighlight 34
//#define BraceHighlightIncomplete 35
#define _allowTWOverride true
#define _allowIDOverride true

///@todo move PNStringToColor into another place...
// export this function, other things may find it useful.
COLORREF PNStringToColor(const char* input);

// pre-define...
class CSchemeCompiler;

class CStyleDetails
{
	public:
		CStyleDetails();
		CStyleDetails(const CStyleDetails& copy){*this = copy;}
		CStyleDetails& operator = (const CStyleDetails& copy);
		std::string FontName;
		int FontSize;
		COLORREF ForeColor;
		std::string FCString;
		COLORREF BackColor;
		std::string BCString;
		bool Bold;
		bool Italic;
		bool Underline;
		bool EOLFilled;

		void Send(CSchemeCompiler* compiler, int Style);
};

class CSchemeCompiler : public CRecordingScintilla
{

	friend class CStyleDetails;
protected:
	FILE*				m_out;
	CIniFile*			m_ini;
	eNextRec			m_next;
	eTextType			m_tType;
	int					m_SchemeOffset;
	CStyleDetails		m_DefStyle;
	CStyleDetails		m_NextStyle;

	bool				m_BracesSloppy;
	int					m_BracesStyle;

	void NormaliseFileTimes(LPCTSTR setfrom, LPCTSTR set);

	virtual void Record(long Msg, WPARAM wParam, LPARAM lParam);

	void ParseStyle(LPCTSTR scS, int Style, bool CanSend = true);
	void ParseDefaultStyle();
	std::string ParseKeyWords(LPCTSTR Section, int KId);

	bool CheckNecessary(long Msg, WPARAM wParam, LPARAM lParam);
public:
	bool Compile(LPCTSTR filename, LPCTSTR outfile);

	bool BeginParse();
};

#endif