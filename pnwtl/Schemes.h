/**
 * @file Schemes.h
 * @brief Define Scheme and SchemeManager.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef schemes_h__included
#define schemes_h__included

class CommandDispatch;

#ifdef _MSC_VER
	#pragma once
#endif

#include "Scintillaif.h"
#include "styles.h"

static const int INDIC_MARKALL = INDIC_CONTAINER;
static const int INDIC_SMARTHIGHLIGHT = INDIC_CONTAINER + 1;
static const int INDIC_OVERWRITETARGET = INDIC_CONTAINER + 2;
static const int INDIC_TEXTCLIPFIELD = INDIC_CONTAINER + 3;

static const int DEFAULT_SMARTHIGHLIGHT_COLOUR = RGB(0, 255, 0);
static const int DEFAULT_TEXTCLIPFIELD_COLOUR = RGB(0, 0, 255);
static const int DEFAULT_MARKALL_COLOUR = RGB(255, 0, 0);
static const int DEFAULT_OVERWRITE_COLOUR = RGB(0, 0xF0, 0x80);

static const int DEFAULT_INDIC_ALPHA_LEVEL = 70;

/**********************************************
 * Stuff for compiled scheme files
 **********************************************/

// File Content Defines
#define CompileVersion 0x07
#define FileID "Caffeine.Scheme"

typedef struct tagCompiledSchemeHdr
{
	char Magic[16];
	int	Version;
} CompiledHdrRec;

#define SC_HDR_NAMESIZE 11
#define SC_HDR_TITLESIZE 40
#define SC_HDR_COMMENTTEXTSIZE 11
#define SC_HDR_COMMENTBLOCKTEXTSIZE 81

/**
 * File header for compiled scheme files
 */
typedef struct tagSchemeHdr
{
	char Name[SC_HDR_NAMESIZE];
	char Title[SC_HDR_TITLESIZE];
	UINT Flags;
	short TabWidth;
} SchemeHdrRec;

typedef struct tagCommentSpecRec
{
	char CommentLineText[SC_HDR_COMMENTTEXTSIZE];
	char CommentStreamStart[SC_HDR_COMMENTTEXTSIZE];
	char CommentStreamEnd[SC_HDR_COMMENTTEXTSIZE];
	char CommentBlockStart[SC_HDR_COMMENTBLOCKTEXTSIZE];
	char CommentBlockEnd[SC_HDR_COMMENTBLOCKTEXTSIZE];
	char CommentBlockLine[SC_HDR_COMMENTTEXTSIZE];
} CommentSpecRec;

/**
 * Define some text that will follow, used
 * for storing textual bits of configuration
 */
typedef struct tagSchemeTextRec
{
	unsigned long	TextLength;
	long			MsgNum;
	long			lParam;
	long			wParam;
	char			TextType;
} TextRec;

/**
 * Define a standard message to send to the editor
 */
typedef struct tagSchemeMsg
{
	int MsgNum;
	long lParam;
	long wParam;
} MsgRec;

/**
 * define a scheme property to be forwarded to
 * the lexer. After this will follow the name
 * then the value.
 */
typedef struct tagSchemeProp
{
	unsigned long NameLength;
	unsigned long ValueLength;
} PropRec;

/// Text types used in the TextRec struct
typedef enum {ttFontName, ttKeywords, ttLexerLanguage, ttWordChars} eTextType;

/// Used to store what the next thing to expect is
typedef enum {nrMsgRec, nrTextRec, nrPropRec, nrCommentRec} eNextRec;

/// Flags for folding
typedef enum {fldEnabled = 0x01, fldCompact = 0x02, fldComments = 0x04, fldPreProc = 0x08, fldElse = 0x10} eFoldFlags;

/// Flags for general other settings
typedef enum {schUseTabs = 0x10, schInternal = 0x20, schOverrideTabs = 0x40, schOverrideTabSize = 0x80} eSchemeFlags;

#define USETABFOLDFLAGSMASK (schOverrideTabs | schUseTabs)

#define SCHEMEMANAGER_SELECTSCHEME	0x01

class SchemeManager;
class CFile;

///@todo Add a m_CompiledFile member to save repeatedly changing the file extension and path.
class Scheme
{
	public:
		explicit Scheme();
		explicit Scheme(SchemeManager* pManager);
		explicit Scheme(SchemeManager* pManager, const wchar_t* filename);
		
		explicit Scheme(const Scheme& copy);

		virtual ~Scheme();

		virtual void Load(CScintilla& sc, bool allSettings = true, const wchar_t* filename = NULL);
		
		virtual StylesList* CreateStylesList();

		virtual void SetName(const char* name);
		virtual void SetTitle(const TCHAR* title);
		
		void SetFileName(const wchar_t* filename);

		virtual bool CheckName();

		virtual const char* GetName() const;
		virtual const TCHAR* GetTitle() const;
		virtual const wchar_t* GetFileName() const;
		virtual const char* GetLexer() const;

		const CommentSpecRec& GetCommentSpec() const;

		bool IsInternal() const;

		void SetSchemeManager(SchemeManager* pManager);

		bool operator < (const Scheme& compare) const;
		bool operator > (const Scheme& compare) const;
		const Scheme& operator = (const Scheme& copy);

	protected:
		wchar_t*		m_SchemeFile;
		char*			m_Name;
		TCHAR*			m_Title;
		bool			m_bInternal;
		SchemeManager*	m_pManager;
		CommentSpecRec	m_CommentSpec;
		std::string		m_Lexer;

		bool InitialLoad(CFile& file, SchemeHdrRec& hdr);

		void SetupScintilla(CScintilla& sc, bool allSettings = true);
		void Init();
};

/**
 * DefaultScheme is a special case because it must always
 * be available - even if no other schemes are. Therefore,
 * it will always exist and provide default settings.
 */
class DefaultScheme : public Scheme
{
	public:
		DefaultScheme();		
		virtual ~DefaultScheme(){}

		virtual void Load(CScintilla& sc, LPCTSTR filename = NULL);

		// Can't set name, it's always whatever's set in the constructor from the resource.
		virtual void SetName(const char* name){}

		virtual void CheckName(const wchar_t* filename = NULL){}

		virtual const char* GetName() const { return "default"; }
};

typedef std::list<Scheme>				SCHEME_LIST;
typedef SCHEME_LIST::iterator			SCIT;	 
typedef std::map<tstring, Scheme*>		SCHEME_MAP;
typedef std::map<std::string, Scheme*>	SCHEME_MAPA;
typedef SCHEME_MAP::iterator			SCHEME_MAPIT;
typedef SCHEME_MAP::value_type			SCMITEM;

typedef struct 
{
	Scheme* pScheme;
	int iCommand;
} menuid_scheme_pair;

typedef std::list<menuid_scheme_pair> MISCHEMELIST;

class CSchemeSwitcher
{
	public:
		CSchemeSwitcher();
		~CSchemeSwitcher();

		void Reset(CommandDispatch* pDispatch, int iCommand = SCHEMEMANAGER_SELECTSCHEME);

		void SetActiveScheme(Scheme* pCurrent);

		operator HMENU ();

	protected:
		void BuildMenu(int iCommand, CommandDispatch* dispatch);

		MISCHEMELIST	m_list;
		CSPopupMenu		m_menu;
};

#endif //#ifndef schemes_h__included