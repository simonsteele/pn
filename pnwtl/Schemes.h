/**
 * @file Schemes.h
 * @brief Define Scheme and SchemeManager.
 * @author Simon Steele
 * @note Copyright (c) 2002-2006 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
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

/**********************************************
 * Stuff for compiled scheme files
 **********************************************/

// File Content Defines
#define CompileVersion 0x06
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
typedef enum {ttFontName, ttKeywords, ttLexerLanguage} eTextType;

/// Used to store what the next thing to expect is
typedef enum {nrMsgRec, nrTextRec, nrPropRec, nrCommentRec} eNextRec;

/// Flags for folding
typedef enum {fldEnabled = 0x01, fldCompact = 0x02, fldComments = 0x04, fldPreProc = 0x08} eFoldFlags;

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
		Scheme();
		Scheme(SchemeManager* pManager);
		Scheme(SchemeManager* pManager, LPCTSTR filename);
		
		Scheme(const Scheme& copy);

		~Scheme();

		virtual void Load(CScintilla& sc, bool allSettings = true, LPCTSTR filename = NULL);
		
		virtual StylesList* CreateStylesList();

		virtual void SetName(LPCTSTR name);
		virtual void SetTitle(LPCTSTR title);
		
		void SetFileName(LPCTSTR filename);

		virtual bool CheckName();

		virtual LPCTSTR GetName() const;
		virtual LPCTSTR GetTitle() const;
		virtual LPCTSTR GetFileName() const;
		virtual LPCTSTR GetLexer() const;

		const CommentSpecRec& GetCommentSpec() const;

		bool IsInternal() const;

		void SetSchemeManager(SchemeManager* pManager);

		bool operator < (const Scheme& compare) const;
		bool operator > (const Scheme& compare) const;
		const Scheme& operator = (const Scheme& copy);

	protected:
		TCHAR*			m_SchemeFile;
		TCHAR*			m_Name;
		TCHAR*			m_Title;
		bool			m_bInternal;
		SchemeManager*	m_pManager;
		CommentSpecRec	m_CommentSpec;
		tstring			m_Lexer;

		bool InitialLoad(CFile& file, SchemeHdrRec& hdr);

		void SetupScintilla(CScintilla& sc, bool allSettings = true);
		void Init();
};

/**
 * DefaultScheme is a special case because it must always
 * be available - even if no other schemes are. Therefore,
 * while it *may* eventually be able to load settings from
 * a file, it will always exist and provide default settings.
 */
class DefaultScheme : public Scheme
{
	public:
		DefaultScheme(){}

		virtual void Load(CScintilla& sc, LPCTSTR filename = NULL);

		// Can't set name, it's always "Default"
		virtual void SetName(LPCTSTR name){}

		virtual void CheckName(LPCTSTR filename = NULL){}

		virtual LPCTSTR GetName() const {return _T("Default");}

		virtual LPCTSTR GetTitle() const {return _T("Plain Text");}
};

typedef std::list<Scheme>				SCHEME_LIST;
typedef SCHEME_LIST::iterator			SCIT;	 
typedef std::map<tstring, Scheme*>		SCHEME_MAP;
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

class SchemeManager : public Singleton<SchemeManager, true>
{
	public:
		SchemeManager() : m_SchemePath(NULL), m_CompiledPath(NULL){}
		SchemeManager(LPCTSTR schemepath, LPCTSTR compiledpath=NULL);
		~SchemeManager();
		
		void SetPath(LPCTSTR schemepath);
		void SetCompiledPath(LPCTSTR compiledpath);

		LPCTSTR GetPath(){return m_SchemePath;}
		void GetPath(tstring& csPath){if(m_SchemePath) csPath = m_SchemePath;}
		LPCTSTR GetCompiledPath(){return m_CompiledPath;}
		void GetCompiledPath(tstring& csPath){if(m_CompiledPath) csPath = m_CompiledPath;}

		void Load();
		void Compile();
		void LoadExtMap(SCHEME_MAP& extMap, SCHEME_MAP& fnMap, bool noUserMap = false);
		
		Scheme* SchemeForFile(LPCTSTR filename);
		Scheme* SchemeForExt(LPCTSTR ext);
		Scheme* SchemeByName(LPCTSTR name);
		Scheme* GetDefaultScheme() {return &m_DefaultScheme;}

		SCHEME_LIST* GetSchemesList() {return &m_Schemes;}

		SCHEME_MAP* GetExtensionMap() { return &m_SchemeExtMap; }
		SCHEME_MAP* GetFilenameMap() { return &m_SchemeFileNameMap; }

		void BuildMenu(HMENU menu, CommandDispatch* pDispatch, CommandEventHandler* pHandler, int iCommand = SCHEMEMANAGER_SELECTSCHEME, bool bNewMenu = true);

		void SaveExtMap();

	private:
		Scheme* internalSchemeForFileName(const tstring& filename);
		Scheme* internalSchemeForExt(const tstring& extension);
		void internalLoadExtMap(LPCTSTR filename, SCHEME_MAP& extMap, SCHEME_MAP& fnMap);
		void internalLoad(bool forceCompile);

	private:
		TCHAR*			m_SchemePath;
		TCHAR*			m_CompiledPath;
		SCHEME_LIST		m_Schemes;

		SCHEME_MAP		m_SchemeNameMap;
		SCHEME_MAP		m_SchemeExtMap;
		SCHEME_MAP		m_SchemeFileNameMap;

		DefaultScheme	m_DefaultScheme;
};

#endif //#ifndef schemes_h__included