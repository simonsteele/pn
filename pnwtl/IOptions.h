/**
 * @file IOptions.h
 * @brief PN Options Manager Interface
 * @author Simon Steele
 * @note Copyright (c) 2002-2007 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef ioptions_h__included
#define ioptions_h__included

static const TCHAR* PNSK_MRU = _T("MRU");
static const TCHAR* PNSK_MRUP = _T("MRUProjects");
static const TCHAR* PNSK_INTERFACE = _T("Interface Settings");
static const TCHAR* PNSK_EDITOR = _T("Editor Settings");
static const TCHAR* PNSK_PRINT = _T("Print Settings");
static const TCHAR* PNSK_DEFGUI = _T("\\default");
static const TCHAR* PNSK_FIND = _T("Find");
static const TCHAR* PNSK_SCHEMES = _T("Schemes");
static const TCHAR* PNSK_GENERAL = _T("General Settings");

#define PNPATH_PN				0
#define	PNPATH_SCHEMES			1
#define PNPATH_CLIPS			3
#define PNPATH_TOOLS			5
#define PNPATH_TAGGERS			6
#define PNPATH_PROJECTTEMPLATES	7
#define PNPATH_PRESETS			8

#define PNPATH_USERMIN			50

#define	PNPATH_USERSETTINGS		50
#define PNPATH_USERTOOLS		50 // PNPATH_USERSETTINGS
#define PNPATH_COMPILEDSCHEMES	50 // PNPATH_USERSETTINGS

#define PNPATH_USERCLIPS		51

namespace extensions {

class ISearchOptions;

/**
 * @brief Options Manager Interface
 * 
 * Base class (pure virtual) for all options implementations, used to spread
 * the love across dll boundaries.
 */
class IOptions
{
public:
	typedef enum
		{
			OUseTabs					= 0,
			OTabWidth					= 1,
			OShowIndentGuides			= 2,
			OLineNumbers				= 3,
			OMaximiseNew				= 4,
			OShowFullPath				= 5,
			OLineHighlight				= 6,
			OWordWrap					= 7,
			ORightGuide					= 8,
			ORightColumn				= 9,
			OLineHighlightColour		= 10,
			ORightGuideColour			= 11,
			OLineEndings				= 12,
			OAlreadyOpenDropAction		= 13,
			OAlreadyOpenAction			= 14,
			ODefaultCodePage			= 15,
			ODefaultScintillaCache		= 16,
			OFindAlphaEnabled			= 17,
			OFindAlphaPercent			= 18,
			OVisibleLineEndings			= 19,
			OVisibleWhiteSpace			= 20,
			OManageTabOrder				= 21,
			OConvertLinesOnPaste		= 22,
			ODefaultCharSet				= 23,
			OAutoComplete				= 24,
			OAutoCompleteUseKeywords	= 25,
			OAutoCompleteUseTags		= 26,
			OAutoCompleteStartChars		= 27,
			OAutoCompleteActivation		= 28,
			OAutoCompleteTags			= 29,
			OLineHighlightAlpha			= 30,
			OFoldingEnabled				= 31,
			OCaretXMove					= 32,
			OCaretYMove					= 33,
			OCaretXFlags				= 34,
			OCaretYFlags				= 35,
			OPTION_COUNT				= 36
		} ECachedOption;

		/// Set a bool value
		virtual void Set(LPCTSTR subkey, LPCTSTR value, bool bVal) = 0;
		
		/// Set an int value
		virtual void Set(LPCTSTR subkey, LPCTSTR value, int iVal) = 0;
		
		/// Set a string value
		virtual void Set(LPCTSTR subkey, LPCTSTR value, LPCTSTR szVal) = 0;

		/// Get a bool value
		virtual bool Get(LPCTSTR subkey, LPCTSTR value, bool bDefault) = 0;
		
		/// Get an int value
		virtual int Get(LPCTSTR subkey, LPCTSTR value, int iDefault) = 0;
		
		/// Get a string (note, you should free this using IPN::ReleaseString from an extension)
		virtual const char* GetS(LPCTSTR subkey, LPCTSTR value, LPCTSTR szDefault) = 0;

		/// Get a PN path (note, you should free this using IPN::ReleaseString from an extension)
		virtual const char* GetPNPath(int pathtype = PNPATH_PN) = 0;

		/// Get a cached option
		virtual int GetCached(ECachedOption option) = 0;
		
		/// Set a cached option
		virtual void SetCached(ECachedOption option, int value) = 0;

		/// Start a group options operation
		virtual void BeginGroupOperation(LPCTSTR subkey) = 0;
		
		/// End a group options operation
		virtual void EndGroupOperation() = 0;

		/// Get the global search options cache
		virtual ISearchOptions*	GetSearchOptions() = 0;
};

} //namespace extensions

#endif //#ifndef ioptions_h__included