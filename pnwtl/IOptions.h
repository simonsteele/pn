/**
 * @file IOptions.h
 * @brief PN Options Manager Interface
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef ioptions_h__included
#define ioptions_h__included

static const wchar_t* PNSK_MRU = L"MRU";
static const wchar_t* PNSK_MRUP = L"MRUProjects";
static const wchar_t* PNSK_INTERFACE = L"Interface Settings";
static const wchar_t* PNSK_EDITOR = L"Editor Settings";
static const wchar_t* PNSK_PRINT = L"Print Settings";
static const wchar_t* PNSK_DEFGUI = L"\\default";
static const wchar_t* PNSK_FIND = L"Find";
static const wchar_t* PNSK_SCHEMES = L"Schemes";
static const wchar_t* PNSK_GENERAL = L"General Settings";

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
			ODefaultCodePage			= 15, // Deprecated
			ODefaultScintillaCache		= 16,
			OFindAlphaEnabled			= 17,
			OFindAlphaPercent			= 18,
			OVisibleLineEndings			= 19,
			OVisibleWhiteSpace			= 20,
			OManageTabOrder				= 21,
			OConvertLinesOnPaste		= 22,
			ODefaultCharSet				= 23, // Deprecated
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
			OSmartHighlight				= 36,
			OLinePaddingTop				= 37,
			OLinePaddingBottom			= 38,
			ODefaultEncoding			= 39,
			OMultiByteCodePage			= 40,
			OPTION_COUNT				= 41
		} ECachedOption;

		/// Set a bool value
		virtual void Set(const wchar_t* subkey, const wchar_t* value, bool bVal) = 0;
		
		/// Set an int value
		virtual void Set(const wchar_t* subkey, const wchar_t* value, int iVal) = 0;
		
		/// Set a string value
		virtual void Set(const wchar_t* subkey, const wchar_t* value, const wchar_t* szVal) = 0;

		/// Get a bool value
		virtual bool Get(const wchar_t* subkey, const wchar_t* value, bool bDefault) = 0;
		
		/// Get an int value
		virtual int Get(const wchar_t* subkey, const wchar_t* value, int iDefault) = 0;
		
		/// Get a string (note, you should free this using IPN::ReleaseString from an extension)
		virtual wchar_t* GetS(const wchar_t* subkey, const wchar_t* value, const wchar_t* szDefault) = 0;

		/// Get a PN path (note, you should free this using IPN::ReleaseString from an extension)
		virtual wchar_t* GetPNPath(int pathtype = PNPATH_PN) = 0;

		/// Get a cached option
		virtual int GetCached(ECachedOption option) = 0;
		
		/// Set a cached option
		virtual void SetCached(ECachedOption option, int value) = 0;

		/// Start a group options operation
		virtual void BeginGroupOperation(const wchar_t* subkey) = 0;
		
		/// End a group options operation
		virtual void EndGroupOperation() = 0;

		/// Get the global search options cache
		virtual ISearchOptions*	GetSearchOptions() = 0;
};

} //namespace extensions

#endif //#ifndef ioptions_h__included