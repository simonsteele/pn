/**
 * @file optionsmanager.h
 * @brief Configuration functionality.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef optionsmanager_h__included
#define optionsmanager_h__included

static const TCHAR* pnregroot = _T("Software\\Echo Software\\PN2\\");
static const TCHAR* PNSK_MRU = _T("MRU");
static const TCHAR* PNSK_MRUP = _T("MRUProjects");
static const TCHAR* PNSK_INTERFACE = _T("Interface Settings");
static const TCHAR* PNSK_EDITOR = _T("Editor Settings");
static const TCHAR* PNSK_PRINT = _T("Print Settings");
static const TCHAR* PNSK_DEFGUI = _T("\\default");
static const TCHAR* PNSK_FIND = _T("Find");

#define PNPATH_PN				0
#define	PNPATH_SCHEMES			1
#define PNPATH_CLIPS			3
#define PNPATH_TOOLS			5
#define PNPATH_TAGGERS			6
#define PNPATH_PROJECTTEMPLATES	7

#define PNPATH_USERMIN			50

#define	PNPATH_USERSETTINGS		50
#define PNPATH_USERTOOLS		50 // PNPATH_USERSETTINGS
#define PNPATH_COMPILEDSCHEMES	50 // PNPATH_USERSETTINGS

#define PNPATH_USERCLIPS		51

class OptionsFactory;

/**
 * This class is the gateway for all option saving and
 * loading. The class provides a cache for a number of
 * options that are frequently accessed. This is actually
 * the virtual base class for a number of implementation
 * classes which use different storage methods.
 */
class Options
{
	friend class OptionsFactory;

	public:
		virtual ~Options();

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
			OPTION_COUNT				= 21
		} ECachedOption;

		void LoadCache();

		virtual void Set(LPCTSTR subkey, LPCTSTR value, bool bVal) = 0;
		virtual void Set(LPCTSTR subkey, LPCTSTR value, int iVal) = 0;
		virtual void Set(LPCTSTR subkey, LPCTSTR value, LPCTSTR szVal) = 0;

		virtual bool Get(LPCTSTR subkey, LPCTSTR value, bool bDefault) = 0;
		virtual int Get(LPCTSTR subkey, LPCTSTR value, int iDefault) = 0;
		virtual tstring Get(LPCTSTR subkey, LPCTSTR value, LPCTSTR szDefault) = 0;

		virtual void SavePrintSettings(SPrintOptions* pSettings);
		virtual void LoadPrintSettings(SPrintOptions* pSettings);

		int GetCached(ECachedOption option);
		void SetCached(ECachedOption option, int value);

		void GetPNPath(tstring& path, int pathtype = PNPATH_PN);

		virtual void SetUserSettingsPath(LPCTSTR path);
		
		//SFindOptions*		GetFindOptions()		{return &m_FindOptions;}
		//SReplaceOptions*	GetReplaceOptions()		{return &m_ReplaceOptions;}
		SearchOptions*		GetSearchOptions()		{return &m_SearchOptions;}

		void BeginGroupOperation(LPCTSTR subkey);
		void EndGroupOperation();

	protected:
		Options();

		void copy(Options* other);

		virtual void group(LPCTSTR location) = 0;
		virtual void ungroup() = 0;

		//SFindOptions			m_FindOptions;
		//SReplaceOptions			m_ReplaceOptions;
		SearchOptions			m_SearchOptions;
		tstring					m_UserSettingsPath;

		void loadCache();
		void saveCache();
		int cache[OPTION_COUNT];
};

class OptionsFactory
{
public:
	typedef enum {OTRegistry, OTIni} EOptionsType;

	/**
	 * This function will create a new options object of a given type.
	 * If you pass in an old options object, its cached values will be 
	 * copied into the new one and it will then be freed.
	 */
	static Options* GetOptions(EOptionsType type, Options* oldOptions = NULL);

	/**
	 * This function frees an Options instance.
	 */
	static void Release(Options* options);

protected:
	OptionsFactory(){}
};

#endif