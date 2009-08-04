/**
 * @file optionsmanager.h
 * @brief Configuration functionality.
 * @author Simon Steele
 * @note Copyright (c) 2002-2005 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef optionsmanager_h__included
#define optionsmanager_h__included

static const TCHAR* pnregroot = _T("Software\\Echo Software\\PN2\\");
class OptionsFactory;

#include "IOptions.h"

/**
 * This class is the gateway for all option saving and
 * loading. The class provides a cache for a number of
 * options that are frequently accessed. This is actually
 * the virtual base class for a number of implementation
 * classes which use different storage methods.
 */
class Options : public extensions::IOptions
{
	friend class OptionsFactory;

	public:
		virtual ~Options();

		void LoadCache();

		virtual void Set(LPCTSTR subkey, LPCTSTR value, bool bVal) = 0;
		virtual void Set(LPCTSTR subkey, LPCTSTR value, int iVal) = 0;
		virtual void Set(LPCTSTR subkey, LPCTSTR value, uint64_t iVal) = 0;
		virtual void Set(LPCTSTR subkey, LPCTSTR value, LPCTSTR szVal) = 0;

		virtual bool Get(LPCTSTR subkey, LPCTSTR value, bool bDefault) = 0;
		virtual int Get(LPCTSTR subkey, LPCTSTR value, int iDefault) = 0;
		virtual uint64_t Get(LPCTSTR subkey, LPCTSTR value, uint64_t iDefault) = 0;
		virtual tstring Get(LPCTSTR subkey, LPCTSTR value, LPCTSTR szDefault) = 0;

		virtual void Clear(LPCTSTR subkey) = 0;

		virtual void SavePrintSettings(SPrintOptions* pSettings);
		virtual void LoadPrintSettings(SPrintOptions* pSettings);

		virtual int GetCached(ECachedOption option);
		virtual void SetCached(ECachedOption option, int value);

		void GetPNPath(tstring& path, int pathtype = PNPATH_PN);

		virtual void SetUserSettingsPath(LPCTSTR path);
		
		virtual extensions::ISearchOptions*	GetSearchOptions();

		virtual void BeginGroupOperation(LPCTSTR subkey);
		virtual void EndGroupOperation();

		// Used for a very special case of getting PN's Path at startup before
		// there is an options manager.
		static void StaticGetPNPath(tstring& path);

		// IOptions Specials::
		virtual wchar_t* GetPNPath(int pathtype = PNPATH_PN);
		virtual wchar_t* GetS(const wchar_t* subkey, const wchar_t* value, const wchar_t* szDefault);

	protected:
		Options();

		void copy(Options* other);

		virtual void group(LPCTSTR location) = 0;
		virtual void ungroup() = 0;

		SearchOptions			m_SearchOptions;
		tstring					m_UserSettingsPath;

		void loadCache();
		void saveCache();
		int cache[OPTION_COUNT];
};

class OptionsFactory
{
public:
	typedef enum {OTRegistry, OTIni, OTXml, OTSqlite} EOptionsType;

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