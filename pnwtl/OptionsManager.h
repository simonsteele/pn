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
static const TCHAR* pnmrukey = _T("MRU");
static const TCHAR* PNSK_INTERFACE = _T("Interface Settings");
static const TCHAR* PNSK_EDITOR = _T("Editor Settings");
static const TCHAR* PNSK_PRINT = _T("Print Settings");

#define PNPATH_PN				0
#define	PNPATH_SCHEMES			1
#define	PNPATH_USERSETTINGS		2

/**
 * This class represents all of the major
 * options that each editor may wish to check at any
 * given time. To avoid an enormous virtual function
 * table, this class basically contains public
 * members for frequently used options, and the rest
 * are accessed through functions.
 */
class COptionsManager
{
	// Class Functionality
	public:
		COptionsManager();
		~COptionsManager();

		void Load();
		void Save();

		static COptionsManager*	GetInstance();
		static COptionsManager&	GetInstanceRef();
		static void DeleteInstance();

	// Class Members
	public:
		bool UseTabs;
		int TabWidth;
		bool ShowIndentGuides;
		bool LineNumbers;
		bool MaximiseNew;
		EPNSaveFormat LineEndings;
		EAlreadyOpenAction AlreadyOpenAction;

		void Set(LPCTSTR subkey, LPCTSTR value, bool bVal);
		void Set(LPCTSTR subkey, LPCTSTR value, int iVal);
		void Set(LPCTSTR subkey, LPCTSTR value, LPCTSTR szVal);

		bool Get(LPCTSTR subkey, LPCTSTR value, bool bDefault);
		int Get(LPCTSTR subkey, LPCTSTR value, int iDefault);
		tstring Get(LPCTSTR subkey, LPCTSTR value, LPCTSTR szDefault);

		void SavePrintSettings(SPrintOptions* pSettings);
		void LoadPrintSettings(SPrintOptions* pSettings);
		
		void GetSchemesPaths(ctcString& path, ctcString& compiledPath);
		void GetPNPath(tstring& path, int pathtype = PNPATH_PN);
		
		SFindOptions*		GetFindOptions(){return &m_FindOptions;}
		SReplaceOptions*	GetReplaceOptions(){return &m_ReplaceOptions;}

	protected:
		static COptionsManager* s_pInstance;
		SFindOptions			m_FindOptions;
		SReplaceOptions			m_ReplaceOptions;
};

#endif