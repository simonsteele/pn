/**
 * @file Schemes.h
 * @brief Define CScheme and CSchemeManager.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef schemes_h__included
#define schemes_h__included

#include "Scintillaif.h"
#include "Files.h"
#include "SchemeCompiler.h"
#include "ssmenus.h"
#include <list>
#include <map>

#define SCHEMEMANAGER_SELECTSCHEME	0x01

using namespace std;

class CSchemeManager;

///@todo Add a m_CompiledFile member to save repeatedly changing the file extension and path.
class CScheme
{
	public:
		CScheme();
		CScheme(CSchemeManager* pManager);
		CScheme(CSchemeManager* pManager, LPCTSTR filename);

		CScheme(const CScheme& copy){Init(); *this = copy;}

		~CScheme();

		virtual void Load(CScintilla& sc, LPCTSTR filename = NULL);
		
		virtual StylesList* CreateStylesList();

		virtual void SetName(LPCTSTR name);
		virtual void SetTitle(LPCTSTR title);
		
		void SetFileName(LPCTSTR filename);

		virtual void CheckName();

		virtual LPCTSTR GetName() const
		{
			return m_Name;
		}

		virtual LPCTSTR GetTitle() const
		{
			return m_Title;
		}

		virtual LPCTSTR GetFileName() const
		{
			return m_SchemeFile;
		}

		void SetSchemeManager(CSchemeManager* pManager);

		bool operator < (const CScheme& compare) const;
		bool operator > (const CScheme& compare) const;
		const CScheme& operator = (const CScheme& copy);

	protected:
		TCHAR* m_SchemeFile;
		TCHAR* m_Name;
		TCHAR* m_Title;
		CSchemeManager* m_pManager;

		bool OpenCompiledFile(CFile& file, LPCTSTR filename = NULL);

		void SetupScintilla(CScintilla& sc);
		void Init();
};

/**
 * CDefaultScheme is a special case because it must always
 * be available - even if no other schemes are. Therefore,
 * while it *may* eventually be able to load settings from
 * a file, it will always exist and provide default settings.
 */
class CDefaultScheme : public CScheme
{
	public:
		CDefaultScheme(){}

		virtual void Load(CScintilla& sc, LPCTSTR filename = NULL);

		// Can't set name, it's always "Default"
		virtual void SetName(LPCTSTR name){}

		virtual void CheckName(LPCTSTR filename = NULL){}

		virtual LPCTSTR GetName() const {return _T("Default");}

		virtual LPCTSTR GetTitle() const {return _T("Plain Text");}
};

typedef std::list<CScheme>				SCHEME_LIST;
typedef SCHEME_LIST::iterator			SCIT;	 
typedef std::map<tstring, CScheme*>		SCHEME_MAP;
typedef SCHEME_MAP::iterator			SCHEME_MAPIT;
typedef SCHEME_MAP::value_type			SCMITEM;

typedef struct 
{
	CScheme* pScheme;
	int iCommand;
} menuid_scheme_pair;

typedef list<menuid_scheme_pair> MISCHEMELIST;

class CSchemeSwitcher
{
	public:
		CSchemeSwitcher();
		~CSchemeSwitcher();

		void Reset(int iCommand = SCHEMEMANAGER_SELECTSCHEME);

		void SetActiveScheme(CScheme* pCurrent);

		operator HMENU ();

	protected:
		void BuildMenu(int iCommand);

		MISCHEMELIST	m_list;
		CSPopupMenu		m_menu;
};

class CSchemeManager
{
	public:
		CSchemeManager() : m_SchemePath(NULL), m_CompiledPath(NULL){}
		CSchemeManager(LPCTSTR schemepath, LPCTSTR compiledpath=NULL);
		~CSchemeManager();
		
		void SetPath(LPCTSTR schemepath);
		void SetCompiledPath(LPCTSTR compiledpath);

		LPCTSTR GetPath(){return m_SchemePath;}
		void GetPath(tstring& csPath){if(m_SchemePath) csPath = m_SchemePath;}
		LPCTSTR GetCompiledPath(){return m_CompiledPath;}
		void GetCompiledPath(tstring& csPath){if(m_CompiledPath) csPath = m_CompiledPath;}

		void Load();
		void Compile();
		void LoadExtMap();
		
		CScheme* SchemeForFile(LPCTSTR filename);
		CScheme* SchemeForExt(LPCTSTR ext);
		CScheme* SchemeByName(LPCTSTR name);
		CScheme* GetDefaultScheme(){return &m_DefaultScheme;}

		SCHEME_LIST* GetSchemesList(){return &m_Schemes;}

		void BuildMenu(HMENU menu, CSMenuEventHandler* pHandler, int iCommand = SCHEMEMANAGER_SELECTSCHEME, bool bNewMenu = true);

		static CSchemeManager * GetInstance();
		static CSchemeManager & GetInstanceRef();
		static void DeleteInstance();

	protected:
		TCHAR*			m_SchemePath;
		TCHAR*			m_CompiledPath;
		SCHEME_LIST		m_Schemes;

		SCHEME_MAP		m_SchemeNameMap;
		SCHEME_MAP		m_SchemeExtMap;
		SCHEME_MAP		m_SchemeFileNameMap;

		CDefaultScheme	m_DefaultScheme;

		static CSchemeManager* s_pInstance;

		CScheme* InternalSchemeForFileName(const tstring& filename);
		CScheme* InternalSchemeForExt(const tstring& extension);
};

#endif //#ifndef schemes_h__included