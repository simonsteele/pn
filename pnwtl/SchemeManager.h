/**
 * @file SchemeManager.cpp
 * @brief Implement SchemeManager.
 * @author Simon Steele
 * @note Copyright (c) 2002-2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef schememanager_h__included
#define schememanager_h__included

class Scheme;

/**
 * @class SchemeManager
 * @brief Manage instances of Scheme for the lifetime of PN
 */
class SchemeManager : public Singleton<SchemeManager, true>
{
	public:
		SchemeManager() : m_SchemePath(NULL), m_CompiledPath(NULL){}
		SchemeManager(LPCTSTR schemepath, LPCTSTR compiledpath=NULL);
		~SchemeManager();
		
		void SetPath(LPCTSTR schemepath);
		void SetCompiledPath(LPCTSTR compiledpath);

		LPCTSTR GetPath() const;
		void GetPath(tstring& csPath) const;
		LPCTSTR GetCompiledPath() const;
		void GetCompiledPath(tstring& csPath) const;

		void Load();
		void Compile();
		void LoadExtMap(SCHEME_MAP& extMap, SCHEME_MAP& fnMap, bool noUserMap = false);
		
		Scheme* SchemeForFile(LPCTSTR filename);
		Scheme* SchemeForExt(LPCTSTR ext);
		Scheme* SchemeByName(LPCTSTR name);
		
		Scheme* GetDefaultScheme();

		SCHEME_LIST* GetSchemesList();

		SCHEME_MAP* GetExtensionMap();
		SCHEME_MAP* GetFilenameMap();

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

#endif