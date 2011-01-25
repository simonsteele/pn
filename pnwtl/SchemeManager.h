/**
 * @file SchemeManager.cpp
 * @brief Implement SchemeManager.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef schememanager_h__included
#define schememanager_h__included

class Scheme;
class FileFinderData;
class SMFindData;

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
		Scheme* SchemeByName(LPCSTR name);
		
		Scheme* GetDefaultScheme();

		SCHEME_LIST* GetSchemesList();

		SCHEME_MAP* GetExtensionMap();
		SCHEME_MAP* GetFilenameMap();

		void BuildMenu(HMENU menu, CommandDispatch* pDispatch, CommandEventHandler* pHandler, int iCommand = SCHEMEMANAGER_SELECTSCHEME);

		void SaveExtMap();

		void _compiledFileFound(LPCTSTR path, SMFindData& file, bool& shouldContinue);
		void _schemeFileFound(LPCTSTR path, SMFindData& file, bool& /*shouldContinue*/);

	private:
		Scheme* internalSchemeForFileName(const tstring& filename);
		Scheme* internalSchemeForExt(const tstring& extension);
		void internalLoadExtMap(LPCTSTR filename, SCHEME_MAP& extMap, SCHEME_MAP& fnMap);
		bool internalLoad(bool forceCompile);
		bool schemesHaveChanged();

	private:
		TCHAR*			m_SchemePath;
		TCHAR*			m_CompiledPath;
		SCHEME_LIST		m_Schemes;

		SCHEME_MAPA		m_SchemeNameMap;
		SCHEME_MAP		m_SchemeExtMap;
		SCHEME_MAP		m_SchemeFileNameMap;

		DefaultScheme	m_DefaultScheme;
		bool			m_ForceCompile;
};

#endif