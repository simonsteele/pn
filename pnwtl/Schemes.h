
#ifndef __SchemeManager_H__
#define __SchemeManager_H__

#include "Scintillaif.h"
#include "Files.h"
#include "IniFile.h"
#include "SchemeCompiler.h"
#include <list>
#include <map>

class CSchemeManager;

///@todo Add a m_CompiledFile member to save repeatedly changing the file extension and path.
class CScheme
{
	protected:
		TCHAR* m_SchemeFile;
		TCHAR* m_Name;
		CSchemeManager* m_pManager;
		CScheme();

		void SetupScintilla(CScintilla& sc);

	public:
		CScheme(CSchemeManager* pManager);
		CScheme(CSchemeManager* pManager, LPCTSTR filename);
		~CScheme();

		virtual bool Compile(LPCTSTR outfile=NULL);

		virtual bool IsCompiled();

		virtual void EnsureCompiled();

		virtual void Load(CScintilla& sc, LPCTSTR filename = NULL);

		virtual void SetName(LPCTSTR name);

		virtual void CheckName(LPCTSTR filename = NULL);

		virtual LPCTSTR GetName(){return m_Name;}
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
		virtual bool Compile(LPCTSTR outfile = NULL){return true;}

		virtual void Load(CScintilla& sc, LPCTSTR filename = NULL){SetupScintilla(sc);}
		
		virtual bool IsCompiled(){return true;}
		
		virtual void EnsureCompiled(){}

		// Can't set name, it's always "Default"
		virtual void SetName(LPCTSTR name){}

		virtual void CheckName(LPCTSTR filename = NULL){}

		virtual LPCTSTR GetName(){return _T("Default");}
};

typedef std::list<CScheme*>				SCHEME_LIST;
typedef SCHEME_LIST::iterator			scit;	 
typedef std::map<ctcString, CScheme*>	SCHEME_MAP;
typedef SCHEME_MAP::iterator			SCHEME_MAPIT;
typedef SCHEME_MAP::value_type			SCMITEM;

class CSchemeManager
{
private:
	TCHAR*			m_SchemePath;
	TCHAR*			m_CompiledPath;
	SCHEME_LIST		m_Schemes;

	SCHEME_MAP		m_SchemeNameMap;
	SCHEME_MAP		m_SchemeExtMap;

	CDefaultScheme	m_DefaultScheme;

public:
	CSchemeManager() : m_SchemePath(NULL), m_CompiledPath(NULL){}
	CSchemeManager(LPCTSTR schemepath, LPCTSTR compiledpath=NULL);
	~CSchemeManager();
	
	void Load(LPCTSTR fromfolder = NULL);
	void LoadExtMap(LPCTSTR folder);
	void SetPath(LPCTSTR schemepath);
	void SetCompiledPath(LPCTSTR compiledpath);
	void GetPath(ctcString& csPath){if(m_SchemePath) csPath = m_SchemePath;}
	LPCTSTR GetCompiledPath(){return m_CompiledPath;}
	void GetCompiledPath(ctcString& csPath){if(m_CompiledPath) csPath = m_CompiledPath;}

	CScheme* SchemeForExt(LPCTSTR ext);
	CScheme* SchemeByName(LPCTSTR name);
};

#endif