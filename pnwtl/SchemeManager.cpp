/**
 * @file SchemeManager.cpp
 * @brief Implement SchemeManager.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "SchemeManager.h"
#include "Schemes.h"
#include "SchemeCompiler.h"
#include "Resource.h"
#include "include/filefinder.h"
#include "l10n.h"

class SMFindData : public FileFinderData
{
public:
	SMFindData() : Newest(0), FoundNewer(false), ForceCompile(false) {}

	uint64_t Newest;
	bool     FoundNewer;
	bool	 ForceCompile;
};

///////////////////////////////////////////////////////////
// SchemeManager
///////////////////////////////////////////////////////////

SchemeManager::SchemeManager(LPCTSTR schemepath, LPCTSTR compiledpath)
{
	SetPath(schemepath);
	
	if(compiledpath != NULL)
	{
		SetCompiledPath(compiledpath);
	}
	else
	{
		SetCompiledPath(schemepath);
	}

	Load();
}

SchemeManager::~SchemeManager()
{
	if(m_SchemePath)
		delete [] m_SchemePath;
	
	if(m_CompiledPath)
		delete [] m_CompiledPath;

	m_Schemes.clear();

	m_SchemeNameMap.clear();
	m_SchemeExtMap.clear();
}

void SetSMString(TCHAR*& str, LPCTSTR newpath)
{
	// Set the path, ensuring a trailing slash.
	int i = _tcslen(newpath);
	if( newpath[i-1] != _T('\\') && newpath[i-1] != _T('/'))
		i++;
	
	if(str != NULL)
		delete [] str;

	str = new TCHAR[i+1];
	_tcscpy(str, newpath);
	if(i != (int)_tcslen(newpath))
		_tcscat(str, _T("\\"));
}

void SchemeManager::SetCompiledPath(LPCTSTR compiledpath)
{
	SetSMString(m_CompiledPath, compiledpath);
}

void SchemeManager::SetPath(LPCTSTR schemepath)
{
	SetSMString(m_SchemePath, schemepath);
}

LPCTSTR SchemeManager::GetPath() const
{
	return m_SchemePath;
}

void SchemeManager::GetPath(tstring& csPath) const
{
	if(m_SchemePath)
		csPath = m_SchemePath;
}

LPCTSTR SchemeManager::GetCompiledPath() const
{
	return m_CompiledPath;
}

void SchemeManager::GetCompiledPath(tstring& csPath) const
{
	if(m_CompiledPath) 
		csPath = m_CompiledPath;
}

#define BAD_CSCHEME_FILE 0x01

void SchemeManager::Load()
{
	if (!internalLoad(false))
	{
		internalLoad(true);
	}
}

/**
 * @return true if successful, false if we get an error
 */
bool SchemeManager::internalLoad(bool forceCompile)
{
	PNASSERT(m_SchemePath != NULL);
	PNASSERT(m_CompiledPath != NULL);
	
	tstring usersettings = m_CompiledPath;
	usersettings += _T("UserSettings.xml");
	
	tstring defaultPath = m_CompiledPath;
	defaultPath += _T("default.cscheme");

	m_DefaultScheme.SetFileName(defaultPath.c_str());

	m_ForceCompile = forceCompile;

	if(forceCompile || !FileExists(defaultPath.c_str()) || schemesHaveChanged())
	{
		Compile();
	}

	FileFinder<SchemeManager, SMFindData> f(this, &SchemeManager::_compiledFileFound);
	f.GetFindData().ForceCompile = forceCompile;
	if (!f.Find(m_CompiledPath, _T("*.cscheme")))
	{
		m_SchemeNameMap.clear();
		m_Schemes.clear();
		return false;
	}

	m_Schemes.sort();

	LoadExtMap(m_SchemeExtMap, m_SchemeFileNameMap);

	return true;
}

bool SchemeManager::schemesHaveChanged()
{
	FileFinder<SchemeManager, SMFindData> f(this, &SchemeManager::_schemeFileFound);
	f.GetFindData().Newest = OPTIONS->Get(PNSK_SCHEMES, _T("NewestScheme"), static_cast<uint64_t>(0));

	f.Find(m_SchemePath, _T("*.scheme"));
	f.Find(m_SchemePath, _T("*.schemedef"));
	
	return f.GetFindData().FoundNewer;
}

void SchemeManager::_schemeFileFound(LPCTSTR path, SMFindData& file, bool& shouldContinue)
{
	if (file.GetLastWriteTime() > file.Newest)
	{
		file.FoundNewer = true;
		shouldContinue = false;
	}
}

void SchemeManager::_compiledFileFound(LPCTSTR path, SMFindData& file, bool& shouldContinue)
{
	if(_tcscmp(_T("default.cscheme"), file.GetFilename()) != 0)
	{
		Scheme sch;
		SCIT csi = m_Schemes.insert(m_Schemes.end(), sch);
		Scheme* cs = &(*csi);

		cs->SetSchemeManager(this);
		
		tstring to_open = m_CompiledPath;
		to_open += file.GetFilename();

		cs->SetFileName(to_open.c_str());

		if(!cs->CheckName())
		{
			tstring dbgout = _T("Skipping bad scheme: ");
			dbgout += file.GetFilename();
			m_Schemes.erase(csi);
			LOG(dbgout.c_str());

			// If this is our first time around, we throw an exception
			// to cause the outer function to force us to try a recompile...
			if(!file.ForceCompile)
			{
				shouldContinue = false;
			}
		}
		else
		{
			std::string schemeName(cs->GetName());
			std::transform(schemeName.begin(), schemeName.end(), schemeName.begin(), tolower);
			
			m_SchemeNameMap.insert(m_SchemeNameMap.end(), SCHEME_MAPA::value_type(schemeName, cs));
		}
	}
}

/**
 * Load the extension to filetype mappings from a flat
 * properties style key=value file. The file must be formatted:
 * .extension=schemename\r\n
 */
void SchemeManager::LoadExtMap(SCHEME_MAP& extMap, SCHEME_MAP& fnMap, bool noUserMap)
{
	PNASSERT(m_SchemePath != NULL);
	PNASSERT(m_CompiledPath != NULL);
	
	tstring fn;
	
	if(!noUserMap)
	{
		fn = m_CompiledPath;
		fn += _T("extmap.dat");

		internalLoadExtMap(fn.c_str(), extMap, fnMap);
	}
	
	fn = m_SchemePath;
	fn += _T("extmap.dat");
	internalLoadExtMap(fn.c_str(), extMap, fnMap);
}

Scheme* SchemeManager::internalSchemeForFileName(const tstring& filename)
{
	SCHEME_MAPIT i = m_SchemeFileNameMap.find(filename);
	
	if(i != m_SchemeFileNameMap.end())
		return (*i).second;
	return NULL;
}

Scheme* SchemeManager::internalSchemeForExt(const tstring& extension)
{
	SCHEME_MAPIT i = m_SchemeExtMap.find(extension);
	
	if(i != m_SchemeExtMap.end())
		return (*i).second;
	return NULL;
}

/**
 * @return The scheme for extension "ext" e.g. .pas
 */
Scheme* SchemeManager::SchemeForExt(LPCTSTR ext)
{
	TCHAR *e = new TCHAR[_tcslen(ext)+1];
	_tcscpy(e, ext);
	e = CharLower(e);

	Scheme* pRet = internalSchemeForExt( tstring(e) );

	delete [] e;

	if(pRet == NULL)
		return &m_DefaultScheme;

	return pRet;
}

/**
 * @return whatever scheme is appropriate for the filename passed in.
 */
Scheme* SchemeManager::SchemeForFile(LPCTSTR filename)
{
	CFileName fn(filename);
	fn.ToLower();

	Scheme* pScheme = internalSchemeForExt( fn.GetExtension() );
	if(!pScheme)
	{
		// See if we can match on a simple filename basis.
		// This is basically for makefiles etc.
		pScheme = internalSchemeForFileName( fn.GetFileName() );
	}

	if(!pScheme)
		return &m_DefaultScheme;
	
	return pScheme;
}

/**
 * @return Scheme* identified by "name"
 */
Scheme* SchemeManager::SchemeByName(LPCSTR name)
{
	std::string strName(name);
	std::transform(strName.begin(), strName.end(), strName.begin(), tolower);

	SCHEME_MAPA::const_iterator i = m_SchemeNameMap.find(strName);

	if(i != m_SchemeNameMap.end())
	{
		return (*i).second;
	}
	else
		return &m_DefaultScheme;
}

Scheme* SchemeManager::GetDefaultScheme() 
{
	return &m_DefaultScheme;
}

SCHEME_LIST* SchemeManager::GetSchemesList()
{
	return &m_Schemes;
}

SCHEME_MAP* SchemeManager::GetExtensionMap()
{ 
	return &m_SchemeExtMap; 
}

SCHEME_MAP* SchemeManager::GetFilenameMap() 
{ 
	return &m_SchemeFileNameMap; 
}

/**
 * Compile all available schemes
 */
void SchemeManager::Compile()
{
	PNASSERT(m_SchemePath != NULL);
	PNASSERT(m_CompiledPath != NULL);

	SchemeCompiler sc;
	sc.Compile(m_SchemePath, m_CompiledPath, _T("master.scheme"));

	OPTIONS->Set(PNSK_SCHEMES, _T("NewestScheme"), sc.GetNewestFileTime());
}

void SchemeManager::BuildMenu(HMENU menu, CommandDispatch* pDispatch, CommandEventHandler* pHandler, int iCommand)
{
	CSMenuHandle m(menu);
	int id;

	m.AddItem(LS(IDS_FILE_NEW), ID_FILE_NEW);
	m.AddItem(LS(IDS_FILE_NEW_PROJECT), ID_FILE_NEW_PROJECT);
	m.AddItem(LS(IDS_FILE_NEW_WORKSPACE), ID_FILE_NEW_WORKSPACE);
	m.AddSeparator();
	
	id = pDispatch->RegisterCallback(pHandler, iCommand, (LPVOID)GetDefaultScheme());
	m.AddItem(LS(IDS_DEFAULTSCHEME), id);

	for(SCIT i = m_Schemes.begin(); i != m_Schemes.end(); ++i)
	{
		if( !(*i).IsInternal() )
		{
			id = pDispatch->RegisterCallback(pHandler, iCommand, (LPVOID)&(*i));
			m.AddItem( (*i).GetTitle(), id);
		}
	}

	pDispatch->UpdateMenuShortcuts(menu);
}

void SchemeManager::SaveExtMap()
{
	// Keep a map around to see if we're actually different from the originals.
	SCHEME_MAP origExts;
	LoadExtMap(origExts, origExts, true);
	
	// Get the file...
	tstring fn;
	fn = m_CompiledPath;
	fn += _T("extmap.dat");
	CTextFile file;
	if(!file.Open(fn.c_str(), CFile::modeWrite | CFile::modeText))
		return;

	tstring line;

	for(SCHEME_MAP::const_iterator i = m_SchemeExtMap.begin(); i != m_SchemeExtMap.end(); ++i)
	{
		SCHEME_MAP::const_iterator match = origExts.find((*i).first);
		if(match != origExts.end())
		{
			if((*match).second == (*i).second)
				continue; // they're the same, skip it.
		}

		// write
		CA2CT schemeName((*i).second->GetName());
		line = (*i).first + _T("=");
		line += schemeName;
		line += _T("\n");
		file.WriteLine(line.c_str());
	}

	for(SCHEME_MAP::const_iterator j = m_SchemeFileNameMap.begin(); j != m_SchemeFileNameMap.end(); ++j)
	{
		SCHEME_MAP::const_iterator match = origExts.find((*j).first);
		if(match != origExts.end())
		{
			if((*match).second == (*j).second)
				continue; // they're the same, skip it.
		}

		// write
		CA2CT schemeName((*j).second->GetName());
		line = (*j).first + _T("=");
		line += schemeName;
		line += _T("\n");
		file.WriteLine(line.c_str());
	}

	file.Close();
}

void SchemeManager::internalLoadExtMap(LPCTSTR filename, SCHEME_MAP& extMap, SCHEME_MAP& fnMap)
{
	CTextFile file;
	
	if(file.Open(filename, CFile::modeText))
	{
		CString buf;
		
		tstring ext;
		tstring scheme;
		
		Scheme* sch;
		int pos;

		while (file.ReadLine(buf))
		{
			if(buf.GetLength() == 0)
			{
				UNEXPECTED(_T("Read an empty line from the extension map file."));
				continue;
			}

			pos = buf.Find(_T('='));
			ext = buf.Left(pos);
			scheme = buf.Mid(pos+1);
			
			if (scheme.length() > SC_HDR_NAMESIZE)
			{
				scheme.resize(SC_HDR_NAMESIZE);
			}

			CT2CA schemeName(scheme.c_str());
			sch = SchemeByName(schemeName);
			if(sch == NULL)
			{
				UNEXPECTED(_T("Failed to get an instance from SchemeByName"));
			}
			else
			{
				if(ext[0] != _T('.'))
					fnMap.insert(fnMap.end(), SCMITEM(ext, sch));
				else
					extMap.insert(extMap.end(), SCMITEM(ext, sch));
			}
		}

		file.Close();
	}
}