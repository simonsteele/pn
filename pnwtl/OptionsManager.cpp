/**
 * @file optionsmanager.cpp
 * @brief Configuration functionality.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "OptionsManager.h"
#include "files.h"
#include "ssreg.h"
using namespace ssreg;

COptionsManager* COptionsManager::s_pInstance = NULL;

COptionsManager::COptionsManager()
{
	Load();
}

COptionsManager::~COptionsManager()
{
	Save();
}

void COptionsManager::Load()
{
	CSRegistry reg;
	ctcString root(pnregroot);
	ctcString cs(root);
	
	// Editor Settings ----------------------
	cs += PNSK_EDITOR;
	reg.OpenKey(cs.c_str(), true);
	ShowIndentGuides = reg.ReadBool(_T("IndentationMarkers"), true);
	LineEndings = (ELineEndings)reg.ReadInt(_T("DefaultLineEndings"), leCRLF);
	TabWidth = reg.ReadInt(_T("TabWidth"), 4);

	// Find and Replace Settings ------------

	m_FindOptions.Direction = true;
	m_FindOptions.Loop = true;
	m_FindOptions.FindText = _T("");
	
	m_ReplaceOptions.Direction = true;
	m_ReplaceOptions.Loop = true;
	m_ReplaceOptions.FindText = _T("");
	m_ReplaceOptions.ReplaceText = _T("");

	//cs = root + PNSK_INTERFACE
	//reg.OpenKey(cs, true);
}

void COptionsManager::Save()
{
	ssreg::CSRegistry reg;
	ctcString root(pnregroot);
	ctcString cs;

	// Editor Settings ----------------------
	cs = root + _T("Editor Settings");
	reg.OpenKey(cs.c_str(), true);
	reg.WriteBool(_T("IndentationMarkers"), ShowIndentGuides);
	reg.WriteInt(_T("DefaultLineEndings"), LineEndings);
	reg.WriteInt(_T("TabWidth"), TabWidth);
	
	//cs = root + _T("Interface Settings");
	//reg.OpenKey(cs, true);
}

bool COptionsManager::Get(LPCTSTR subkey, LPCTSTR value, bool bDefault)
{
	CSRegistry reg;
	ctcString root(pnregroot);
	root += subkey;

	reg.OpenKey(root.c_str());
	return reg.ReadBool(value, bDefault);
}

int COptionsManager::Get(LPCTSTR subkey, LPCTSTR value, int iDefault)
{
	CSRegistry reg;
	ctcString root(pnregroot);
	root += subkey;

	reg.OpenKey(root.c_str());
	return reg.ReadInt(value, iDefault);
}

void COptionsManager::Set(LPCTSTR subkey, LPCTSTR value, bool bVal)
{
	CSRegistry reg;
	ctcString root(pnregroot);
	root += subkey;

	reg.OpenKey(root.c_str());
	reg.WriteBool(value, bVal);
}

void COptionsManager::Set(LPCTSTR subkey, LPCTSTR value, int iVal)
{
	CSRegistry reg;
	ctcString root(pnregroot);
	root += subkey;

	reg.OpenKey(root.c_str());
	reg.WriteInt(value, iVal);
}

void COptionsManager::SavePrintSettings(SPrintOptions* pSettings)
{
	CSRegistry reg;
	ctcString root(pnregroot);
	root += PNSK_PRINT;

	if ( reg.OpenKey(root.c_str()) )
	{
		reg.WriteInt(_T("LeftMargin"), pSettings->rcMargins.left);
		reg.WriteInt(_T("TopMargin"), pSettings->rcMargins.top);
		reg.WriteInt(_T("RightMargin"), pSettings->rcMargins.right);
		reg.WriteInt(_T("BottomMargin"), pSettings->rcMargins.bottom);
	}
}

void COptionsManager::LoadPrintSettings(SPrintOptions* pSettings)
{
	CSRegistry reg;
	ctcString root(pnregroot);
	root += PNSK_PRINT;

	if ( reg.OpenKey(root.c_str()) )
	{
		pSettings->rcMargins.left = reg.ReadInt(_T("LeftMargin"), 10);
		pSettings->rcMargins.top = reg.ReadInt(_T("TopMargin"), 10);
		pSettings->rcMargins.right = reg.ReadInt(_T("RightMargin"), 10);
		pSettings->rcMargins.bottom = reg.ReadInt(_T("BottomMargin"), 10);
	}
}

COptionsManager* COptionsManager::GetInstance()
{
	if(!s_pInstance)
		s_pInstance = new COptionsManager;

	return s_pInstance;
}

COptionsManager& COptionsManager::GetInstanceRef()
{
	return *GetInstance();
}

void COptionsManager::DeleteInstance()
{
	if(s_pInstance)
	{
		delete s_pInstance;
		s_pInstance = NULL;
	}
}

/**
 * @param path Path buffer, must be at least MAX_PATH big...
 * @param folder Folder ID
 */
BOOL PNGetSpecialFolderPath (LPTSTR path, int folder)
{
    ITEMIDLIST *pidl;		// Shell Item ID List ptr
    IMalloc    *imalloc;	// Shell IMalloc interface ptr
    BOOL result;			// Return value

    if (SHGetSpecialFolderLocation (NULL, folder, &pidl) != NOERROR)
        return FALSE;

    result = SHGetPathFromIDList (pidl, path);

    if (SHGetMalloc (&imalloc) == NOERROR) {
		imalloc->Free(pidl);
        imalloc->Release();
    }

    return result;
}

void COptionsManager::GetSchemesPaths(ctcString& path, ctcString& compiledPath)
{
	TCHAR buf[MAX_PATH +1];
	
	GetModuleFileName(NULL, buf, MAX_PATH);
	path = buf;
	
	int cutoff = path.rfind(_T('\\'));
	path = path.substr(0, cutoff+1);
	path += "Schemes\\";
	
	/*ss 20/01/2003 Fix SF Bug #671357
	SHGetSpecialFolderPath(NULL, buf, CSIDL_APPDATA, TRUE)*/
	if(PNGetSpecialFolderPath(buf, CSIDL_APPDATA))
	{
		compiledPath = buf;
		if(compiledPath[compiledPath.length()-1] != _T('\\'))
		{
			compiledPath += _T('\\');
		}

		compiledPath += _T("Echo Software\\PN2\\");

		CreateDirectoryRecursive(compiledPath.c_str());
	}
	else
	{
		// fallback and compile into the schemes definition folder.
        compiledPath = path;
	}
}