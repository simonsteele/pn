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
	// Initialisation
	m_FindOptions.Found = false;
	m_ReplaceOptions.Found = false;
	m_ReplaceOptions.FindText = _T("");
	m_ReplaceOptions.ReplaceText = _T("");
	m_FindOptions.FindText = _T("");

	// Load settings
	Load();
}

COptionsManager::~COptionsManager()
{
	Save();
}

void COptionsManager::Load()
{
	CSRegistry reg;
	tstring root(pnregroot);
	tstring cs(root);
	
	// Editor Settings ----------------------
	cs += PNSK_EDITOR;
	reg.OpenKey(cs.c_str(), true);
	ShowIndentGuides = reg.ReadBool(_T("IndentationMarkers"), true);
	LineEndings = (EPNSaveFormat)reg.ReadInt(_T("DefaultLineEndings"), PNSF_Windows);
	TabWidth = reg.ReadInt(_T("TabWidth"), 4);
	UseTabs = reg.ReadBool(_T("UseTabs"), true);
	LineNumbers = reg.ReadBool(_T("LineNumbers"), false);
	AlreadyOpenAction = (EAlreadyOpenAction)reg.ReadInt(_T("AlreadyOpenAction"), eSwitch);
	LineHighlight = reg.ReadBool(_T("LineHighlight"), false);
	LineHighlightColour = reg.ReadInt(_T("LineHighlightColour"), RGB(255, 255, 224));
	RightGuide = reg.ReadInt(_T("RightGuide"), 0);
	RightColumn = reg.ReadInt(_T("RightColumn"), 76);
	RightGuideColour = reg.ReadInt(_T("RightGuideColour"), RGB(215, 215, 215));
	
	// Interface Settings -------------------
	cs = root + PNSK_INTERFACE;
	reg.OpenKey(cs.c_str(), true);
	MaximiseNew = reg.ReadBool(_T("MaximiseNew"), false);
	ShowFullPath = reg.ReadBool(_T("ShowFullPath"), false);

	// Find and Replace Settings ------------
	cs = root + PNSK_FIND;
	reg.OpenKey(cs.c_str(), true);
	m_FindOptions.Direction = reg.ReadBool(_T("Find Direction"), true);
	tstring val;
	if( reg.ReadString(_T("Find FindText"), val) )
		m_FindOptions.FindText = val.c_str();
	m_FindOptions.Loop = reg.ReadBool(_T("Find Loop"), true);
	m_FindOptions.MatchCase = reg.ReadBool(_T("Find MatchCase"), false);
	m_FindOptions.MatchWholeWord = reg.ReadBool(_T("Find MatchWholeWord"), false);
	m_FindOptions.UseRegExp = reg.ReadBool(_T("Find UseRegExp"), false);
	m_FindOptions.UseSlashes = reg.ReadBool(_T("Find UseSlashes"), false);

	m_ReplaceOptions.Direction = reg.ReadBool(_T("Replace Direction"), true);
	if( reg.ReadString(_T("Replace FindText"), val) )
		m_ReplaceOptions.FindText = val.c_str();
	if( reg.ReadString(_T("Replace ReplaceText"), val) )
		m_ReplaceOptions.ReplaceText = val.c_str();
	m_ReplaceOptions.Loop = reg.ReadBool(_T("Replace Loop"), true);
	m_ReplaceOptions.MatchCase = reg.ReadBool(_T("Replace MatchCase"), false);
	m_ReplaceOptions.MatchWholeWord = reg.ReadBool(_T("Replace MatchWholeWord"), false);
	m_ReplaceOptions.UseRegExp = reg.ReadBool(_T("Replace UseRegExp"), false);
	m_ReplaceOptions.UseSlashes = reg.ReadBool(_T("Replace UseSlashes"), false);
}

void COptionsManager::Save()
{
	ssreg::CSRegistry reg;
	tstring root(pnregroot);
	tstring cs;

	// Editor Settings ----------------------
	cs = root + _T("Editor Settings");
	reg.OpenKey(cs.c_str(), true);
	reg.WriteBool(_T("IndentationMarkers"), ShowIndentGuides);
	reg.WriteInt(_T("DefaultLineEndings"), LineEndings);
	reg.WriteInt(_T("TabWidth"), TabWidth);
	reg.WriteBool(_T("UseTabs"), UseTabs);
	reg.WriteBool(_T("LineNumbers"), LineNumbers);
	reg.WriteInt(_T("AlreadyOpenAction"), AlreadyOpenAction);
	reg.WriteBool(_T("LineHighlight"), LineHighlight);
	reg.WriteInt(_T("LineHighlightColour"), LineHighlightColour);
	reg.WriteInt(_T("RightGuide"), RightGuide);
	reg.WriteInt(_T("RightColumn"), RightColumn);
	reg.WriteInt(_T("RightGuideColour"), RightGuideColour);
	
	// Interface Settings -------------------
	cs = root + PNSK_INTERFACE;
	reg.OpenKey(cs.c_str(), true);
	reg.WriteBool(_T("MaximiseNew"), MaximiseNew);
	reg.WriteBool(_T("ShowFullPath"), ShowFullPath);

	// Find and Replace Settings ------------
	cs = root + PNSK_FIND;
	reg.OpenKey(cs.c_str(), true);
	reg.WriteBool(_T("Find Direction"), m_FindOptions.Direction);
	reg.WriteString(_T("Find FindText"), m_FindOptions.FindText);
	reg.WriteBool(_T("Find Loop"), m_FindOptions.Loop);
	reg.WriteBool(_T("Find MatchCase"), m_FindOptions.MatchCase);
	reg.WriteBool(_T("Find MatchWholeWord"), m_FindOptions.MatchWholeWord);
	reg.WriteBool(_T("Find UseRegExp"), m_FindOptions.UseRegExp);
	reg.WriteBool(_T("Find UseSlashes"), m_FindOptions.UseSlashes);

	reg.WriteBool(_T("Replace Direction"), m_ReplaceOptions.Direction);
	reg.WriteString(_T("Replace FindText"), m_ReplaceOptions.FindText);
	reg.WriteString(_T("Replace ReplaceText"), m_ReplaceOptions.ReplaceText);
	reg.WriteBool(_T("Replace Loop"), m_ReplaceOptions.Loop);
	reg.WriteBool(_T("Replace MatchCase"), m_ReplaceOptions.MatchCase);
	reg.WriteBool(_T("Replace MatchWholeWord"), m_ReplaceOptions.MatchWholeWord);
	reg.WriteBool(_T("Replace UseRegExp"), m_ReplaceOptions.UseRegExp);
	reg.WriteBool(_T("Replace UseSlashes"), m_ReplaceOptions.UseSlashes);
}

bool COptionsManager::Get(LPCTSTR subkey, LPCTSTR value, bool bDefault)
{
	CSRegistry reg;
	tstring root(pnregroot);
	root += subkey;

	reg.OpenKey(root.c_str());
	return reg.ReadBool(value, bDefault);
}

int COptionsManager::Get(LPCTSTR subkey, LPCTSTR value, int iDefault)
{
	CSRegistry reg;
	tstring root(pnregroot);
	root += subkey;

	reg.OpenKey(root.c_str());
	return reg.ReadInt(value, iDefault);
}

tstring COptionsManager::Get(LPCTSTR subkey, LPCTSTR value, LPCTSTR szDefault)
{
	CSRegistry reg;
	tstring root(pnregroot);
	root += subkey;

	reg.OpenKey(root.c_str());
	tstring str;
	if(!reg.ReadString(value, str))
		str = szDefault;
	return str;
}

void COptionsManager::Set(LPCTSTR subkey, LPCTSTR value, bool bVal)
{
	CSRegistry reg;
	tstring root(pnregroot);
	root += subkey;

	reg.OpenKey(root.c_str());
	reg.WriteBool(value, bVal);
}

void COptionsManager::Set(LPCTSTR subkey, LPCTSTR value, int iVal)
{
	CSRegistry reg;
	tstring root(pnregroot);
	root += subkey;

	reg.OpenKey(root.c_str());
	reg.WriteInt(value, iVal);
}

void COptionsManager::Set(LPCTSTR subkey, LPCTSTR value, LPCTSTR szVal)
{
	CSRegistry reg;
	tstring root(pnregroot);
	root += subkey;

	reg.OpenKey(root.c_str());
	reg.WriteString(value, szVal);
}

void COptionsManager::SavePrintSettings(SPrintOptions* pSettings)
{
	CSRegistry reg;
	tstring root(pnregroot);
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
	tstring root(pnregroot);
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
    BOOL		result;		// Return value

    if (SHGetSpecialFolderLocation (NULL, folder, &pidl) != NOERROR)
        return FALSE;

    result = SHGetPathFromIDList (pidl, path);

    if (SHGetMalloc (&imalloc) == NOERROR) {
		imalloc->Free(pidl);
        imalloc->Release();
    }

    return result;
}

void COptionsManager::GetSchemesPaths(tstring& path, tstring& compiledPath)
{
	GetPNPath(path, PNPATH_SCHEMES);
	GetPNPath(compiledPath, PNPATH_USERSETTINGS);
}

void COptionsManager::GetPNPath(tstring& path, int pathtype)
{
	TCHAR buf[MAX_PATH +1];
	memset(buf, 0, sizeof(buf));

	if(pathtype == PNPATH_PN || pathtype == PNPATH_SCHEMES)
	{
		GetModuleFileName(NULL, buf, MAX_PATH);
		path = buf;
		
		int cutoff = path.rfind(_T('\\'));
		path = path.substr(0, cutoff+1);

		if(pathtype == PNPATH_SCHEMES)
			path += _T("Schemes\\");
	}
	else if(pathtype == PNPATH_USERSETTINGS)
	{
		/*ss 20/01/2003 Fix SF Bug #671357
		SHGetSpecialFolderPath(NULL, buf, CSIDL_APPDATA, TRUE)*/
		if(PNGetSpecialFolderPath(buf, CSIDL_APPDATA))
		{
			path = buf;
			if(path[path.length()-1] != _T('\\'))
			{
				path += _T('\\');
			}

			path += _T("Echo Software\\PN2\\");

			CreateDirectoryRecursive(path.c_str());
		}
		else
		{
			// fallback and compile into the schemes definition folder.
			///@todo should we fallback to the temp directory?
			GetPNPath(path, PNPATH_SCHEMES);
		}
	}
}