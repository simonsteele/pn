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

//////////////////////////////////////////////////////////////////////////////
// Options
//////////////////////////////////////////////////////////////////////////////

Options::Options()
{
	// Initialisation
	m_FindOptions.Found = false;
	m_ReplaceOptions.Found = false;
	m_ReplaceOptions.FindText = _T("");
	m_ReplaceOptions.ReplaceText = _T("");
	m_FindOptions.FindText = _T("");
}

Options::~Options()
{
}

int Options::GetCached(ECachedOption option)
{
	return cache[option];
}

void Options::SetCached(ECachedOption option, int value)
{
	cache[option] = value;
}

void Options::BeginGroupOperation(LPCTSTR subkey)
{
	group(subkey);
}

void Options::EndGroupOperation()
{
	ungroup();
}

void Options::loadCache()
{
	// Editor Settings ----------------------
	group(PNSK_EDITOR);	
	
	cache[OShowIndentGuides]		= Get(NULL, _T("IndentationMarkers"), true);
	cache[OLineEndings]				= Get(NULL, _T("DefaultLineEndings"), PNSF_Windows);
	cache[OTabWidth]				= Get(NULL, _T("TabWidth"), 4);
	cache[OUseTabs]					= Get(NULL, _T("UseTabs"), true);
	cache[OLineNumbers]				= Get(NULL, _T("LineNumbers"), false);
	cache[OLineHighlight]			= Get(NULL, _T("LineHighlight"), false);
	cache[OLineHighlightColour]		= Get(NULL, _T("LineHighlightColour"), (int)RGB(255, 255, 224));
	cache[ORightGuide]				= Get(NULL, _T("RightGuide"), 0);
	cache[ORightColumn]				= Get(NULL, _T("RightColumn"), 76);
	cache[ORightGuideColour]		= Get(NULL, _T("RightGuideColour"), (int)RGB(215, 215, 215));
	cache[ODefaultCodePage]			= Get(NULL, _T("DefaultCodePage"), PNCP_Default);
	cache[OWordWrap]				= Get(NULL, _T("WordWrap"), false);
	cache[ODefaultScintillaCache]	= Get(NULL, _T("LayoutCacheMode"), SC_CACHE_CARET);
	cache[OVisibleLineEndings]		= Get(NULL, _T("VisibleLineEndings"), FALSE);
	cache[OVisibleWhiteSpace]		= Get(NULL, _T("VisibleWhiteSpace"), FALSE);
	ungroup();
	
	// Interface Settings -------------------
	group(PNSK_INTERFACE);

	cache[OMaximiseNew]				= Get(NULL, _T("MaximiseNew"), false);
	cache[OShowFullPath]			= Get(NULL, _T("ShowFullPath"), false);
	cache[OAlreadyOpenAction]		= Get(NULL, _T("AlreadyOpenAction"), eSwitch);
	cache[OAlreadyOpenDropAction]	= Get(NULL, _T("AlreadyOpenDropAction"), eSwitch);
	ungroup();

	// Find and Replace Settings ------------
	group(PNSK_FIND);
	
	m_FindOptions.Direction			= (BOOL)Get(NULL, _T("Find Direction"), true);
	m_FindOptions.FindText			= Get(NULL, _T("Find FindText"), _T("")).c_str();
	m_FindOptions.Loop				= (BOOL)Get(NULL, _T("Find Loop"), true);
	m_FindOptions.MatchCase			= (BOOL)Get(NULL, _T("Find MatchCase"), false);
	m_FindOptions.MatchWholeWord	= (BOOL)Get(NULL, _T("Find MatchWholeWord"), false);
	m_FindOptions.UseRegExp			= (BOOL)Get(NULL, _T("Find UseRegExp"), false);
	m_FindOptions.UseSlashes		= (BOOL)Get(NULL, _T("Find UseSlashes"), false);

	m_ReplaceOptions.Direction		= (BOOL)Get(NULL, _T("Replace Direction"), true);
	m_ReplaceOptions.FindText		= Get(NULL, _T("Replace FindText"), _T("")).c_str();
	m_ReplaceOptions.ReplaceText	= Get(NULL, _T("Replace ReplaceText"), _T("")).c_str();
	m_ReplaceOptions.Loop			= (BOOL)Get(NULL, _T("Replace Loop"), true);
	m_ReplaceOptions.MatchCase		= (BOOL)Get(NULL, _T("Replace MatchCase"), false);
	m_ReplaceOptions.MatchWholeWord = (BOOL)Get(NULL, _T("Replace MatchWholeWord"), false);
	m_ReplaceOptions.UseRegExp		= (BOOL)Get(NULL, _T("Replace UseRegExp"), false);
	m_ReplaceOptions.UseSlashes		= (BOOL)Get(NULL, _T("Replace UseSlashes"), false);

	// New Search Options...
	m_SearchOptions.FindText		= Get(NULL, _T("FindText"), _T("")).c_str();
	m_SearchOptions.ReplaceText		= Get(NULL, _T("ReplaceText"), _T("")).c_str();
	m_SearchOptions.Path			= Get(NULL, _T("Path"), _T("")).c_str();
	m_SearchOptions.FileExts		= Get(NULL, _T("FileExts"), _T("")).c_str();
	
	m_SearchOptions.Direction		= (BOOL)Get(NULL, _T("Direction"), true);
	m_SearchOptions.Loop			= (BOOL)Get(NULL, _T("Loop"), true);
	m_SearchOptions.MatchCase		= (BOOL)Get(NULL, _T("MatchCase"), false);
	m_SearchOptions.MatchWholeWord	= (BOOL)Get(NULL, _T("MatchWholeWord"), false);
	m_SearchOptions.Recurse			= (BOOL)Get(NULL, _T("Recurse"), true);
	m_SearchOptions.UseRegExp		= (BOOL)Get(NULL, _T("UseRegExp"), false);
	m_SearchOptions.UseSlashes		= (BOOL)Get(NULL, _T("UseSlashes"), false);	

	cache[OFindAlphaEnabled]		= Get(NULL, _T("FindAlphaEnabled"), true);
	cache[OFindAlphaPercent]		= Get(NULL, _T("FindAlphaPercent"), 60);

	ungroup();
}

void Options::saveCache()
{
	// Editor Settings ----------------------
	group(PNSK_EDITOR);
	
	Set(NULL, _T("IndentationMarkers"),		cache[OShowIndentGuides]);
	Set(NULL, _T("DefaultLineEndings"),		cache[OLineEndings]);
	Set(NULL, _T("TabWidth"),				cache[OTabWidth]);
	Set(NULL, _T("UseTabs"),				cache[OUseTabs]);
	Set(NULL, _T("LineNumbers"),			cache[OLineNumbers]);
	Set(NULL, _T("LineHighlight"),			cache[OLineHighlight]);
	Set(NULL, _T("LineHighlightColour"),	cache[OLineHighlightColour]);
	Set(NULL, _T("RightGuide"),				cache[ORightGuide]);
	Set(NULL, _T("RightColumn"),			cache[ORightColumn]);
	Set(NULL, _T("RightGuideColour"),		cache[ORightGuideColour]);
	Set(NULL, _T("DefaultCodePage"),		cache[ODefaultCodePage]);
	Set(NULL, _T("WordWrap"),				cache[OWordWrap]);
	Set(NULL, _T("DefaultScintillaCache"),	cache[ODefaultScintillaCache]);
	Set(NULL, _T("VisibleLineEndings"),		cache[OVisibleLineEndings]);
	Set(NULL, _T("VisibleWhiteSpace"),		cache[OVisibleWhiteSpace]);

	ungroup();
	
	// Interface Settings -------------------
	group(PNSK_INTERFACE);
	
	Set(NULL, _T("MaximiseNew"),			cache[OMaximiseNew]);
	Set(NULL, _T("ShowFullPath"),			cache[OShowFullPath]);
	Set(NULL, _T("AlreadyOpenAction"),		cache[OAlreadyOpenAction]);
	Set(NULL, _T("AlreadyOpenDropAction"),	cache[OAlreadyOpenDropAction]);

	ungroup();

	// Find and Replace Settings ------------
	group(PNSK_FIND);
	
	Set(NULL, _T("Find Direction"),			m_FindOptions.Direction);
	Set(NULL, _T("Find FindText"),			m_FindOptions.FindText);
	Set(NULL, _T("Find Loop"),				m_FindOptions.Loop);
	Set(NULL, _T("Find MatchCase"),			m_FindOptions.MatchCase);
	Set(NULL, _T("Find MatchWholeWord"),	m_FindOptions.MatchWholeWord);
	Set(NULL, _T("Find UseRegExp"),			m_FindOptions.UseRegExp);
	Set(NULL, _T("Find UseSlashes"),		m_FindOptions.UseSlashes);

	Set(NULL, _T("Replace Direction"),		m_ReplaceOptions.Direction);
	Set(NULL, _T("Replace FindText"),		m_ReplaceOptions.FindText);
	Set(NULL, _T("Replace ReplaceText"),	m_ReplaceOptions.ReplaceText);
	Set(NULL, _T("Replace Loop"),			m_ReplaceOptions.Loop);
	Set(NULL, _T("Replace MatchCase"),		m_ReplaceOptions.MatchCase);
	Set(NULL, _T("Replace MatchWholeWord"), m_ReplaceOptions.MatchWholeWord);
	Set(NULL, _T("Replace UseRegExp"),		m_ReplaceOptions.UseRegExp);
	Set(NULL, _T("Replace UseSlashes"),		m_ReplaceOptions.UseSlashes);

	// New search options
	Set(NULL, _T("FindText"),				m_SearchOptions.FindText);
	Set(NULL, _T("ReplaceText"),			m_SearchOptions.ReplaceText);
	Set(NULL, _T("Path"),					m_SearchOptions.Path);
	Set(NULL, _T("FileExts"),				m_SearchOptions.FileExts);
	
	Set(NULL, _T("Direction"),				m_SearchOptions.Direction);
	Set(NULL, _T("Loop"),					m_SearchOptions.Loop);
	Set(NULL, _T("MatchCase"),				m_SearchOptions.MatchCase);
	Set(NULL, _T("MatchWholeWord"),			m_SearchOptions.MatchWholeWord);
	Set(NULL, _T("Recurse"),				m_SearchOptions.Recurse);
	Set(NULL, _T("UseRegExp"),				m_SearchOptions.UseRegExp);
	Set(NULL, _T("UseSlashes"),				m_SearchOptions.UseSlashes);

	Set(NULL, _T("FindAlphaEnabled"),		cache[OFindAlphaEnabled]);
	Set(NULL, _T("FindAlphaPercent"),		cache[OFindAlphaPercent]);

	ungroup();
}

void Options::copy(Options* other)
{
	memcpy(cache, other->cache, sizeof(int)*OPTION_COUNT);

	m_FindOptions.Direction			= other->m_FindOptions.Direction;
	m_FindOptions.FindText			= other->m_FindOptions.FindText;
	m_FindOptions.Loop				= other->m_FindOptions.Loop;
	m_FindOptions.MatchCase			= other->m_FindOptions.MatchCase;
	m_FindOptions.MatchWholeWord	= other->m_FindOptions.MatchWholeWord;
	m_FindOptions.UseRegExp			= other->m_FindOptions.UseRegExp;
	m_FindOptions.UseSlashes		= other->m_FindOptions.UseSlashes;

	m_ReplaceOptions.Direction		= other->m_ReplaceOptions.Direction;
	m_ReplaceOptions.FindText		= other->m_ReplaceOptions.FindText;
	m_ReplaceOptions.ReplaceText	= other->m_ReplaceOptions.ReplaceText;
	m_ReplaceOptions.Loop			= other->m_ReplaceOptions.Loop;
	m_ReplaceOptions.MatchCase		= other->m_ReplaceOptions.MatchCase;
	m_ReplaceOptions.MatchWholeWord = other->m_ReplaceOptions.MatchWholeWord;
	m_ReplaceOptions.UseRegExp		= other->m_ReplaceOptions.UseRegExp;
	m_ReplaceOptions.UseSlashes		= other->m_ReplaceOptions.UseSlashes;
}

void Options::SavePrintSettings(SPrintOptions* pSettings)
{
	group(PNSK_PRINT);

	Set(NULL, _T("LeftMargin"), pSettings->rcMargins.left);
	Set(NULL, _T("TopMargin"), pSettings->rcMargins.top);
	Set(NULL, _T("RightMargin"), pSettings->rcMargins.right);
	Set(NULL, _T("BottomMargin"), pSettings->rcMargins.bottom);

	Set(NULL, _T("Header"), pSettings->Header);
	Set(NULL, _T("Footer"), pSettings->Footer);

	ungroup();
}

void Options::LoadPrintSettings(SPrintOptions* pSettings)
{
	group(PNSK_PRINT);

	// defaults are in 10ths of a millimeter, so we go for a centimeter on each side.
	pSettings->rcMargins.left	= Get(NULL, _T("LeftMargin"), 1000);
	pSettings->rcMargins.top	= Get(NULL, _T("TopMargin"), 1000);
	pSettings->rcMargins.right	= Get(NULL, _T("RightMargin"), 1000);
	pSettings->rcMargins.bottom = Get(NULL, _T("BottomMargin"), 1000);

	pSettings->Header = Get(NULL, _T("Header"), _T("Programmers Notepad - %f")).c_str();
	pSettings->Footer = Get(NULL, _T("Footer"), _T("Page %p, %c - %t")).c_str();

	ungroup();
}

void Options::GetPNPath(tstring& path, int pathtype)
{
	TCHAR buf[MAX_PATH +1];
	memset(buf, 0, sizeof(buf));

	// Do paths that are relative to PN home dir.
	if(pathtype < PNPATH_USERMIN)
	{
		GetModuleFileName(NULL, buf, MAX_PATH);
		path = buf;
		
		int cutoff = path.rfind(_T('\\'));
		path = path.substr(0, cutoff+1);

		switch(pathtype)
		{
			case PNPATH_SCHEMES:
				path += _T("Schemes\\");
				break;
			case PNPATH_CLIPS:
				path += _T("Clips\\");
				break;
			case PNPATH_TOOLS:
				path += _T("Tools\\");
				break;
			case PNPATH_TAGGERS:
				path += _T("Taggers\\");
				break;
			case PNPATH_PROJECTTEMPLATES:
				path += _T("Projects\\");
				break;
		}
	}
	else if(pathtype == PNPATH_USERSETTINGS || pathtype == PNPATH_USERCLIPS)
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

//////////////////////////////////////////////////////////////////////////////
// OptionsFactory
//////////////////////////////////////////////////////////////////////////////

#include "OptionsRegistry.h"
#include "OptionsIni.h"

Options* OptionsFactory::GetOptions(EOptionsType type, Options* oldOptions)
{
	Options* newInstance = NULL;
	switch(type)
	{
		case OTRegistry:
			newInstance = new RegistryOptions;
			break;
		case OTIni:
			newInstance = new IniOptions;
			break;
	}

	if(oldOptions)
	{
		newInstance->copy(oldOptions);
		delete oldOptions;
	}
	else
	{
		newInstance->loadCache();
	}

	return newInstance;
}

void OptionsFactory::Release(Options* options)
{
	options->saveCache();
	delete options;
}