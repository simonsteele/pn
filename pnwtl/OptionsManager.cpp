/**
 * @file optionsmanager.cpp
 * @brief Configuration functionality.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "OptionsManager.h"
#include "files.h"
#include "fileutil.h"

//////////////////////////////////////////////////////////////////////////////
// Options
//////////////////////////////////////////////////////////////////////////////

Options::Options() : m_UserSettingsPath(_T(""))
{
}

Options::~Options()
{
}

void Options::LoadCache()
{
	loadCache();
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
	cache[OLineHighlightColour]		= Get(NULL, _T("LineHighlightBaseColour"), (int)RGB(0, 0, 0x5e));
	cache[OLineHighlightAlpha]		= Get(NULL, _T("LineHighlightAlpha"), 30);
	cache[ORightGuide]				= Get(NULL, _T("RightGuide"), 0);
	cache[ORightColumn]				= Get(NULL, _T("RightColumn"), 76);
	cache[ORightGuideColour]		= Get(NULL, _T("RightGuideColour"), (int)RGB(215, 215, 215));
	cache[ODefaultCodePage]			= Get(NULL, _T("DefaultCodePage"), PNCP_Default);
	cache[OWordWrap]				= Get(NULL, _T("WordWrap"), false);
	cache[ODefaultScintillaCache]	= Get(NULL, _T("LayoutCacheMode"), SC_CACHE_CARET);
	cache[OVisibleLineEndings]		= Get(NULL, _T("VisibleLineEndings"), FALSE);
	cache[OVisibleWhiteSpace]		= Get(NULL, _T("VisibleWhiteSpace"), FALSE);
	cache[OConvertLinesOnPaste]		= Get(NULL, _T("ConvertLineEndingsOnPaste"), TRUE);
	cache[ODefaultCharSet]			= Get(NULL, _T("DefaultCharSet"), SC_CHARSET_DEFAULT);
	cache[OAutoComplete]			= Get(NULL, _T("Autocomplete"), TRUE);
	cache[OAutoCompleteUseKeywords]	= Get(NULL, _T("AutocompleteUseKeywords"), TRUE);
	cache[OAutoCompleteUseTags]		= Get(NULL, _T("AutocompleteUseTags"), TRUE);
	cache[OAutoCompleteStartChars]	= Get(NULL, _T("AutocompleteStartChars"), 2);
	cache[OAutoCompleteTags]		= Get(NULL, _T("AutocompleteTags"), TRUE);
	cache[OAutoCompleteActivation]	= Get(NULL, _T("AutocompleteActivation"), eacManual);
	cache[OFoldingEnabled]			= Get(NULL, _T("Folding"), TRUE);
	cache[OCaretXFlags]				= Get(NULL, _T("CaretXFlagsNew"), CARET_SLOP);
	cache[OCaretXMove]				= Get(NULL, _T("CaretXMove"), 20);
	cache[OCaretYFlags]				= Get(NULL, _T("CaretYFlags"), CARET_SLOP | CARET_EVEN);
	cache[OCaretYMove]				= Get(NULL, _T("CaretYMove"), 3);
	cache[OLinePaddingTop]			= Get(NULL, _T("OLinePaddingTop"), 1);
	cache[OLinePaddingBottom]		= Get(NULL, _T("OLinePaddingBottom"), 0);
	cache[ODefaultEncoding]			= Get(NULL, _T("DefaultEncoding"), eUnknown);
	cache[OMultiByteCodePage]		= Get(NULL, _T("MultiByteCodePage"), PNCP_Default);
	ungroup();
	
	// Interface Settings -------------------
	group(PNSK_INTERFACE);

	cache[OMaximiseNew]				= Get(NULL, _T("MaximiseNew"), true);
	cache[OShowFullPath]			= Get(NULL, _T("ShowFullPath"), false);
	cache[OAlreadyOpenAction]		= Get(NULL, _T("AlreadyOpenAction"), eSwitch);
	cache[OAlreadyOpenDropAction]	= Get(NULL, _T("AlreadyOpenDropAction"), eSwitch);
	cache[OManageTabOrder]			= Get(NULL, _T("ManageTabOrder"), true);
	ungroup();

	// Find and Replace Settings ------------
	group(PNSK_FIND);

	// New Search Options...
	m_SearchOptions.SetFindText(Get(NULL, _T("FindText"), _T("")).c_str());
	m_SearchOptions.SetReplaceText(Get(NULL, _T("ReplaceText"), _T("")).c_str());
	m_SearchOptions.SetSearchPath(Get(NULL, _T("Path"), _T("")).c_str());
	m_SearchOptions.SetFileExts(Get(NULL, _T("FileExts"), _T("")).c_str());
	
	m_SearchOptions.SetSearchBackwards(!(BOOL)Get(NULL, _T("Direction"), true));
	m_SearchOptions.SetLoopOK((BOOL)Get(NULL, _T("Loop"), true));
	m_SearchOptions.SetMatchCase((BOOL)Get(NULL, _T("MatchCase"), false));
	m_SearchOptions.SetMatchWholeWord((BOOL)Get(NULL, _T("MatchWholeWord"), false));
	m_SearchOptions.SetRecurse((BOOL)Get(NULL, _T("Recurse"), true));
	m_SearchOptions.SetIncludeHidden((BOOL)Get(NULL, _T("IncludeHidden"), true));
	m_SearchOptions.SetUseRegExp((BOOL)Get(NULL, _T("UseRegExp"), false));
	m_SearchOptions.SetUseSlashes((BOOL)Get(NULL, _T("UseSlashes"), false));

	cache[OFindAlphaEnabled]		= Get(NULL, _T("FindAlphaEnabled"), true);
	cache[OFindAlphaPercent]		= Get(NULL, _T("FindAlphaPercent"), 60);
	cache[OSmartHighlight]			= Get(NULL, _T("SmartHighlight"), true);

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
	Set(NULL, _T("LineHighlightBaseColour"),cache[OLineHighlightColour]);
	Set(NULL, _T("LineHighlightAlpha"),		cache[OLineHighlightAlpha]);
	Set(NULL, _T("RightGuide"),				cache[ORightGuide]);
	Set(NULL, _T("RightColumn"),			cache[ORightColumn]);
	Set(NULL, _T("RightGuideColour"),		cache[ORightGuideColour]);
	Set(NULL, _T("DefaultCodePage"),		cache[ODefaultCodePage]);
	Set(NULL, _T("WordWrap"),				cache[OWordWrap]);
	Set(NULL, _T("DefaultScintillaCache"),	cache[ODefaultScintillaCache]);
	Set(NULL, _T("VisibleLineEndings"),		cache[OVisibleLineEndings]);
	Set(NULL, _T("VisibleWhiteSpace"),		cache[OVisibleWhiteSpace]);
	Set(NULL, _T("ConvertLineEndingsOnPaste"), cache[OConvertLinesOnPaste]);
	Set(NULL, _T("DefaultCharSet"),			cache[ODefaultCharSet]);
	Set(NULL, _T("Autocomplete"),			cache[OAutoComplete]);
	Set(NULL, _T("AutocompleteUseKeywords"),cache[OAutoCompleteUseKeywords]);
	Set(NULL, _T("AutocompleteUseTags"),	cache[OAutoCompleteUseTags]);
	Set(NULL, _T("AutocompleteStartChars"), cache[OAutoCompleteStartChars]);
	Set(NULL, _T("AutocompleteTags"),		cache[OAutoCompleteTags]);
	Set(NULL, _T("AutocompleteActivation"), cache[OAutoCompleteActivation]);
	Set(NULL, _T("Folding"),				cache[OFoldingEnabled]);
	Set(NULL, _T("CaretXFlagsNew"),			cache[OCaretXFlags]);
	Set(NULL, _T("CaretXMove"),				cache[OCaretXMove]);
	Set(NULL, _T("CaretYFlags"),			cache[OCaretYFlags]);
	Set(NULL, _T("CaretYMove"),				cache[OCaretYMove]);
	Set(NULL, _T("OLinePaddingTop"),		cache[OLinePaddingTop]);
	Set(NULL, _T("OLinePaddingBottom"),		cache[OLinePaddingBottom]);
	Set(NULL, _T("DefaultEncoding"),		cache[ODefaultEncoding]);
	Set(NULL, _T("MultiByteCodePage"),		cache[OMultiByteCodePage]);

	ungroup();
	
	// Interface Settings -------------------
	group(PNSK_INTERFACE);
	
	Set(NULL, _T("MaximiseNew"),			cache[OMaximiseNew]);
	Set(NULL, _T("ShowFullPath"),			cache[OShowFullPath]);
	Set(NULL, _T("AlreadyOpenAction"),		cache[OAlreadyOpenAction]);
	Set(NULL, _T("AlreadyOpenDropAction"),	cache[OAlreadyOpenDropAction]);
	Set(NULL, _T("ManageTabOrder"),			cache[OManageTabOrder]);

	ungroup();

	// Find and Replace Settings ------------
	group(PNSK_FIND);

	// New search options
	Set(NULL, _T("FindText"),				m_SearchOptions.GetFindText());
	Set(NULL, _T("ReplaceText"),			m_SearchOptions.GetReplaceText());
	Set(NULL, _T("Path"),					m_SearchOptions.GetSearchPath());
	Set(NULL, _T("FileExts"),				m_SearchOptions.GetFileExts());
	
	Set(NULL, _T("Direction"),				!m_SearchOptions.GetSearchBackwards());
	Set(NULL, _T("Loop"),					m_SearchOptions.GetLoopOK());
	Set(NULL, _T("MatchCase"),				m_SearchOptions.GetMatchCase());
	Set(NULL, _T("MatchWholeWord"),			m_SearchOptions.GetMatchWholeWord());
	Set(NULL, _T("Recurse"),				m_SearchOptions.GetRecurse());
	Set(NULL, _T("IncludeHidden"),			m_SearchOptions.GetIncludeHidden());
	Set(NULL, _T("UseRegExp"),				m_SearchOptions.GetUseRegExp());
	Set(NULL, _T("UseSlashes"),				m_SearchOptions.GetUseSlashes());

	Set(NULL, _T("FindAlphaEnabled"),		cache[OFindAlphaEnabled]);
	Set(NULL, _T("FindAlphaPercent"),		cache[OFindAlphaPercent]);
	Set(NULL, _T("SmartHighlight"),			cache[OSmartHighlight]);

	ungroup();
}

void Options::copy(Options* other)
{
	memcpy(cache, other->cache, sizeof(int)*OPTION_COUNT);

	/*m_FindOptions.Direction			= other->m_FindOptions.Direction;
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
	m_ReplaceOptions.UseSlashes		= other->m_ReplaceOptions.UseSlashes;*/

	//TODO Copy m_SearchOptions
}

void Options::SavePrintSettings(SPrintOptions* pSettings)
{
	group(PNSK_PRINT);

	Set(NULL, _T("LeftMargin"), pSettings->rcMargins.left);
	Set(NULL, _T("TopMargin"), pSettings->rcMargins.top);
	Set(NULL, _T("RightMargin"), pSettings->rcMargins.right);
	Set(NULL, _T("BottomMargin"), pSettings->rcMargins.bottom);

	Set(NULL, _T("Header"), pSettings->Header.c_str());
	Set(NULL, _T("Footer"), pSettings->Footer.c_str());

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

	pSettings->Header = Get(NULL, _T("Header"), _T("Programmer's Notepad - %f")).c_str();
	pSettings->Footer = Get(NULL, _T("Footer"), _T("Page %p, %c - %t")).c_str();

	ungroup();
}

void Options::GetPNPath(tstring& path, int pathtype)
{
	TCHAR buf[MAX_PATH +1];
	buf[0] = '\0';

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
				// path += _T("Taggers\\"); - taggers now in the main directory.
				break;
			case PNPATH_PROJECTTEMPLATES:
				path += _T("Projects\\");
				break;
			case PNPATH_PRESETS:
				path += _T("Presets\\");
				break;
		}
	}
	else if(pathtype == PNPATH_USERSETTINGS || pathtype == PNPATH_USERCLIPS)
	{
		if(m_UserSettingsPath.length() > 0)
		{
			path = m_UserSettingsPath;
		}
		else if (FileUtil::PNGetSpecialFolderPath(buf, CSIDL_APPDATA))
		{
			path = buf;
			if(path[path.length()-1] != _T('\\'))
			{
				path += _T('\\');
			}

			path += _T("Echo Software\\PN2\\");

			//CreateDirectoryRecursive(path.c_str());
		}
		else
		{
			// fallback and compile into the schemes definition folder.
			///@todo should we fallback to the temp directory?
			GetPNPath(path, PNPATH_SCHEMES);
		}
	}
}

void Options::StaticGetPNPath(tstring& path)
{
	TCHAR buf[MAX_PATH +1];
	buf[0] = '\0';

	GetModuleFileName(NULL, buf, MAX_PATH);
	path = buf;
	
	int cutoff = path.rfind(_T('\\'));
	path = path.substr(0, cutoff+1);
}

void Options::SetUserSettingsPath(LPCTSTR path)
{
	CPathName usPath(path);
	m_UserSettingsPath = usPath.c_str();
}

extensions::ISearchOptions* Options::GetSearchOptions()
{
	return static_cast<extensions::ISearchOptions*>(&m_SearchOptions);
}

wchar_t* Options::GetPNPath(int pathtype)
{
	std::wstring s;
	GetPNPath(s, pathtype);
	wchar_t* ret = new wchar_t[s.size()+1];
	wcscpy(ret, s.c_str());
	return ret;
}

wchar_t* Options::GetS(LPCTSTR subkey, LPCTSTR value, LPCTSTR szDefault)
{
	std::wstring s = Get(subkey, value, szDefault);
	wchar_t* ret = new wchar_t[s.size()+1];
	wcscpy(ret, s.c_str());
	return ret;
}

//////////////////////////////////////////////////////////////////////////////
// OptionsFactory
//////////////////////////////////////////////////////////////////////////////

#include "OptionsRegistry.h"
#include "OptionsIni.h"
#include "OptionsXml.h"
//#include "..\optionstest\OptionsSqlite.h"

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
		case OTXml:
			newInstance = new XmlOptions;
			break;
		//case OTSqlite:
		//	newInstance = new SqliteOptions;
		//	break;
	}

	if(oldOptions)
	{
		newInstance->copy(oldOptions);
		delete oldOptions;
	}

	return newInstance;
}

void OptionsFactory::Release(Options* options)
{
	options->saveCache();
	delete options;
}