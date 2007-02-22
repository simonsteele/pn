/**
 * @file pntypes.h
 * @brief Define structs etc. used throughout pn2.
 * @author Simon Steele
 * @note Copyright (c) 2002-2004 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef pntypes_h__included
#define pntypes_h__included

#include "scintilla.h"

typedef struct tagFindOptions
{
	CString FindText;
	bool MatchWholeWord;
	bool MatchCase;
	bool UseRegExp;
	bool SearchAll;
	bool Direction;		// true is down.
	bool Loop;
	bool Found;
	bool UseSlashes;
} SFindOptions;

typedef struct tagSearchOptions : tagFindOptions
{
	// Replace
	CString ReplaceText;
	bool	InSelection;

	// Find In Files
	CString	FileExts;
	CString Path;
	bool Recurse;
	bool IncludeHidden;
} BasicSearchOptions;

typedef BasicSearchOptions SReplaceOptions;

/**
 * Safe external version of SearchOptions
 */
class SearchOptions : public BasicSearchOptions, public extensions::ISearchOptions
{
public:
	// Basic Options:
	virtual const char* GetFindText() const;
	virtual void SetFindText(const char* findText);
	
	virtual bool GetMatchWholeWord() const;
	virtual void SetMatchWholeWord(bool matchWholeWord);
	
	virtual bool GetMatchCase() const;
	virtual void SetMatchCase(bool matchCase);
	
	virtual bool GetUseRegExp() const;
	virtual void SetUseRegExp(bool useRegExp);
	
	//bool SearchAll;
	
	virtual bool GetSearchBackwards() const;
	virtual void SetSearchBackwards(bool backwards);
	
	virtual bool GetLoopOK() const;
	virtual void SetLoopOK(bool loop);
	
	virtual bool GetUseSlashes() const;
	virtual void SetUseSlashes(bool slashes);

	// Replace Options:
	virtual const char* GetReplaceText() const;
	virtual void SetReplaceText(const char* text);
	
	virtual bool GetReplaceInSelection() const;
	virtual void SetReplaceInSelection(bool inSelection);

	// Find In Files Options:
	virtual const char* GetFileExts() const;
	virtual void SetFileExts(const char* extensions);
	
	virtual const char* GetSearchPath() const;
	virtual void SetSearchPath(const char* path);
	
	virtual bool GetRecurse() const;
	virtual void SetRecurse(bool recurse);

	virtual bool GetIncludeHidden() const;
	virtual void SetIncludeHidden(bool hidden);
	
	// Result:
	virtual bool GetFound() const;
};

typedef struct tagPrintOptions
{
	// Cached object references...
	HGLOBAL hDevMode;
	HGLOBAL hDevNames;
	
	// Print margins...
	RECT	rcMargins;

	// Header and Footer text...
	CString Header;
	CString Footer;

	// Filename
	CString Filename;

} SPrintOptions;

#define TOOL_CAPTURE		0x001
#define TOOL_ISFILTER		0x002
#define TOOL_SAVEALL		0x004
#define TOOL_GLOBALOUTPUT	0x008
#define TOOL_CUSTOMPARSER	0x010
#define TOOL_CLEAROUTPUT	0x020
#define TOOL_SAVEONE		0x040
#define TOOL_REVERSESLASHES	0x080
#define TOOL_ISPROJECTTOOL	0x100
#define TOOL_ISTEXTFILTER	0x200
#define TOOL_WANTSTDIN		0x400

/**
 * @brief Defines a single external tool.
 */
class ToolDefinition
{
public:
	ToolDefinition()
	{
		CommandID = 0;
		iFlags = 0;
		Shortcut = 0;
		Index = 0;
	}

	ToolDefinition(const ToolDefinition& copy)
	{
		_copy(copy);
	}

	void SetFlags(unsigned long flags)
	{
		iFlags = flags;
	}

	unsigned long GetFlags() const
	{
		return iFlags;
	}

	tstring	Name;
	tstring Command;
	tstring	Folder;
	tstring	Params;
	WORD	Shortcut;
	tstring CustomParsePattern;
	int		CommandID;
	int		Index;

	bool CaptureOutput() const { return (iFlags & TOOL_CAPTURE) != 0; }
	bool IsFilter() const { return (iFlags & TOOL_ISFILTER) != 0; }
	bool SaveAll() const { return (iFlags & TOOL_SAVEALL) != 0; }
	bool SaveOne() const { return (iFlags & TOOL_SAVEONE) != 0; }
	bool GlobalOutput() const { return (iFlags & TOOL_GLOBALOUTPUT) != 0; }
	bool UseCustomParser() const { return (iFlags & TOOL_CUSTOMPARSER) != 0; }
	bool ShouldClearOutput() const { return (iFlags & TOOL_CLEAROUTPUT) != 0; }
	bool ShouldUseForwardSlashes() const { return (iFlags & TOOL_REVERSESLASHES) != 0; }
	bool IsProjectTool() const { return (iFlags & TOOL_ISPROJECTTOOL) != 0; }
	bool WantStdIn() const { return (iFlags & TOOL_WANTSTDIN) != 0; }
	bool IsTextFilter() const { return (iFlags & TOOL_ISTEXTFILTER) != 0; }

protected:
	void _copy(const ToolDefinition& copy)
	{
		Name = copy.Name;
		Command = copy.Command;
		Folder = copy.Folder;
		Params = copy.Params;
		Shortcut = copy.Shortcut;
		CommandID = copy.CommandID;
		CustomParsePattern = copy.CustomParsePattern;
		iFlags = copy.iFlags;
		Index = copy.Index;
	}

private:
	unsigned long iFlags;
};

typedef enum { PNSF_Windows = SC_EOL_CRLF, PNSF_Mac = SC_EOL_CR, PNSF_Unix = SC_EOL_LF, PNSF_NoChange} EPNSaveFormat;

typedef enum {
	eUnknown,
	eUtf16BigEndian,
	eUtf16LittleEndian,  // Default on Windows
	eUtf8,
	eUtf8NoBOM,
	eLast
} EPNEncoding;

static const unsigned char BOMLengthLookup [eLast] = {
	0,
	2,
	2,
	3,
	0
};

typedef enum {
	eOpenAgain,
	eWarnOpen,
	eSwitch
} EAlreadyOpenAction;

/*
On Windows, code page can be set to 932 (Japanese Shift-JIS), 
936 (Simplified Chinese GBK), 949 (Korean Unified Hangul Code), 
950 (Traditional Chinese Big5), or 1361 (Korean Johab) although 
these may require installation of language specific support.
*/
typedef enum {
	PNCP_Default = 0,
	PNCP_Unicode = SC_CP_UTF8,
	PNCP_ShiftJIS = 932,
	PNCP_ChineseGBK = 936,
	PNCP_KoreanHangul = 949,
	PNCP_ChineseBig5 = 950,
	PNCP_KoreanJohab = 1361
} ECodePage;

typedef enum { eftFind, eftReplace, eftFindInFiles, eftInvalid } EFindDialogType;

typedef enum {FN_FULL, FN_FILE, FN_PATH, FN_FILEPART} EGFNType;

typedef enum { eacManual, eacTextMatch } EACMode;

#endif