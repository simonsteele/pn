/**
 * @file pntypes.h
 * @brief Define structs etc. used throughout pn2.
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef pntypes_h__included
#define pntypes_h__included

typedef unsigned __int64	uint64_t;
typedef __int64				int64_t;

#include "third_party/scintilla/include/scintilla.h"
#include "searchoptions.h"

typedef struct tagPrintOptions
{
	// Cached object references...
	HGLOBAL hDevMode;
	HGLOBAL hDevNames;
	
	// Print margins...
	RECT	rcMargins;

	// Header and Footer text...
	tstring Header;
	tstring Footer;

	// Filename
	tstring Filename;

} SPrintOptions;

#define TOOL_CAPTURE			0x001
#define TOOL_ISFILTER			0x002
#define TOOL_SAVEALL			0x004
#define TOOL_GLOBALOUTPUT		0x008
#define TOOL_CUSTOMPARSER		0x010
#define TOOL_CLEAROUTPUT		0x020
#define TOOL_SAVEONE			0x040
#define TOOL_REVERSESLASHES		0x080
#define TOOL_ISPROJECTTOOL		0x100
#define TOOL_ISTEXTFILTER		0x200
#define TOOL_WANTSTDIN			0x400
#define TOOL_SAVEPROJECTGROUP	0x800

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
	std::string CustomParsePattern;
	int		CommandID;
	int		Index;

	bool CaptureOutput() const { return (iFlags & TOOL_CAPTURE) != 0; }
	bool IsFilter() const { return (iFlags & TOOL_ISFILTER) != 0; }
	bool SaveAll() const { return (iFlags & TOOL_SAVEALL) != 0; }
	bool SaveOne() const { return (iFlags & TOOL_SAVEONE) != 0; }
	bool SaveProjectGroup() const { return (iFlags & TOOL_SAVEPROJECTGROUP) != 0; }
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

typedef enum { scfNone = 0, scfNoViewSettings = 1, scfNoRestyle = 2 } ESchemeChangeFlags;

#endif