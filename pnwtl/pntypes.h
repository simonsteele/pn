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

#include <string>
typedef std::basic_string<TCHAR> tstring;

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

/*typedef struct tagReplaceOptions : tagFindOptions
{
	CString ReplaceText;
	bool	InSelection;
} SReplaceOptions;

typedef struct tagFindInFilesOptions
{
	CString FindText;
	CString	FileExts;
	CString Path;
	bool MatchCase;
	bool Recurse;
} SFindInFilesOptions;*/

typedef struct tagSearchOptions : tagFindOptions
{
	// Replace
	CString ReplaceText;
	bool	InSelection;

	// Find In Files
	CString	FileExts;
	CString Path;
	//bool MatchCase;
	bool Recurse;
} SearchOptions;

typedef SearchOptions SReplaceOptions;

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

#define TOOL_CAPTURE		0x01
#define TOOL_ISFILTER		0x02
#define TOOL_SAVEALL		0x04
#define TOOL_GLOBALOUTPUT	0x08
#define TOOL_CUSTOMPARSER	0x10
#define TOOL_CLEAROUTPUT	0x20
#define TOOL_SAVEONE		0x40
#define TOOL_REVERSESLASHES	0x80

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

	tstring	Name;
	tstring Command;
	tstring	Folder;
	tstring	Params;
	WORD	Shortcut;
	tstring CustomParsePattern;
	int		CommandID;
	int		iFlags;
	int		Index;

	bool CaptureOutput() const { return (iFlags & TOOL_CAPTURE) != 0; }
	bool IsFilter() const { return (iFlags & TOOL_ISFILTER) != 0; }
	bool SaveAll() const { return (iFlags & TOOL_SAVEALL) != 0; }
	bool SaveOne() const { return (iFlags & TOOL_SAVEONE) != 0; }
	bool GlobalOutput() const { return (iFlags & TOOL_GLOBALOUTPUT) != 0; }
	bool UseCustomParser() const { return (iFlags & TOOL_CUSTOMPARSER) != 0; }
	bool ShouldClearOutput() const { return (iFlags & TOOL_CLEAROUTPUT) != 0; }
	bool ShouldUseForwardSlashes() const { return (iFlags & TOOL_REVERSESLASHES) != 0; }

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

};

/**
 * @brief This defines a simple interface used to send tool output to a window.
 */
class IToolOutputSink
{
public:
	virtual void _AddToolOutput(LPCTSTR output, int nLength = -1) = 0;
	virtual void SetToolBasePath(LPCTSTR path) = 0;
	virtual void SetToolParser(bool bBuiltIn, LPCTSTR customExpression = NULL) = 0;
};

typedef enum { PNSF_Windows = SC_EOL_CRLF, PNSF_Mac = SC_EOL_CR, PNSF_Unix = SC_EOL_LF, PNSF_NoChange} EPNSaveFormat;

typedef enum {
	eUnknown,
	eUtf16BigEndian,
	eUtf16LittleEndian,  // Default on Windows
	eUtf8,
	eLast
} EPNEncoding;

typedef enum {
	eOpenAgain,
	eWarnOpen,
	eSwitch
} EAlreadyOpenAction;

typedef enum {
	PNCP_Default = 0,
	PNCP_Unicode = SC_CP_UTF8,
	PNCP_ShiftJIS = 932
} ECodePage;

#endif