/**
 * @file pntypes.h
 * @brief Define structs etc. used throughout pn2.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
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

typedef struct tagReplaceOptions : tagFindOptions
{
	CString ReplaceText;
	bool	InSelection;
} SReplaceOptions;

typedef struct tagPrintOptions
{
	HGLOBAL hDevMode;
	HGLOBAL hDevNames;
	RECT	rcMargins;
} SPrintOptions;

#define TOOL_CAPTURE		0x01
#define TOOL_ISFILTER		0x02
#define TOOL_SAVEALL		0x04
#define TOOL_GLOBALOUTPUT	0x08

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
	}

	ToolDefinition(const ToolDefinition& copy)
	{
		Name = copy.Name;
		Command = copy.Command;
		Folder = copy.Folder;
		Params = copy.Params;
		Shortcut = copy.Shortcut;
		CommandID = copy.CommandID;
		iFlags = copy.iFlags;
	}

	tstring	Name;
	tstring Command;
	tstring	Folder;
	tstring	Params;
	tstring Shortcut;
	int		CommandID;
	int		iFlags;

	bool CaptureOutput() const { return (iFlags & TOOL_CAPTURE) != 0; }
	bool IsFilter() const { return (iFlags & TOOL_ISFILTER) != 0; }
	bool SaveAll() const { return (iFlags & TOOL_SAVEALL) != 0; }
	bool GlobalOutput() const { return (iFlags & TOOL_GLOBALOUTPUT) != 0; }
};

/**
 * @brief This defines a simple interface used to send tool output to a window.
 */
class IToolOutputSink
{
public:
	virtual void AddToolOutput(LPCTSTR output, int nLength = -1) = 0;
};

typedef enum { PNSF_Windows = SC_EOL_CRLF, PNSF_Unix = SC_EOL_LF, PNSF_Mac = SC_EOL_CR, PNSF_NoChange} EPNSaveFormat;

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

#endif