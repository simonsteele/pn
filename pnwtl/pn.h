/**
 * @file pn.h
 * @brief Main Header File for Programmers Notepad 2, defines the application level services.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * Thanks to the author of kPad for the g_Context here!
 */

//#define PN_INITIALISEFRAME	(WM_USER+35)
#define PN_NOTIFY			(WM_USER+36)
#define PN_CHECKAGE			(WM_USER+37)
#define PN_OPTIONSUPDATED	(WM_USER+38)
#define PN_TOGGLEOUTPUT		(WM_USER+39)
#define PN_TOOLFINISHED		(WM_USER+40)
#define PN_SCHEMECHANGED	(WM_USER+41)
#define PN_HANDLEHSCLICK	(WM_USER+42)
#define PN_ESCAPEPRESSED	(WM_USER+43)

#define PN_MDIACTIVATE		0x1
#define TOOLS_RUNTOOL		0x2

#include "resource.h"

#include "pntypes.h"

class ToolWrapper;

struct IMainFrame
{
	virtual CWindow* GetWindow() = 0;
	virtual ToolWrapper* MakeGlobalOutputWrapper(ToolDefinition* pDefinition) = 0;
	virtual void AddMRUEntry(LPCTSTR lpszFile) = 0;
	virtual void SetActiveScheme(HWND notifier, LPVOID pScheme) = 0;
	virtual BOOL TrackPopupMenu(HMENU hMenu, UINT uFlags, int x, int y, LPTPMPARAMS lpParams = NULL, HWND hWndCaller = NULL) = 0;
	virtual void SetStatusText(LPCTSTR text) = 0;
	virtual void SaveAll() = 0;
	virtual void OpenFile(LPCTSTR lpszFilename) = 0;
	virtual bool CheckAlreadyOpen(LPCTSTR lpszFilename, EAlreadyOpenAction action) = 0;
};

struct _Context 
{
	IMainFrame *m_frame;
};

extern __declspec( thread ) _Context g_Context;

HWND GetCurrentEditor();

// Utility Classes and Definitions:
#include "pntabs.h"
#include "pndocking.h"
#include "xmlparser.h"
#include "ssmenus.h"

#include "optionsmanager.h"

#include "ScintillaWTL.h"

#include "schemes.h"

#include "pnutils.h"
#include "pndialogs.h"