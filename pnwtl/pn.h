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

#define PN_NOTIFY (WM_USER+37)

#define TOOLS_RUNTOOL	0x2

#include "resource.h"

struct IMainFrame
{
	virtual CWindow* GetWindow() = 0;
	virtual void AddMRUEntry(LPCTSTR lpszFile) = 0;
	virtual void SetActiveScheme(HWND notifier, LPVOID pScheme) = 0;
	virtual BOOL TrackPopupMenu(HMENU hMenu, UINT uFlags, int x, int y, LPTPMPARAMS lpParams = NULL) = 0;
	virtual void SetStatusText(LPCTSTR text) = 0;
	virtual void SaveAll() = 0;
};

struct _Context 
{
	IMainFrame *m_frame;
};

extern __declspec( thread ) _Context g_Context;

HWND GetCurrentEditor();

// Utility Classes and Definitions:
#include "pntypes.h"
#include "pntabs.h"
#include "xmlparser.h"
#include "ssmenus.h"

#include "optionsmanager.h"

#include "ScintillaWTL.h"

#include "schemes.h"

#include "pnutils.h"
#include "pndialogs.h"
#include "textview.h"
#include "ChildFrm.h"
#include "finddlg.h"