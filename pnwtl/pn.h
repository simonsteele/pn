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

#include "pntypes.h"
#include "xmlparser.h"

#define PN_NOTIFY (WM_USER+37)

struct IMainFrame
{
	virtual CWindow* GetWindow() = 0;
	virtual void AddMRUEntry(LPCTSTR lpszFile) = 0;
	virtual void SetActiveScheme(HWND notifier, LPVOID pScheme) = 0;
};

struct _Context 
{
	IMainFrame *m_frame;
};

extern __declspec( thread ) _Context g_Context;

HWND GetCurrentEditor();