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

#define PN_INITIALISEFRAME	(WM_USER+35)
#define PN_NOTIFY			(WM_USER+36)
#define PN_CHECKAGE			(WM_USER+37)
#define PN_OPTIONSUPDATED	(WM_USER+38)
#define PN_TOGGLEOUTPUT		(WM_USER+39)
#define PN_TOOLRUNUPDATE	(WM_USER+40)
#define PN_SCHEMECHANGED	(WM_USER+41)
#define PN_HANDLEHSCLICK	(WM_USER+42)
#define PN_ESCAPEPRESSED	(WM_USER+43)

#define PN_MDIACTIVATE		0x1
#define TOOLS_RUNTOOL		0x2
#define PN_MDIDESTROY		0x3

#include "pntypes.h"

// Pre-declarations...

class ToolWrapper;
class Options;
namespace Projects {
	class Workspace;
}



struct IMainFrame
{
	virtual CWindow* GetWindow() = 0;
	virtual ToolWrapper* MakeGlobalOutputWrapper(ToolDefinition* pDefinition) = 0;
	virtual void AddMRUEntry(LPCTSTR lpszFile) = 0;
	virtual void SetActiveScheme(HWND notifier, LPVOID pScheme) = 0;
	virtual BOOL TrackPopupMenu(HMENU hMenu, UINT uFlags, int x, int y, LPTPMPARAMS lpParams = NULL, HWND hWndCaller = NULL) = 0;
	virtual void SetStatusText(LPCTSTR text, bool bLongLife = true) = 0;
	virtual bool CloseAll() = 0;
	virtual void SaveAll() = 0;
	virtual bool Open(LPCTSTR lpszFilename, bool bAddMRU = false) = 0;
	virtual bool CheckAlreadyOpen(LPCTSTR lpszFilename, EAlreadyOpenAction action) = 0;
	virtual Projects::Workspace* GetActiveWorkspace() = 0;
};

#include "pnutils.h"

struct _Context 
{
	IMainFrame				*m_frame;
	MultipleInstanceManager *m_miManager;
	Options					*options;
	OSVERSIONINFO			OSVersion;
};

/// This is the global application context.
extern /*__declspec( thread )*/ _Context g_Context;

HWND GetCurrentEditor();

/// This function is used to show that something odd and unexpected has happened.
void pn__Unexpected(LPCTSTR file, int line, LPCTSTR message);

// Utility Classes and Definitions:
#include "pntabs.h"
#include "xmlparser.h"
#include "ssmenus.h"

#include "optionsmanager.h"

#include "ScintillaWTL.h"

#include "schemes.h"

#define OPTIONS \
	g_Context.options

//#if defined(DEBUG_)
	#define UNEXPECTED(message) \
	{ \
		pn__Unexpected(__FILE__, __LINE__, message); \
	}

	#define RETURN_UNEXPECTED(message, ret) \
	{ \
		pn__Unexpected(__FILE__, __LINE__, message); \
		return ret; \
	}
/*#else
	#define UNEXPECTED(message) ;
	#define RETURN_UNEXPECTED(message, ret) return ret;
#endif*/