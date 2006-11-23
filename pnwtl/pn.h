/**
 * @file pn.h
 * @brief Main Header File for Programmers Notepad 2, defines the application level services.
 * @author Simon Steele
 * @note Copyright (c) 2002-2006 Simon Steele <s.steele@pnotepad.org>
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
#define PN_UPDATEFINDTEXT	(WM_USER+44)
#define PN_PROJECTNOTIFY	(WM_USER+45)
#define PN_FIFMATCH			(WM_USER+46)
#define PN_GOTOLINE			(WM_USER+47)
#define PN_GETMDICLIENTRECT (WM_USER+48)

// Command IDs used around the place...
#define PN_MDIACTIVATE		0x1
#define TOOLS_RUNTOOL		0x2
#define PN_MDIDESTROY		0x3
#define COMMANDS_RUNEXT		0x4

#define PNID_SAVEAS			14
#define PNID_OVERWRITE		15

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

#define LOG(message) \
	::OutputDebugString(message)

#include "extiface.h"
#include "pntypes.h"

// Pre-declarations...

class ToolWrapper;
class Options;
namespace Projects {
	class Workspace;
}

typedef enum {
	PNDW_OUTPUT = 0,
	PNDW_PROJECTS = 1,
	PNDW_TEXTCLIPS = 2,
	PNDW_CTAGS = 3,
	PNDW_FINDRESULTS = 4,
} EDockingWindow;

#include "pnutils.h"
#include "pnstrings.h"
#include "include/singleton.h"
#include "l10n.h"

#include "Document.h"
typedef boost::shared_ptr<Document> DocumentPtr;
typedef std::list< DocumentPtr > DocumentList;

struct IMainFrame
{
	// Window Accessors
	virtual CWindow* GetWindow() = 0;
	
	// Global UI
	virtual void AddMRUEntry(LPCTSTR lpszFile) = 0;
	virtual void SetStatusText(LPCTSTR text, bool bLongLife = true) = 0;
	virtual BOOL TrackPopupMenu(HMENU hMenu, UINT uFlags, int x, int y, LPTPMPARAMS lpParams = NULL, HWND hWndCaller = NULL) = 0;
	virtual void ToggleDockingWindow(EDockingWindow window, bool bSetValue = false, bool bShowing = true) = 0;
	
	// Document Operations
	virtual bool CloseAll() = 0;
	virtual bool SaveAll(bool ask = false) = 0;
	virtual bool Open(LPCTSTR lpszFilename, bool bAddMRU = false) = 0;
	virtual void OpenProject(LPCTSTR lpszFilename, bool intoExistingGroup = false) = 0;
	virtual void OpenProjectGroup(LPCTSTR lpszFilename) = 0;
	virtual bool CheckAlreadyOpen(LPCTSTR lpszFilename, EAlreadyOpenAction action) = 0;
	virtual void SetActiveScheme(HWND notifier, LPVOID pScheme) = 0;

	// Document Management Operations
	virtual void GetOpenDocuments(DocumentList& list) = 0;
	virtual void GetOpenWorkspaceDocuments(DocumentList& list) = 0;

	// Projects
	virtual Projects::Workspace* GetActiveWorkspace() = 0;
	virtual HWND GetJumpViewHandle() = 0;
	
	// Tools/Output
	virtual extensions::ITextOutput* GetGlobalOutputWindow() = 0;
	virtual ToolWrapper* MakeGlobalOutputWrapper(ToolDefinition* pDefinition) = 0;
	
	// Search
	virtual void FindInFiles(SearchOptions* options) = 0;
};

static void LogWndPos(LPCTSTR codeLoc, HWND hWnd)
{
	WINDOWPLACEMENT wp;
    wp.length = sizeof(WINDOWPLACEMENT);
    bool bRes=false;
    if (::GetWindowPlacement(hWnd,&wp))
    {
		TCHAR buf[2000];
		_stprintf(buf, _T("%s: %dx%d at %dx%d\n"), 
			codeLoc,
			wp.rcNormalPosition.right - wp.rcNormalPosition.left,
			wp.rcNormalPosition.bottom - wp.rcNormalPosition.top,
			wp.rcNormalPosition.left,
			wp.rcNormalPosition.top);
		LOG(buf);
	}
}

static void LogWndPos(HWND hWnd)
{
	LogWndPos(_T("Unknown"), hWnd);
}

class App;

struct _Context 
{
	IMainFrame				*m_frame;
	MultipleInstanceManager *m_miManager;
	Options					*options;
	OSVERSIONINFO			OSVersion;
	App						*ExtApp;
};

/// This is the global application context.
extern /*__declspec( thread )*/ _Context g_Context;

HWND GetCurrentEditor();

/// This function is used to show that something odd and unexpected has happened.
void pn__Unexpected(LPCTSTR file, int line, LPCTSTR message);

// Utility Classes and Definitions:
#include "pntabs.h"
#include "xmlparser.h"
#include "commands.h"
#include "ssmenus.h"

#include "optionsmanager.h"

#include "ScintillaWTL.h"

#include "schemes.h"
#include "schememanager.h"

#include "files.h"

#define OPTIONS \
	g_Context.options