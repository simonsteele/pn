/**
 * @file pn.h
 * @brief Main Header File for Programmers Notepad 2, defines the application level services.
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * Thanks to the author of kPad for the g_Context here!
 */

#define PN_INITIALISEFRAME	(WM_APP+1)
#define PN_NOTIFY			(WM_APP+2)
#define PN_CHECKAGE			(WM_APP+3)
#define PN_OPTIONSUPDATED	(WM_APP+4)
#define PN_TOGGLEOUTPUT		(WM_APP+5)
#define PN_TOOLRUNUPDATE	(WM_APP+6)
#define PN_SCHEMECHANGED	(WM_APP+7)
#define PN_HANDLEHSCLICK	(WM_APP+8)
#define PN_ESCAPEPRESSED	(WM_APP+9)
#define PN_UPDATEFINDTEXT	(WM_APP+10)
#define PN_PROJECTNOTIFY	(WM_APP+11)
#define PN_FIFMATCH			(WM_APP+12)
#define PN_GOTOLINE			(WM_APP+13)
#define PN_GETMDICLIENTRECT (WM_APP+14)
#define PN_MDISETMENU		(WM_APP+15)
#define PN_REFRESHUPDATEUI  (WM_APP+16)
#define PN_UPDATECHILDUI	(WM_APP+17)
#define PN_CLOSEALLOTHER	(WM_APP+18)
#define PN_OVERWRITETARGET	(WM_APP+19)
#define PN_INSERTCLIP		(WM_APP+20)
#define PN_SETFOCUS			(WM_APP+21)
#define	PN_COMPLETECLIP		(WM_APP+22)
#define PN_INSERTCLIPTEXT   (WM_APP+23)
#define PN_SETSCHEME		(WM_APP+24)

// Command IDs used around the place...
#define PN_MDIACTIVATE		0x1
#define TOOLS_RUNTOOL		0x2
#define PN_MDIDESTROY		0x3
#define COMMANDS_RUNEXT		0x4
#define PN_UPDATEDISPLAY	0x5
#define PN_UPDATEAVAILABLE	0x6
#define PN_COMMAND_EDITOR   0x7
#define PN_COMMAND_PLUGIN	0x8

#define PNID_SAVEAS			14
#define PNID_OVERWRITE		15

#define PNID_DONTASKUSER	253

#ifdef _UNICODE
	#define WIDEN2(x) L ## x
	#define WIDEN(x) WIDEN2(x)
	#define __WFILE__ WIDEN(__FILE__)
	#define __TFILE__ __WFILE__
#else
	#define __TFILE__ __FILE__
#endif


//#if defined(DEBUG_)
	#define UNEXPECTED(message) \
	{ \
		pn__Unexpected(__TFILE__, __LINE__, message); \
	}

	#define RETURN_UNEXPECTED(message, ret) \
	{ \
		pn__Unexpected(__TFILE__, __LINE__, message); \
		return ret; \
	}
/*#else
	#define UNEXPECTED(message) ;
	#define RETURN_UNEXPECTED(message, ret) return ret;
#endif*/

#define LOG(message) \
	::OutputDebugString(message)

#include "allocator.h"
#include "pnextstring.h"
#include "extiface.h"
#include "third_party/scintilla/include/Platform.h"
#include "pntypes.h"

// Pre-declarations...
class App;
class ToolWrapper;
class Options;
class MultipleInstanceManager;
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

	// Scripts
	virtual void RecordingStopped() = 0;
};

struct _Context 
{
	IMainFrame				*m_frame;
	MultipleInstanceManager *m_miManager;
	Options					*options;
	OSVERSIONINFO			OSVersion;
	App						*ExtApp;
};

/// This is the global application context.
extern _Context g_Context;

HWND GetCurrentEditor();

/// This function is used to show that something odd and unexpected has happened.
void pn__Unexpected(LPCTSTR file, int line, LPCTSTR message);

// Utility Classes and Definitions:
#include "pntabs.h"
#include "xmlparser.h"
#include "commands.h"
#include "ssmenus.h"
#include "ifilesource.h"

#include "optionsmanager.h"

#include "ScintillaWTL.h"

#include "schemes.h"
#include "schememanager.h"

#include "files.h"
#include "filename.h"

#include "pntaskdialog.h"

#define OPTIONS g_Context.options