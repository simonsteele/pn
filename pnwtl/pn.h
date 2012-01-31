/**
 * @file pn.h
 * @brief Main Header File for Programmers Notepad 2, defines the application level services.
 * @author Simon Steele
 * @note Copyright (c) 2002-2012 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef PN_H_INCLUDED
#define PN_H_INCLUDED

#if PLAT_WIN

#include "../libpeanut/libpeanut/win/core/defs.h"

#endif 

#include "allocator.h"
#include "pnextstring.h"
#include "extiface.h"
#include "third_party/scintilla/include/Platform.h"
#include "pntypes.h"

#define PN_MDIACTIVATE          0x1
#define TOOLS_RUNTOOL           0x2
#define PN_MDIDESTROY           0x3
#define COMMANDS_RUNEXT         0x4
#define PN_UPDATEDISPLAY        0x5
#define PN_UPDATEAVAILABLE      0x6
#define PN_COMMAND_EDITOR       0x7
#define PN_COMMAND_PLUGIN       0x8

#define PNID_SAVEAS             14
#define PNID_OVERWRITE          15

#define PNID_DONTASKUSER        253

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

#include "pnstrings.h"
#include "include/singleton.h"
#include "l10n.h"

#include "Document.h"
typedef boost::shared_ptr<Document> DocumentPtr;
typedef std::list< DocumentPtr > DocumentList;
class Scheme;

class IEditorFactory
{
public:
    virtual ~IEditorFactory() {}
    /// Create a new editor with this scheme:
    virtual IEditorFrame* WithScheme(Scheme* scheme) = 0;
    virtual IEditorFrame* Default() = 0;
    virtual IEditorFrame* FromFile(LPCTSTR pathname, Scheme* pScheme, 
                                   EPNEncoding encoding, bool& bOpened) = 0;
};

class IMainFrame
{
public:
	// Window Accessors
#if PLAT_WIN
	virtual CWindow* GetWindow() = 0;
#endif
	
	// Global UI
	virtual void AddMRUEntry(LPCTSTR lpszFile) = 0;
	virtual void SetStatusText(LPCTSTR text, bool bLongLife = true) = 0;
    
#if PLAT_WIN
	virtual BOOL TrackPopupMenu(HMENU hMenu, UINT uFlags, int x, int y, LPTPMPARAMS lpParams = NULL, HWND hWndCaller = NULL) = 0;
	virtual void ToggleDockingWindow(EDockingWindow window, bool bSetValue = false, bool bShowing = true) = 0;
#endif
    
    virtual IEditorFactory& GetFactory() const = 0;
    
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
#if PLAT_WIN
	OSVERSIONINFO			OSVersion;
#endif
	App						*ExtApp;
};

/// This is the global application context.
extern _Context g_Context;

HWND GetCurrentEditor();

/// This function is used to show that something odd and unexpected has happened.
void pn__Unexpected(LPCTSTR file, int line, LPCTSTR message);


namespace PN { namespace Platform {

    static tstring GetDefaultEditorFont()
    {
#if PLAT_WIN
        if (WTL::RunTimeHelper::IsVista())
        {
            return _T("Consolas");
        }
        else
        {
            return _T("Lucida Console");
        }
#else
        return _T("Menlo Regular");
#endif
    }
    
    /**
     * Set the clipboard contents.
     * @param data Null-terminated buffer containing text for the clipboard.
     * @param length Length of data including null.
     */
    static void SetClipboardContent(const char* data, int length)
    {
#if PLAT_WIN
        HGLOBAL hData = ::GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT, length+1);
        if( hData )
        {
            if( OpenClipboard(GetCurrentEditor()) )
            {
                EmptyClipboard();
                char* pBuf = static_cast<char*>(::GlobalLock(hData));
                memcpy(pBuf, data, length);
                ::GlobalUnlock(hData);
                ::SetClipboardData(CF_TEXT, hData);
                CloseClipboard();
            }
        }
#else
        throw "Unimplemented";
#endif
    }
    
    /**
     * Provide information about the current user.
     */
    class UserInformation
    {
    public:
        explicit UserInformation()
        {
            set();
        }
        
        tstring UserName;
        
    private:
        void set();
    };
    
    /**
     * Get the directory that PN is running from.
     */
    tstring GetExecutableDirectory();
    
    static bool IsDBCSLeadByte(int codePage, char ch)
    {
#if PLAT_WIN
        return ::IsDBCSLeadByteEx(codePage, ch) != 0;
#endif
        throw "Unimplemented";
    }
    
    /**
     * Provide system-locale formatted Date/Time.
     */
    class DateTimeInformation
    {
    public:
        explicit DateTimeInformation()
        {
            set();
        }
        
        tstring CurrentDate;
        tstring CurrentTime;
        
    private:
        void set();
    };
    
    /**
     * Provide User-Friendly File Information.
     */
    class FileInformation
    {
    public:
        explicit FileInformation(LPCTSTR filePath)
        {
            set(filePath);
        }
        
        tstring FileDate;
        tstring FileTime;
        tstring FileAttr;
        
    private:
        void set(LPCTSTR filePath);
    };
}} // PN::Platform

class OpTimeLogger
{
public:
    OpTimeLogger(LPCTSTR op) : m_op(op), m_startTime(::GetTickCount()) {}
    ~OpTimeLogger()
    {
        unsigned long durn = ::GetTickCount() - m_startTime;
        std::string log = boost::str(boost::format("Timer [%1%]: %2%ms") % m_op % durn);
        LOG(log.c_str());
    }
private:
    tstring m_op;
    unsigned long m_startTime;
};

// Windows-only UI bits:
#if PLAT_WIN
#include "pntabs.h"
#include "ScintillaWTL.h"
#include "pntaskdialog.h"
#include "ssmenus.h"
#include "pnutils.h"
#endif

// Utility Classes and Definitions:
#include "xmlparser.h"
#include "commands.h"
#include "ifilesource.h"

#include "optionsmanager.h"

#include "schemes.h"
#include "schememanager.h"

#include "files.h"
#include "filename.h"

#define OPTIONS g_Context.options

#endif //#ifndef PN_H_INCLUDED
