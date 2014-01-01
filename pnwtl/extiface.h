/**
 * @file extiface.h
 * @brief PN Extensions Interface
 * @author Simon Steele
 * @note Copyright (c) 2006-2014 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * @namespace extensions 
 * @brief The PN 2 Extensions Interface
 * 
 * To make an extension:
 * 
 * 1. Create a DLL using C++
 * 2. Export functions that look like this:
 * 
 * bool __stdcall pn_init_extension(int iface_version, IPN* pn);
 *  - return false if your iface_version does not match and you will
 *    be safely unloaded.
 *  - The IPN instance given to you is your gateway to the rest of PN.
 *  - @see init_pn_extension
 *
 * void __stdcall pn_exit_extension();
 *  - Unhook all of your event sinks, you're being unloaded.
 *  - @see exit_pn_extension
 *
 * void __stdcall get_extension_info(PN::BaseString& name, PN::BaseString& version);
 *  - Set name and version to strings representing your plugin name and version!
 */

#ifndef EXTIFACE_H__INCLUDED_670F47C6_1FF6_4605_9F74_6EC70FD85C26
#define EXTIFACE_H__INCLUDED_670F47C6_1FF6_4605_9F74_6EC70FD85C26

#ifndef PNASSERT
	#ifndef _DEBUG
		#define PNASSERT(expr) ((void)0)
	#else
		#define PNASSERT(expr) assert(expr)
	#endif
#endif

#include "IOptions.h"

// TODO: See if we can abstract the string interface further and avoid
// exposing the allocator code
#include "allocator.h"
#include "string.h"

/// Find Next Result Enum
typedef enum {fnNotFound, fnFound, fnReachedStart, fnInvalidRegex, fnInvalidSearch} FindNextResult;

/// Search Type Enum
typedef enum {stFindNext, stReplace, stReplaceAll} SearchType;

namespace extensions
{

#define PN_EXT_IFACE_VERSION	12

/////////////////////////////////////////////////////////////////////////////
// Predeclare types
//-------------------------------------------------------------------------
class IPN;
class IDocument;
class IAppEventSink;
class IDocumentEventSink;
class ITextEditorEventSink;
class IScriptRegistry;
class ITextOutput;
class ISearchOptions;
class ITagSource;
class IMenuItems;
class IRecorder;
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// Smart Pointers
//-------------------------------------------------------------------------
typedef boost::shared_ptr<IDocument> IDocumentPtr;
typedef boost::shared_ptr<IDocumentEventSink> IDocumentEventSinkPtr;
typedef boost::shared_ptr<ITextEditorEventSink> ITextEditorEventSinkPtr;
typedef boost::shared_ptr<IAppEventSink> IAppEventSinkPtr;
typedef boost::shared_ptr<IRecorder> IRecorderPtr;
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// Classes
//-------------------------------------------------------------------------

/**
 * @brief Main Interface to PN
 * 
 * The main PN interface class. This is your interface to PN allowing
 * you access to the options system, script registry and most importantly
 * documents. It also allows you to register for events.
 */
class IPN
{
public:
	virtual ~IPN(){}

	/// Retrieve the version of the interface being used, 
	/// if this doesn't match you shouldn't do anything.
	virtual unsigned int GetIFaceVersion() const = 0;
	
	/// Get the version of PN loading the extension
	virtual const char* GetVersion() const = 0;
	
	/// Add an event sink for application events
	virtual void AddEventSink(IAppEventSinkPtr sink) = 0;
	/// Remove an event sink for application events
	virtual void RemoveEventSink(IAppEventSinkPtr sink) = 0;

	/// Get the script registry for registering built-in scripts
	virtual IScriptRegistry* GetScriptRegistry() = 0;
	
	/// Get the PN options manager, use this to load and save extension options
	virtual IOptions* GetOptionsManager() = 0;

	/// Get the current document
	virtual IDocumentPtr GetCurrentDocument() = 0;

	/// Get the output window
	virtual ITextOutput* GetGlobalOutputWindow() = 0;

	/// Get the main application window
	virtual HWND GetMainWindow() = 0;

	/// Get the users search options
	virtual ISearchOptions* GetUserSearchOptions() = 0;

	/// Start a find in files operation
	virtual void FindInFiles(ISearchOptions* options) = 0;

	/// Get some input from the user
	virtual wchar_t* InputBox(const wchar_t* title, const wchar_t* caption) = 0;

	// Create a new document
	virtual IDocumentPtr NewDocument(const char* scheme) = 0;

	/// Open a document
	virtual IDocumentPtr OpenDocument(const wchar_t* filepath, const char* scheme) = 0;

	/// Utility function to safely free strings given to you by PN
	virtual void ReleaseString(const wchar_t* str) = 0;

	/// Add a tag source (e.g. ctagsnavigator)
	virtual void AddTagSource(ITagSource* tagSource) = 0;

	/// Add plugin menu items
	virtual void AddPluginMenuItems(IMenuItems* menuItems) = 0;

	/// Add a recorder (currently sets the /only/ recorder)
	virtual void AddRecorder(IRecorderPtr recorder) = 0;
};

/**
 * @brief The Document Interface
 * 
 * The document interface. This allows you to control a given document,
 * get information about it and register for events.
 */
class IDocument
{
public:
	virtual ~IDocument(){}

	/// Get the title of this document
	virtual const wchar_t* GetTitle() const = 0;
	
	/// Get the filename of this document
	virtual const wchar_t* GetFileName() const = 0;
	
	/// Get the name of the scheme being used (the unique name, not the friendly name)
	virtual const char* GetCurrentScheme() const = 0;

	/// Get the handle of the Scintilla window (or NULL for non-text views)
	virtual HWND GetScintillaHWND() const = 0;

	/// Is the document modified in memory?
	virtual bool GetModified() const = 0;

	/// Is the document write protect?
	virtual bool GetWriteProtect() const = 0;

	/// Can this document be saved (does it have a valid filename yet?)
	virtual bool GetCanSave() const = 0;

	/// Save this file to the filename passed, optionally setting the UI filename
	virtual bool Save(const wchar_t* filename, bool setFilename) = 0;

	/// Send a message to Scintilla
	virtual LRESULT SendEditorMessage(UINT msg, WPARAM wParam, LPARAM lParam) = 0;
	/// Send a string format message to Scintilla
	virtual LRESULT SendEditorMessage(UINT msg, WPARAM wParam, const char* strParam) = 0;

	/// Check this document is valid.
	virtual bool IsValid() const = 0;

	/// Add a document event sink
	virtual void AddEventSink(IDocumentEventSinkPtr& sink) = 0;
	/// Remove a document event sink
	virtual void RemoveEventSink(IDocumentEventSinkPtr& sink) = 0;

	/// Add a text editor event sink
	virtual void AddEventSink(ITextEditorEventSinkPtr& sink) = 0;
	/// Remove a text editor event sink
	virtual void RemoveEventSink(ITextEditorEventSinkPtr& sink) = 0;

	/// Find Next
	virtual FindNextResult FindNext(ISearchOptions* options) = 0;

	/**
	 * Replace
	 * @remarks You should perform a FindNext first
	 */
	virtual bool Replace(ISearchOptions* options) = 0;

	/**
	 * Replace All
	 * @returns Replacement Count
	 */
	virtual int ReplaceAll(ISearchOptions* options) = 0;

	/**
	 * Close this document
	 */
	virtual void Close(bool dontAskUserIfUnsaved) = 0;

	/**
	 * Activate (focus) this document
	 */
	virtual void Activate() = 0;
};

/**
 * @brief Application Event Interface
 * 
 * Event sink interface for application events, register using @see IPN
 */
class IAppEventSink
{
public:
	virtual ~IAppEventSink(){}

	/// Called when a new document is opened/created
	virtual void OnNewDocument(IDocumentPtr& doc) = 0;
	
	/// Called when PN is closing (you are about to be unloaded!)
	virtual void OnAppClose() = 0;

	/// Called when the user switches to a different document
	virtual void OnDocSelected(IDocumentPtr& doc) = 0;

	/// Called when the very first Scintilla window is created, used for loading external lexers
	virtual void OnFirstEditorCreated(HWND hWndScintilla) = 0;
};

/**
 * @brief Text Editor Event Interface
 * 
 * Text editor event sink, see also @see IDocumentEventSink
 */
class ITextEditorEventSink
{
public:
	virtual ~ITextEditorEventSink(){}

	/// Called when a character is added to the document
    virtual void OnCharAdded(char c) = 0;
};

/**
 * @brief Document Event Interface
 * 
 * This event sink is used on a per-document basis to fire
 * events related to that document
 */
class IDocumentEventSink
{
public:
	virtual ~IDocumentEventSink(){}

	/// Called when the scheme changes
	virtual void OnSchemeChange(const char* scheme) = 0;
	
	/// Called when the document closes
	virtual void OnDocClosing() = 0;

	/// Called after a document is loaded
	virtual void OnAfterLoad() = 0;

	/// Called before the document is saved
	virtual void OnBeforeSave(const wchar_t* filename) = 0;

	/// Called after the document is saved
	virtual void OnAfterSave() = 0;

	/// Called when the document modified state changes
	virtual void OnModifiedChanged(bool modified) = 0;

	/// Called when the document write protect state changes
	virtual void OnWriteProtectChanged(bool writeProtect) = 0;
};

/**
 * Execution flags for IScriptRunner.
 * efCaptureOutput instructs Exec to return stdout rather than the Eval(x) of the script
 * efBuiltIn instructs Exec that the script to be run is part of the PN support libaries, for PyPN this means it's in glue.
 */
typedef enum { efCaptureOutput = 0x01, efBuiltIn = 0x02 } EExecFlags;

/**
 * @brief Script Runner Interface - for Script Engine Implementors
 * 
 * Interface for something that can run scripts from the 
 * script manager, this is how PN tells you to run a script whether
 * it be a file or a document.
 * 
 * It also provides methods allowing a scripting provider to let scripting
 * be used in places like parameter lists.
 */
class IScriptRunner
{
public:
	virtual ~IScriptRunner(){}

	/**
	 * This method requests that a runner runs a named
	 * script that it has previously registered with the
	 * registry.
	 */
	virtual void RunScript(const char* name) = 0;
	
	/**
	 * This method requests that a runner runs the text
	 * of a given document as a script.
	 */
	virtual void RunDocScript(IDocumentPtr& doc) = 0;

	/**
	 * Request a runner to attempt to run the script fragment provided,
	 * this is used for allowing scripting to be used in tool parameters
	 * etc.
	 */
	virtual void Eval(const char* script, PN::BaseString& output) = 0;

	/**
	 * Execute a method by name, with one optional parameter.
	 * @param flags Optional extra hints as to how to run the function.
	 */
	virtual void Exec(const char* function, const char* param, int flags, PN::BaseString& output) = 0;
};

/**
 * @brief The Script Registry
 * 
 * Interface for the script registry. The script registry maps scripts to
 * runners (@see IScriptRunner) using a unique identifier (e.g. "python", "tcl").
 */
class IScriptRegistry
{
public:
	virtual ~IScriptRegistry(){}

	/**
	 * Add a named script to the registry.
	 * @param group Name of a group to insert the script in
	 * @param name Friendly name for the script
	 * @param scriptref Reference for the script, in the form "runnerId:scriptId"
	 */
	virtual void Add(const char* group, const char* name, const char* scriptref) = 0;

	/**
	 * Register a script runner using a unique runner ID
	 */
	virtual void RegisterRunner(const char* id, extensions::IScriptRunner* runner) = 0;
	
	/**
	 * Remove a runner by ID
	 */
	virtual void RemoveRunner(const char* id) = 0;
	
	/**
	 * Get a runner for a given ID
	 */
	virtual extensions::IScriptRunner* GetRunner(const char* id) = 0;

	/**
	 * Enable scripts for a given scheme id. The runner id is used by PN to
	 * find the right runner to run the script.
	 *
	 * e.g. EnableSchemeScripts("python", "python") means that PN allows the
	 * user to set python files as scripts at run-time.
	 */
	virtual void EnableSchemeScripts(const char* scheme, const char* runnerId) = 0;
};

/**
 * @brief Interface to output windows
 * 
 * Interface implemented by something that can show some text output,
 * like the Output window.
 */
class ITextOutput
{
public:
	virtual ~ITextOutput(){}

	/// Add some text to the window, @param nLength to use a fixed length or -1 to calculate, @param output The text to output
	virtual void AddToolOutput(const wchar_t* output, int nLength = -1) = 0;
	/// Set the base directory for messages being placed in the output window (for error matching)
	virtual void SetToolBasePath(const wchar_t* path) = 0;
	/// Set the error parser for the window, optionally specifying a regular expression
	virtual void SetToolParser(bool bBuiltIn, const char* customExpression = NULL) = 0;
	/// Clear the output window
	virtual void ClearOutput() = 0;
	/// Show the output window
	virtual void ShowOutput() = 0;
	/// Hide the output window
	virtual void HideOutput() = 0;
};

typedef enum { fifPath, fifSingleFile, fifOpenFiles, fifActiveProjectFiles } EFIFFileSet;
typedef enum { elwCurrentDoc, elwAllDocs, elwCurrentProj, elwSelection } EFindWhere;

/**
 * @brief Interface to search options
 */
class ISearchOptions
{
public:
	virtual ~ISearchOptions(){}

	// Basic Options:
	virtual const wchar_t* GetFindText() const = 0;
	virtual void SetFindText(const wchar_t* findText) = 0;
	
	virtual bool GetMatchWholeWord() const = 0;
	virtual void SetMatchWholeWord(bool matchWholeWord) = 0;
	
	virtual bool GetMatchCase() const = 0;
	virtual void SetMatchCase(bool matchCase) = 0;
	
	virtual bool GetUseRegExp() const = 0;
	virtual void SetUseRegExp(bool useRegExp) = 0;
	
	virtual EFindWhere GetFindTarget() const = 0;
	virtual void SetFindTarget(EFindWhere target) = 0;
	
	virtual bool GetSearchBackwards() const = 0;
	virtual void SetSearchBackwards(bool backwards) = 0;
	
	virtual bool GetLoopOK() const = 0;
	virtual void SetLoopOK(bool loop) = 0;
	
	virtual bool GetUseSlashes() const = 0;
	virtual void SetUseSlashes(bool slashes) = 0;

	virtual bool GetNoCursorMove() const = 0;
	virtual void SetNoCursorMove(bool reposition) = 0;

	// Replace Options:
	virtual const wchar_t* GetReplaceText() const = 0;
	virtual void SetReplaceText(const wchar_t* text) = 0;
	
	virtual bool GetReplaceInSelection() const = 0;
	virtual void SetReplaceInSelection(bool inSelection) = 0;

	// Find In Files Options:
	virtual const wchar_t* GetFileExts() const = 0;
	virtual void SetFileExts(const wchar_t* extensions) = 0;
	
	virtual const wchar_t* GetSearchPath() const = 0;
	virtual void SetSearchPath(const wchar_t* path) = 0;
	
	virtual bool GetRecurse() const = 0;
	virtual void SetRecurse(bool recurse) = 0;

	virtual bool GetIncludeHidden() const = 0;
	virtual void SetIncludeHidden(bool hidden) = 0;

	virtual EFIFFileSet GetFileSet() const = 0;
	virtual void SetFileSet(EFIFFileSet fileSet) = 0;
	
	// Result:
	virtual bool GetFound() const = 0;
	virtual void SetFound(bool found) = 0;
};

typedef enum {miItem, miSubmenu} EMenuItemType;

typedef _W64 unsigned long cookie_t;

typedef struct tagMenuItem
{
	/**
	 * Set this to miSubmenu if the SubItems pointer is valid, otherwise use miItem.
	 */
	EMenuItemType Type;
	
	/**
	 * Title of your menu item, Programmer's Notepad will copy this.
	 */
	wchar_t* Title;
	
	/**
	 * User data to be passed back to your command handler. Ignored for miSubmenu.
	 */
	cookie_t UserData;
	
	/**
	 * Handler function to be called when your command is executed. Ignored for miSubmenu.
	 */
	boost::function<void (cookie_t cookie)> Handler;
	
	/**
	 * Pointer to subitems, only used when Type == miSubmenu
	 */
	IMenuItems* SubItems;
} MenuItem;

/**
 * Interface to be implemented by a menu item provider.
 */
class IMenuItems
{
public:
	virtual ~IMenuItems(){};

	/**
	 * Get the number of MenuItem instances that can be retrieved.
	 */
	virtual int GetItemCount() const = 0;

	/**
	 * Get an individual MenuItem instance by index.
	 */
	virtual const MenuItem& GetItem(int index) const = 0;
};

/**
 * Interface to be implemented to provide script/macro recording.
 */
class IRecorder
{
public:
	virtual ~IRecorder(){};

	/**
	 * Called when Scintilla reports an action, these will be withheld during any operation
	 * that this interface has a stronger contract for (e.g. find/replace).
	 */
	virtual void RecordScintillaAction(int message, WPARAM wParam, LPARAM lParam) = 0;

	/**
	 * Called when a search action occurs, such as Find Next, Replace, Replace All
	 */
	virtual void RecordSearchAction(SearchType type, const ISearchOptions* options, FindNextResult result) = 0;

	/**
	 * Called to start the record process.
	 */
	virtual void StartRecording() = 0;

	/**
	 * Called to stop the record process.
	 */
	virtual void StopRecording() = 0;
};

/**
 * @brief Plugin Initialisation Function
 * 
 * Implement and export a function with this prototype called "init_pn_extension" to 
 * have PN recognise your DLL as a plugin and load it successfully.
 *
 * @param iface_version The interface version PN was compiled with, if this does not
 * match your version you should return false and refuse to load.
 * @param pn The pointer to an IPN instance giving you access to PN to do your stuff!
 */
typedef bool (__stdcall *pn_ext_init_fn)(int iface_version, IPN* pn);

/**
 * @brief Plugin Information Function
 * 
 * Implement and export a function with this prototype called "pn_get_extension_info" to 
 * have PN read display information from your plugin for use in the options dialog
 *
 * @param name Write the name of your plugin into this object
 * @param version Write the version of your plugin into this object
 */
typedef void (__stdcall *pn_ext_info_fn)(PN::BaseString& name, PN::BaseString& version);

/**
 * @brief Plugin Unload Function
 *
 * Called by PN when it is about to unload your plugin.
 */
typedef void (__stdcall *pn_ext_exit_fn)();

} // namespace extensions

#endif