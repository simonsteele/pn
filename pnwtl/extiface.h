/**
 * @file extiface.h
 * @brief PN Extensions Interface
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * To make an extension:
 * ---------------------
 * 1. Create a DLL using C++
 * 2. Export functions that look like this:
 * 
 * bool init_pn_extension(int iface_version, IPN* pn);
 *  - return false if your iface_version does not match and you will
 *    be safely unloaded.
 *  - The IPN instance given to you is your gateway to the rest of PN.
 *
 * void exit_pn_extension();
 *  - Unhook all of your event sinks, you're being unloaded.
 *
 * Maybe later: get_extension_info()...
 */

#ifndef extiface_h__included_670F47C6_1FF6_4605_9F74_6EC70FD85C26
#define extiface_h__included_670F47C6_1FF6_4605_9F74_6EC70FD85C26

#include "IOptions.h"

namespace extensions
{

#define PN_EXT_IFACE_VERSION	1

/////////////////////////////////////////////////////////////////////////////
// Predeclare types
//-------------------------------------------------------------------------
class IPN;
class IDocument;
class IAppEventSink;
class IDocumentEventSink;
class IScriptRegistry;
class ITextOutput;
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// Smart Pointers
//-------------------------------------------------------------------------
typedef boost::shared_ptr<IDocument> IDocumentPtr;
typedef boost::shared_ptr<IDocumentEventSink> IDocumentEventSinkPtr;
typedef boost::shared_ptr<IAppEventSink> IAppEventSinkPtr;
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// Classes
//-------------------------------------------------------------------------

/**
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

	/// Utility function to safely free strings given to you by PN
	virtual void ReleaseString(const TCHAR* str) = 0;
};

/**
 * The document interface. This allows you to control a given document,
 * get information about it and register for events.
 */
class IDocument
{
public:
	virtual ~IDocument(){}

	/// Get the title of this document
	virtual const char* GetTitle() const = 0;
	
	/// Get the filename of this document
	virtual const char* GetFileName() const = 0;
	
	/// Get the name of the scheme being used (the unique name, not the friendly name)
	virtual const char* GetCurrentScheme() const = 0;

	/// Get the handle of the Scintilla window (or NULL for non-text views)
	virtual HWND GetScintillaHWND() const = 0;

	/// Send a message to Scintilla
	virtual LRESULT SendEditorMessage(UINT msg, WPARAM wParam, LPARAM lParam) = 0;
	/// Send a string format message to Scintilla
	virtual LRESULT SendEditorMessage(UINT msg, WPARAM wParam, const char* strParam) = 0;

	/// Check this document is valid.
	virtual bool IsValid() const = 0;

	/// Add a document event sink
	virtual void AddEventSink(IDocumentEventSinkPtr sink) = 0;
	/// Remove a document event sink
	virtual void RemoveEventSink(IDocumentEventSinkPtr sink) = 0;
};

/**
 * Event sink interface for application events.
 */
class IAppEventSink
{
public:
	virtual ~IAppEventSink(){}

	/// Called when a new document is opened/created
	virtual void OnNewDocument(IDocumentPtr doc) = 0;
	/// Called when PN is closing (you are about to be unloaded!)
	virtual void OnAppClose() = 0;
};

/**
 * Text editor event sink
 */
class ITextEditorEventSink
{
public:
	virtual ~ITextEditorEventSink(){}

	/// Called when a character is added to the document
    virtual void OnCharAdded(char c) = 0;
};

/**
 * Document event sink
 */
class IDocumentEventSink : public ITextEditorEventSink
{
public:
	virtual ~IDocumentEventSink(){}

	/// Called when the scheme changes
	virtual void OnSchemeChange(const char* scheme) = 0;
	/// Called when the document closes
	virtual void OnDocClosing() = 0;
};

/**
 * Interface for something that can run scripts from the 
 * script manager
 */
class IScriptRunner
{
public:
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
	virtual void RunDocScript(IDocumentPtr doc) = 0;
};

/**
 * Interface for the script registry. The script registry maps scripts to
 * runners (@see IScriptRunner).
 */
class IScriptRegistry
{
public:
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
 * Interface implemented by something that can show some text output,
 * like the Output window.
 */
class ITextOutput
{
public:
	/// Add some text to the window, @param nLength to use a fixed length or -1 to calculate
	virtual void AddToolOutput(LPCTSTR output, int nLength = -1) = 0;
	/// Set the base directory for messages being placed in the output window (for error matching)
	virtual void SetToolBasePath(LPCTSTR path) = 0;
	/// Set the error parser for the window, optionally specifying a regular expression
	virtual void SetToolParser(bool bBuiltIn, LPCTSTR customExpression = NULL) = 0;
	/// Clear the output window
	virtual void ClearOutput() = 0;
};

typedef bool (__stdcall *pn_ext_init_fn)(int iface_version, IPN* pn);
typedef void (__stdcall *pn_ext_exit_fn)();

} // namespace extensions

#endif