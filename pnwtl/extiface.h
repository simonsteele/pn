/**
 * @file extiface.h
 * @brief PN Extensions Interface
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef extiface_h__included_670F47C6_1FF6_4605_9F74_6EC70FD85C26
#define extiface_h__included_670F47C6_1FF6_4605_9F74_6EC70FD85C26

#include "IOptions.h"

namespace extensions
{

#define PN_EXT_IFACE_VERSION	1

class IPN;
class IDocument;
class IAppEventSink;
class IDocumentEventSink;
class IScriptRegistry;
class ITextOutput;

typedef boost::shared_ptr<IDocument> IDocumentPtr;
typedef boost::shared_ptr<IDocumentEventSink> IDocumentEventSinkPtr;
typedef boost::shared_ptr<IAppEventSink> IAppEventSinkPtr;

/**
 * The main PN interface class. This is your interface to PN allowing
 * you access to the options system, script registry and most importantly
 * documents. It also allows you to register for events.
 */
class IPN
{
public:
	virtual ~IPN(){}

	virtual unsigned int get_iface_version() const = 0;
	virtual const char* get_version() const = 0;
	
	virtual void AddEventSink(IAppEventSinkPtr sink) = 0;
	virtual void RemoveEventSink(IAppEventSinkPtr sink) = 0;

	virtual IScriptRegistry* GetScriptRegistry() = 0;
	virtual IOptions* GetOptionsManager() = 0;

	virtual IDocumentPtr GetCurrentDocument() = 0;

	virtual ITextOutput* GetGlobalOutputWindow() = 0;
};

/**
 * The document interface. This allows you to control a given document,
 * get information about it and register for events.
 */
class IDocument
{
public:
	virtual ~IDocument(){}

	virtual const char* GetTitle() const = 0;
	virtual const char* GetFileName() const = 0;
	virtual const char* GetCurrentScheme() const = 0;

	virtual HWND GetScintillaHWND() const = 0;

	virtual LRESULT SendEditorMessage(UINT msg, WPARAM wParam, LPARAM lParam) = 0;
	virtual LRESULT SendEditorMessage(UINT msg, WPARAM wParam, const char* strParam) = 0;

	virtual bool IsValid() const = 0;

	virtual void AddEventSink(IDocumentEventSinkPtr sink) = 0;
	virtual void RemoveEventSink(IDocumentEventSinkPtr sink) = 0;
};

/**
 * Event sink interface for application events.
 */
class IAppEventSink
{
public:
	virtual ~IAppEventSink(){}

	virtual void OnNewDocument(IDocumentPtr doc) = 0;
};

/**
 * Text editor event sink
 */
class ITextEditorEventSink
{
public:
	virtual ~ITextEditorEventSink(){}

    virtual void OnCharAdded(char c) = 0;
};

/**
 * Document event sink
 */
class IDocumentEventSink : public ITextEditorEventSink
{
public:
	virtual ~IDocumentEventSink(){}

	virtual void OnSchemeChange(const char* scheme) = 0;
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

class ITextOutput
{
public:
	virtual void AddToolOutput(LPCTSTR output, int nLength = -1) = 0;
	virtual void SetToolBasePath(LPCTSTR path) = 0;
	virtual void SetToolParser(bool bBuiltIn, LPCTSTR customExpression = NULL) = 0;
	virtual void ClearOutput() = 0;
};

// Make sure you export a function that looks like this:
// bool init_pn_extension(int iface_version, IPN* pn);
//  - return false if your iface_version does not match and you will
//    be safely unloaded.
// void exit_pn_extension();

// Maybe later: get_extension_info()...

typedef bool (__stdcall *pn_ext_init_fn)(int iface_version, IPN* pn);
typedef void (__stdcall *pn_ext_exit_fn)();

} // namespace extensions

#endif