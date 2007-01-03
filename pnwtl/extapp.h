/**
 * @file extapp.h
 * @brief Define App, the basic application services
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef extapp_h__included
#define extapp_h__included

////////////////////////
// Predeclarations:
namespace extensions { class Extension; }
class AppSettings;
class CommandDispatch;
////////////////////////

/**
 * The core App object implementing IPN. This manages extensions
 * and provides the basic interface that extensions use.
 */
class App : public extensions::IPN
{
	typedef std::list<extensions::IAppEventSinkPtr> EventSinkList;
	typedef std::list<extensions::Extension*> ExtensionList;

public:
	App();
	~App();

	void Init();

	CommandDispatch& GetCommandDispatch();
	const AppSettings& GetSettings();
	
	void LoadExtensions();

	void RunExtensionCommand(const char* command);

	void SetCanLoadExtensions(bool canLoad);

	/**
	 * This method is provided to clear out the PN user data
	 * store when things go badly wrong. It will also remove
	 * the UI registry settings.
	 */
	bool ClearUserData();

// Implement IPN:
public:
	/// Get the extension interface version
	virtual unsigned int GetIFaceVersion() const;
	/// Get the PN version
	virtual const char* GetVersion() const;
	
	/// Add an application event sink
	virtual void AddEventSink(extensions::IAppEventSinkPtr sink);
	/// Remove an application event sink
	virtual void RemoveEventSink(extensions::IAppEventSinkPtr sink);
	
	/// Get the script registry
	virtual extensions::IScriptRegistry* GetScriptRegistry();
	
	/// Get the options manager
	virtual extensions::IOptions* GetOptionsManager();

	/// Get the current active document
	virtual extensions::IDocumentPtr GetCurrentDocument();

	/// Get the global output window
	virtual extensions::ITextOutput* GetGlobalOutputWindow();

	/// Get the main application window
	virtual HWND GetMainWindow();

	/// Get the users search options
	virtual extensions::ISearchOptions* GetUserSearchOptions();

	/// Start a find in files operation
	virtual void FindInFiles(extensions::ISearchOptions* options);

	/// Get some text from the user
	virtual char* InputBox(const char* title, const char* caption);

	/// Make a new document
	virtual extensions::IDocumentPtr NewDocument(const char* scheme);

	/// Open a document
	virtual extensions::IDocumentPtr OpenDocument(const char* filepath, const char* scheme);

	/// Utility function to safely free strings given to you by PN
	virtual void ReleaseString(const TCHAR* str);

// Stuff to signal event sinks...
public:
	void OnNewDocument(extensions::IDocumentPtr doc);

private:
	void deinit();
	void ensureUserSettingsDir();
	void unloadExtensions();

	bool			m_bCanLoadExtensions;
	EventSinkList	m_sinks;
	ExtensionList	m_exts;
	AppSettings*	m_settings;
	CommandDispatch*m_dispatch;
};

#endif