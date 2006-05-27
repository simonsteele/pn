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

// Implement IPN:
public:
	/// Get the extension interface version
	virtual unsigned int get_iface_version() const;
	/// Get the PN version
	virtual const char* get_version() const;
	
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

// Stuff to signal event sinks...
public:
	void OnNewDocument(extensions::IDocumentPtr doc);

private:
	void deinit();

	void loadExtensions();
	void unloadExtensions();

	EventSinkList	m_sinks;
	ExtensionList	m_exts;
	AppSettings*	m_settings;
	CommandDispatch*m_dispatch;
};

#endif