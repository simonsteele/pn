/**
 * @file extapp.h
 * @brief Define App, the basic application services
 * @author Simon Steele
 * @note Copyright (c) 2006-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
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

class ExtensionMenuItem;
typedef std::vector<ExtensionMenuItem*> ExtensionItemList;

class ExtensionMenuItem : CommandEventHandler
{
public:
	explicit ExtensionMenuItem(const extensions::MenuItem& source) : m_title(source.Title), m_item(source), m_commandId(0)
	{
		if (source.Type == extensions::miSubmenu)
		{
			for (int i = 0; i < source.SubItems->GetItemCount(); ++i)
			{
				m_items.push_back(new ExtensionMenuItem(source.SubItems->GetItem(i)));
			}
		}
	}

	~ExtensionMenuItem()
	{
		BOOST_FOREACH(ExtensionMenuItem* i, m_items)
		{
			delete i;
		}
	}

	const extensions::MenuItem& GetMenuItem() const
	{
		return m_item;
	}

	const wchar_t* GetTitle() const
	{
		return m_title.c_str();
	}

	const ExtensionItemList& GetSubItems() const
	{
		return m_items;
	}

	bool IsSubMenu() const
	{
		return m_item.Type == extensions::miSubmenu;
	}

	void BuildMenu(HMENU menu, CommandDispatch* dispatcher)
	{
		if (m_title.size() == 0)
		{
			return;
		}

		CSMenuHandle m(menu);
		CW2CT titlet(m_title.c_str());

		if (m_item.Type == extensions::miSubmenu)
		{
			CSPopupMenu submenu;

			BOOST_FOREACH(ExtensionMenuItem* i, m_items)
			{
				i->BuildMenu(submenu.GetHandle(), dispatcher);
			}

			::AppendMenu(m.GetHandle(), MF_POPUP, reinterpret_cast<UINT_PTR>(submenu.GetHandle()), titlet);
		}
		else if (m_title[0] == _T('-'))
		{
			m.AddSeparator();
		}
		else
		{
			if (!m_commandId)
			{
				m_commandId = dispatcher->RegisterCallback(this, PN_COMMAND_PLUGIN, NULL);
			}
			
			m.AddItem(titlet, m_commandId);
		}
	}

	bool SHandleDispatchedCommand(int iCommand, LPVOID data)
	{
		m_item.Handler(m_item.UserData);
		return true;
	}

private:
	extensions::MenuItem m_item;
	std::wstring m_title;
	ExtensionItemList m_items;
	int m_commandId;
};

/**
 * The core App object implementing IPN. This manages extensions
 * and provides the basic interface that extensions use.
 */
class App : public extensions::IPN
{
	typedef std::list<extensions::IAppEventSinkPtr> EventSinkList;
public:
	typedef std::list<extensions::Extension*> ExtensionList;

	App();
	~App();

	void Init();

	CommandDispatch& GetCommandDispatch();
	const AppSettings& GetSettings();

	int FindExtensions();

	void LoadExtensions();

	void RunExtensionCommand(const char* command);

	void SetCanLoadExtensions(bool canLoad);

	ExtensionList& GetExtensions();

	ExtensionItemList& GetExtensionMenuItems();

	/**
	 * This method is provided to clear out the PN user data
	 * store when things go badly wrong. It will also remove
	 * the UI registry settings.
	 */
	bool ClearUserData();

	/**
	 * Clear out and regenerate all compiled schemes.
	 */
	bool CompileSchemes();

	/**
	 * Get any instance of a recorder that has been registered
	 */
	extensions::IRecorderPtr GetRecorder() const;

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

	/// Add a tag source (e.g. ctagsnavigator)
	virtual void AddTagSource(extensions::ITagSource* tagSource);
	
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
	virtual wchar_t* InputBox(const wchar_t* title, const wchar_t* caption);

	/// Make a new document
	virtual extensions::IDocumentPtr NewDocument(const char* scheme);

	/// Open a document
	virtual extensions::IDocumentPtr OpenDocument(const wchar_t* filepath, const char* scheme);

	/// Utility function to safely free strings given to you by PN
	virtual void ReleaseString(const wchar_t* str);

	/// Add plugin menu items
	virtual void AddPluginMenuItems(extensions::IMenuItems *);

	/// Register a script recorder
	virtual void AddRecorder(extensions::IRecorderPtr recorder);

// Stuff to signal event sinks...
public:
	void OnNewDocument(extensions::IDocumentPtr doc);
	void OnSelectDocument(extensions::IDocumentPtr doc);
	void OnFirstEditorCreated(HWND hWndEditor);

private:
	void deinit();
	void ensureUserSettingsDir();
	void unloadExtensions();
	void setAppLanguage();

	bool			m_bCanLoadExtensions;
	EventSinkList	m_sinks;
	ExtensionList   m_exts;
	AppSettings*	m_settings;
	CommandDispatch*m_dispatch;
	ExtensionItemList m_pluginMenuItems;
	extensions::IRecorderPtr m_recorder;
};

#endif