/**
 * @file extapp.cpp
 * @brief Implement IPN and the basic App services
 * @author Simon Steele
 * @note Copyright (c) 2006-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "extiface.h"
#include "appsettings.h"
#include "extension.h"

#include "extapp.h"

#include "scriptregistry.h"

#include "resource.h"
#include "childfrm.h"

#include "pndialogs.h"

#include "pndocking.h"
#include "mainfrm.h"

#include "version.h"

/**
 * Constructor - stuff that happens when PN starts
 */
App::App() : m_dispatch(NULL), m_bCanLoadExtensions(true)
{
	// Now we initialise any l10n stuff...
	// Note that some error checking stuff in AppSettings will make use of StringLoader 
	// so there is a cyclic	dependancy to cope with - this is why we initialise once here
	// and then again in setAppLanguage if necessary.
	L10N::StringLoader::InitResourceLoader();

	// This loads some global app settings, including what to
	// use as the options store and where user settings files are
	// to be stored.
	m_settings = new AppSettings();

	// Now we have the most important settings, we can make the options object...
	g_Context.options = m_settings->MakeOptions();

	// Switch languages if necessary.
	setAppLanguage();

	// Now ensure the user settings directory is available!
	ensureUserSettingsDir();

	// Finally load the cached or default cached options
	OPTIONS->LoadCache();
}

/**
 * Destructor
 */
App::~App()
{
	deinit();
}

/**
 * Init PN
 */
void App::Init()
{
	// Where are the Schemes stored?
	tstring path;
	tstring cpath;
	tstring keypath;
	OPTIONS->GetPNPath(path, PNPATH_SCHEMES);
	OPTIONS->GetPNPath(cpath, PNPATH_COMPILEDSCHEMES);
	OPTIONS->GetPNPath(keypath, PNPATH_USERSETTINGS);
	keypath += _T("keymap.dat");

	// Sort out the schemes...
	SchemeManager& SM = SchemeManager::GetInstanceRef();
	SM.SetPath(path.c_str());
	SM.SetCompiledPath(cpath.c_str());
	SM.Load();

	// Create the command dispatcher
	m_dispatch = new CommandDispatch(keypath.c_str());
}

CommandDispatch& App::GetCommandDispatch()
{
	return *m_dispatch;
}

const AppSettings& App::GetSettings()
{
	return *m_settings;
}

/**
 * Shutdown the app
 */
void App::deinit()
{
	// Remove all plugin menu items
	BOOST_FOREACH(ExtensionMenuItem* item, m_pluginMenuItems)
	{
		delete item;
	}

	// Remove any registered recorder instance
	m_recorder.reset();

	// Now it's safe to unload the extensions
	unloadExtensions();

	delete m_settings;
	
	delete m_dispatch;

	DeletionManager::DeleteAll();

	// Free up the options object, thus storing the options.
	OptionsFactory::Release(g_Context.options);
	g_Context.options = NULL;
}

/**
 * Make sure the user settings dir exists
 */
void App::ensureUserSettingsDir()
{
	// Now ensure the user settings directory is available again!
	tstring usPath;
	OPTIONS->GetPNPath(usPath, PNPATH_USERSETTINGS);
	if(!CreateDirectoryRecursive(usPath.c_str()))
		UNEXPECTED(_T("Could not create user settings folder"));
}

int App::FindExtensions()
{
	// Search for extensions:
	m_settings->FindExtensions();
	
	// Store:
	m_settings->Save();
	
	return 0;
}

/**
 * Load configured extensions, configuration is retrieved from AppSettings
 */
void App::LoadExtensions()
{
	// Allow safe-mode override of loading extensions
	if(!m_bCanLoadExtensions)
		return;

	const extlist& extensions = m_settings->GetExtensions();

	for(extlist::const_iterator i = extensions.begin();
		i != extensions.end();
		++i)
	{
		const ExtDetails& details = (*i);
		if(!details.Disabled && details.Exists())
		{
			extensions::Extension* ext = new extensions::Extension(details.FullPath.c_str(), this);
			if(ext->Valid())
			{
				m_exts.push_back(ext);
			}
			else
			{
				delete ext;
				tstring msg(_T("Failed to load extension: "));
				msg += details.Path;
				LOG(msg.c_str());
			}
		}
	}
}

/**
 * An extension command has a type and a command, examples are:
 * ext:RegisteredExtensionCommand
 * python:PythonScriptRef
 * tcl:SomeTCLCommand
 *
 * The "ext" type is reserved for future use for extensions that
 * register commands. Any other type can be registered by extensions
 * for use in running scripts.
 */
void App::RunExtensionCommand(const char* command)
{
	std::string str(command);
	size_t rindex = str.find(':');
	if(rindex == -1)
		return;

	std::string runner_id = str.substr(0, rindex);
	if (runner_id == "ext")
	{
		UNEXPECTED(_T("Not Yet Implemented"));
	}
	else
	{
		// Let Script run this!
		Script s("", command);
		s.Run();
	}
}

void App::SetCanLoadExtensions(bool canLoad)
{
	m_bCanLoadExtensions = canLoad;
}

App::ExtensionList& App::GetExtensions()
{
	return m_exts;
}

ExtensionItemList& App::GetExtensionMenuItems()
{
	return m_pluginMenuItems;
}

/**
 * This method is provided to clear out the PN user data
 * store when things go badly wrong. It will also remove
 * the UI registry settings.
 *
 * 1. Remove *.* from PN User Settings folder
 * 2. Clear the UI settings
 */
bool App::ClearUserData()
{
	tstring userSettingsDir;
	OPTIONS->GetPNPath(userSettingsDir, PNPATH_USERSETTINGS);
	
	// Go for the hard-core directory deletion approach!
	if( !DeleteDirectory(userSettingsDir.c_str(), true) )
	{
		RETURN_UNEXPECTED(_T("Failed to delete user settings directory!"), false);
	}

	// Now clear out the UI settings
	OPTIONS->Clear(PNSK_INTERFACE);

	// Re-create the user settings dir
	ensureUserSettingsDir();

	return true;
}

/**
 * Clear out and regenerate all compiled schemes.
 */
bool App::CompileSchemes()
{
	SchemeManager::GetInstance()->Compile();
	
	return true;
}

extensions::IRecorderPtr App::GetRecorder() const
{
	return m_recorder;
}

/**
 * Unload all the extensions, signalling OnAppClose along the way
 */
void App::unloadExtensions()
{
	for(EventSinkList::const_iterator i = m_sinks.begin(); i != m_sinks.end(); ++i)
	{
		(*i)->OnAppClose();
	}

	m_sinks.clear();

	for(ExtensionList::const_iterator i = m_exts.begin(); i != m_exts.end(); ++i)
	{
		(*i)->Unload();
		delete (*i);
	}

	m_exts.clear();
}

void App::setAppLanguage()
{
	tstring language = OPTIONS->Get(PNSK_INTERFACE, _T("Language"), _T(""));
	if (language.size())
	{
		language = _T("pnlang_") + language;
		language += _T("_") PN_VERSTRING_T _T(".dll");

		HINSTANCE languageResources = ::LoadLibrary(language.c_str());
		if (languageResources)
		{
			_Module.SetResourceInstance(languageResources);
		}

		// Re-initialize resource loader from this point:
		L10N::StringLoader::InitResourceLoader();
	}
}

unsigned int App::GetIFaceVersion() const
{
	return PN_EXT_IFACE_VERSION;
}

const char* App::GetVersion() const
{
	return PN_VERSTRING;
}

void App::AddEventSink(extensions::IAppEventSinkPtr sink)
{
	m_sinks.push_back(sink);
}

void App::RemoveEventSink(extensions::IAppEventSinkPtr sink)
{
	m_sinks.remove(sink);
}

/// Add a tag source (e.g. ctagsnavigator)
void App::AddTagSource(extensions::ITagSource* tagSource)
{
	JumpToHandler::GetInstance()->AddSource(tagSource);
}

/**
 * Get the current script registry interface
 */
extensions::IScriptRegistry* App::GetScriptRegistry()
{
	return ScriptRegistry::GetInstance();
}

/**
 * Get the options manager instance in use
 */
extensions::IOptions* App::GetOptionsManager()
{
	return OPTIONS;
}

/**
 * Notify event consumers of a new document
 */
void App::OnNewDocument(extensions::IDocumentPtr doc)
{
	for (EventSinkList::const_iterator i = m_sinks.begin(); i != m_sinks.end(); ++i)
	{
		(*i)->OnNewDocument(doc);
	}
}

/**
 * Notify event consumers of a document selection change
 */
void App::OnSelectDocument(extensions::IDocumentPtr doc)
{
	for (EventSinkList::const_iterator i = m_sinks.begin(); i != m_sinks.end(); ++i)
	{
		(*i)->OnDocSelected(doc);
	}
}

/**
 * Notify event consumers that the first Scintilla window is up.
 */
void App::OnFirstEditorCreated(HWND hWndEditor)
{
	for (EventSinkList::const_iterator i = m_sinks.begin(); i != m_sinks.end(); ++i)
	{
		(*i)->OnFirstEditorCreated(hWndEditor);
	}
}

/**
 * Get the current document (if there is one)
 */
extensions::IDocumentPtr App::GetCurrentDocument()
{
	CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor());
	if(pChild)
	{
		return pChild->GetDocument();
	}
	
	return extensions::IDocumentPtr();
}

/**
 * Get a pointer to the global output window
 */
extensions::ITextOutput* App::GetGlobalOutputWindow()
{
	return g_Context.m_frame->GetGlobalOutputWindow();
}

/**
 * Get a pointer to the mainframe window
 */
HWND App::GetMainWindow()
{
	return static_cast<CMainFrame*>(g_Context.m_frame)->m_hWnd;
}

/// Get the users search options
extensions::ISearchOptions* App::GetUserSearchOptions()
{
	return OPTIONS->GetSearchOptions();
}

/**
 * Perform a find in files operation with the options
 * passed in.
 */
void App::FindInFiles(extensions::ISearchOptions* options)
{
	g_Context.m_frame->FindInFiles(static_cast<SearchOptions*>(options));
}

/**
 * Present an InputBox to get input from the user
 */
wchar_t* App::InputBox(const wchar_t* title, const wchar_t* caption)
{
	CW2CT titleconv(title);
	CW2CT captionconv(caption);
	CInputDialog ib(titleconv, captionconv);

	if(ib.DoModal() == IDOK)
	{
		if(ib.GetInput() != NULL)
		{
			CT2CW input(ib.GetInput());
			return wcsnewdup(input);
		}	
	}

	return NULL;
}

/**
 * Open a file, optionally specify a scheme name to open the file
 * using or specify NULL to use the default.
 */
extensions::IDocumentPtr App::OpenDocument(const wchar_t* filepath, const char* scheme)
{
	Scheme* pScheme(NULL);
	if(scheme != NULL)
	{
		pScheme = SchemeManager::GetInstance()->SchemeByName(scheme);
	}

	CMainFrame* mainFrame = static_cast<CMainFrame*>(g_Context.m_frame); 

	if (mainFrame->CheckAlreadyOpen(filepath, eSwitch))
	{
		extensions::IDocumentPtr doc = GetCurrentDocument();
		return doc;
	}

	bool opened = mainFrame->OpenFile(filepath, pScheme);
	if(opened)
	{
		extensions::IDocumentPtr doc = GetCurrentDocument();
		return doc;
	}

	return extensions::IDocumentPtr();
}

/**
 * Create a new document, optionally specify a scheme name
 */
extensions::IDocumentPtr App::NewDocument(const char* scheme)
{
	Scheme* pScheme(NULL);
	if (scheme)
	{
		pScheme = SchemeManager::GetInstance()->SchemeByName(scheme);
	}
	
	if (pScheme != NULL)
	{
		static_cast<CMainFrame*>(g_Context.m_frame)->GetFactory().WithScheme(pScheme);
	}
	else
	{
		static_cast<CMainFrame*>(g_Context.m_frame)->GetFactory().Default();
	}

	return GetCurrentDocument();
}

/**
 * Release a C string allocated by PN
 */
void App::ReleaseString(const wchar_t* str)
{
	delete [] str;
}

void App::AddPluginMenuItems(extensions::IMenuItems *source)
{
	for (int i = 0; i < source->GetItemCount(); ++i)
	{
		m_pluginMenuItems.push_back(new ExtensionMenuItem(source->GetItem(i)));
	}
}

void App::AddRecorder(extensions::IRecorderPtr recorder)
{
	m_recorder = recorder;
}