/**
 * @file extapp.cpp
 * @brief Implement IPN and the basic App services
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
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

App::App() : m_dispatch(NULL)
{
	// Now we initialise any l10n stuff...
	//TODO: Make this check settings in AppSettings to work out what to do. Note that some
	//error checking stuff in AppSettings will make use of StringLoader so there is a cyclic
	//dependancy to cope with.
	L10N::StringLoader::InitResourceLoader();

	// This loads some global app settings, including what to
	// use as the options store and where user settings files are
	// to be stored.
	m_settings = new AppSettings();

	// Now we have the most important settings, we can make the options object...
	g_Context.options = m_settings->MakeOptions();

	// Now ensure the user settings directory is available!
	tstring usPath;
	OPTIONS->GetPNPath(usPath, PNPATH_USERSETTINGS);
	if(!CreateDirectoryRecursive(usPath.c_str()))
		UNEXPECTED(_T("Could not create user settings folder"));

	// Finally load the cached or default cached options
	OPTIONS->LoadCache();
}

App::~App()
{
	deinit();
}

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

	// Load our extensions - done from outside
	//loadExtensions();
}

CommandDispatch& App::GetCommandDispatch()
{
	return *m_dispatch;
}

const AppSettings& App::GetSettings()
{
	return *m_settings;
}

void App::deinit()
{
	unloadExtensions();

	delete m_settings;
	
	DeletionManager::DeleteAll();

	// Free up the options object, thus storing the options.
	OptionsFactory::Release(g_Context.options);
	g_Context.options = NULL;

	delete m_dispatch;
}

void App::LoadExtensions()
{
	const tstring_list& extensions = m_settings->GetExtensions();

	for(tstring_list::const_iterator i = extensions.begin();
		i != extensions.end();
		++i)
	{
		const tstring& path = (*i);
		if(path[0] != '#' && path[0] != '!')
		{
			extensions::Extension* ext = new extensions::Extension((*i).c_str(), this);
			if(ext->Valid())
			{
				m_exts.push_back(ext);
			}
			else
			{
				tstring msg(_T("Failed to load extension: "));
				msg += path;
				LOG(msg.c_str());
			}
		}
	}
}

void App::unloadExtensions()
{
	for(EventSinkList::const_iterator i = m_sinks.begin(); i != m_sinks.end(); ++i)
	{
		(*i)->OnAppClose();
	}

	for(ExtensionList::const_iterator i = m_exts.begin(); i != m_exts.end(); ++i)
	{
		(*i)->Unload();
	}
	m_exts.clear();
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

extensions::IScriptRegistry* App::GetScriptRegistry()
{
	return ScriptRegistry::GetInstance();
}

extensions::IOptions* App::GetOptionsManager()
{
	return OPTIONS;
}

void App::OnNewDocument(extensions::IDocumentPtr doc)
{
	for(EventSinkList::const_iterator i = m_sinks.begin(); i != m_sinks.end(); ++i)
	{
		(*i)->OnNewDocument(doc);
	}
}

extensions::IDocumentPtr App::GetCurrentDocument()
{
	CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor());
	if(pChild)
	{
		return pChild->GetDocument();
	}
	
	return extensions::IDocumentPtr();
}

extensions::ITextOutput* App::GetGlobalOutputWindow()
{
	return g_Context.m_frame->GetGlobalOutputWindow();
}

void App::ReleaseString(const TCHAR* str)
{
	delete [] str;
}