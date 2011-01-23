/**
 * @file plugin.cpp
 * @brief Plugin Interface Implementation
 * @author Simon Steele
 * @note Copyright (c) 2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

std::wstring g_SchemesPath; // extern from stdafx.h

class App : public extensions::IAppEventSink
{
public:
	virtual ~App() {}
	App(extensions::IPN* pn) : m_pn(pn)
	{
	}

	extensions::IPN* GetPN() const { return m_pn; }

	// IAppEventSink

	/// Called when a new document is opened/created
	void OnNewDocument(extensions::IDocumentPtr& doc) {}
	
	/// Called when PN is closing (you are about to be unloaded!)
	void OnAppClose() {}

	/// Called when the user switches to a different document
	void OnDocSelected(extensions::IDocumentPtr& doc) {}

	/// Called when the very first Scintilla window is created, used for loading external lexers
	void OnFirstEditorCreated(HWND hWndScintilla)
	{
		std::vector<char> buffer(MAX_PATH+1);
		DWORD dwRes = ::GetModuleFileNameA(theModule, &buffer[0], MAX_PATH+1);

		if (dwRes == 0 || dwRes == MAX_PATH + 1)
		{
			// FAIL
		}
		else
		{
			::SendMessage(hWndScintilla, SCI_LOADLEXERLIBRARY, 0, reinterpret_cast<LPARAM>(&buffer[0]));
		}
	}

private:
	extensions::IPN* m_pn;
};

static boost::shared_ptr<App> g_app;

bool __stdcall pn_init_extension(int iface_version, extensions::IPN* pn)
{
	if(iface_version != PN_EXT_IFACE_VERSION)
		return false;

	g_app.reset(new App(pn));

	pn->AddEventSink(g_app);

	wchar_t* schemesPath = pn->GetOptionsManager()->GetPNPath(PNPATH_SCHEMES);

	g_SchemesPath = schemesPath;

	pn->ReleaseString(schemesPath);

	return true;
}

void __declspec(dllexport) __stdcall pn_get_extension_info(PN::BaseString& name, PN::BaseString& version)
{
	name = "Custom Scheme Lexer";
	version = "2.0";
}

void __declspec(dllexport) __stdcall pn_exit_extension()
{
	g_app->GetPN()->RemoveEventSink(g_app);
}