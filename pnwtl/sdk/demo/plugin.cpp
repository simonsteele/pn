// plugin.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"
#include "sinks.h"

BOOL APIENTRY DllMain( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
					 )
{
    return TRUE;
}

void PluginFunc(extensions::cookie_t /*cookie*/)
{
	::MessageBox(NULL, _T("Hello!"), _T("Demo Plugin"), MB_ICONINFORMATION | MB_OK);
}

class Menu : public extensions::IMenuItems
{
public:
	Menu()
	{
		item1.Handler = &PluginFunc;
		item1.Title = L"Hello World";
		item1.Type = extensions::miItem;
		item1.UserData = 0;
	}

	/**
	 * Get the number of MenuItem instances that can be retrieved.
	 */
	virtual int GetItemCount() const
	{
		return 1;
	}

	/**
	 * Get an individual MenuItem instance by index.
	 */
	virtual extensions::MenuItem& GetItem(int index) const
	{
		if (index == 0)
			return const_cast<extensions::MenuItem&>(item1);
	}

private:
	extensions::MenuItem item1;
};

bool __stdcall pn_init_extension(int iface_version, extensions::IPN* pn)
{
	if(iface_version != PN_EXT_IFACE_VERSION)
		return false;

	g_pn = pn;

	// Do your initialisation stuff here...
	pn->GetGlobalOutputWindow()->AddToolOutput("Hello from the demo extension!");

	extensions::IAppEventSinkPtr appSink(new AppEventSink());
	pn->AddEventSink(appSink);

	Menu menu;
	pn->AddPluginMenuItems(&menu);

	return true;
}

void __declspec(dllexport) __stdcall pn_get_extension_info(PN::BaseString& name, PN::BaseString& version)
{
	name = "Demo Plugin";
	version = "0.1";
}

void __declspec(dllexport) __stdcall pn_exit_extension()
{

}