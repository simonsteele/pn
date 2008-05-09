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

bool __stdcall pn_init_extension(int iface_version, extensions::IPN* pn)
{
	if(iface_version != PN_EXT_IFACE_VERSION)
		return false;

	g_pn = pn;

	// Do your initialisation stuff here...
	pn->GetGlobalOutputWindow()->AddToolOutput("Hello from the demo extension!");

	extensions::IAppEventSinkPtr appSink(new AppEventSink());
	pn->AddEventSink(appSink);

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