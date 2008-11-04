// pypn.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"
#include "sinks.h"
#include "app.h"
#include "modules.h"

using namespace boost::python;

//#if defined (_DEBUG)
//	#define new DEBUG_NEW
//	#undef THIS_FILE
//	static char THIS_FILE[] = __FILE__;
//#endif

extensions::IAppEventSinkPtr g_appsink;
App *g_app = NULL;

#define PN_INIT_PYTHON_MODULE(name) \
	if(PyImport_AppendInittab(#name, init##name) == -1) \
	throw std::runtime_error("Failed to add " #name " module to builtins");

bool __stdcall pn_init_extension(int iface_version, extensions::IPN* pn)
{
	if(iface_version != PN_EXT_IFACE_VERSION)
		return false;

	PN_INIT_PYTHON_MODULE(pn);
	PN_INIT_PYTHON_MODULE(debug);
	PN_INIT_PYTHON_MODULE(scintilla);

	Py_Initialize();

	g_app = new App( handle<>(borrowed(PyImport_AddModule("__main__"))), pn );
	g_appsink.reset( g_app );

	g_app->Initialise();

	pn->AddEventSink(g_appsink);

	return true;
}

void __declspec(dllexport) __stdcall pn_get_extension_info(PN::BaseString& name, PN::BaseString& version)
{
	name = "PyPN";
	version = "0.10";
}

void __declspec(dllexport) __stdcall pn_exit_extension()
{
	g_app->GetPN()->RemoveEventSink(g_appsink);
	
	// This will delete the app too...
	g_app = NULL;
	g_appsink.reset();

	Py_Finalize();
}

BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
    return TRUE;
}