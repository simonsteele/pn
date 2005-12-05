// pypn.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"
#include "sinks.h"
#include "env.h"
#include "modules.h"

using namespace boost::python;

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

extensions::IAppEventSinkPtr g_appsink;
App *g_app = NULL;

#define PN_INIT_PYTHON_MODULE(name) \
	if(PyImport_AppendInittab(#name, init##name) == -1) \
	throw std::runtime_error("Failed to add " #name " module to builtins");

bool __stdcall init_pn_extension(int iface_version, extensions::IPN* pn)
{
	if(iface_version != PN_EXT_IFACE_VERSION)
		return false;

	PN_INIT_PYTHON_MODULE(pn);
	PN_INIT_PYTHON_MODULE(debug);
	PN_INIT_PYTHON_MODULE(scintilla);

	Py_Initialize();

	g_app = new App( handle<>(borrowed(PyImport_AddModule("__main__"))) );

	g_appsink.reset( new AppSink(pn) );
	pn->AddEventSink(g_appsink);

	return true;
}

void __declspec(dllexport) __stdcall exit_pn_extension()
{
	AppSink* pAppSink = static_cast<AppSink*>( g_appsink.get() );
	pAppSink->GetPN()->RemoveEventSink(g_appsink);
	g_appsink.reset();

	delete g_app;

	Py_Finalize();
}

std::string getPythonErrorString() {
    // Extra paranoia...
    if (!PyErr_Occurred()) {
        return "No Python error";
    }

    PyObject *type, *value, *traceback;
    PyErr_Fetch(&type, &value, &traceback);
    PyErr_Clear();

    std::string message = "Python error: ";
    if (type) {
        type = PyObject_Str(type);
        message += PyString_AsString(type);
    }
    if (value) {
        value = PyObject_Str(value);
        message += ": ";
        message += PyString_AsString(value);
    }
    Py_XDECREF(type);
    Py_XDECREF(value);
    Py_XDECREF(traceback);

    return message;
}

BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
    return TRUE;
}