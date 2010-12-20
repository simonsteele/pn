// plugin.cpp : Defines the entry point for the Programmer's Notepad Plugin
//

#include "stdafx.h"

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

	// Do your initialisation stuff here...

	// You probably want to store pn somewhere, and hook up to some app events. These
	// events will tell you when documents are opened, closed, etc.
	
	// extensions::IAppEventSinkPtr sink(new MyEventSink());
	// pn->AddEventSink(sink);

	// You can control various basic functionality right from the pn instance:

	// pn->NewDocument(NULL);
	// pn->OpenDocument("Sample.cpp", "java"); // Open Sample.cpp with the java scheme
	// pn->OpenDocument("Sample.cpp", NULL);   // Open Sample.cpp with the default scheme for *.cpp
	// pn->GetCurrentDocument()->GetFileName();

	// Documents have events too:

	// extensions::IDocumentEventSinkPtr docSink(new DocEvents(pn->GetCurrentDocument()));
	// pn->GetCurrentDocument()->AddEventSink(docSink);
	// extensions::IDocumentEventSinkPtr editSink(new EditEvents(pn->GetCurrentDocument()));
	// pn->GetCurrentDocument()->AddEventSink(editSink);

	return true;
}

void __declspec(dllexport) __stdcall pn_get_extension_info(PN::BaseString& name, PN::BaseString& version)
{
	name = "My Plugin";
	version = "0.1";
}

void __declspec(dllexport) __stdcall pn_exit_extension()
{

}