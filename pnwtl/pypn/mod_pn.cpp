#include "stdafx.h"
#include "sinks.h"
#include "app.h"

using namespace extensions;
using namespace boost::python;

void RegisterScript(const char* scriptname, const char* group, const char* name)
{
	g_app->RegisterScript(scriptname, group, name);
}

boost::shared_ptr<IDocument> CurrentDoc()
{
	return g_app->GetPN()->GetCurrentDocument();
}

std::string GetPNPath()
{
	std::string str;
	g_app->GetPN()->GetOptionsManager()->GetPNPath(str);
	return str;
}

BOOST_PYTHON_MODULE(pn)
{
	///////////////////////////////////////////////////////////////////////////////////////////////
	// Expose Useful Bits

	def("CurrentDoc", &CurrentDoc);
	def("RegisterScript", &RegisterScript);
	def("AppPath", &GetPNPath);

	///////////////////////////////////////////////////////////////////////////////////////////////
	// Expose IDocument

	// This bit of magic gets overloaded functions working...
	LRESULT (IDocument::*pSendMessage)(UINT msg, WPARAM wParam, LPARAM lParam) = &IDocument::SendEditorMessage;
	LRESULT (IDocument::*pSendMessage2)(UINT msg, WPARAM wParam, const char* strParam) = &IDocument::SendEditorMessage;

	class_<IDocument, /*boost::shared_ptr<IDocument>,*/ boost::noncopyable >("IDocument", no_init)
		.def("GetTitle", &IDocument::GetTitle)
		.def("GetFileName", &IDocument::GetFileName)
		.def("GetCurrentScheme", &IDocument::GetCurrentScheme)

		.def("SendMessage", pSendMessage)
		.def("SendMessage", pSendMessage2)
	
		.def("IsValid", &IDocument::IsValid)
    ;

	try
	{
		register_ptr_to_python< boost::shared_ptr<IDocument> >();
	}
	catch(error_already_set&)
	{
		std::string s = getPythonErrorString();
		OutputDebugString(s.c_str());
	}
}