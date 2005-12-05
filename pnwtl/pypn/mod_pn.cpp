#include "stdafx.h"

using namespace extensions;
using namespace boost::python;

BOOST_PYTHON_MODULE(pn)
{
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

	//

	try
	{
		register_ptr_to_python< boost::shared_ptr<IDocument> >();
	}
	catch(error_already_set&)
	{
		std::string s = getPythonErrorString();
		OutputDebugString(s.c_str());
	}

	/*
	Tried and failed:
	// Temporary code for smart pointers
		objects::class_value_wrapper< 
		boost::shared_ptr< IDocument >, objects::make_ptr_instance< 
			IDocument, objects::pointer_holder< 
			boost::shared_ptr< IDocument >, IDocument >
		>
		>();
	*/
}