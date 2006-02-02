#include "stdafx.h"
#include "sinks.h"
#include "app.h"

using namespace extensions;
using namespace boost::python;

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

DocSink::DocSink(IDocumentPtr doc) : m_doc(doc)
{

}

DocSink::~DocSink()
{

}

void DocSink::OnDocClosing()
{
	m_doc.reset();
}

void DocSink::OnCharAdded(char c)
{
	try
	{
		boost::python::call_method<void>(g_app->PyPnGlue().ptr(), "onCharAdded", c, (m_doc));
		/*g_app->main_namespace. = c;
		g_app->main_namespace.attr("doc") = m_doc;

		handle<> ignored(PyRun_String("onCharAdded(c, doc)",
			Py_file_input, 
			g_app->main_namespace.ptr(),
			g_app->main_namespace.ptr()
		));*/
	}
	catch(error_already_set&)
	{
		std::string s = getPythonErrorString();
		OutputDebugString(s.c_str());
	}
}