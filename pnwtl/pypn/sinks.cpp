/**
 * @file sinks.cpp
 * @brief Defines miscellaneous sinks used to register for events
 * @author Simon Steele
 * @note Copyright (c) 2006-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "sinks.h"
#include "app.h"

using namespace extensions;
using namespace boost::python;

/*#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif*/

DocSink::DocSink(IDocumentPtr& doc) : m_doc(doc)
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
	}
	catch(error_already_set&)
	{
		std::string s = getPythonErrorString();
		OutputDebugString(s.c_str());
	}
}

void DocSink::OnAfterLoad()
{
	try
	{
		boost::python::call_method<void>(g_app->PyPnGlue().ptr(), "onDocLoad", (m_doc));
	}
	catch(error_already_set&)
	{
		std::string s = getPythonErrorString();
		OutputDebugString(s.c_str());
	}
}

void DocSink::OnBeforeSave(const wchar_t* filename)
{
	try
	{
		std::wstring fn(filename);
		boost::python::call_method<void>(g_app->PyPnGlue().ptr(), "onDocSave", fn, (m_doc));
	}
	catch(error_already_set&)
	{
		std::string s = getPythonErrorString();
		OutputDebugString(s.c_str());
	}
}

void DocSink::OnAfterSave()
{
	try
	{
		boost::python::call_method<void>(g_app->PyPnGlue().ptr(), "onDocSaved", (m_doc));
	}
	catch(error_already_set&)
	{
		std::string s = getPythonErrorString();
		OutputDebugString(s.c_str());
	}
}

void DocSink::OnModifiedChanged(bool modified)
{
	try
	{
		boost::python::call_method<void>(g_app->PyPnGlue().ptr(), "onModifiedChanged", modified, (m_doc));
	}
	catch(error_already_set&)
	{
		std::string s = getPythonErrorString();
		OutputDebugString(s.c_str());
	}
}

void DocSink::OnWriteProtectChanged(bool writeProtect)
{
	try
	{
		boost::python::call_method<void>(g_app->PyPnGlue().ptr(), "onWriteProtectChanged", writeProtect, (m_doc));
	}
	catch(error_already_set&)
	{
		std::string s = getPythonErrorString();
		OutputDebugString(s.c_str());
	}
}
