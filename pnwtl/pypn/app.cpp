#include "stdafx.h"
#include "sinks.h" 
#include "app.h"
#include "utils.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

typedef std::basic_string<TCHAR> tstring;

App::App(boost::python::handle<>& obj, extensions::IPN* app) : m_app(app), main_module(obj)
{
	m_registry = m_app->GetScriptRegistry();
	if(m_registry)
		m_registry->RegisterRunner("python", this);

	m_registry->Add("test", "Test Script", "python:testScript");

	main_namespace = main_module.attr("__dict__");
}

App::~App()
{
	int a = 0;
}

void App::Initialise()
{
	// Before we do anything else, make sure python can find our files!
	std::string s;
	m_app->GetOptionsManager()->GetPNPath(s);
	if((s[s.length()-1]) == '\\')
	{
		s.erase(s.length()-1);
	}

	std::string setuppaths("import sys\nsys.path.append(r'" + s + "')\nsys.path.append(r'" + s + "\\scripts')\n\n");
	OutputDebugString(setuppaths.c_str());
	if(PyRun_String(setuppaths.c_str(), Py_file_input, main_namespace.ptr(),main_namespace.ptr()) != 0)
		OutputDebugString(getPythonErrorString().c_str());

	// Now run the init.py file
	loadInitScript();
}

void App::RegisterScript(const char* scriptname, const char* group, const char* name)
{
	std::string scriptref("python:");
	scriptref += scriptname;
	m_registry->Add(group, name, scriptref.c_str());
}

void App::RunScript(const char* name)
{
	try
	{
		boost::python::call_method<void>(m_glue.ptr(), "runScript", name);
	}
	catch(boost::python::error_already_set&)
	{
		std::string s = getPythonErrorString();
		OutputDebugString(s.c_str());
	}
}

void App::loadInitScript()
{
	extensions::IOptions* opts = m_app->GetOptionsManager();
	tstring path;
	opts->GetPNPath(path);

	TCHAR szpath[MAX_PATH+1];
	_tcscpy(szpath, path.c_str());

	::PathAppend(szpath, "init.py");

	runFile(szpath);

	m_glue = main_module.attr("glue");
}

void App::runFile(const char* szpath)
{
	struct _stat statbuf;
	int result;
	result = _stat( szpath, &statbuf );
	if(result == 0)
	{
		FILE* init_script = fopen(szpath, "r");
		
		if(init_script != NULL)
		{
			std::string init;
			char* buf = new char[statbuf.st_size+1];
			while( fgets(buf, statbuf.st_size+1, init_script) )
			{
				init += buf;
			}
			fclose(init_script);

			try
			{
				boost::python::handle<> ignored(PyRun_String(init.c_str(),
					
					Py_file_input, 
					main_namespace.ptr(),
					main_namespace.ptr()
				));
			}
			catch(boost::python::error_already_set&)
			{
				std::string s = PyTracebackToString();
				OutputDebugString(s.c_str());
			}

			delete [] buf;
		}
	}
}