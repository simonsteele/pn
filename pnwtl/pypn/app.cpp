/**
 * @file app.cpp
 * @brief Plugin Main Implementation
 * @author Simon Steele
 * @note Copyright (c) 2006-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "sinks.h" 
#include "app.h"
#include "utils.h"
#include "wrapscintilla.h"
#include "recorder.h"
#include "../include/encoding.h"

/*#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif*/

typedef std::basic_string<TCHAR> tstring;

/**
 * Constructor
 */
App::App(boost::python::handle<>& obj, extensions::IPN* app) : m_app(app), main_module(obj)
{
	m_registry = m_app->GetScriptRegistry();
	if(m_registry)
	{
		m_registry->RegisterRunner("python", this);
		m_registry->EnableSchemeScripts("python", "python");
	}

	main_namespace = main_module.attr("__dict__");

	m_outputShown = false;
	m_output = NULL;
}

App::~App()
{
}

/**
 * Initialise the Python app environment
 */
void App::Initialise()
{
	// Before we do anything else, make sure python can find our files!
	// Find Me
	
	const wchar_t* path = m_app->GetOptionsManager()->GetPNPath();
	Utf16_Windows1252 pathconv(path);
	m_app->ReleaseString(path);

	std::string s(pathconv);

	if((s[s.length()-1]) == '\\')
	{
		s.erase(s.length()-1);
	}

	// Run some Python...
	std::string setuppaths("import sys\nsys.path.append(r\"" + s + "\")\nsys.path.append(r\"" + s + "\\scripts\")\n\n");
	//OutputDebugString(setuppaths.c_str());
	try
	{
		boost::python::handle<> ignored(PyRun_String(setuppaths.c_str(), Py_file_input, main_namespace.ptr(), main_namespace.ptr()));
	}
	catch(boost::python::error_already_set&)
	{
		std::string s = getPythonErrorString();
		AddOutput(s.c_str());
	}

	// Now run the init.py file
	loadInitScript();

	// And now we're good to handle script recording:
	m_recorder.reset(new Recorder(this));
	m_app->AddRecorder(m_recorder);
}

void App::OnNewDocument(extensions::IDocumentPtr& doc)
{
	boost::shared_ptr<DocSink> p(new DocSink(doc));
	extensions::IDocumentEventSinkPtr pd(p);
	extensions::ITextEditorEventSinkPtr pe(p);
	doc->AddEventSink(pd);
	doc->AddEventSink(pe);
}

void App::OnAppClose()
{	
}

void App::OnDocSelected(extensions::IDocumentPtr& doc)
{
}

void App::OnFirstEditorCreated(HWND hWndScintilla)
{
}

/**
 * Run a script that we registered
 */
void App::RunScript(const char* name)
{
	try
	{
		m_outputShown = false;
		boost::python::call_method<void>(m_glue.ptr(), "runScript", name);
	}
	catch(boost::python::error_already_set&)
	{
		std::string s = getPythonErrorString();
		AddOutput(s.c_str());
	}
}

// Move the text back one character each time we find a CR
void fixCRLFLineEnds(char* buffer)
{
	bool sliding(false);
	char* pfixed(buffer);
	while(*buffer)
	{
		if(*buffer != 0x0D)
		{
			if(sliding)
				*pfixed = *buffer;
			pfixed++;
		}
		else
		{
			sliding = true;
		}
			
		buffer++;
	}
	*pfixed = NULL;
}

// Change the CRs to LFs
void fixCRLineEnds(char* buffer)
{
	while(*buffer)
	{
		if(*buffer == 0x0D)
		{
			*buffer = 0x0A;
		}
			
		buffer++;
	}
}

/**
 * Run an open document as if it was a script
 */
void App::RunDocScript(extensions::IDocumentPtr& doc)
{
	HWND hWndScintilla = doc->GetScintillaHWND();
	if(hWndScintilla)
	{
		PNScintilla sc(hWndScintilla);
		
		int length = sc.GetLength();
		char* buffer = new char[length+1];
		sc.GetText(length+1, buffer);
		//buffer[length] = '\0';
		switch(sc.GetEOLMode())
		{
		case SC_EOL_CRLF:
			fixCRLFLineEnds(buffer);
			break;
		case SC_EOL_CR:
			fixCRLineEnds(buffer);
			break;
		}

		try
		{
			boost::python::handle<> ignored(PyRun_String(buffer,
				Py_file_input, 
				main_namespace.ptr(),
				main_namespace.ptr()
			));
		}
		catch(boost::python::error_already_set&)
		{
			std::string s = PyTracebackToString();
			AddOutput(s.c_str());
		}

		delete [] buffer;
	}
}

void App::Eval(const char* script, PN::BaseString& output)
{
	try
	{
		boost::python::str result(boost::python::eval(script, main_namespace, main_namespace));
		output.Add(boost::python::extract<const char*>(result));
		/*boost::python::handle<> ignored(PyRun_String(buffer,
			Py_file_input, 
			main_namespace.ptr(),
			main_namespace.ptr()
		));*/
	}
	catch(boost::python::error_already_set&)
	{
		std::string s = PyTracebackToString();
		AddOutput(s.c_str());
	}
}

void App::Exec(const char* function, const char* param, int flags, PN::BaseString& output)
{
	std::string result;

	try
	{
		PyObject* scope = (flags & extensions::efBuiltIn) ? m_glue.ptr() : main_namespace.ptr();

		if (flags & extensions::efCaptureOutput)
		{
			boost::python::call_method<void>(m_glue.ptr(), "startCapturingStdOut");
			boost::python::call_method<void>(scope, function, param);
		}
		else
		{
			result = boost::python::call_method<std::string>(scope, function, param);
		}
	}
	catch(boost::python::error_already_set&)
	{
		std::string s = PyTracebackToString();
		AddOutput(s.c_str());
	}

	try
	{
		if (flags & extensions::efCaptureOutput)
		{
			std::string outputCapture;
			outputCapture = boost::python::call_method<std::string>(m_glue.ptr(), "finishCapturingStdOut");
			output.Add(outputCapture.c_str());
		}
		else
		{
			output.Add(result.c_str());
		}
	}
	catch(boost::python::error_already_set&)
	{
		std::string s = PyTracebackToString();
		AddOutput(s.c_str());
	}
}

/**
 * Register a script with PN
 */
void App::RegisterScript(const char* scriptname, const char* group, const char* name)
{
	std::string scriptref("python:");
	scriptref += scriptname;
	m_registry->Add(group, name, scriptref.c_str());
}

extensions::IPN* App::GetPN() const
{
	return m_app;
}

boost::python::object& App::PyModule()
{
	return main_module;
}

boost::python::object& App::PyNamespace()
{
	return main_namespace;
}

boost::python::object& App::PyPnGlue()
{
	return m_glue;
}

void App::AddOutput(const char* text, int length)
{
	if(ensureOutput())
	{
		// Yes, more ascii-to-unicode inefficiency here...
		if (length >= 0)
		{
			std::string t(text, length);
			Windows1252_Utf16 textconv(t.c_str());
			m_output->AddToolOutput(textconv, -1);
		}
		else
		{
			Windows1252_Utf16 textconv(text);
			m_output->AddToolOutput(textconv, -1);
		}
	}
}

void App::ClearOutput()
{
	if(ensureOutput())
	{
		m_output->ClearOutput();
	}
}

void App::SetOutputRegEx(const char* regex)
{
	if(ensureOutput())
	{
		m_output->SetToolParser(false, regex);
	}
}

void App::SetOutputDefaultParser()
{
	if(ensureOutput())
	{
		m_output->SetToolParser(true);
	}
}

void App::SetOutputBasePath(const wchar_t* path)
{
	if(ensureOutput())
	{
		m_output->SetToolBasePath(path);
	}
}

bool App::ensureOutput()
{
	if(m_output == NULL)
		m_output = m_app->GetGlobalOutputWindow();

	// We want to show the output only once per script run to avoid flashing...
	if(m_output != NULL && !m_outputShown)
	{
		m_outputShown = true;
		m_output->ShowOutput();
	}
	
	return m_output != NULL;
}

/**
 * Load and run init.py
 */
void App::loadInitScript()
{
	extensions::IOptions* opts = m_app->GetOptionsManager();
	
	const wchar_t* szpnpath = opts->GetPNPath();

	Utf16_Windows1252 pathconv(szpnpath);
	char szpath[MAX_PATH];
	strcpy(szpath, pathconv);
	
	m_app->ReleaseString(szpnpath);

	try
	{
		::PathAppend(szpath, "init.py");

		runFile(szpath);

		m_glue = main_module.attr("glue");
	}
	catch(boost::python::error_already_set&)
	{
		std::string s = PyTracebackToString();
		AddOutput(s.c_str());
	}
}

/**
 * Load a file and run it through the python interpreter
 */
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
				AddOutput(s.c_str());
			}

			delete [] buf;
		}
	}
}