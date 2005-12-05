#pragma once

class App
{
public:
	App(boost::python::handle<>& obj) : main_module(obj)
	{
		main_namespace = main_module.attr("__dict__");

		struct _stat statbuf;
		int result;
		result = _stat( "c:\\projects\\pnpython\\bin\\init.py", &statbuf );
		if(result == 0)
		{
			FILE* init_script = fopen("c:\\projects\\pnpython\\bin\\init.py", "r");
			
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
					std::string s = getPythonErrorString();
					OutputDebugString(s.c_str());
				}

				delete [] buf;
			}
		}
	}

	boost::python::object main_module;
	boost::python::object main_namespace;
};

extern App* g_app;