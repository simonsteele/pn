#ifndef app_h__included
#define app_h__included

class App : 
	public extensions::IAppEventSink,
	public extensions::IScriptRunner
{
public:
	App(boost::python::handle<>& obj, extensions::IPN* app);

	virtual ~App();

	void Initialise();

	virtual void on_new_document(extensions::IDocumentPtr doc)
	{
		extensions::IDocumentEventSinkPtr p(new DocSink(doc));
		doc->AddEventSink(p);
	}

	virtual void RunScript(const char* name)
	{
		std::string s("RunScript: ");
		s += name;
		s += "\n";
		::OutputDebugString(s.c_str());
	}

	extensions::IPN* GetPN() const
	{
		return m_app;
	}

	boost::python::object& PyModule()
	{
		return main_module;
	}

	boost::python::object& PyNamespace()
	{
		return main_module;
	}

	void RegisterScript(const char* scriptname, const char* group, const char* name);

protected:
	void loadInitScript();
	void runFile(const char* filename);
	
private:
	extensions::IScriptRegistry* m_registry;
	extensions::IPN* m_app;
	boost::python::object main_module;
	boost::python::object main_namespace;
};

extern App* g_app;

#endif // #ifndef app_h__included