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

	virtual void RunScript(const char* name);

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
		return main_namespace;
	}

	boost::python::object& PyPnGlue()
	{
		return m_glue;
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
	boost::python::object m_glue;
};

extern App* g_app;

#endif // #ifndef app_h__included