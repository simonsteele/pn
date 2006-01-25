#ifndef sinks_h__included
#define sinks_h__included

class DocSink : public extensions::IDocumentEventSink
{
public:
	DocSink(extensions::IDocumentPtr doc);
	virtual ~DocSink();

	virtual void OnDocClosing();
	virtual void OnCharAdded(char c);
	virtual void OnSchemeChange(const char *scheme){}

private:
	extensions::IDocumentPtr m_doc;
};

class AppSink : public extensions::IAppEventSink,
	public extensions::IScriptRunner
{
public:
	AppSink(extensions::IPN* app) : m_app(app)
	{
		m_registry = m_app->GetScriptRegistry();
		if(m_registry)
			m_registry->RegisterRunner("python", this);

		m_registry->Add("test", "Test Script", "python:test.testScript");
	}

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

private:
	extensions::IScriptRegistry* m_registry;
	extensions::IPN* m_app;
};

#endif //#ifndef sinks_h__included