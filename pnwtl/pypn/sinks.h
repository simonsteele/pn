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

class AppSink : public extensions::IAppEventSink
{
public:
	AppSink(extensions::IPN* app) : m_app(app)
	{
	}

	virtual void on_new_document(extensions::IDocumentPtr doc)
	{
		extensions::IDocumentEventSinkPtr p(new DocSink(doc));
		doc->AddEventSink(p);
	}

	extensions::IPN* GetPN() const
	{
		return m_app;
	}

private:
	extensions::IPN* m_app;
};

#endif //#ifndef sinks_h__included