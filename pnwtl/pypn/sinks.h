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

#endif //#ifndef sinks_h__included