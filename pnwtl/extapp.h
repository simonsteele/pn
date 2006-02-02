#ifndef extapp_h__included
#define extapp_h__included

namespace extensions
{

class App : public IPN
{
	typedef std::list<IAppEventSinkPtr> EventSinkList;
public:
	virtual unsigned int get_iface_version() const;
	virtual const char* get_version() const;
	
	virtual void AddEventSink(IAppEventSinkPtr sink);
	virtual void RemoveEventSink(IAppEventSinkPtr sink);
	virtual IScriptRegistry* GetScriptRegistry();
	virtual IOptions* GetOptionsManager();

	virtual IDocumentPtr GetCurrentDocument();

// Stuff to signal event sinks...
public:
	void OnNewDocument(IDocumentPtr doc);

private:
	 EventSinkList m_sinks;
};

}

#endif