#ifndef extapp_h__included
#define extapp_h__included

namespace extensions
{

class App : public IPN
{
	typedef std::list<IAppEventSinkPtr> EventSinkList;
public:
	virtual unsigned int get_iface_version() const
	{
		return PN_EXT_IFACE_VERSION;
	}

	virtual const char* get_version() const
	{
		return PN_VERSTRING;
	}
	
	virtual void AddEventSink(IAppEventSinkPtr sink)
	{
		m_sinks.push_back(sink);
	}

	virtual void RemoveEventSink(IAppEventSinkPtr sink)
	{
		m_sinks.remove(sink);
	}

	virtual IScriptRegistry* GetScriptRegistry();

// Stuff to signal event sinks...
public:
	void OnNewDocument(IDocumentPtr doc)
	{
		for(EventSinkList::const_iterator i = m_sinks.begin(); i != m_sinks.end(); ++i)
		{
			(*i)->on_new_document(doc);
		}
	}


private:
	 EventSinkList m_sinks;
};

}

#endif