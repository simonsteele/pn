#include "stdafx.h"
#include "extiface.h"
#include "extapp.h"

#include "scriptregistry.h"

#include "resource.h"
#include "childfrm.h"

namespace extensions {

unsigned int App::get_iface_version() const
{
	return PN_EXT_IFACE_VERSION;
}

const char* App::get_version() const
{
	return PN_VERSTRING;
}

void App::AddEventSink(IAppEventSinkPtr sink)
{
	m_sinks.push_back(sink);
}

void App::RemoveEventSink(IAppEventSinkPtr sink)
{
	m_sinks.remove(sink);
}

IScriptRegistry* App::GetScriptRegistry()
{
	return ScriptRegistry::GetInstance();
}

IOptions* App::GetOptionsManager()
{
	return OPTIONS;
}

void App::OnNewDocument(IDocumentPtr doc)
{
	for(EventSinkList::const_iterator i = m_sinks.begin(); i != m_sinks.end(); ++i)
	{
		(*i)->on_new_document(doc);
	}
}

IDocumentPtr App::GetCurrentDocument()
{
	CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor());
	if(pChild)
	{
		return pChild->GetDocument();
	}
	
	return IDocumentPtr();
}

}