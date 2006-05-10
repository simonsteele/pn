/**
 * @file extapp.cpp
 * @brief Implement IPN
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

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
		(*i)->OnNewDocument(doc);
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

ITextOutput* App::GetGlobalOutputWindow()
{
	return g_Context.m_frame->GetGlobalOutputWindow();
}

}