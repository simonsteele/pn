/**
 * @file editorfactory.cpp
 * @brief Create Editors
 * @author Simon Steele
 * @note Copyright (c) 2008-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "editorFactory.h"
#include "childfrm.h"
#include "extapp.h"
#include "autocompletemanager.h"

EditorFactory::EditorFactory(CommandDispatch* pCommandDispatch, TextClips::TextClipsManager* pClipManager, HWND hWndMDIClient) :
	m_hWndMDIClient(hWndMDIClient),
	m_pClipManager(pClipManager),
	m_pCommandDispatch(pCommandDispatch),
	m_AutoComplete(new AutoCompleteManager())
{
}

CChildFrame* EditorFactory::Default()
{
	DocumentPtr pD;
	CChildFrame* pChild = createChild(pD);
	notifyChild(pD);
	return pChild;
}

CChildFrame* EditorFactory::FromFile(LPCTSTR pathname, Scheme* pScheme, EPNEncoding encoding, bool& bOpened)
{
	bOpened = false;

	DocumentPtr pD;
	CChildFrame* pChild = createChild(pD);
	if (pathname)
	{
		notifyChild(pD);

		bOpened = pChild->PNOpenFile(pathname, pScheme, encoding);
	}

	return pChild;
}

CChildFrame* EditorFactory::WithScheme(Scheme* pScheme)
{
	DocumentPtr pD;
	CChildFrame* pChild = createChild(pD);
	pChild->SetScheme(pScheme);
	notifyChild(pD);
 	return pChild;
}

void EditorFactory::SetMdiClient(HWND mdiClient)
{
	m_hWndMDIClient = mdiClient;
}

CChildFrame* EditorFactory::createChild(DocumentPtr& pD)
{
	pD.reset(new Document());
	CChildFrame* pChild = new CChildFrame(pD, m_pCommandDispatch, m_pClipManager, m_AutoComplete.get());
	PNASSERT(pChild != NULL);
	pD->AddChildFrame(pChild);

	// Give the user the option to always maximise new windows.
	bool bMax = OPTIONS->GetCached(Options::OMaximiseNew) != 0;
	pChild->CreateEx(m_hWndMDIClient, 0, 0, bMax ? WS_MAXIMIZE : 0);

	return pChild;
}

void EditorFactory::notifyChild(DocumentPtr& pD)
{
	g_Context.ExtApp->OnNewDocument( pD );
}