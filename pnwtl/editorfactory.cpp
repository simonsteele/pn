#include "stdafx.h"
#include "editorFactory.h"
#include "childfrm.h"
#include "extapp.h"

EditorFactory::EditorFactory(CommandDispatch* pCommandDispatch, TextClips::TextClipsManager* pClipManager, HWND hWndMDIClient) :
	m_hWndMDIClient(hWndMDIClient),
	m_pClipManager(pClipManager),
	m_pCommandDispatch(pCommandDispatch)
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
	CChildFrame* pChild = new CChildFrame(pD, m_pCommandDispatch, m_pClipManager);
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