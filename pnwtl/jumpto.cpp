#include "stdafx.h"
#include "resource.h"
#include "childfrm.h"
#include "plugins.h"
#include "outputview.h"
#include "jumpto.h"

#include "include/filefinder.h"

//////////////////////////////////////////////////////////////////////////////
// JumpToPlugin
//////////////////////////////////////////////////////////////////////////////

JumpToPlugin::JumpToPlugin(LPCTSTR filename) : Plugin(filename)
{
	pfnGetCaps = NULL;
	pfnGetSchemes = NULL;
	pfnGetMethods = NULL;
	
	if(Plugin::Valid())
	{
		pfnGetCaps = (LPFnGetCapabilities)FindFunction("PNGetCapabilities");

		if(!pfnGetCaps)
			return;

		int caps = pfnGetCaps();
		if((caps & PNCAPS_FUNCTIONFINDER) == 0)
		{
			unload();
			return;
		}

		pfnGetSchemes = (LPFnGetSchemesSupported)FindFunction("PNFPGetSchemesSupported");
		pfnGetMethods = (LPFnGetMethods)FindFunction("PNFPGetMethods");

		if(pfnGetMethods == NULL || pfnGetSchemes == NULL)
		{
			unload();
		}
	}
}

bool JumpToPlugin::Valid()
{
	bool valid = Plugin::Valid;

	return valid && (pfnGetMethods != NULL);
}

tstring JumpToPlugin::GetSchemesSupported()
{
	if(!Valid())
		return tstring("");

	wchar_t buffer[200];
	pfnGetSchemes(buffer, 200);

	USES_CONVERSION;
	return tstring(CW2T(buffer));
}

bool JumpToPlugin::GetMethods(const wchar_t* filename, HWND editorWnd, FP_CALLBACK callback)
{
	if(!Valid())
		return false;

	return pfnGetMethods(filename, editorWnd, callback);
}

//////////////////////////////////////////////////////////////////////////////
// JumpToHandler
//////////////////////////////////////////////////////////////////////////////

JumpToHandler::JumpToHandler()
{
	FileFinder<JumpToHandler> finder(this, &JumpToHandler::LoadHandler);

	tstring pnpath;
	COptionsManager::GetInstance()->GetPNPath(pnpath, PNPATH_TAGGERS);

	finder.Find(pnpath.c_str(), _T("*.dll"), false);
}

JumpToHandler::~JumpToHandler()
{
	for(PLUGINS_LIST::iterator i = plugins.begin(); i != plugins.end(); ++i)
	{
		delete (*i);
	}
	plugins.clear();
	handlers.clear();
}

void JumpToHandler::LoadHandler(LPCTSTR path, LPCTSTR filename)
{
	tstring fn = path;
	fn += filename;

	JumpToPlugin* pPlugin = new JumpToPlugin(fn.c_str());

	if(pPlugin->Valid())
	{
		plugins.insert(plugins.end(), pPlugin);
		
		tstring supportedSchemes = pPlugin->GetSchemesSupported();
		
		if(supportedSchemes.size())
		{
			TCHAR* buf = new TCHAR[supportedSchemes.size()+1];
			_tcscpy(buf, supportedSchemes.c_str());
			
			TCHAR* p = _tcstok(buf, _T(";"));
			while(p)
			{
				handlers.insert(HANDLERS_MAP::value_type(tstring(p), pPlugin));
				p = _tcstok(NULL, _T(";"));
			}
			
			delete [] buf;
		}		
	}
	else
		delete pPlugin;
}

void JumpToHandler::DoJumpTo(CChildFrame* pChildFrame)
{
	// First let's find out what scheme it is...
	CScheme* pScheme = pChildFrame->GetTextView()->GetCurrentScheme();
	tstring schemeName = pScheme->GetName();
	
	JumpToPlugin* pPlugin = NULL;

	HANDLERS_MAP::iterator iHandler = handlers.find(schemeName);
	if(iHandler != handlers.end())
	{
		pPlugin = (*iHandler).second;
	}

	if(!pPlugin)
		return;

	tstring fn = pChildFrame->GetFileName();

	outputWindow = pChildFrame->GetOutputWindow();

	USES_CONVERSION;

	const wchar_t* pFN = CT2CW(fn.c_str());

	pPlugin->GetMethods(pFN, pChildFrame->m_hWnd, &JumpToHandler::callback);
}

void __stdcall JumpToHandler::callback(int dataCount, LPMETHODINFO methodInfo)
{
	JumpToHandler::GetInstance()->outputWindow->AddToolOutput(methodInfo->methodName);
	JumpToHandler::GetInstance()->outputWindow->AddToolOutput(_T("\n"));
}