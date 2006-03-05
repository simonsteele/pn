#include "stdafx.h"
#include "resource.h"
#include "childfrm.h"
#include "plugins.h"
#include "outputview.h"
#include "jumpto.h"

#include "include/filefinder.h"
#include "include/tempfile.h"

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

bool JumpToPlugin::GetMethods(const wchar_t* filename, HWND editorWnd, FP_CALLBACK callback, int mask, const wchar_t* scheme, LPVOID cookie)
{
	if(!Valid())
		return false;

	return pfnGetMethods(filename, editorWnd, callback, mask, scheme, cookie);
}

//////////////////////////////////////////////////////////////////////////////
// JumpToHandler
//////////////////////////////////////////////////////////////////////////////

JumpToHandler::JumpToHandler()
{
	FileFinder<JumpToHandler> finder(this, &JumpToHandler::LoadHandler);

	tstring pnpath;
	OPTIONS->GetPNPath(pnpath, PNPATH_TAGGERS);

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

void JumpToHandler::DoJumpTo(CChildFrame* pChildFrame, IJumpToFindSink* pNotifySink)
{
	USES_CONVERSION;

	// First let's find out what scheme it is...
	Scheme* pScheme = pChildFrame->GetTextView()->GetCurrentScheme();
	tstring schemeName = pScheme->GetName();
	
	JumpToPlugin* pPlugin = NULL;

	HANDLERS_MAP::iterator iHandler = handlers.find(schemeName);
	if(iHandler != handlers.end())
	{
		pPlugin = (*iHandler).second;
	}

	if(!pPlugin)
		return;

	tstring fn;
	//const wchar_t* pFN = NULL;
	TempFileName* tfn = NULL;

	std::wstring fnstr;

	if(pChildFrame->GetModified() || !pChildFrame->CanSave())
	{
		if(pChildFrame->CanSave())
		{
			tfn = new TempFileName(pChildFrame->GetFileName().c_str(), NULL, true, true);
			fnstr = tfn->w_str();
		}
		else
		{
			tfn = new TempFileName(NULL, _T(".tmp"), true);
			fnstr = tfn->w_str();
		}

		pChildFrame->GetTextView()->SaveFile(tfn->t_str());
	}
	else
	{
		fn = pChildFrame->GetFileName();
		fnstr = CT2CW(fn.c_str());
	}

	const wchar_t* pSN = CT2CW(pChildFrame->GetTextView()->GetCurrentScheme()->GetName());

	sink = pNotifySink;
	
	if(!pPlugin->GetMethods(fnstr.c_str(), pChildFrame->m_hWnd, &JumpToHandler::callback, TAGM_ALL, pSN, (LPVOID)this))
	{
		g_Context.m_frame->SetStatusText(_T("Failed to run tagger."));
	}

	if(tfn != NULL)
	{
		tfn->erase();
		delete tfn;
	}
}

void __stdcall JumpToHandler::callback(int dataCount, LPMETHODINFO methodInfo, LPVOID cookie)
{
	static_cast<JumpToHandler*>(cookie)->sink->OnFound(dataCount, methodInfo);
}