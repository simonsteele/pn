/**
 * @file pn.cpp
 * @brief Main Source File for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"

#include "version.h"

#include "pndocking.h"
#include "MainFrm.h"

#include "extension.h"
#include "extapp.h"

#include "FileAssoc.h"

#include "singleinstance.h"

//#ifdef _DEBUG
	#include "include/mdump.h"
//#endif

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;

	// This allows Debug builds in VS2005 to load dlls
	// that reference Release libraries.
	// #pragma comment(linker, "\"/manifestdependency:type='Win32' name='Microsoft.VC80.CRT' version='8.0.50608.0' processorArchitecture='X86' publicKeyToken='1fc8b3b9a1e18e3b' language='*'\"")
#endif

/**
 * ATL AppModule
 */
CPNAppModule _Module;

/**
 * Global application context object - stores
 * access for all main app objects like the main
 * window, options and that sort of thing.
 */
_Context g_Context;

void pn__Unexpected(LPCTSTR file, int line, LPCTSTR message)
{
	tstring msg = _T("Unexpected error in file: ");
	msg += file;
	msg += _T(", line: ");
	msg += IntToTString(line);
	msg += _T(", version: ");
	
	CA2CT verstring(PN_VERSION);
	msg += verstring;
	msg += _T(".\n");
	msg += message;
	msg += _T("\n\nPlease post this to the forums, or file a bug.");
	::MessageBox(NULL, msg.c_str(), LS(IDR_MAINFRAME), MB_OK | MB_ICONWARNING);
}

HWND GetCurrentEditor()
{
	return static_cast<CMDIWindow*>(g_Context.m_frame->GetWindow())->MDIGetActive();
}

int Run(LPTSTR /*lpstrCmdLine*/ = NULL, int nCmdShow = SW_SHOWDEFAULT)
{
	MiniDumper dumper(_T("PN2_") PN_VERSTRING_T);

	// Store the current OS version
	ZeroMemory(&g_Context.OSVersion, sizeof(OSVERSIONINFO));
	g_Context.OSVersion.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	::GetVersionEx(&g_Context.OSVersion);

	// This is our multiple instance checker...
	MultipleInstanceManager checkMI( _T("{FCA6FB45-3224-497a-AC73-C30E498E9ADA}") );
	g_Context.m_miManager = &checkMI;

	// Create the App object thus initialising options and extension interfaces, amongst other bits
	App* theApp = new App();
	g_Context.ExtApp = theApp;

	// See if we allow multiple instances
	bool bAllowMulti = OPTIONS->Get(PNSK_INTERFACE, _T("AllowMultiInstance"), false);

	// Command-line argument parsing:
	std::list<tstring>* cmdLine = new std::list<tstring>();
	*cmdLine = GetCommandLineArgs();

	for(std::list<tstring>::const_iterator i = cmdLine->begin();
		i != cmdLine->end();
		++i)
	{
		const tstring& arg = (*i);
		if( arg.size() > 2 && ((arg[0] == _T('-')) || (arg[0] == _T('/'))) )
		{
			// command-line arg:
			if(_tcsicmp(&arg.c_str()[1], _T("-reset")) == 0)
			{
				// Clear user settings to get around broken schemes or user settings
				return theApp->ClearUserData() ? 0 : 1;
			}
			else if(_tcsicmp(&arg.c_str()[1], _T("-cleancschemes")) == 0)
			{
				theApp->Init();
				return theApp->CompileSchemes() ? 0 : 1;
			}
			else if(_tcsicmp(&arg.c_str()[1], _T("-checkassoc")) == 0)
			{
				// Check file associations
				FileAssocManager fam;
				if(fam.CheckAssociations())
				{
					fam.UpdateAssociations();
				}
			}
			else if(_tcsicmp(&arg.c_str()[1], _T("-allowmulti")) == 0)
			{
				// Override multiple instance code
				bAllowMulti = true;
			}
			else if(_tcsicmp(&arg.c_str()[1], _T("-exit")) == 0)
			{
				// Force exit after running command-line args
				return 0;
			}
			else if(_tcsicmp(&arg.c_str()[1], _T("-safemode")) == 0)
			{
				theApp->SetCanLoadExtensions(false);
			}
			else if(_tcsicmp(&arg.c_str()[1], _T("-findexts")) == 0)
			{
				return theApp->FindExtensions();
			}
			else if(_tcscmp(&arg.c_str()[1], _T("-upgrade")) == 0)
			{
				theApp->FindExtensions();
				theApp->Init();
				SchemeManager::GetInstance()->Compile();
				return 0;
			}
		}
	}

	// Check multi instances
	if(!bAllowMulti)
	{
		if(checkMI.AlreadyActive())
		{
			LOG( _T("PN2 has an instance already, sending parameters and exiting") );
			checkMI.SendParameters();

			return 0;
		}
	}

	_Module.m_ShellAllocator.Init();

	CMessageLoop theLoop;
	_Module.AddMessageLoop(&theLoop);

	// Load scheme types and do other pre-run init.
	theApp->Init();

	// Set up the main window for the app.
	CMainFrame wndMain(&theApp->GetCommandDispatch(), cmdLine);
	g_Context.m_frame = static_cast<IMainFrame*>(&wndMain);

	if(wndMain.CreateEx() == NULL)
	{
		ATLTRACE(_T("Main window creation failed!\n"));
		return 0;
	}

	// Using nCmdShow here stops us from setting the window to
	// maximised in the Create function. This does break externally
	// specifying maximised state, though.

	// Don't show here, we'll show in the OnInitialiseFrame message of the main wnd.
	//STARTUPINFO si;
	//GetStartupInfo(&si);
	//if( (si.wShowWindow & SW_MAXIMIZE) == SW_MAXIMIZE )
	//	wndMain.ShowWindow(SW_MAXIMIZE);
	//else
	//	wndMain.ShowWindow(SW_SHOW/*nCmdShow*/);

	// Signal that we're ready to be able to handle command-line parameters from other PN instances
	checkMI.AllowParameters();

	int nRet = theLoop.Run();

	delete theApp;

	_Module.RemoveMessageLoop();

	_Module.m_ShellAllocator.Term();

	return nRet;
}

int WINAPI _tWinMain(HINSTANCE hInstance, HINSTANCE /*hPrevInstance*/, LPTSTR lpstrCmdLine, int nCmdShow)
{
	// If you are running on NT 4.0 or higher you can use the following call instead to 
	// make the EXE free threaded. This means that calls come in on a random RPC thread.
	//	HRESULT hRes = ::CoInitializeEx(NULL, COINIT_MULTITHREADED);
	//HRESULT hRes = ::CoInitialize(NULL);
	//HRESULT hRes = ::CoInitializeEx(NULL, COINIT_MULTITHREADED);

	// We now use OleInitialize so that we can use IDropTarget for the projects view 
	// (and maybe other stuff later).
	HRESULT hRes = ::OleInitialize(NULL);
	ATLASSERT(SUCCEEDED(hRes));

#ifdef _DEBUG
	//_CrtSetBreakAlloc(5371);
#endif

#if (_WIN32_IE >= 0x0300)
	INITCOMMONCONTROLSEX iccx;
	iccx.dwSize = sizeof(iccx);
	iccx.dwICC = ICC_COOL_CLASSES | ICC_BAR_CLASSES | ICC_USEREX_CLASSES;
	BOOL bRet = ::InitCommonControlsEx(&iccx);
	bRet;
	ATLASSERT(bRet);
#else
	::InitCommonControls();
#endif

	hRes = _Module.Init(NULL, hInstance);
	ATLASSERT(SUCCEEDED(hRes));

	int nRet = Run(lpstrCmdLine, nCmdShow);

	_Module.Term();
	::CoUninitialize();

#ifdef _DEBUG
	_CrtDumpMemoryLeaks();
#endif

	return 0;
}
