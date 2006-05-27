/**
 * @file pn.cpp
 * @brief Main Source File for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-2006 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"

#include "version.h"

#include "pndocking.h"
#include "MainFrm.h"

#include "extension.h"
#include "extapp.h"

//#ifdef _DEBUG
	#include "include/mdump.h"
//#endif

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

CAppModule _Module;

/*__declspec( thread )*/ _Context g_Context /*= {0}*/;

void pn__Unexpected(LPCTSTR file, int line, LPCTSTR message)
{
	tstring msg = _T("Unexpected error in file: ");
	msg += file;
	msg += _T(", line: ");
	msg += IntToTString(line);
	msg += _T(", version: ");
	msg += PN_VERSION;
	msg += _T(".\n");
	msg += message;
	msg += _T("\n\nPlease e-mail this information to ss@pnotepad.org.");
	::MessageBox(NULL, msg.c_str(), LS(IDR_MAINFRAME), MB_OK | MB_ICONWARNING);
}

HWND GetCurrentEditor()
{
	return static_cast<CMDIWindow*>(g_Context.m_frame->GetWindow())->MDIGetActive();
}

int Run(LPTSTR /*lpstrCmdLine*/ = NULL, int nCmdShow = SW_SHOWDEFAULT)
{
//#ifdef _DEBUG
	tstring str(_T("PN2_"));
	str += PN_VERSTRING;

	MiniDumper dumper(str.c_str());
//#endif
	
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
	if(!bAllowMulti)
	{
		LOG( _T("PN2 should only run one instance") );
		if(checkMI.AlreadyActive())
		{
			LOG( _T("PN2 has an instance already, sending parameters and exiting") );
			//GetAsyncKeyState() - use to see if shift is down.
			checkMI.SendParameters();

			return 0;
		}
	}
	else
	{
		LOG( _T("PN2 is allowed multiple instances") );
	}

	CMessageLoop theLoop;
	_Module.AddMessageLoop(&theLoop);

	// Load scheme types and do other pre-run init.
	theApp->Init();

	// Set up the main window for the app.
	CMainFrame wndMain(&theApp->GetCommandDispatch());
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

	int nRet = theLoop.Run();

	delete theApp;

	_Module.RemoveMessageLoop();

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
	//_CrtSetBreakAlloc(3174);
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

	return nRet;
}
