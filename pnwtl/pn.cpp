/**
 * @file pn.cpp
 * @brief Main Source File for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"

#include "version.h"
#include "pnutils.h"

#include "appsettings.h"

#include "pndocking.h"
#include "MainFrm.h"

//#ifdef _DEBUG
	#include "include/mdump.h"
//#endif

CAppModule _Module;

/*__declspec( thread )*/ _Context g_Context = {0};

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
	::MessageBox(NULL, msg.c_str(), _T("Programmers Notepad 2"), MB_OK | MB_ICONWARNING);
}

HWND GetCurrentEditor()
{
	return static_cast<CMDIWindow*>(g_Context.m_frame->GetWindow())->MDIGetActive();
}

void Init()
{
	// Where are the Schemes stored?
	tstring path;
	tstring cpath;
	OPTIONS->GetPNPath(path, PNPATH_SCHEMES);
	OPTIONS->GetPNPath(cpath, PNPATH_COMPILEDSCHEMES);

	CSchemeManager& SM = CSchemeManager::GetInstanceRef();
	SM.SetPath(path.c_str());
	SM.SetCompiledPath(cpath.c_str());
	SM.Load();
}

void Shutdown()
{
	CSchemeManager::DeleteInstance();
	CSMenuManager::ReleaseInstance();

	DeletionManager::DeleteAll();

	// Free up the options object, thus storing the options.
	OptionsFactory::Release(g_Context.options);
	g_Context.options = NULL;
}

int Run(LPTSTR /*lpstrCmdLine*/ = NULL, int nCmdShow = SW_SHOWDEFAULT)
{
//#ifdef _DEBUG
	tstring str(_T("PN2_"));
	str += PN_VERSTRING;

	MiniDumper dumper(str.c_str());
//#endif

	CString appTitle;
	appTitle.LoadString(_Module.m_hInst, IDR_MAINFRAME);
	g_Context.AppTitle = appTitle;
	
	ZeroMemory(&g_Context.OSVersion, sizeof(OSVERSIONINFO));
	g_Context.OSVersion.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	::GetVersionEx(&g_Context.OSVersion);

	// This is our multiple instance checker...
	MultipleInstanceManager checkMI( _T("{FCA6FB45-3224-497a-AC73-C30E498E9ADA}") );
	g_Context.m_miManager = &checkMI;

	// This loads some global app settings, including what to
	// use as the options store and where user settings files are
	// to be stored.
	AppSettings as;

	// This is our options object.
	g_Context.options = OptionsFactory::GetOptions(as.GetOptionsType());
	
	// See if there's a custom user settings dir.
	if(as.HaveUserPath())
		g_Context.options->SetUserSettingsPath(as.GetUserPath());

	// Now ensure the user settings directory is available!
	tstring usPath;
	OPTIONS->GetPNPath(usPath, PNPATH_USERSETTINGS);
	if(!CreateDirectoryRecursive(usPath.c_str()))
		UNEXPECTED(_T("Could not create user settings folder"));
	
	// See if we allow multiple instances...
	bool bAllowMulti = OPTIONS->Get(PNSK_INTERFACE, _T("AllowMultiInstance"), true);
	if(!bAllowMulti)
	{
		if(checkMI.AlreadyActive())
		{
			//GetAsyncKeyState() - use to see if shift is down.
			checkMI.SendParameters();

			return 0;
		}
	}

	CMessageLoop theLoop;
	_Module.AddMessageLoop(&theLoop);

	// Set up the main window for the app.
	CMainFrame wndMain;
	g_Context.m_frame = static_cast<IMainFrame*>(&wndMain);

	// Load scheme types and do other pre-run init.
	Init();

	if(wndMain.CreateEx() == NULL)
	{
		ATLTRACE(_T("Main window creation failed!\n"));
		return 0;
	}

	// Using nCmdShow here stops us from setting the window to
	// maximised in the Create function. This does break externally
	// specifying maximised state, though.

	STARTUPINFO si;
	GetStartupInfo(&si);
	if( (si.wShowWindow & SW_MAXIMIZE) == SW_MAXIMIZE )
		wndMain.ShowWindow(SW_MAXIMIZE);
	else
		wndMain.ShowWindow(SW_SHOW/*nCmdShow*/);

	int nRet = theLoop.Run();

	Shutdown();

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
