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

//#include "callback.h"

#include "pnutils.h"

//#include "tools.h"

#include "MainFrm.h"

CAppModule _Module;

__declspec( thread ) _Context g_Context = {0};

HWND GetCurrentEditor()
{
	return static_cast<CMDIWindow*>(g_Context.m_frame->GetWindow())->MDIGetActive();
}

void Init()
{
	// Where are the Schemes stored?
	tstring path;
	tstring cpath;
	COptionsManager::GetInstance()->GetSchemesPaths(path, cpath);

	CSchemeManager& SM = CSchemeManager::GetInstanceRef();
	SM.SetPath(path.c_str());
	SM.SetCompiledPath(cpath.c_str());
	SM.Load();
}

void Shutdown()
{
	CSchemeManager::DeleteInstance();
	COptionsManager::DeleteInstance();
	CSMenuManager::ReleaseInstance();

	DeletionManager::DeleteAll();
}

int Run(LPTSTR /*lpstrCmdLine*/ = NULL, int nCmdShow = SW_SHOWDEFAULT)
{
	CMessageLoop theLoop;
	_Module.AddMessageLoop(&theLoop);

	CMainFrame wndMain;
	g_Context.m_frame = static_cast<IMainFrame*>(&wndMain);

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
	HRESULT hRes = ::CoInitialize(NULL);
// If you are running on NT 4.0 or higher you can use the following call instead to 
// make the EXE free threaded. This means that calls come in on a random RPC thread.
//	HRESULT hRes = ::CoInitializeEx(NULL, COINIT_MULTITHREADED);
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
