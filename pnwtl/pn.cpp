// pn.cpp : main source file for pn.exe
//

#include "stdafx.h"
#include "resource.h"

#include <atlframe.h>
#include <atlctrls.h>
#include <atldlgs.h>
#include <atlctrlw.h>
#include <atlctrlx.h>

#include "TabbingFramework\atlgdix.h"
#include "TabbingFramework\CoolTabCtrls.h"
#include "TabbingFramework\TabbedFrame.h"
#include "TabbingFramework\TabbedMDI.h"

// Utility Classes and Definitions:
#include "pntypes.h"
#include "callback.h"
#include "ssfiledialog.h"

#include "pndialogs.h"

#include "ScintillaWTL.h"
#include "textview.h"
#include "aboutdlg.h"
#include "ChildFrm.h"

#include "finddlg.h"

#include "MainFrm.h"

CAppModule _Module;
CPNAppState theApp;


HWND GetCurrentEditor(CWindow* pMDIFrameWnd)
{
	return ((CMDIWindow*)pMDIFrameWnd)->MDIGetActive();
}

/////////////////////////////////////////////////////////////////////////
// CPNAppState
/////////////////////////////////////////////////////////////////////////

CPNAppState::CPNAppState()
{
	// Where are the Schemes stored?
	TCHAR *buf = new TCHAR[MAX_PATH +1];
	GetModuleFileName(NULL, buf, MAX_PATH);
	ctcString csPath(buf);
	delete [] buf;
	int cutoff = csPath.rfind(_T('\\'));
	csPath = csPath.substr(0, cutoff+1);
	csPath += "Schemes\\";

	m_FindOptions.Direction = true;
	m_FindOptions.Loop = true;
	m_FindOptions.FindText = _T("");
	
	m_ReplaceOptions.Direction = true;
	m_ReplaceOptions.Loop = true;
	m_ReplaceOptions.FindText = _T("");
	m_ReplaceOptions.ReplaceText = _T("");
	
	m_Schemes.SetPath(csPath.c_str());
	m_Schemes.SetCompiledPath(csPath.c_str());
	m_Schemes.Load();
}

CPNAppState::~CPNAppState()
{

}

int Run(LPTSTR /*lpstrCmdLine*/ = NULL, int nCmdShow = SW_SHOWDEFAULT)
{
	CMessageLoop theLoop;
	_Module.AddMessageLoop(&theLoop);

	CMainFrame wndMain;

	if(wndMain.CreateEx() == NULL)
	{
		ATLTRACE(_T("Main window creation failed!\n"));
		return 0;
	}

	wndMain.ShowWindow(nCmdShow);

	int nRet = theLoop.Run();

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
	iccx.dwICC = ICC_COOL_CLASSES | ICC_BAR_CLASSES;
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

	return nRet;
}
