#ifndef PN_WIN_COREINCLUDES_
#define PN_WIN_COREINCLUDES_

#pragma once

// Change these values to use different versions
#define WINVER			0x0500	// Changed to allow CDotNetTabCtrl to compile with COLOR_HOTLIGHT
#define _WIN32_WINNT	0x0600
#define _WIN32_IE		0x0600
#define _RICHEDIT_VER	0x0100

// Disable the "unreferenced formal parameter" warning. I see no reason for it.
#pragma warning( disable: 4100 )

// Disable the CRT deprecation warnings in VS 2005
#define _CRT_SECURE_NO_DEPRECATE
#define _SCL_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_WARNINGS

// Implement debug mode memory allocation checking.
#ifdef _DEBUG
	#define CRTDBG_MAP_ALLOC
	#include <stdlib.h>
	#include <crtdbg.h>
	#define DEBUG_NEW  new(_NORMAL_BLOCK, THIS_FILE, __LINE__)
#endif // #ifdef _DEBUG

#define _ATL_FREE_THREADED

#include <atlbase.h>

#if (_ATL_VER >= 0x0700)
	#include <atlstr.h>
	#include <atltypes.h>
	#define _WTL_NO_CSTRING
	#define _WTL_NO_WTYPES
	#define _WTL_NO_UNION_CLASSES
	//extern "C" const int _fltused = 0;
#endif

#include <atlwin.h>
#include <atlapp.h>
#include "include/atlshellextbase.h"

class CPNAppModule : public CAppModule
{
public:
	CShellMalloc	m_ShellAllocator;
};

extern CPNAppModule _Module;

#include <atlmisc.h>
#include <atlddx.h>

#include <atlctrls.h>
#include <atldlgs.h>
#include <atlframe.h>
#include <atlctrlw.h>
#include <atlctrlx.h>
#include <atltheme.h>
#include <shlwapi.h>
#include <shellapi.h>

#define PNASSERT ATLASSERT

typedef unsigned __int64	uint64_t;
typedef __int64				int64_t;

#endif PN_WIN_CORE_COREINCLUDES_
