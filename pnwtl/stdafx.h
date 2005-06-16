/**
 * @file stdafx.h
 * @brief Pre-compiled header file.
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 * 
 * Notes: 
 * Include file for standard system include files, or project specific 
 * include files that are used frequently, but are changed infrequently.
 */

#if !defined(pn2_stdafx_h__included)
#define pn2_stdafx_h__included

// Change these values to use different versions
#define WINVER			0x0500	// Changed to allow CDotNetTabCtrl to compile with COLOR_HOTLIGHT
#define _WIN32_WINNT	0x0501
#define _WIN32_IE		0x0501
#define _RICHEDIT_VER	0x0100

// Disable the "unreferenced formal parameter" warning. I see no reason for it.
#pragma warning( disable: 4100 )

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
	extern "C" const int _fltused = 0;
#endif

#include <atlwin.h>
#include <atlapp.h>

extern CAppModule _Module;

#include <atlmisc.h>
#include <atlddx.h>

#include <atlctrls.h>
#include <atldlgs.h>
#include <atlframe.h>
#include <atlctrlw.h>
#include <atlctrlx.h>
#include <atltheme.h>

#include <list>
#include <string>
typedef std::basic_string<TCHAR> tstring;

//Boosty Goodness
#include <boost/config.hpp>
#include <boost/shared_ptr.hpp>

#define PNASSERT ATLASSERT

#ifdef _DEBUG
	#define _NO_COPY(x) private: x(x&){;} x& operator = (x& copy){;}
#else
	#define _NO_COPY(x) /##/
#endif

// Lots of good stuff in here...
#include "pn.h"

#endif // !defined(pn2_stdafx_h__included)
