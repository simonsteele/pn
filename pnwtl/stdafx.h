// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#if !defined(AFX_STDAFX_H__41A3A8B3_1419_494D_BBD8_4394DCE4A180__INCLUDED_)
#define AFX_STDAFX_H__41A3A8B3_1419_494D_BBD8_4394DCE4A180__INCLUDED_

// Change these values to use different versions
#define WINVER		0x0400
#define _WIN32_IE	0x0400
#define _RICHEDIT_VER	0x0100

#define _WIN32_WINNT 0x0500

#include <atlbase.h>
#include <atlapp.h>

extern CAppModule _Module;

#include <atlwin.h>
#include <atlmisc.h>
#include <atlddx.h>
//#include <atlcrack.h>

#include <string>

#ifndef ctcString
	#ifdef UNICODE
		#define ctcString std::wstring
	#else
		#define ctcString std::string
	#endif
#endif

#include "pn.h"

extern CPNAppState theApp;

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STDAFX_H__41A3A8B3_1419_494D_BBD8_4394DCE4A180__INCLUDED_)
