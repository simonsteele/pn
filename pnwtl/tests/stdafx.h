// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once

#ifndef _WIN32_WINNT		// Allow use of features specific to Windows XP or later.                   
#define _WIN32_WINNT 0x0501	// Change this to the appropriate value to target other versions of Windows.
#endif						

#include <stdio.h>
#include <tchar.h>
#include <string.h>
#include <stdexcept>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define TESTASSERT(x) \
	if(!(x)) \
	{ \
	throw std::exception("Test Failed: " #x); \
	}

#include <assert.h>
#define PNASSERT assert
#define PN_NO_CSTRING 1

#include <atlbase.h>
#include <shellapi.h>

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

#include "../allocator.h"
#include "../pnextstring.h"
#include "../xmlparser.h"

#include <vector>
#include <list>
#include <string>
#include <map>

#define AtlIsValidString(x) true

typedef std::basic_string<TCHAR> tstring;
typedef std::vector<tstring> tstring_array;

// Boosty Goodness:
#include <boost/shared_ptr.hpp>
#include <boost/xpressive/xpressive.hpp>
#include <boost/foreach.hpp>
#include <boost/spirit/include/qi.hpp>

namespace boost { namespace xpressive {
#ifdef _UNICODE
	typedef wsregex tsregex;
	typedef wsmatch tsmatch;
#else
	typedef sregex tsregex;
	typedef smatch tsmatch;
#endif
}} // namespace boost::xpressive

// PN Stuff:
#include "../scintillaif.h"
#include "../pnstrings.h"
#include "../extiface.h"
#include "../pntypes.h"
#include "../files.h"
#include "../filename.h"
#include "mocks/mockoptions.h"

extern IOptionsWithString* g_Options;
#define OPTIONS g_Options
#define LOG(x) ::OutputDebugString(x);

#define RETURN_UNEXPECTED(x, ret) assert(false);
#define UNEXPECTED(x) assert(false);