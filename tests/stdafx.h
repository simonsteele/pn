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

#include "../pnwtl/allocator.h"
#include "../pnwtl/string.h"

#include <vector>
#include <list>
#include <string>
#include <map>

#define AtlIsValidString(x) true

typedef std::string tstring;
typedef std::vector<tstring> tstring_array;