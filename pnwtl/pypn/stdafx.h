// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once

// Disable the CRT deprecation warnings in VS 2005
#define _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_WARNINGS

#include <map>
#include <vector>
#include <string>
#include <list>

#define PNASSERT ATLASSERT

#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers
#include <windows.h>

typedef std::basic_string<TCHAR> tstring;

#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/python.hpp>

#include <atlstr.h>

#include "../allocator.h"
#include "../pnextstring.h"
#include "../extiface.h"
#include "../pnstrings.h"
#include "../searchoptions.h"

extern std::string getPythonErrorString();