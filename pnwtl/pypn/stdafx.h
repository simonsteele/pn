// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once

#include <map>
#include <vector>
#include <string>

#define PNASSERT ATLASSERT

#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers
#include <windows.h>

// Disable the CRT deprecation warnings in VS 2005
#define _CRT_SECURE_NO_DEPRECATE

typedef std::basic_string<TCHAR> tstring;

#include <boost/shared_ptr.hpp>
#include <boost/python.hpp>

#include <atlstr.h>

#include "../extiface.h"
#include "../pnstrings.h"
#include "../pntypes.h"

//bool __declspec(dllexport) __stdcall init_pn_extension(int iface_version, extensions::IPN* pn);
//void __declspec(dllexport) __stdcall exit_pn_extension();

#ifdef _DEBUG
	#define CRTDBG_MAP_ALLOC
	#include <stdlib.h>
	#include <crtdbg.h>
	#define DEBUG_NEW  new(_NORMAL_BLOCK, THIS_FILE, __LINE__)
#endif // #ifdef _DEBUG

extern std::string getPythonErrorString();