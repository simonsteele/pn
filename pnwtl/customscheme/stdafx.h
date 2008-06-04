// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once

// Disable the CRT deprecation warnings in VS 2005
#define _CRT_SECURE_NO_DEPRECATE

#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers

#define _WIN32_IE 0x0501

// Windows Header Files:
#include <windows.h>
#include <tchar.h>
#include <shellapi.h>

#include <assert.h>
#define ATLASSERT assert

#include <string>

typedef std::basic_string<TCHAR> tstring;

typedef unsigned __int64 uint64_t;

#define PN_NO_CSTRING

#ifdef CUSTOMSCHEME_EXPORTS
#define EXPORT __declspec(dllexport)
#else
#define EXPORT __declspec(dllimport)
#endif