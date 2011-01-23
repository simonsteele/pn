// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once

// Disable the CRT deprecation warnings in VS 2005
#define _CRT_SECURE_NO_DEPRECATE

#define _WIN32_IE 0x0501

#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers
#include <windows.h>
#include <tchar.h>
#include <shellapi.h>

extern HMODULE theModule;

#include <assert.h>
#define ATLASSERT assert
#define PNASSERT ATLASSERT

#ifdef CUSTOMSCHEME_EXPORTS
#define EXPORT __declspec(dllexport)
#else
#define EXPORT __declspec(dllimport)
#endif

// Implement debug mode memory allocation checking.
#ifdef _DEBUG
	#define CRTDBG_MAP_ALLOC
	#include <stdlib.h>
	#include <crtdbg.h>
	#define DEBUG_NEW  new(_NORMAL_BLOCK, THIS_FILE, __LINE__)
#endif // #ifdef _DEBUG

#include <vector>
#include <functional>
#include <string>

typedef std::basic_string<TCHAR> tstring;
typedef unsigned __int64 uint64_t;

#define PN_NO_CSTRING

extern std::wstring g_SchemesPath;

#include <boost/shared_ptr.hpp>
#include <boost/function.hpp>

// PN:
#include "../allocator.h"
#include "../pnextstring.h"
#include "../extiface.h"

// Scintilla:
#include "../third_party/scintilla/include/Scintilla.h"
#include "../third_party/scintilla/include/ILexer.h"
#include "../third_party/scintilla/lexlib/PropSetSimple.h"
#include "../third_party/scintilla/lexlib/WordList.h"
#include "../third_party/scintilla/lexlib/LexAccessor.h"
#include "../third_party/scintilla/lexlib/Accessor.h"
#include "../third_party/scintilla/lexlib/StyleContext.h"

#include "../third_party/scintilla/lexlib/LexerModule.h"
#include "../third_party/scintilla/lexlib/LexerBase.h"
#include "../third_party/scintilla/lexlib/LexerNoExceptions.h"