/**
 * @file CustomScheme.cpp
 * @brief Defines the entry point for the custom schemes DLL.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "CustomScheme.h"

HMODULE theModule;

BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
		theModule = static_cast<HMODULE>( hModule );
		break;
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		break;
	}
    return TRUE;
}

std::vector<LexerConfig*> theLexers;

//////////////////////////////////////////////////////////////////////////
// Exported Functions
//////////////////////////////////////////////////////////////////////////

int FindLastSlash(const TCHAR *inp) 
{
	int ret = -1;
	for (int i = static_cast<int>(_tcslen(inp)) - 1; i >= 0; i--) 
	{
		if (inp[i] == _T('\\') || inp[i] == _T('/')) 
		{
			ret = i;
			break;
		}
	}
	return ret;
}

void FindLexers()
{
	static bool bFound = false;

	if(!bFound)
	{
		bFound = true;

		TCHAR path[MAX_PATH + 1];

		GetModuleFileName(theModule, path, MAX_PATH);

		int i = FindLastSlash(path);

		if (i == -1)
			i = static_cast<int>(_tcslen(path));

		tstring sPath(path, 0, i);

		CustomLexerFactory factory(sPath.c_str(), theLexers);
	}
}

//////////////////////////////////////////////////////////////////////////
// Exported Functions
//////////////////////////////////////////////////////////////////////////

ILexer* CustomLexer_Factory(int index)
{
	return new CustomLexer(*theLexers[index]);
}

#define MAX_LEXERS 20

#define LEXER_FACTORY_FN(n) ILexer* CustomLexer_Factory##n() { return CustomLexer_Factory(n); }

LEXER_FACTORY_FN(0)
LEXER_FACTORY_FN(1)
LEXER_FACTORY_FN(2)
LEXER_FACTORY_FN(3)
LEXER_FACTORY_FN(4)
LEXER_FACTORY_FN(5)
LEXER_FACTORY_FN(6)
LEXER_FACTORY_FN(7)
LEXER_FACTORY_FN(8)
LEXER_FACTORY_FN(9)
LEXER_FACTORY_FN(10)
LEXER_FACTORY_FN(11)
LEXER_FACTORY_FN(12)
LEXER_FACTORY_FN(13)
LEXER_FACTORY_FN(14)
LEXER_FACTORY_FN(15)
LEXER_FACTORY_FN(16)
LEXER_FACTORY_FN(17)
LEXER_FACTORY_FN(18)
LEXER_FACTORY_FN(19)

#define LEXER_FACTORY_CASE(i) case i: return CustomLexer_Factory##i;

int EXPORT __stdcall GetLexerCount()
{
	// We've been loaded and Scintilla wants our lexers, better find them...
	FindLexers();

	return min(theLexers.size(), MAX_LEXERS);
}

static void LengthSet(char *val, const char* newval, int size)
{
	if ( (int)strlen(newval) < size-1)
	{
		strcpy(val, newval);
	} 
	else 
	{
		strncpy(val, newval, size-1);
	}
}

void EXPORT __stdcall GetLexerName(unsigned int index, char *name, int buflength)
{
	LengthSet(name, theLexers[index]->GetName(), buflength);
}

LexerFactoryFunction EXPORT __stdcall GetLexerFactory(unsigned int index)
{
	// DOES NOT WORK:
	// Instead need to create a bunch of simple factory functions for 1:N, and then file a Scintilla change request.
	switch (index)
	{
		LEXER_FACTORY_CASE(0)
		LEXER_FACTORY_CASE(1)
		LEXER_FACTORY_CASE(2)
		LEXER_FACTORY_CASE(3)
		LEXER_FACTORY_CASE(4)
		LEXER_FACTORY_CASE(5)
		LEXER_FACTORY_CASE(6)
		LEXER_FACTORY_CASE(7)
		LEXER_FACTORY_CASE(8)
		LEXER_FACTORY_CASE(9)
		LEXER_FACTORY_CASE(10)
		LEXER_FACTORY_CASE(11)
		LEXER_FACTORY_CASE(12)
		LEXER_FACTORY_CASE(13)
		LEXER_FACTORY_CASE(14)
		LEXER_FACTORY_CASE(15)
		LEXER_FACTORY_CASE(16)
		LEXER_FACTORY_CASE(17)
		LEXER_FACTORY_CASE(18)
		LEXER_FACTORY_CASE(19)
	}

	return NULL;
}