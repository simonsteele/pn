/**
 * @file languageMapping.cpp
 * @brief Language mappings.
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "languageMapping.h"

LPCWSTR GetLanguage(LPCWSTR filename, LPCWSTR scheme)
{
/* Unsupported so far: Awk, BETA, REXX, Scheme, SLang, SML, Vera*/

	wchar_t* ext = wcsrchr(filename, L'.');
	if(ext != 0)
	{
		if(wcsnicmp(ext, L".tmp", 4) != 0)
			return NULL;
	}

	if(wcscmp(L"cpp", scheme) == 0)
		return L"C++";
	else if(wcscmp(L"csharp", scheme) == 0)
		return L"C#";
	else if(wcscmp(L"assembler", scheme) == 0)
		return L"Asm";
	else if(wcscmp(L"cobol", scheme) == 0)
		return L"Cobol";
	else if(wcscmp(L"eiffel", scheme) == 0)
		return L"Eiffel";
	else if(wcscmp(L"erlang", scheme) == 0)
		return L"Erlang";
	else if(wcscmp(L"fortran", scheme) == 0)
		return L"Fortran";
	else if(wcscmp(L"java", scheme) == 0)
		return L"Java";
	else if(wcscmp(L"javascript", scheme) == 0)
		return L"JavaScript";
	else if(wcscmp(L"web", scheme) == 0)
		return L"PHP"; // hope for the best...
	else if(wcscmp(L"lisp", scheme) == 0)
		return L"Lisp";
	else if(wcscmp(L"lua", scheme) == 0)
		return L"Lua";
	else if(wcscmp(L"makefile", scheme) == 0)
		return L"Make";
	else if(wcscmp(L"pascal", scheme) == 0)
		return L"Pascal";
	else if(wcscmp(L"perl", scheme) == 0)
		return L"Perl";
	else if(wcscmp(L"python", scheme) == 0)
		return L"Python";
	else if(wcscmp(L"plsql", scheme) == 0)
		return L"SQL";
	else if(wcscmp(L"ruby", scheme) == 0)
		return L"Ruby";
	else if(wcscmp(L"shell", scheme) == 0)
		return L"Sh";
	else if(wcscmp(L"tcl", scheme) == 0)
		return L"Tcl";
	else if(wcscmp(L"verilog", scheme) == 0)
		return L"Verilog";
	else if(wcscmp(L"vim", scheme) == 0)
		return L"Vim";
	else if(wcscmp(L"yacc", scheme) == 0)
		return L"YACC";
	else if(wcscmp(L"vhdl", scheme) == 0)
		return L"Vhdl";

	return NULL;
}