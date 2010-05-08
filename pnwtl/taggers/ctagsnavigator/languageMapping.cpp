/**
 * @file languageMapping.cpp
 * @brief Language mappings.
 * @author Simon Steele
 * @note Copyright (c) 2004-2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "languageMapping.h"

LPCWSTR GetLanguage(LPCWSTR filename, const char* scheme)
{
/* Unsupported so far: Awk, BETA, REXX, Scheme, SLang, SML, Vera*/

	const wchar_t* ext = wcsrchr(filename, L'.');
	if(ext != 0)
	{
		if(_wcsnicmp(ext, L".tmp", 4) != 0)
			return NULL;
	}

	if (strcmp("batch", scheme) == 0)
		return L"dosbatch";
	else if(strcmp("cpp", scheme) == 0)
		return L"C++";
	else if(strcmp("csharp", scheme) == 0)
		return L"C#";
	else if(strcmp("assembler", scheme) == 0)
		return L"Asm";
	else if(strcmp("cobol", scheme) == 0)
		return L"Cobol";
	else if(strcmp("eiffel", scheme) == 0)
		return L"Eiffel";
	else if(strcmp("erlang", scheme) == 0)
		return L"Erlang";
	else if(strcmp("fortran", scheme) == 0)
		return L"Fortran";
	else if(strcmp("fortran77", scheme) == 0)
		return L"Fortran";
	else if(strcmp("java", scheme) == 0)
		return L"Java";
	else if(strcmp("javascript", scheme) == 0)
		return L"JavaScript";
	else if(strcmp("web", scheme) == 0)
		return L"PHP"; // hope for the best...
	else if(strcmp("php", scheme) == 0)
		return L"PHP";
	else if(strcmp("phpscript", scheme) == 0)
		return L"PHP";
	else if(strcmp("lisp", scheme) == 0)
		return L"Lisp";
	else if(strcmp("lua", scheme) == 0)
		return L"Lua";
	else if(strcmp("makefile", scheme) == 0)
		return L"Make";
	else if(strcmp("pascal", scheme) == 0)
		return L"Pascal";
	else if(strcmp("perl", scheme) == 0)
		return L"Perl";
	else if(strcmp("python", scheme) == 0)
		return L"Python";
	else if(strcmp("plsql", scheme) == 0)
		return L"SQL";
	else if(strcmp("ruby", scheme) == 0)
		return L"Ruby";
	else if(strcmp("shell", scheme) == 0)
		return L"Sh";
	else if(strcmp("tcl", scheme) == 0)
		return L"Tcl";
	else if(strcmp("verilog", scheme) == 0)
		return L"Verilog";
	else if(strcmp("vim", scheme) == 0)
		return L"Vim";
	else if(strcmp("yacc", scheme) == 0)
		return L"YACC";
	else if(strcmp("vhdl", scheme) == 0)
		return L"Vhdl";

	return NULL;
}