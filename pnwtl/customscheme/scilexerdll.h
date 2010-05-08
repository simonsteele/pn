/**
 * @file scilexerdll.h
 * @brief DLL entry points.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef scilexerdll_h__included
#define scilexerdll_h__included

// The order of these includes is important.
#include <stdlib.h>
#include "scintilla\platform.h"
#include "scintilla\propset.h"

// These functions are exported:
int EXPORT __stdcall GetLexerCount();

void EXPORT __stdcall GetLexerName(unsigned int lexer, char *name, int buf);

void EXPORT __stdcall Lex(unsigned int lexer, unsigned int startPos, int length, 
						 int initStyle, char *words[], WindowID window, char *props);

void EXPORT __stdcall Fold(unsigned int lexer, unsigned int startPos, int length, 
						 int initStyle, char *words[], WindowID window, char *props);

#endif