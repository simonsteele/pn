/**
 * @file ctagsnavigator.h
 * @brief CTAGS output parser for jump to function implementation.
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the CPPNAVIGATOR_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// CPPNAVIGATOR_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.
#ifdef CPPNAVIGATOR_EXPORTS
#define CPPNAVIGATOR_API __declspec(dllexport) __stdcall 
#else
#define CPPNAVIGATOR_API __declspec(dllimport) __stdcall
#endif