/**
 * @file modules.h
 * @brief Defines the modules that will be exported to python using Boost Python
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef pypnmodules_h__included
#define pypnmodules_h__included

#ifdef _MSC_VER
	#pragma once
#endif

extern "C" __declspec(dllexport) void initdebug();
extern "C" __declspec(dllexport) void initpn();
extern "C" __declspec(dllexport) void initscintilla();

#endif // #ifndef pypnmodules_h__included