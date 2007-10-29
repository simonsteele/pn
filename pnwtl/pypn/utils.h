/**
 * @file utils.h
 * @brief Utilities for working with embedded python
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef utils_h__included
#define utils_h__included

#ifdef _MSC_VER
	#pragma once
#endif

std::string getPythonErrorString();

std::string PyTracebackToString(void);

#endif // #ifndef utils_h__included