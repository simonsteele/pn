/**
 * @file languageMapping.h
 * @brief Function definition for language mappings.
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef languageMapping_h__included
#define languageMapping_h__included

LPCWSTR GetLanguage(LPCWSTR filename, const char* scheme);

#endif // #ifndef languageMapping_h__included