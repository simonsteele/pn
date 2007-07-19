/**
 * @file languageTypeTables.h
 * @brief Function definitions for type mappings.
 * @author Simon Steele
 * @note Copyright (c) 2004-2007 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef languageTypeTables_h__included
#define languageTypeTables_h__included

#include <string>

void getTables(const char* schemeName, int** lcTypes, int** ucTypes);

void loadExternalTables(const char* fileName, std::string* moreSchemes);

#endif //#ifndef languageTypeTables_h__included