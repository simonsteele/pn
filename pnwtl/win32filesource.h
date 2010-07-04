/**
 * @file win32filesource.h
 * @brief Interfaces for file handling.
 * @author Simon Steele
 * @note Copyright (c) 2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef WIN32FILESOURCE_H_INCLUDED
#define WIN32FILESOURCE_H_INCLUDED

#include "ifilesource.h"

/**
 * Implement IFileSource for win32 file i/o.
 */
class Win32FileSource : public IFileSource
{
public:
	virtual ~Win32FileSource();

	IFilePtr OpenWrite(const wchar_t* filename);
	IFilePtr OpenRead(const wchar_t* filename);
};

#endif //#ifndef WIN32FILESOURCE_H_INCLUDED