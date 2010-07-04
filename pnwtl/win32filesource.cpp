/**
 * @file win32filesource.cpp
 * @brief Interfaces for file handling.
 * @author Simon Steele
 * @note Copyright (c) 2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "win32filesource.h"
#include "win32file.h"

Win32FileSource::~Win32FileSource()
{
}

IFilePtr Win32FileSource::OpenWrite(const wchar_t* filename)
{
	return Win32File::Create(filename);
}

IFilePtr Win32FileSource::OpenRead(const wchar_t* filename)
{
	return Win32File::Open(filename);
}