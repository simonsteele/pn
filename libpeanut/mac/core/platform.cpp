/**
 * @file platform.cpp
 * @brief Mac OS/X implementations of various platform utilities.
 * @author Simon Steele
 * @note Copyright (c) 2012 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include <stdafx.h>
#include "../../../../pnwtl/pn.h"

mach_timebase_info_data_t TimeBaseInfo::sTimebaseInfo;

namespace PN { namespace Platform {

/**
 * Provide information about the current user.
 */
void UserInformation::set()
{
    UserName = "";
}

/**
 * Provide system-locale formatted Date/Time.
 */
void DateTimeInformation::set()
{
    // CurrentDate = ...
    // CurrentTime = ...
}

/**
 * Provide User-Friendly File Information.
 */
void FileInformation::set(LPCTSTR filePath)
{
    // FileDate;
    // FileTime;
    // FileAttr;
}
    
/**
 * Get the directory that PN is running from.
 */
tstring GetExecutableDirectory()
{
    throw "Unimplemented";
}

}} // namespace PN::Platform