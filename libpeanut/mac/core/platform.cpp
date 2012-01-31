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
#include "../../../pnwtl/pn.h"

mach_timebase_info_data_t TimeBaseInfo::sTimebaseInfo;

namespace {
    
    tstring CFStringToString(CFStringRef cfString)
    {
        std::vector<unsigned char> buffer;
        const char *useUTF8StringPtr = NULL;
        
        CFIndex stringLength = CFStringGetLength(cfString), usedBytes = 0L;
        
        if((useUTF8StringPtr = CFStringGetCStringPtr(cfString, kCFStringEncodingUTF8)) == NULL)
        {
            // TODO: This might need adjusting for UTF8 characters - is only unicode character count?
            buffer.resize(stringLength + 1);
            CFStringGetBytes(cfString, CFRangeMake(0L, stringLength), kCFStringEncodingUTF8, '?', false, &buffer[0], stringLength, &usedBytes);
            buffer[usedBytes] = 0;
            useUTF8StringPtr = reinterpret_cast<char*>(&buffer[0]);
        }
        
        long utf8Length = (long)(buffer.size() ? usedBytes : stringLength);
        
        if(useUTF8StringPtr != NULL)
        {
            // useUTF8StringPtr points to a NULL terminated UTF8 encoded string.
            // utf8Length contains the length of the UTF8 string.
            return tstring(useUTF8StringPtr, utf8Length);
        }
        
        return tstring("");
    }
    
    template <class T>
    class CFReleaseGuard
    {
    public:
        CFReleaseGuard(T releaseable) : m_r(releaseable) {}
        ~CFReleaseGuard() { CFRelease(m_r); }
        
        operator T () const { return m_r; }
    private:
        T m_r;
    };
}

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
    CFBundleRef main(CFBundleGetMainBundle());
    
    CFReleaseGuard<CFURLRef> resources(CFBundleCopyResourcesDirectoryURL(main));
    CFReleaseGuard<CFStringRef> dir(CFURLCopyFileSystemPath(resources, kCFURLPOSIXPathStyle));

    return CFStringToString(dir);
}

}} // namespace PN::Platform