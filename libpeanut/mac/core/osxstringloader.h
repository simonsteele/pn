/**
 * @file osxstringloader.h
 * @brief PN Internationalisation
 * @author Simon Steele
 * @note Copyright (c) 2012 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef peanut_osxstringloader_h
#define peanut_osxstringloader_h

namespace PN { namespace L10N {

    class OsxStringLoader : public ::L10N::StringLoader
{
public:
    OsxStringLoader() {}
    virtual ~OsxStringLoader(){}
    
    // Implement StringLoader
protected:
    virtual tstring load(UINT dwStringID);
    
private:
};
    
} } // namespace PN::L10N

#endif
