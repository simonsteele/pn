//
//  filesourcefactory.h
//  libpeanut
//
//  Created by Simon Steele on 06/01/2012.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#ifndef libpeanut_filesourcefactory_h
#define libpeanut_filesourcefactory_h

#include "../../pnwtl/ifilesource.h"

#if PLAT_WIN
#include "../../pnwtl/win32filesource.h"
typedef Win32FileSource DefaultFileSource;
#else
#include "io/stdiofilesource.h"
typedef PN::IO::StdIoFileSource DefaultFileSource;
#endif

namespace PN { namespace IO {

class FileFactory
{
public:
    static IFilePtr OpenWrite(LPCTSTR path)
    {
        return DefaultFileSource().OpenWrite(path);
    }
    
    static IFilePtr OpenRead(LPCTSTR path)
    {
        return DefaultFileSource().OpenRead(path);
    }
    
private:
    FileFactory() {}
};
    
}} // namespace PN::IO

#endif
