//
//  stdiofilesource.h
//  libpeanut
//
//  Created by Simon Steele on 06/01/2012.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#ifndef libpeanut_stdiofilesource_h
#define libpeanut_stdiofilesource_h

#include "../../../pnwtl/ifilesource.h"

namespace PN { namespace IO {

namespace Impl {
    
class StdIoFile : public IFile
{
public:
    StdIoFile(LPCTSTR filename, bool write) : m_f(NULL)
    {
        m_filename = filename;
        if (write)
        {
            m_f = fopen(filename, "wb");
        }
        else
        {
            m_f = fopen(filename, "rb");
        }
    }
    
    virtual ~StdIoFile() { close(); }
	
    size_t Write(const void* buffer, int count)
    {
        return fwrite(buffer, count, 1, m_f);
    }
    
    size_t Read(void* buffer, int count)
    {
        return fread(buffer, 1, count, m_f);
    }
    
	void Close()
    {
        close();
    }
    
	tstring GetFilename() const
    {
        return m_filename;
    }
    
private:
    void close()
    {
        if (m_f) {
            fclose(m_f);
        }
    }
    
    tstring m_filename;
    FILE* m_f;
};
    
} //namespace Impl

class StdIoFileSource : public IFileSource
{
public:
    virtual ~StdIoFileSource() {}
    
    virtual IFilePtr OpenWrite(LPCTSTR filename)
    {
        return IFilePtr(new Impl::StdIoFile(filename, true));
    }
	
    virtual IFilePtr OpenRead(LPCTSTR filename)
    {
        return IFilePtr(new Impl::StdIoFile(filename, false));
    }
};
    
} } // namespace PN::IO


#endif
