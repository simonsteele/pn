/**
 * @file boostfilefinder.cpp
 * @brief Find files according to a spec.
 * @author Simon Steele
 * @note Copyright (c) 2011-2012 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include <stdafx.h>
#include "filefinder.h"
#include "io/dos_wildcard.h"

using boost::filesystem::directory_iterator;

BoostFileFinderImpl::BoostFileFinderImpl(IFileFinderResultHandler& resultHandler, BoostFileFinderData& data) : m_data(data), m_handler(resultHandler)
{
}

bool BoostFileFinderImpl::Find(LPCTSTR foundPath, LPCTSTR filespec, bool bRecurse, bool bIncludeHidden)
{
    boost::filesystem::path dir(foundPath);
    // TODO: Refactor to share wildcard over recursion, or avoid recursion:
    PN::IO::dos_wildcard wildcard(filespec, true);
    bool shouldContinue(true);
    
    directory_iterator end;
    for (directory_iterator i(dir); i != end; ++i)
    {
        if (i->path().is_directory()) && bRecurse)
        {
            // TODO: Start directory
            shouldContinue = Find(i->path().string().c_str(), filespec, true, bIncludeHidden);
            // TODO: End directory
        }
        else if (i->path().is_file())
        {
            if (wildcard.match(i->path().filename()))
            {
                m_data.SetFilename(i->path().filename().string());
                m_handler.OnFileFound(i->path().parent_path().c_str(), m_data, shouldContinue);
            }
        }
        
        if (!shouldContinue)
        {
            break;
        }
    }
    
    return shouldContinue;
}