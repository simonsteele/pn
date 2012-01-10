/**
 * @file winfilefinder.cpp
 * @brief Find files according to a spec.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "filefinder.h"

#if PLAT_WIN

WinFileFinderImpl::WinFileFinderImpl(IFileFinderResultHandler resultHandler)
{
    handler = resultHandler;
}

bool WinFileFinderImpl::Find(LPCTSTR path, LPCTSTR filespec, bool bRecurse = false, bool bIncludeHidden = true)
{
    HANDLE hFind;
    BOOL found = true;
    bool shouldContinue = true;
    
    WIN32_FIND_DATA& FindFileData =  findData.m_findData;
    
    tstring sPattern(path);
    sPattern += filespec;
    
    hFind = FindFirstFile(sPattern.c_str(), &FindFileData);
    if (hFind != INVALID_HANDLE_VALUE) 
    {
        T* pT = static_cast<T*>(this);
        
        while (found && pT->shouldContinue())
        {	
            //if we are not including hidden files/directories and this one is hidden skip checking
            if( !( !bIncludeHidden && ((FindFileData.dwFileAttributes & FILE_ATTRIBUTE_HIDDEN) != 0) ) )
            {
                if( bRecurse && ((FindFileData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0) )
                {
                    if( pT->shouldRecurse(path, FindFileData.cFileName) )
                    {
                        tstring path2(path);
                        path2 += FindFileData.cFileName;
                        path2 += _T('\\');
                        if(fDirChange)
                            (owner->*fDirChange)(path2.c_str());
                        Find(path2.c_str(), filespec, bRecurse, bIncludeHidden);
                        if(fFinishDir)
                            (owner->*fFinishDir)(path2.c_str());
                    }
                }
                else
                {
                    // Call owner class with found data...
                    if( pT->shouldMatch(FindFileData.cFileName) )
                    {
                        (owner->*f)(path, findData, shouldContinue);
                        
                        if (!shouldContinue)
                        {
                            break;
                        }
                    }
                }
            }
            
            found = FindNextFile(hFind, &FindFileData);
        }
        
        FindClose(hFind);
        
        return shouldContinue;
    }
    
    return false;
}

#endif