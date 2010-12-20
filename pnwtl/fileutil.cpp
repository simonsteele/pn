#include "stdafx.h"
#include "fileutil.h"
#include "filename.h"

namespace FileUtil
{

bool GetFileAttributes(LPCTSTR filename, FileAttributes_t& atts)
{
	return ::GetFileAttributesEx(filename, GetFileExInfoStandard, &atts) != 0;
}

bool IsReadOnly(const FileAttributes_t& atts)
{
	return (atts.dwFileAttributes & FILE_ATTRIBUTE_READONLY) != 0;
}

bool IsHidden(const FileAttributes_t& atts)
{
	return (atts.dwFileAttributes & FILE_ATTRIBUTE_HIDDEN) != 0;
}

uint64_t GetFileAge(const FileAttributes_t& atts)
{
	return *((uint64_t*)&atts.ftLastWriteTime);
}

bool RemoveReadOnly(LPCTSTR filename)
{
	DWORD dwFileAtts = ::GetFileAttributes(filename);
	if(dwFileAtts & FILE_ATTRIBUTE_READONLY)
	{
		dwFileAtts &= ~FILE_ATTRIBUTE_READONLY;
		return ::SetFileAttributes(filename, dwFileAtts) != 0;
	}
	
	return false;
}

bool SetReadOnly(LPCTSTR filename)
{
	DWORD dwFileAtts = ::GetFileAttributes(filename);
	if((dwFileAtts & FILE_ATTRIBUTE_READONLY) == 0)
	{
		dwFileAtts |= FILE_ATTRIBUTE_READONLY;
		return ::SetFileAttributes(filename, dwFileAtts) != 0;
	}
	else
	{
		// File is already read only, we'll claim the glory.
		return true;
	}
}

bool FileIsReadOnly(LPCTSTR filename)
{
	DWORD dwFileAttributes = ::GetFileAttributes(filename);
	return (dwFileAttributes & FILE_ATTRIBUTE_READONLY) != 0;
	
	// This code will check whether we really can change a file, but it feels dirty
	// to me so is currently disabled.
	//{
	//	if (SetFileAttributes(pathname,dwFileAttributes |FILE_ATTRIBUTE_READONLY ) == 0)
	//	{
	//		// write permission
	//		return true;
	//	}
	//	else
	//	{
	//		// no write permission -> restore File Attributes
	//		SetFileAttributes(pathname,dwFileAttributes);
	//		return false;
	//	}
	//}
}

bool CreateBackupFile(LPCTSTR path, LPCTSTR prefix, LPCTSTR extension)
{
	tstring backupname(path);
	
	if (prefix != NULL)
	{
		backupname = prefix + backupname;
	}

	if (extension != NULL)
	{
		backupname += extension;
	}
	else
	{
		backupname += _T(".bak");
	}

	return CopyFile(path, backupname.c_str(), FALSE) != 0;
}

/**
 * @param path Path buffer, must be at least MAX_PATH big...
 * @param folder Folder ID
 */
BOOL PNGetSpecialFolderPath (LPTSTR path, int folder)
{
    ITEMIDLIST *pidl;		// Shell Item ID List ptr
    IMalloc    *imalloc;	// Shell IMalloc interface ptr
    BOOL		result;		// Return value

    if (SHGetSpecialFolderLocation (NULL, folder, &pidl) != NOERROR)
        return FALSE;

    result = SHGetPathFromIDList (pidl, path);

    if (SHGetMalloc (&imalloc) == NOERROR) {
		imalloc->Free(pidl);
        imalloc->Release();
    }

    return result;
}

} // namespace File