/**
 * @file fileutil.h
 * @brief File utility methods
 * @author Simon Steele
 * @note Copyright (c) 2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

namespace FileUtil
{

bool RemoveReadOnly(LPCTSTR filename);

bool FileIsReadOnly(LPCTSTR filename);

bool CreateBackupFile(LPCTSTR path, LPCTSTR prefix, LPCTSTR extension);

/**
 * @param path Path buffer, must be at least MAX_PATH big...
 * @param folder Folder ID
 */
BOOL PNGetSpecialFolderPath (LPTSTR path, int folder);

}
