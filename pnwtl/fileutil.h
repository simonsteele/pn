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

typedef WIN32_FILE_ATTRIBUTE_DATA FileAttributes_t;

bool RemoveReadOnly(LPCTSTR filename);

bool SetReadOnly(LPCTSTR filename);

bool FileIsReadOnly(LPCTSTR filename);

bool CreateBackupFile(LPCTSTR path, LPCTSTR prefix, LPCTSTR extension);

bool GetFileAttributes(LPCTSTR filename, FileAttributes_t& atts);

bool IsReadOnly(const FileAttributes_t& atts);

bool IsHidden(const FileAttributes_t& atts);

uint64_t GetFileAge(const FileAttributes_t& atts);

/**
 * @param path Path buffer, must be at least MAX_PATH big...
 * @param folder Folder ID
 */
BOOL PNGetSpecialFolderPath (LPTSTR path, int folder);

}
