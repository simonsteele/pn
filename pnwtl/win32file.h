/**
 * @file win32file.h
 * @brief Interfaces for file handling.
 * @author Simon Steele
 * @note Copyright (c) 2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef WIN32FILE_H_INCLUDED
#define WIN32FILE_H_INCLUDED

#include "ifilesource.h"

/**
 * Win32 File I/O Implementation of IFile.
 */
class Win32File : public IFile
{
public:
	virtual ~Win32File();

	/**
	 * Factory function to create a file or open it for writing.
	 */
	static IFilePtr Create(const wchar_t* filename);
	
	/**
	 * Factory function to open a file for reading.
	 */
	static IFilePtr Open(const wchar_t* filename);

	/**
	 * Write to the file.
	 */
	virtual size_t Write(const void* buffer, int count);
	
	/**
	 * Read from the file.
	 */
	virtual size_t Read(void* buffer, int count);
	
	/**
	 * Close this file.
	 */
	virtual void Close();

	/**
	 * Get the filename for this file.
	 */
	virtual std::wstring GetFilename() const;

private:
	Win32File(HANDLE hFile, const std::wstring& filename);
	HANDLE m_hFile;
	std::wstring m_filename;
};

#endif // #ifndef WIN32FILE_H_INCLUDED