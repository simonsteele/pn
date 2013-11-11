/**
 * @file win32file.cpp
 * @brief Interfaces for file handling.
 * @author Simon Steele
 * @note Copyright (c) 2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "win32file.h"
#include "fileutil.h"

namespace {
	bool FileExistsAsHidden(const wchar_t* filename)
	{
		FileUtil::FileAttributes_t atts;
		if (::FileExists(filename) &&
			FileUtil::GetFileAttributes(filename, atts) && 
			FileUtil::IsHidden(atts))
		{
			return true;
		}
		
		return false;
	}
} // namespace

Win32File::Win32File(HANDLE hFile, const std::wstring& filename) : m_hFile(hFile), m_filename(filename)
{
}

Win32File::~Win32File()
{
	Close();
}

/**
 * Static Factory function to create a file or open it for writing.
 * 
 * Open the file for writing rather than always creating so as to keep attached streams
 * and attributes.
 */
IFilePtr Win32File::Create(const wchar_t* filename)
{
	HANDLE hFile = ::CreateFile(filename, GENERIC_WRITE, 0, NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
	int lastError(GetLastError());
	if (hFile == INVALID_HANDLE_VALUE && lastError == ERROR_ACCESS_DENIED && FileExistsAsHidden(filename))
	{
		// If the file is hidden, we'll try to create again with hidden flagged.
		hFile = ::CreateFile(filename, GENERIC_WRITE, 0, NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_HIDDEN, NULL);
		if (hFile == INVALID_HANDLE_VALUE)
		{
			lastError = ::GetLastError();
		}
	}

	if (hFile == INVALID_HANDLE_VALUE)
	{
		throw FileSourceException(lastError);
	}

	// We opened the file, if it succeeded and the file wasn't created then we need to truncate it:
	if (lastError == ERROR_ALREADY_EXISTS)
	{
		::SetFilePointer(hFile, 0, 0, FILE_BEGIN);
		::SetEndOfFile(hFile);
	}

	IFilePtr fp(new Win32File(hFile, std::wstring(filename)));
	return fp;
}

/**
 * Static Factory function to open a file for reading.
 */
IFilePtr Win32File::Open(const wchar_t* filename)
{
	HANDLE hFile = ::CreateFile(filename, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (hFile == INVALID_HANDLE_VALUE)
	{
		throw FileSourceException(::GetLastError());
	}

	IFilePtr fp(new Win32File(hFile, std::wstring(filename)));
	return fp;
}

/**
 * Write to the file.
 */
size_t Win32File::Write(const void* buffer, int count)
{
	PNASSERT(m_hFile != NULL);

	DWORD written;
	if (!::WriteFile(m_hFile, buffer, count, &written, NULL))
	{
		throw FileSourceException(::GetLastError());
	}

	return written;
}

/**
 * Read from the file.
 */
size_t Win32File::Read(void* buffer, int count)
{
	PNASSERT(m_hFile != NULL);

	DWORD read;
	if (!::ReadFile(m_hFile, buffer, count, &read, NULL))
	{
		int error = ::GetLastError();
		if (error == ERROR_HANDLE_EOF)
		{
			return 0;
		}

		throw FileSourceException(error);
	}

	return read;
}

/**
 * Close this file.
 */
void Win32File::Close()
{
	if (m_hFile)
	{
		::CloseHandle(m_hFile);
		m_hFile = NULL;
	}
}

std::wstring Win32File::GetFilename() const
{
	return m_filename;
}