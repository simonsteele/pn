/**
 * @file files.cpp
 * @brief File access wrappers
 * @author Simon Steele
 * @note Copyright (c) 2005-2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"

#include <shlobj.h>

#include "Files.h"
#include "resource.h"

/**
 * @brief Get the dos file time of a file.
 * @param FileName fully qualified path.
 */
uint64_t FileAge(LPCTSTR FileName)
{
	WIN32_FILE_ATTRIBUTE_DATA data;
	if (::GetFileAttributesEx(FileName, GetFileExInfoStandard, &data))
	{
		return *((uint64_t*)&data.ftLastWriteTime);
	}
	
	return (uint64_t)~0;

	/*HANDLE Handle;
	WIN32_FIND_DATA FindData;
	FILETIME LocalFileTime;

	WORD wHigh;
	WORD wLow;
	
	Handle = FindFirstFile(FileName, &FindData);
	if (Handle != INVALID_HANDLE_VALUE)
	{
		FindClose(Handle);
		if ((FindData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == 0)
		{
			FileTimeToLocalFileTime(&FindData.ftLastWriteTime, &LocalFileTime);
			if (FileTimeToDosDateTime(&LocalFileTime, &wHigh, &wLow) != 0)
			{
				return (int)MAKELONG(wLow, wHigh);
			}
		}
	}	
	return ~0;*/
}

bool DirExists(LPCTSTR szDir)
{
	return IsDirectory(szDir);
}

bool FileExists(LPCTSTR FileName)
{
	// Changed to GetFileAttributes, see http://blogs.msdn.com/oldnewthing/archive/2007/10/23/5612082.aspx
	return ::GetFileAttributes(FileName) != INVALID_FILE_ATTRIBUTES;
}

bool IsDirectory(LPCTSTR szDir)
{
	DWORD fa = ::GetFileAttributes(szDir);
	return (fa != INVALID_FILE_ATTRIBUTES && ((fa & FILE_ATTRIBUTE_DIRECTORY) == FILE_ATTRIBUTE_DIRECTORY));
}

/**
 * @brief Recursively create a directory.
 * @param pszDirectory full directory path to create.
 * @param lpSA pointer to a SECURITY_ATTRIBUTES structure to be passed to CreateDirectory
 *
 * Credit to Eugen@Fastfertig.com who posted almost exactly this code
 * to CodeGuru. Saved me writing it!
 * link: http://www.codeguru.com/mfc/comments/39715.shtml
 *
 * 13/10/2004: Security and code safety fixes thanks to Joerg Hoh.
 * 24/01/2005: Updated to work with UNC paths (hopefully)
 */
bool CreateDirectoryRecursive(LPCTSTR pszDirectory, LPSECURITY_ATTRIBUTES lpSA) 
{
	if(_tcslen(pszDirectory) >= MAX_PATH)
		return false;

	TCHAR szDir[MAX_PATH];
	TCHAR *p, *pNext;
	_tcscpy(szDir, pszDirectory);

	pNext = NULL;

	// See if we have a UNC path and if so skip past the first section.
	if(_tcslen(pszDirectory) >= 3)
	{
		if(szDir[0] == _T('\\') && szDir[1] == _T('\\'))
		{
			pNext = _tcschr(&szDir[2], _T('\\'));
		}
	}

	if(!pNext)
		pNext = _tcschr(szDir, '\\');

	if (pNext)
	{
		pNext++;
		while ( *pNext && ( p = _tcschr(pNext, '\\') ) != 0 )
		{
			*p = NULL;
			if (GetFileAttributes(szDir) == -1)
			{
				if (CreateDirectory(szDir, lpSA) == FALSE)
				{
					return FALSE;
				}
			}
			*p = '\\';
			pNext = p+1;
		}
	}
	if (GetFileAttributes (szDir) == -1)
	{
		if (CreateDirectory(szDir, lpSA) == FALSE)
		{
			return false;
		}
	}

	return true;
}

bool DeleteDirectory(LPCTSTR szDir, bool undoable)
{
	// Create a buffer with the directory in it (dual null-terminated)
	int inLen = _tcslen(szDir);
		
	TCHAR* buf = new TCHAR[inLen+2];
	_tcscpy(buf, szDir);
	buf[inLen+1] = '\0';
	
	if(szDir[inLen-1] == '\\' || szDir[inLen-1] == '/')
		buf[inLen-1] = '\0';

	SHFILEOPSTRUCT fo;
	fo.hwnd = NULL;
	fo.wFunc = FO_DELETE;
	fo.pFrom = buf;
	fo.pTo = NULL;
	fo.fFlags = FOF_NOCONFIRMATION | FOF_WANTNUKEWARNING;
	if(undoable)
		fo.fFlags |= FOF_ALLOWUNDO;
	
	bool bRet = SHFileOperation(&fo) == 0;

	delete [] buf;

	return bRet;
}

///////////////////////////////////////////////////////////////////////////
// CFile - an MFC CFile replacement. No win api stuff, just pure Cpp.
///////////////////////////////////////////////////////////////////////////

CFile::CFile()
{
	m_file = NULL;
}

CFile::~CFile()
{
	Close();
}

bool CFile::Open(LPCTSTR filename, UINT flags)
{
	tstring mode;

	if(flags != 0)
	{
		if(modeWrite & flags)
			mode = _T("w");
		else if(modeReadWrite & flags)
			mode = _T("r+");
		else mode = _T("r");

		if(modeText & flags)
			mode += _T("t");
		else
			mode += _T("b");
	}
	else
		mode = _T("rb");
		

	m_file = _tfopen(filename, mode.c_str());
	
	return (m_file != NULL);
}

int CFile::Read(void* lpBuf, UINT nCount)
{
	return (int)fread(lpBuf, 1, nCount, m_file);
}

int CFile::Write(void* lpBuf, UINT nCount)
{
	return (int)fwrite(lpBuf, nCount, 1, m_file);
}

void CFile::Close()
{
	if(m_file != NULL)
	{
		fclose(m_file);
		m_file = NULL;
	}
}

long CFile::GetPosition() const
{
	if(m_file)
	{
		return ftell(m_file);
	}
	else
		return -1;
}

void CFile::Seek(UINT offset, CFile::EFrom from)
{
	if(m_file)
		fseek(m_file, offset, (int)from);
}

long CFile::GetLength()
{
	if(m_file)
	{
		long curPos = GetPosition();
		fseek(m_file, 0, SEEK_END);
		long ret = GetPosition();
		fseek(m_file, curPos, SEEK_SET);
		return ret;
	}
	else
		return -1;
}

int CFile::ShowError(LPCTSTR filename, LPCTSTR app, bool bOpen)
{
	int err = GetLastError();
	int ret = 0;
	
	TCHAR* fstr;
	
	switch(err)
	{
	case ERROR_ACCESS_DENIED:
	case ERROR_NOT_DOS_DISK:
	case ERROR_WRITE_PROTECT:
		if (bOpen )
			fstr = CFILE_LoadAccessDenied;
		else
			fstr = CFILE_SaveAccessDenied;
		break;
		
	case ERROR_DISK_FULL:
	case ERROR_HANDLE_DISK_FULL:
		if (bOpen)
			fstr = CFILE_CouldNotLoadError;
		else
			fstr = CFILE_SaveDiskFullError;
		break;
		
	case ERROR_SHARING_VIOLATION:
	case ERROR_LOCK_VIOLATION:
		if (bOpen)
			fstr = CFILE_LoadShareViolation;
		else
			fstr = CFILE_SaveShareViolation;
		break;
		
	case ERROR_DEV_NOT_EXIST:
	case ERROR_BAD_NETPATH:
	case ERROR_NETWORK_BUSY:
		if (bOpen)
			fstr = CFILE_NetLoadError;
		else
			fstr = CFILE_NetSaveError;
		break;
		
	default:
		if (bOpen)
			fstr = CFILE_CouldNotLoadError;
		else
			fstr = CFILE_CouldNotSaveError;
	}

	int bs = _tcslen(fstr) + _tcslen(filename) + 10;
	TCHAR* buffer = new TCHAR[bs];
	_sntprintf(buffer, bs, fstr, filename);
	if(bOpen)
		ret = ::MessageBox(NULL, (LPCTSTR)buffer, app, MB_OK);
	else
		ret = ::MessageBox(NULL, (LPCTSTR)buffer, app, MB_YESNOCANCEL);

	delete [] buffer;
	return ret;
}

///////////////////////////////////////////////////////////////////////////
// CTextFile - text file operations...
///////////////////////////////////////////////////////////////////////////

// basically taken from MFC's CStdioString...
#ifndef PN_NO_CSTRING
bool CTextFile::ReadLine(CString& line)
{
	line = _T("");
	const int nMaxSize = 128;
	LPTSTR lpsz = line.GetBuffer(nMaxSize);
	LPTSTR lpszResult;
	int nLen = 0;
	
	for (;;)
	{
		lpszResult = _fgetts(lpsz, nMaxSize+1, m_file);
		line.ReleaseBuffer();

		// handle error/eof case
		if (lpszResult == NULL && !feof(m_file))
		{
			clearerr(m_file);
			// could throw exception here...
		}

		// if string is read completely or EOF
		if (lpszResult == NULL ||
			(nLen = lstrlen(lpsz)) < nMaxSize ||
			lpsz[nLen-1] == '\n')
			break;

		nLen = line.GetLength();
		lpsz = line.GetBuffer(nMaxSize + nLen) + nLen;
	}

	// remove '\n' from end of string if present
	lpsz = line.GetBuffer(0);
	nLen = line.GetLength();
	if (nLen != 0 && lpsz[nLen-1] == '\n')
		line.GetBufferSetLength(nLen-1);

	return lpszResult != NULL;
}
#endif

bool CTextFile::WriteLine(LPCTSTR line)
{
	int ret = _fputts(line, m_file);
	return (ret != _TEOF);
}