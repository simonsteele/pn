#include "StdAfx.h"
#include "Files.h"

///////////////////////////////////////////////////////////////////////////
// FileAge - gets the timestamp of a file in Windows.
///////////////////////////////////////////////////////////////////////////

int FileAge(LPCTSTR FileName)
{
	HANDLE Handle;
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
  return -1;
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
	m_file = fopen(filename, "rb");
	return (m_file != NULL);
}

int CFile::Read(void* lpBuf, UINT nCount)
{
	return (int)fread(lpBuf, nCount, 1, m_file);
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

///////////////////////////////////////////////////////////////////////////
// CFileName - allows lots of useful operations on a filename.
///////////////////////////////////////////////////////////////////////////

CFileName& CFileName::operator = (LPCTSTR filename)
{
	m_FileName = filename;
	return *this;
}

CFileName& CFileName::operator = (const CFileName& filename)
{
	m_FileName = filename.m_FileName;
	return *this;
}

int CFileName::GetLastSlashPos()
{
	int pos = m_FileName.rfind(_T('\\'));
	if(pos == m_FileName.npos)
	{
		// couldn't find '\\', try again for '/'
		pos = m_FileName.rfind(_T('/'));
	}
	return ++pos;
}

int CFileName::GetLastDotPos(cfnString* str)
{
	if(!str)
	{
		int pos = m_FileName.rfind(_T('.'));
		if(pos != m_FileName.npos)
			return ++pos;
		else
			return m_FileName.npos;
	}
	else
	{
		int pos = str->rfind(_T('.'));
		if(pos != m_FileName.npos)
			return ++pos;
		else
			return m_FileName.npos;
	}
}

void CFileName::GetPath(cfnString& buf)
{
	int pos = GetLastSlashPos();
	if (pos != m_FileName.npos)
	{
		buf = m_FileName.substr(0, pos);
	}
	else
	{
		buf = "";
	}
}

void CFileName::GetFileName(cfnString& buf)
{
	int pos = GetLastSlashPos();

	if (pos != m_FileName.npos)
	{
		buf = m_FileName.substr(pos);
	}
	else
	{
		buf = m_FileName;
	}
}

///@todo this crashes if the file has no extension!!!
cfnString CFileName::GetExtension()
{
	int pos = GetLastDotPos(&m_FileName);
	if(pos != m_FileName.npos)
	{
		return m_FileName.substr(pos-1);
	}
	else
		return cfnString("");
}

void CFileName::GetFileName_NoExt(cfnString& buf)
{
	cfnString work;
	
	int pos = GetLastSlashPos();
	
	if (pos != m_FileName.npos)
	{
		work = m_FileName.substr(pos); 
	}
	else
	{
		work = m_FileName;
	}
	
	pos = GetLastDotPos(&work);
	if(pos != work.npos)
	{
		work = work.substr(0, pos-1);
	}

	buf = work;
}

void CFileName::ChangeExtensionTo(LPCTSTR newext)
{
	cfnString	str;
	cfnString	buf;
	GetPath(buf);
	str = buf;
	GetFileName_NoExt(buf);
	str += buf;
	str += newext;
	m_FileName = str;
}

void CFileName::ChangePathTo(LPCTSTR newpath)
{
	cfnString str;
	GetFileName(str);
	str = newpath + str;
	m_FileName = str;
}

const TCHAR* CFileName::c_str()
{
	return m_FileName.c_str();
}

int CFileName::GetLength()
{
	return m_FileName.length();
}

int CFileName::GetFileAge()
{
	return FileAge(m_FileName.c_str());
}
