#include "StdAfx.h"
#include "Files.h"
#include "shellapi.h"
#include <algorithm>

#ifndef pnutils_h__included
static TCHAR* tcsnewdup(LPCTSTR strin)
{
	TCHAR* ret = new TCHAR[_tcslen(strin)+1];
	_tcscpy(ret, strin);
	return ret;
}
#endif

/**
 * @brief Get the dos file time of a file.
 * @param FileName fully qualified path.
 */
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

bool DirExists(LPCTSTR szDir)
{
	return (GetFileAttributes(szDir) != INVALID_FILE_ATTRIBUTES);
}

bool FileExists(LPCTSTR FileName)
{
	HANDLE h;
	WIN32_FIND_DATA FindData;

	h = FindFirstFile(FileName, &FindData);
	FindClose(h);
	
	return h != INVALID_HANDLE_VALUE;
}

bool IsDirectory(LPCTSTR szDir)
{
	DWORD fa = GetFileAttributes(szDir);
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
 */
bool CreateDirectoryRecursive(LPCTSTR pszDirectory, LPSECURITY_ATTRIBUTES lpSA) 
{
	char szDir[MAX_PATH];
	char *p, *pNext;
	strcpy(szDir, pszDirectory);

	pNext = strchr(szDir, '\\');
	if (pNext)
	{
		pNext++;
		while ( ( p = strchr(pNext, '\\') ) != 0 )
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

int CFile::ShowError(LPCTSTR filename, bool bOpen)
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
		ret = ::MessageBox(NULL, (LPCTSTR)buffer, _T("Programmers Notepad 2"), MB_OK);
	else
		ret = ::MessageBox(NULL, (LPCTSTR)buffer, _T("Programmers Notepad 2"), MB_YESNOCANCEL);

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

CFileName& CFileName::operator = (const tstring& filename)
{
	m_FileName = filename;
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
	if(pos != m_FileName.npos)
		return ++pos;
	else
		return m_FileName.npos;
}

int CFileName::GetLastDotPos(tstring* str)
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

tstring CFileName::GetPath()
{
	int pos = GetLastSlashPos();
	if (pos != m_FileName.npos)
	{
		return m_FileName.substr(0, pos);
	}
	else
	{
		return _T("");
	}
}

tstring CFileName::GetDirectoryName()
{
	tstring path = GetPath();
	char cLast = path[path.length() - 1];
	if(cLast == _T('\\') || cLast == _T('/'))
	{
		// remove the last char.
		path.erase(path.length()-1);
	}

	int pos = path.rfind(_T('\\'));
	if(pos == path.npos)
	{
		// couldn't find '\\', try again for '/'
		pos = path.rfind(_T('/'));
	}
	if(pos != path.npos)
	{
		path = path.substr(++pos);
	}
	
	return path;
}

void CFileName::GetPath(tstring& buf)
{
	buf = GetPath();
}

/**
 * We assume that a filename that starts with a \ or has a drive letter 
 * at the start is non-relative. Any path with no slash at all is 
 * definitely relative.
 */
bool CFileName::IsRelativePath()
{
	// If there's no slash in it, it's definitely relative...
	int spos = GetLastSlashPos();
	if( spos == m_FileName.npos )
		return true;

	// If the length is less than 3 then there isn't a drive letter.
	if( m_FileName.length() < 3 )
		return true;

	// Is there a drive letter as the first two characters (then it's not relative)?
	if( ::isalpha(m_FileName[0]) && (m_FileName[1] == _T(':')) )
		return false;

	// If it begins with a slash then it's not relative - mostly linux style paths.
	if( m_FileName[0] == _T('\\') || m_FileName[0] == _T('/') )
		return false;

	return true;
}

/**
 * Get the relative path from path to us.
 */
tstring CFileName::GetRelativePath(LPCTSTR path)
{
	if(IsSubElementOf(path))
	{
		TCHAR slash;
		int chopLen = _tcslen(path);
		
		if(_tcschr(path, _T('/')) != NULL)
			slash = _T('/');
		else
			slash = _T('\\');

		// We're walking down the tree...
		if(path[_tcslen(path)-1] == slash)
		{
			chopLen++;
		}

        return m_FileName.substr(chopLen-1);
	}
	else if(PathIsParentElementOf(path))
	{
		//Cop out.
		return m_FileName;
	}
	else
	{
		///@todo Add a relative filename thing to walk down if we're a sub-element of path.
		
		//Cop out.
		return m_FileName;
	}
}

bool CFileName::CanGetRelativePath(LPCTSTR path)
{
	return (IsSubElementOf(path) || PathIsParentElementOf(path));
}

/**
 * This function first checks if there are duplicate slashes, and then
 * combines the current path with the rootPath provided.
 */
void CFileName::Root(LPCTSTR rootPath)
{
	tstring root(rootPath);
	bool bForwards = false;

	// We always have the trailing slash in root, so make sure that
	// m_FileName doesn't have a starting one.
	if( m_FileName[0] == _T('\\') || m_FileName[0] == _T('/') )
	{
		m_FileName.erase(m_FileName.begin());
	}

	// Are we using forward slashes?
	if( root.find(_T('/')) != root.npos )
		bForwards = true;

	// Make sure there's a trailing slash.
	if( root[root.length()-1] != _T('\\') && root[root.length()-1] != _T('/') )
		root += (bForwards ? _T('/') : _T('\\'));

	TCHAR slash = bForwards ? _T('/') : _T('\\');

	int len = m_FileName.length();

	if(len >= 2)
	{
		while(m_FileName[0] == _T('.'))
		{
			if(len >= 3 && m_FileName[1] == _T('.') && m_FileName[2] == slash) // Do we have a ..\?
			{
				//1. Remove one folder from the end of root.
				tstring::size_type slashPos = root.rfind(slash, root.length() - 2);
				if( slashPos != root.npos )
				{
					root.erase(slashPos + 1);
				}
				
				//2. Remove the ..\ from m_FileName;
				m_FileName.erase(0, 3);
			}
			else if(len >= 2 && m_FileName[1] == slash) // Do we have a .\?
			{
				//1. Remove the dotslash
				m_FileName.erase(0, 2);
			}

			len = m_FileName.length();
		}
	}

	root += m_FileName;

	m_FileName = root;
}

/**
 * Find out if we're below path in the filesystem.
 */
bool CFileName::IsSubElementOf(LPCTSTR path)
{
	// This function simply checks if the path string passed in is
	// the start of our filename string.
	return (_tcsnicmp(m_FileName.c_str(), path, _tcslen(path)) == 0);
}

/**
 * Find out if the path passed in is below us in the file system.
 */
bool CFileName::PathIsParentElementOf(LPCTSTR path)
{
	// This function simply checks if the path string passed in has our
	// filename as its beginning.
	tstring myPath = GetPath();
	return (_tcsnicmp(path, myPath.c_str(), myPath.length()) == 0);
}

void CFileName::SetForwardSlashes()
{
	LPTSTR p = tcsnewdup(m_FileName.c_str());

	LPTSTR pSlash = _tcschr(p, _T('\\'));
	while(pSlash)
	{
		*pSlash = _T('/');
		pSlash = _tcschr(pSlash, _T('\\'));
	}

	m_FileName = p;
	delete [] p;
}

TCHAR hexcharval(TCHAR inp)
{
	int Result = 0;
	
	if (inp >= '0' && inp <= '9') 
	{
		Result = inp - 48;
	}
	else if (inp >= 'a' && inp <= 'f')
	{
		Result = inp - 87;
	}
	else if (inp >= 'A' && inp <= 'F') 
	{
		Result = inp - 55;
	}
  
	return Result;
}

/**
 * This function removes stupidities from paths, whether they be
 * file:// style URLs or /\ issues.
 */
tstring& CFileName::Sanitise()
{
	UINT fpos = m_FileName.find(_T("file://"));
	if( fpos != m_FileName.npos )
	{
		// We have a file:// url. Fix it up.
		m_FileName.erase(fpos, 7);
		
		/*fpos = m_FileName.find(_T("%3A"));
		if( fpos != m_FileName.npos )
		{
			m_FileName.replace(fpos, 3, _T(":"));
		}*/

		fpos = m_FileName.find(_T("%"));
		TCHAR replaceBuf[2];
		replaceBuf[1] = _T('\0');
		while( fpos != m_FileName.npos )
		{
			if( ((fpos + 1) != m_FileName.npos) && ((fpos + 2) != m_FileName.npos) )
			{
				if(_istxdigit(m_FileName.at(fpos+1)) && _istxdigit(m_FileName.at(fpos+2)))
				{
					replaceBuf[0] = (16*hexcharval(m_FileName.at(fpos+1))) + hexcharval(m_FileName.at(fpos+2));
					m_FileName.replace(fpos, 3, replaceBuf);
				}

				fpos = m_FileName.find(_T('%'), fpos);
			}
			else
				fpos = m_FileName.npos;
		}
	}

	LPCTSTR in = m_FileName.c_str();
	LPTSTR res = new TCHAR[_tcslen(in)+1];

	LPCTSTR fnd = _tcschr(in, _T(':'));
	bool bColon = fnd != NULL;

	if( bColon )
	{
        // There's a colon, so there's a drive letter.
		// Make sure drive letter is at the start of the string.
		while( (!_istalpha( *in )) && (in != fnd) )
		{
			in++;
		}
	}

	bool bSlash = false;
	LPTSTR build = res;

	// Step through the input string and catch things like double-slashes,
	// and forward instead of backward slashes.
	while(*in != NULL)
	{
		char ch = *in;
		switch(ch)
		{
			// Pretend forward slashes are back-slashes and fall-through
			// to back-slash handling.
			case _T('/'):
				ch = _T('\\');
			
			// If there is a colon (i.e. a drive letter) and a double
			// slash, then it's a duplicate to skip. If there is no colon
			// and a double slash closer than 4 characters to the start
			// of the filename then we assume it's ok.
			case _T('\\'):
				if( (bSlash && bColon) ||
					bSlash && ((in - m_FileName.c_str()) > 4) )
				{
					bSlash = false;
					break;
				}
				bSlash = true;
			
			default:
				if(ch != _T('\\'))
					bSlash = false;
				*build++ = ch;
		}
		
		in++;
	}

	*build++ = NULL;

	m_FileName = res;

	delete [] res;

	return m_FileName;
}

void CFileName::GetFileName(tstring& buf)
{
	buf = GetFileName();
}

tstring CFileName::GetFileName()
{
	int pos = GetLastSlashPos();

	if (pos != m_FileName.npos)
		return m_FileName.substr(pos);
	else
		return m_FileName;
}

tstring CFileName::GetFileName_NoExt()
{
	tstring work;
	
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

	return work;
}

///@todo this crashes if the file has no extension!!!
tstring CFileName::GetExtension()
{
	int pos = GetLastDotPos(&m_FileName);
	if(pos != m_FileName.npos)
	{
		return m_FileName.substr(pos-1);
	}
	else
		return tstring(_T(""));
}

void CFileName::GetFileName_NoExt(tstring& buf)
{
	buf = GetFileName_NoExt();
}

void CFileName::ChangeExtensionTo(LPCTSTR newext)
{
	tstring	str;
	tstring	buf;
	GetPath(buf);
	str = buf;
	GetFileName_NoExt(buf);
	str += buf;
	str += newext;
	m_FileName = str;
}

void CFileName::ChangePathTo(LPCTSTR newpath)
{
	tstring str;
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

const tstring& CFileName::ToLower()
{
	std::transform (m_FileName.begin(), m_FileName.end(),    // source
               m_FileName.begin(),             // destination
               tolower);

	return m_FileName;
}

tstring CFileName::GetCurrentDirectory()
{
	TCHAR buf[MAX_PATH+1];
	::GetCurrentDirectory(MAX_PATH, buf);
	return tstring(buf);
}

CPathName::CPathName(LPCTSTR path)
{
	m_FileName = path;
	char cLast = m_FileName[m_FileName.length() - 1];
	if(cLast != _T('\\') && cLast != _T('/'))
		m_FileName += _T("\\");
}

CPathName& CPathName::operator = (const tstring& filename)
{
	m_FileName = filename;
	return *this;
}