#include "stdafx.h"
#include "filename.h"

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
	size_t pos = m_FileName.rfind(_T('\\'));
	if (pos == m_FileName.npos)
	{
		// couldn't find '\\', try again for '/'
		pos = m_FileName.rfind(_T('/'));
	}
	
	if (pos != m_FileName.npos)
	{
		return static_cast<int>(++pos);
	}
	else
	{
		return static_cast<int>(m_FileName.npos);
	}
}

int CFileName::GetLastDotPos(tstring* str)
{
	if(str == NULL)
	{
		str = &m_FileName;
	}

	size_t pos = str->rfind(_T('.'));
	if (pos != str->npos)
	{
		return static_cast<int>(++pos);
	}
	else
	{
		return static_cast<int>(str->npos);
	}
}

tstring CFileName::GetPath()
{
	int pos = GetLastSlashPos();
	if (pos != static_cast<int>(m_FileName.npos))
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
	if (!path.size())
	{
		return path;
	}

	TCHAR cLast = path[path.length() - 1];
	if (cLast == _T('\\') || cLast == _T('/'))
	{
		// remove the last char.
		path.erase(path.length()-1);
	}

	size_t pos = path.rfind(_T('\\'));
	if (pos == path.npos)
	{
		// couldn't find '\\', try again for '/'
		pos = path.rfind(_T('/'));
	}
	
	if (pos != path.npos)
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
	if (spos == static_cast<int>(m_FileName.npos))
	{
		return true;
	}

	// If the length is less than 3 then there isn't a drive letter.
	if (m_FileName.length() < 3)
	{
		return true;
	}

	// Is there a drive letter as the first two characters (then it's not relative)?
	if (::isalpha(m_FileName[0]) && (m_FileName[1] == _T(':')))
	{
		return false;
	}

	// If it begins with a slash then it's not relative - mostly linux style paths.
	if (m_FileName[0] == _T('\\') || m_FileName[0] == _T('/'))
	{
		return false;
	}

	return true;
}

/**
 * Get the relative path from path to us.
 */
tstring CFileName::GetRelativePath(LPCTSTR path)
{
	TCHAR slash;
	if (_tcschr(path, _T('/')) != NULL)
	{
		slash = _T('/');
	}
	else
	{
		slash = _T('\\');
	}

	// We want to always compare to a full path, if there's no trailing slash we should insert one:
	tstring compareTo(path);
	if (!(*compareTo.rbegin() == slash))
	{
		compareTo += slash;
	}

	if (IsSubElementOf(compareTo.c_str()))
	{
		int chopLen = compareTo.size();

        return m_FileName.substr(chopLen);
	}
	else if(PathIsParentElementOf(compareTo.c_str()))
	{
		//Cop out.
		return m_FileName;
	}
	else
	{
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
			else
			{
				// Filename must begin with a dot!
				break;
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
	int bufsize = max(_tcslen(in)+1, MAX_PATH);
	std::vector<TCHAR> res(bufsize);

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
	LPTSTR build = &res[0];

	// Step through the input string and catch things like double-slashes,
	// and forward instead of backward slashes.
	while(*in != NULL)
	{
		TCHAR ch = *in;
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

	m_FileName = &res[0];

	if (m_FileName.length() >= 2 && m_FileName[1] == _T(':') || m_FileName[0] == _T('\\'))
	{
		// Ask windows to make this path better for us:
		PathCanonicalize(&res[0], m_FileName.c_str());
		m_FileName = &res[0];
		
		int expandResult(0);
		if ((expandResult = GetLongPathName(m_FileName.c_str(), &res[0], bufsize)) > bufsize)
		{
			res.resize(expandResult+1);
			GetLongPathName(m_FileName.c_str(), &res[0], expandResult + 1);
		}
		
		m_FileName = &res[0];
	}

	return m_FileName;
}

void CFileName::GetFileName(tstring& buf)
{
	buf = GetFileName();
}

tstring CFileName::GetFileName()
{
	int pos = GetLastSlashPos();

	if (pos != static_cast<int>(m_FileName.npos))
	{
		return m_FileName.substr(pos);
	}
	else
	{
		return m_FileName;
	}
}

tstring CFileName::GetFileName_NoExt()
{
	tstring work;
	
	int pos = GetLastSlashPos();
	
	if (pos != static_cast<int>(m_FileName.npos))
	{
		work = m_FileName.substr(pos); 
	}
	else
	{
		work = m_FileName;
	}
	
	pos = GetLastDotPos(&work);
	if(pos != static_cast<int>(work.npos))
	{
		work = work.substr(0, pos - 1);
	}

	return work;
}

///@todo this crashes if the file has no extension!!!
tstring CFileName::GetExtension()
{
	int pos = GetLastDotPos(&m_FileName);
	if (pos != static_cast<int>(m_FileName.npos))
	{
		return m_FileName.substr(pos-1);
	}
	else
	{
		return tstring(_T(""));
	}
}

void CFileName::GetFileName_NoExt(tstring& buf)
{
	buf = GetFileName_NoExt();
}

void CFileName::AddExtension(LPCTSTR newext)
{
	m_FileName += newext;
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

uint64_t CFileName::GetFileAge()
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

///////////////////////////////////////////////////////////////////////////
// CPathName
///////////////////////////////////////////////////////////////////////////

CPathName::CPathName(LPCTSTR path)
{
	m_FileName = path;
	if(m_FileName.length())
	{
		TCHAR cLast = m_FileName[m_FileName.length() - 1];
		if(cLast != _T('\\') && cLast != _T('/'))
			m_FileName += _T("\\");
	}
}

CPathName& CPathName::operator = (const tstring& filename)
{
	m_FileName = filename;
	return *this;
}

void CPathName::ChangeLastElement(LPCTSTR lastEl)
{
	tstring path = GetPath();
	TCHAR cLast = path[path.length() - 1];
	if(cLast == _T('\\') || cLast == _T('/'))
	{
		// remove the last char.
		path.erase(path.length()-1);
	}

	m_FileName = path;
	int pos = GetLastSlashPos();
	if (pos != static_cast<int>(m_FileName.npos))
	{
		m_FileName = path.substr(0, pos);
		m_FileName += lastEl;
		m_FileName += _T('\\');
	}
	else
	{
		m_FileName = lastEl;
	}
}