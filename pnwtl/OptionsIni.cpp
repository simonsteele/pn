/**
 * @file OptionsIni.h
 * @brief Ini configuration functionality.
 * @author Simon Steele
 * @note Copyright (c) 2004-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "OptionsManager.h"
#include "OptionsIni.h"

#pragma warning( push )
#pragma warning(disable: 4996) // see MSDN on hash_map

#if (_ATL_VER >= 0x0700)
	#include <hash_map>
	class IniKeyMap : public stdext::hash_map<tstring, tstring>{};
#else
	#include <map>
	class IniKeyMap : public std::map<tstring, tstring>{};
#endif

IniOptions::IniOptions() : 
	groupLocked(false), 
	keyMap(new IniKeyMap()),
	_filename(NULL)
{
	tstring userSettingsPath;
	GetPNPath(userSettingsPath, PNPATH_USERSETTINGS);
	userSettingsPath += _T("UserSettings.ini");
	_filename = new TCHAR[userSettingsPath.length()+1];
	_tcscpy(_filename, userSettingsPath.c_str());
}

IniOptions::~IniOptions()
{
	if(keyMap)
	{
		delete keyMap;
		keyMap = NULL;
	}

	if(_filename)
	{
		delete [] _filename;
		_filename = NULL;
	}
}

void IniOptions::SetUserSettingsPath(LPCTSTR path)
{
	// Call the base class.
	Options::SetUserSettingsPath(path);
	
	if(_filename)
	{
		delete [] _filename;
		_filename = NULL;
	}
	
	// Update our stored filename...
	tstring userSettingsPath;
	GetPNPath(userSettingsPath, PNPATH_USERSETTINGS);
	userSettingsPath += _T("UserSettings.ini");
	_filename = new TCHAR[userSettingsPath.length()+1];
	_tcscpy(_filename, userSettingsPath.c_str());
}

void IniOptions::Set(LPCTSTR subkey, LPCTSTR value, bool bVal)
{
	::WritePrivateProfileString(groupLocked ? _group : subkey, value, bVal ? _T("1") : _T("0"), _filename);
}

void IniOptions::Set(LPCTSTR subkey, LPCTSTR value, int iVal)
{
	TCHAR cbuf[40];
	_itot(iVal, cbuf, 10);
	::WritePrivateProfileString(groupLocked ? _group : subkey, value, cbuf, _filename);
}

void IniOptions::Set(LPCTSTR subkey, LPCTSTR value, uint64_t iVal)
{
	TCHAR cbuf[70];
	_ui64tot(iVal, cbuf, 10);
	::WritePrivateProfileString(groupLocked ? _group : subkey, value, cbuf, _filename);
}

void IniOptions::Set(LPCTSTR subkey, LPCTSTR value, LPCTSTR szVal)
{
	::WritePrivateProfileString(groupLocked ? _group : subkey, value, szVal, _filename);
}


bool IniOptions::Get(LPCTSTR subkey, LPCTSTR value, bool bDefault)
{
	return Get(subkey, value, bDefault ? 1 : 0) != 0;
}

int IniOptions::Get(LPCTSTR subkey, LPCTSTR value, int iDefault)
{
	if(groupLocked)
	{
		IniKeyMap::const_iterator i = keyMap->find(tstring(value));
		if(i != keyMap->end())
			return _ttoi((*i).second.c_str());
		else
			return iDefault;
	}
	else
	{
		return GetPrivateProfileInt(subkey, value, iDefault, _filename);
	}
}

uint64_t IniOptions::Get(LPCTSTR subkey, LPCTSTR value, uint64_t iDefault)
{
	if(groupLocked)
	{
		IniKeyMap::const_iterator i = keyMap->find(tstring(value));
		if(i != keyMap->end())
		{
			TCHAR* end(NULL);
			return _tcstoui64((*i).second.c_str(), &end, 10);
		}
		else
			return iDefault;
	}
	else
	{
		tstring srep = Get(subkey, value, _T(""));
		if (srep.size())
		{
			TCHAR* end(NULL);
			return _tcstoui64(srep.c_str(), &end, 10);
		}
		else
		{
			return iDefault;
		}
	}
}

tstring IniOptions::Get(LPCTSTR subkey, LPCTSTR value, LPCTSTR szDefault)
{
	if(groupLocked)
	{
		IniKeyMap::const_iterator i = keyMap->find(tstring(value));
		if(i != keyMap->end())
			return (*i).second;
		else
			return tstring(szDefault);
	}
	else
	{
		GArray<TCHAR> tcbuf;
	
		int nBufferLen = 64;
		int nLen;
		do
		{
			nBufferLen *= 2;
			tcbuf.grow(nBufferLen);
			nLen = ::GetPrivateProfileString(subkey, value, szDefault, &tcbuf[0], nBufferLen, _filename);
		}
		while (nLen == nBufferLen-1);

		return tstring(&tcbuf[0]);
	}
}

void IniOptions::Clear(LPCTSTR subkey)
{
	const TCHAR* emptySection = _T("\0");
	::WritePrivateProfileSection(subkey, emptySection, _filename);
}

void IniOptions::group(LPCTSTR location)
{
	//The return value specifies the number of characters copied to the buffer, not including the terminating null character. If the buffer is not large enough to contain all the key name and value pairs associated with the named section, the return value is equal to nSize minus two.
	DWORD bufsize = 2048;
	TCHAR* buffer = new TCHAR[bufsize];
	while( GetPrivateProfileSection(location, buffer, bufsize, _filename) == (bufsize -2) )
	{
		bufsize *= 2;
		delete [] buffer;
		buffer = new TCHAR[bufsize];
	}

	TCHAR* p = buffer;
	TCHAR* comma;
	while(*p)
	{
		comma = _tcschr(p, _T('='));

		if (comma == NULL)
		{
			break;
		}

		*comma = NULL;
		comma++;
		keyMap->insert(IniKeyMap::value_type(tstring(p), tstring(comma)));
		p = comma + _tcslen(comma) + 1;
	}

	delete [] buffer;

	groupLocked = true;
	_group = location;
}

void IniOptions::ungroup()
{
	keyMap->clear();
	groupLocked = false;
}

#pragma warning( pop ) // 4996 - deprecated hash_map.