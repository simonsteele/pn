/**
 * @file ssreg.cpp
 * @brief CSRegistry windows registry functionality wrapper implementation...
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "ssreg.h"

namespace ssreg
{

CSRegistry::~CSRegistry()
{
	// Destructor - make sure we disconnect etc.
	if(m_open)
		CloseKey();
}

bool CSRegistry::OpenKey(LPCTSTR key, bool bCreate)
{
	if(m_open)
		CloseKey();

	bool bSuccess;

	if(bCreate)
	{
		// Can create if necessary
		bSuccess = RegCreateKeyEx(
			m_root,
			key,
			0,
			NULL,
			0,
			KEY_ALL_ACCESS,
			NULL,
			&m_hKey,
			NULL
		) == ERROR_SUCCESS;
	}
	else
	{
		bSuccess = RegOpenKeyEx(
			m_root,
			key,
			0,
			KEY_ALL_ACCESS,
			&m_hKey
		) == ERROR_SUCCESS;
	}
	if(bSuccess)
		m_open = true;

	return bSuccess;
}

void CSRegistry::WriteInt(LPCTSTR valname, int value)
{
	/*LONG RegSetValueEx(
		HKEY hKey,           // handle to key
		LPCTSTR lpValueName, // value name
		DWORD Reserved,      // reserved
		DWORD dwType,        // value type
		CONST BYTE *lpData,  // value data
		DWORD cbData         // size of value data
	);*/

	DWORD val = value;

	RegSetValueEx(
		m_hKey,
		valname,
		0,
		REG_DWORD,
		(LPBYTE)&val,
		sizeof(DWORD)
	);
}

int CSRegistry::ReadInt(LPCTSTR valname, int defaultval)
{
	DWORD val;
	DWORD dwType;
	DWORD dwCount = sizeof(DWORD);

	if(!m_open) throw "CSRegistry Exception - no open key.";

	if (
		RegQueryValueEx(m_hKey,
			valname, 
			0, 
			&dwType,
			(LPBYTE)&val, 
			&dwCount) == ERROR_SUCCESS )
	{
		if (dwType == REG_DWORD)
			return val;
		else
			return defaultval;
	}
	else
	{
		return defaultval;
	}	
}

void CSRegistry::WriteBool(LPCTSTR valname, bool value)
{
	WriteInt(valname, (value ? 1 : 0));
}

bool CSRegistry::ReadBool(LPCTSTR valname, bool defaultval)
{
	return (ReadInt(valname, (defaultval ? 1 : 0)) == 1 ? true : false);
}

void CSRegistry::WriteString(LPCTSTR valname, LPCTSTR value)
{
	
	if(!m_open) throw "CSRegistry Exception - no open key.";

	int len = strlen(value);
	long lResult = 0;
    
	if(len != 0)
	{
		lResult = RegSetValueEx(m_hKey,			// Key handle returned from RegOpenKeyEx.
                             valname,			// Default string title
                             NULL,				// Reserved, dword = NULL.
                             REG_SZ,			// Type of data.
                             (LPBYTE) value,	// Data buffer.
                              len);				// Size of data buffer.
		if(lResult != ERROR_SUCCESS)
			throw "CSRegistry Exception - Error writing to registry.";
	}
	else
	{
		int iDat=0;
		lResult = RegSetValueEx(m_hKey,			// Key handle returned from RegOpenKeyEx.
                             valname,			// Default string title
                             NULL,				// Reserved, dword = NULL.
                             REG_DWORD,			// Type of data.
                             (LPBYTE) &iDat,	// Data buffer.
                             sizeof(int) );		// Size of data buffer.
		
		if(lResult != ERROR_SUCCESS)
			throw "CSRegistry Exception - Error writing to registry.";
	}
}

bool CSRegistry::ReadString(LPCTSTR valname, ctcString& value)
{
	if(!m_open) throw "CSRegistry Exception - no open key.";

	DWORD	dwType;
	DWORD	dwCount = 0;
	bool	bRes = false;

	if( RegQueryValueEx(m_hKey, valname, 0, &dwType, NULL, &dwCount) == ERROR_SUCCESS )
	{
		// dwCount is now the size of the required buffer...
		if (dwCount > 1)
		{
			BYTE	*bBuff = new BYTE[dwCount];
			long lResult = RegQueryValueEx(m_hKey, valname, 0, &dwType, bBuff, &dwCount);

			if(lResult == ERROR_SUCCESS && dwType == REG_SZ)
			{
				value = (const char *)bBuff;
				bRes = true;
			}
			delete [] bBuff;
		}
		else
			value = "";
	}
	
	return bRes;
}

void CSRegistry::CloseKey()
{
	RegCloseKey(m_hKey);
}

bool CSRegistry::DeleteValue(LPCTSTR valname)
{
	return ::RegDeleteValue(m_hKey, valname) == ERROR_SUCCESS;
}

} // namespace ssreg