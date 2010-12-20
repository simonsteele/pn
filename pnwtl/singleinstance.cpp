/**
 * @file singleinstance.cpp
 * @brief Single Instance Controller
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "singleinstance.h"

///////////////////////////////////////////////////////////////
// MultipleInstanceManager
///////////////////////////////////////////////////////////////

/**
 * If this instance gets to be "the one" then it needs to call AllowRequests to free 
 * up the mutex for other processes to send parameters.
 */
MultipleInstanceManager::MultipleInstanceManager(LPCTSTR pszKey)
{
	// Create the mutex and request initial ownership
	m_hMutex = ::CreateMutex(NULL, TRUE, pszKey);

	// If ERROR_ALREADY_EXISTS then another PN instance has the mutex and we didn't get ownership.
	m_bAlreadyActive = (::GetLastError() == ERROR_ALREADY_EXISTS);
	
#ifdef _DEBUG
	LOG( m_bAlreadyActive ? _T("PN2 is already running") : _T("First PN2 Instance") );
#endif
	
	PNASSERT(m_hMutex != NULL);

	m_sKey = pszKey;

	m_uiMessage = ::RegisterWindowMessage(pszKey);

	bool bWin95 = (g_Context.OSVersion.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS) &&
		( (g_Context.OSVersion.dwMajorVersion == 4) && (g_Context.OSVersion.dwMinorVersion == 0) );

	m_hUser32 = ::LoadLibrary(_T("User32.dll"));

	if(m_hUser32 != NULL)
	{

		if(bWin95)
		{
#ifdef _DEBUG
			LOG( _T("PN2 believes it is running on Windows 95") );
#endif
			m_pfnBSM = (PFNBroadcastSystemMessage)::GetProcAddress(m_hUser32, "BroadcastSystemMessage");
		}
		else
		{
#ifdef _UNICODE
			m_pfnBSM = (PFNBroadcastSystemMessage)::GetProcAddress(m_hUser32, "BroadcastSystemMessageW");
#else
			m_pfnBSM = (PFNBroadcastSystemMessage)::GetProcAddress(m_hUser32, "BroadcastSystemMessageA");
#endif
		}
	}
	else
	{
#ifdef _DEBUG
		LOG( _T("PN2 was unable to locate the BroadcastSystemMessage* function in User32.dll") );
#endif
		m_hUser32 = NULL;
		m_pfnBSM = NULL;
	}
}

MultipleInstanceManager::~MultipleInstanceManager()
{
	::CloseHandle(m_hMutex);

	if(m_hUser32)
	{
		::FreeLibrary(m_hUser32);
		m_hUser32 = NULL;
	}
}

void MultipleInstanceManager::ActivateOther()
{
	if(m_pfnBSM == NULL)
		return;

	DWORD dwRecipients = BSM_APPLICATIONS;
	
	//long res = 
	m_pfnBSM(
		BSF_ALLOWSFW | BSF_FORCEIFHUNG | BSF_IGNORECURRENTTASK,
		&dwRecipients, 
		m_uiMessage,
		MIM_ACTIVATE,
		0);
}

bool MultipleInstanceManager::AlreadyActive()
{
	return m_bAlreadyActive;
}

bool MultipleInstanceManager::CreateSharedData(BYTE** buffer, HANDLE* hMappedFile, size_t size)
{
	tstring strName = _T("PN.");
	strName += m_sKey;

	HANDLE hMapping = ::CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, size, strName.c_str());
	if(hMapping == NULL)
		return false;

	BYTE *buf = (BYTE*)::MapViewOfFile(hMapping, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, 0);
	if (buf == NULL)
	{
		CloseHandle(hMapping);
		return false;
	}
	
	*hMappedFile = hMapping;
	*buffer = buf;

	return true;
}

void MultipleInstanceManager::ReleaseSharedData(BYTE* buffer, HANDLE hMappedFile)
{
	if(buffer)
	{
		::UnmapViewOfFile(buffer);
	}

	if(hMappedFile)
	{
		::CloseHandle(hMappedFile);
	}
}

void MultipleInstanceManager::SendParameters()
{
	if(m_pfnBSM == NULL)
		return;

	if(__argc < 2)
	{
		// If there are no arguments to send, then
		// we simply activate the other instance.
		ActivateOther();
		return;
	}

	std::list<tstring> args = GetCommandLineArgs();
	SendParameters(args);
}

void MultipleInstanceManager::SendParameters(const std::list<tstring>& args)
{
	GArray<TCHAR> parmarray;
	int size = 0;
	int paSize = 0;
	
	for(std::list<tstring>::const_iterator i = args.begin(); i != args.end(); ++i)
	{
		paSize = (*i).size();
		
		parmarray.grow( size + paSize + 1);

		_tcscpy(&parmarray[size], (*i).c_str());

		size += (paSize + 1);
	}

	// Append another NULL.
	parmarray.grow(size + 1);
	parmarray[size] = _T('\0');

	HANDLE	hMappedFile;
	BYTE*	buffer;

	if(RequestPermission())
	{
		if( !CreateSharedData(&buffer, &hMappedFile, parmarray.size() * sizeof(TCHAR)) )
			return;

		// Copy the big buffer into the other big buffer :)
		memcpy(buffer, &parmarray[0], parmarray.size() * sizeof(TCHAR));

		DWORD dwRecipients = BSM_APPLICATIONS;
		long res = m_pfnBSM(
			BSF_ALLOWSFW | BSF_FORCEIFHUNG | BSF_IGNORECURRENTTASK,
			&dwRecipients, 
			m_uiMessage,
			MIM_PARAMETER_ARRAY,
			parmarray.size() * sizeof(TCHAR));

		PNASSERT(res != -1);

		ReleaseSharedData(buffer, hMappedFile);
		Release();
	}
	else
	{
		LOG(_T("PN failed to enter mutex to send parameters"));
	}
}

void MultipleInstanceManager::AllowParameters()
{
	Release();
}

bool MultipleInstanceManager::GetParameters(std::list<tstring>& params, DWORD size)
{
	HANDLE hMappedFile;
	BYTE* buffer;

	if( !CreateSharedData(&buffer, &hMappedFile, size) )
		return false;

	TCHAR* pParam = reinterpret_cast<TCHAR*>( buffer );
	while( *pParam )
	{
		params.push_back( tstring(pParam) );

		pParam += (_tcslen(pParam) + 1);
	}

	ReleaseSharedData(buffer, hMappedFile);

	return true;
}

UINT MultipleInstanceManager::GetMessageID()
{
	return m_uiMessage;
}

bool MultipleInstanceManager::RequestPermission()
{
	return ::WaitForSingleObject(m_hMutex, 30000) == WAIT_OBJECT_0;
}

void MultipleInstanceManager::Release()
{
	::ReleaseMutex(m_hMutex);
}