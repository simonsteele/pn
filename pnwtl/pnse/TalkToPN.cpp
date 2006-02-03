#include "stdafx.h"
#include "TalkToPN.h"

typedef std::basic_string<TCHAR> tstring;

PNCommunicator::PNCommunicator(LPCTSTR pszKey)
{
	m_sKey = pszKey;
	m_uiMessage = ::RegisterWindowMessage(pszKey);

	OSVERSIONINFO OSVersion;

	ZeroMemory(&OSVersion, sizeof(OSVERSIONINFO));
	OSVersion.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	::GetVersionEx(&OSVersion);

	bool bWin95 = (OSVersion.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS) &&
		( (OSVersion.dwMajorVersion == 4) && (OSVersion.dwMinorVersion == 0) );

	m_hUser32 = ::LoadLibrary("User32.dll");

	if(m_hUser32 != NULL)
	{

		if(bWin95)
		{
			LOG( _T("PN2 believes it is running on Windows 95") );
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
		LOG( _T("PN2 was unable to locate the BroadcastSystemMessage* function in User32.dll") );
		m_hUser32 = NULL;
		m_pfnBSM = NULL;
	}
}

PNCommunicator::~PNCommunicator()
{
	if(m_hUser32)
	{
		::FreeLibrary(m_hUser32);
		m_hUser32 = NULL;
	}
}

void PNCommunicator::ActivateOther()
{
	if(m_pfnBSM == NULL)
		return;

	DWORD dwRecipients = BSM_APPLICATIONS;
	long res = m_pfnBSM(
		BSF_ALLOWSFW | BSF_FORCEIFHUNG | BSF_IGNORECURRENTTASK,
		&dwRecipients, 
		m_uiMessage,
		MIM_ACTIVATE,
		0);
}

bool PNCommunicator::CreateSharedData(BYTE** buffer, HANDLE* hMappedFile, size_t size)
{
	tstring strName = _T("PN.");
	strName += m_sKey;

	HANDLE hMapping = ::CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, (DWORD)size, strName.c_str());
	if(hMapping == NULL)
		return false;

	BYTE *buf = (BYTE*)::MapViewOfFile(hMapping, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, 0);
	if (buf == NULL)
	{
		::CloseHandle(hMapping);
		return false;
	}
	
	*hMappedFile = hMapping;
	*buffer = buf;

	return true;
}

void PNCommunicator::ReleaseSharedData(BYTE* buffer, HANDLE hMappedFile)
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

void PNCommunicator::SendParameters(std::list<tstring> files)
{
	if(m_pfnBSM == NULL)
		return;

	// Build a null separated list of parameters, store in shared memory 
	// and broadcast that it's there. We also pad out any filenames to include
	// a path if they're relative.
	//TCHAR curDir[MAX_PATH+1];
	//::GetCurrentDirectory(MAX_PATH+1, curDir);

	size_t bufsize(0);
	for(std::list<tstring>::const_iterator i = files.begin(); i != files.end(); ++i)
	{
		// Add size for each string
		bufsize = bufsize + ((*i).size() + 1);
	}

	// Add one for final terminating NULL
	bufsize += 1;

	// Make the buffer and copy the filenames in
	TCHAR* filebuf = new TCHAR[bufsize];
	size_t index(0);
	for(std::list<tstring>::const_iterator i = files.begin(); i != files.end(); ++i)
	{
		_tcscpy(&filebuf[index], (*i).c_str());
		index += (*i).size() + 1;
	}

	filebuf[index] = _T('\0');

	///////////////////////////////////////////////////////////////////////
	// Now we place the data in shared memory...

	HANDLE	hMappedFile;
	BYTE*	buffer;

	if( !CreateSharedData(&buffer, &hMappedFile, bufsize) )
		return;

	// Copy the big buffer into the other big buffer :)
	memcpy(buffer, filebuf, bufsize);

	DWORD dwRecipients = BSM_APPLICATIONS;
	long res = m_pfnBSM(
		BSF_ALLOWSFW | BSF_FORCEIFHUNG | BSF_IGNORECURRENTTASK,
		&dwRecipients, 
		m_uiMessage,
		MIM_PARAMETER_ARRAY,
		bufsize);

	PNASSERT(res != -1);

	ReleaseSharedData(buffer, hMappedFile);
}