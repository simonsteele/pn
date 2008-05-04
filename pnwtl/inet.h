/**
 * @file inet.h
 * @brief Wrap the WinInet API allowing simple HTTP file open
 * @author Simon Steele
 * @note Copyright (c) 2008 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef inet_h__included
#define inet_h__included

#include <wininet.h>

#define UA_PN _T("Programmer's Notepad/") PN_VERSTRING

class Inet
{
public:
	Inet() : m_hInet(NULL), m_hConnection(NULL)
	{
		m_hInet = ::InternetOpen(
			UA_PN,
			INTERNET_OPEN_TYPE_PRECONFIG,
			NULL,
			NULL,
			/*INTERNET_FLAG_ASYNC*/0);

		/*if (m_hInet)
		{
			::InternetSetStatusCallback(m_hInet, &Inet::InternetStatusCallback);
		}*/

	}

	~Inet()
	{
		if (m_hConnection)
		{
			::InternetCloseHandle(m_hConnection);
			m_hConnection = NULL;
		}

		if (m_hInet)
		{
			::InternetCloseHandle(m_hInet);
			m_hInet = NULL;
		}
	}

	/** 
	 * Callback called from inside internet request, dispatch to class instance to handle.
	 */
	/*static void CALLBACK InternetStatusCallback(
	  __in  HINTERNET hInternet,
	  __in  DWORD_PTR dwContext,
	  __in  DWORD dwInternetStatus,
	  __in  LPVOID lpvStatusInformation,
	  __in  DWORD dwStatusInformationLength
	)
	{
		Inet* inet = reinterpret_cast<Inet*>(dwContext);
		inet->handleCallback(hInternet, dwInternetStatus, lpvStatusInformation, dwStatusInformationLength);
	}*/

	bool Open(LPCTSTR url)
	{
		m_hConnection = ::InternetOpenUrl(
			m_hInet,
			url,
			NULL /*headers*/,
			0 /*headers length*/,
			INTERNET_FLAG_NO_AUTH | INTERNET_FLAG_NO_CACHE_WRITE | INTERNET_FLAG_NO_COOKIES | INTERNET_FLAG_NO_UI, 
			reinterpret_cast<DWORD_PTR>(this));

		return m_hConnection != NULL;
	}

	bool ReadFile(LPVOID lpBuffer, DWORD dwNumberOfBytesToRead, LPDWORD dwRead)
	{
		PNASSERT(m_hConnection != NULL);

		return ::InternetReadFile(m_hConnection, lpBuffer, dwNumberOfBytesToRead, dwRead) != 0;
	}

	void Close()
	{
		if (m_hConnection != NULL)
		{
			::InternetCloseHandle(m_hConnection);
			m_hConnection = NULL;
		}
	}

	//void handleCallback(HINTERNET hInternet, DWORD dwInternetStatus, LPVOID lpvStatusInformation, DWORD dwStatusInformationLength)
	//{
	//	if (dwInternetStatus == INTERNET_STATUS_REQUEST_COMPLETE)
	//	{
	//		INTERNET_ASYNC_RESULT* pResult = reinterpret_cast<INTERNET_ASYNC_RESULT*>(lpvStatusInformation);
	//		//pResult->dwResult
	//		int a = pResult->dwResult;
	//	}
	//}

private:
	HINTERNET m_hInet;
	HINTERNET m_hConnection;
};

#endif //#ifndef inet_h__included