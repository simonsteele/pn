/**
 * @file inet.h
 * @brief Wrap the WinInet API allowing simple HTTP file open
 * @author Simon Steele
 * @note Copyright (c) 2008-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef inet_h__included
#define inet_h__included

#include <wininet.h>

#define UA_PN _T("Programmer's Notepad/") PN_VERSTRING_T

/**
 * Simple wrapper around the WinInet library.
 */
class Inet
{
public:
	explicit Inet() : m_hInet(NULL), m_hConnection(NULL)
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
		Close();

		if (m_hInet)
		{
			::InternetCloseHandle(m_hInet);
			m_hInet = NULL;
		}
	}

	/**
	 * Attempt to open a URL for reading.
	 * @return false if we don't have a valid internet connection, the url is null, or we fail to open the url, true otherwise.
	 */
	bool Open(LPCTSTR url)
	{
		if (m_hInet == NULL)
		{
			return false;
		}

		if (url == NULL)
		{
			return false;
		}

		m_hConnection = ::InternetOpenUrl(
			m_hInet,
			url,
			NULL /*headers*/,
			0 /*headers length*/,
			INTERNET_FLAG_NO_AUTH | INTERNET_FLAG_NO_CACHE_WRITE | INTERNET_FLAG_NO_COOKIES | INTERNET_FLAG_NO_UI, 
			reinterpret_cast<DWORD_PTR>(this));

		return m_hConnection != NULL;
	}

	/**
	 * Read from a connection opened with Open.
	 * @return true if we read data.
	 */
	bool ReadFile(LPVOID lpBuffer, DWORD dwNumberOfBytesToRead, LPDWORD dwRead)
	{
		PNASSERT(m_hConnection != NULL);

		return ::InternetReadFile(m_hConnection, lpBuffer, dwNumberOfBytesToRead, dwRead) != 0;
	}

	/**
	 * Close any open connection.
	 */
	void Close()
	{
		if (m_hConnection != NULL)
		{
			::InternetCloseHandle(m_hConnection);
			m_hConnection = NULL;
		}
	}

private:
	HINTERNET m_hInet;
	HINTERNET m_hConnection;
};

#endif //#ifndef inet_h__included