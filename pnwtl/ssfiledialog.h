#include <atldlgs.h>

typedef struct tagSSOFNEXA {
   DWORD        lStructSize;
   HWND         hwndOwner;
   HINSTANCE    hInstance;
   LPCSTR       lpstrFilter;
   LPSTR        lpstrCustomFilter;
   DWORD        nMaxCustFilter;
   DWORD        nFilterIndex;
   LPSTR        lpstrFile;
   DWORD        nMaxFile;
   LPSTR        lpstrFileTitle;
   DWORD        nMaxFileTitle;
   LPCSTR       lpstrInitialDir;
   LPCSTR       lpstrTitle;
   DWORD        Flags;
   WORD         nFileOffset;
   WORD         nFileExtension;
   LPCSTR       lpstrDefExt;
   LPARAM       lCustData;
   LPOFNHOOKPROC lpfnHook;
   LPCSTR       lpTemplateName;
#ifdef _MAC
   LPEDITMENU   lpEditInfo;
   LPCSTR       lpstrPrompt;
#endif
   void *       pvReserved;
   DWORD        dwReserved;
   DWORD        FlagsEx;
} SSOPENFILENAMEEXA, *LPSSOPENFILENAMEEXA;

typedef struct tagSSOFNEXW {
   DWORD        lStructSize;
   HWND         hwndOwner;
   HINSTANCE    hInstance;
   LPCWSTR      lpstrFilter;
   LPWSTR       lpstrCustomFilter;
   DWORD        nMaxCustFilter;
   DWORD        nFilterIndex;
   LPWSTR       lpstrFile;
   DWORD        nMaxFile;
   LPWSTR       lpstrFileTitle;
   DWORD        nMaxFileTitle;
   LPCWSTR      lpstrInitialDir;
   LPCWSTR      lpstrTitle;
   DWORD        Flags;
   WORD         nFileOffset;
   WORD         nFileExtension;
   LPCWSTR      lpstrDefExt;
   LPARAM       lCustData;
   LPOFNHOOKPROC lpfnHook;
   LPCWSTR      lpTemplateName;
#ifdef _MAC
   LPEDITMENU   lpEditInfo;
   LPCSTR       lpstrPrompt;
#endif
   void *       pvReserved;
   DWORD        dwReserved;
   DWORD        FlagsEx;
} SSOPENFILENAMEEXW, *LPSSOPENFILENAMEEXW;

#ifdef UNICODE
typedef SSOPENFILENAMEEXW SSOPENFILENAMEEX;
typedef LPSSOPENFILENAMEEXW LPSSOPENFILENAMEEX;
#else
typedef SSOPENFILENAMEEXA SSOPENFILENAMEEX;
typedef LPSSOPENFILENAMEEXA LPSSOPENFILENAMEEX;
#endif // UNICODE

class CSSFileDialog : public CFileDialogImpl<CSSFileDialog>
{
public:
	CSSFileDialog(BOOL bOpenFileDialog, // TRUE for FileOpen, FALSE for FileSaveAs
		LPCTSTR lpszDefExt = NULL,
		LPCTSTR lpszFileName = NULL,
		DWORD dwFlags = OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		LPCTSTR lpszFilter = NULL,
		HWND hWndParent = NULL,
		bool bUsePipeChar = true)
		: CFileDialogImpl<CSSFileDialog>(bOpenFileDialog, lpszDefExt, lpszFileName, dwFlags, lpszFilter, hWndParent)
	{ 
		// Only real change here is to set up the m_ofnex structure
		memset(&m_ofnex, 0, sizeof(m_ofnex)); // initialize structure to 0/NULL
		m_bShowPlacesBar = true;

		if(lpszFilter != NULL && bUsePipeChar)
		{
			m_szFilter = new TCHAR[_tcslen(lpszFilter)+1];
			_tcscpy(m_szFilter, lpszFilter);
			
			LPTSTR pch = m_szFilter;
			while ((pch = _tcschr(pch, '|')) != NULL)
				*pch++ = '\0';

			m_ofn.lpstrFilter = m_szFilter;
		}
		else
		{
			m_szFilter = NULL;
			m_ofn.lpstrFilter = lpszFilter;
		}
	}

	virtual ~CSSFileDialog()
	{
		if(m_szFilter != NULL)
			delete [] m_szFilter;
	}

	bool IsEnhancedWinVer()
	{
		OSVERSIONINFO ver;
		ver.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
		if (! ::GetVersionEx(&ver) )
		{
			return false;
		}

		if (ver.dwPlatformId == VER_PLATFORM_WIN32_NT)
		{
			if (ver.dwMajorVersion >= 5)
				return true;
		}
		else
		{
			if ( (ver.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS) &&
				 (ver.dwMajorVersion >= 4) &&
				 (ver.dwMinorVersion >= 90) )
				return true;
		}
		return false;
	}

	INT_PTR DoModal(HWND hWndParent = ::GetActiveWindow())
	{
		ATLASSERT(m_ofn.Flags & OFN_ENABLEHOOK);
		ATLASSERT(m_ofn.lpfnHook != NULL);	// can still be a user hook

		ATLASSERT(m_ofn.Flags & OFN_EXPLORER);

		if(m_ofn.hwndOwner == NULL)		// set only if not specified before
			m_ofn.hwndOwner = hWndParent;

		ATLASSERT(m_hWnd == NULL);
		_Module.AddCreateWndData(&m_thunk.cd, (CDialogImplBase*)this);

		// This is where we make our changes! If we are in Windows 2000 mode,
		// we copy the contents of m_ofn into m_ofnex and call GetOpenFileName etc.
		// with m_ofnex instead of m_ofn.

		BOOL bRet;

		if( IsEnhancedWinVer() && m_bShowPlacesBar )
		{
			// Prepare m_ofnex:
			memcpy(&m_ofnex, &m_ofn, sizeof(SSOPENFILENAMEEX) - (sizeof(DWORD)*2) - sizeof(void *));
			m_ofnex.lStructSize = sizeof(SSOPENFILENAMEEX);
			m_ofnex.FlagsEx = 0; // Enable Places Bar...

			if(m_bOpenFileDialog)
				bRet = ::GetOpenFileName((OPENFILENAME*)&m_ofnex);
			else
				bRet = ::GetSaveFileName((OPENFILENAME*)&m_ofnex);

			// And now we need to re fill in m_ofn:
			memcpy(&m_ofn, &m_ofnex, sizeof(OPENFILENAME));
		}
		else
		{
			if (m_bOpenFileDialog)
				bRet = ::GetOpenFileName(&m_ofn);
			else
				bRet = ::GetSaveFileName(&m_ofn);
		}

		m_hWnd = NULL;

		return bRet ? IDOK : IDCANCEL;
	}

	// override base class map and references to handlers
	DECLARE_EMPTY_MSG_MAP()

	bool m_bShowPlacesBar;

protected:
	TCHAR*				m_szFilter;
	SSOPENFILENAMEEX	m_ofnex;
};