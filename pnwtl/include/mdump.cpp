#include "stdafx.h"
#include "mdump.h"

LPCTSTR MiniDumper::m_szAppName;

#define SCRATCH_BUFFER_LENGTH _MAX_PATH*4

MiniDumper::MiniDumper( LPCTSTR szAppName )
{
	// if this assert fires then you have two instances of MiniDumper
	// which is not allowed
	PNASSERT( m_szAppName == NULL );

	m_szAppName = szAppName ? _tcsdup(szAppName) : _T("Application");

	::SetUnhandledExceptionFilter( TopLevelFilter );

	_set_invalid_parameter_handler( InvalidParameterHandler );
}

LONG MiniDumper::TopLevelFilter( struct _EXCEPTION_POINTERS *pExceptionInfo )
{
	LONG retval = EXCEPTION_CONTINUE_SEARCH;
	HWND hParent = NULL;						// find a better value for your app

	// firstly see if dbghelp.dll is around and has the function we need
	// look next to the EXE first, as the one in System32 might be old 
	// (e.g. Windows 2000)
	HMODULE hDll = NULL;
	TCHAR szDbgHelpPath[_MAX_PATH];

	if (GetModuleFileName( NULL, szDbgHelpPath, _MAX_PATH ))
	{
		TCHAR *pSlash = _tcsrchr( szDbgHelpPath, _T('\\') );
		if (pSlash)
		{
			_tcscpy( pSlash+1, _T("DBGHELP.DLL") );
			hDll = ::LoadLibrary( szDbgHelpPath );
		}
	}

	if (hDll==NULL)
	{
		// load any version we can
		hDll = ::LoadLibrary( _T("DBGHELP.DLL") );
	}

	LPCTSTR szResult = NULL;

	if (hDll)
	{
		MINIDUMPWRITEDUMP pDump = (MINIDUMPWRITEDUMP)::GetProcAddress( hDll, "MiniDumpWriteDump" );
		if (pDump)
		{
			TCHAR szDumpPath[_MAX_PATH];
			TCHAR szScratch [SCRATCH_BUFFER_LENGTH + 1];
			szScratch[SCRATCH_BUFFER_LENGTH] = NULL;

			// work out a good place for the dump file
			if (!GetTempPath( _MAX_PATH, szDumpPath ))
				_tcscpy( szDumpPath, _T("c:\\temp\\") );

			_tcscat( szDumpPath, m_szAppName );
			_tcscat( szDumpPath, _T(".dmp") );

			// ask the user if they want to save a dump file
			if (::MessageBox( hParent, _T("Programmer's Notepad 2 has experienced an unexpected problem and is going to close, we apologise for this inconvenience.\n\n Would you like to save a diagnostic file to aid the development team in fixing this problem?"), m_szAppName, MB_YESNO )==IDYES)
			{
				// create the file
				HANDLE hFile = ::CreateFile( szDumpPath, GENERIC_WRITE, FILE_SHARE_WRITE, NULL, CREATE_ALWAYS,
											FILE_ATTRIBUTE_NORMAL, NULL );

				if (hFile != INVALID_HANDLE_VALUE)
				{
					_MINIDUMP_EXCEPTION_INFORMATION ExInfo;

					ExInfo.ThreadId = ::GetCurrentThreadId();
					ExInfo.ExceptionPointers = pExceptionInfo;
					ExInfo.ClientPointers = NULL;

					// write the dump
					BOOL bOK = pDump(GetCurrentProcess(), GetCurrentProcessId(), hFile, MiniDumpNormal, &ExInfo, NULL, NULL);
					if (bOK)
					{
						_sntprintf(szScratch, SCRATCH_BUFFER_LENGTH, _T("A diagnostic dump file has been saved at this location:\n '%s'\n\n\nPlease offer this file to the PN development team using either the discussion mailing list or the bug tracker\n and provide the diagnostic file so that they can try to fix the problem that you have experienced."), szDumpPath);
						szResult = szScratch;
						retval = EXCEPTION_EXECUTE_HANDLER;
					}
					else
					{
						_sntprintf(szScratch, SCRATCH_BUFFER_LENGTH, _T("Failed to save dump file to '%s' (error %d)"), szDumpPath, GetLastError());
						szResult = szScratch;
					}

					::CloseHandle(hFile);
				}
				else
				{
					_sntprintf(szScratch, SCRATCH_BUFFER_LENGTH, _T("Failed to create dump file '%s' (error %d)"), szDumpPath, GetLastError());
					szResult = szScratch;
				}
			}
		}
		else
		{
			szResult = _T("DBGHELP.DLL too old");
		}
	}
	else
	{
		szResult = _T("DBGHELP.DLL not found");
	}

	if (szResult)
	{
		::MessageBox( hParent, szResult, m_szAppName, MB_OK );
	}

	return retval;
}

void MiniDumper::InvalidParameterHandler( const wchar_t * expression, const wchar_t * function, const wchar_t * file, unsigned int line, uintptr_t /*pReserved*/)
{
#ifdef _DEBUG
	wchar_t store_expression[255];
	wchar_t store_function[255];
	wchar_t store_file[MAX_PATH+1];

	wcsncpy(store_expression, expression, 254);
	wcsncpy(store_function, function, 254);
	wcsncpy(store_file, file, MAX_PATH);
#endif

	// Restore UnhandledExceptionFilter
	::SetUnhandledExceptionFilter( TopLevelFilter );
	throw "Invalid Parameter";//std::exception("Invalid Parameter");
}