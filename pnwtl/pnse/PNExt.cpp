/**
 * @file PNExt.cpp
 * @brief Shell Extension for Programmer's Notepad
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "..\singleinstance.h"
#include "PNExt.h"
#include "module.h"

#define PN_KEY _T("{FCA6FB45-3224-497a-AC73-C30E498E9ADA}")

/**
 * Global state, just the OS version for this shell extension.
 */
Context g_Context;

// CPNExt
CPNExt::CPNExt()
{
	ZeroMemory(&g_Context.OSVersion, sizeof(OSVERSIONINFO));
	g_Context.OSVersion.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	::GetVersionEx(&g_Context.OSVersion);
}

HRESULT CPNExt::Initialize( LPCITEMIDLIST pidlFolder, LPDATAOBJECT pDataObj, HKEY hProgID )
{
	FORMATETC fmt = { CF_HDROP, NULL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL };
	STGMEDIUM stg = { TYMED_HGLOBAL };
	HDROP     hDrop;
	TCHAR	  szFile[MAX_PATH+1];

	m_files.clear();

    // Look for CF_HDROP data in the data object.
    if( FAILED( pDataObj->GetData ( &fmt, &stg )) )
    {
		// Nope! Return an "invalid argument" error back to Explorer.
		return E_INVALIDARG;
    }

    // Get a pointer to the actual data.
	hDrop = (HDROP) ::GlobalLock( stg.hGlobal );

    // Make sure it worked.
	if ( NULL == hDrop )
	{
		::ReleaseStgMedium( &stg );
        return E_INVALIDARG;
	}

	// Sanity check – make sure there is at least one filename.
	UINT uNumFiles = ::DragQueryFile( hDrop, 0xFFFFFFFF, NULL, 0 );

    if ( 0 == uNumFiles )
	{
		::GlobalUnlock ( stg.hGlobal );
		::ReleaseStgMedium ( &stg );
        return E_INVALIDARG;
	}

	// Get all the filenames...
	for( UINT uFile = 0; uFile < uNumFiles; uFile++ )
	{
        // Get the next filename.
		if ( 0 == ::DragQueryFile( hDrop, uFile, szFile, MAX_PATH ))
            continue;

		m_files.push_back( tstring(szFile) );
	}

	::GlobalUnlock ( stg.hGlobal );
	::ReleaseStgMedium ( &stg );

    return ( m_files.size() > 0 ) ? S_OK : E_INVALIDARG;
}

/**
 * IContextMenu::QueryContextMenu lets us modify the shell's menu
 */
HRESULT CPNExt::QueryContextMenu(
	HMENU hmenu,		// Handle to the menu
    UINT  uMenuIndex,	// Index where we should start adding our items
    UINT  uidFirstCmd,	// First ID we can use for our command
    UINT  uidLastCmd,	// Last ID we can use for our command
    UINT  uFlags		// Why explorer is calling us
	)
{
	// We should not do anything if CMF_DEFAULTONLY is set.
	if ( uFlags & CMF_DEFAULTONLY )
	{
		return MAKE_HRESULT( SEVERITY_SUCCESS, FACILITY_NULL, 0 );
	}

	// Insert our one item.
	::InsertMenu( hmenu, uMenuIndex, MF_BYPOSITION, uidFirstCmd, _T("Programmer's Notepad") );

    return MAKE_HRESULT( SEVERITY_SUCCESS, FACILITY_NULL, 1 );
}

/**
 * IContextMenu::GetCommandString provides context help for our shell
 * extension and also provide a command verb.
 */
HRESULT CPNExt::GetCommandString (
    UINT idCmd,			// 0 based index of our command (always 0 for this one)
    UINT uFlags,		// Flags
    UINT *pwReserved,	// reserved.
    LPSTR pszName,		// Where we store the text
    UINT cchMax			// How much text we can store
	)
{
	USES_CONVERSION;

	// Check idCmd, it must be 0 since we have only one menu item.
	if ( 0 != idCmd )
		return E_INVALIDARG;

	// If Explorer is asking for a help string, copy our string into the
	// supplied buffer.
	if ( uFlags & GCS_HELPTEXT )
	{
		LPCTSTR szText = _T("Edit file(s) with Programmer's Notepad");

		if ( uFlags & GCS_UNICODE )
		{
			// We need to cast pszName to a Unicode string, and then use the
			// Unicode string copy API.
			lstrcpynW( (LPWSTR) pszName, T2CW(szText), cchMax );
		}
		else
		{
			// Use the ANSI string copy API to return the help string.
			lstrcpynA( pszName, T2CA(szText), cchMax );
		}

		return S_OK;
	}

	return E_INVALIDARG;
}

/**
 * Hey hey! The user wants us to do our stuff...
 */
HRESULT CPNExt::InvokeCommand( LPCMINVOKECOMMANDINFO pCmdInfo )
{
	// If lpVerb really points to a string, ignore this function call and bail out.
	if ( 0 != HIWORD( pCmdInfo->lpVerb ))
		return E_INVALIDARG;

	// Get the command index - the only valid one is 0.
	switch ( LOWORD( pCmdInfo->lpVerb ))
	{
	case 0:
		{
#ifdef DEBUG
			::OutputDebugString(_T("Sending File(s) to PN"));
#endif
			InformPN();

			return S_OK;
		}
		break;

	default:
		return E_INVALIDARG;
	}
}

void CPNExt::InformPN()
{
	// TODO Add "Local\\" to the key.
	MultipleInstanceManager checkMI(PN_KEY);
	if (checkMI.AlreadyActive())
	{
		checkMI.SendParameters(m_files);
	}
	else
	{
		InformNewPN();
	}
}

void CPNExt::InformNewPN()
{
	TCHAR szPath[MAX_PATH+1];
	DWORD res = ::GetModuleFileName( _hInstance, szPath, MAX_PATH );
	
	// Check failure values...
	if(res == 0 || res == MAX_PATH)
		return;

	if(::PathRemoveFileSpec(szPath) == 0)
		return;

	if(!::PathAppend(szPath, _T("pn.exe")))
		return;

	// Build command-line
	tstring cmdline(_T("\""));
	cmdline += szPath;
	cmdline += _T("\"");
	for(std::list<tstring>::const_iterator i = m_files.begin(); i != m_files.end(); ++i)
	{
		cmdline += _T(" \"");
		cmdline += (*i);
		cmdline += _T("\"");
	}

	// Launch the process
	STARTUPINFO si = {0};
	si.cb = sizeof(STARTUPINFO);

	PROCESS_INFORMATION pi = {0};

	TCHAR* buf = new TCHAR[cmdline.size()+1];
	_tcscpy(buf, cmdline.c_str());

#ifdef DEBUG
	::OutputDebugString(_T("Going to CreateProcess PN"));
	::OutputDebugString(buf);
#endif
	if( ::CreateProcess(
		NULL,
		buf,
		NULL,
		NULL,
		FALSE,
		0,
		NULL,
		NULL,
		&si,
		&pi) )
	{
		// Congratulations, it's a beautiful baby PN, now let go!
		if(pi.hThread)
			::CloseHandle(pi.hThread);
		if(pi.hProcess)
			::CloseHandle(pi.hProcess);
	}

	delete [] buf;
}