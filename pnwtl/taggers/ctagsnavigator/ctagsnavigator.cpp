/**
 * @file ctagsnavigator.cpp
 * @brief CTAGS output parser for jump to function implementation.
 * @author Simon Steele
 * @note Copyright (c) 2004-2008 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "ctagsnavigator.h"
#include "../tagtypes.h"
#include "languageTypeTables.h"
#include "languageMapping.h"

using namespace extensions;

#ifdef _DEBUG
#include <stdio.h>
#endif

/**
 * Global instance of the tag source
 */
CTagsTagSource g_source;

/**
 * DllMain
 */
BOOL APIENTRY DllMain( HANDLE /*hModule*/, 
                       DWORD  /*ul_reason_for_call*/, 
                       LPVOID /*lpReserved*/
					 )
{
    return TRUE;
}

/**
 * Initialise the extension
 */
bool __stdcall pn_init_extension(int iface_version, extensions::IPN* pn)
{
	if(iface_version != PN_EXT_IFACE_VERSION)
		return false;

	// Get Taggers Path
	extensions::IOptions* options = pn->GetOptionsManager();
	LPCTSTR taggersPath = options->GetPNPath( PNPATH_TAGGERS );
	g_source.SetTaggersPath(taggersPath);
	pn->ReleaseString( taggersPath );

	g_source.LoadAdditionalLanguages();

	pn->AddTagSource(&g_source);

	return true;
}

/**
 * Get display info
 */
void __stdcall pn_get_extension_info(PN::BaseString& name, PN::BaseString& version)
{
	name = "CTags Tagger";
	version = "1.6";
}

/**
 * Close the extension
 */
void __stdcall pn_exit_extension()
{
	// TODO: Cleanup
}

// Converts wide character to multibyte string
// result is heap allocated
std::wstring ConvertMBtoWC( const char* mbString )
{
	if ( NULL == mbString )
	{
		return NULL;
	}

	// Get necessary size for wide string
	size_t wideSize = ::mbstowcs( NULL, mbString, 0 );
	if ( (size_t)-1 == wideSize )
	{
		return std::wstring(L"");
	}

	std::wstring wstr;
	wstr.resize(wideSize+1);

	// Allocate space for a wide string, leave room for NULL terminator
	//wchar_t* wideString = new wchar_t[ wideSize + 1 ];
	
	// Convert string
	size_t resultSize = ::mbstowcs( &wstr[0], mbString, wideSize );
	if ( (size_t)-1 == resultSize )
	{
		return std::wstring(L"");
	}

	wstr.resize(resultSize);
	return wstr;
}

/**
 * Constructor
 */
CTagsTagSource::CTagsTagSource() : 
	ITagSource(),
	m_schemes( "ant;assembler;basic;cobol;cpp;csharp;batch;eiffel;erlang;flex;fortran;fortran77;java;javascript;lisp;lua;makefile;matlab;ocaml;pascal;perl;php;phpscript;plsql;python;ruby;shell;tcl;tex;vb;verilog;vhdl;vim;yacc;web" )
{
}

/**
 * Load externally defined ctags languages.
 */
void CTagsTagSource::LoadAdditionalLanguages()
{
	std::wstring optionsFile = m_taggersPath + L"ctags\\additionalLanguages.conf";
	
	// Check for file's existence
	DWORD val = ::GetFileAttributes( optionsFile.c_str() );
	if ( INVALID_FILE_ATTRIBUTES != val && ( FILE_ATTRIBUTE_DIRECTORY & val ) == 0 )
	{
		if (optionsFile.size())
		{
			m_optionsParam = L" --options=\"";
			m_optionsParam += optionsFile;
			m_optionsParam += L"\" ";
		}
	}

	std::wstring schemesFile = m_taggersPath + L"ctags\\additionalSupportedSchemes.ini";
	val = ::GetFileAttributes( schemesFile.c_str() );
	if ( INVALID_FILE_ATTRIBUTES != val && ( FILE_ATTRIBUTE_DIRECTORY & val ) == 0 )
	{
		// Load external language type tables and add the supported schemes
		std::string moreSchemes;
		loadExternalTables( schemesFile.c_str(), &moreSchemes );
		m_schemes += moreSchemes;
	}
}

/**
 * Set a path to look in for external ctags definitions
 */
void CTagsTagSource::SetTaggersPath(const wchar_t* path)
{
	m_taggersPath = path;
}

/**
 * Get a list of schemes supported
 */
const char* CTagsTagSource::GetSchemesSupported()
{
	return m_schemes.c_str();
}

#define MAX_LANGUAGE	12
#define CTAGSOPTS	L" --fields=+n -f - " // must start and end with a space.
#define CTAGSLANGOPTS L" --language-force="

/**
 * Called by PN when it wants to have methods returned to it.
 * @param filename the filename of the file to parse. Null if CAPS_PARSEDIRECT is returned in capabilities.
 * @param editorWnd the HWND for the editor window containing code to parse.
 * @param callback function pointer for the callback to provide method information with.
 * @return false if the information cannot be parsed. True otherwise.
 */
bool CTagsTagSource::FindTags(ITagSink* sink,
		const wchar_t*	filename, 
		void*			userData,
		MASKSTRUCT		mask,
		const char*		scheme)
{
	bool bRet = true;

	int* ltypes;
	int* utypes;
	getTables(scheme, &ltypes, &utypes);

	PARSESTATE state;
	memset(&state, 0, sizeof(PARSESTATE));
	state.sink = sink;

	wchar_t* dir = NULL;
	std::wstring clopts(L"ctags");
	clopts += m_optionsParam.c_str();

	LPCWSTR lang = GetLanguage(filename, scheme);
	if(lang != NULL)
	{
		clopts += CTAGSLANGOPTS;
		clopts += lang;
	}

	clopts += CTAGSOPTS;
	clopts += L"\"";
	clopts += filename;
	clopts += L"\"";

	OSVERSIONINFOW osv = {sizeof(OSVERSIONINFOW), 0, 0, 0, 0, L""};

	::GetVersionExW(&osv);
	bool bWin9x = osv.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS;
	
	SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES), 0, 0};
	sa.bInheritHandle = TRUE;
	sa.lpSecurityDescriptor = NULL;

	SECURITY_DESCRIPTOR sd;
	if(!bWin9x)
	{
		// On NT we can have a proper security descriptor...
		::InitializeSecurityDescriptor(&sd, SECURITY_DESCRIPTOR_REVISION);
		::SetSecurityDescriptorDacl(&sd, TRUE, NULL, FALSE);
		sa.lpSecurityDescriptor = &sd;
	}

	HANDLE hWritePipe, hReadPipe;
	HANDLE hStdInWrite, hStdInRead;
	
    if( ! ::CreatePipe(&hReadPipe, &hWritePipe, &sa, 0) )
	{
		return false;
	}

	// read handle, write handle, security attributes,  number of bytes reserved for pipe - 0 default
	
	if( ! ::CreatePipe(&hStdInRead, &hStdInWrite, &sa, 0) )
	{
		::CloseHandle(hReadPipe);
		::CloseHandle(hWritePipe);
		return false;
	}

	::SetHandleInformation(hReadPipe, HANDLE_FLAG_INHERIT, 0);
	::SetHandleInformation(hStdInWrite, HANDLE_FLAG_INHERIT, 0);

	STARTUPINFOW si;
	memset(&si, 0, sizeof(STARTUPINFOW));
	si.cb = sizeof(STARTUPINFOW);
	si.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
	si.wShowWindow = SW_HIDE;
	si.hStdInput = hStdInRead;
	si.hStdOutput = hWritePipe;
	//si.hStdError = hWritePipe;

	PROCESS_INFORMATION pi = {0, 0, 0, 0};

	// CreateProcess can modify the contents of lpCommandLine when using the wide
	// character version, so we put the command line into a byte buffer where this is
	// safe.
	std::vector<wchar_t> cmdbuf(clopts.length() + 1);
	memcpy(&cmdbuf[0], clopts.c_str(), clopts.size()*sizeof(wchar_t));

	bool bCreated = ::CreateProcessW(
		NULL, 
		&cmdbuf[0],
		&sa, /*LPSECURITY_ATTRIBUTES lpProcessAttributes*/
		NULL, /*LPSECURITYATTRIBUTES lpThreadAttributes*/
		TRUE, /*BOOL bInheritHandles*/ 
		CREATE_NEW_PROCESS_GROUP, /*DWORD dwCreationFlags*/
		NULL, /*LPVOID lpEnvironment*/
		dir, /*LPCTSTR lpWorkingDir*/
		&si, /*LPSTARTUPINFO lpStartupInfo*/
		&pi /*LPPROCESS_INFORMATION lpProcessInformation*/ 
	) != 0;

	if(!bCreated)
	{
		::CloseHandle(hReadPipe);
		::CloseHandle(hWritePipe);
		::CloseHandle(hStdInRead);
		::CloseHandle(hStdInWrite);

		return false;
	}

	DWORD dwBytesAvail, dwBytesRead, exitCode, timeDeathDetected;
	dwBytesAvail = dwBytesRead = exitCode = timeDeathDetected = 0;
	bool bCompleted = false;
	bool bLastRead = false;
	
	// Always read 50% into the buffer. Buffer is 2xPARSE_BUFFER_SIZE for
	// continuation purposes.
	char* buffer = &state.buffer[PARSE_BUFFER_SIZE];

//#ifdef _DEBUG
//	DWORD ticks = ::GetTickCount();
//	FILE* fDump = fopen("c:\\tagdump.txt", "wb");
//#endif

	while(!bCompleted || bLastRead)
	{
		// Give ctags a chance to do some work - we give up our timeslice.
		Sleep(0);

		// The PeekNamedPipe function copies data from a named or 
		// anonymous pipe into a buffer without removing it from the pipe.
		if(! ::PeekNamedPipe(hReadPipe, NULL, 0, NULL, &dwBytesAvail, NULL) )
		{
			dwBytesAvail = 0;
		}

		if(dwBytesAvail > 0)
		{
			BOOL bRead = ::ReadFile(hReadPipe, buffer, PARSE_BUFFER_SIZE, &dwBytesRead, NULL);

			if(bRead && dwBytesRead)
			{
//#ifdef _DEBUG
//				fwrite(buffer, dwBytesRead, 1, fDump);
//#endif
				// Parse the CTAGS data:
				if (!parseData(&state, dwBytesRead, mask, userData, ltypes, utypes))
				{
					// Parse failure - most likely we were going to blow our buffer.
					break;
				}
			}
			else
			{
				// Couldn't read from the pipe, must be finished...
				bCompleted = true;
			}
		}
		else
		{
			exitCode = STILL_ACTIVE;

			if(bCompleted)
				bLastRead = false;

			// No data from the process, is it still active?
			::GetExitCodeProcess(pi.hProcess, &exitCode);
			if(STILL_ACTIVE != exitCode)
			{
				if(bWin9x)
				{
					// If we're running on Windows 9x then we give the
					// process some time to return the remainder of its data.
					// We wait until a pre-set amount of time has elapsed and
					// then exit.

					if(timeDeathDetected == 0)
					{
						timeDeathDetected = ::GetTickCount();
					}
					else
					{
						///@todo Get this value from the registry...
						if((::GetTickCount() - timeDeathDetected) > 500)
						{
							bCompleted = true;
						}
					}
				}
				else
				{
					// If NT, then the process is already dead.
					bCompleted = true;

					// Prevent a race condition where the process writes data, and 
					// then exits between our peek and our dead check.
					if(! ::PeekNamedPipe(hReadPipe, NULL, 0, NULL, &dwBytesAvail, NULL) )
					{
						dwBytesAvail = 0;
					}
					else if(dwBytesAvail > 0)
						bLastRead = true;
				}
			}
		}
	} // while (!bCompleted)

//#ifdef _DEBUG
//	ticks = GetTickCount() - ticks;
//	wchar_t timebuf[300];
//	swprintf(timebuf, L"ctagsnavigator time taken: %d\n", ticks);
//	::OutputDebugString(timebuf);
//#endif

	if (WAIT_OBJECT_0 != ::WaitForSingleObject(pi.hProcess, 1000)) 
	{
		bRet = false;
		::TerminateProcess(pi.hProcess, 2);
	}

	::GetExitCodeProcess(pi.hProcess, &exitCode);

	::CloseHandle(pi.hProcess);
	::CloseHandle(pi.hThread);
	::CloseHandle(hReadPipe);
	::CloseHandle(hWritePipe);
	::CloseHandle(hStdInRead);
	::CloseHandle(hStdInWrite);

//#ifdef _DEBUG
//	::fclose(fDump);
//#endif

	return bRet;
}

// check for a CR before the end of the buffer.
bool CTagsTagSource::canParse(char* buffer, DWORD dwLength)
{
	if(dwLength <= 2)
		return false;

	int lastChar = (dwLength/sizeof(char))-1;
	if(buffer[lastChar] == '\n' && buffer[lastChar-1] == '\r')
		return true;

	// ensure we can check right up to the last character.
	char chFinal = buffer[lastChar];
	buffer[lastChar] = '\0';

	bool bReturn = ( strchr(buffer, '\n') != NULL );

	buffer[lastChar] = chFinal;

	return bReturn;
}

#define TAB "\t"

/**
 * Parse a buffer that's been read
 */
bool CTagsTagSource::parseData(LPPARSESTATE state, DWORD dwBytesRead, MASKSTRUCT mask, void* userData, int* ltypes, int* utypes)
{
	char*	pLineEnd;
	
	// offset the start back by any carry-over from the last run.
	char*	pStart = &state->buffer[PARSE_BUFFER_SIZE] - state->extra;
	char*	p = pStart;
	
	// add the carry-over to the bytes read for the available data length.
	int bytesTotal = dwBytesRead + state->extra;
	int bytesLeft = bytesTotal;

	while(bytesLeft > 0)
	{
		if (canParse(p, bytesLeft))
		{
			METHODINFO mi = {0};
			mi.userData = userData;
			
			// terminate the string at the end of this line...
			if ((pLineEnd = strchr(p, '\r')) == NULL)
			{
				if ((pLineEnd = strchr(p, '\n')) == NULL)
				{
					// Couldn't find an end-of-line, we didn't get sensible data
					return false;
				}
			}

			// Make the current line a null-terminated string for parsing.
			*pLineEnd = '\0';

			if (processLine(p, pLineEnd, ltypes, utypes, mi))
			{
				// send method.
				if ( ((maskVals[mi.type].mask1 & mask.mask1) != 0) || ((maskVals[mi.type].mask2 & mask.mask2) != 0) )
					state->sink->OnFound(1, &mi);
			}
			
			p = pLineEnd;

			// skip [13]([10])
			p++;
			if((*p) == '\n')
				p++;

			bytesLeft = bytesTotal - (p - pStart);
		}
		else
			break;
	}
	
	// else store for next run
	if(bytesLeft > 0)
	{
		if (bytesLeft <= PARSE_BUFFER_SIZE)
		{
			// We have room to store what we have and continue parsing:
			memcpy(&state->buffer[PARSE_BUFFER_SIZE-bytesLeft], p, bytesLeft);
			state->extra = bytesLeft;
		}
		else
		{
			// Our buffer is now too big to store and continue parsing, we have to abort.
			return false;
		}
	}
	else
	{
		state->extra = 0;
	}

	return true;
}

bool CTagsTagSource::processLine(char* p, const char* pLineEnd, const int* ltypes, const int* utypes, METHODINFO& mi)
{
	// Skip comment lines...
	if (*p == '!')
	{
		return false;
	}

	// tag
	p = strtok(p, TAB);
	mi.methodName = p;
	
	// skip filename token...
	p = strtok(NULL, TAB);
	if (p == NULL)
	{
		// no TAB found --> return
		return false;
	}

	p += strlen(p) + 1;

	// skip /^ - now in expression to find function (i.e. declaration)
	if (*p == '/' && p[1] == '^')
	{
		p += (2 * sizeof(char));
		
		// find $/
		char* pEndDecl = strrchr(p, '$');
		if (pEndDecl == NULL)
			return false;
		if (*(pEndDecl+1) != '/')  //Check '/' following '$'
			return false;
		*pEndDecl = '\0';
		
		mi.fullText = p;

		// skip ;"[tab]
		p = pEndDecl + 4;
	}
	else
	{
		char* pEndDecl = strchr(p, ';');
		if (pEndDecl == NULL)
			return false;
		p = pEndDecl + 3;
	}

	p = strtok(p, TAB);
	
	// Extensions are all key:value except type which has no : and is just a letter.
	while(p < pLineEnd && p != NULL)
	{
		char* pDelim = strchr(p, ':');
		if(pDelim)
		{
			// delimit and skip...
			*pDelim++ = '\0';
			if(strcmp(p, "line") == 0)
			{
				mi.lineNumber = atoi(pDelim);
			}
			else if(strcmp(p, "class") == 0)
			{
				mi.parentName = pDelim;
			}
			else if(strcmp(p, "struct") == 0)
			{
				mi.parentName = pDelim;
			}
			else if(strcmp(p, "union") == 0)
			{
				mi.parentName = pDelim;
			}
			else if(strcmp(p, "file") == 0)
			{
				// who cares!
			}
		}
		else
		{
			// it's the type...
			if(*p <= 'Z')
				mi.type = utypes[*p - 65];
			else
				mi.type = ltypes[*p - 97];
		}
		
		p = strtok(NULL, TAB);
	}

	return true;
}