/**
 * @file ctagsnavigator.cpp
 * @brief CTAGS output parser for jump to function implementation.
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "ctagsnavigator.h"
#include "../tagtypes.h"
#include "languageTypeTables.h"
#include "languageMapping.h"

#include <stdlib.h>

#ifdef _DEBUG
#include <stdio.h>
#endif

BOOL APIENTRY DllMain( HANDLE /*hModule*/, 
                       DWORD  ul_reason_for_call, 
                       LPVOID /*lpReserved*/
					 )
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		break;
	}
    return TRUE;
}

// PN Plugin Capabilities...
#define PNCAPS_FUNCTIONFINDER		1
#define PNCAPS_PARSEDIRECT			1 << 1

typedef struct tagMethodInfo
{
	int			type;			// i.e. PNMETHOD_FUNCTION, PNMETHOD_PROCEDURE, PNMETHOD_CLASS etc.
	const char* methodName;		// i.e. Tag
	const char* parentName;		// i.e. class name, package name etc.
	const char* fullText;		// i.e. void myfunction(string, banana);
	long		lineNumber;		// line number of method in file.
	short		image;			// i.e. PNMETHODIMAGE_FUNCTION, PNMETHODIMAGE_...
} METHODINFO, * LPMETHODINFO;

typedef void (__stdcall *FP_CALLBACK)(int dataCount, LPMETHODINFO methodInfo, LPVOID cookie);

/**
 * Get the capabilities of this plugin - a combination of PNCAPS_ values.
 */
int CPPNAVIGATOR_API PNGetCapabilities()
{
	return PNCAPS_FUNCTIONFINDER;
}

/**
 * Copy the semi-colon separated list of scheme names that this
 * plugin supports for definition finding into the buffer provided.
 */
void CPPNAVIGATOR_API PNFPGetSchemesSupported(wchar_t* schemesBuffer, int cchBuffer)
{
	wcsncpy(schemesBuffer, L"assembler;cobol;cpp;csharp;eiffel;erlang;java;javascript;lisp;lua;makefile;pascal;perl;plsql;python;ruby;shell;tcl;verilog;vim;yacc;web", cchBuffer);
}

#define CTAGSOPTS	L" --fields=+n -f - " // must start and end with a space.
#define CTAGSLANGOPTS L" --language-force="

class TinyString
{
public:
	TinyString(wchar_t* str)
	{
		_str = str;
	}

	~TinyString()
	{
		delete [] _str;
	}

protected:
	wchar_t* _str;
};

#define PARSE_BUFFER_SIZE	16384

typedef struct tagParseState
{
	char		buffer[PARSE_BUFFER_SIZE*2];
	int			extra;
	FP_CALLBACK	callback;
} PARSESTATE, * LPPARSESTATE;

// check for a CR before the end of the buffer.
bool canParse(char* buffer, DWORD dwLength)
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

void parseData(LPPARSESTATE state, DWORD dwBytesRead, int mask, LPVOID cookie)
{
	METHODINFO mi;

	char*	pEndDecl;
	char*	pLineEnd;
	char*	pDelim;
	
	// offset the start back by any carry-over from the last run.
	char*	pStart = &state->buffer[PARSE_BUFFER_SIZE] - state->extra;
	char*	p = pStart;
	
	// add the carry-over to the bytes read for the available data length.
	int bytesTotal = dwBytesRead + state->extra;
	int bytesLeft = bytesTotal;

	int* ltypes;
	int* utypes;
	getTables(L"", &ltypes, &utypes);

	memset(&mi, 0, sizeof(METHODINFO));

	while(bytesLeft > 0)
	{
		if(canParse(p, bytesLeft))
		{
			mi.methodName = NULL;
			mi.parentName = NULL;
			mi.fullText = NULL;

			// terminate the string at the end of this line...
			pLineEnd = strchr(p, 13);
			*pLineEnd = '\0';

			//TODO if first char is ! skip line.
			if(*p != '!')
			{
				// tag
				p = strtok(p, TAB);
				mi.methodName = p;
				
				// skip filename token...
				p = strtok(NULL, TAB);
				p += strlen(p) + 1;

				// skip /^ - now in expression to find function (i.e. declaration)
				if(*p == '/' && p[1] == '^')
				{
					p += (2*sizeof(char));
					
					// find $/
					pEndDecl = strchr(p, '$');
					*pEndDecl = '\0';
					
					mi.fullText = p;

					// skip ;"[tab]
					p = pEndDecl + 4;
				}
				else
				{
					pEndDecl = strchr(p, ';');
					p = pEndDecl + 3;
				}

				p = strtok(p, TAB);
				
				// Extensions are all key:value except type which has no : and is just a letter.
				while(p < pLineEnd && p != NULL)
				{
					pDelim = strchr(p, ':');
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
			}
			
			p = pLineEnd;

			// skip [13]([10])
			p++;
			if((*p) == '\n')
				p++;

			// send method.
			if((maskVals[mi.type] & mask) != 0)
				state->callback(1, &mi, cookie);

			bytesLeft = bytesTotal - (p - pStart);
		}
		else
			break;
	}
	// else store for next run
	if(bytesLeft > 0)
	{
		memcpy(&state->buffer[PARSE_BUFFER_SIZE-bytesLeft], p, bytesLeft);
		state->extra = bytesLeft;
	}
	else
		state->extra = 0;
}

#define MAX_LANGUAGE	12

/**
 * Called by PN when it wants to have methods returned to it.
 * @param filename the filename of the file to parse. Null if CAPS_PARSEDIRECT is returned in capabilities.
 * @param editorWnd the HWND for the editor window containing code to parse.
 * @param callback function pointer for the callback to provide method information with.
 * @return false if the information cannot be parsed. True otherwise.
 */
bool CPPNAVIGATOR_API PNFPGetMethods(
		const wchar_t* filename, 
		HWND /*editorWnd*/, 
		FP_CALLBACK callback, 
		int mask,
		const wchar_t* scheme,
		LPVOID cookie)
{
	bool bRet = true;

	PARSESTATE state;
	memset(&state, 0, sizeof(PARSESTATE));
	state.callback = callback;

	wchar_t* cmd = L"ctags";
	wchar_t* dir = NULL;
	wchar_t* clopts = new wchar_t[wcslen(cmd)+wcslen(CTAGSOPTS)+wcslen(CTAGSLANGOPTS)+MAX_LANGUAGE+wcslen(filename)+3];
	wcscpy(clopts, cmd);

	LPCWSTR lang = GetLanguage(filename, scheme);
	if(lang != NULL)
	{
		wcscat(clopts, CTAGSLANGOPTS);
		wcscat(clopts, lang);
	}

	wcscat(clopts, CTAGSOPTS);
	wcscat(clopts, L"\"");
	wcscat(clopts, filename);
	wcscat(clopts, L"\"");

	TinyString runopts(clopts);

	::OutputDebugString(clopts);
	 ::OutputDebugString(L"\n");

	OSVERSIONINFO osv = {sizeof(OSVERSIONINFO), 0, 0, 0, 0, L""};

	::GetVersionEx(&osv);
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

	STARTUPINFO si;
	memset(&si, 0, sizeof(STARTUPINFO));
	si.cb = sizeof(STARTUPINFO);
	si.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
	si.wShowWindow = SW_HIDE;
	si.hStdInput = hStdInRead;
	si.hStdOutput = hWritePipe;
	si.hStdError = hWritePipe;

	PROCESS_INFORMATION pi = {0, 0, 0, 0};

	bool bCreated = ::CreateProcessW(
		NULL, 
		clopts, 
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

#ifdef _DEBUG
	DWORD ticks = ::GetTickCount();
	FILE* fDump = fopen("c:\\tagdump.txt", "wb");
#endif

	while(!bCompleted || bLastRead)
	{
		// Give ctags a chance to do some work - we give up our timeslice.
		Sleep(0);

		//The PeekNamedPipe function copies data from a named or 
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
#ifdef _DEBUG
				fwrite(buffer, dwBytesRead, 1, fDump);
#endif
				parseData(&state, dwBytesRead, mask, cookie);
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

#ifdef _DEBUG
	ticks = GetTickCount() - ticks;
	wchar_t timebuf[300];
	swprintf(timebuf, L"ctagsnavigator time taken: %d\n", ticks);
	::OutputDebugString(timebuf);
#endif

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

#ifdef _DEBUG
	::fclose(fDump);
#endif

	return bRet;
}