/**
 * @file toolrunner.cpp
 * @brief Run external tools
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"

#include "ChildFrm.h"   // for document info
#include "tools.h"      // tools
#include "toolrunner.h"

#include <time.h>
#include <sys/timeb.h>

//////////////////////////////////////////////////////////////////////////////
// ToolRunner
//////////////////////////////////////////////////////////////////////////////

ToolRunner::ToolRunner(ToolWrapper* pWrapper)
{
	m_pWrapper = pWrapper;
	m_RetCode = 0;
	m_pNext = NULL;
}

ToolRunner::~ToolRunner()
{

}

bool ToolRunner::GetThreadedExecution()
{
	if(m_pWrapper)
	{
		return m_pWrapper->CaptureOutput();
	}

	return false;
}

int ToolRunner::GetExitCode()
{
	return m_RetCode;
}

/**
 * Thread Run Function, calls Run_CreateProcess and notifies all 
 * interested parties on completion.
 */
void ToolRunner::Run()
{
	time(&m_starttime);
	m_pWrapper->OnStart();
	m_RetCode = Run_Capture(m_pWrapper->Command.c_str(), m_pWrapper->Params.c_str(), m_pWrapper->Folder.c_str());
	PostRun();
	m_pWrapper->SetRunning(false);
	m_pWrapper->OnFinished();
	ToolOwner::GetInstance()->MarkToolForDeletion(this);
}

void ToolRunner::OnException()
{
	LOG(_T("PN2: Exception whilst running a tool.\n"));
}

int ToolRunner::Run_Capture(LPCTSTR command, LPCTSTR params, LPCTSTR dir)
{
	tstring clopts(_T("\"\" "));
	clopts.insert(1, command);
	clopts += params;

	if (!m_pWrapper->IsTextFilter())
	{
		tstring tempstr(clopts);
		tempstr.insert(0, _T("> "));
		tempstr += _T("\n");
		m_pWrapper->_AddToolOutput(tempstr.c_str());
	}

	if (_tcslen(dir) == 0)
		dir = NULL;

	OSVERSIONINFO osv = {sizeof(OSVERSIONINFO), 0, 0, 0, 0, _T("")};

	::GetVersionEx(&osv);
	bool bWin9x = osv.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS;
	
	SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES), 0, 0};
	sa.bInheritHandle = TRUE;
	sa.lpSecurityDescriptor = NULL;

	SECURITY_DESCRIPTOR sd;
	if (!bWin9x)
	{
		// On NT we can have a proper security descriptor...
		::InitializeSecurityDescriptor(&sd, SECURITY_DESCRIPTOR_REVISION);
		::SetSecurityDescriptorDacl(&sd, TRUE, NULL, FALSE);
		sa.lpSecurityDescriptor = &sd;
	}

	HANDLE hWritePipe, hReadPipe;
	HANDLE hStdInWrite, hStdInRead;
	
    if (!::CreatePipe(&hReadPipe, &hWritePipe, &sa, 0))
	{
		CLastErrorInfo lei;
		m_pWrapper->_AddToolOutput(_T("\n> Failed to create StdOut and StdErr Pipe: "));
		m_pWrapper->_AddToolOutput((LPCTSTR)lei);

		return lei.GetErrorCode();
	}

	// read handle, write handle, security attributes,  number of bytes reserved for pipe - 0 default
	
	if (!::CreatePipe(&hStdInRead, &hStdInWrite, &sa, 0))
	{
		CLastErrorInfo lei;

		m_pWrapper->_AddToolOutput(_T("\n> Failed to create StdIn Pipe: "));
		m_pWrapper->_AddToolOutput((LPCTSTR)lei);

		return lei.GetErrorCode();
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

	DWORD dwCreationFlags = CREATE_NEW_CONSOLE; // CREATE_NEW_PROCESS_GROUP;

	std::vector<TCHAR> cmdbuf(clopts.length() + 1);
	memcpy(&cmdbuf[0], clopts.c_str(), clopts.size() * sizeof(TCHAR));

	bool bCreated = ::CreateProcess(
		NULL, 
		&cmdbuf[0], 
		&sa, /*LPSECURITY_ATTRIBUTES lpProcessAttributes*/
		NULL, /*LPSECURITYATTRIBUTES lpThreadAttributes*/
		TRUE, /*BOOL bInheritHandles*/ 
		dwCreationFlags, /*DWORD dwCreationFlags*/
		NULL, /*LPVOID lpEnvironment*/
		dir, /*LPCTSTR lpWorkingDir*/
		&si, /*LPSTARTUPINFO lpStartupInfo*/
		&pi /*LPPROCESS_INFORMATION lpProcessInformation*/ 
	) != 0;

	if (!bCreated)
	{
		::CloseHandle(hReadPipe);
		::CloseHandle(hWritePipe);
		::CloseHandle(hStdInRead);
		::CloseHandle(hStdInWrite);

		CLastErrorInfo lei;
		m_pWrapper->_AddToolOutput(_T("\n> Failed to create process: "));
		m_pWrapper->_AddToolOutput((LPCTSTR)lei);

		return lei.GetErrorCode();
	}

	unsigned int dwBytesToWrite;
	unsigned char* stdinbuf;
	stdinbuf = m_pWrapper->GetStdIOBuffer(dwBytesToWrite);
	if (!dwBytesToWrite)
		stdinbuf = NULL;

	DWORD dwBytesAvail, dwBytesRead, exitCode, timeDeathDetected;
	dwBytesAvail = dwBytesRead = exitCode = timeDeathDetected = 0;
	DWORD dwBytesWritten = 0;
	bool bCompleted = false;
	BYTE buffer[TOOLS_BUFFER_SIZE];

	while (!bCompleted)
	{
		Sleep(50);

		if (dwBytesToWrite > 0)
		{
			// We have a filter (we think), so write the filter data into the stdin
			// of the process we started.
			DWORD dwWrote;
			::WriteFile(hStdInWrite, stdinbuf+dwBytesWritten, dwBytesToWrite, &dwWrote, NULL);
			dwBytesWritten += dwWrote;
			dwBytesToWrite -= dwWrote;
			
			if(dwBytesToWrite == 0)
			{
				// Now close stdin so that the filter doesn't wait forever for more data.
				::CloseHandle(hStdInWrite);
				hStdInWrite = NULL;
			}
		}

		//The PeekNamedPipe function copies data from a named or 
		// anonymous pipe into a buffer without removing it from the pipe.
		if (!::PeekNamedPipe(hReadPipe, NULL, 0, NULL, &dwBytesAvail, NULL) )
		{
			dwBytesAvail = 0;
		}

		if (dwBytesAvail > 0)
		{
			BOOL bRead = ::ReadFile(hReadPipe, buffer, sizeof(buffer)-1, &dwBytesRead, NULL);
			buffer[dwBytesRead] = NULL;

			if (bRead && dwBytesRead)
			{
				CA2CT conv(reinterpret_cast<const char*>(buffer));
				m_pWrapper->_AddToolOutput(&conv[0], -1);
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

			// No data from the process, is it still active?
			::GetExitCodeProcess(pi.hProcess, &exitCode);
			if (STILL_ACTIVE != exitCode)
			{
				if (bWin9x)
				{
					// If we're running on Windows 9x then we give the
					// process some time to return the remainder of its data.
					// We wait until a pre-set amount of time has elapsed and
					// then exit.

					if (timeDeathDetected == 0)
					{
						timeDeathDetected = ::GetTickCount();
					}
					else
					{
						///@todo Get this value from the registry...
						if ((::GetTickCount() - timeDeathDetected) > 500)
						{
							bCompleted = true;
						}
					}
				}
				else
				{
					// If NT, then the process is already dead.
					bCompleted = true;
				}
			}
		}

		// While we're here, we check to see if we've been told to close.
		if (!GetCanRun())
		{
			if (WAIT_OBJECT_0 != ::WaitForSingleObject(pi.hProcess, 500)) 
			{
				// We should do this only if the GUI process is stuck and
				// don't answer to a normal termination command.
				// This function is dangerous: dependant DLLs don't know the process
				// is terminated, and memory isn't released.
				m_pWrapper->_AddToolOutput(_T("\n> Forcefully terminating process...\n"));
				::TerminateProcess(pi.hProcess, 1);
			}

			bCompleted = true;
		}
	} // while (!bCompleted)

	if (WAIT_OBJECT_0 != ::WaitForSingleObject(pi.hProcess, 1000)) 
	{
		m_pWrapper->_AddToolOutput(_T("\n> Process failed to respond; forcing abrupt termination..."));
		::TerminateProcess(pi.hProcess, 2);
	}

	::GetExitCodeProcess(pi.hProcess, &exitCode);

	m_RetCode = exitCode;

	::CloseHandle(pi.hProcess);
	::CloseHandle(pi.hThread);
	::CloseHandle(hReadPipe);
	::CloseHandle(hWritePipe);
	::CloseHandle(hStdInRead);
	
	if (hStdInWrite) // might already be closed
		::CloseHandle(hStdInWrite);

	return m_RetCode;
}

struct ShellErr 
{
	DWORD code;
	LPCTSTR description;
};

int ToolRunner::Run_NoCapture(LPCTSTR command, LPCTSTR params, LPCTSTR dir)
{
	int result = 0;

	tstring tempstr(_T("\"\" "));
	tempstr.insert(1, command);
	tempstr += params;

	if (_tcslen(dir) == 0)
		dir = NULL;

	OSVERSIONINFO osv = {sizeof(OSVERSIONINFO), 0, 0, 0, 0, _T("")};

	::GetVersionEx(&osv);
	bool bWin9x = osv.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS;
	
	SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES), 0, 0};
	sa.bInheritHandle = TRUE;
	sa.lpSecurityDescriptor = NULL;

	SECURITY_DESCRIPTOR sd;
	if (!bWin9x)
	{
		// On NT we can have a proper security descriptor...
		::InitializeSecurityDescriptor(&sd, SECURITY_DESCRIPTOR_REVISION);
		::SetSecurityDescriptorDacl(&sd, TRUE, NULL, FALSE);
		sa.lpSecurityDescriptor = &sd;
	}

	STARTUPINFO si;
	memset(&si, 0, sizeof(STARTUPINFO));
	si.cb = sizeof(STARTUPINFO);

	PROCESS_INFORMATION pi = {0, 0, 0, 0};

	std::vector<TCHAR> cmdbuf(tempstr.length()+1);
	memcpy(&cmdbuf[0], tempstr.c_str(), tempstr.size() * sizeof(TCHAR));

	DWORD dwCreationFlags = CREATE_NEW_CONSOLE; // CREATE_NEW_PROCESS_GROUP - causes Ctrl-C to be disabled.

	bool bCreated = ::CreateProcess(
		NULL, 
		&cmdbuf[0], 
		&sa, /*LPSECURITY_ATTRIBUTES lpProcessAttributes*/
		NULL, /*LPSECURITYATTRIBUTES lpThreadAttributes*/
		TRUE, /*BOOL bInheritHandles*/ 
		dwCreationFlags, /*DWORD dwCreationFlags*/
		NULL, /*LPVOID lpEnvironment*/
		dir, /*LPCTSTR lpWorkingDir*/
		&si, /*LPSTARTUPINFO lpStartupInfo*/
		&pi /*LPPROCESS_INFORMATION lpProcessInformation*/ 
	) != 0;

	if (!bCreated)
	{
		CLastErrorInfo lei;
		m_pWrapper->_AddToolOutput(_T("\n> Failed to create process: "));
		m_pWrapper->_AddToolOutput((LPCTSTR)lei);

		return lei.GetErrorCode();
	}

	// We're waiting for this one to finish because it's a filter process - 
	// i.e. it will change the current file.
	if (m_pWrapper->IsFilter())
	{
		::WaitForSingleObject(pi.hProcess, INFINITE);
		DWORD dwExitCode;
		::GetExitCodeProcess(pi.hProcess, &dwExitCode);
		result = dwExitCode;
	}
	else
	{
		result = pi.dwProcessId;
	}
	
	// Detach...
	::CloseHandle(pi.hProcess);
	::CloseHandle(pi.hThread);

	PostRun();

	return result;
}

int ToolRunner::Execute()
{
	ToolCommandString builder;
	builder.reversePathSeps = m_pWrapper->ShouldUseForwardSlashes();
	builder.pChild = m_pWrapper->GetActiveChild();
	
	try
	{
		m_pWrapper->Command = builder.Build(m_pWrapper->Command.c_str());
		m_pWrapper->Params = builder.Build(m_pWrapper->Params.c_str());
		m_pWrapper->Folder = builder.Build(m_pWrapper->Folder.c_str());
	}
	catch (FormatStringBuilderException&)
	{
		LOG(_T("FormatStringBuilderException building format string - user wants to cancel"));
		return 0;
	}

	if(!m_pWrapper->CaptureOutput())
	{
		Run_NoCapture(m_pWrapper->Command.c_str(), m_pWrapper->Params.c_str(), m_pWrapper->Folder.c_str());
	}
	else
	{
		m_pWrapper->SetToolParser( !m_pWrapper->UseCustomParser(), m_pWrapper->CustomParsePattern.c_str() );
		m_pWrapper->SetToolBasePath( m_pWrapper->Folder.c_str() );

		if(m_pWrapper->ShouldClearOutput())
			m_pWrapper->ClearOutput();

		// Launch the thread which will run the CreateProcess stuff...
		Start();
	}

	return 1;
}

void ToolRunner::PostRun()
{
	if( m_pWrapper->CaptureOutput() && !m_pWrapper->IsTextFilter() )
	{
		tstring exitcode(LS(IDS_TOOL_EXITCODE));
		exitcode += IntToTString(GetExitCode());
		exitcode += LS(IDS_TOOL_TIMETAKEN);
		time_t endtime;
		time(&endtime);
		endtime -= m_starttime;
		
		char buf[100];
		strftime(buf, 100, "%M:%S", gmtime(&endtime));

		CA2CT bufconv(buf);
		exitcode += bufconv;
		
		exitcode += _T("\n");

		m_pWrapper->_AddToolOutput(exitcode.c_str());
	}

	if( m_pWrapper->IsFilter() )
		m_pWrapper->Revert();
}

//////////////////////////////////////////////////////////////////////////////
// ToolOwner
//////////////////////////////////////////////////////////////////////////////

ToolOwner::ToolOwner()
{
	::InitializeCriticalSection(&m_crRunningTools);
}

ToolOwner::~ToolOwner()
{
	::DeleteCriticalSection(&m_crRunningTools);
}

/**
 * @param pTool ToolWrapper instance to be orphaned to ToolOwner.
 * @param OwnerID Unique Identifier for the owning object - use "this".
 */
void ToolOwner::RunTool(ToolWrapperPtr& pTool, ToolOwnerID OwnerID)
{
	_ToolWrapper _wrapper = {0};
	_wrapper.OwnerID = OwnerID;
	_wrapper.pWrapper = pTool;
	_wrapper.pRunner = new ToolRunner( pTool.get() );

	bool bThreaded = _wrapper.pRunner->GetThreadedExecution();
	
	if( bThreaded )
	{
		CSSCritLock lock(&m_crRunningTools);

		m_RunningTools.push_back(_wrapper);	 // Add this tool to our list to mind.
	}

	if(pTool->SaveAll())
	{
		g_Context.m_frame->SaveAll();
	}
	else if(pTool->SaveProjectGroup())
	{
		DocumentList list;
		g_Context.m_frame->GetOpenWorkspaceDocuments(list);

		for (DocumentList::iterator i = list.begin(); i != list.end(); ++i)
		{
			CChildFrame* frame = (*i)->GetFrame();
			if (frame != NULL && frame->GetModified())
			{
				frame->Save(true); // save and notify change
			}
		}
	}
	else if (pTool->SaveOne())
	{
		CChildFrame* pChild = pTool->GetActiveChild();
		if (pChild && pChild->GetModified())
		{
			pChild->Save(true); // save and notify change
		}
	}

	if (!_wrapper.pRunner->Execute() && bThreaded)
	{
		// Signal this tool wrapper as done with.
		MarkToolForDeletion(_wrapper.pRunner);
	}
	else if( !bThreaded )
	{
		delete _wrapper.pRunner;
		_wrapper.pWrapper.reset();
	}

	///@todo
	//pT->UpdateRunningTools();
}

/**
 * @brief ToolRunner calls this after finishing running. 
 * This avoids deadlock because we do lazy deletion.
 */
void ToolOwner::MarkToolForDeletion(ToolRunner* pRunningTool)
{
	CSSCritLock lock(&m_crRunningTools);

	for(RTOOLS_LIST::iterator i = m_RunningTools.begin();
		i != m_RunningTools.end();
		++i)
	{
		_ToolWrapper& tool = (*i);
		if(tool.pRunner == pRunningTool)
		{
			tool.bDelete = true;
			break;
		}
	}
}

/**
 * @return true if the owner has any tools still running.
 */
bool ToolOwner::HaveRunningTools(ToolOwnerID OwnerID)
{
	CSSCritLock lock(&m_crRunningTools);

	// Get rid of any that are hanging around.
	cleanup();

	for(RTOOLS_LIST::const_iterator i = m_RunningTools.begin();
		i != m_RunningTools.end();
		++i)
	{
		if((OwnerID == 0 || (*i).OwnerID == OwnerID) && (*i).pWrapper->IsRunning())
		{
			return true;
		}
	}

	return false;
}

/**
 * cleanup performs lazy deletion on the tool wrappers that we know 
 * about. Anything marked as deletable in here is finished running
 * and can be deleted.
 */
void ToolOwner::cleanup()
{
	CSSCritLock lock(&m_crRunningTools);

	RTOOLS_LIST::iterator i = m_RunningTools.begin();
	while( i != m_RunningTools.end() )
	{
		_ToolWrapper& tool = (*i);
		if(tool.bDelete)
		{
			delete tool.pRunner;
			tool.pWrapper.reset();
			RTOOLS_LIST::iterator del = i;
			++i;
			m_RunningTools.erase(del);
		}
		else
			++i;
	}
}

void ToolOwner::KillTools(bool bWaitForKill, ToolOwnerID OwnerID)
{
	int iLoopCount = 0;

	// Signal to all tools to exit, scope to enter and exit critical section
	{
		CSSCritLock lock(&m_crRunningTools);

		for(RTOOLS_LIST::iterator i = m_RunningTools.begin();
			i != m_RunningTools.end();
			++i)
		{
			_ToolWrapper& tool = (*i);
			if(OwnerID == 0 || tool.OwnerID == OwnerID)
				tool.pRunner->SetCanRun(false);
		}
	}

	while(bWaitForKill)
	{
		// Normally, we give all the tools a chance to exit before continuing...
		Sleep(100);
		iLoopCount++;

		// Don't tolerate more than 5 seconds of waiting...
		if(iLoopCount > 50)
		{
			LOG(_T("PN2: Gave up waiting for tools to finish."));
			break;
		}

		{
			CSSCritLock lock(&m_crRunningTools);

			// Delete any items that have been marked as finished.
			cleanup();

			bool bFound = false;
		
			for(RTOOLS_LIST::iterator j = m_RunningTools.begin();
				j != m_RunningTools.end();
				++j)
			{
				// See if there are any tools matching the OwnerID still running.
				if(OwnerID == 0 || (*j).OwnerID == OwnerID)
				{
					bFound = true;
					break;
				}
			}

			// If we didn't find any running tools then we're home and dry.
			if( !bFound )
				break;
		}
	}

	// Run a last cleanup for good measure.
	cleanup();
}