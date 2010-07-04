/**
 * @file toolrunner.h
 * @brief Run external tools
 * @author Simon Steele
 * @note Copyright (c) 2002-2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef toolrunner_h__included
#define toolrunner_h__included

#include "include/ssthreads.h"

#define TOOLS_BUFFER_SIZE 16384

// Predeclarations:
class ToolWrapper;

/**
 * Class to run external tools.
 */
class ToolRunner : public CSSThread
{
public:
	ToolRunner(ToolWrapper* pWrapper);
	~ToolRunner();
	
	int Execute();

	bool GetThreadedExecution();

	ToolRunner* m_pNext;

protected:
	int Run_NoCapture(LPCTSTR command, LPCTSTR params, LPCTSTR dir);
	int Run_Capture(LPCTSTR command, LPCTSTR params, LPCTSTR dir);

	virtual void Run();
	virtual void OnException();

	int GetExitCode();
	void PostRun();

protected:
	ToolWrapper*		m_pWrapper;
	int					m_RetCode;
	time_t				m_starttime;
};

#endif //#ifndef toolrunner_h__included