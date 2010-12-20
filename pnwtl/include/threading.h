#ifndef PNOTEPAD_INCLUDE_THREADING_H__INCLUDED
#define PNOTEPAD_INCLUDE_THREADING_H__INCLUDED

namespace pnutils { 

class WinHandle
{
public:
	explicit WinHandle(HANDLE handle) : m_handle(handle)
	{
	}

	~WinHandle()
	{
		::CloseHandle(m_handle);
	}

	HANDLE Get() const
	{
		return m_handle;
	}

private:
	HANDLE m_handle;
};

namespace threading {

class CriticalSection
{
public:
	explicit CriticalSection()
	{
		::InitializeCriticalSection(&m_crit);
	}

	~CriticalSection()
	{
		::DeleteCriticalSection(&m_crit);
	}

	void Enter()
	{
		::EnterCriticalSection(&m_crit);
	}

	void Leave()
	{
		::LeaveCriticalSection(&m_crit);
	}

private:
	CRITICAL_SECTION m_crit;
};

class CritLock
{
public:
	explicit CritLock(CriticalSection& criticalSection) : m_cs(criticalSection)
	{
		m_cs.Enter();
	}

	~CritLock()
	{
		m_cs.Leave();
	}

private:
	CriticalSection& m_cs;
};

class WinEvent
{
public:
	explicit WinEvent(bool manualReset) : m_hEvent(::CreateEvent(NULL, manualReset, FALSE, NULL))
	{
	}

	HANDLE Get() const
	{
		return m_hEvent.Get();
	}

	void Reset()
	{
		::ResetEvent(m_hEvent.Get());
	}

	void Set()
	{
		::SetEvent(m_hEvent.Get());
	}

	DWORD Wait(DWORD timeOut)
	{
		return ::WaitForSingleObject(m_hEvent.Get(), timeOut);
	}

private:
	WinHandle m_hEvent;
};

class ManualResetEvent : public WinEvent
{
public:
	explicit ManualResetEvent() : WinEvent(true)
	{
	}
};

class Thread
{
public:
	explicit Thread() : m_thread(NULL)
	{
	}

	~Thread()
	{
		if (m_thread != NULL)
		{
			Join(10000);
			Reset();
		}
	}

	HANDLE Get() const
	{
		return m_thread;
	}

	bool Create(unsigned (__stdcall *startAddress )( void * ), void* args)
	{
		unsigned int thrdid;
		m_thread = (HANDLE)_beginthreadex(NULL, 0, startAddress, args, 0, &thrdid);
		return m_thread != NULL;
	}

	bool Join(int waitMilliseconds)
	{
		return ::WaitForSingleObject(m_thread, 10000) == WAIT_OBJECT_0;
	}

	void Reset()
	{
		::CloseHandle(m_thread);
		m_thread = NULL;
	}

	bool Valid() const
	{
		return m_thread != NULL;
	}

private:
	HANDLE m_thread;
};

}} // namespace pnutils::threading

#endif // #ifndef PNOTEPAD_INCLUDE_THREADING_H__INCLUDED