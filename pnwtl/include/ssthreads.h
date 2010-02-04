/**
 * @brief Lightweight Win32 C thread wrapper classes
 * @author Simon Steele & Duncan Edwards 
 *
 * The main class in this file (CSSThread) implements thread functionality based
 * around _beginthreadex. There are two ways to use this class:
 *
 * 1. Derive from CSSThread and implement a Run() function. 
 * 2. Use the (rather scary) CSSThreadT and give it a pointer to your function.
 *
 * To start the thread running, simply call Start(). This is a blocking method,
 * and will return only once it is sure the thread is running. After this, you can
 * call Stop() and this function will block until the thread has finished. 
 * 
 * Beware, Stop() has a timeout: it will wait for SS_STOP_WAIT milliseconds and 
 * then if the thread has not returned, it will call TerminateThread on the thread 
 * (assuming it to be locked). If you do not want this behaviour, then you can 
 * change SS_STOP_WAIT to be equal to INFINITE - thus never calling TerminateThread.
 *
 * How will your thread know to finish? Calling Stop() calls SetCanRun(false). In
 * your thread function, you should regularly call GetCanRun() or wait on evtStop.
 * When you exit your Run() function, the class will take care of the rest of the
 * closedown (firing events, closing handles - the whole shebang).
 *
 * This class should be perfectly safe to start and stop willy-nilly.
 *
 * Using the (rather scary) CSSThreadT class:
 * ------------------------------------------
 * Assuming that the class which contains the thread function is called CMyClass,
 * you create a CSSThreadT instance like this:
 *		CSSThreadT<CMyClass>		m_Thread;
 * 
 * In your CMyClass class, you will also need a Thread Run function:
 *		void MyRunFunction(CSSThread* pThread);
 * 
 * You also have to tell the class what function to call, and give it a pointer
 * to the instance of CMyClass:
 *		m_Thread.SetRunFunction(this, MyRunFunction);
 *
 * note: You could initialise these when constructing the thread object:
 *		CSSThreadT<CMyClass> thread(this, MyRunFunction);
 *
 * Once your thread class is configured, simply call m_Thread.Start(), and if you want
 * to wait for it to finish then something like this:
 *		while(!m_Thread.GetStopped(20));
 * would do the trick nicely, waiting 20 milliseconds between checks. Alternatively, simply
 * call Stop() and wait for it to return.
 *
 * History
 * -------
 * 13/12/2002 ss	Added a Critical Section to CSSThread to prevent a deadlock. 
 *					If the thread was started and the run function was empty, then
 *					evtStopped was re-set before the Start() function ever saw it as
 *					false. The critical section prevents this from happening. It may
 *					be used for more synchronisation in the future. GetStopped can now
 *					also wait for a period of time before returning.
 */

#ifndef ssthread_h__included
#define ssthread_h__included

#include <process.h>

// if you don't like this wait timeout, then #define your own
// *before* #including this file (e.g. #define SS_STOP_WAIT INFINITE)
#ifndef SS_STOP_WAIT
#define SS_STOP_WAIT 15000
#endif

#ifndef SSTASSERT
	#ifdef ASSERT
		#define SSTASSERT ASSERT
	#else
		#ifdef ATLASSERT
			#define SSTASSERT ATLASSERT
		#else
			#define SSTASSERT(x) ((void)0)
		#endif
	#endif
#endif

/**
 * @class CSSCritLock
 * @brief A nice Non-MFC CRITICAL_SECTION functionality wrapper.
 */
class CSSCritLock
{
	public:
		// critical-section locking object constructor
		CSSCritLock(CRITICAL_SECTION* pCrit)
		{
			m_pCrit = pCrit;
			if (NULL != m_pCrit)
				::EnterCriticalSection(m_pCrit);
		}

		// critical-section locking object automatically
		// leaves the critical section on destruction
		~CSSCritLock()
		{
			if (NULL != m_pCrit)
				::LeaveCriticalSection(m_pCrit);
		}

	protected:
		CRITICAL_SECTION*	m_pCrit;
};

/**
 * @class CSSThread
 * @brief Thread functionality wrapper.
 *
 * You can derive from this class and provide a Run function, or
 * you can use CSSThreadT which allows you to use more than one
 * instance per class. Start and Stop do the obvious, and you
 * can start after stopping (i.e. run more than once).
 *
 * In your Run() function, either wait on the Handle returned by
 * GetStopHandle() or use the function GetCanRun().
 */
class CSSThread
{
	public:
		CSSThread()
		{
			m_hThread		= NULL;
			m_evtStop		= CreateEvent(NULL, TRUE, FALSE, NULL);
			m_evtStopped	= CreateEvent(NULL, TRUE, FALSE, NULL);
			::SetEvent(m_evtStop);
			::SetEvent(m_evtStopped);
			::InitializeCriticalSection(&m_csInternal);
		}

		virtual ~CSSThread()
		{
			Stop();
			::CloseHandle(m_evtStop);
			::CloseHandle(m_evtStopped);
			::DeleteCriticalSection(&m_csInternal);
		}

		void Start()
		{
			Stop();
			SetCanRun(true);

			unsigned int dwId = 0;
			m_hThread = (HANDLE) _beginthreadex(NULL, 0, &SSThreadFunc, this, 0, &dwId);
			SSTASSERT(m_hThread != NULL);

			::EnterCriticalSection(&m_csInternal);

			while(GetStopped())
				Sleep(1);

			::LeaveCriticalSection(&m_csInternal);
		}

		bool Stop()
		{
			SetCanRun(false);
			if( ::WaitForSingleObject(m_evtStopped, SS_STOP_WAIT) != WAIT_OBJECT_0 )
			{
				::TerminateThread(m_hThread, 0);
			}

			// close the thread-handle
			if (NULL != m_hThread)
			{
				// Note: Should not call CloseHandle on a beginthreadex thread...
				//::CloseHandle(m_hThread);
				m_hThread = NULL;

				return true;
			}

			return false;
		}

		HANDLE GetStopHandle()
		{
			return m_evtStop;
		}
		
		HANDLE GetStoppedHandle()
		{
			return m_evtStopped;
		}

		void SetCanRun(bool bContinue/*=true*/)
		{
			if (bContinue)
				::ResetEvent(m_evtStop);
			else
				::SetEvent(m_evtStop);
		}

		void SetStopped(bool bStopped/*=true*/)
		{
			if (bStopped)
			{
				CSSCritLock lock(&m_csInternal);
				
				::SetEvent(m_evtStopped);
			}
			else
				::ResetEvent(m_evtStopped);
		}

		bool GetCanRun(int timeout = 0)
		{
			// is the "stop" event signalled?
			if (WAIT_OBJECT_0 == ::WaitForSingleObject(m_evtStop, timeout))
				return false;	// don't continue running

			return true;		// do continue running
		}

		bool GetStopped(int timeout = 0)
		{
			// is the "stopped" event signalled?
			if (WAIT_OBJECT_0 == ::WaitForSingleObject(m_evtStopped, timeout))
			{
				if(WAIT_OBJECT_0 == ::WaitForSingleObject(m_hThread, timeout))
					return true;	// thread has stopped
			}
			

			return false;		// thread has not stopped
		}

	protected:
		virtual void Run() = 0;
		virtual void OnException(){}

		virtual void OnBeginThread()
		{
			::CoInitialize(NULL);
		}
		
		virtual void OnEndThread()
		{
			::CoUninitialize();
		}

		CRITICAL_SECTION m_csInternal;
		HANDLE m_evtStopped;
		HANDLE m_evtStop;
		HANDLE m_hThread;

	private:
		static unsigned int __stdcall SSThreadFunc(LPVOID pVoid)
		{
			CSSThread* pThreadClass = static_cast<CSSThread*>(pVoid);

			pThreadClass->OnBeginThread();
			pThreadClass->SetStopped(false);
			
			try
			{
				pThreadClass->Run();
			}
			catch (...)
			{
				pThreadClass->OnException();
			}
			
			pThreadClass->OnEndThread();
			pThreadClass->SetStopped(true);
			
			_endthreadex(0);
			return 0;
		}
};

/**
 * @class CSSThreadT
 * @brief Templated implementation of CSSThread.
 */
template <class T>
class CSSThreadT : public CSSThread
{
	public:
		typedef void (T::*Func)(CSSThread* pThread);

		CSSThreadT() : CSSThread()
		{
			m_pFunc = NULL;
			m_pT = NULL;
		}

		CSSThreadT(T* pInstance, Func function) : CSSThread()
		{
			m_pFunc = function;
			m_pT = pInstance;
		}

		void SetRunFunction(T* pInstance, Func function)
		{
			m_pT = pInstance;
			m_pFunc = function;
		}

	protected:
		Func	m_pFunc;
		T*		m_pT;
		
		virtual void Run()
		{
			SSTASSERT(NULL != m_pT);
			SSTASSERT(NULL != m_pFunc);

			(m_pT->*m_pFunc)(this);
		}
};

#endif
