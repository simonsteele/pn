/**
 * @file tools.h
 * @brief External tools code
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef tools_h__included
#define tools_h__included

typedef std::list<ToolDefinition*> TOOLDEFS_LIST;

#include "include/ssthreads.h"

#define TOOLS_BUFFER_SIZE 16384

class CChildFrame;

/**
 * @brief Collection class representing tools associated with one scheme
 */
class SchemeTools
{
	public:
		SchemeTools(){}
		SchemeTools(LPCTSTR schemename);
		~SchemeTools();

		TOOLDEFS_LIST&	GetTools();
		int				GetMenu(CSMenuHandle& menu, int iInsertAfter, int iCommand = TOOLS_RUNTOOL);

		void			Add(ToolDefinition* pDef);
		void			Delete(ToolDefinition* pDef);

		void			WriteDefinition(ofstream& stream);

		void			ReleaseMenuResources();

	protected:
		void			BuildMenu(int iCommand);
		void			InternalWriteDefinition(ofstream& stream);
		
		TOOLDEFS_LIST	m_Tools;
		tstring			m_Scheme;
		CSPopupMenu		m_Menu;
};

class GlobalTools : public SchemeTools
{
	public:
		//GlobalTools();
		void WriteDefinition(ofstream& stream);
};

/**
 * @brief Class can be used both standalone and as a singleton.
 */
class SchemeToolsManager : 
	public Singleton<SchemeToolsManager, SINGLETON_AUTO_DELETE>, 
	public XMLParseState
{
	public:
		SchemeToolsManager();
		virtual ~SchemeToolsManager();

		SchemeTools* GetGlobalTools();

		SchemeTools* GetToolsFor(LPCTSTR scheme);
		int GetMenuFor(LPCTSTR scheme, CSMenuHandle& menu, int iInsertBefore);

		void ReLoad(bool bWantMenuResources = false);
		void Save();

		int UpdateToolsMenu(CSMenuHandle& tools, int iFirstToolCmd, int iDummyID, LPCSTR schemename);

	protected:
		void Clear(bool bWantMenuResources = false);

		// Scheme & Tool Creation
		void processScheme(XMLAttributes& atts);
		void processGlobal(XMLAttributes& atts);
		void processTool(XMLAttributes& atts);

		// XML Parsing
		virtual void startElement(LPCTSTR name, XMLAttributes& atts);
		virtual void endElement(LPCTSTR name);
		virtual void characterData(LPCTSTR data, int len){}

	protected:
		typedef std::map<tstring, SchemeTools*> SCHEMETOOLS_MAP;

		SchemeTools*	m_pCur;
		GlobalTools*	m_pGlobalTools;
		SCHEMETOOLS_MAP m_toolSets;
};

#include "aboutdlg.h"

/**
 * Format string builder class to build up command-line parameters for a tool.
 */
class CToolCommandString : public CustomFormatStringBuilder<CToolCommandString>
{
	public:
		void OnFormatChar(TCHAR thechar);

		CChildFrame* pChild;

	protected:
		TCHAR itosbuf[100];
};

/**
 * Class which formats GetLastError information into a useful string.
 */
class CLastErrorInfo
{
	public:
		CLastErrorInfo()
		{
			DWORD m_nRetCode = ::GetLastError();
			m_lpMsgBuf = NULL;
			::FormatMessage(
				FORMAT_MESSAGE_ALLOCATE_BUFFER |
				FORMAT_MESSAGE_FROM_SYSTEM |
				FORMAT_MESSAGE_IGNORE_INSERTS,
				NULL,
				m_nRetCode,
				MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),   // Default language
				reinterpret_cast<LPTSTR>(&m_lpMsgBuf),
				0,
				NULL
			);
		}

		~CLastErrorInfo()
		{
			if(m_lpMsgBuf)
				::LocalFree(m_lpMsgBuf);
		}

		operator LPCTSTR ()
		{
			return reinterpret_cast<LPCTSTR>(m_lpMsgBuf);
		}

		int GetErrorCode()
		{
			return m_nRetCode;
		}

	protected:
		LPVOID	m_lpMsgBuf;
		int		m_nRetCode;
};

/**
 * Class to run external tools.
 */
class ToolRunner : public CSSThread
{
public:
	ToolRunner(CChildFrame* pChild, ToolDefinition* pDef, IToolOutputSink* pOutputSink);
	~ToolRunner();
	
	int Execute();

	bool GetThreadedExecution();

	const ToolDefinition* GetToolDef();

	ToolRunner* m_pNext;

	int GetExitCode();

protected:
	int Run_ShellExecute(LPCTSTR command, LPCTSTR params, LPCTSTR dir);
	int Run_CreateProcess(LPCTSTR command, LPCTSTR params, LPCTSTR dir);

	virtual void Run();
	virtual void OnException();

protected:
	CChildFrame*		m_pChild;
	ToolDefinition*		m_pTool;
	int					m_RetCode;
	ToolDefinition*		m_pCopyDef;
	IToolOutputSink*	m_pOutputter;
};

/**
 * @param t_bDoc True if the implementing class is a document, and thus has a Revert() method.
 */
template <class T, bool t_bDoc = true>
class ToolOwner : public IToolOutputSink
{
	protected:
		void OnRunTool(LPVOID pVoid)
		{
			T* pT = static_cast<T*>(this);
			ToolDefinition* pTool = reinterpret_cast<ToolDefinition*>(pVoid);
			ToolRunner *r = new ToolRunner(pT, pTool, pTool->GlobalOutput() ? GetGlobalOutputSink() : this );
			
			bool bThreaded = r->GetThreadedExecution();
			if(bThreaded)
				AddRunningTool(r);
			
			if(pTool->SaveAll())
				g_Context.m_frame->SaveAll();

			r->Execute();
			
			if(!bThreaded)
			{
				PostRun(r, pTool);

				delete r;
			}
		}

		LRESULT OnToggleOutputWindow(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
		{
			T* pT = static_cast<T*>(this);
			pT->ToggleOutputWindow(wParam != 0, lParam != 0);

			return 0;
		}

		virtual void _AddToolOutput(LPCSTR outputstring, int nLength = -1)
		{
			T* pT = static_cast<T*>(this);
			
			// We do a sendmessage so that the windows are created in the 
			// window thread, and not in any calling thread.
			pT->SendMessage(PN_TOGGLEOUTPUT, 1, 1);

			pT->AddToolOutput(outputstring, nLength);
		}

		void AddRunningTool(ToolRunner* pRunner)
		{
			T* pT = static_cast<T*>(this);

			CSSCritLock lock(&m_crRunningTools);

			if(m_pFirstTool)
				pRunner->m_pNext = m_pFirstTool;

			m_pFirstTool = pRunner;

			pT->UpdateRunningTools();
		}

		void ToolFinished(ToolRunner* pRunner)
		{
			T* pT = static_cast<T*>(this);

			CSSCritLock lock(&m_crRunningTools);

			if(m_pFirstTool)
			{
				ToolRunner* pTool = m_pFirstTool;
				ToolRunner* pP = NULL;

				while(pTool != NULL && pTool != pRunner)
				{
					pP = pTool;
					pTool = pTool->m_pNext;
				}

				if(pTool)
				{
					if(pP)
						pP->m_pNext = pTool->m_pNext;

					if(m_pFirstTool == pTool)
						m_pFirstTool = pTool->m_pNext;
				}

				do{
				}
				while(!pRunner->GetStopped(20));

				if(pRunner->GetToolDef())
				{
					PostRun(pRunner, pRunner->GetToolDef());
				}

				delete pRunner;

				pT->UpdateRunningTools();
			}
		}

		void PostRun(ToolRunner* r, const ToolDefinition* t)
		{
			T* pT = static_cast<T*>(this);

			if( t->CaptureOutput() )
			{
				tstring exitcode(_T("\n> Process Exit Code: "));
				exitcode += IntToTString(r->GetExitCode());
				exitcode += _T("\n");
				
				IToolOutputSink* pSink = t->GlobalOutput() ? 
					GetGlobalOutputSink() : pT;
				
				pSink->_AddToolOutput(exitcode.c_str());
			}

			if(t_bDoc)
			{
				if( t->IsFilter() )
					pT->Revert();
			}
		}

		void KillTools(bool bWaitForKill)
		{
			int iLoopCount = 0;

			// Signal to all tools to exit, scope to enter and exit critical section
			{
				CSSCritLock lock(&m_crRunningTools);

				ToolRunner* pTool = m_pFirstTool;
				while(pTool)
				{
					pTool->SetCanRun(false);
					pTool = pTool->m_pNext;
				}
			}

			while(bWaitForKill)
			{
				// Normally, we give all the tools a chance to exit before continuing...
				Sleep(100);
				iLoopCount++;

				// Don't tolerate more than 5 seconds of waiting...
				if(iLoopCount > 50)
					break;

				{
					CSSCritLock lock(&m_crRunningTools);
					if(!m_pFirstTool)
						break;
				}
			}
		}

		IToolOutputSink* GetGlobalOutputSink()
		{
			return g_Context.m_frame->GetGlobalOutputSink();
		}

		//void UpdateTools(CScheme* pScheme);

		//IMPLEMENT THESE:
		//void ToggleOutputWindow(bool bSetValue = false, bool bSetShowing = true);
		//void UpdateRunningTools();

	protected:
		CRITICAL_SECTION	m_crRunningTools;
		ToolRunner*			m_pFirstTool;
};

#define IMPLEMENT_TOOLOWNER() \
	MESSAGE_HANDLER(PN_TOGGLEOUTPUT, OnToggleOutputWindow)

#endif