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

		void			MoveUp(ToolDefinition* pDef);
		void			MoveDown(ToolDefinition* pDef);

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

//#include "aboutdlg.h"

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

class ToolWrapper : public ToolDefinition
{
	public:
		ToolWrapper(CChildFrame* pActiveChild, const ToolDefinition& definition)
		{
			m_hNotifyWnd = NULL;
			m_pActiveChild = pActiveChild;
			ToolDefinition::_copy(definition);

			::InitializeCriticalSection(&m_csStatusLock);
			SetRunning(true);
		}

		virtual ~ToolWrapper()
		{
			::DeleteCriticalSection(&m_csStatusLock);
		}

		CChildFrame* GetActiveChild()
		{
			return m_pActiveChild;
		}

		void SetNotifyWindow(HWND hWnd)
		{
			m_hNotifyWnd = hWnd;
		}

		virtual void OnFinished()
		{
			if(m_hNotifyWnd)
			{
				::PostMessage(m_hNotifyWnd, PN_TOOLFINISHED, 0, 0);
			}
		}

		void SetRunning(bool bRunning)
		{
			CSSCritLock lock(&m_csStatusLock);
			m_bRunning = bRunning;
		}

		bool IsRunning()
		{
			CSSCritLock lock(&m_csStatusLock);
			return m_bRunning;
		}
		
		virtual void Revert() = 0;
		virtual void ShowOutputWindow() = 0;
		virtual void _AddToolOutput(LPCTSTR output, int nLength = -1) = 0;
		virtual void SetToolBasePath(LPCTSTR path) = 0;
		virtual void SetToolParser(bool bBuiltIn, LPCTSTR customExpression = NULL) = 0;
		virtual void ClearOutput() = 0;

	protected:
		CChildFrame*		m_pActiveChild;
		CRITICAL_SECTION	m_csStatusLock;
		HWND				m_hNotifyWnd;
		bool				m_bRunning;
};

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
	int Run_ShellExecute(LPCTSTR command, LPCTSTR params, LPCTSTR dir);
	int Run_CreateProcess(LPCTSTR command, LPCTSTR params, LPCTSTR dir);

	virtual void Run();
	virtual void OnException();

	int GetExitCode();
	void PostRun();

protected:
	ToolWrapper*		m_pWrapper;
	int					m_RetCode;
	IToolOutputSink*	m_pOutputter;
};

template <class TWindowOwner, class TOutputSink>
class ToolWrapperT : public ToolWrapper
{
	public:
		ToolWrapperT(TWindowOwner* pWindowOwner, TOutputSink* pOutputSink, CChildFrame* pActiveChild, const ToolDefinition& definition)
			:ToolWrapper(pActiveChild, definition)
		{
			m_pWindowOwner = pWindowOwner;
			m_pOutputSink = pOutputSink;
		}

		virtual ~ToolWrapperT(){}

		virtual void Revert() 
		{
			if( m_pActiveChild != NULL )
				m_pActiveChild->Revert();
		}

		virtual void ShowOutputWindow()
		{
			m_pWindowOwner->ToggleOutputWindow(true, true);
		}

		virtual void _AddToolOutput(LPCTSTR output, int nLength = -1)
		{
			ShowOutputWindow();
			m_pOutputSink->AddToolOutput(output, nLength);
		}

		virtual void SetToolBasePath(LPCTSTR path)
		{
			m_pOutputSink->SetToolBasePath(path);
		}

		virtual void SetToolParser(bool bBuiltIn, LPCTSTR customExpression = NULL)
		{
			m_pOutputSink->SetToolParser(bBuiltIn, customExpression);
		}

		virtual void ClearOutput()
		{
			m_pOutputSink->ClearOutput();
		}

	protected:
		TWindowOwner*	m_pWindowOwner;
		TOutputSink*	m_pOutputSink;
};

typedef void* ToolOwnerID;

/**
 * To run a tool, the caller must orphan a ToolWrapper instance to the
 * ToolOwner class. This class is then used to provide access to the 
 * methods necessary for a tool to be run with output capturing etc.
 */
class ToolOwner : public Singleton<ToolOwner, SINGLETON_AUTO_DELETE>
{
	friend class Singleton<ToolOwner, SINGLETON_AUTO_DELETE>;

	public:
		void RunTool(ToolWrapper* pTool, ToolOwnerID OwnerID);

		void KillTools(bool bWaitForKill, ToolOwnerID OwnerID = 0);

		void MarkToolForDeletion(ToolRunner* pRunningTool);

		bool HaveRunningTools(ToolOwnerID OwnerID = 0);

	protected:
		ToolOwner();
		~ToolOwner();

		struct _ToolWrapper
		{
			ToolOwnerID		OwnerID;
			ToolWrapper*	pWrapper;
			ToolRunner*		pRunner;
			bool			bDelete;
		};

		typedef std::list<_ToolWrapper>	RTOOLS_LIST;

		void cleanup();

		//void UpdateTools(CScheme* pScheme);

		//IMPLEMENT THESE:
		//void ToggleOutputWindow(bool bSetValue = false, bool bSetShowing = true);
		//void UpdateRunningTools();

	protected:
		CRITICAL_SECTION	m_crRunningTools;
		RTOOLS_LIST			m_RunningTools;
		ToolRunner*			m_pFirstTool;
};

#define IMPLEMENT_TOOLOWNER() \
	MESSAGE_HANDLER(PN_TOGGLEOUTPUT, OnToggleOutputWindow)

#endif