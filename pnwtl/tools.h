/**
 * @file tools.h
 * @brief External tools code
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef tools_h__included
#define tools_h__included

#include "include/threading.h"

// Predeclares:

class CChildFrame;
class ToolRunner;
class ToolsXMLWriter;

namespace Projects
{
	class Workspace;
	class Project;
}

// Typedefs:

typedef void* ToolOwnerID;

// Classes:

class ToolSource
{
public:
	tstring FileName;
};

class SourcedToolDefinition : public ToolDefinition
{
public:
	SourcedToolDefinition(const ToolSource* source_);
	SourcedToolDefinition(const SourcedToolDefinition& copy);

public:
	const ToolSource* source;
};

typedef std::list<SourcedToolDefinition*> TOOLDEFS_LIST;

/**
 * @brief Collection class representing tools associated with one scheme
 */
class SchemeTools
{
	public:
		SchemeTools();
		SchemeTools(LPCSTR schemename);
		virtual ~SchemeTools();

		TOOLDEFS_LIST&	GetTools();
		//int				GetMenu(CSMenuHandle& menu, int iInsertAfter, int iCommand = TOOLS_RUNTOOL);

		virtual void	Add(SourcedToolDefinition* pDef);
		void			Delete(ToolDefinition* pDef);

		void			MoveUp(ToolDefinition* pDef);
		void			MoveDown(ToolDefinition* pDef);

		virtual void	WriteDefinition(ToolsXMLWriter& writer, ToolSource* source);

		// You only need to do the following if you can't call GetMenu on the manager...
		void			AllocateMenuResources(CommandDispatch* dispatcher, int iCommand = TOOLS_RUNTOOL);
		void			ReleaseMenuResources(CommandDispatch* dispatcher);

		HACCEL			GetAcceleratorTable();

	protected:
		bool			ToolsInSource(ToolSource* source);
		void			BuildMenu(int iCommand);
		void			InternalWriteDefinition(ToolsXMLWriter& writer, ToolSource* source);
		
		TOOLDEFS_LIST	m_Tools;
		std::string		m_Scheme;
		CSPopupMenu		m_Menu;
		HACCEL			m_hAccel;
};

class GlobalTools : public SchemeTools
{
	public:
		void WriteDefinition(ToolsXMLWriter& writer, ToolSource* source);
};

class ProjectTools : public SchemeTools
{
	public:
		ProjectTools(LPCTSTR id);

		virtual void Add(SourcedToolDefinition* pDef);
		virtual void WriteDefinition(ToolsXMLWriter& writer, ToolSource* source);
	
	protected:
		tstring	m_ProjectID;
};

class GlobalProjectTools : public ProjectTools
{
	public:
		GlobalProjectTools();
		virtual void WriteDefinition(ToolsXMLWriter& writer, ToolSource* source);
};

/**
 * Format string builder class to build up command-line parameters for a tool.
 */
class ToolCommandString : public CustomFormatStringBuilder<ToolCommandString>
{
	public:
		void OnFormatChar(TCHAR thechar);
		void OnFormatKey(LPCTSTR key);
		void OnFormatPercentKey(LPCTSTR key);
		void OnFormatScriptRef(LPCTSTR key);

		CChildFrame* pChild;
		bool reversePathSeps;

	protected:
		TCHAR itosbuf[100];

		Projects::Workspace* GetWorkspace();
		Projects::Project* GetActiveProject();
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
 * Provide functionality needed for a tool when running.
 */
class ToolWrapper : public ToolDefinition
{
	public:
		ToolWrapper(CChildFrame* pActiveChild, const ToolDefinition& definition);
		virtual ~ToolWrapper();

		CChildFrame* GetActiveChild();

		void SetNotifyWindow(HWND hWnd);

		virtual void OnStart();
		virtual void OnFinished();

		void SetRunning(bool bRunning);
		bool IsRunning();

		void SwapInStdInBuffer(std::vector<unsigned char>& buffer);
		unsigned char* GetStdIOBuffer(unsigned int& size);

		virtual void Revert() = 0;
		virtual void ShowOutputWindow() = 0;
		virtual void _AddToolOutput(LPCTSTR output, int nLength = -1) = 0;
		virtual void SetToolBasePath(LPCTSTR path) = 0;
		virtual void SetToolParser(bool bBuiltIn, const char* customExpression = NULL) = 0;
		virtual void ClearOutput() = 0;

	protected:
		std::vector<unsigned char> m_stdin;
		CChildFrame*		m_pActiveChild;
		pnutils::threading::CriticalSection m_csStatusLock;
		HWND				m_hNotifyWnd;
		bool				m_bRunning;
};

/**
 * Implements the default behaviour for tool output, regardless of the
 * type of text view being used.
 */
template <class TWindowOwner, class TOutputSink>
class ToolWrapperT : public ToolWrapper
{
	public:
		ToolWrapperT(TWindowOwner* pWindowOwner, TOutputSink* pOutputSink, CChildFrame* pActiveChild, const ToolDefinition& definition)
			:ToolWrapper(pActiveChild, definition)
		{
			m_pWindowOwner = pWindowOwner;
			m_pOutputSink = pOutputSink;
			m_bOutputShown = false;
		}

		virtual ~ToolWrapperT(){}

		/**
		 * Revert is called for tools that modify the current file.
		 */
		virtual void Revert()
		{
			if (m_pActiveChild != NULL)
			{
				// Send message, we're probably not in the UI thread here.
				m_pActiveChild->SendMessage(WM_COMMAND, MAKEWPARAM(ID_FILE_REVERT, 0), 0);
			}
		}

		/**
		 * Display whichever output window is relevant
		 */
		virtual void ShowOutputWindow()
		{
			m_pWindowOwner->ToggleOutputWindow(true, true);
		}

		/**
		 * Add output text
		 */
		virtual void _AddToolOutput(LPCTSTR output, int nLength = -1)
		{
			if (!m_bOutputShown)
			{
				ShowOutputWindow();
				m_bOutputShown = true;
			}

			m_pOutputSink->AddToolOutput(output, nLength);
		}

		/**
		 * Set the path the tool is being run from, allowing relative path matching in the output window to work
		 */
		virtual void SetToolBasePath(LPCTSTR path)
		{
			m_pOutputSink->SetToolBasePath(path);
		}

		/**
		 * Set the parser for the output
		 */
		virtual void SetToolParser(bool bBuiltIn, const char* customExpression = NULL)
		{
			m_pOutputSink->SetToolParser(bBuiltIn, customExpression);
		}

		/**
		 * Clear the output view
		 */
		virtual void ClearOutput()
		{
			m_pOutputSink->ClearOutput();
		}

	protected:
		bool			m_bOutputShown;
		TWindowOwner*	m_pWindowOwner;
		TOutputSink*	m_pOutputSink;
};

typedef boost::shared_ptr<ToolWrapper> ToolWrapperPtr;

/**
 * To run a tool, the caller must orphan a ToolWrapper instance to the
 * ToolOwner class. This class is then used to provide access to the 
 * methods necessary for a tool to be run with output capturing etc.
 */
class ToolOwner : public Singleton<ToolOwner, SINGLETON_AUTO_DELETE>
{
	friend class Singleton<ToolOwner, SINGLETON_AUTO_DELETE>;

	public:
		void RunTool(ToolWrapperPtr& pTool, ToolOwnerID OwnerID);

		void KillTools(bool bWaitForKill, ToolOwnerID OwnerID = 0);

		void MarkToolForDeletion(ToolRunner* pRunningTool);

		bool HaveRunningTools(ToolOwnerID OwnerID = 0);

	protected:
		ToolOwner();
		~ToolOwner();

		struct _ToolWrapper
		{
			ToolOwnerID		OwnerID;
			ToolRunner*		pRunner;
			bool			bDelete;

			ToolWrapperPtr pWrapper;
		};

		typedef std::list<_ToolWrapper>	RTOOLS_LIST;

		void cleanup();

	protected:
		CRITICAL_SECTION	m_crRunningTools;
		RTOOLS_LIST			m_RunningTools;
		ToolRunner*			m_pFirstTool;
};

#endif