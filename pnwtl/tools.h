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
		void OnFormatChar(TCHAR thechar)
		{
			switch(thechar)
			{
				case _T('f'):
					m_string += pChild->GetFileName(FN_FILE);
					break;

				case _T('d'):
					m_string += pChild->GetFileName(FN_PATH);
					break;

				case _T('n'):
					m_string += pChild->GetFileName(FN_FILEPART);
					break;

				case _T('l'):
					_itoa(pChild->GetPosition(EP_LINE), itosbuf, 10);
					m_string += itosbuf;
					break;

				case _T('c'):
					_itoa(pChild->GetPosition(EP_COL), itosbuf, 10);
					m_string += itosbuf;
					break;

				case _T('?'):
					{
						CInputDialog* dlg = new CInputDialog(_T("Tool Parameters"), _T("Parameters:"));
						if( dlg->DoModal() == IDOK )
						{
							m_string += dlg->GetInput();
						}
						delete dlg;
					}
					break;
			}		
		}

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
	ToolRunner(CChildFrame* pChild, ToolDefinition* pDef);
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
};

#endif