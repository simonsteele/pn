/**
 * @file tools.cpp
 * @brief External tools code
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "tools.h"
#include "childfrm.h"

#include "resource.h"
#include "pndialogs.h" // for InputBox...

#include "project.h"

#include <sstream>
#include <fstream>
#include <algorithm>

#if defined (_DEBUG)
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

//////////////////////////////////////////////////////////////////////////////
// SchemeTools
//////////////////////////////////////////////////////////////////////////////

SchemeTools::SchemeTools()
{
	m_hAccel = NULL;
}

SchemeTools::SchemeTools(LPCTSTR schemename)
{
	m_Scheme = schemename;
	m_hAccel = NULL;
}

SchemeTools::~SchemeTools()
{
	for(TOOLDEFS_LIST::iterator i = m_Tools.begin(); i != m_Tools.end(); ++i)
	{
		delete *i;
	}

	m_Tools.clear();

	if(m_hAccel)
	{
		::DestroyAcceleratorTable(m_hAccel);
		m_hAccel = NULL;
	}
}

TOOLDEFS_LIST& SchemeTools::GetTools()
{
	return m_Tools;
}

#include "include/sscontainers.h"

//CString CHotKeyCtrl::GetKeyName(UINT vk, BOOL fExtended)
tstring SchemeTools::GetKeyName(UINT vk, bool extended) const
{
	LONG lScan = MapVirtualKey(vk, 0) << 16;

	// if it's an extended key, add the extended flag
	if (extended)
		lScan |= 0x01000000L;

	tstring str;
	GArray<TCHAR> tcbuf;
	
	int nBufferLen = 64;
	int nLen;
	do
	{
		nBufferLen *= 2;
		//LPTSTR psz = str.GetBufferSetLength(nBufferLen);
		tcbuf.grow(nBufferLen);
		nLen = ::GetKeyNameText(lScan, &tcbuf[0], nBufferLen + 1);
	}
	while (nLen == nBufferLen);
	return tstring(&tcbuf[0]);
}

static const TCHAR szPlus[] = _T(" + ");

tstring SchemeTools::GetShortcutText(int wCode, int wModifiers) const
{
	tstring strKeyName;

	if (wCode != 0 || wModifiers != 0)
	{
		if (wModifiers & HOTKEYF_CONTROL)
		{
			strKeyName += GetKeyName(VK_CONTROL, false);
			strKeyName += szPlus;
		}

		if (wModifiers & HOTKEYF_SHIFT)
		{
			strKeyName += GetKeyName(VK_SHIFT, false);
			strKeyName += szPlus;
		}

		if (wModifiers & HOTKEYF_ALT)
		{
			strKeyName += GetKeyName(VK_MENU, false);
			strKeyName += szPlus;
		}

		strKeyName += GetKeyName(wCode, (wModifiers & HOTKEYF_EXT) != 0);
	}

	return strKeyName;
}

///@return ID of last command added...
int SchemeTools::GetMenu(CSMenuHandle& menu, int iInsertBefore, int iCommand)
{
	int iLastCommand = iInsertBefore;
	if(m_Tools.size() != 0)
	{
		CSMenuManager* pMan = CSMenuManager::GetInstance();
		
		for(TOOLDEFS_LIST::const_iterator i = m_Tools.begin(); i != m_Tools.end(); ++i)
		{
			ToolDefinition* pT = (*i);
			
            if(pT->CommandID == -1)
				pT->CommandID = pMan->RegisterCallback(pT->CommandID, NULL, iCommand, (LPVOID)pT);

			tstring str = pT->Name;
			if(pT->Shortcut != 0)
			{
				tstring sc = GetShortcutText(LOBYTE(pT->Shortcut), HIBYTE(pT->Shortcut));
				if(sc.length() > 0)
				{
					str += "\t";
					str += sc;
				}
			}

			::InsertMenu(menu, iInsertBefore, MF_BYCOMMAND | MF_STRING, pT->CommandID, str.c_str());
			iLastCommand = pT->CommandID;
		}
	}

	return iLastCommand;
}

void SchemeTools::AllocateMenuResources(int iCommand)
{
	if(m_Tools.size() != 0)
	{
		CSMenuManager* pMan = CSMenuManager::GetInstance();
		for(TOOLDEFS_LIST::const_iterator i = m_Tools.begin(); i != m_Tools.end(); ++i)
		{
			ToolDefinition* pT = (*i);
			
			if(pT->CommandID == -1)
				pT->CommandID = pMan->RegisterCallback(pT->CommandID, NULL, iCommand, (LPVOID)pT);
		}
	}
}

/**
 * This function should be called before this class is deleted if the 
 * menu command IDs used by it are to be made available again.
 */
void SchemeTools::ReleaseMenuResources()
{
	if(m_Tools.size() != 0)
	{
		ToolDefinition* pT = NULL;
		CSMenuManager* pMan = CSMenuManager::GetInstance();

		for(TOOLDEFS_LIST::const_iterator i = m_Tools.begin(); i != m_Tools.end(); ++i)
		{
			pT = (*i);
			if(pT->CommandID != -1)
			{
				pMan->UnRegisterCallback(pT->CommandID);
				pT->CommandID = -1;
			}
		}
	}
}

void AccelFromCode(ACCEL& accel, WORD wCmd, DWORD hotkey)
{
	ATLASSERT(wCmd != 0);
	ATLASSERT(hotkey != 0);
	accel.cmd = wCmd;
	accel.key = LOBYTE(hotkey);
	accel.fVirt = FVIRTKEY | FNOINVERT;
	if( HIBYTE(hotkey) & HOTKEYF_ALT ) accel.fVirt |= FALT;
	if( HIBYTE(hotkey) & HOTKEYF_CONTROL ) accel.fVirt |= FCONTROL;
	if( HIBYTE(hotkey) & HOTKEYF_SHIFT ) accel.fVirt |= FSHIFT;      
}

HACCEL SchemeTools::GetAcceleratorTable()
{
	if(m_hAccel)
		return m_hAccel;

	if(m_Tools.size() == 0)
		return NULL;

	ACCEL* accels = new ACCEL[m_Tools.size()];
	int accelCount = 0;
	ToolDefinition* pT = NULL;
	for(TOOLDEFS_LIST::const_iterator i = m_Tools.begin(); i != m_Tools.end(); ++i)
	{
		pT = (*i);
		
		if(pT->CommandID == -1) // check we've got command ids sorted.
			throw "You must allocate menu resources before asking for an accelerator table.";

		if(pT->Shortcut != 0)
		{
			AccelFromCode(accels[accelCount], pT->CommandID, pT->Shortcut);
			accelCount++;
		}
	}

	if(accelCount > 0)
		m_hAccel = ::CreateAcceleratorTable(accels, accelCount);

	delete [] accels;

	return m_hAccel;
}

void SchemeTools::Add(SourcedToolDefinition* pDef)
{
	m_Tools.push_back(pDef);
	pDef->CommandID = -1;
}

void SchemeTools::Delete(ToolDefinition* pDef)
{
	m_Tools.remove((SourcedToolDefinition*)pDef);
	delete pDef;
}

void SchemeTools::MoveUp(ToolDefinition* pDef)
{
	TOOLDEFS_LIST::iterator i = std::find(m_Tools.begin(), m_Tools.end(), (SourcedToolDefinition*)pDef);
	if(i != m_Tools.end() && i != m_Tools.begin())
	{
		TOOLDEFS_LIST::iterator j = i;
		i--;
		std::iter_swap(i, j);
	}
}

void SchemeTools::MoveDown(ToolDefinition* pDef)
{
	TOOLDEFS_LIST::iterator i = std::find(m_Tools.begin(), m_Tools.end(), (SourcedToolDefinition*)pDef);
	if(i != m_Tools.end())
	{
		TOOLDEFS_LIST::iterator j = i;
		i++;
		if(i != m_Tools.end())
			std::iter_swap(i, j);
	}
}

void SchemeTools::WriteDefinition(ofstream& stream, ToolSource* source)
{
	if(ToolsInSource(source))
	{
		stream << "\t<scheme name=\"" << m_Scheme << "\">\n";
		
		InternalWriteDefinition(stream, source);	

		stream << "\t</scheme>\n";
	}
}

void SchemeTools::InternalWriteDefinition(ofstream& stream, ToolSource* source)
{
	for(TOOLDEFS_LIST::const_iterator i = m_Tools.begin(); i != m_Tools.end(); ++i)
	{
		if( (*i)->source != source )
			continue;
		int flags = (*i)->iFlags;
		stream << "\t\t<tool name=\"" << FormatXML((*i)->Name) << "\" ";
		stream << "command=\"" << FormatXML((*i)->Command) << "\" ";
		stream << "folder=\"" << FormatXML((*i)->Folder) << "\" ";
		stream << "params=\"" << FormatXML((*i)->Params) << "\" ";
		stream << "shortcut=\"" << (*i)->Shortcut << "\" ";
		stream << "parsepattern=\"" << FormatXML((*i)->CustomParsePattern) << "\" ";
		stream << "flags=\"" << flags << "\" ";
		stream << "/>\n";
	}
}

bool SchemeTools::ToolsInSource(ToolSource* source)
{
	for(TOOLDEFS_LIST::const_iterator i = m_Tools.begin(); i != m_Tools.end(); ++i)
	{
		if( (*i)->source == source )
			return true;
	}

	return false;
}

//////////////////////////////////////////////////////////////////////////////
// GlobalTools
//////////////////////////////////////////////////////////////////////////////

void GlobalTools::WriteDefinition(ofstream& stream, ToolSource* source)
{
	if(ToolsInSource(source))
	{
		stream << "\t<global>\n";
		
		InternalWriteDefinition(stream, source);	

		stream << "\t</global>\n";
	}
}

//////////////////////////////////////////////////////////////////////////////
// CToolCommandString
//////////////////////////////////////////////////////////////////////////////

void CToolCommandString::OnFormatChar(TCHAR thechar)
{
	switch(thechar)
	{
		case _T('f'):
			m_string += pChild->GetFileName(FN_FILE);
			break;

		case _T('d'):
			{
				CPathName pn(pChild->GetFileName(FN_PATH));
				if(reversePathSeps)
					pn.SetForwardSlashes();
				m_string += pn;
			}
			break;

		case _T('n'):
			m_string += pChild->GetFileName(FN_FILEPART);
			break;

		case _T('l'):
			_itot(pChild->GetPosition(EP_LINE), itosbuf, 10);
			m_string += itosbuf;
			break;

		case _T('c'):
			_itot(pChild->GetPosition(EP_COL), itosbuf, 10);
			m_string += itosbuf;
			break;

		case _T('w'):
			m_string += pChild->GetTextView()->GetCurrentWord();
			break;

		// current project file.
		case _T('p'):
			{
				Projects::Workspace* pWorkspace = g_Context.m_frame->GetActiveWorkspace();
				if(pWorkspace != NULL)
				{
					Projects::Project* pProject = pWorkspace->GetActiveProject();
					
					if(pProject != NULL && pProject->Exists())
					{
						CFileName fn(pProject->GetFileName());
						if(reversePathSeps)
							fn.SetForwardSlashes();
						m_string += fn;
					}
				}
			}
			break;

		// current project group (workspace) file.
		case _T('g'):
			{
				Projects::Workspace* pWorkspace = GetWorkspace();
				if(pWorkspace != NULL && pWorkspace->CanSave())
				{
					CFileName fn(pWorkspace->GetFileName());
					if(reversePathSeps)
						fn.SetForwardSlashes();
					m_string += fn;
				}
			}
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

#define MATCH(s) \
	(_tcscmp(key, s) == 0)

void CToolCommandString::OnFormatKey(LPCTSTR key)
{
	if(MATCH(_T("ProjectPath")))
	{
		Projects::Project* pP = GetActiveProject();
		if(pP)
		{
			CFileName fn(pP->GetFileName());
			if(reversePathSeps)
				fn.SetForwardSlashes();
			m_string += fn.GetPath();
		}
	}
	else if(MATCH(_T("ProjectGroupPath")))
	{
		Projects::Workspace* pWorkspace = GetWorkspace();
		if(pWorkspace != NULL && pWorkspace->CanSave())
		{
			CFileName fn(pWorkspace->GetFileName());
			if(reversePathSeps)
				fn.SetForwardSlashes();
			m_string += fn.GetPath();
		}
	}
	else
	{
		tstring s = _T("Unknown constant: $(");
		s += key;
		s += ").";
		g_Context.m_frame->SetStatusText(s.c_str());
	}
}

#undef MATCH

Projects::Workspace* CToolCommandString::GetWorkspace()
{
	Projects::Workspace* pWorkspace = g_Context.m_frame->GetActiveWorkspace();
	return pWorkspace;
}

Projects::Project* CToolCommandString::GetActiveProject()
{
	Projects::Workspace* pWorkspace = GetWorkspace();
	if(pWorkspace != NULL)
	{
		Projects::Project* pProject = pWorkspace->GetActiveProject();
		
		if(pProject != NULL && pProject->Exists())
			return pProject;
	}
	return NULL;
}

//////////////////////////////////////////////////////////////////////////////
// SchemeToolsManager
//////////////////////////////////////////////////////////////////////////////

//SchemeToolsManager* SchemeToolsManager::s_pTheInstance = NULL;

SchemeToolsManager::SchemeToolsManager()
{
	m_pCur = NULL;
	m_pGlobalTools = NULL;

	ReLoad();
}

SchemeToolsManager::~SchemeToolsManager()
{
	Clear();
}

void SchemeToolsManager::Clear(bool bWantMenuResources)
{
	for(SCHEMETOOLS_MAP::iterator i = m_toolSets.begin(); i != m_toolSets.end(); ++i)
	{
		if(bWantMenuResources)
			(*i).second->ReleaseMenuResources();
		delete (*i).second;
	}

	for(SOURCES_LIST::iterator j = m_toolSources.begin(); j != m_toolSources.end(); ++j)
	{
		if((*j) != &m_DefaultToolsSource)
			delete (*j);
	}

	m_toolSets.clear();
	m_toolSources.clear();

	if(m_pGlobalTools)
	{
		if(bWantMenuResources)
			m_pGlobalTools->ReleaseMenuResources();
		delete m_pGlobalTools;
		m_pGlobalTools = NULL;
	}
}

SchemeTools* SchemeToolsManager::GetGlobalTools()
{
	if(!m_pGlobalTools)
		m_pGlobalTools = new GlobalTools;

	return m_pGlobalTools;
}

SchemeTools* SchemeToolsManager::GetToolsFor(LPCTSTR scheme)
{
	tstring stofind(scheme);
	SCHEMETOOLS_MAP::iterator i = m_toolSets.find(stofind);
	
	SchemeTools* pRet = NULL;

	if(i != m_toolSets.end())
	{
		pRet = (*i).second;
	}
	else
	{
		pRet = new SchemeTools(stofind.c_str());
		m_toolSets.insert(SCHEMETOOLS_MAP::value_type(stofind, pRet));
	}

	return pRet;
}

int SchemeToolsManager::GetMenuFor(LPCTSTR scheme, CSMenuHandle& menu, int iInsertAfter)
{
	SchemeTools* pTools = GetToolsFor(scheme);
	if(pTools)
	{
		return pTools->GetMenu(menu, iInsertAfter);
	}
	
	return iInsertAfter;
}

int SchemeToolsManager::UpdateToolsMenu(CSMenuHandle& tools, int iFirstToolCmd, int iDummyID, LPCSTR schemename)
{
	HMENU m = tools;
	
	//First we ensure there's a marker item...
	if(iFirstToolCmd != iDummyID)
	{
		bool bDeleting = false;
		int iCount = tools.GetCount();
		int id;
		for(int i = iCount - 1; i >= 0; i--)
		{
			id = ::GetMenuItemID(m, i);
			if(id == iFirstToolCmd)
			{
				::InsertMenu(m, i+1, MF_BYPOSITION | MF_STRING, iDummyID, _T("Add Tools..."));
				bDeleting = true;
			}

			if(bDeleting)
			{
				if(id == 0) // found separator
					bDeleting = false;
				else
					::RemoveMenu(m, id, MF_BYCOMMAND);
			}
		}
	}

	if(m_pGlobalTools)
		iFirstToolCmd = m_pGlobalTools->GetMenu(tools, iDummyID);
	else
		iFirstToolCmd = iDummyID;
	if(schemename)
	{
		int iNextFirst = GetMenuFor(schemename, tools, iDummyID);
		iFirstToolCmd = (iNextFirst != iDummyID ? iNextFirst : iFirstToolCmd);
	}
	
	if(iFirstToolCmd != iDummyID)
		::RemoveMenu(m, iDummyID, MF_BYCOMMAND);

	return iFirstToolCmd;
}

const ToolSource* SchemeToolsManager::GetDefaultToolStore()
{
	return &m_DefaultToolsSource;
}

void SchemeToolsManager::ReLoad(bool bWantMenuResources)
{
	Clear(bWantMenuResources);

	XMLParser parser;
	parser.SetParseState(this);

	/// Try and load the user tools file.
	tstring uspath;
	OPTIONS->GetPNPath(uspath, PNPATH_USERTOOLS);
	uspath += _T("UserTools.xml");

	m_DefaultToolsSource.FileName = uspath;
	m_toolSources.push_back(&m_DefaultToolsSource);
	m_pCurSource = &m_DefaultToolsSource;

	if(FileExists(uspath.c_str()))
	{
		try
		{
			parser.LoadFile(uspath.c_str());
		}
		catch ( XMLParserException& ex )
		{
			tstring str(_T("XML Parser Exception loading Scheme Tools:"));
			str += ex.GetMessage();
			UNEXPECTED(str.c_str());
		}

	}

	// Now we try and find any pre-shipped tool configurations.
	OPTIONS->GetPNPath(uspath, PNPATH_TOOLS);
	tstring pattern(uspath);
	tstring to_open;

	pattern += "*.xml";

	WIN32_FIND_DATA FindFileData;
	HANDLE hFind = ::FindFirstFile(pattern.c_str(), &FindFileData);
	if(hFind != INVALID_HANDLE_VALUE)
	{
		BOOL found = TRUE;
		while (found)
		{
			to_open = uspath;
			to_open += FindFileData.cFileName;

			try
			{
				ToolSource* ts = new ToolSource;
				ts->FileName = to_open;
				m_toolSources.push_back(ts);
				m_pCurSource = ts;

				parser.Reset();
				parser.LoadFile(to_open.c_str());
			}
			catch( XMLParserException& ex)
			{
				tstring str(_T("XML Parser Exception loading Scheme Tools:"));
				str += ex.GetMessage();
				UNEXPECTED(str.c_str());
			}

			found = ::FindNextFile(hFind, &FindFileData);
		}

		::FindClose(hFind);
	}
}

void SchemeToolsManager::Save()
{
	// Save all the source files.
	for(SOURCES_LIST::iterator i = m_toolSources.begin(); i != m_toolSources.end(); ++i)
	{
		ofstream str;
		str.open((*i)->FileName.c_str(), ios_base::out);
		if(str.is_open())
		{
			str << "<?xml version=\"1.0\"?>\n<schemetools>\n";
			if(m_pGlobalTools)
				m_pGlobalTools->WriteDefinition(str, (*i));
			for(SCHEMETOOLS_MAP::const_iterator j = m_toolSets.begin(); j != m_toolSets.end(); ++j)
			{
				(*j).second->WriteDefinition(str, (*i));
			}
			str << "</schemetools>";

			str.close();
		}
	}
}

void SchemeToolsManager::processScheme(XMLAttributes& atts)
{
	LPCTSTR schemename = atts.getValue(_T("name"));
	if(schemename)
	{
		// Only ever one SchemeTools object per named scheme, independent of source files.
		SchemeTools* old = GetToolsFor(schemename);
		if(old)
			m_pCur = old;
		else
		{
			m_pCur = new SchemeTools(schemename);

			tstring stoadd(schemename);
			m_toolSets.insert(SCHEMETOOLS_MAP::value_type(stoadd, m_pCur));
		}
	}
}

void SchemeToolsManager::processGlobal(XMLAttributes& atts)
{
	if(!m_pGlobalTools)
		m_pGlobalTools = new GlobalTools;
	m_pCur = m_pGlobalTools;
}

void SchemeToolsManager::processTool(XMLAttributes& atts)
{
	LPCTSTR toolname = atts.getValue(_T("name"));
	if(m_pCur && toolname)
	{
		SourcedToolDefinition* pDef = new SourcedToolDefinition(m_pCurSource);
		
		// Initialise members...
		pDef->Name = toolname;
		
		int c = atts.getCount();

		for(int i = 0; i < c; ++i)
		{
			LPCTSTR attr = atts.getName(i);
			LPCTSTR val = atts.getValue(i);
			
			if(_tcscmp(attr, _T("command")) == 0)
				pDef->Command = val;
			else if(_tcscmp(attr, _T("params")) == 0)
				pDef->Params = val;
			else if(_tcscmp(attr, _T("folder")) == 0)
				pDef->Folder = val;
			else if(_tcscmp(attr, _T("shortcut")) == 0)
				pDef->Shortcut = _ttoi(val);
			else if(_tcscmp(attr, _T("parsepattern")) == 0)
				pDef->CustomParsePattern = val;
			else if(_tcscmp(attr, _T("flags")) == 0)
				pDef->iFlags = _ttoi(val);
		}

		m_pCur->Add(pDef);
	}
}

void SchemeToolsManager::startElement(LPCTSTR name, XMLAttributes& atts)
{
	if(_tcscmp(name, _T("scheme")) == 0)
	{
		processScheme(atts);
	}
	else if(_tcscmp(name, _T("global")) == 0)
	{
		processGlobal(atts);
	}
	else if(_tcscmp(name, _T("tool")) == 0)
	{
		processTool(atts);
	}
}

void SchemeToolsManager::endElement(LPCTSTR name)
{
	if(_tcscmp(name, _T("scheme")) == 0 || _tcscmp(name, _T("global")) == 0)
		m_pCur = NULL;
}

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
	m_pWrapper->OnStart();
	m_RetCode = Run_Capture(m_pWrapper->Command.c_str(), m_pWrapper->Params.c_str(), m_pWrapper->Folder.c_str());
	PostRun();
	m_pWrapper->SetRunning(false);
	m_pWrapper->OnFinished();
	ToolOwner::GetInstance()->MarkToolForDeletion(this);
}

void ToolRunner::OnException()
{
	::OutputDebugString(_T("PN2: Exception whilst running a tool.\n"));
}

int ToolRunner::Run_Capture(LPCTSTR command, LPCTSTR params, LPCTSTR dir)
{
	tstring tempstr(_T("\"\" "));
	tempstr.insert(1, command);
	tempstr += params;

	TCHAR* commandBuf = new TCHAR[tempstr.size() + 1];
	_tcscpy(commandBuf, tempstr.c_str());

	tempstr.insert(0, _T("> "));
	tempstr += _T("\n");
	m_pWrapper->_AddToolOutput(tempstr.c_str());

	if(_tcslen(dir) == 0)
		dir = NULL;

	OSVERSIONINFO osv = {sizeof(OSVERSIONINFO), 0, 0, 0, 0, _T("")};

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
		CLastErrorInfo lei;
		m_pWrapper->_AddToolOutput("\n> Failed to create StdOut and StdErr Pipe: ");
		m_pWrapper->_AddToolOutput((LPCTSTR)lei);

		return lei.GetErrorCode();
	}

	// read handle, write handle, security attributes,  number of bytes reserved for pipe - 0 default
	
	if( ! ::CreatePipe(&hStdInRead, &hStdInWrite, &sa, 0) )
	{
		CLastErrorInfo lei;

		m_pWrapper->_AddToolOutput("\n> Failed to create StdIn Pipe: ");
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

	bool bCreated = ::CreateProcess(
		NULL, 
		commandBuf, 
		&sa, /*LPSECURITY_ATTRIBUTES lpProcessAttributes*/
		NULL, /*LPSECURITYATTRIBUTES lpThreadAttributes*/
		TRUE, /*BOOL bInheritHandles*/ 
		CREATE_NEW_PROCESS_GROUP, /*DWORD dwCreationFlags*/
		NULL, /*LPVOID lpEnvironment*/
		dir, /*LPCTSTR lpWorkingDir*/
		&si, /*LPSTARTUPINFO lpStartupInfo*/
		&pi /*LPPROCESS_INFORMATION lpProcessInformation*/ 
	) != 0;

	delete [] commandBuf;

	if(!bCreated)
	{
		::CloseHandle(hReadPipe);
		::CloseHandle(hWritePipe);
		::CloseHandle(hStdInRead);
		::CloseHandle(hStdInWrite);

		CLastErrorInfo lei;
		m_pWrapper->_AddToolOutput("\n> Failed to create process: ");
		m_pWrapper->_AddToolOutput((LPCTSTR)lei);

		return lei.GetErrorCode();
	}

	DWORD dwBytesAvail, dwBytesRead, exitCode, timeDeathDetected;
	dwBytesAvail = dwBytesRead = exitCode = timeDeathDetected = 0;
	bool bCompleted = false;
	char buffer[TOOLS_BUFFER_SIZE];

	while(!bCompleted)
	{
		Sleep(50);

		//The PeekNamedPipe function copies data from a named or 
		// anonymous pipe into a buffer without removing it from the pipe.
		if(! ::PeekNamedPipe(hReadPipe, NULL, 0, NULL, &dwBytesAvail, NULL) )
		{
			dwBytesAvail = 0;
		}

		if(dwBytesAvail > 0)
		{
			BOOL bRead = ::ReadFile(hReadPipe, buffer, sizeof(buffer), &dwBytesRead, NULL);

			if(bRead && dwBytesRead)
			{
				m_pWrapper->_AddToolOutput(buffer, dwBytesRead);
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
				}
			}
		}

		// While we're here, we check to see if we've been told to close.
		if(!GetCanRun())
		{
			if (WAIT_OBJECT_0 != ::WaitForSingleObject(pi.hProcess, 500)) 
			{
				// We should do this only if the GUI process is stuck and
				// don't answer to a normal termination command.
				// This function is dangerous: dependant DLLs don't know the process
				// is terminated, and memory isn't released.
				m_pWrapper->_AddToolOutput("\n> Forcefully terminating process...\n");
				::TerminateProcess(pi.hProcess, 1);
			}
			bCompleted = true;
		}
	} // while (!bCompleted)

	if (WAIT_OBJECT_0 != ::WaitForSingleObject(pi.hProcess, 1000)) 
	{
		m_pWrapper->_AddToolOutput("\n> Process failed to respond; forcing abrupt termination...");
		::TerminateProcess(pi.hProcess, 2);
	}

	::GetExitCodeProcess(pi.hProcess, &exitCode);

	m_RetCode = exitCode;

	::CloseHandle(pi.hProcess);
	::CloseHandle(pi.hThread);
	::CloseHandle(hReadPipe);
	::CloseHandle(hWritePipe);
	::CloseHandle(hStdInRead);
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

	TCHAR* commandBuf = new TCHAR[tempstr.size() + 1];
	_tcscpy(commandBuf, tempstr.c_str());

	if(_tcslen(dir) == 0)
		dir = NULL;

	OSVERSIONINFO osv = {sizeof(OSVERSIONINFO), 0, 0, 0, 0, _T("")};

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

	STARTUPINFO si;
	memset(&si, 0, sizeof(STARTUPINFO));
	si.cb = sizeof(STARTUPINFO);
	/*si.dwFlags = STARTF_USESHOWWINDOW; - don't want to hide these processes!
	si.wShowWindow = SW_HIDE;*/

	PROCESS_INFORMATION pi = {0, 0, 0, 0};

	bool bCreated = ::CreateProcess(
		NULL, 
		commandBuf, 
		&sa, /*LPSECURITY_ATTRIBUTES lpProcessAttributes*/
		NULL, /*LPSECURITYATTRIBUTES lpThreadAttributes*/
		TRUE, /*BOOL bInheritHandles*/ 
		CREATE_NEW_PROCESS_GROUP, /*DWORD dwCreationFlags*/
		NULL, /*LPVOID lpEnvironment*/
		dir, /*LPCTSTR lpWorkingDir*/
		&si, /*LPSTARTUPINFO lpStartupInfo*/
		&pi /*LPPROCESS_INFORMATION lpProcessInformation*/ 
	) != 0;

	// We're waiting for this one to finish because it's a filter process - 
	// i.e. it will change the current file.
	if( m_pWrapper->IsFilter() )
	{
		::WaitForSingleObject( pi.hProcess, INFINITE );
		DWORD dwExitCode;
		::GetExitCodeProcess(pi.hProcess, &dwExitCode);
		result = dwExitCode;
	}
	else
		result = pi.dwProcessId;
	
	// Detach...
	::CloseHandle(pi.hProcess);
	::CloseHandle(pi.hThread);

	PostRun();

	return result;
}

int ToolRunner::Execute()
{
	CToolCommandString builder;
	builder.reversePathSeps = m_pWrapper->ShouldUseForwardSlashes();
	builder.pChild = m_pWrapper->GetActiveChild();
	
	m_pWrapper->Command = builder.Build(m_pWrapper->Command.c_str());
	m_pWrapper->Params = builder.Build(m_pWrapper->Params.c_str());
	m_pWrapper->Folder = builder.Build(m_pWrapper->Folder.c_str());

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

	return 0;
}

void ToolRunner::PostRun()
{
	if( m_pWrapper->CaptureOutput() )
	{
		tstring exitcode(_T("\n> Process Exit Code: "));
		exitcode += IntToTString(GetExitCode());
		exitcode += _T("\n");

		m_pWrapper->_AddToolOutput(exitcode.c_str());
	}

	if( m_pWrapper->IsFilter() )
		m_pWrapper->Revert();
}

//////////////////////////////////////////////////////////////////////////////
// ToolOwner
//////////////////////////////////////////////////////////////////////////////

//ToolOwner* ToolOwner::s_pTheInstance = NULL;

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
void ToolOwner::RunTool(ToolWrapper* pTool, ToolOwnerID OwnerID)
{
	_ToolWrapper _wrapper = {0};
	_wrapper.OwnerID = OwnerID;
	_wrapper.pWrapper = pTool;
	_wrapper.pRunner = new ToolRunner( pTool );

	bool bThreaded = _wrapper.pRunner->GetThreadedExecution();
	
	if( bThreaded )
	{
		CSSCritLock lock(&m_crRunningTools);

		m_RunningTools.push_back(_wrapper);	 // Add this tool to our list to mind.
	}

	if( pTool->SaveAll() )
	{
		g_Context.m_frame->SaveAll();
	}
	else if( pTool->SaveOne() )
	{
		CChildFrame* pChild = pTool->GetActiveChild();
		if(pChild)
		{
			pChild->Save();
		}
	}

	_wrapper.pRunner->Execute();

	if( !bThreaded )
	{
		delete _wrapper.pRunner;
		delete _wrapper.pWrapper;
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

		//pT->UpdateRunningTools();
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
			delete tool.pWrapper;
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
			::OutputDebugString(_T("PN2: Gave up waiting for tools to finish."));
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