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
#include "tools.h"

#include <sstream>
#include <fstream>

//////////////////////////////////////////////////////////////////////////////
// SchemeTools
//////////////////////////////////////////////////////////////////////////////

SchemeTools::SchemeTools(LPCTSTR schemename)
{
	m_Scheme = schemename;
}

SchemeTools::~SchemeTools()
{
	for(TOOLDEFS_LIST::iterator i = m_Tools.begin(); i != m_Tools.end(); ++i)
	{
		delete *i;
	}

	m_Tools.clear();
}

TOOLDEFS_LIST& SchemeTools::GetTools()
{
	return m_Tools;
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
			SToolDefinition* pT = (*i);
			
            if(pT->CommandID == -1)
				pT->CommandID = pMan->RegisterCallback(pT->CommandID, NULL, iCommand, (LPVOID)pT);

			::InsertMenu(menu, iInsertBefore, MF_BYCOMMAND | MF_STRING, pT->CommandID, pT->Name.c_str());
			iLastCommand = pT->CommandID;
		}
	}

	return iLastCommand;
}

/**
 * This function should be called before this class is deleted if the 
 * menu command IDs used by it are to be made available again.
 */
void SchemeTools::ReleaseMenuResources()
{
	if(m_Tools.size() != 0)
	{
		SToolDefinition* pT = NULL;
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

void SchemeTools::Add(SToolDefinition* pDef)
{
	m_Tools.push_back(pDef);
	pDef->CommandID = -1;
}

void SchemeTools::Delete(SToolDefinition* pDef)
{
	m_Tools.remove(pDef);
	delete pDef;
}

void SchemeTools::WriteDefinition(ofstream& stream)
{
	if(m_Tools.size() != 0)
	{
		stream << "\t<scheme name=\"" << m_Scheme << "\">\n";
		
		for(TOOLDEFS_LIST::const_iterator i = m_Tools.begin(); i != m_Tools.end(); ++i)
		{
			stream << "\t\t<tool name=\"" << FormatXML((*i)->Name) << "\" ";
			stream << "command=\"" << FormatXML((*i)->Command) << "\" ";
			stream << "folder=\"" << FormatXML((*i)->Folder) << "\" ";
			stream << "params=\"" << FormatXML((*i)->Params) << "\" ";
			stream << "shortcut=\"" << FormatXML((*i)->Shortcut) << "\" ";
			stream << "/>\n";
		}

		stream << "\t</scheme>\n";
	}
}

//////////////////////////////////////////////////////////////////////////////
// SchemeToolsManager
//////////////////////////////////////////////////////////////////////////////

SchemeToolsManager* SchemeToolsManager::s_pTheInstance = NULL;

SchemeToolsManager::SchemeToolsManager()
{
	m_pCur = NULL;

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

	m_toolSets.clear();
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

void SchemeToolsManager::ReLoad(bool bWantMenuResources)
{
	Clear(bWantMenuResources);

	XMLParser parser;
	parser.SetParseState(this);

	tstring uspath;
	COptionsManager::GetInstance()->GetPNPath(uspath, PNPATH_USERSETTINGS);
	uspath += _T("UserTools.xml");

	if(FileExists(uspath.c_str()))
	{
		try
		{
			parser.LoadFile(uspath.c_str());
		}
		catch ( XMLParserException& ex )
		{
			::OutputDebugString(_T("XML Parser Exception loading Scheme Tools:"));
			::OutputDebugString(ex.GetMessage());
		}

	}
}

void SchemeToolsManager::Save()
{
	tstring uspath;
	COptionsManager::GetInstance()->GetPNPath(uspath, PNPATH_USERSETTINGS);
	uspath += _T("UserTools.xml");

	ofstream str;
	str.open(uspath.c_str(), ios_base::out);
	if(str.is_open())
	{
		str << "<?xml version=\"1.0\"?>\n<schemetools>";
		for(SCHEMETOOLS_MAP::const_iterator i = m_toolSets.begin(); i != m_toolSets.end(); ++i)
		{
			(*i).second->WriteDefinition(str);
		}
		str << "</schemetools>";

		str.close();
	}
}

void SchemeToolsManager::processScheme(XMLAttributes& atts)
{
	LPCTSTR schemename = atts.getValue(_T("name"));
	if(schemename)
	{
		m_pCur = new SchemeTools(schemename);

		tstring stoadd(schemename);
		m_toolSets.insert(SCHEMETOOLS_MAP::value_type(stoadd, m_pCur));
	}
}

void SchemeToolsManager::processTool(XMLAttributes& atts)
{
	LPCTSTR toolname = atts.getValue(_T("name"));
	if(m_pCur && toolname)
	{
		SToolDefinition* pDef = new SToolDefinition;
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
				pDef->Shortcut = val;
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
	else if(_tcscmp(name, _T("tool")) == 0)
	{
		processTool(atts);
	}
}

void SchemeToolsManager::endElement(LPCTSTR name)
{
	if(_tcscmp(name, _T("scheme")) == 0)
		m_pCur = NULL;
}

ToolRunner::ToolRunner(CChildFrame* pActiveChild, SToolDefinition* pDef)
{
	m_pTool = pDef;
	m_pChild = pActiveChild;
}

struct ShellErr 
{
	DWORD code;
	LPCTSTR description;
};

int ToolRunner::Run_ShellExecute(LPCTSTR command, LPCTSTR params, LPCTSTR dir)
{
	UINT result = reinterpret_cast<UINT>(::ShellExecute(NULL, _T("open"), command, params, dir, SW_SHOW));

	if(! (result > 32)) // ShellExecute failed...
	{
		// This section shamelessly copied from SciTE
		const int numErrcodes = 15;
		static const ShellErr field[numErrcodes] = 
		{
			{ 0, _T("The operating system is out of memory or resources.") },
			{ ERROR_FILE_NOT_FOUND, _T("The specified file was not found.") },
			{ ERROR_PATH_NOT_FOUND, _T("The specified path was not found.") },
			{ ERROR_BAD_FORMAT, _T("The .exe file is invalid (non-Win32\256 .exe or error in .exe image).") },
			{ SE_ERR_ACCESSDENIED, _T("The operating system denied access to the specified file.") },
			{ SE_ERR_ASSOCINCOMPLETE, _T("The file name association is incomplete or invalid.") },
			{ SE_ERR_DDEBUSY, _T("The DDE transaction could not be completed because other DDE transactions were being processed.") },
			{ SE_ERR_DDEFAIL, _T("The DDE transaction failed.") },
			{ SE_ERR_DDETIMEOUT, _T("The DDE transaction could not be completed because the request timed out.") },
			{ SE_ERR_DLLNOTFOUND, _T("The specified dynamic-link library was not found.") },
			{ SE_ERR_FNF, _T("The specified file was not found.") },
			{ SE_ERR_NOASSOC, _T("There is no application associated with the given file name extension.") },
			{ SE_ERR_OOM, _T("There was not enough memory to complete the operation.") },
			{ SE_ERR_PNF, _T("The specified path was not found.") },
			{ SE_ERR_SHARE, _T("A sharing violation occurred.") },
		};

		for (int i = 0; i < numErrcodes; ++i) 
		{
			if (field[i].code == result)
				break;
		}

		basic_ostringstream<TCHAR> errmsg;

		errmsg << _T("Could not run tool: \nCommand: ") << command << 
			_T("\n\nMessage: ");

		if(i < numErrcodes)
		{
			errmsg << field[i].description;
		}
		else
		{
			errmsg << _T("Unknown error code (") << result << _T(").");
		}

		::MessageBox(m_pChild->m_hWnd, errmsg.str().c_str(), _T("Programmers Notepad"), MB_ICONWARNING | MB_OK);
	}

	return 0;
}

int ToolRunner::Execute()
{
	CToolCommandString builder;
	builder.pChild = m_pChild;
	
	tstring command;
	command = builder.Build(m_pTool->Command.c_str());
    
	tstring params;
	params = builder.Build(m_pTool->Params.c_str());

	tstring workingdir;
	workingdir = builder.Build(m_pTool->Folder.c_str());

	#if 0
	::CreateProcess(command.c_str(), params.c_str(), 
		lpProcessAttributes, /*LPSECURITY_ATTRIBUTES lpProcessAttributes*/
		lpThreadAttributes, /*LPSECURITYATTRIBUTES lpThreadAttributes*/
		bInheritHandles, /*BOOL bInheritHandles*/ 
		dwCreationFlags, /*DWORD dwCreationFlags*/
		lpEnvironment, /*LPVOID lpEnvironment*/
		workingdir.c_str(), /*LPCTSTR lpWorkingDir*/
		lpStartupInfo, /*LPSTARTUPINFO lpStartupInfo*/
		lpProcessInformation /*LPPROCESS_INFORMATION lpProcessInformation*/ 
	);
	#endif

	Run_ShellExecute(command.c_str(), params.c_str(), workingdir.c_str());

	return 0;
}