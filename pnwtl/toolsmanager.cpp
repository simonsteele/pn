/**
 * @file toolsmanager.cpp
 * @brief Manage External Tools
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "tools.h"
#include "toolsmanager.h"
#include "toolsxmlwriter.h"
#include "include/filefinder.h"

//////////////////////////////////////////////////////////////////////////////
// ToolsManager
//////////////////////////////////////////////////////////////////////////////

ToolsManager::ToolsManager()
{
	m_pCur = NULL;
	m_pGlobalTools = NULL;
	m_pGlobalProjectTools = NULL;

	ReLoad();
}

ToolsManager::~ToolsManager()
{
	Clear();
}

void ToolsManager::Clear(CommandDispatch* pDispatch)
{
	for(SCHEMETOOLS_MAP::iterator i = m_toolSets.begin(); i != m_toolSets.end(); ++i)
	{
		if(pDispatch)
			(*i).second->ReleaseMenuResources(pDispatch);
		delete (*i).second;
	}

	for(SCHEMETOOLS_MAP::iterator k = m_projectTools.begin(); k != m_projectTools.end(); ++k)
	{
		if(pDispatch)
			(*k).second->ReleaseMenuResources(pDispatch);
		delete (*k).second;
	}

	for(SOURCES_LIST::iterator j = m_toolSources.begin(); j != m_toolSources.end(); ++j)
	{
		if((*j) != &m_DefaultToolsSource)
			delete (*j);
	}

	m_toolSets.clear();
	m_projectTools.clear();
	m_toolSources.clear();

	if(m_pGlobalTools)
	{
		if(pDispatch)
			m_pGlobalTools->ReleaseMenuResources(pDispatch);
		delete m_pGlobalTools;
		m_pGlobalTools = NULL;
	}

	if(m_pGlobalProjectTools)
	{
		if(pDispatch)
			m_pGlobalProjectTools->ReleaseMenuResources(pDispatch);
		delete m_pGlobalProjectTools;
		m_pGlobalProjectTools = NULL;
	}
}

SchemeTools* ToolsManager::GetGlobalTools()
{
	if(!m_pGlobalTools)
		m_pGlobalTools = new GlobalTools;

	return m_pGlobalTools;
}

SchemeTools* ToolsManager::GetGlobalProjectTools()
{
	if(!m_pGlobalProjectTools)
		m_pGlobalProjectTools = new GlobalProjectTools();
	
	return m_pGlobalProjectTools;
}

SchemeTools* ToolsManager::GetToolsFor(LPCSTR scheme)
{
	SchemeTools* tools = find(scheme, m_toolSets);
	if(!tools)
	{
		std::string sid(scheme);
		tools = new SchemeTools(scheme);
		m_toolSets.insert(SCHEMETOOLS_MAP::value_type(sid, tools));
	}

	return tools;
}

ProjectTools* ToolsManager::GetToolsForProject(LPCTSTR id)
{
	CT2CA idconv(id);
	ProjectTools* tools = static_cast<ProjectTools*>( find(idconv, m_projectTools) );
	if(!tools)
	{
		tstring sid(id);
		tools = new ProjectTools(id);
		m_projectTools.insert(SCHEMETOOLS_MAP::value_type(std::string(idconv), tools));
	}

	return tools;
}

SchemeTools* ToolsManager::find(LPCSTR id, SCHEMETOOLS_MAP& col)
{
	std::string stofind(id);
	SCHEMETOOLS_MAP::iterator i = col.find(id);
	
	SchemeTools* pRet = NULL;

	if(i != col.end())
	{
		pRet = (*i).second;
	}

	return pRet;
}

int ToolsManager::UpdateToolsMenu(CSMenuHandle& tools, CommandDispatch* dispatcher, int iFirstToolCmd, int iDummyID, LPCSTR schemename, LPCTSTR projectId)
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

	TOOLDEFS_LIST theTools;
	
	if(m_pGlobalTools)
	{
		TOOLDEFS_LIST& gt = m_pGlobalTools->GetTools();
		theTools.insert(theTools.end(), gt.begin(), gt.end());
	}

	if(schemename)
	{
		SchemeTools* pS = GetToolsFor(schemename);
		theTools.insert(theTools.end(), pS->GetTools().begin(), pS->GetTools().end());
	}

	if(m_pGlobalProjectTools)
	{
		if(g_Context.m_frame->GetActiveWorkspace() != NULL)
		{
			TOOLDEFS_LIST& gt = m_pGlobalProjectTools->GetTools();
			theTools.insert(theTools.end(), gt.begin(), gt.end());
		}
	}

	if(projectId)
	{
		ProjectTools* pS = GetToolsForProject(projectId);
		theTools.insert(theTools.end(), pS->GetTools().begin(), pS->GetTools().end());
	}

	if(theTools.size() > 0)
	{
		iFirstToolCmd = BuildMenu(theTools, dispatcher, tools, iDummyID);
	}
	else
		iFirstToolCmd = iDummyID;
	
	if(iFirstToolCmd != iDummyID)
		::RemoveMenu(m, iDummyID, MF_BYCOMMAND);

	return iFirstToolCmd;
}

/**
 * @return ID of last command added...
 */
int ToolsManager::BuildMenu(TOOLDEFS_LIST& tools, CommandDispatch* dispatcher, CSMenuHandle& menu, int iInsertBefore, int iCommand)
{
	int iLastCommand = iInsertBefore;
	if(tools.size() != 0)
	{		
		for(TOOLDEFS_LIST::const_iterator i = tools.begin(); i != tools.end(); ++i)
		{
			ToolDefinition* pT = (*i);
			
            if(pT->CommandID == -1)
				pT->CommandID = dispatcher->RegisterCallback(pT->CommandID, NULL, iCommand, (LPVOID)pT);

			tstring str = pT->Name;
			if(pT->Shortcut != 0)
			{
				tstring sc = dispatcher->GetShortcutText(LOBYTE(pT->Shortcut), HIBYTE(pT->Shortcut));
				if(sc.length() > 0)
				{
					str += _T("\t");
					str += sc;
				}
			}

			::InsertMenu(menu, iInsertBefore, MF_BYCOMMAND | MF_STRING, pT->CommandID, str.c_str());
			iLastCommand = pT->CommandID;
		}
	}

	return iLastCommand;	
}

const ToolSource* ToolsManager::GetDefaultToolStore()
{
	return &m_DefaultToolsSource;
}

void ToolsManager::ReLoad(CommandDispatch* pDispatch)
{
	Clear(pDispatch);

	// Try and load the user tools file.
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
			XMLParser parser;
			parser.SetParseState(this);
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

	FileFinder<ToolsManager> finder(this, &ToolsManager::toolsFileFound);
	finder.Find(uspath.c_str(), _T("*.xml"), false);
}

void ToolsManager::Save()
{
	ToolsXMLWriter writer;

	// Save all the source files.
	for(SOURCES_LIST::iterator i = m_toolSources.begin(); i != m_toolSources.end(); ++i)
	{
		writer.Start((*i)->FileName.c_str());

		if(writer.IsValid())
		{
			writer.beginSchemeTools();

			if(m_pGlobalTools)
				m_pGlobalTools->WriteDefinition(writer, (*i));

			if(m_pGlobalProjectTools)
				m_pGlobalProjectTools->WriteDefinition(writer, (*i));

			for(SCHEMETOOLS_MAP::const_iterator j = m_toolSets.begin(); j != m_toolSets.end(); ++j)
			{
				(*j).second->WriteDefinition(writer, (*i));
			}

			for(SCHEMETOOLS_MAP::const_iterator k = m_projectTools.begin(); k != m_projectTools.end(); ++k)
			{
				(*k).second->WriteDefinition(writer, (*i));
			}
			
			writer.endSchemeTools();
			writer.Close();
		}		
	}
}

void ToolsManager::toolsFileFound(LPCTSTR path, FileFinderData& details, bool& shouldContinue)
{
	CFileName fn(details.GetFilename());
	fn.Root(path);

	try
	{
		ToolSource* ts = new ToolSource;
		ts->FileName = fn.c_str();
		m_toolSources.push_back(ts);
		m_pCurSource = ts;

		XMLParser parser;
		parser.SetParseState(this);
		parser.LoadFile(fn.c_str());
	}
	catch( XMLParserException& ex)
	{
		tstring str(_T("XML Parser Exception loading Scheme Tools:"));
		str += ex.GetMessage();
		UNEXPECTED(str.c_str());
	}
}

void ToolsManager::processScheme(const XMLAttributes& atts)
{
	LPCTSTR schemename = atts.getValue(_T("name"));
	if(schemename)
	{
		CT2CA scheme(schemename);
		// Only ever one SchemeTools object per named scheme, independent of source files.
		SchemeTools* old = GetToolsFor(scheme);
		PNASSERT(old != NULL);
		m_pCur = old;
	}
}

void ToolsManager::processProject(const XMLAttributes& atts)
{
	LPCTSTR projectid = atts.getValue(_T("projectid"));
	if(projectid)
	{
		// Only ever one SchemeTools object per project type id, independent of source files.
		ProjectTools* old = GetToolsForProject(projectid);
		PNASSERT(old != NULL);
		m_pCur = old;
	}
}

void ToolsManager::processGlobal(const XMLAttributes& atts)
{
	if(!m_pGlobalTools)
		m_pGlobalTools = new GlobalTools;
	m_pCur = m_pGlobalTools;
}

void ToolsManager::processAllProjects(const XMLAttributes& atts)
{
	if(!m_pGlobalProjectTools)
		m_pGlobalProjectTools = new GlobalProjectTools;
	m_pCur = m_pGlobalProjectTools;
}

void ToolsManager::processTool(const XMLAttributes& atts)
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
				pDef->Params = Xml_Tcs(val);
			else if(_tcscmp(attr, _T("folder")) == 0)
				pDef->Folder = Xml_Tcs(val);
			else if(_tcscmp(attr, _T("shortcut")) == 0)
				pDef->Shortcut = _ttoi(val);
			else if(_tcscmp(attr, _T("parsepattern")) == 0)
				pDef->CustomParsePattern = Xml_Windows1252(val);
			else if(_tcscmp(attr, _T("flags")) == 0)
			{
				LPTSTR end;
				pDef->SetFlags( _tcstoul(val, &end, 10) );
			}
			else if(_tcscmp(attr, _T("index")) == 0)
				pDef->Index = _ttoi(val);
		}

		m_pCur->Add(pDef);
	}
}

void ToolsManager::startElement(LPCTSTR name, const XMLAttributes& atts)
{
	if(_tcscmp(name, _T("scheme")) == 0)
	{
		processScheme(atts);
	}
	else if(_tcscmp(name, _T("global")) == 0)
	{
		processGlobal(atts);
	}
	else if(_tcscmp(name, _T("project")) == 0)
	{
		processProject(atts);
	}
	else if(_tcscmp(name, _T("tool")) == 0)
	{
		processTool(atts);
	}
	else if(_tcscmp(name, _T("allProjects")) == 0)
	{
		processAllProjects(atts);
	}
}

bool CompareToolDefinitions(ToolDefinition* t1, ToolDefinition* t2)
{
	return (t1->Index < t2->Index);
}

void ToolsManager::endElement(LPCTSTR name)
{
	if(_tcscmp(name, _T("scheme")) == 0 || _tcscmp(name, _T("global")) == 0 || _tcscmp(name, _T("project")) == 0 || _tcscmp(name, _T("allProjects")) == 0)
	{
		if(m_pCur != NULL)
			m_pCur->GetTools().sort(CompareToolDefinitions);
		m_pCur = NULL;
	}
}