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
			stream << "\t\t<tool name=\"" << (*i)->Name << "\" ";
			stream << "command=\"" << (*i)->Command << "\" ";
			stream << "folder=\"" << (*i)->Folder << "\" ";
			stream << "params=\"" << (*i)->Params << "\" ";
			stream << "shortcut=\"" << (*i)->Shortcut << "\" ";
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