/**
 * @file tools.cpp
 * @brief External tools code
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"

#include "childfrm.h"
#include "pndialogs.h"  // for InputBox...
#include "project.h"
#include "projectmeta.h"
#include "projectprops.h"
#include "tools.h"
#include "toolsxmlwriter.h"

#include "include/sscontainers.h"
#include "include/threading.h"

#include <algorithm>

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

//////////////////////////////////////////////////////////////////////////////
// SourcedToolDefinition
//////////////////////////////////////////////////////////////////////////////

SourcedToolDefinition::SourcedToolDefinition(const ToolSource* source_) : ToolDefinition(), source(source_)
{
}

SourcedToolDefinition::SourcedToolDefinition(const SourcedToolDefinition& copy)
{
	source = copy.source;
	_copy(copy);
}

//////////////////////////////////////////////////////////////////////////////
// SchemeTools
//////////////////////////////////////////////////////////////////////////////

SchemeTools::SchemeTools() : 
	m_hAccel(NULL)
{
}

SchemeTools::SchemeTools(LPCSTR schemename) :
	m_Scheme(schemename),
	m_hAccel(NULL)
{
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

void SchemeTools::AllocateMenuResources(CommandDispatch* dispatcher, int iCommand)
{
	if(m_Tools.size() != 0)
	{
		for(TOOLDEFS_LIST::const_iterator i = m_Tools.begin(); i != m_Tools.end(); ++i)
		{
			ToolDefinition* pT = (*i);
			
			if(pT->CommandID == -1)
				pT->CommandID = dispatcher->RegisterCallback(pT->CommandID, NULL, iCommand, (LPVOID)pT);
		}
	}
}

/**
 * This function should be called before this class is deleted if the 
 * menu command IDs used by it are to be made available again.
 */
void SchemeTools::ReleaseMenuResources(CommandDispatch* dispatcher)
{
	if(m_Tools.size() != 0)
	{
		ToolDefinition* pT = NULL;

		for(TOOLDEFS_LIST::const_iterator i = m_Tools.begin(); i != m_Tools.end(); ++i)
		{
			pT = (*i);
			if(pT->CommandID != -1)
			{
				dispatcher->UnRegisterCallback(pT->CommandID);
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

	std::vector<ACCEL> accels(m_Tools.size());
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
		m_hAccel = ::CreateAcceleratorTable(&accels[0], accelCount);

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

void SchemeTools::WriteDefinition(ToolsXMLWriter& writer, ToolSource* source)
{
	if(ToolsInSource(source))
	{
		writer.beginScheme(m_Scheme.c_str());
		
		InternalWriteDefinition(writer, source);	

		writer.endScheme();
	}
}

void SchemeTools::InternalWriteDefinition(ToolsXMLWriter& writer, ToolSource* source)
{
	for(TOOLDEFS_LIST::const_iterator i = m_Tools.begin(); i != m_Tools.end(); ++i)
	{
		if( (*i)->source != source )
			continue;
		writer.writeTool((*i));
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

void GlobalTools::WriteDefinition(ToolsXMLWriter& writer, ToolSource* source)
{
	if(ToolsInSource(source))
	{
		writer.beginGlobal();
		InternalWriteDefinition(writer, source);
		writer.endGlobal();
	}
}

//////////////////////////////////////////////////////////////////////////////
// ProjectTools
//////////////////////////////////////////////////////////////////////////////

ProjectTools::ProjectTools(LPCTSTR id) :
	m_ProjectID(id)
{
}

void ProjectTools::Add(SourcedToolDefinition* pDef)
{
	pDef->SetFlags(pDef->GetFlags() | TOOL_ISPROJECTTOOL);
	SchemeTools::Add(pDef);
}

void ProjectTools::WriteDefinition(ToolsXMLWriter& writer, ToolSource* source)
{
	if(ToolsInSource(source))
	{
		writer.beginProject(m_ProjectID.c_str());
		InternalWriteDefinition(writer, source);
		writer.endProject();
	}
}

//////////////////////////////////////////////////////////////////////////////
// GlobalProjectTools
//////////////////////////////////////////////////////////////////////////////

GlobalProjectTools::GlobalProjectTools() : ProjectTools(_T(""))
{

}

void GlobalProjectTools::WriteDefinition(ToolsXMLWriter& writer, ToolSource* source)
{
	if(ToolsInSource(source))
	{
		writer.beginAllProjects();
		InternalWriteDefinition(writer, source);
		writer.endAllProjects();
	}
}

//////////////////////////////////////////////////////////////////////////////
// ToolWrapper
//////////////////////////////////////////////////////////////////////////////

ToolWrapper::ToolWrapper(CChildFrame* pActiveChild, const ToolDefinition& definition) :
	m_hNotifyWnd(NULL),
	m_pActiveChild(pActiveChild)
{
	ToolDefinition::_copy(definition);

	SetRunning(true);
}

ToolWrapper::~ToolWrapper()
{
}

CChildFrame* ToolWrapper::GetActiveChild()
{
	return m_pActiveChild;
}

void ToolWrapper::SetNotifyWindow(HWND hWnd)
{
	m_hNotifyWnd = hWnd;
}

void ToolWrapper::OnStart()
{
	if(m_hNotifyWnd)
	{
		::PostMessage(m_hNotifyWnd, PN_TOOLRUNUPDATE, 0, 0);
	}
}

void ToolWrapper::OnFinished()
{
	if(m_hNotifyWnd)
	{
		::PostMessage(m_hNotifyWnd, PN_TOOLRUNUPDATE, 0, 0);
	}
}

void ToolWrapper::SetRunning(bool bRunning)
{
	pnutils::threading::CritLock lock(m_csStatusLock);
	m_bRunning = bRunning;
}

bool ToolWrapper::IsRunning()
{
	pnutils::threading::CritLock lock(m_csStatusLock);
	return m_bRunning;
}

/// Orphan a buffer of data off to this class
void ToolWrapper::SwapInStdInBuffer(std::vector<unsigned char>& buffer)
{
	std::swap(m_stdin, buffer);
}

unsigned char* ToolWrapper::GetStdIOBuffer(unsigned int& size)
{
	size = m_stdin.size();
	if (!size)
	{
		return 0;
	}
	
	return &m_stdin[0];
}