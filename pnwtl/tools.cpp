/**
 * @file tools.cpp
 * @brief External tools code
 * @author Simon Steele
 * @note Copyright (c) 2002-2007 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
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
#include "include/ssthreads.h"
#include "include/pcreplus.h"

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

SchemeTools::SchemeTools(LPCTSTR schemename) :
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

GlobalProjectTools::GlobalProjectTools() : ProjectTools("")
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
// ToolCommandString
//////////////////////////////////////////////////////////////////////////////

void ToolCommandString::OnFormatChar(TCHAR thechar)
{
	switch(thechar)
	{
		case _T('f'):
			if(pChild)
                m_string += pChild->GetFileName(FN_FILE);
			break;

		case _T('d'):
			{
				if(pChild)
				{
					CPathName pn(pChild->GetFileName(FN_PATH));
					if(reversePathSeps)
						pn.SetForwardSlashes();
					m_string += pn;
				}
			}
			break;

		case _T('n'):
			if(pChild)
				m_string += pChild->GetFileName(FN_FILEPART);
			break;

		case _T('l'):
			if(pChild)
			{
				_itot(pChild->GetPosition(EP_LINE), itosbuf, 10);
				m_string += itosbuf;
			}
			break;

		case _T('c'):
			if(pChild)
			{
				_itot(pChild->GetPosition(EP_COL), itosbuf, 10);
				m_string += itosbuf;
			}
			break;

		case _T('w'):
			if(pChild)
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

#define MATCH_START(s) \
	((_tcslen(key) > _tcslen(s)) && (_tcsnicmp(s, key, _tcslen(s)) == 0))

void ToolCommandString::OnFormatKey(LPCTSTR key)
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
	else if(MATCH_START(_T("ProjectProp:")))
	{
		Projects::Project* pP = GetActiveProject();
		if(!pP)
			return;

		Projects::ProjectTemplate* pTemplate = pP->GetTemplate();
		if(!pTemplate)
			return;

		PCRE::RegExp re("ProjectProp:(?P<group>[-_a-zA-Z0-9]+)\\.(?P<cat>[-_a-zA-Z0-9]+)\\.(?P<val>[-_a-zA-Z0-9]+)");
		if( re.Match(key) )
		{
			tstring group;
			tstring cat;
			tstring val;
        
			// Extract the named matches from the RE, noting if there was a line or column.
			bool ok = true;
			ok &= re.GetNamedMatch("group", group);
			ok &= re.GetNamedMatch("cat", cat);
			ok &= re.GetNamedMatch("val", val);

			if(!ok)
				return;
		
			LPCTSTR retval = pP->GetUserData().Lookup(pTemplate->GetNamespace(), group.c_str(), cat.c_str(), val.c_str(), _T(""));
			if(retval != NULL)
			{
				m_string += retval;
			}
		}
	}
	else if(MATCH_START(_T("FileProp:")))
	{
		Projects::Project* pP = GetActiveProject();
		if(!pP)
			return;

		Projects::ProjectTemplate* pTemplate = pP->GetTemplate();
		if(!pTemplate)
			return;

		if(!pChild)
			return;
		
		Projects::File* pFileObj = pP->FindFile( pChild->GetFileName().c_str() );
		if(!pFileObj)
			return;

		PCRE::RegExp re("ProjectProp:(?P<group>[-_a-zA-Z0-9]+)\\.(?P<cat>[-_a-zA-Z0-9]+)\\.(?P<val>[-_a-zA-Z0-9]+)");
		if( re.Match(key) )
		{
			tstring group;
			tstring cat;
			tstring val;
        
			// Extract the named matches from the RE, noting if there was a line or column.
			bool ok = true;
			ok &= re.GetNamedMatch("group", group);
			ok &= re.GetNamedMatch("cat", cat);
			ok &= re.GetNamedMatch("val", val);

			if(!ok)
				return;
		
			LPCTSTR retval = pFileObj->GetUserData().Lookup(pTemplate->GetNamespace(), group.c_str(), cat.c_str(), val.c_str(), _T(""));
			if(retval != NULL)
			{
				m_string += retval;
			}
		}
	}
	else if(MATCH(_T("ProjectName")))
	{
		Projects::Project *pP = GetActiveProject();
		if(pP)
		{
			m_string += pP->GetName();
		}
	}
	else if(MATCH(_T("ProjectGroupName")))
	{
		Projects::Workspace *pW = GetWorkspace();
		if(pW != NULL)
		{	
			m_string += pW->GetName();
		}
	}
	else if(MATCH(_T("PNPath")))
	{
		std::string pn;
		OPTIONS->GetPNPath(pn);
		m_string += pn;
	}
	else
	{
		tstring s = _T("Unknown constant: $(");
		s += key;
		s += ").";
		g_Context.m_frame->SetStatusText(s.c_str());
	}
}

/**
 * Support environment variables
 */
void ToolCommandString::OnFormatPercentKey(LPCTSTR key)
{
	LPTSTR value = new TCHAR[32767]; // Max size for an environment variable
	
	if (GetEnvironmentVariable(key, value, 32767) == 0)
	{
		value[0] = 0; // Make sure it's an empty string even on error
	}
	m_string += value;
	
	delete [] value;
}

#undef MATCH
#undef MATCH_START

Projects::Workspace* ToolCommandString::GetWorkspace()
{
	Projects::Workspace* pWorkspace = g_Context.m_frame->GetActiveWorkspace();
	return pWorkspace;
}

Projects::Project* ToolCommandString::GetActiveProject()
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
// ToolWrapper
//////////////////////////////////////////////////////////////////////////////

ToolWrapper::ToolWrapper(CChildFrame* pActiveChild, const ToolDefinition& definition) :
	m_pStdIOBuffer(NULL),
	m_StdIOBufferSize(NULL),
	m_hNotifyWnd(NULL),
	m_pActiveChild(pActiveChild)
{
	ToolDefinition::_copy(definition);

	::InitializeCriticalSection(&m_csStatusLock);
	SetRunning(true);
}

ToolWrapper::~ToolWrapper()
{
	if(m_pStdIOBuffer != NULL)
		delete [] m_pStdIOBuffer;
	::DeleteCriticalSection(&m_csStatusLock);
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
	CSSCritLock lock(&m_csStatusLock);
	m_bRunning = bRunning;
}

bool ToolWrapper::IsRunning()
{
	CSSCritLock lock(&m_csStatusLock);
	return m_bRunning;
}

/// Orphan a buffer of data off to this class
void ToolWrapper::SetStdIOBuffer(unsigned char* buffer, unsigned int size)
{
	m_pStdIOBuffer = buffer;
	m_StdIOBufferSize = size;
}

unsigned char* ToolWrapper::GetStdIOBuffer(unsigned int& size) const
{
	size = m_StdIOBufferSize;
	return m_pStdIOBuffer;
}