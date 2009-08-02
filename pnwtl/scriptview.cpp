/**
 * @file scriptview.cpp
 * @brief Scripts Docker
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "document.h"
#include "scriptregistry.h"
#include "scriptview.h"

CScriptDocker::CScriptDocker()
{
}

LRESULT CScriptDocker::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	HICON hIconSmall = (HICON)::LoadImage(_Module.GetResourceInstance(), MAKEINTRESOURCE(IDI_SCRIPTS), 
			IMAGE_ICON, ::GetSystemMetrics(SM_CXSMICON), ::GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);
	SetIcon(hIconSmall, FALSE);

	CRect rc;
	GetClientRect(&rc);

	m_view.Create(m_hWnd, rc, _T("ScriptsList"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | TVS_DISABLEDRAGDROP | TVS_HASLINES | TVS_LINESATROOT, 0, IDC_SCRIPTSLIST);
	m_view.ShowWindow(SW_SHOW);
	m_view.SetWindowTheme(L"explorer", NULL);

	buildInitial();

	ScriptRegistry::GetInstanceRef().SetEventSink(this);

	return 0;
}

LRESULT CScriptDocker::OnSize(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	if(wParam != SIZE_MINIMIZED )
	{
		RECT rc;
		GetClientRect(&rc);
		
		m_view.SetWindowPos(NULL, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top, SWP_NOZORDER | SWP_NOACTIVATE);
	}

	bHandled = FALSE;

	return 0;
}

/**
 * For some reason the WM_CTLCOLOR* messages do not get to the child
 * controls with the docking windows (todo with reflection). This returns
 * the proper result.
 */
LRESULT CScriptDocker::OnCtlColor(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	return (LRESULT)::GetSysColorBrush( COLOR_WINDOW );
}

LRESULT CScriptDocker::OnTreeDblClick(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	HTREEITEM hSel = m_view.GetSelectedItem();
	
	// Check if it's a group...
	if(m_view.GetParentItem(hSel) == NULL)
		return 0;

	Script* script = reinterpret_cast<Script*>( m_view.GetItemData(hSel) );
	if(script)
	{
		script->Run();
	}

	return 0;
}

void CScriptDocker::OnScriptAdded(ScriptGroup* group, Script* script)
{
	CA2CT groupName(group->GetName());
	HTREEITEM ti_group = findGroup(groupName);
	if(!ti_group)
		ti_group = addGroup(groupName);

	addScript(ti_group, script);
}

void CScriptDocker::OnScriptRemoved(ScriptGroup* group, Script* script)
{
	CA2CT scriptName(script->Name.c_str());
	CA2CT groupName(group->GetName());
	HTREEITEM tiScript = findScript(groupName, scriptName);
	if(tiScript)
		m_view.DeleteItem(tiScript);
}

HTREEITEM CScriptDocker::addScript(HTREEITEM group, Script* script)
{
	CA2CT scriptName(script->Name.c_str());
	HTREEITEM item = m_view.InsertItem(scriptName, group, NULL);
	m_view.SetItemData(item, reinterpret_cast<DWORD_PTR>(script));
	m_view.Expand(group);

	return item;
}

HTREEITEM CScriptDocker::findScript(LPCTSTR group, LPCTSTR name)
{
	HTREEITEM tiGroup = findGroup(group);
	if(!tiGroup)
		return NULL;

	HTREEITEM snode = m_view.GetChildItem(tiGroup);
	while(snode)
	{
		CString csName;
		m_view.GetItemText(snode, csName);
		if(csName.Compare(name) == 0)
			return snode;
		snode = m_view.GetNextSiblingItem(snode);
	}
	
	return NULL;
}

HTREEITEM CScriptDocker::findGroup(LPCTSTR name)
{
	HTREEITEM node = m_view.GetRootItem();
	while(node)
	{
		CString csName;
		m_view.GetItemText(node, csName);
		if(csName.CompareNoCase(name) == 0)
			return node;
		node = m_view.GetNextSiblingItem(node);
	}

	return NULL;
}

HTREEITEM CScriptDocker::addGroup(LPCTSTR name)
{
	HTREEITEM group = m_view.InsertItem(name, NULL, NULL);
	return group;
}

void CScriptDocker::buildInitial()
{
	ScriptRegistry* r = ScriptRegistry::GetInstance();
	
	for(group_list_t::const_iterator i = r->GetGroups().begin(); i != r->GetGroups().end(); ++i)
	{
		//tstring gname = (*i)->GetName();
		CA2CT gname((*i)->GetName());
		HTREEITEM group = addGroup( gname );
		for(script_list_t::const_iterator j = (*i)->GetScripts().begin(); j != (*i)->GetScripts().end(); ++j)
		{
			addScript( group, (*j) );
		}
	}
}