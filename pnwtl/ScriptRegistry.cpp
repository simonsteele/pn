#include "stdafx.h"
#include "scriptregistry.h"

ScriptRegistry::ScriptRegistry()
{
	m_sink = NULL;
}

void ScriptRegistry::Add(LPCTSTR group, LPCTSTR name, LPCTSTR scriptref)
{
	ScriptGroup* pGroup(NULL);

	for(group_list_t::iterator i = m_groups.begin(); i != m_groups.end(); ++i)
	{
		if(_tcsicmp((*i)->GetName(), group) == 0)
		{
			pGroup = (*i);
			break;
		}
	}

	if(!pGroup)
	{
		pGroup = new ScriptGroup(group);
		m_groups.push_back(pGroup);
	}

	pGroup->Add(name, scriptref);
}

void ScriptRegistry::Clear()
{
	clear();
}

const group_list_t& ScriptRegistry::GetGroups()
{
	return m_groups;
}

void ScriptRegistry::RegisterRunner(LPCTSTR id, extensions::IScriptRunner* runner)
{
	m_runners.insert(s_runner_map::value_type(tstring(id), runner));
}

void ScriptRegistry::RemoveRunner(LPCTSTR id)
{
	s_runner_map::iterator i = m_runners.find(tstring(id));
	if(i != m_runners.end())
		m_runners.erase(i);
}

extensions::IScriptRunner* ScriptRegistry::GetRunner(LPCTSTR id)
{
	s_runner_map::const_iterator i = m_runners.find(tstring(id));
	if(i != m_runners.end())
	{
		return (*i).second;
	}
	else
		return NULL;
}

void ScriptRegistry::clear()
{
	for(group_list_t::iterator i = m_groups.begin(); i != m_groups.end(); ++i)
	{
		delete (*i);
	}
	m_groups.clear();
}

//////////////////////////////////////////////////////////////////////////
// ScriptGroup
//////////////////////////////////////////////////////////////////////////

ScriptGroup::ScriptGroup(LPCTSTR name)
{
	m_name = name;
}

Script* ScriptGroup::Add(LPCTSTR name, LPCTSTR scriptref)
{
	Script* pScript = new Script(name, scriptref);
	m_scripts.push_back(pScript);
	return pScript;
}

void ScriptGroup::Clear()
{
	clear();
}

const script_list_t& ScriptGroup::GetScripts()
{
	return m_scripts;
}

LPCTSTR ScriptGroup::GetName() const
{
	return m_name.c_str();
}

void ScriptGroup::clear()
{
	for(script_list_t::iterator i = m_scripts.begin(); i != m_scripts.end(); ++i)
	{
		delete (*i);
	}
	m_scripts.clear();
}

//////////////////////////////////////////////////////////////////////////
// Script
//////////////////////////////////////////////////////////////////////////

void Script::Run()
{
	tstring str(ScriptRef);
	size_t rindex = str.find(':');
	if(rindex == -1)
		return;

	tstring runner_id = str.substr(0, rindex-1);
	extensions::IScriptRunner* runner = ScriptRegistry::GetInstanceRef().GetRunner(runner_id.c_str());

	tstring script = str.substr(rindex+1);
	runner->RunScript(script.c_str());
}