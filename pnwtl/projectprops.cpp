/**
 * @file projectprops.cpp
 * @brief Project Properties
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "project.h"
#include "projectprops.h"

namespace Projects
{

//////////////////////////////////////////////////////////////////////////////
// ProjectProp
//////////////////////////////////////////////////////////////////////////////

ProjectProp::ProjectProp(LPCTSTR name)
{
	m_name = name;
}

ProjectProp::ProjectProp(LPCTSTR name, PropType type)
{
	m_name = name;
	m_type = type;
}

LPCTSTR ProjectProp::GetName() const
{
	return m_name.c_str();
}

PropType ProjectProp::GetType() const
{
	return m_type;
}

//////////////////////////////////////////////////////////////////////////////
// PropCategory
//////////////////////////////////////////////////////////////////////////////

PropCategory::PropCategory(LPCTSTR name)
{
	m_name = name;
}

PropCategory::~PropCategory()
{
	clear();
}

LPCTSTR PropCategory::GetName() const
{
	return m_name.c_str();
}

void PropCategory::Add(ProjectProp* property)
{
	m_list.insert(m_list.end(), property);
}

void PropCategory::Remove(ProjectProp* property)
{
	m_list.remove(property);
}

PropList& PropCategory::GetProperties()
{
	return m_list;
}

void PropCategory::clear()
{
	for(PropList::const_iterator i = m_list.begin();
		i != m_list.end();
		++i)
	{
		delete (*i);
	}
	m_list.clear();
}

//////////////////////////////////////////////////////////////////////////////
// PropGroup
//////////////////////////////////////////////////////////////////////////////

PropGroup::PropGroup(LPCTSTR name)
{
	m_name = name;
}

PropGroup::~PropGroup()
{
	clear();
}

LPCTSTR PropGroup::GetName() const
{
	return m_name.c_str();
}

void PropGroup::clear()
{
	for(PropGroupList::const_iterator i = m_subgroups.begin();
		i != m_subgroups.end();
		++i)
	{
		delete (*i);
	}
	m_subgroups.clear();

	for(PropCatList::const_iterator j = m_cats.begin();
		j != m_cats.end();
		++j)
	{
		delete (*j);
	}
	m_cats.clear();
}

void PropGroup::Add(PropCategory* cat)
{
	m_cats.insert(m_cats.end(), cat);
}

void PropGroup::Remove(PropCategory* cat)
{
	m_cats.remove(cat);
}

void PropGroup::Add(PropGroup* subGroup)
{
	m_subgroups.insert(m_subgroups.end(), subGroup);
}

void PropGroup::Remove(PropGroup* subGroup)
{
	m_subgroups.remove(subGroup);
}

PropGroupList& PropGroup::GetSubGroups()
{
	return m_subgroups;
}

PropCatList& PropGroup::GetCategories()
{
	return m_cats;
}

//////////////////////////////////////////////////////////////////////////////
// PropSet
//////////////////////////////////////////////////////////////////////////////

PropSet::~PropSet()
{
	clear();
}

void PropSet::Add(PropGroup* group)
{
	m_groups.insert(m_groups.end(), group);
}

void PropSet::Remove(PropGroup* group)
{
	m_groups.remove(group);
}

void PropSet::SetCriteria(PROJECT_TYPE pType, LPCTSTR extmask)
{
	m_type = pType;
	if(extmask)
		m_mask = extmask;
	else
		m_mask = _T("");
}

bool PropSet::Matches(PROJECT_TYPE pType, LPCTSTR extmask) const
{
	if(m_type != pType)
		return false;

	//TODO match the extension if it's not NULL;
	if(extmask != NULL)
	{

	}

	return true;
}

PropGroupList& PropSet::GetGroups()
{
	return m_groups;
}

void PropSet::clear()
{
	for(PropGroupList::const_iterator i = m_groups.begin();
		i != m_groups.end();
		++i)
	{
		delete (*i);
	}
	m_groups.clear();
}

//////////////////////////////////////////////////////////////////////////////
// ProjectTemplate
//////////////////////////////////////////////////////////////////////////////

ProjectTemplate::ProjectTemplate(LPCTSTR name, LPCTSTR path)
{
	m_name = name;
}

ProjectTemplate::~ProjectTemplate()
{
	for(PropSetList::const_iterator i = m_propsets.begin(); i != m_propsets.end(); ++i)
	{
		delete (*i);
	}
	m_propsets.clear();
}

LPCTSTR ProjectTemplate::GetName() const
{
	return m_name.c_str();
}

PropSet* ProjectTemplate::GetProperties(PROJECT_TYPE type) const
{
	for(PropSetList::const_iterator i = m_propsets.begin(); i != m_propsets.end(); ++i)
	{
		if((*i)->Matches(type, NULL))
			return *i;
	}

	return NULL;
}

void ProjectTemplate::AddProperties(PROJECT_TYPE type, LPCTSTR extmask, PropGroupList& orphanProps)
{
	PropSet* pPropSet = new PropSet();
	pPropSet->SetCriteria(type, extmask);
	PropGroupList& props = pPropSet->GetGroups();
	
	for(PropGroupList::iterator i = orphanProps.begin();
		i != orphanProps.end();
		++i)
	{
		props.insert(props.end(), (*i));
	}

	orphanProps.clear();

	m_propsets.insert(m_propsets.end(), pPropSet);
}

} // namespace Projects