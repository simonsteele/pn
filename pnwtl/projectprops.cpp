/**
 * @file projectprops.cpp
 * @brief Project Properties
 * @author Simon Steele
 * @note Copyright (c) 2004-2005 Simon Steele - http://untidy.net/
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

ProjectProp::ProjectProp(LPCTSTR name, LPCTSTR description)
{
	m_name = name;
	m_description = description;
	m_helpid = 0;
}

ProjectProp::ProjectProp(LPCTSTR name, LPCTSTR description, PropType type)
{
	m_name = name;
	m_description = description;
	m_type = type;
	m_helpid = 0;
}

ProjectProp::ProjectProp(LPCTSTR name, LPCTSTR description, LPCTSTR defval, int helpid, PropType type)
{
	m_name = name;
	m_description = description;
	m_type = type;
	m_default = defval;
	m_helpid = helpid;
}

LPCTSTR ProjectProp::GetName() const
{
	return m_name.c_str();
}

LPCTSTR ProjectProp::GetDescription() const
{
	return m_description.c_str();
}

LPCTSTR ProjectProp::GetDefault() const
{
	return m_default.c_str();
}

int ProjectProp::GetDefaultAsInt() const
{
	if(m_default.size() > 0)
		return _ttoi(m_default.c_str());
	else
		return 0;
}

bool ProjectProp::GetDefaultAsBool() const
{
	if(m_default.size() >= 4)
	{
		if(m_default[0] == _T('t') || m_default[0] == _T('T')) // dirty
		{
			return true;
		}
	}
	
	return false;
}

PropType ProjectProp::GetType() const
{
	return m_type;
}

int ProjectProp::GetHelpId() const
{
	return m_helpid;
}

//////////////////////////////////////////////////////////////////////////////
// ListProp
//////////////////////////////////////////////////////////////////////////////

ListProp::ListProp(LPCTSTR name, LPCTSTR description, LPCTSTR defval, int helpid) : ProjectProp(name, description, defval, helpid, propChoice)
{
}

ListProp::~ListProp()
{
	clear();
}

void ListProp::Add(Choice* value)
{
	m_values.insert(m_values.end(), value);
}

const ValueList& ListProp::GetValues()
{
	return m_values;
}

void ListProp::clear()
{
	for(ValueList::const_iterator i = m_values.begin();
		i != m_values.end();
		++i)
	{
		delete (*i);
	}
	m_values.clear();
}

//////////////////////////////////////////////////////////////////////////////
// PropCategory
//////////////////////////////////////////////////////////////////////////////

PropCategory::PropCategory(LPCTSTR name, LPCTSTR description)
{
	m_name = name;
	m_description = description;
}

PropCategory::~PropCategory()
{
	clear();
}

LPCTSTR PropCategory::GetName() const
{
	return m_name.c_str();
}

LPCTSTR PropCategory::GetDescription() const
{
	return m_description.c_str();
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

PropGroup::PropGroup(LPCTSTR name, LPCTSTR description)
{
	m_name = name;
	m_description = description;
}

PropGroup::~PropGroup()
{
	clear();
}

LPCTSTR PropGroup::GetName() const
{
	return m_name.c_str();
}

LPCTSTR PropGroup::GetDescription() const
{
	return m_description.c_str();
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

ProjectTemplate::ProjectTemplate(LPCTSTR id, LPCTSTR name, LPCTSTR ns)
{
	m_id = id;
	m_name = name;
	m_namespace = ns;
}

ProjectTemplate::ProjectTemplate(LPCTSTR id, LPCTSTR name, LPCTSTR ns, LPCTSTR icon, LPCTSTR helpfile)
{
	m_id = id;
	m_name = name;
	m_namespace = ns;
	if(icon)
		m_imagepath = icon;
	if(helpfile)
		m_helpfile = helpfile;
}

ProjectTemplate::~ProjectTemplate()
{
	for(PropSetList::const_iterator i = m_propsets.begin(); i != m_propsets.end(); ++i)
	{
		delete (*i);
	}
	m_propsets.clear();
}

LPCTSTR ProjectTemplate::GetID() const
{
	return m_id.c_str();
}

LPCTSTR ProjectTemplate::GetName() const
{
	return m_name.c_str();
}

LPCTSTR ProjectTemplate::GetNamespace() const
{
	return m_namespace.c_str();
}

LPCTSTR ProjectTemplate::GetImagePath() const
{
	return m_imagepath.c_str();
}

LPCTSTR ProjectTemplate::GetHelpFile() const
{
	return m_helpfile.c_str();
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

//////////////////////////////////////////////////////////////////////////////
// TemplateLoader
//////////////////////////////////////////////////////////////////////////////

#define PTL_DEFAULT	0
#define PTL_CONFIG	1
#define PTL_SET		2
#define PTL_GROUP	3
#define PTL_CAT		4
#define PTL_OPTLIST	5

#define MATCH(ename) \
	(_tcscmp(name, ename) == 0)

#define IN_STATE(state) \
	(m_parseState == state)

#define STATE(state) \
	m_parseState = state

#define ATTVAL(attname) \
	atts.getValue(attname)

#define BEGIN_HANDLERS() \
	if(name == NULL) \
	{	return;		}

#define HANDLE(ename, fn) \
	if(MATCH(ename)) \
	{ \
		fn(atts); \
	} else

#define HANDLEEND(ename, fn) \
	if(MATCH(ename)) \
	{ \
		fn(); \
	} else

#define HANDLE_NEWSTATE(ename, fn, s) \
	if(MATCH(ename)) \
	{ \
		fn(atts); \
		STATE(s); \
	} else

#define HANDLEEND_NEWSTATE(ename, fn, s) \
	if(MATCH(ename)) \
	{ \
		fn(); \
		STATE(s); \
	} else

#define MATCH_NEWSTATE(ename, s) \
	if(MATCH(ename)) \
	{ \
		STATE(s); \
	} else

#define BEGIN_STATE(state) \
	if(m_parseState == state) \
	{ \

#define END_STATE() \
		{} \
	} else

#define END_STATE_HANDLE_UNKNOWN(fn) \
		{ \
			fn(name, atts); \
		} \
	}

#define HANDLE_UNKNOWN_NEWSTATE(fn, s) \
	else \
	{ \
		fn(name, atts); \
		STATE(s); \
	}

#define END_HANDLERS() \
	{}

TemplateLoader::TemplateLoader()
{
	m_pTemplate = NULL;
	m_pParentGroup = NULL;
	m_pCurrentGroup = NULL;
	m_pCurrentCat = NULL;
	m_pCurrentListProp = NULL;
}

ProjectTemplate* TemplateLoader::FromFile(LPCTSTR path)
{
	XMLParser parser;
	parser.SetParseState(this);
	m_parseState = 0;

	try
	{
		parser.LoadFile(path);
	}
	catch( XMLParserException& E )
	{
		CString err;
		err.Format(_T("Error Parsing Project Template XML: %s\n (file: %s, line: %d, column %d)"), 
			XML_ErrorString(E.GetErrorCode()), E.GetFileName(), E.GetLine(), E.GetColumn());
		::MessageBox(g_Context.m_frame->GetWindow()->m_hWnd, err, _T("XML Parse Error"), MB_OK | MB_ICONWARNING);

		return NULL;
	}

	return m_pTemplate;
}

void TemplateLoader::startElement(LPCTSTR name, const XMLAttributes& atts)
{
	BEGIN_HANDLERS()
		BEGIN_STATE(PTL_DEFAULT)
			HANDLE_NEWSTATE(_T("projectConfig"), onProjectConfig, PTL_CONFIG)
		END_STATE()
		BEGIN_STATE(PTL_CONFIG)
			HANDLE_NEWSTATE(_T("set"), onSet, PTL_SET)
		END_STATE()
		BEGIN_STATE(PTL_SET)
			HANDLE_NEWSTATE(_T("group"), onGroup, PTL_GROUP)
		END_STATE()
		BEGIN_STATE(PTL_GROUP)
			HANDLE_NEWSTATE(_T("category"), onCategory, PTL_CAT)
			HANDLE(_T("group"), onGroup)
		END_STATE()
		BEGIN_STATE(PTL_CAT)
			HANDLE(_T("option"), onOption)
			HANDLE(_T("folderPath"), onFolderPath)
			HANDLE(_T("filePath"), onFilePath)
			HANDLE(_T("int"), onInt)
			HANDLE_NEWSTATE(_T("optionlist"), onOptionList, PTL_OPTLIST)
			HANDLE(_T("text"), onText)
		END_STATE()
		BEGIN_STATE(PTL_OPTLIST)
			HANDLE(_T("value"), onOptionListValue)
		END_STATE()
	END_HANDLERS()
}

void TemplateLoader::endElement(LPCTSTR name)
{
	BEGIN_HANDLERS()
		BEGIN_STATE(PTL_CONFIG)
			MATCH_NEWSTATE(_T("projectConfig"), PTL_DEFAULT)
		END_STATE()
		BEGIN_STATE(PTL_SET)
			HANDLEEND_NEWSTATE(_T("set"), onEndSet, PTL_CONFIG)
		END_STATE()
		BEGIN_STATE(PTL_GROUP)
			HANDLEEND(_T("group"), onEndGroup)
		END_STATE()
		BEGIN_STATE(PTL_CAT)
			HANDLEEND_NEWSTATE(_T("category"), onEndCategory, PTL_GROUP)
		END_STATE()
		BEGIN_STATE(PTL_OPTLIST)
			HANDLEEND_NEWSTATE(_T("optionlist"), onEndOptionList, PTL_CAT)
		END_STATE()
	END_HANDLERS()
}

void TemplateLoader::characterData(LPCTSTR data, int len)
{

}

void TemplateLoader::onProjectConfig(const XMLAttributes& atts)
{
	LPCTSTR name = ATTVAL(_T("name"));
	if(name == NULL)
		name = _T("unknown");
	LPCTSTR ns = ATTVAL(_T("ns"));
	if(ns == NULL)
		ns = _T("http://example.com/error");
	LPCTSTR id = ATTVAL(_T("id"));
	if(id == NULL)
		id = _T("error");
	LPCTSTR icon = ATTVAL(_T("icon"));
	LPCTSTR helpfile = ATTVAL(_T("helpfile"));
	m_pTemplate = new ProjectTemplate(id, name, ns, icon, helpfile);
}

void TemplateLoader::onSet(const XMLAttributes& atts)
{
	LPCTSTR type = ATTVAL(_T("type"));
	PROJECT_TYPE pType;
	if(type != NULL)
	{
		if(_tcscmp(type, _T("file")) == 0)
			pType = ptFile;
		else if(_tcscmp(type, _T("project")) == 0)
			pType = ptProject;
		else if(_tcscmp(type, _T("folder")) == 0)
			pType = ptFolder;
		else
			pType = ptFile;
	}
	else
		pType = ptFile;
	
	m_currentSetType = pType;
}

void TemplateLoader::onEndSet()
{
	if(m_PropGroups.size() > 0)
	{
		m_pTemplate->AddProperties(m_currentSetType, NULL, m_PropGroups);
		m_PropGroups.clear();
	}
}

void TemplateLoader::onGroup(const XMLAttributes& atts)
{
	if(m_pCurrentGroup != NULL)
	{
		// We're already in a group, this is a sub-group.
		if(m_pParentGroup != NULL)
		{
			// ARGH! Too much nesting, can't cope.
			// TODO: See if we can handle this better...
			return;
		}

		m_pParentGroup = m_pCurrentGroup;
	}

	LPCTSTR name = ATTVAL(_T("name"));
	LPCTSTR desc = ATTVAL(_T("description"));
	if(name == NULL)
	{
		m_pCurrentGroup = NULL;
		return;
	}
	if(desc == NULL)
		desc = _T("ERROR");

	m_pCurrentGroup = new PropGroup(name, desc);
	m_PropGroups.insert(m_PropGroups.end(), m_pCurrentGroup);
}

void TemplateLoader::onEndGroup()
{
	if(m_pCurrentGroup != NULL)
	{
		m_pCurrentGroup = NULL;
	}
	if(m_pParentGroup != NULL)
	{
		m_pCurrentGroup = m_pParentGroup;
		m_pParentGroup = NULL;
	}
	else
	{
		STATE(PTL_SET);
	}
}

void TemplateLoader::onCategory(const XMLAttributes& atts)
{
	if(!m_pCurrentGroup)
		return;

	LPCTSTR name = ATTVAL(_T("name"));
	LPCTSTR desc = ATTVAL(_T("description"));
	if(name == NULL)
	{
		m_pCurrentCat = NULL;
		return;
	}
	if(desc == NULL)
		desc = _T("ERROR");

	m_pCurrentCat = new PropCategory(name, desc);
	m_pCurrentGroup->Add(m_pCurrentCat);
}

void TemplateLoader::onEndCategory()
{
	m_pCurrentCat = NULL;
}

void TemplateLoader::onOption(const XMLAttributes& atts)
{
	makeProp(atts, propBool);
}

void TemplateLoader::onInt(const XMLAttributes& atts)
{
	makeProp(atts, propInt);
}

void TemplateLoader::onFilePath(const XMLAttributes& atts)
{
	makeProp(atts, propFile);
}

void TemplateLoader::onFolderPath(const XMLAttributes& atts)
{
	makeProp(atts, propFolder);
}

void TemplateLoader::onOptionList(const XMLAttributes& atts)
{
	if(!m_pCurrentCat)
		return;

	makeProp(atts, propChoice);
}

void TemplateLoader::onOptionListValue(const XMLAttributes& atts)
{
	if(!m_pCurrentListProp)
		return;

	LPCTSTR desc = ATTVAL(_T("description"));
	LPCTSTR value = ATTVAL(_T("value"));

	if(!value)
		return;
	if(!desc)
		desc = _T("ERROR");

	Choice* pChoice = new Choice();
	pChoice->Description = desc;
	pChoice->Value = value;
	m_pCurrentListProp->Add(pChoice);
}

void TemplateLoader::onEndOptionList()
{
	m_pCurrentListProp = NULL;
}

void TemplateLoader::onText(const XMLAttributes& atts)
{
	makeProp(atts, propString);
}

void TemplateLoader::makeProp(const XMLAttributes& atts, PropType type)
{
	if(!m_pCurrentCat)
		return;

	LPCTSTR name = ATTVAL(_T("name"));
	LPCTSTR desc = ATTVAL(_T("description"));
	LPCTSTR def = ATTVAL(_T("default"));
	LPCTSTR szHelpid = ATTVAL(_T("helpid"));
	int helpid = 0;

	if(name == NULL)
		return;
	if(desc == NULL)
		desc = _T("ERROR");
	if(def == NULL)
		def = _T("");
	if(szHelpid != NULL)
		helpid = _ttoi(szHelpid);

	ProjectProp* pProp;

	if(type == propChoice)
	{
		ListProp* pListProp = new ListProp(name, desc, def, helpid);
		pProp = static_cast<ProjectProp*>(pListProp);
		m_pCurrentListProp = pListProp;
	}
	else
	{
		pProp = new ProjectProp(name, desc, def, helpid, type);
	}

	m_pCurrentCat->Add(pProp);
}

} // namespace Projects