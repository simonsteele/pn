/**
 * @file projectprops.cpp
 * @brief Project Properties
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef projectprops_h__included
#define projectprops_h__included

namespace Projects
{

/**
 * Types of property
 */
typedef enum {ptBool, ptInt, ptChoice, ptString, ptLongString} PropType;

/**
 * Represent one property in a template.
 */
class ProjectProp
{
public:
	ProjectProp(LPCTSTR name);
	ProjectProp(LPCTSTR name, PropType type);

	LPCTSTR GetName() const;
	PropType GetType() const;

protected:
	PropType	m_type;
	tstring		m_name;
};

typedef std::list<ProjectProp*> PropList;

/**
 * Named collection of properties.
 */
class PropCategory
{
public:
	PropCategory(LPCTSTR name);
	~PropCategory();

	void Add(ProjectProp* property);
	void Remove(ProjectProp* property);

	LPCTSTR GetName() const;

	PropList& GetProperties();

protected:
	void clear();

protected:
	tstring			m_name;
	PropList		m_list;
};

typedef std::list<PropCategory*> PropCatList;

class PropGroup;

typedef std::list<PropGroup*> PropGroupList;

/**
 * Class containing a number of categories and possible
 * subsets of categories.
 */
class PropGroup
{
public:
	PropGroup(LPCTSTR name);
	~PropGroup();

	void Add(PropCategory* cat);
	void Remove(PropCategory* cat);

	PropCatList& GetCategories();

	void Add(PropGroup* subGroup);
	void Remove(PropGroup* subGroup);

	PropGroupList& GetSubGroups();

	LPCTSTR GetName() const;

protected:
	void clear();

protected:
	PropGroupList	m_subgroups;
	PropCatList		m_cats;
	tstring			m_name;
};

/**
 * A set of categories, groups and properties - designed to match one criteria.
 */
class PropSet
{
public:
	~PropSet();

	void Add(PropGroup* group);
	void Remove(PropGroup* group);

	PropGroupList& GetGroups();

	void SetCriteria(PROJECT_TYPE pType, LPCTSTR extmask);

	bool Matches(PROJECT_TYPE pType, LPCTSTR extmask) const;

protected:
	void clear();

protected:
	PropGroupList	m_groups;
	PROJECT_TYPE	m_type;
	tstring			m_mask;
};

typedef std::list<PropSet*> PropSetList;

/**
 * This class will define a single project template.
 */
class ProjectTemplate
{
public:
	ProjectTemplate(LPCTSTR name, LPCTSTR path);
	~ProjectTemplate();

	LPCTSTR GetName() const;

	PropSet* GetProperties(PROJECT_TYPE type) const;

	void AddProperties(PROJECT_TYPE type, LPCTSTR extmask, PropGroupList& orphanProps);

protected:
	PropSetList	m_propsets;
	tstring		m_name;
};

} // namespace Projects

#endif //#ifndef projectprops_h__included