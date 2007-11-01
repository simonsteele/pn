/**
 * @file projectregistry.h
 * @brief Project Registry
 * @author Simon Steele
 * @note Copyright (c) 2005 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef projectregistry_h__included_4BCE3DE7_E517_4ef9_BFF0_449208151EFA
#define projectregistry_h__included_4BCE3DE7_E517_4ef9_BFF0_449208151EFA

#include <map>

namespace Projects
{

class ProjectTemplate;

typedef std::map<tstring, ProjectTemplate*> TEMPLATE_MAP;

/**
 * Singleton Project Type Registry, Auto-Delete
 */
class Registry : public Singleton<Registry, true>
{
	friend class Singleton<Registry, true>;
public:
	~Registry();

	void _onFoundFile(LPCTSTR path, FileFinderData& file, bool& /*shouldContinue*/);

	/// Get a Template for the passed ID. Returns NULL if not found.
	ProjectTemplate* FromID(LPCTSTR id) const;

	const TEMPLATE_MAP& GetTemplates() const;

protected:
	Registry();

	void clear();
	void loadCache();

protected:
	TEMPLATE_MAP m_templates;
};

}

#endif //#ifndef projectregistry_h__included_4BCE3DE7_E517_4ef9_BFF0_449208151EFA