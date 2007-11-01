/**
 * @file projectregistry.cpp
 * @brief Project Registry
 * @author Simon Steele
 * @note Copyright (c) 2005 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "projectregistry.h"
#include "project.h"
#include "projectprops.h"
#include "include/filefinder.h"

#if defined (_DEBUG)
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

namespace Projects
{

ProjectTemplate* Registry::FromID(LPCTSTR id) const
{
	TEMPLATE_MAP::const_iterator iT = m_templates.find(tstring(id));
	if(iT != m_templates.end())
	{
		return (*iT).second;
	}
	else
		return NULL;
}

Registry::Registry()
{
	loadCache();
}

Registry::~Registry()
{
	clear();
}

void Registry::clear()
{
	for(TEMPLATE_MAP::const_iterator i = m_templates.begin();
		i != m_templates.end();
		++i)
	{
		delete (*i).second;
	}
	m_templates.clear();
}

void Registry::loadCache()
{
	tstring templatePath;
	OPTIONS->GetPNPath(templatePath, PNPATH_PROJECTTEMPLATES);
	PNASSERT(templatePath.size() > 0);

	FileFinder<Registry> finder(this, &Registry::_onFoundFile);

	// Find all *.pnpt files in the project template directory, don't recurse.
	finder.Find(templatePath.c_str(), _T("*.pnpt"), false);
}

void Registry::_onFoundFile(LPCTSTR path, FileFinderData& file, bool& /*shouldContinue*/)
{
	TemplateLoader loader;
	
	CFileName fn(file.GetFilename());
	fn.Root(path);

	Projects::ProjectTemplate* theTemplate = loader.FromFile(fn.c_str());
	if(theTemplate != NULL)
	{
		// Get Unique ID
		tstring id = theTemplate->GetID();

		if(id.length() > 0)
		{
			m_templates.insert(TEMPLATE_MAP::value_type(id, theTemplate));
		}
		else
		{
			delete theTemplate;
		}
	}
}

const TEMPLATE_MAP& Registry::GetTemplates() const
{
	return m_templates;
}

}