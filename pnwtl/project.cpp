/**
 * @file project.cpp
 * @brief Projects
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "project.h"

namespace Projects
{

//////////////////////////////////////////////////////////////////////////////
// Folder
//////////////////////////////////////////////////////////////////////////////

Folder::Folder(LPCTSTR name_, LPCTSTR basepath)
{
	name = name_;
	basePath = basepath;
}

Folder::~Folder()
{
	Clear();
}

LPCTSTR Folder::GetName()
{
	return name.c_str();
}

const FOLDER_LIST& Folder::GetFolders()
{
	return children;
}

const FILE_LIST& Folder::GetFiles()
{
	return files;
}

void Folder::AddChild(Folder* folder)
{
	children.insert(children.begin(), folder);
}

void Folder::AddFile(LPCTSTR file)
{
	files.insert(files.begin(), tstring(file));
}

void Folder::Clear()
{
	for(FL_IT i = children.begin(); i != children.end(); ++i)
	{
		delete (*i);
	}

	children.clear();
}

//////////////////////////////////////////////////////////////////////////////
// Solution
//////////////////////////////////////////////////////////////////////////////

Solution::~Solution()
{
	Clear();
}

void Solution::AddProject(Project* project)
{
	projects.insert(projects.begin(), project);
}

const PROJECT_LIST Solution::GetProjects()
{
	return projects;
}

void Solution::Clear()
{
	for(PL_IT i = projects.begin(); i != projects.end(); ++i)
	{
		delete (*i);
	}
}

} // namespace Projects