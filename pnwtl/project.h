/**
 * @file project.h
 * @brief Projects
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef project_h__included
#define project_h__included

// Implement a Solution->Project->Folders hierarchy.

namespace Projects
{

class Folder;

typedef std::list<Folder*>		FOLDER_LIST;
typedef FOLDER_LIST::iterator	FL_IT;

typedef std::list<tstring>		FILE_LIST;

/**
 * @brief Represents a collection of files.
 */
class Folder
{
	public:
		Folder(LPCTSTR name_, LPCTSTR basepath);
		~Folder();

		LPCTSTR GetName();

		void AddChild(Folder* folder);
		void AddFile(LPCTSTR file);

		const FOLDER_LIST&	GetFolders();
		const FILE_LIST&	GetFiles();

	protected:
		void Clear();

	protected:
		tstring		name;
		tstring		basePath;
		FOLDER_LIST	children;
		FILE_LIST	files;
};

/**
 * @brief Represents a project within a solution.
 * Inherits from Folder to get all the folder/sub-folder behaviour.
 */
class Project : public Folder
{

};

typedef std::list<Project*>		PROJECT_LIST;
typedef PROJECT_LIST::iterator	PL_IT;

/**
 * @brief Represents a collection of Projects.
 */
class Solution
{
	public:
		~Solution();

		void AddProject(Project* project);

		const PROJECT_LIST	GetProjects();

	protected:
		void Clear();

	protected:
		PROJECT_LIST	projects;
};

}

#endif //#ifndef project_h__included