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

#include "xmlparser.h"


class Folder;
class File;

typedef std::list<Folder*>		FOLDER_LIST;
typedef FOLDER_LIST::iterator	FL_IT;

typedef std::list<File*>		FILE_LIST;
typedef FILE_LIST::iterator		FILE_IT;

typedef enum {ptFile, ptFolder, ptProject, ptWorkspace} PROJECT_TYPE;

class ProjectType
{
public:
	ProjectType(PROJECT_TYPE);
	PROJECT_TYPE GetType();
protected:
	PROJECT_TYPE type;
};

class File : public ProjectType
{
	public:
		File(LPCTSTR basePath, LPCTSTR path, Projects::Folder* parent);

		LPCTSTR GetDisplayName();
		LPCTSTR GetFileName();

		Folder* GetFolder();

		bool Rename(LPCTSTR newFilePart);

	protected:
		tstring displayName;
		tstring fullPath;
		tstring relPath;
		Folder*	parentFolder;
};

/**
 * @brief Represents a collection of files.
 */
class Folder : public ProjectType
{
	public:
		Folder();
		Folder(LPCTSTR name_, LPCTSTR basepath);
		~Folder();

		void SetName(LPCTSTR name_);
		LPCTSTR GetName();

		LPCTSTR GetBasePath();

		void AddChild(Folder* folder);
		File* AddFile(LPCTSTR file);

		const FOLDER_LIST&	GetFolders();
		const FILE_LIST&	GetFiles();

		void SetParent(Folder* folder);
		Folder* GetParent();

	protected:
		void Clear();

	protected:
		tstring		name;
		tstring		basePath;
		FOLDER_LIST	children;
		FILE_LIST	files;
		Folder*		parent;
};

/**
 * @brief Represents a project within a solution.
 * Inherits from Folder to get all the folder/sub-folder behaviour.
 */
class Project : public Folder, XMLParseState
{
public:
	Project(LPCTSTR projectFile);

	bool Exists();

//Implement XMLParseState
protected:
	virtual void startElement(LPCTSTR name, XMLAttributes& atts);
	virtual void endElement(LPCTSTR name);
	virtual void characterData(LPCTSTR data, int len){};

	void processProject(XMLAttributes& atts);
	void processFolder(XMLAttributes& atts);
	void processFile(XMLAttributes& atts);

protected:
	
	void parse();

	Folder*	currentFolder;
	tstring fileName;
	bool	bExists;
	int		parseState;
	int		nestingLevel;
};

typedef std::list<Project*>		PROJECT_LIST;
typedef PROJECT_LIST::iterator	PL_IT;

/**
 * @brief Represents a collection of Projects.
 */
class Workspace : public ProjectType
{
	public:
		Workspace();
		Workspace(LPCTSTR projectFile);
		~Workspace();

		void AddProject(Project* project);

		void SetName(LPCTSTR name_);

		LPCTSTR GetName();

		const PROJECT_LIST	GetProjects();

		bool CanSave();

	protected:
		void Clear();

	protected:
		PROJECT_LIST	projects;
		tstring			name;
		tstring			fileName;
};

}

#endif //#ifndef project_h__included