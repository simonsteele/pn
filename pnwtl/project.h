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
	
	/*
	Note, this function doesn't need to be
	virtual, but if it isn't then it breaks the
	type-casting for any of the sub-classes which
	do have a virtual function table due to the
	fact that the offset to GetType() is different.
	*/
	virtual PROJECT_TYPE GetType();

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

		void WriteDefinition(ofstream& definition);

	protected:
		void setDirty();

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
	friend class File;

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

		void RemoveChild(Folder* folder);
		void RemoveFile(File* file);

		void SetParent(Folder* folder);
		Folder* GetParent();

		void WriteDefinition(ofstream& definition);

	protected:
		void Clear();
		void writeContents(ofstream& definition);

		virtual void setDirty();

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

	void Save();

	void SetFileName(LPCTSTR filename);

	bool IsDirty();

//Implement XMLParseState
protected:
	virtual void startElement(LPCTSTR name, XMLAttributes& atts);
	virtual void endElement(LPCTSTR name);
	virtual void characterData(LPCTSTR data, int len){};

protected:
	void writeDefinition(ofstream& definition);	

	void processProject(XMLAttributes& atts);
	void processFolder(XMLAttributes& atts);
	void processFile(XMLAttributes& atts);

	virtual void setDirty();
	
	void parse();

	Folder*	currentFolder;
	tstring fileName;
	bool	bExists;
	bool	bDirty;
	int		parseState;
	int		nestingLevel;
};

typedef std::list<Project*>		PROJECT_LIST;
typedef PROJECT_LIST::iterator	PL_IT;
typedef PROJECT_LIST::const_iterator	PL_CIT;

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

		void RemoveProject(Project* project);

		void SetName(LPCTSTR name_);
		void SetFileName(LPCTSTR filename_);

		LPCTSTR GetName();

		const PROJECT_LIST	GetProjects();

		bool CanSave();

		void Save();

		void ClearDirty();
		bool IsDirty(bool bRecurse = true);

	protected:
		void Clear();

	protected:
		PROJECT_LIST	projects;
		tstring			name;
		tstring			fileName;
		bool			bDirty;
};

}

#endif //#ifndef project_h__included