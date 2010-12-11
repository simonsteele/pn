/**
 * @file project.h
 * @brief Implement a Solution->Project->Folders hierarchy.
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef project_h__included
#define project_h__included

#include "projectmeta.h"

namespace Projects
{

#include <list>

#include "xmlparser.h"

class Workspace;
class ProjectViewState;
class Project;
class Folder;
class File;
class ProjectTemplate;
class ProjectWriter;
class MagicFolder;

typedef std::list<Folder*>		FOLDER_LIST;
typedef FOLDER_LIST::iterator	FL_IT;

typedef std::list<File*>		FILE_LIST;
typedef FILE_LIST::iterator		FILE_IT;

typedef enum {ptFile, ptFolder, ptMagicFolder, ptProject, ptWorkspace} PROJECT_TYPE;
typedef enum {pcAdd, pcRemove, pcEdit, pcClear, pcDirty, pcClean, pcActive} PROJECT_CHANGE_TYPE;

class FolderAdder;
class MagicFolderAdder;
class MagicFolderCache;

template <class TProjectItem>
struct ProjectTypeTraits { };

template<> struct ProjectTypeTraits<File> { static inline PROJECT_TYPE GetType() { return ptFile; } static inline bool CanChangeFrom(PROJECT_TYPE type) { return type == ptFile; } };
template<> struct ProjectTypeTraits<Folder> { static inline PROJECT_TYPE GetType() { return ptFolder; } static inline bool CanChangeFrom(PROJECT_TYPE type) { return type == ptFolder || type == ptProject || type == ptMagicFolder; } };
template<> struct ProjectTypeTraits<MagicFolder> { static inline PROJECT_TYPE GetType() { return ptMagicFolder; } static inline bool CanChangeFrom(PROJECT_TYPE type) { return type == ptMagicFolder; } };
template<> struct ProjectTypeTraits<Project> { static inline PROJECT_TYPE GetType() { return ptProject; } static inline bool CanChangeFrom(PROJECT_TYPE type) { return type == ptProject; } };
template<> struct ProjectTypeTraits<Workspace> { static inline PROJECT_TYPE GetType() { return ptWorkspace; } static inline bool CanChangeFrom(PROJECT_TYPE type) { return type == ptWorkspace; } };

/**
 * Base-type for all Projects objects
 */
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

	UserData& GetUserData();

	virtual void SetDirty() = 0;

protected:
	UserData	 userData;
	PROJECT_TYPE type;
};

/**
 * Interface class to be implemented by classes wishing to be notified
 * of project changes.
 */
class IProjectWatcher
{
public:
	virtual ~IProjectWatcher(){}
	
	/**
	 * @param changeType Type of change that occurred.
	 * @param changeContainer Folder that contains the changed/added/deleted item
	 * @param changeItem Item that was changed/added/deleted
	 */
	virtual void OnProjectItemChange(PROJECT_CHANGE_TYPE changeType, Folder* changeContainer, ProjectType* changeItem) = 0;
};

class File : public ProjectType
{
	friend class Folder;

	public:
		File(LPCTSTR basePath, LPCTSTR path, Projects::Folder* parent);

		LPCTSTR GetDisplayName();
		LPCTSTR GetFileName();
		LPCTSTR GetRelativePath();

		Folder* GetFolder();

		virtual void SetDirty();

		bool Rename(LPCTSTR newFilePart);		

	protected:
		void SetFolder(Folder* folder);

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
		explicit Folder();
		explicit Folder(LPCTSTR name_, LPCTSTR basepath);
		virtual ~Folder();

		void SetName(LPCTSTR name_);
		LPCTSTR GetName();

		LPCTSTR GetBasePath();

		void AddChild(Folder* folder);
		//void AddChild(Folder* folder, Folder* insertBelow);
		File* AddFile(LPCTSTR file);
		void AddFile(File* file);
		Folder* AddFolder(LPCTSTR path, LPCTSTR filter, bool recursive);

		virtual const FOLDER_LIST&	GetFolders();
		virtual const FILE_LIST&	GetFiles();

		void GetAllFiles(std::vector<tstring>& files);

		File* FindFile(LPCTSTR filename);
		File* FindRelativeFile(LPCTSTR filename);

		void RemoveChild(Folder* folder);
		void RemoveFile(File* file);

		void DetachChild(Folder* folder);
		void DetachFile(File* file);

		void SetParent(Folder* folder);
		Folder* GetParent();
		Project* GetProject();

		static bool MoveFile(File* file, Projects::Folder* into);
		static bool MoveChild(Projects::Folder* folder, Projects::Folder* into);

		virtual void SetDirty();

	protected:
		void Clear();
		bool hasUserData();
		virtual void notify(PROJECT_CHANGE_TYPE changeType);
		virtual void notify(PROJECT_CHANGE_TYPE changeType, ProjectType* changeItem);
		virtual void notify(PROJECT_CHANGE_TYPE changeType, Folder* changeContainer, ProjectType* changeItem);

	protected:
		bool		m_canNotify;
		tstring		name;
		tstring		basePath;
		FOLDER_LIST	children;
		FILE_LIST	files;
		Folder*		parent;
};

class MagicFolder : public Folder
{
	friend class MagicFolderCache;
	friend class Project;

	public:
		MagicFolder(LPCTSTR name_, /*LPCTSTR path,*/ LPCTSTR basePath);
		virtual ~MagicFolder();

		virtual const FOLDER_LIST&	GetFolders();
		virtual const FILE_LIST&	GetFiles();

		void SetGotContents(bool bGotContents);

		void Refresh();

		LPCTSTR GetFullPath() const;
		void SetFullPath(LPCTSTR newPath);

		LPCTSTR GetFilter() const;
		void SetFilter(LPCTSTR filter);

		LPCTSTR GetExcludedFileFilter() const;
		void SetExcludedFileFilter(LPCTSTR value);

		LPCTSTR GetFolderFilter() const;
		void SetFolderFilter(LPCTSTR filter);

		bool RenameFolder(LPCTSTR newName);

	protected:
		tstring GetFolderCachePath();

		void HandleReadCache(XMLParser* parser, XMLParseState* parent);

		static tstring getMagicFolderPath(MagicFolder* last);

	protected:
		//tstring				path;
		tstring				filter;
		tstring             excludedFileFilter;
		tstring				folderFilter;		
		bool				read;
		MagicFolderCache*	cache;
};

typedef struct tagTStringStack
{
	tstring				val;
	tagTStringStack*	previous;
} SStringStack;

/**
 * The MagicFolderCache class holds user data for files and folders that
 * were configured inside of MagicFolder folders. Because we re-read the
 * entire tree each time, this class helps us to match up the user-data
 * for each folder and file, while leaving the data intact for saved projects
 * so that external tools can use it.
 *
 * Each folder is stored in a map with a path like: \src\com\banana\, the parent
 * of which would be \src\com\. The very top folder is \.
 */
class MagicFolderCache : XMLParseState
{
	public:
		MagicFolderCache(LPCTSTR name, XMLParser* parser, XMLParseState* parent);
		~MagicFolderCache();

		Folder* GetCachedFolder(MagicFolder* actual);

		virtual void startElement(LPCTSTR name, const XMLAttributes& atts);
		virtual void endElement(LPCTSTR name);
		virtual void characterData(LPCTSTR data, int len);

	protected:
		void processUserData(LPCTSTR name, const XMLAttributes& atts);
		void processFile(const XMLAttributes& atts);

	protected:
		class FolderMap;

		XMLParser*		_parser;
		XMLParseState*	_parent;
		SStringStack*	_pathStack;
		Folder*			_current;
		File*			_currentFile;
		FolderMap*		_map;
		int				_depth;

		// Like Project
		Folder*			currentFolder;
		
		XmlNode*		lastNode;
		int				parseState;
		int				udNestingLevel;
		int				udBase;
};

/**
 * @brief Represents a project within a solution.
 * Inherits from Folder to get all the folder/sub-folder behaviour.
 */
class Project : public Folder, XMLParseState
{
	public:
		Project(LPCTSTR projectFile);
		~Project();

		bool Exists();

		void Save();

		void SetFileName(LPCTSTR filename);
		tstring GetFileName();

		bool IsDirty();

		static bool CreateEmptyProject(LPCTSTR projectname, LPCTSTR filename, LPCTSTR templateGuid = NULL);

		virtual void SetDirty();

		ProjectTemplate* GetTemplate() const;
		ProjectViewState* GetViewState();

		void SaveViewState();

	//Implement XMLParseState
	protected:
		virtual void startElement(LPCTSTR name, const XMLAttributes& atts);
		virtual void endElement(LPCTSTR name);
		virtual void characterData(LPCTSTR data, int len);

	protected:
		Project();

		friend class Workspace;
		friend class ProjectWriter;

		virtual void notify(PROJECT_CHANGE_TYPE changeType, Folder* changeContainer, ProjectType* changeItem);

		void setWorkspace(Workspace* workspace);

		void writeDefinition(ProjectWriter definition);	

		void processProject(const XMLAttributes& atts);
		void processFolder(const XMLAttributes& atts);
		void processFile(const XMLAttributes& atts);
		void processMagicFolder(const XMLAttributes& atts);
		void processUserData(LPCTSTR name, const XMLAttributes& atts);
		
		void parse();

	protected:
		XMLParser*			theParser;
		Folder*				currentFolder;
		File*				lastParsedFile;
		XmlNode*			lastNode;
		Workspace*			parentWorkspace;
		ProjectTemplate*	m_template;
		ProjectViewState*	m_viewState;
		tstring				fileName;
		tstring				udText;
		tstring				typeID;
		bool				bExists;
		bool				bDirty;
		int					parseState;
		int					nestingLevel;
		int					udNestingLevel;
		int					udBase;
};

typedef std::list<Project*>		PROJECT_LIST;
typedef PROJECT_LIST::iterator	PL_IT;
typedef PROJECT_LIST::const_iterator	PL_CIT;

/**
 * @brief Represents a collection of Projects.
 */
class Workspace : public ProjectType, XMLParseState
{
	public:
		Workspace();
		Workspace(LPCTSTR projectFile);
		~Workspace();

		void AddProject(Project* project);
		void InsertProject(Project* project, Project* insertAfter);
		void RemoveProject(Project* project);
		void DetachProject(Project* project);

		void MoveProject(Project* project, Project* moveAfter);

		void SetName(LPCTSTR name_);
		void SetFileName(LPCTSTR filename_);

		LPCTSTR GetName();
		LPCTSTR GetFileName();

		const PROJECT_LIST& GetProjects();

		bool CanSave();

		void Save();

		void ClearDirty();
		bool IsDirty(bool bRecurse = true);
		virtual void SetDirty();

		File* FindFile(LPCTSTR filename);

		Projects::Project* GetActiveProject();
		void SetActiveProject(Projects::Project* project);

		void Notify(PROJECT_CHANGE_TYPE changeType, Folder* changeContainer, ProjectType* changeItem);

		///Set to NULL to cancel watcher.
		void SetWatcher(IProjectWatcher* newWatcher);

	//Implement XMLParseState
	protected:
		virtual void startElement(LPCTSTR name, const XMLAttributes& atts);
		virtual void endElement(LPCTSTR name);
		virtual void characterData(LPCTSTR data, int len){};

	protected:
		void parse();
		void Clear();

	protected:
		int					parseState;
		PROJECT_LIST		projects;
		tstring				name;
		tstring				fileName;
		bool				bDirty;
		Projects::Project*	activeProject;
		IProjectWatcher*	watcher;
};

class ProjectViewState : XMLParseState
{
	public:
		ProjectViewState();
		~ProjectViewState();

		bool ShouldExpand(Folder* folder);

		void SetExpand(Folder* folder, bool expand);
		void SetExpand(LPCTSTR folderPath, bool expand);

		tstring GetFolderPath(Folder* folder);

		void Load(LPCTSTR filename);
		void Save(LPCTSTR filename);

		void Clear();

	protected:
		virtual void startElement(LPCTSTR name, const XMLAttributes& atts);
		virtual void endElement(LPCTSTR name);
		virtual void characterData(LPCTSTR data, int len){};

	protected:
		void getFolderPath(Folder* folder, tstring& path);

	protected:
		class ExpandCache;

		ExpandCache*	cache;
		int				parseState;
};

}

#endif //#ifndef project_h__included