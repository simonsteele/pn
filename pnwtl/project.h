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

#include <list>

#include "xmlparser.h"

typedef struct tagProjectWriter* ProjectWriter;

class Folder;
class File;

typedef std::list<Folder*>		FOLDER_LIST;
typedef FOLDER_LIST::iterator	FL_IT;

typedef std::list<File*>		FILE_LIST;
typedef FILE_LIST::iterator		FILE_IT;

typedef enum {ptFile, ptMagicFile, ptFolder, ptMagicFolder, ptProject, ptWorkspace} PROJECT_TYPE;

class XmlNode;
class XmlAttribute;
class FolderAdder;
class MagicFolderAdder;
class MagicFolderCache;

typedef std::list<XmlNode*>			LIST_NODES;
typedef std::list<XmlAttribute*>	LIST_ATTRS;

typedef LIST_NODES::iterator		XN_IT;
typedef LIST_NODES::const_iterator	XN_CIT;

typedef LIST_ATTRS::const_iterator	XA_IT;

class XmlNode
{
	public:
		XmlNode(LPCTSTR qualifiedName);
		XmlNode(LPCTSTR lpszNamespace, LPCTSTR lpszName);
		~XmlNode();
		
		void Write(ProjectWriter writer);

		void AddAttributes(XMLAttributes& atts);
		void AddChild(XmlNode* pChild);

		XmlNode* GetParent();

	protected:
		tstring		sNamespace;
		tstring		sName;

		XmlNode*	pParent;
		
		LIST_NODES	children;
		LIST_ATTRS	attributes;
};

class XmlAttribute
{
	public:
		XmlAttribute(LPCTSTR lpszNamespace, LPCTSTR lpszName, LPCTSTR lpszValue);

		void Write(ProjectWriter writer);

	protected:
		tstring		sNamespace;
		tstring		sName;
		tstring		sValue;
};

class UserData
{
	public:
		~UserData();
		
		void Add(XmlNode* node);
		//void AddAttribute(XmlAttribute* attribute);
		//void AddAttributes(XMLAttributes& atts);

		const LIST_NODES& GetNodes();

		const int GetCount();

		void Write(ProjectWriter writer);

		XN_CIT	begin();
		XN_CIT	end();

	protected:
		LIST_NODES nodes;
		//LIST_ATTRS attrs;
};

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
	friend class Folder;

	public:
		File(LPCTSTR basePath, LPCTSTR path, Projects::Folder* parent);

		LPCTSTR GetDisplayName();
		LPCTSTR GetFileName();

		Folder* GetFolder();

		bool Rename(LPCTSTR newFilePart);

		void WriteDefinition(ProjectWriter definition);

		UserData& GetUserData();

	protected:
		void setDirty();

		void SetFolder(Folder* folder);

	protected:
		tstring displayName;
		tstring fullPath;
		tstring relPath;
		Folder*	parentFolder;
		
		UserData	userData;
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

		File* FindFile(LPCTSTR filename);
		File* FindRelativeFile(LPCTSTR filename);

		void RemoveChild(Folder* folder);
		void RemoveFile(File* file);

		void DetachChild(Folder* folder);
		void DetachFile(File* file);

		void SetParent(Folder* folder);
		Folder* GetParent();

		virtual void WriteDefinition(ProjectWriter definition);

		UserData& GetUserData();

		static bool MoveFile(File* file, Projects::Folder* into);
		static bool MoveChild(Projects::Folder* folder, Projects::Folder* into);

	protected:
		void Clear();
		bool hasUserData();
		void writeContents(ProjectWriter definition);

		virtual void setDirty();

	protected:
		tstring		name;
		tstring		basePath;
		FOLDER_LIST	children;
		FILE_LIST	files;
		Folder*		parent;

		UserData	userData;
};

class MagicFolder : public Folder
{
	friend class MagicFolderCache;
	friend class Project;

	public:
		MagicFolder(LPCTSTR name_, LPCTSTR path, LPCTSTR basePath);
		virtual ~MagicFolder();

		virtual const FOLDER_LIST&	GetFolders();
		virtual const FILE_LIST&	GetFiles();

		virtual void WriteDefinition(ProjectWriter definition);

		void SetGotContents(bool bGotContents);

		void Refresh();

		LPCTSTR GetFullPath() const;

		LPCTSTR GetFilter() const;
		void SetFilter(LPCTSTR filter);

	protected:
		tstring GetFolderCachePath();

		void HandleReadCache(XMLParser* parser, XMLParseState* parent);

		static tstring getMagicFolderPath(MagicFolder* last);

	protected:
		tstring				path;
		tstring				filter;
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

		virtual void startElement(LPCTSTR name, XMLAttributes& atts);
		virtual void endElement(LPCTSTR name);
		virtual void characterData(LPCTSTR data, int len);

	protected:
		void processUserData(LPCTSTR name, XMLAttributes& atts);
		void processFile(XMLAttributes& atts);

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

		bool Exists();

		void Save();

		void SetFileName(LPCTSTR filename);
		tstring GetFileName();

		bool IsDirty();

		static bool CreateEmptyProject(LPCTSTR projectname, LPCTSTR filename);

	//Implement XMLParseState
	protected:
		virtual void startElement(LPCTSTR name, XMLAttributes& atts);
		virtual void endElement(LPCTSTR name);
		virtual void characterData(LPCTSTR data, int len){};

	protected:
		Project();

		void writeDefinition(ProjectWriter definition);	

		void processProject(XMLAttributes& atts);
		void processFolder(XMLAttributes& atts);
		void processFile(XMLAttributes& atts);
		void processMagicFolder(XMLAttributes& atts);
		void processUserData(LPCTSTR name, XMLAttributes& atts);

		virtual void setDirty();
		
		void parse();

		XMLParser*	theParser;
		Folder*		currentFolder;
		File*		lastParsedFile;
		XmlNode*	lastNode;
		tstring		fileName;
		bool		bExists;
		bool		bDirty;
		int			parseState;
		int			nestingLevel;
		int			udNestingLevel;
		int			udBase;
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

		const PROJECT_LIST	GetProjects();

		bool CanSave();

		void Save();

		void ClearDirty();
		bool IsDirty(bool bRecurse = true);

		File* FindFile(LPCTSTR filename);

		Projects::Project* GetActiveProject();
		void SetActiveProject(Projects::Project* project);

	//Implement XMLParseState
	protected:
		virtual void startElement(LPCTSTR name, XMLAttributes& atts);
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
};

class ProjectViewState
{
	public:
		ProjectViewState();
		~ProjectViewState();

		bool ShouldExpand(Folder* folder);

		void SetExpand(Folder* folder, bool expand);
		void SetExpand(LPCTSTR folderPath, bool expand);

		tstring GetFolderPath(Folder* folder);

	protected:
		void getFolderPath(Folder* folder, tstring path);

	protected:
		class ExpandCache;

		ExpandCache*	cache;
};

}

#endif //#ifndef project_h__included