/**
 * @file projectview.h
 * @brief View to display project trees.
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef projectview_h__included
#define projectview_h__included

class ShellImageList;

class CProjectTreeCtrl : public CWindowImpl<CProjectTreeCtrl, CTreeViewCtrl>
{
	typedef CWindowImpl<CProjectTreeCtrl, CTreeViewCtrl> baseClass;
public:
	DECLARE_WND_CLASS(_T("ProjectTree"))

	CProjectTreeCtrl();
	~CProjectTreeCtrl();

	BEGIN_MSG_MAP(CProjectTreeCtrl)
		REFLECTED_NOTIFY_CODE_HANDLER(TVN_SELCHANGED, OnSelChanged)
		REFLECTED_NOTIFY_CODE_HANDLER(TVN_ENDLABELEDIT, OnEndLabelEdit)
		REFLECTED_NOTIFY_CODE_HANDLER(NM_RCLICK, OnRightClick)
		COMMAND_ID_HANDLER(ID_PROJECT_OPEN, OnOpenFile)
		COMMAND_ID_HANDLER(ID_PROJECT_ADDFILES, OnAddFiles)
		COMMAND_ID_HANDLER(ID_PROJECT_ADDFOLDER, OnAddFolder)
		COMMAND_ID_HANDLER(ID_PROJECT_OPENALLFILES, OnOpenAll)
		COMMAND_ID_HANDLER(ID_PROJECT_REMOVE, OnRemove)
		COMMAND_ID_HANDLER(ID_PROJECT_DELETE, OnDelete)
		COMMAND_ID_HANDLER(ID_WORKSPACE_NEWPROJECT, OnNewProject)
		COMMAND_ID_HANDLER(ID_WORKSPACE_ADDPROJECT, OnAddProject)		
	END_MSG_MAP()

	HWND Create(HWND hWndParent, WTL::_U_RECT rect = NULL, LPCTSTR szWindowName = NULL,
			DWORD dwStyle = 0, DWORD dwExStyle = 0,
			WTL::_U_MENUorID MenuOrID = 0U, LPVOID lpCreateParam = NULL);

	void			AddProject(Projects::Project* project);
	Projects::File* GetSelectedFile();
	void			SetWorkspace(Projects::Workspace* ws);	

protected:
	void		buildTree();
	void		buildProject(HTREEITEM hParentNode, Projects::Project* pj);
	HTREEITEM	buildFolders(HTREEITEM hParentNode, const Projects::FOLDER_LIST& folders);
	HTREEITEM	buildFiles(HTREEITEM hParentNode, HTREEITEM hInsertAfter, const Projects::FILE_LIST& files);
	void		clearTree();
	void		setStatus(Projects::ProjectType* selection);
	void		openAll(Projects::Folder* folder);

	HTREEITEM	AddFileNode(Projects::File* file, HTREEITEM hParent, HTREEITEM hInsertAfter);
	HTREEITEM	AddFolderNode(Projects::Folder* folder, HTREEITEM hParent, HTREEITEM hInsertAfter);

	LRESULT		OnSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
	LRESULT		OnRightClick(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
	LRESULT		OnEndLabelEdit(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	LRESULT		OnOpenFile(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT		OnAddFiles(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT		OnAddFolder(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT		OnOpenAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT		OnRemove(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT		OnDelete(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnNewProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnAddProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

protected:
	HTREEITEM				hLastItem;
	ShellImageList*			shellImages;
	Projects::ProjectType*	lastItem;
	Projects::Workspace*	workspace;

};

class CProjectDocker : public CPNDockingWindow<CProjectDocker>
{
	typedef CProjectDocker thisClass;
	typedef CPNDockingWindow<CProjectDocker> baseClass;

public:
	DECLARE_WND_CLASS(_T("CProjectDocker"))

	CProjectDocker();
	~CProjectDocker();

	BEGIN_MSG_MAP(thisClass)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_SIZE, OnSize)
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy)
		MESSAGE_HANDLER(WM_CTLCOLOREDIT, OnCtlColor)
		COMMAND_ID_HANDLER(ID_OUTPUT_HIDE, OnHide)
		NOTIFY_ID_HANDLER(100, OnTreeNotify)
		REFLECT_NOTIFICATIONS()
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	void SetWorkspace(Projects::Workspace* ws);
	Projects::Workspace* GetWorkspace();

	void AddProject(Projects::Project* project);

protected:
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);
	LRESULT OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	LRESULT OnCtlColor(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);

	LRESULT OnTreeNotify(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	void buildTree();

	Projects::Workspace*	workspace;
	CProjectTreeCtrl		m_view;
};

#endif