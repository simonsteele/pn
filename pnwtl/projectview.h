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

#include "include/wtltreems.h"
#include "include/droptargetimpl.h"

class ShellImageList;

class CProjectTreeCtrl : public CMSTreeViewCtrl
{
	typedef CMSTreeViewCtrl baseClass;
	
	typedef DropTargetImpl<CProjectTreeCtrl> DropTarget;
	friend class DropTarget;

public:
	DECLARE_WND_CLASS(_T("ProjectTree"))

	CProjectTreeCtrl();
	~CProjectTreeCtrl();

	BEGIN_MSG_MAP(CProjectTreeCtrl)
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy)
		MESSAGE_HANDLER(WM_KEYDOWN, OnKeyDown)

		// Drag and Drop...
		REFLECTED_NOTIFY_CODE_HANDLER(TVN_BEGINDRAG, OnBeginDrag)
		MESSAGE_HANDLER(WM_MOUSEMOVE, OnMouseMove)
		MESSAGE_HANDLER(WM_LBUTTONUP, OnLButtonUp)
		MESSAGE_HANDLER(WM_RBUTTONDOWN, OnRButtonDown)
		MESSAGE_HANDLER(WM_CAPTURECHANGED, OnCaptureChanged)
		MESSAGE_HANDLER(WM_MOUSEWHEEL, OnMouseWheel)
		MESSAGE_HANDLER(WM_TIMER, OnTimer)

		REFLECTED_NOTIFY_CODE_HANDLER(TVN_ENDLABELEDIT, OnEndLabelEdit)
		REFLECTED_NOTIFY_CODE_HANDLER(NM_RCLICK, OnRightClick)
		
		MESSAGE_HANDLER(WM_CONTEXTMENU, OnContextMenu)
		COMMAND_ID_HANDLER(ID_PROJECT_OPEN, OnOpenFile)
		COMMAND_ID_HANDLER(ID_PROJECT_ADDFILES, OnAddFiles)
		COMMAND_ID_HANDLER(ID_PROJECT_ADDFOLDER, OnAddFolder)
		COMMAND_ID_HANDLER(ID_PROJECT_ADDMAGICFOLDER, OnAddMagicFolder)
		COMMAND_ID_HANDLER(ID_PROJECT_OPENALLFILES, OnOpenAll)
		COMMAND_ID_HANDLER(ID_PROJECT_REMOVE, OnRemove)
		COMMAND_ID_HANDLER(ID_PROJECT_DELETE, OnDelete)
		COMMAND_ID_HANDLER(ID_PROJECT_SETACTIVEPROJECT, OnSetActiveProject)
		COMMAND_ID_HANDLER(ID_PROJECT_SORTFOLDERS, OnSortFolders)
		COMMAND_ID_HANDLER(ID_WORKSPACE_NEWPROJECT, OnNewProject)
		COMMAND_ID_HANDLER(ID_WORKSPACE_ADDPROJECT, OnAddProject)
		
		CHAIN_MSG_MAP(baseClass)
		
		REFLECTED_NOTIFY_CODE_HANDLER(TVN_SELCHANGED, OnSelChanged)
	
	END_MSG_MAP()

	HWND Create(HWND hWndParent, _U_RECT rect = NULL, LPCTSTR szWindowName = NULL,
			DWORD dwStyle = 0, DWORD dwExStyle = 0,
			_U_MENUorID MenuOrID = 0U, LPVOID lpCreateParam = NULL);

	void			AddProject(Projects::Project* project);
	Projects::File* GetSelectedFile();
	void			SetWorkspace(Projects::Workspace* ws);	

protected:
	HTREEITEM	addFileNode(Projects::File* file, HTREEITEM hParent, HTREEITEM hInsertAfter);
	HTREEITEM	addFolderNode(Projects::Folder* folder, HTREEITEM hParent, HTREEITEM hInsertAfter);
	void		buildTree();
	HTREEITEM	buildProject(HTREEITEM hParentNode, Projects::Project* pj, HTREEITEM hInsertAfter = NULL);
	HTREEITEM	buildFolders(HTREEITEM hParentNode, const Projects::FOLDER_LIST& folders, Projects::ProjectViewState& viewState);
	HTREEITEM	buildFiles(HTREEITEM hParentNode, HTREEITEM hInsertAfter, const Projects::FILE_LIST& files);
	void		clearTree();
	void		doContextMenu(LPPOINT pt);
	HTREEITEM	getLastFolderItem(HTREEITEM hParentNode);
	void		handleRemove();
	void		handleRightClick(LPPOINT pt);
	void		openAll(Projects::Folder* folder);
	void		setStatus(Projects::ProjectType* selection);
	void		sort(HTREEITEM hFolderNode, bool bSortFolders = false, bool bRecurse = false);

	// IDropTarget Drop
	void		handleDrop(HDROP hDrop, HTREEITEM hDropItem, Projects::Folder* pFolder);

	// Drag and Drop
	bool		canDrag();
	bool		canDrop();
	bool		dropSelectionContainsItem(HTREEITEM item);
	bool		dropSelectionContainsParent(HTREEITEM item);
	bool		handleDrop();
	bool		handleFolderDrop(Projects::Folder* target);
	bool		handleProjectDrop(Projects::Project* target);
	void		handleEndDrag();

	static int CompareWorker(LPARAM lParam1, LPARAM lParam2, LPARAM caseSensitive, bool sortAll);
	static int CALLBACK CompareItem(LPARAM lParam1, LPARAM lParam2, LPARAM caseSensitive);
	static int CALLBACK CompareItemSortAll(LPARAM lParam1, LPARAM lParam2, LPARAM caseSensitive);

protected:
	// IDropTarget Handlers
	HRESULT		OnDragEnter(LPDATAOBJECT /*pDataObject*/, DWORD /*dwKeyState*/, POINTL /*pt*/, LPDWORD /*pdwEffect*/);
	HRESULT		OnDragOver(DWORD /*dwKeyState*/, POINTL /*pt*/, LPDWORD /*pdwEffect*/);
	HRESULT		OnDragLeave(void);
	HRESULT		OnDrop(LPDATAOBJECT /*pDataObject*/, DWORD /*dwKeyState*/, POINTL /*pt*/, LPDWORD /*pdwEffect*/);

	// Notification Handlers
	LRESULT		OnSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
	LRESULT		OnRightClick(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
	LRESULT		OnEndLabelEdit(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
	LRESULT		OnBeginDrag(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	// Command Handlers
	LRESULT		OnNewProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT		OnAddProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT		OnOpenFile(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT		OnAddFiles(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT		OnAddFolder(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT		OnAddMagicFolder(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT		OnOpenAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT		OnRemove(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT		OnDelete(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT		OnSetActiveProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT		OnSortFolders(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	// Drag and Drop Handlers
	LRESULT		OnMouseMove(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT		OnLButtonUp(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT		OnRButtonDown(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT		OnCaptureChanged(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT		OnMouseWheel(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT		OnTimer(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	// Misc. Message Handlers
	LRESULT		OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT		OnKeyDown(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT		OnContextMenu(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

protected:
	HTREEITEM				hLastItem;
	ShellImageList*			shellImages;
	Projects::ProjectType*	lastItem;
	Projects::Workspace*	workspace;
	void*					recursiveState;
	int						projectIcon;
	int						workspaceIcon;
	int						badProjectIcon;
	bool					multipleSelection;
	bool					dragging;
	HIMAGELIST				hDragImageList;
	int						dragTimer;
	HTREEITEM				hDropTargetItem;
	HTREEITEM				hDragHoverItem;
	DWORD					dwDragHoverAcquire;
	std::list<HTREEITEM>	dropSelectedItems;
	CComObject<DropTarget>* m_pDropTarget;
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