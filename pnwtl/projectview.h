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

class ShellImageList;

template <class T>
class IDropTargetImpl : public IDropTarget
{
public:

	STDMETHOD(DragEnter)(LPDATAOBJECT pDataObject, DWORD dwKeyState, POINTL pt, LPDWORD pdwEffect)
	{
		return static_cast<T*>(this)->OnDragEnter(pDataObject, dwKeyState, pt, pdwEffect);
	}

	STDMETHOD(DragOver)(DWORD dwKeyState, POINTL pt, LPDWORD pdwEffect)
	{
		return static_cast<T*>(this)->OnDragOver(dwKeyState, pt, pdwEffect);
	}

	STDMETHOD(DragLeave)(void)
	{
		return static_cast<T*>(this)->OnDragLeave();
	}

	STDMETHOD(Drop)(LPDATAOBJECT pDataObject, DWORD dwKeyState, POINTL pt, LPDWORD pdwEffect)
	{
		return static_cast<T*>(this)->OnDrop(pDataObject, dwKeyState, pt, pdwEffect);
	}

	HRESULT OnDragEnter(LPDATAOBJECT /*pDataObject*/, DWORD /*dwKeyState*/, POINTL /*pt*/, LPDWORD /*pdwEffect*/)
	{
		return E_NOTIMPL;
	}

	HRESULT OnDragOver(DWORD /*dwKeyState*/, POINTL /*pt*/, LPDWORD /*pdwEffect*/)
	{
		return E_NOTIMPL;
	}

	HRESULT OnDragLeave(void)
	{
		return E_NOTIMPL;
	}

	HRESULT OnDrop(LPDATAOBJECT /*pDataObject*/, DWORD /*dwKeyState*/, POINTL /*pt*/, LPDWORD /*pdwEffect*/)
	{
		return E_NOTIMPL;
	}

};

template<class callbacks>
class DropTargetImpl : public CComObjectRoot,
	public IDropTargetImpl<DropTargetImpl>
{	
	//DECLARE_POLY_AGGREGATABLE(DropTargetImpl)

	BEGIN_COM_MAP(DropTargetImpl)
        COM_INTERFACE_ENTRY(IDropTarget)
    END_COM_MAP()

	public:
		DropTargetImpl()
		{
			pCallbacks = NULL;
		}

		callbacks* pCallbacks;

		HRESULT OnDragEnter(LPDATAOBJECT pDataObject, DWORD dwKeyState, POINTL pt, LPDWORD pdwEffect)
		{
			if(!pCallbacks) return E_NOTIMPL;
			return pCallbacks->OnDragEnter(pDataObject, dwKeyState, pt, pdwEffect);
		}

		HRESULT OnDragOver(DWORD dwKeyState, POINTL pt, LPDWORD pdwEffect)
		{
			if(!pCallbacks) return E_NOTIMPL;
			return pCallbacks->OnDragOver(dwKeyState, pt, pdwEffect);
		}

		HRESULT OnDragLeave(void)
		{
			if(!pCallbacks) return E_NOTIMPL;
			return pCallbacks->OnDragLeave();
		}

		HRESULT OnDrop(LPDATAOBJECT pDataObject, DWORD dwKeyState, POINTL pt, LPDWORD pdwEffect)
		{
			if(!pCallbacks) return E_NOTIMPL;
			return pCallbacks->OnDrop(pDataObject, dwKeyState, pt, pdwEffect);
		}
};

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
		REFLECTED_NOTIFY_CODE_HANDLER(TVN_ENDLABELEDIT, OnEndLabelEdit)
		REFLECTED_NOTIFY_CODE_HANDLER(NM_RCLICK, OnRightClick)
		COMMAND_ID_HANDLER(ID_PROJECT_OPEN, OnOpenFile)
		COMMAND_ID_HANDLER(ID_PROJECT_ADDFILES, OnAddFiles)
		COMMAND_ID_HANDLER(ID_PROJECT_ADDFOLDER, OnAddFolder)
		COMMAND_ID_HANDLER(ID_PROJECT_OPENALLFILES, OnOpenAll)
		COMMAND_ID_HANDLER(ID_PROJECT_REMOVE, OnRemove)
		COMMAND_ID_HANDLER(ID_PROJECT_DELETE, OnDelete)
		COMMAND_ID_HANDLER(ID_PROJECT_SETACTIVEPROJECT, OnSetActiveProject)
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
	HRESULT OnDragEnter(LPDATAOBJECT /*pDataObject*/, DWORD /*dwKeyState*/, POINTL /*pt*/, LPDWORD /*pdwEffect*/);
	HRESULT OnDragOver(DWORD /*dwKeyState*/, POINTL /*pt*/, LPDWORD /*pdwEffect*/);
	HRESULT OnDragLeave(void);
	HRESULT OnDrop(LPDATAOBJECT /*pDataObject*/, DWORD /*dwKeyState*/, POINTL /*pt*/, LPDWORD /*pdwEffect*/);

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
	LRESULT		OnSetActiveProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnNewProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnAddProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	void handleDrop(HDROP hDrop, HTREEITEM hDropItem, Projects::Folder* pFolder);

protected:
	HTREEITEM				hLastItem;
	ShellImageList*			shellImages;
	Projects::ProjectType*	lastItem;
	Projects::Workspace*	workspace;
	int						projectIcon;
	int						workspaceIcon;
	int						badProjectIcon;
	bool					multipleSelection;
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