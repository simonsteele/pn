/**
 * @file projectview.cpp
 * @brief View to display project trees.
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "project.h"
#include "projectview.h"
#include "pndialogs.h"
#include "include/shellicons.h"

using namespace Projects;

//////////////////////////////////////////////////////////////////////////////
// CProjectTreeCtrl
//////////////////////////////////////////////////////////////////////////////

CProjectTreeCtrl::CProjectTreeCtrl()
{
	lastItem = NULL;
	shellImages = new ShellImageList();
	projectIcon = shellImages->AddIcon( ::LoadIcon( _Module.m_hInst, MAKEINTRESOURCE(IDI_PROJECTFOLDER)) );
	badProjectIcon = shellImages->AddIcon( ::LoadIcon( _Module.m_hInst, MAKEINTRESOURCE(IDI_BADPROJECT)) );
	workspaceIcon = shellImages->AddIcon( ::LoadIcon( _Module.m_hInst, MAKEINTRESOURCE(IDI_WORKSPACE)) );
}

CProjectTreeCtrl::~CProjectTreeCtrl()
{
	delete shellImages;
}

HWND CProjectTreeCtrl::Create(HWND hWndParent, _U_RECT rect, LPCTSTR szWindowName ,
		DWORD dwStyle, DWORD dwExStyle,
		_U_MENUorID MenuOrID, LPVOID lpCreateParam)
{
	HWND hWndRet = baseClass::Create(hWndParent, rect.m_lpRect, szWindowName, dwStyle, dwExStyle, MenuOrID.m_hMenu, lpCreateParam);

	SetImageList(shellImages->GetImageList(), TVSIL_NORMAL);

	return hWndRet;
}

void CProjectTreeCtrl::AddProject(Projects::Project* project)
{
	workspace->AddProject(project);
	buildProject(GetRootItem(), project);
	Expand(GetRootItem());
}

void CProjectTreeCtrl::SetWorkspace(Projects::Workspace* ws)
{
	workspace = ws;

	clearTree();

	if(workspace != NULL)
		buildTree();
}

File* CProjectTreeCtrl::GetSelectedFile()
{
	HTREEITEM sel = GetSelectedItem();
	if(sel != NULL)
	{
		ProjectType* pt = reinterpret_cast<ProjectType*>( GetItemData(sel) );
		if(pt->GetType() == ptFile)
		{
			File* file = static_cast<File*>( pt );
			return file;
		}
	}

	return NULL;
}

void CProjectTreeCtrl::buildTree()
{
	// Image, SelImage, hParent, hInsertAfter
	SetRedraw(FALSE);

	try
	{
		HTREEITEM hTopItem = InsertItem( workspace->GetName(), workspaceIcon, workspaceIcon, NULL, NULL );
		SetItemData(hTopItem, reinterpret_cast<DWORD_PTR>( workspace ));
		const PROJECT_LIST& projects = workspace->GetProjects();

		for(PROJECT_LIST::const_iterator i = projects.begin(); i != projects.end(); ++i)
		{
			buildProject(hTopItem, (*i));
		}

		Expand(hTopItem);
		Select(hTopItem, TVGN_CARET);
	}
	catch(...)
	{
	}

	SetRedraw(TRUE);
}

void CProjectTreeCtrl::buildProject(HTREEITEM hParentNode, Projects::Project* pj)
{
	HTREEITEM hProject = InsertItem( pj->GetName(), projectIcon, projectIcon, hParentNode, NULL );
	ProjectType* pPT = static_cast<ProjectType*>(pj);
	SetItemData(hProject, reinterpret_cast<DWORD_PTR>( pPT ));

	if(pj->Exists())
	{
		HTREEITEM hLastChild = NULL;

		const FOLDER_LIST& folders = pj->GetFolders();
		if( folders.size() > 0 )
			hLastChild = buildFolders(hProject, folders);

		buildFiles(hProject, hLastChild, pj->GetFiles());
	}
	else
	{
		HTREEITEM hni = InsertItem( _T("Could not load project..."), badProjectIcon, badProjectIcon, hProject, NULL );
		SetItemData(hni, NULL);
	}
	
	Expand(hProject);
}

HTREEITEM CProjectTreeCtrl::buildFolders(HTREEITEM hParentNode, const FOLDER_LIST& folders)
{
	HTREEITEM hFolder = NULL;
	HTREEITEM hLastChild = NULL;

	for(FOLDER_LIST::const_iterator i = folders.begin(); i != folders.end(); ++i)
	{
		hFolder = InsertItem( (*i)->GetName(), 0, 0, hParentNode, hFolder );
		SetItemData(hFolder, reinterpret_cast<DWORD_PTR>( (*i)));
		const FOLDER_LIST& folders2 = (*i)->GetFolders();
		if( folders2.size() > 0 )
			hLastChild = buildFolders(hFolder, folders2);

		const FILE_LIST& files = (*i)->GetFiles();
		hLastChild = buildFiles(hFolder, hLastChild, files);

		SortChildren(hFolder);
		Expand(hFolder);
	}

	return hFolder;
}

HTREEITEM CProjectTreeCtrl::buildFiles(HTREEITEM hParentNode, HTREEITEM hInsertAfter, const FILE_LIST& files)
{
	HTREEITEM hFile = hInsertAfter;

	for(FILE_LIST::const_iterator i = files.begin(); i != files.end(); ++i)
	{
		hFile = AddFileNode((*i), hParentNode, hFile);
	}

	return hFile;
}

void CProjectTreeCtrl::clearTree()
{
	SetRedraw(FALSE);
	DeleteAllItems();
	SetRedraw(TRUE);
}

void CProjectTreeCtrl::setStatus(Projects::ProjectType* selection)
{
	switch( selection->GetType() )
	{
	case ptFile:
		{
			File* file = static_cast<File*>( selection );
			tstring s(_T("Project file: "));
			s += file->GetFileName();
			g_Context.m_frame->SetStatusText(s.c_str());
		}
		break;

	case ptFolder:
		{
			Projects::Folder* folder = static_cast<Projects::Folder*>( selection );
			tstring s(_T("Folder root: "));
			s += folder->GetBasePath();
			g_Context.m_frame->SetStatusText(s.c_str());
		}
		break;

	case ptProject:
		{
		g_Context.m_frame->SetStatusText(_T("Project selected."));
		}
		break;

	case ptWorkspace:
		g_Context.m_frame->SetStatusText(_T("Workspace selected."));
		break;
	}
}

LRESULT CProjectTreeCtrl::OnSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMTREEVIEW n = (LPNMTREEVIEW)pnmh;

	ProjectType* pt = reinterpret_cast<ProjectType*>( GetItemData( n->itemNew.hItem ) );
	if(!pt)
		return 0;

	setStatus(pt);

	return 0;
}

LRESULT CProjectTreeCtrl::OnRightClick(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	TV_HITTESTINFO	tvhti;
	memset(&tvhti, 0, sizeof(TV_HITTESTINFO));

	CPoint pt(GetMessagePos());
	CPoint pt2(pt);

	// Keyboard right-click...
	if(pt.x == -1)
		return 0;
		
	ScreenToClient(&pt2);

	tvhti.pt = pt2;
    HitTest(&tvhti);

	lastItem = NULL;
	hLastItem = NULL;

	if(tvhti.hItem != NULL)
	{
		if (tvhti.flags & (TVHT_ONITEMLABEL|TVHT_ONITEMICON))
		{

			ProjectType* ptype = reinterpret_cast<ProjectType*>( GetItemData(tvhti.hItem) );
			hLastItem = tvhti.hItem;
			lastItem = ptype;

			if(!ptype)
				return 0;

			if(GetSelectedCount() > 1)
			{
				// We have a multiple-selection thing going on. Check that all items
				// are of the same type.
				
				HTREEITEM sel = GetFirstSelectedItem();
				while(sel)
				{
					// If any items do not match the main type, we bail.
					ProjectType* ptypeCheck = reinterpret_cast<ProjectType*>( GetItemData(sel) );
					if( !ptypeCheck )
						return 0;
					if( !(ptype->GetType() == ptypeCheck->GetType()) )
						return 0;

					sel = GetNextSelectedItem(sel);
				}

				multipleSelection = true;
			}
			else
				multipleSelection = false;

			switch(ptype->GetType())
			{
				case ptFile:
				{
					CSPopupMenu popup(IDR_POPUP_PROJECTFILE);

					CMenuItemInfo mii;
					mii.fMask = MIIM_STATE;
					mii.fState = MFS_ENABLED | MFS_DEFAULT;
					
					///@todo This doesn't work, but I'll leave it in to remind me to try
					// and fix it sometime. Stupid menus.
					::SetMenuItemInfo(popup, ID_PROJECT_OPEN, FALSE, &mii);
					
					g_Context.m_frame->TrackPopupMenu(popup, 0, pt.x, pt.y, NULL, m_hWnd);
				}
				break;

				case ptFolder:
				{
					CSPopupMenu popup(IDR_POPUP_PROJECTFOLDER);
					g_Context.m_frame->TrackPopupMenu(popup, 0, pt.x, pt.y, NULL, m_hWnd);
				}
				break;

				case ptProject:
				{
					Projects::Project* project = static_cast<Projects::Project*>(ptype);

					if(project->Exists())
					{
						CSPopupMenu popup(IDR_POPUP_PROJECT);

						if(multipleSelection)
						{
							CMenuItemInfo mii;
							mii.fMask = MIIM_STATE;
							mii.fState = MFS_DISABLED | MFS_GRAYED;

							::SetMenuItemInfo(popup, ID_PROJECT_SETACTIVEPROJECT, FALSE, &mii);
						}
						else if(workspace->GetActiveProject() == project)
						{
							CMenuItemInfo mii;
							mii.fMask = MIIM_STATE | MIIM_STRING;
							mii.fState = MFS_ENABLED | MFS_CHECKED;
							mii.dwTypeData = _T("Active Project");

							::SetMenuItemInfo(popup, ID_PROJECT_SETACTIVEPROJECT, FALSE, &mii);
						}

						g_Context.m_frame->TrackPopupMenu(popup, 0, pt.x, pt.y, NULL, m_hWnd);
					}
				}
				break;

				case ptWorkspace:
				{
					CSPopupMenu popup(IDR_POPUP_WORKSPACE);
					g_Context.m_frame->TrackPopupMenu(popup, 0, pt.x, pt.y, NULL, m_hWnd);
				}
				break;
			}
		}
	}

	return 0;
}

LRESULT CProjectTreeCtrl::OnEndLabelEdit(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMTVDISPINFO ptvdi = (LPNMTVDISPINFO)pnmh;

	// Edit cancelled...
	if(ptvdi->item.pszText == NULL)
		return 0;
	
	ProjectType* type = reinterpret_cast<ProjectType*>( GetItemData(ptvdi->item.hItem) );
	switch( type->GetType() )
	{
		case ptProject:
		case ptFolder:
		{
			Projects::Folder* pF = static_cast<Projects::Folder*>(type);
			pF->SetName(ptvdi->item.pszText);
			
			PNASSERT(ptvdi->item.mask == TVIF_TEXT);
			SetItem(&ptvdi->item);
		}
		break;

		case ptFile:
		{
			File* pF = static_cast<File*>(type);
			if( pF->Rename(ptvdi->item.pszText) )
			{
				PNASSERT(ptvdi->item.mask == TVIF_TEXT);
				SetItem(&ptvdi->item);
			}
		}
		break;

		case ptWorkspace:
		{
			Projects::Workspace* pW = static_cast<Projects::Workspace*>(type);
			pW->SetName(ptvdi->item.pszText);

			PNASSERT(ptvdi->item.mask == TVIF_TEXT);
			SetItem(&ptvdi->item);
		}
		break;
	}

	if(GetSelectedItem() == ptvdi->item.hItem)
		setStatus(type);

	return 0;
}

LRESULT CProjectTreeCtrl::OnOpenFile(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(lastItem != NULL && (lastItem->GetType() == ptFile))
	{
		HTREEITEM sel = GetFirstSelectedItem();
		while(sel)
		{
			File* pFile = reinterpret_cast<File*>( GetItemData(sel) );
			if( !g_Context.m_frame->CheckAlreadyOpen(pFile->GetFileName(), eSwitch) )
				g_Context.m_frame->Open(pFile->GetFileName(), true);

			sel = GetNextSelectedItem(sel);
		}		
	}

	return 0;
}

LRESULT CProjectTreeCtrl::OnAddFiles(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(lastItem != NULL)
	{
		Projects::Folder* folder = NULL;
		HTREEITEM hParent = NULL;

		switch(lastItem->GetType())
		{
			case ptFile:
			{
				File* file = static_cast<Projects::File*>( lastItem );
				folder = file->GetFolder();
				hParent = GetParentItem(hLastItem);
			}
			break;

			case ptProject:
			case ptFolder:
			{
				folder = static_cast<Projects::Folder*>( lastItem );
				hParent = hLastItem;
			}
			break;
		}

		if(folder == NULL)
			return 0;

		CPNOpenDialog dlgOpen(_T("All Files (*.*)|*.*|"));
		dlgOpen.m_ofn.Flags |= OFN_ALLOWMULTISELECT;
		dlgOpen.SetTitle(_T("Add Files"));
		if(dlgOpen.DoModal() == IDOK)
		{
			HTREEITEM hLastInsert = NULL;
			for(CPNOpenDialog::const_iterator i = dlgOpen.begin(); 
				i != dlgOpen.end();
				++i)
			{
				File* newFile = folder->AddFile((*i).c_str());
				
				hLastInsert = AddFileNode(newFile, hParent, hLastInsert);
			}
			
			SortChildren(hParent);
			Expand(hParent);
		}
	}

	return 0;
}

LRESULT CProjectTreeCtrl::OnAddFolder(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(lastItem == NULL)
		return 0;

	if(lastItem->GetType() == ptFolder || lastItem->GetType() == ptProject)
	{
		Projects::Folder* folder = static_cast<Projects::Folder*>(lastItem);
		Projects::Folder* newFolder = new Projects::Folder(_T("New Folder"), folder->GetBasePath());

		folder->AddChild(newFolder);

		HTREEITEM hFolderNode = AddFolderNode(newFolder, hLastItem, NULL);

		Expand(hLastItem);
		EditLabel(hFolderNode);
	}

	return 0;
}

void CProjectTreeCtrl::openAll(Projects::Folder* folder)
{
	for(FOLDER_LIST::const_iterator i = folder->GetFolders().begin();
		i != folder->GetFolders().end();
		++i)
	{
		openAll((*i));
	}

	for(FILE_LIST::const_iterator i = folder->GetFiles().begin();
		i != folder->GetFiles().end();
		++i)
	{
		if( !g_Context.m_frame->CheckAlreadyOpen((*i)->GetFileName(), eSwitch) )
			g_Context.m_frame->Open((*i)->GetFileName(), true);
	}
}

LRESULT CProjectTreeCtrl::OnOpenAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(lastItem == NULL || lastItem->GetType() == ptFile)
		return 0;

	if(lastItem->GetType() == ptWorkspace)
	{
		Workspace* pW = static_cast<Workspace*>(lastItem);
		
		for(PROJECT_LIST::const_iterator i = pW->GetProjects().begin();
			i != pW->GetProjects().end();
			++i)
		{
			openAll((*i));
		}
	}
	else
	{
		HTREEITEM sel = GetFirstSelectedItem();
		while(sel)
		{
			Projects::Folder* pF = reinterpret_cast<Projects::Folder*>( GetItemData(sel) );
			
			openAll(pF);

			sel = GetNextSelectedItem(sel);
		}
	}

	return 0;
}

LRESULT CProjectTreeCtrl::OnRemove(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(lastItem == NULL)
		return 0;

	// We can't delete lots of items and still be in the middle of using the
	// GetFirst/GetNextSelectedItem loop because the current selection will
	// change. 
	std::list<HTREEITEM> selectedItems;
	HTREEITEM sel = GetFirstSelectedItem();
	while(sel)
	{
		selectedItems.push_front(sel);
		sel = GetNextSelectedItem(sel);
	}

	std::list<HTREEITEM>::iterator i = selectedItems.begin();

	// just to be safe we cache the selected list and
	// clear the selection to be safe.
	//ClearSelection();

	switch(lastItem->GetType())
	{
		case ptFile:
		{
			// Remove a file from a folder.
			for(;i != selectedItems.end(); ++i)
			{
				File* pF = reinterpret_cast<File*>( GetItemData((*i)) );
				Projects::Folder* pFolder = pF->GetFolder();
				pFolder->RemoveFile(pF);
				DeleteItem((*i));
			}
		}
		break;

		case ptFolder:
		{
			// Remove a folder from a folder (or a project).
			for(;i != selectedItems.end(); ++i)
			{
				Projects::Folder* pFolder = reinterpret_cast<Projects::Folder*>( GetItemData((*i)) );
				Projects::Folder* pParent = pFolder->GetParent();
				pParent->RemoveChild(pFolder);
				DeleteItem((*i));
			}
		}
		break;

		case ptProject:
		{
			// All projects belong to single workspace (at the moment).
			for(;i != selectedItems.end(); ++i)
			{
				Project* pProject = reinterpret_cast<Projects::Project*>( GetItemData((*i)) );
				workspace->RemoveProject(pProject);
				DeleteItem((*i));
			}
		}
		break;
	}

	return 0;
}

LRESULT CProjectTreeCtrl::OnDelete(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(lastItem == NULL)
		return 0;

	switch(lastItem->GetType())
	{
		case ptFile:
		{
			// We can't delete lots of items and still be in the middle of using the
			// GetFirst/GetNextSelectedItem loop because the current selection will
			// change. 
			std::list<HTREEITEM> selectedItems;
			HTREEITEM sel = GetFirstSelectedItem();
			while(sel)
			{
				selectedItems.push_front(sel);
				sel = GetNextSelectedItem(sel);
			}

			std::list<HTREEITEM>::iterator i = selectedItems.begin();

			for(std::list<HTREEITEM>::iterator i = selectedItems.begin();
				i != selectedItems.end(); ++i)
			{
				File* pF = reinterpret_cast<File*>( GetItemData((*i)) );
				Projects::Folder* pFolder = pF->GetFolder();
				tstring filename = pF->GetFileName();
				tstring askstr = "Are you sure you wish to delete:\n" + filename;
				if( ::MessageBox(m_hWnd, askstr.c_str(), "Delete File", MB_YESNO | MB_ICONQUESTION) == IDYES )
				{
					if(::DeleteFile(filename.c_str()) != 0)
					{
						pFolder->RemoveFile(pF);
						DeleteItem((*i));
					}
				}

				sel = GetNextSelectedItem(sel);
			}
		}
		break;
	}

	return 0;
}

LRESULT CProjectTreeCtrl::OnSetActiveProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(lastItem == NULL)
		return 0;

	if(lastItem->GetType() != ptProject)
		return 0;

	workspace->SetActiveProject( static_cast<Projects::Project*>(lastItem) );

	return 0;
}

LRESULT CProjectTreeCtrl::OnNewProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	g_Context.m_frame->GetWindow()->PostMessage(WM_COMMAND, ID_FILE_NEW_PROJECT, NULL);
	return 0;
}

LRESULT CProjectTreeCtrl::OnAddProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CPNOpenDialog dlgOpen(_T("Project Files (*.pnproj)|*.pnproj|"));
	dlgOpen.SetTitle(_T("Open Project"));

	if(dlgOpen.DoModal() == IDOK)
	{
		Project* pProject = new Project(dlgOpen.GetSingleFileName());
		workspace->AddProject(pProject);
		buildProject(GetRootItem(), pProject);
		Expand(GetRootItem());
	}
	
	return 0;
}

HTREEITEM CProjectTreeCtrl::AddFileNode(File* file, HTREEITEM hParent, HTREEITEM hInsertAfter)
{
	HTREEITEM hFile = InsertItem( file->GetDisplayName(), 0, 0, hParent, hInsertAfter );
	SetItemData(hFile, reinterpret_cast<DWORD_PTR>( file ));

	int index = shellImages->IndexForFile( file->GetFileName() );
	SetItemImage(hFile, index, index);

	return hFile;
}

HTREEITEM CProjectTreeCtrl::AddFolderNode(Projects::Folder* folder, HTREEITEM hParent, HTREEITEM hInsertAfter)
{
	HTREEITEM hFolder = InsertItem( folder->GetName(), 0, 0, hParent, hInsertAfter );
	SetItemData(hFolder, reinterpret_cast<DWORD_PTR>( folder ));
	return hFolder;
}

//////////////////////////////////////////////////////////////////////////////
// CProjectDocker
//////////////////////////////////////////////////////////////////////////////

CProjectDocker::CProjectDocker()
{
	workspace = NULL;
}

CProjectDocker::~CProjectDocker()
{
	if(workspace != NULL)
	{
		delete workspace;
		workspace = NULL;
	}
}

LRESULT CProjectDocker::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	HICON hIconSmall = (HICON)::LoadImage(_Module.GetResourceInstance(), MAKEINTRESOURCE(IDI_PROJECTS), 
			IMAGE_ICON, ::GetSystemMetrics(SM_CXSMICON), ::GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);
	SetIcon(hIconSmall, FALSE);

	RECT rc;
	GetClientRect(&rc);
	m_view.Create(m_hWnd, rc, _T("ProjectTree"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | TVS_HASBUTTONS | TVS_HASLINES | TVS_EDITLABELS | TVS_SHOWSELALWAYS, 0, 100);

	return 0;
}

LRESULT CProjectDocker::OnSize(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	if(wParam != SIZE_MINIMIZED )
	{
		RECT rc;
		GetClientRect(&rc);
		m_view.SetWindowPos(NULL, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top ,SWP_NOZORDER | SWP_NOACTIVATE);
	}

	bHandled = FALSE;

	return 0;
}

LRESULT CProjectDocker::OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Hide();

	return 0;
}

LRESULT CProjectDocker::OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{

	bHandled = FALSE;

	return 0;
}

void CProjectDocker::SetWorkspace(Projects::Workspace* ws)
{
	workspace = ws;

	m_view.SetWorkspace(ws);
}

void CProjectDocker::AddProject(Projects::Project* project)
{
	m_view.AddProject(project);
}

Projects::Workspace* CProjectDocker::GetWorkspace()
{
	return workspace;
}

LRESULT CProjectDocker::OnTreeNotify(int /*idCtrl*/, LPNMHDR pnmh, BOOL& bHandled)
{
	LPNMTREEVIEW pN = reinterpret_cast<LPNMTREEVIEW>(pnmh);
	if(pnmh->code == NM_DBLCLK)
	{
		File* file = m_view.GetSelectedFile();
		if(file != NULL)
		{
			if( !g_Context.m_frame->CheckAlreadyOpen(file->GetFileName(), eSwitch) )
				g_Context.m_frame->Open(file->GetFileName(), true);
		}
	}
	else
		bHandled = false;

	return 0;
}

/**
 * For some reason the WM_CTLCOLOR* messages do not get to the child
 * controls with the docking windows (todo with reflection). This returns
 * the proper result.
 */
LRESULT CProjectDocker::OnCtlColor(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	return (LRESULT)::GetSysColorBrush( COLOR_WINDOW );
}