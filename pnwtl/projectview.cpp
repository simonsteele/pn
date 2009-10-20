/**
 * @file projectview.cpp
 * @brief View to display project trees.
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "include/shellicons.h"
#include "include/filefinder.h"
#include "project.h"
#include "projectprops.h"
#include "projectview.h"
#include "pndialogs.h"
#include "MagicFolderWiz.h"
#include "projpropsview.h"
#include "ExplorerMenu.h"

using namespace Projects;

#define TCEX_DRAGTIMER	2

//////////////////////////////////////////////////////////////////////////////
// CProjectTreeCtrl
//////////////////////////////////////////////////////////////////////////////

CProjectTreeCtrl::CProjectTreeCtrl() :
	workspace(NULL),
	lastItem(NULL),
	m_pDropTarget(NULL),
	dragging(false),
	processNotifications(true),
	shellImages(new ShellImageList()),
	m_explorerMenu(new ShellContextMenu()),
	m_addingMagicFolder(false)
{
	projectIcon = shellImages->AddIcon( ::LoadIcon( _Module.m_hInst, MAKEINTRESOURCE(IDI_PROJECTFOLDER)) );
	badProjectIcon = shellImages->AddIcon( ::LoadIcon( _Module.m_hInst, MAKEINTRESOURCE(IDI_BADPROJECT)) );
	workspaceIcon = shellImages->AddIcon( ::LoadIcon( _Module.m_hInst, MAKEINTRESOURCE(IDI_WORKSPACE)) );
	magicFolderIcon = shellImages->AddIcon( ::LoadIcon( _Module.m_hInst, MAKEINTRESOURCE(IDI_MAGICFOLDER)) );
}

CProjectTreeCtrl::~CProjectTreeCtrl()
{	
	if(m_pDropTarget != NULL)
	{
		m_pDropTarget->Release();
		m_pDropTarget = NULL;
	}

	delete shellImages;
	delete m_explorerMenu;
}

HWND CProjectTreeCtrl::Create(HWND hWndParent, _U_RECT rect, LPCTSTR szWindowName ,
		DWORD dwStyle, DWORD dwExStyle,
		_U_MENUorID MenuOrID, LPVOID lpCreateParam)
{
	HWND hWndRet = baseClass::Create(hWndParent, rect.m_lpRect, szWindowName, dwStyle, dwExStyle, MenuOrID.m_hMenu, lpCreateParam);

	SetImageList(shellImages->GetImageList(), TVSIL_NORMAL);

	// Create an IDropTarget helper
	CComObject<DropTarget>::CreateInstance(&m_pDropTarget);
	m_pDropTarget->AddRef();
	m_pDropTarget->SetCallbackTarget(this);
	
	HRESULT hr = RegisterDragDrop(hWndRet, m_pDropTarget);
	ATLASSERT(SUCCEEDED(hr));

	// Try to get a Vista style treeview:
	if (WTL::RunTimeHelper::IsVista()) 
	{
		SetWindowTheme(L"explorer", NULL);
	}

	return hWndRet;
}



void CProjectTreeCtrl::AddProject(Projects::Project* project)
{
	workspace->AddProject(project);
}

File* CProjectTreeCtrl::GetSelectedFile()
{
	HTREEITEM sel = GetSelectedItem();
	if(sel != NULL)
	{
		ProjectType* pt = reinterpret_cast<ProjectType*>( GetItemData(sel) );
		if(pt != NULL && pt->GetType() == ptFile)
		{
			File* file = static_cast<File*>( pt );
			return file;
		}
	}

	return NULL;
}

void CProjectTreeCtrl::SetWorkspace(Projects::Workspace* ws)
{
	if(workspace != NULL)
	{
		// Remove our events subscription...
		workspace->SetWatcher(NULL);
	}

	workspace = ws;

	clearTree();

	if(workspace != NULL)
	{
		buildTree();
		workspace->SetWatcher(this);

		SendMessage(g_Context.m_frame->GetWindow()->m_hWnd, PN_PROJECTNOTIFY, (WPARAM)workspace->GetActiveProject(), pcActive);
	}
	else
	{
		SendMessage(g_Context.m_frame->GetWindow()->m_hWnd, PN_PROJECTNOTIFY, NULL, pcActive);
	}
}

void CProjectTreeCtrl::OnProjectItemChange(PROJECT_CHANGE_TYPE changeType, Projects::Folder* changeContainer, Projects::ProjectType* changeItem)
{
	if(!processNotifications)
		return;

	switch(changeType)
	{
		case pcAdd:
		{
			if(changeItem->GetType() == ptFile)
			{
				// Added a file.
				PNASSERT(changeContainer != NULL);
				HTREEITEM hParent = findFolder(changeContainer);
				PNASSERT(hParent != NULL);

				HTREEITEM hLastFolder = getLastFolderItem(hParent);
				
				if(hParent != NULL)
				{
					addFileNode(CastProjectItem<File>(changeItem), hParent, hLastFolder);
					SortChildren(hParent);
					Expand(hParent);
				}
			}
			else if(changeItem->GetType() == ptFolder || changeItem->GetType() == ptMagicFolder)
			{
				PNASSERT(changeContainer != NULL);
				HTREEITEM hParent = findFolder(changeContainer);
				PNASSERT(hParent != NULL);

				if(hParent != NULL)
				{
					Projects::Folder* folder = CastProjectItem<Projects::Folder>(changeItem);
					
					FOLDER_LIST fl;
					fl.push_back(folder);

					ProjectViewState state;
					buildFolders(hParent, fl, state);

					Expand(hParent);
				}
			}
			else if(changeItem->GetType() == ptProject)
			{
				PNASSERT(changeContainer == NULL); // workspace
				buildProject(GetRootItem(), static_cast<Projects::Project*>(changeItem));
				Expand(GetRootItem());
			}
		}
		break;

		case pcDirty:
		{
			HTREEITEM hProjectNode = findFolder(changeContainer);
			PNASSERT(hProjectNode != NULL);

			tstring str = changeContainer->GetName();
			str += _T(" *");

			SetItemText(hProjectNode, str.c_str());
		}
		break;

		case pcClean:
		{
			HTREEITEM hProjectNode = findFolder(changeContainer);
			PNASSERT(hProjectNode != NULL);

			SetItemText(hProjectNode, changeContainer->GetName());

			// Now we also store the current ViewState back into the project
			// for saving.
			Project* pProject = static_cast<Project*>(changeContainer);
			
			ProjectViewState* viewState = pProject->GetViewState();
			viewState->Clear();
			storeViewState(viewState, hProjectNode);
		}
		break;

		case pcActive:
		{
			::SendMessage(g_Context.m_frame->GetWindow()->m_hWnd, PN_PROJECTNOTIFY, (WPARAM)changeItem, pcActive);
		}
		break;

		case pcRemove:
		{
			if(changeItem->GetType() == ptFile)
			{
				// Removed a file.
				PNASSERT(changeContainer != NULL);
				HTREEITEM hParent = findFolder(changeContainer);
				PNASSERT(hParent != NULL);

				HTREEITEM hItem = findItem(changeItem, hParent);
				DeleteItem( hItem );
			}
		}
		break;
	}
}

HTREEITEM CProjectTreeCtrl::addFileNode(File* file, HTREEITEM hParent, HTREEITEM hInsertAfter)
{
	HTREEITEM hFile = InsertItem( file->GetDisplayName(), 0, 0, hParent, hInsertAfter );
	SetItemData(hFile, reinterpret_cast<DWORD_PTR>( file ));

	int index = shellImages->IndexForFile( file->GetFileName() );
	SetItemImage(hFile, index, index);

	return hFile;
}

HTREEITEM CProjectTreeCtrl::addFolderNode(Projects::Folder* folder, HTREEITEM hParent, HTREEITEM hInsertAfter)
{
	HTREEITEM hFolder = InsertItem( folder->GetName(), 0, 0, hParent, hInsertAfter );
	SetItemData(hFolder, reinterpret_cast<DWORD_PTR>( folder ));

	if(folder->GetType() == ptMagicFolder)
		SetItemImage(hFolder, magicFolderIcon, magicFolderIcon);

	return hFolder;
}

void CProjectTreeCtrl::buildTree()
{
	// Image, SelImage, hParent, hInsertAfter
	SetRedraw(FALSE);

	HTREEITEM hTopItem = InsertItem( workspace->GetName(), workspaceIcon, workspaceIcon, NULL, NULL );
	SetItemData(hTopItem, reinterpret_cast<DWORD_PTR>( workspace ));
	const PROJECT_LIST& projects = workspace->GetProjects();

	for(PROJECT_LIST::const_iterator i = projects.begin(); i != projects.end(); ++i)
	{
		buildProject(hTopItem, (*i));
	}

	Expand(hTopItem);
	Select(hTopItem, TVGN_CARET);

	SetRedraw(TRUE);
}

HTREEITEM CProjectTreeCtrl::buildProject(HTREEITEM hParentNode, Projects::Project* pj, HTREEITEM hInsertAfter)
{
	tstring projname = pj->GetName();
	if(pj->IsDirty())
		projname += _T(" *");
	HTREEITEM hProject = InsertItem( projname.c_str(), projectIcon, projectIcon, hParentNode, hInsertAfter );
	ProjectType* pPT = static_cast<ProjectType*>(pj);
	SetItemData(hProject, reinterpret_cast<DWORD_PTR>( pPT ));

	if(pj->Exists())
	{
		HTREEITEM hLastChild = NULL;

		ProjectViewState* state = pj->GetViewState();

		const FOLDER_LIST& folders = pj->GetFolders();
		if( folders.size() > 0 )
			hLastChild = buildFolders(hProject, folders, *state);

		buildFiles(hProject, hLastChild, pj->GetFiles());

		sort(hProject);
	}
	else
	{
		HTREEITEM hni = InsertItem( _T("Could not load project..."), badProjectIcon, badProjectIcon, hProject, NULL );
		SetItemData(hni, NULL);
	}
	
	Expand(hProject);

	return hProject;
}

HTREEITEM CProjectTreeCtrl::buildFolders(HTREEITEM hParentNode, const FOLDER_LIST& folders, Projects::ProjectViewState& viewState)
{
	HTREEITEM hFolder = getLastFolderItem(hParentNode);
	HTREEITEM hLastChild = NULL;

	for(FOLDER_LIST::const_iterator i = folders.begin(); i != folders.end(); ++i)
	{
		hFolder = addFolderNode((*i), hParentNode, hFolder);
		
		const FOLDER_LIST& folders2 = (*i)->GetFolders();
		if( folders2.size() > 0 )
			hLastChild = buildFolders(hFolder, folders2, viewState);

		const FILE_LIST& files = (*i)->GetFiles();
		hLastChild = buildFiles(hFolder, hLastChild, files);

		sort(hFolder);

		if( viewState.ShouldExpand((*i)) )
			Expand(hFolder);
	}

	return hFolder;
}

HTREEITEM CProjectTreeCtrl::buildFiles(HTREEITEM hParentNode, HTREEITEM hInsertAfter, const FILE_LIST& files)
{
	HTREEITEM hFile = hInsertAfter;

	for(FILE_LIST::const_iterator i = files.begin(); i != files.end(); ++i)
	{
		hFile = addFileNode((*i), hParentNode, hFile);
	}

	return hFile;
}

void CProjectTreeCtrl::clearNode(HTREEITEM hItem)
{
	HTREEITEM hN = GetChildItem(hItem);
	while(hN)
	{
		DeleteItem(hN);
		hN = GetChildItem(hItem);
	}	
}

void CProjectTreeCtrl::clearTree()
{
	SetRedraw(FALSE);
	DeleteAllItems();
	SetRedraw(TRUE);
}

void CProjectTreeCtrl::doContextMenu(LPPOINT pt)
{
	if(hLastItem != NULL && lastItem != NULL)
	{
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
					return;
				if( !(lastItem->GetType() == ptypeCheck->GetType()) )
					return;

				sel = GetNextSelectedItem(sel);
			}

			multipleSelection = true;
		}
		else
			multipleSelection = false;

		switch (lastItem->GetType())
		{
			case ptFile:
			{
				File* f = static_cast<File*>(lastItem);
				
				CPidl pidl;
				if (AtlGetFilePidl2(f->GetFileName(), &pidl))
				{
					// We can show the explorer menu:
					CSPopupMenu popup(IDR_POPUP_PROJECTFILE);
					CSPopupMenu explorer;
					InsertMenu(popup.GetHandle(), ID_DUMMY_EXPLORER, MF_BYCOMMAND | MF_POPUP, reinterpret_cast<UINT_PTR>(explorer.GetHandle()), _T("Explorer"));
					DeleteMenu(popup.GetHandle(), ID_DUMMY_EXPLORER, MF_BYCOMMAND);
					
					int nCmd;
					BOOL result = m_explorerMenu->TrackPopupMenu(pidl, pt->x, pt->y, m_hWnd, popup.GetHandle(), explorer.GetHandle(), 10000, 11000, nCmd);
					if (result && (nCmd < 10000 || nCmd > 11000))
					{
						::PostMessage(m_hWnd, WM_COMMAND, nCmd, 0L);
					}
				}
				else
				{
					CSPopupMenu popup(IDR_POPUP_PROJECTFILE);
					DeleteMenu(popup.GetHandle(), ID_DUMMY_EXPLORER, MF_BYCOMMAND);

					g_Context.m_frame->TrackPopupMenu(popup, 0, pt->x, pt->y, NULL, m_hWnd);
				}
			}
			break;

			case ptFolder:
			{
				CSPopupMenu popup(IDR_POPUP_PROJECTFOLDER);
				g_Context.m_frame->TrackPopupMenu(popup, 0, pt->x, pt->y, NULL, m_hWnd);
			}
			break;

			case ptMagicFolder:
			{
				CSPopupMenu popup(IDR_POPUP_PROJECTMFOLDER);

				HTREEITEM hParent = GetParentItem(hLastItem);
				PNASSERT(hParent != NULL);
				ProjectType* pPTParent = reinterpret_cast<ProjectType*>( GetItemData(hParent) );
				if(pPTParent->GetType() == ptMagicFolder)
				{
					popup.EnableMenuItem(ID_PROJECT_REMOVE, false);
				}

				g_Context.m_frame->TrackPopupMenu(popup, 0, pt->x, pt->y, NULL, m_hWnd);
			}
			break;

			case ptProject:
			{
				Projects::Project* project = static_cast<Projects::Project*>(lastItem);

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

					g_Context.m_frame->TrackPopupMenu(popup, 0, pt->x, pt->y, NULL, m_hWnd);
				}
				else
				{
					CSPopupMenu popup(IDR_POPUP_REMOVEPROJECT);

					g_Context.m_frame->TrackPopupMenu(popup, 0, pt->x, pt->y, NULL, m_hWnd);
				}
			}
			break;

			case ptWorkspace:
			{
				CSPopupMenu popup(IDR_POPUP_WORKSPACE);
				g_Context.m_frame->TrackPopupMenu(popup, 0, pt->x, pt->y, NULL, m_hWnd);
			}
			break;
		}
	}
}

HTREEITEM CProjectTreeCtrl::findItem(Projects::ProjectType* item, HTREEITEM startat)
{
	HTREEITEM hN = startat != NULL ? startat : GetRootItem();
	while(hN)
	{
		if(GetItemData(hN) == reinterpret_cast<DWORD_PTR>(item))
		{
			return hN;
		}

		// If we have children, walk'em...
		HTREEITEM hChild = GetChildItem( hN );
		if(hChild != NULL)
		{
			HTREEITEM ret = findItem(item, hChild);
			if(ret)
				return ret;
		}

		hN = GetNextSiblingItem( hN );
	}

	return NULL;
}

HTREEITEM CProjectTreeCtrl::findFolder(Projects::Folder* folder)
{
	HTREEITEM hItem = GetRootItem();
	return findItem(static_cast<Projects::ProjectType*>(folder), hItem);
}

HTREEITEM CProjectTreeCtrl::getLastFolderItem(HTREEITEM hParentNode)
{
	HTREEITEM hN = GetChildItem(hParentNode);
	HTREEITEM hLast = NULL;

	while (hN != NULL)
	{
		ProjectType* pT = reinterpret_cast<ProjectType*>( GetItemData(hN) );
		if(pT->GetType() != ptFolder)
			break;

		hLast = hN;
		hN = GetNextItem(hN, TVGN_NEXT);
	}

	return hLast;
}

void CProjectTreeCtrl::getMagicFolderProps(Projects::UserData& ud, Projects::MagicFolder* mf, Projects::PropGroupList& groups)
{
	Projects::PropGroup* extrasGroup = new PropGroup(_T("Folder"), _T("Magic Folder"));

	// Set up the ud object with MagicFolder options.
	ud.Set(_T(""), _T("Folder"), _T("Folder"), _T("Name"), mf->GetName());
	ud.Set(_T(""), _T("Folder"), _T("Folder"), _T("Path"), mf->GetFullPath());
	ud.Set(_T(""), _T("Folder"), _T("Filters"), _T("IncludeFiles"), mf->GetFilter());
	ud.Set(_T(""), _T("Folder"), _T("Filters"), _T("ExcludeFolders"), mf->GetFolderFilter());
	
	PropCategory* cat = new PropCategory(_T("Folder"), _T("Magic Folders"));
	cat->Add( new ProjectProp(_T("Name"), _T("Display Name"), propString) );
	cat->Add( new ProjectProp(_T("Path"), _T("Folder Path"), propFolder) );
	extrasGroup->Add(cat);

	cat = new PropCategory(_T("Filters"), _T("Filters"));
	cat->Add( new ProjectProp(_T("IncludeFiles"), _T("Include Files"), propString) );
	cat->Add( new ProjectProp(_T("ExcludeFolders"), _T("Exclude Folders"), propString) );
	extrasGroup->Add(cat);

	groups.insert(groups.end(), extrasGroup);	
}

void CProjectTreeCtrl::handleRemove()
{
	if(lastItem == NULL)
		return;

	PROJECT_TYPE ptypeCheck = lastItem->GetType();

	// We can't delete lots of items and still be in the middle of using the
	// GetFirst/GetNextSelectedItem loop because the current selection will
	// change. 
	std::list<HTREEITEM> selectedItems;
	HTREEITEM sel = GetFirstSelectedItem();
	while (sel)
	{
		if (GetProjectItem<ProjectType>(sel)->GetType() != ptypeCheck)
		{
			::MessageBeep(MB_ICONASTERISK);
			return;
		}

		selectedItems.push_front(sel);
		sel = GetNextSelectedItem(sel);
	}

	std::list<HTREEITEM>::iterator i = selectedItems.begin();

	switch(lastItem->GetType())
	{
		case ptFile:
		{
			// Remove a file from a folder.
			for(;i != selectedItems.end(); ++i)
			{
				File* pF = reinterpret_cast<File*>( GetItemData((*i)) );
				Projects::Folder* pFolder = pF->GetFolder();
				
				// We don't remove from magic folders:
				if (pFolder->GetType() == ptMagicFolder)
				{
					continue;
				}

				pFolder->RemoveFile(pF);
			}
		}
		break;

		case ptFolder:
		case ptMagicFolder:
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
				Project* pProject = reinterpret_cast<Projects::Project*>(GetItemData((*i)));

				if(pProject->IsDirty())
				{
					CT2CW name(pProject->GetName());
					CStringW msg;
					msg.Format(IDS_QSAVEPROJBEFOREREMOVE, name);

					std::wstring title(L10N::StringLoader::GetW(IDR_MAINFRAME));
					std::wstring saveAndRemove(L10N::StringLoader::GetW(IDS_SAVEANDREMOVE));
					std::wstring remove(L10N::StringLoader::GetW(IDS_REMOVE));
					std::wstring dontRemove(L10N::StringLoader::GetW(IDS_DONOTREMOVE));

					TASKDIALOG_BUTTON confirmRemoveButtons[3] = {
						{ IDYES, saveAndRemove.c_str() },
						{ IDNO, remove.c_str() },
						{ IDCANCEL, dontRemove.c_str() },
					};

					TASKDIALOGCONFIG cfg = { 0 };
					cfg.cbSize = sizeof(cfg);
					cfg.hwndParent = m_hWnd;
					cfg.hInstance = _Module.GetResourceInstance();
					cfg.pszWindowTitle = title.c_str();
					cfg.pszMainIcon = MAKEINTRESOURCEW(TDT_WARNING_ICON);
					cfg.pszContent = (LPCWSTR)msg;
					cfg.dwCommonButtons = 0;
					cfg.pButtons = confirmRemoveButtons;
					cfg.cButtons = 3;
					cfg.nDefaultButton = IDCANCEL;

					switch(PNTaskDialogIndirect(&cfg))
					{
						case IDCANCEL:
						{
							return;
						}

						case IDYES:
						{
							pProject->Save();
						}
						break;
					}
				}

				workspace->RemoveProject(pProject);
				DeleteItem((*i));
			}
		}
		break;
	}	
}

void CProjectTreeCtrl::openAll(Projects::Folder* folder)
{
	for(FOLDER_LIST::const_iterator i = folder->GetFolders().begin();
		i != folder->GetFolders().end();
		++i)
	{
		openAll((*i));
	}

	for(FILE_LIST::const_iterator j = folder->GetFiles().begin();
		j != folder->GetFiles().end();
		++j)
	{
		if( !g_Context.m_frame->CheckAlreadyOpen((*j)->GetFileName(), eSwitch) )
			g_Context.m_frame->Open((*j)->GetFileName(), true);
	}
}

void CProjectTreeCtrl::refreshMagicFolder(Projects::MagicFolder* folder, HTREEITEM hFolderNode)
{
	// Tell the magic folder that we've not got it's contents any more.
	folder->SetGotContents(false);

	clearNode(hFolderNode);

	// Use for expansion state etc.
	ProjectViewState state;

	processNotifications = false;

	HTREEITEM hLastChild = NULL;

	const FOLDER_LIST& folders2 = folder->GetFolders();
	if( folders2.size() > 0 )
		hLastChild = buildFolders(hFolderNode, folders2, state);

	const FILE_LIST& files = folder->GetFiles();
	hLastChild = buildFiles(hFolderNode, hLastChild, files);

	sort(hFolderNode);

	Expand(hFolderNode);

	processNotifications = true;	
}

void CProjectTreeCtrl::handleRightClick(LPPOINT pt)
{
	//CPoint pt(GetMessagePos());
	CPoint pt2(*pt);

	// Test for keyboard right-click...
	if(pt->x != -1)
	{
		ScreenToClient(&pt2);

		TVHITTESTINFO tvhti;
		memset(&tvhti, 0, sizeof(TV_HITTESTINFO));
		
		tvhti.pt = pt2;
		HitTest(&tvhti);

		lastItem = NULL;
		hLastItem = NULL;

		if(tvhti.hItem != NULL)
		{
			if (tvhti.flags & (TVHT_ONITEM|TVHT_ONITEMRIGHT))
			{

				ProjectType* ptype = GetProjectItem<ProjectType>(tvhti.hItem);
				hLastItem = tvhti.hItem;
				lastItem = ptype;

				if(!ptype)
					return;
			}
		}
	}

	doContextMenu(pt);
}

void CProjectTreeCtrl::setMagicFolderProps(Projects::UserData& ud, Projects::MagicFolder* folder)
{
	HTREEITEM hFolder = findItem(folder, NULL);
	PNASSERT(hFolder != NULL);

	// Set the Name and Filters
	folder->SetName( ud.Lookup(_T(""), _T("Folder"), _T("Folder"), _T("Name"), _T("error")) );
	SetItemText(hFolder, folder->GetName());
	folder->SetFilter( ud.Lookup(_T(""), _T("Folder"), _T("Filters"), _T("IncludeFiles"), _T("")) );
	folder->SetFolderFilter( ud.Lookup(_T(""), _T("Folder"), _T("Filters"), _T("ExcludeFolders"), _T("")) );

	// Now see if we've changed path...
	tstring path = ud.Lookup(_T(""), _T("Folder"), _T("Folder"), _T("Path"), _T(""));
	if(path.length() > 0 && DirExists(path.c_str()))
	{
		CPathName pn(path.c_str());
		if(pn.c_str() != folder->GetFullPath())
		{
			// Path may have changed...
			folder->SetFullPath(path.c_str());

			refreshMagicFolder(folder, hFolder);
		}
	}
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

void CProjectTreeCtrl::sort(HTREEITEM hFolderNode, bool bSortFolders, bool bRecurse)
{
	/*typedef struct tagTVSORTCB {
		HTREEITEM hParent;
		PFNTVCOMPARE lpfnCompare;
		LPARAM lParam;
	} TVSORTCB, *LPTVSORTCB;*/

	TVSORTCB sortcb;
	sortcb.hParent = hFolderNode;
	
	if(bSortFolders)
		sortcb.lpfnCompare = &CProjectTreeCtrl::CompareItemSortAll;
	else
		sortcb.lpfnCompare = &CProjectTreeCtrl::CompareItem;

	BOOL caseSensitive = OPTIONS->Get(_T("Projects"), _T("SortCaseSensitive"), true);
	sortcb.lParam = caseSensitive;

	SortChildrenCB(&sortcb, bRecurse);
}

void CProjectTreeCtrl::storeViewState(Projects::ProjectViewState* vs, HTREEITEM hTreeItem)
{
	HTREEITEM hN = hTreeItem;
	while( hN )
	{
		ProjectType* pt = reinterpret_cast<ProjectType*>( GetItemData(hN) );
		if(ItemHasChildren( hN ))
		{
			DWORD dwState = GetItemState(hN, TVIS_EXPANDED);
			bool bExpanded = (dwState & TVIS_EXPANDED) != 0;
			vs->SetExpand(static_cast<Projects::Folder*>( pt ), bExpanded);

            HTREEITEM hChild = GetChildItem( hN );
			if(hChild && bExpanded)
			{
				storeViewState(vs, hChild);
			}
		}
		hN = GetNextSiblingItem( hN );
	}
}

LRESULT CProjectTreeCtrl::OnSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMTREEVIEW n = (LPNMTREEVIEW)pnmh;

	ProjectType* pt = reinterpret_cast<ProjectType*>( GetItemData( n->itemNew.hItem ) );
	
	if(!pt)
	{
		hLastItem = NULL;
		lastItem = NULL;
		return 0;
	}
	
	hLastItem = n->itemNew.hItem;
	lastItem = pt;

	setStatus(pt);

	return 0;
}

LRESULT CProjectTreeCtrl::OnRightClick(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	CPoint pt(GetMessagePos());
	handleRightClick(&pt);

	return 0;
}

LRESULT CProjectTreeCtrl::OnEndLabelEdit(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMTVDISPINFO ptvdi = (LPNMTVDISPINFO)pnmh;

	// Edit cancelled...
	if(ptvdi->item.pszText == NULL)
	{
		if (m_addingMagicFolder)
		{
			m_addingMagicFolder = false;

			Projects::MagicFolder* pF = GetProjectItem<MagicFolder>(ptvdi->item.hItem);
			pF->GetParent()->RemoveChild(pF);
		}

		return 0;
	}
	
	ProjectType* type = GetProjectItem<ProjectType>(ptvdi->item.hItem);
	switch( type->GetType() )
	{
		case ptProject:
		case ptFolder:
		{
			Projects::Folder* pF = CastProjectItem<Projects::Folder>(type);
			pF->SetName(ptvdi->item.pszText);
			
			PNASSERT(ptvdi->item.mask == TVIF_TEXT);
			SetItem(&ptvdi->item);
		}
		break;

		case ptMagicFolder:
		{
			Projects::MagicFolder* pF = CastProjectItem<Projects::MagicFolder>(type);
			if (m_addingMagicFolder)
			{
				m_addingMagicFolder = false;

				CPathName path(pF->GetFullPath());
				path.ChangeLastElement(ptvdi->item.pszText);

				if (::DirExists(path.c_str()))
				{
					pF->GetParent()->RemoveChild(pF);
					return 0;
				}
				else if (!CreateDirectoryRecursive(path.c_str()))
				{
					pF->GetParent()->RemoveChild(pF);
					return 0;
				}

				pF->SetName(ptvdi->item.pszText);
				pF->SetFullPath(path.c_str());
				
				PNASSERT(ptvdi->item.mask == TVIF_TEXT);
				SetItem(&ptvdi->item);
			}
			else if (pF->GetParent() == NULL || pF->GetParent()->GetType() != ptMagicFolder)
			{
				// This is the root magic folder, renaming just changes the display name.
				pF->SetName(ptvdi->item.pszText);
			}
			else
			{
				if (::PNTaskDialog(m_hWnd, LS(IDR_MAINFRAME), IDS_MAGICFOLDERRENAME, _T(""), TDCBF_YES_BUTTON | TDCBF_NO_BUTTON | TDCBF_CANCEL_BUTTON, TDT_WARNING_ICON) == IDYES)
				{
					if (pF->RenameFolder(ptvdi->item.pszText))
					{
						pF->SetName(ptvdi->item.pszText);
						
						PNASSERT(ptvdi->item.mask == TVIF_TEXT);
						SetItem(&ptvdi->item);
					}
				}
			}
		}
		break;

		case ptFile:
		{
			File* pF = CastProjectItem<File>(type);
			if( pF->Rename(ptvdi->item.pszText) )
			{
				PNASSERT(ptvdi->item.mask == TVIF_TEXT);
				ptvdi->item.mask |= TVIF_IMAGE |TVIF_SELECTEDIMAGE;
				ptvdi->item.iImage = shellImages->IndexForFile(pF->GetFileName());
				ptvdi->item.iSelectedImage = ptvdi->item.iImage;
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

LRESULT	CProjectTreeCtrl::OnBeginDrag(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMTREEVIEW lpnmtv = (LPNMTREEVIEW)pnmh;

	// Clear out values that will be used in the drag and drop stuff...
	hDropTargetItem = NULL;

	// Cache the selected items for use elsewhere in the drag and drop procedure...
	dropSelectedItems.clear();
	HTREEITEM sel = GetFirstSelectedItem();
	while(sel)
	{
		dropSelectedItems.insert(dropSelectedItems.begin(), sel);
		sel = GetNextSelectedItem(sel);
	}

	// See if we're actually allowed to drag...
	if(!canDrag())
		return 0;

	// Tell the tree-view control to create an image to use 
    // for dragging. 
	HIMAGELIST hImageList = TreeView_CreateDragImage(m_hWnd, lpnmtv->itemNew.hItem);
	hDragImageList = hImageList;

	// Get the bounding rectangle of the item being dragged. 
	RECT rcItem;
	TreeView_GetItemRect(m_hWnd, lpnmtv->itemNew.hItem, &rcItem, TRUE);

	if(ImageList_BeginDrag(hImageList, 0, 0, 0) == 0)
		::OutputDebugString(_T("Failed BeginDrag\n"));

	ImageList_DragEnter(m_hWnd, lpnmtv->ptDrag.x, lpnmtv->ptDrag.y);

	//ShowCursor(FALSE);
	SetCapture();
	dragTimer = SetTimer(TCEX_DRAGTIMER, 25, NULL);

	dragging = true;

	return 0;
}

LRESULT CProjectTreeCtrl::OnNewProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	g_Context.m_frame->GetWindow()->PostMessage(WM_COMMAND, ID_FILE_NEW_PROJECT, NULL);
	return 0;
}

LRESULT CProjectTreeCtrl::OnAddProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CAutoOpenDialog dlgOpen(_T("Project Files (*.pnproj)|*.pnproj|"));
	dlgOpen.SetTitle(_T("Open Project"));

	if(dlgOpen.DoModal() == IDOK)
	{
		Project* pProject = new Project(dlgOpen.GetSingleFileName());
		workspace->AddProject(pProject);
	}
	
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

		CAutoOpenDialog dlgOpen(LS(IDS_ALLFILES));
		dlgOpen.SetAllowMultiSelect(true);
		dlgOpen.SetTitle(_T("Add Files"));
		if(dlgOpen.DoModal() == IDOK)
		{
			HTREEITEM hLastInsert = getLastFolderItem(hParent);

			processNotifications = false;

			for (IFileOpenDialogBase::const_iterator i = dlgOpen.begin(); 
				i != dlgOpen.end();
				++i)
			{
				File* newFile = folder->AddFile((*i).c_str());
				
				hLastInsert = addFileNode(newFile, hParent, hLastInsert);
			}

			processNotifications = true;
			
			SortChildren(hParent);
			Expand(hParent);
		}
	}

	return 0;
}

LRESULT CProjectTreeCtrl::OnAddFolder(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if (lastItem == NULL)
		return 0;

	if (lastItem->GetType() == ptFolder || lastItem->GetType() == ptProject)
	{
		Projects::Folder* folder = static_cast<Projects::Folder*>(lastItem);
		Projects::Folder* newFolder = new Projects::Folder(LS(IDS_PROJECTS_TEMPMAGICFOLDER), folder->GetBasePath());

		// Add the folder (this will add it to the tree through notifications)
		folder->AddChild(newFolder);
		
		HTREEITEM hFolderNode = findFolder(newFolder);
		EditLabel(hFolderNode);
	}

	return 0;
}

LRESULT CProjectTreeCtrl::OnAddMagicFolder(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(lastItem == NULL || lastItem->GetType() != ptProject)
		return 0;

	MagicFolderWizard1 wiz;
	MagicFolderWizard2 wiz2;
	CPropertySheet ps(_T("Add Magic Folder"));
	ps.m_psh.dwFlags |= PSH_WIZARD97;
	//ps.SetWizardMode();
	ps.AddPage(wiz);
	ps.AddPage(wiz2);
	
	int res = ps.DoModal();
	if(res == IDOK)
	{
		CPathName pn(wiz.GetSelFolder());

		processNotifications = false;

		// Add a Magic Folder...
		Projects::Folder* folder = GetLastItem<Projects::Folder>();
		Projects::MagicFolder* newFolder = new Projects::MagicFolder(pn.GetDirectoryName().c_str(), pn.c_str());

		newFolder->SetFilter( wiz2.GetFileFilter() );
		newFolder->SetFolderFilter( wiz2.GetFolderFilter() );

		folder->AddChild(newFolder);
			
		FOLDER_LIST fl;
		fl.insert(fl.end(), newFolder);
		ProjectViewState viewState;
		HTREEITEM theFolder = buildFolders(hLastItem, fl, viewState);

		processNotifications = true;

		sort(theFolder);

		Expand(hLastItem);
	}

	return 0;
}

LRESULT CProjectTreeCtrl::OnOpenAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(lastItem == NULL || lastItem->GetType() == ptFile)
		return 0;

	if(lastItem->GetType() == ptWorkspace)
	{
		Workspace* pW = GetLastItem<Workspace>();
		
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
			Projects::Folder* pF = GetProjectItem<Projects::Folder>(sel);
			
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

	handleRemove();

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

			for(std::list<HTREEITEM>::iterator i = selectedItems.begin();
				i != selectedItems.end(); ++i)
			{
				File* pF = GetProjectItem<File>((*i));
				Projects::Folder* pFolder = pF->GetFolder();
				tstring filename = pF->GetFileName();
				tstring askstr = _T("Are you sure you wish to delete:\n") + filename;
				if( ::MessageBox(m_hWnd, askstr.c_str(), _T("Delete File"), MB_YESNO | MB_ICONQUESTION) == IDYES )
				{
					if(::DeleteFile(filename.c_str()) != 0)
					{
						pFolder->RemoveFile(pF);
						DeleteItem((*i));
					}
				}
			}
		}
		break;

		case ptMagicFolder:
		{
			// GetFirst/GetNextSelectedItem loop because the current selection will
			// change. 
			std::list<HTREEITEM> selectedItems;
			HTREEITEM sel = GetFirstSelectedItem();
			while(sel)
			{
				selectedItems.push_front(sel);
				sel = GetNextSelectedItem(sel);
			}

			for(std::list<HTREEITEM>::const_iterator i = selectedItems.begin();
				i != selectedItems.end(); ++i)
			{
				MagicFolder* pMF = GetProjectItem<MagicFolder>((*i));
				tstring msg = _T("Are you sure you wish to delete the folder ");
				msg += pMF->GetFullPath();
				msg += _T(" and all its contents?");
				if( ::MessageBox(m_hWnd, msg.c_str(), _T("Delete Folder"), MB_YESNO | MB_ICONWARNING) == IDYES )
				{
					if(::DeleteDirectory(pMF->GetFullPath()))
					{
						Projects::Folder* pParent = pMF->GetParent();
						pParent->RemoveChild(pMF);
						DeleteItem((*i));
					}
				}
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

	workspace->SetActiveProject(GetLastItem<Projects::Project>());

	return 0;
}

/**
 * The user wants to sort the folders underneath this one.
 */
LRESULT CProjectTreeCtrl::OnSortFolders(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(lastItem == NULL)
		return 0;

	if(lastItem->GetType() != ptProject && lastItem->GetType() != ptFolder)
		return 0;

	sort(hLastItem, true);

	return 0;
}

LRESULT	CProjectTreeCtrl::OnProjectProperties(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CProjPropsView view;

	if (lastItem == NULL)
		return 0;

	Project* project;

	switch (lastItem->GetType())
	{
		case ptProject:
			project = static_cast<Project*>(lastItem);
			break;
		case ptMagicFolder:
		case ptFolder:
			project = static_cast<Projects::Folder*>(lastItem)->GetProject();
			break;
		case ptFile:
			{
				Projects::Folder* pFolder = static_cast<Projects::File*>(lastItem)->GetFolder();
				project = pFolder->GetProject();
			}
			break;
		default:
			RETURN_UNEXPECTED(_T("Unexpected project item type"), 0);
	}

	if (project == NULL)
	{
		return 0;
	}
	
	PropGroupList groups;
	UserData ud;
	bool bTopLevelMagic = false;

	// Build up any custom properties that we want to display on
	// top of the basic project ones.
	if (lastItem->GetType() == ptMagicFolder)
	{
		MagicFolder* mf = static_cast<MagicFolder*>(lastItem);
		if(mf->GetParent() != NULL && mf->GetParent()->GetType() != ptMagicFolder)
		{
			// We only have magic folder properties for the top-level one.
			bTopLevelMagic = true;
			getMagicFolderProps(ud, mf, groups);
		}
	}

	// Get the template from the project.
	ProjectTemplate* pTheTemplate = project->GetTemplate();
	
	// If we got a template or we've got extra settings to display.
	if (pTheTemplate != NULL || groups.size() > 0)
	{
		// Set up those extra properties
		PropViewSet extraSet(&ud);
		extraSet.PropertyGroups = &groups;
		extraSet.PropNamespace = _T("");

		view.SetExtraSet(&extraSet);

		// The template properties
		PropViewSet projectSet(pTheTemplate, lastItem);
		
		// Display 'em.
		if (view.DisplayFor(&projectSet))
		{
			lastItem->SetDirty();

			if(lastItem->GetType() == ptMagicFolder && bTopLevelMagic)
			{
				setMagicFolderProps(ud, static_cast<MagicFolder*>(lastItem));
			}
		}
	}
	else
	{
		PNTaskDialog(m_hWnd, IDR_MAINFRAME, IDS_PROJECTNOPROPS, _T(""), TDCBF_OK_BUTTON, TDT_INFORMATION_ICON);
	}

	for(PropGroupList::const_iterator i = groups.begin(); i != groups.end(); ++i)
	{
		delete (*i);
	}

	return 0;
}

LRESULT	CProjectTreeCtrl::OnSaveProject(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(lastItem->GetType() == ptProject)
	{
		Projects::Project* proj = static_cast<Projects::Project*>(lastItem);
		proj->Save();
	}

	return 0;
}

LRESULT CProjectTreeCtrl::OnRefresh(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(lastItem->GetType() == ptMagicFolder)
	{
		Projects::MagicFolder* mf = static_cast<Projects::MagicFolder*>(lastItem);

		refreshMagicFolder(mf, hLastItem);
	}

	return 0;
}

LRESULT	CProjectTreeCtrl::OnMagicAddFile(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(lastItem->GetType() != ptMagicFolder)
		return 0;

	Projects::MagicFolder* mf = static_cast<Projects::MagicFolder*>(lastItem);
	tstring path = mf->GetFullPath();

	CAutoSaveDialog sd(LS(IDS_ALLFILES));
	sd.SetTitle(_T("New File Name..."));
	sd.SetInitialPath(path.c_str());
	
	if (sd.DoModal() == IDOK)
	{
		CFileName fn(sd.GetSingleFileName());
		fn.ToLower();
		std::transform(path.begin(), path.end(), path.begin(), tolower);
		
		if(fn.GetPath() != path)
		{
			::MessageBox(m_hWnd, LS(IDS_PATHNOTINMAGICFOLDER), LS(IDR_MAINFRAME), MB_ICONWARNING | MB_OK);
		}
		else
		{
			// Make and blank the file...
			FILE* theFile = _tfopen(fn.c_str(), _T("wb"));
			if(theFile)
			{
				mf->AddFile(fn.c_str());
				fclose(theFile);
			}
			else
			{
				::MessageBox(m_hWnd, LS(IDS_CANTCREATEFILE), LS(IDR_MAINFRAME), MB_ICONWARNING | MB_OK);
			}
		}
	}

	return 0;
}

LRESULT	CProjectTreeCtrl::OnMagicAddFolder(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if (lastItem->GetType() != ptMagicFolder)
	{
		return 0;
	}

	m_addingMagicFolder = true;

	Projects::MagicFolder* mf = static_cast<Projects::MagicFolder*>(lastItem);
	tstring path = mf->GetFullPath();
	path += LS(IDS_PROJECTS_TEMPMAGICFOLDER);
	
	Projects::MagicFolder* temp = new Projects::MagicFolder(LS(IDS_PROJECTS_TEMPMAGICFOLDER), path.c_str());
	temp->SetFilter(mf->GetFilter());
	temp->SetFolderFilter(mf->GetFolderFilter());
	temp->SetGotContents(true);
	mf->AddChild(temp);

	// Adding the child should have resulted in a node being added to the tree:
	HTREEITEM hTempFolder = findItem(temp, hLastItem);
	
	// Make the user edit the folder name:
	EditLabel(hTempFolder);

	return 0;
}

LRESULT CProjectTreeCtrl::OnMagicOpenFolder(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(lastItem->GetType() != ptMagicFolder)
		return 0;

	Projects::MagicFolder* mf = static_cast<Projects::MagicFolder*>(lastItem);
	::ShellExecute(m_hWnd, NULL, mf->GetFullPath(), NULL, NULL, SW_SHOWNORMAL);

	return 0;
}

LRESULT CProjectTreeCtrl::OnShellOpenFile(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(lastItem->GetType() != ptFile)
		return 0;

	Projects::File* f = static_cast<Projects::File*>(lastItem);
	::ShellExecute(m_hWnd, _T("open"), f->GetFileName(), NULL, NULL, SW_SHOWNORMAL);
	
	return 0;
}

LRESULT CProjectTreeCtrl::OnBeginRenameItem(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(hLastItem != NULL)
	{
		EditLabel(hLastItem);
	}

	return 0;
}

class ImageListDragShowNoLock
{
public:
	ImageListDragShowNoLock()
	{
		ImageList_DragShowNolock(false);
	}

	~ImageListDragShowNoLock()
	{
		ImageList_DragShowNolock(true);
	}
};

LRESULT CProjectTreeCtrl::OnMouseMove(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = false;

	if(dragging)
	{
		DWORD dwLastPos = GetMessagePos();

		CPoint pt(GET_X_LPARAM(dwLastPos), GET_Y_LPARAM(dwLastPos));
		ScreenToClient(&pt);

		HTREEITEM htiTarget;
		TVHITTESTINFO tvht;
		tvht.pt.x = pt.x;
        tvht.pt.y = pt.y;
        
		if ((htiTarget = TreeView_HitTest(m_hWnd, &tvht)) != NULL) 
        { 
			// This code is surrounded by DragShowNoLock in order
			// to allow the treeview to update it's display when
			// we change the selection. Without this, we get nasty
			// trails.
			{
				ImageListDragShowNoLock lockDrag;
            
				TreeView_SelectDropTarget(m_hWnd, htiTarget);
			}

			hDropTargetItem = htiTarget;

			if(canDrop())
			{
				::SetCursor( ::LoadCursor(NULL, IDC_ARROW) );
			}
			else
			{
				::SetCursor( ::LoadCursor(NULL, IDC_NO) );
			}
        }

		ImageList_DragMove(pt.x, pt.y);
	}

	return 0;
}

LRESULT CProjectTreeCtrl::OnLButtonUp(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = false;
	if(dragging)
	{
		handleEndDrag();

		if(canDrop())
		{
			handleDrop();
		}
	}
	return 0;
}

LRESULT CProjectTreeCtrl::OnRButtonDown(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = false;
	if(dragging)
	{
		handleEndDrag();
	}
	return 0;
}

LRESULT CProjectTreeCtrl::OnCaptureChanged(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	if(dragging && (HWND)lParam != m_hWnd)
		dragging = false;

	return 0;
}

LRESULT CProjectTreeCtrl::OnMouseWheel(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	if(dragging)
	{
		// Prevent corruption when mouse scrolling during a drag operation...
		ImageListDragShowNoLock lockDrag;
		DefWindowProc(uMsg, wParam, lParam);
	}
	else
		bHandled = false;

	return 0;
}

int GetScrollLimit(HWND hWnd, int nBar)
{
	int nMin = 0, nMax = 0;
	::GetScrollRange(hWnd, nBar, &nMin, &nMax);
	SCROLLINFO info = { 0 };
	info.cbSize = sizeof(SCROLLINFO);
	info.fMask = SIF_PAGE;
	if(::GetScrollInfo(hWnd, nBar, &info))
		nMax -= ((info.nPage - 1) > 0) ? (info.nPage - 1) : 0;

	return nMax;
}

LRESULT CProjectTreeCtrl::OnTimer(UINT /*uMsg*/, WPARAM nIDEvent, LPARAM /*lParam*/, BOOL& bHandled)
{
	if(!dragging || nIDEvent != TCEX_DRAGTIMER)
	{
		bHandled = false;
		return 0;
	}

	POINT point;
    GetCursorPos(&point);
    ScreenToClient(&point);

    // highlight target

    TVHITTESTINFO tvHit;
    tvHit.pt = point;
    HTREEITEM hTarget = TreeView_HitTest(m_hWnd, &tvHit);

	// If we're hovering over an item...
	if(hTarget)
	{
		if(hTarget != hDragHoverItem)
		{
			dwDragHoverAcquire = GetTickCount();
			hDragHoverItem = hTarget;
		}
		else
		{
			if((int)(GetTickCount() - dwDragHoverAcquire) > 1000)
			{
				// Prevent corruption
				ImageListDragShowNoLock lockDrag;
				
				// Never toggle the root item - it has no button.
				if(hTarget != GetRootItem())
					Expand(hTarget, TVE_TOGGLE);
				
				// Don't re-collapse unless the user waits another period of x...
				dwDragHoverAcquire = GetTickCount() + 2000;
			}
		}
	}
	else
	{
		RECT rect;
		GetClientRect(&rect);

		int iMaxV = GetScrollLimit(m_hWnd, SB_VERT);
		int iPosV = GetScrollPos  (SB_VERT);

		// up
		if((point.y < rect.top -10) && iPosV)
		{
			HTREEITEM hPrev = GetPrevVisibleItem(GetFirstVisibleItem());
			ImageListDragShowNoLock lockDrag;
			
			EnsureVisible(hPrev);
		}

		// down
		if((point.y > (rect.bottom + 10)) && (iPosV != iMaxV))
		{
			UINT Nb = GetVisibleCount();
			if(Nb != -1)
			{
				HTREEITEM hNext = GetFirstVisibleItem();
				for(UINT i = 0; i < Nb; i++)
					hNext = GetNextVisibleItem(hNext);
				
				ImageListDragShowNoLock lockDrag;
				
				EnsureVisible(hNext);
			}
		}

		int iPosH = GetScrollPos  (SB_HORZ);
		int iMaxH = GetScrollLimit(m_hWnd, SB_HORZ);

		// left
		if((point.x < rect.left) && iPosH)
		{
			ImageListDragShowNoLock lockDrag;

			SendMessage(WM_HSCROLL, SB_LINELEFT);
		}

		// right
		if((point.x > rect.right) && (iPosH != iMaxH))
		{
			ImageListDragShowNoLock lockDrag;
			
			SendMessage(WM_HSCROLL, SB_LINERIGHT);
		}
	}

	return 0;
}

LRESULT CProjectTreeCtrl::OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = FALSE;

	if(m_pDropTarget != NULL)
	{
		HRESULT hr = RevokeDragDrop(m_hWnd);
		ATLASSERT(SUCCEEDED(hr));
	}

	return 0;
}

LRESULT CProjectTreeCtrl::OnKeyDown(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	if(wParam == VK_DELETE)
	{
		handleRemove();
	}
	else bHandled = FALSE;

	return 0;
}

LRESULT CProjectTreeCtrl::OnContextMenu(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	// If this is from a keyboard press...
	if(GET_X_LPARAM(lParam) == -1 && GET_Y_LPARAM(lParam) == -1)
	{
		CRect rc;
		GetItemRect(hLastItem, &rc, TRUE);
		CPoint pt(rc.right, rc.top);
		ClientToScreen(&pt);
		doContextMenu(&pt);
	}
	else
	{
		CPoint pt(GetMessagePos());
		handleRightClick(&pt);
	}

	return 0;
}

HRESULT CProjectTreeCtrl::OnDragEnter(LPDATAOBJECT pDataObject, DWORD /*dwKeyState*/, POINTL pt, LPDWORD pdwEffect)
{
	IEnumFORMATETC* pFormats = NULL;

	*pdwEffect = DROPEFFECT_NONE;

	if( !SUCCEEDED(pDataObject->EnumFormatEtc( DATADIR_GET, &pFormats )) )
		return E_FAIL;

	FORMATETC etcDetails[3];

	ULONG fetched = 0;
	bool found = false;
	
	do
	{
		pFormats->Next( 3, &etcDetails[0], &fetched);

		for(unsigned int i = 0; i < fetched; i++)
		{
			if(etcDetails[i].cfFormat == CF_HDROP)
			{
				found = true;
				break;
			}
		}
		
	} while(fetched > 0 && !found);

	pFormats->Release();

	if(found)
	{
		CPoint pt2(pt.x, pt.y);
		ScreenToClient(&pt2);

		TVHITTESTINFO hti;
		memset(&hti, 0, sizeof(TVHITTESTINFO));
		hti.pt.x = pt2.x;
		hti.pt.y = pt2.y;
		TreeView_HitTest(m_hWnd, &hti);

		if(hti.flags != TVHT_NOWHERE)
		{
			*pdwEffect = DROPEFFECT_LINK;
		}
	}
	
	return S_OK;
}


HRESULT CProjectTreeCtrl::OnDragOver(DWORD /*dwKeyState*/, POINTL pt, LPDWORD pdwEffect)
{
	CPoint pt2(pt.x, pt.y);
	ScreenToClient(&pt2);

	TVHITTESTINFO hti;
	memset(&hti, 0, sizeof(TVHITTESTINFO));
	hti.pt.x = pt2.x;
	hti.pt.y = pt2.y;
	TreeView_HitTest(m_hWnd, &hti);
	

	if(hti.flags != TVHT_NOWHERE)
	{
		*pdwEffect = DROPEFFECT_LINK;
		TreeView_SelectDropTarget(m_hWnd, hti.hItem);
	}
	else
		*pdwEffect = DROPEFFECT_NONE;

	return S_OK;
}

HRESULT CProjectTreeCtrl::OnDragLeave(void)
{
	TreeView_SelectDropTarget(m_hWnd, NULL);
	return S_OK;
}


HRESULT CProjectTreeCtrl::OnDrop(LPDATAOBJECT pDataObject, DWORD /*dwKeyState*/, POINTL pt, LPDWORD pdwEffect)
{
	*pdwEffect = DROPEFFECT_NONE;

	CPoint pt2(pt.x, pt.y);
	ScreenToClient(&pt2);

	TVHITTESTINFO hti;
	memset(&hti, 0, sizeof(TVHITTESTINFO));
	hti.pt.x = pt2.x;
	hti.pt.y = pt2.y;
	TreeView_HitTest(m_hWnd, &hti);

	HRESULT hret = S_OK;

	if(hti.flags != TVHT_NOWHERE && hti.hItem != NULL)
	{
		ProjectType* ptype = reinterpret_cast<ProjectType*>( GetItemData(hti.hItem) );
		if(ptype != NULL && (ptype->GetType() == ptFolder || ptype->GetType() == ptProject))
		{
			FORMATETC fmtetc;
			fmtetc.cfFormat = CF_HDROP;
			fmtetc.ptd = NULL;
			fmtetc.dwAspect = DVASPECT_CONTENT;
			fmtetc.lindex = -1;
			fmtetc.tymed = TYMED_HGLOBAL;

			// get the CF_HDROP data from drag source
			STGMEDIUM medium;
			HRESULT hr = pDataObject->GetData(&fmtetc, &medium);

			if (!FAILED(hr))
			{
				// call the helper routine which will notify the Form
				// of the drop
				handleDrop((HDROP)medium.hGlobal, hti.hItem, static_cast<Projects::Folder*>( ptype ));

				if (medium.pUnkForRelease)
					ReleaseStgMedium(&medium);
				else
					GlobalFree(medium.hGlobal);

				*pdwEffect = DROPEFFECT_LINK;
			}
			else
				hret = hr;
		}		
	}

	TreeView_SelectDropTarget(m_hWnd, NULL);

	return hret;
}

void CProjectTreeCtrl::handleDrop(HDROP hDrop, HTREEITEM hDropItem, Projects::Folder* pFolder)
{
	TCHAR	buf[MAX_PATH+1];
	
	int files = ::DragQueryFile(hDrop, 0xFFFFFFFF, NULL, 0);
	for(int i = 0; i < files; i++)
	{
		::DragQueryFile(hDrop, i, buf, MAX_PATH);
	
		if(::IsDirectory(buf))
		{
			pFolder->AddFolder(buf, _T("*.*"), true);
		}
		else
		{
			pFolder->AddFile(buf);
		}
	}
}

/**
 * See if we can perform a drag->drop operation given the current
 * selection.
 */
bool CProjectTreeCtrl::canDrag()
{
	ProjectType* ptSelItem;
	
	// We Check:
	// 1) If the user is trying to drag projects and non-projects.
	// 2) If the user is trying to drag a project group.
	bool hasProject = false;
	bool hasNonProject = false;
	bool hasWorkspace = false;

	std::list<HTREEITEM>::const_iterator i;

	for(i = dropSelectedItems.begin(); 
		(i != dropSelectedItems.end()) && (!hasProject || !hasNonProject);
		++i)
	{
		ptSelItem = reinterpret_cast<ProjectType*>( GetItemData((*i)) );
		switch(ptSelItem->GetType())
		{
		case ptProject:
			hasProject = true;
			break;

		case ptWorkspace:
			hasWorkspace = true;
			break;

		default:
			hasNonProject = true;
			break;
		}
	}

	// either way is fine, but not both.
	if(hasProject && hasNonProject)
	{
		g_Context.m_frame->SetStatusText(_T("Cannot drag projects and non-projects at the same time."));
		return false;
	}
	else if(hasWorkspace)
	{
		g_Context.m_frame->SetStatusText(_T("Cannot drag project groups."));
		return false;
	}

	return true;
}

/**
 * See if we can drop the current drag->drop selection on the item
 * the user is hovering over.
 */
bool CProjectTreeCtrl::canDrop()
{
	//hDropTargetItem is the one the mouse is over.
	if(hDropTargetItem == NULL)
		return false;

	//1) Check if the item is selected - can't drop an item on itself.
	if( GetItemState(hDropTargetItem, TVIS_SELECTED) == TVIS_SELECTED )
		return false;

	ProjectType* ptype = reinterpret_cast<ProjectType*>( GetItemData(hDropTargetItem) );

	if(ptype == NULL)
		return false;

	//2) Can't drop an item on itself.
	if(dropSelectionContainsItem(hDropTargetItem))
		return false;

	//3) check that we're not trying to drop a parent.
	if(dropSelectionContainsParent(hDropTargetItem))
	{
		g_Context.m_frame->SetStatusText(_T("Cannot drop a parent onto a child."));
		return false;
	}

	std::list<HTREEITEM>::iterator i;
	
	//4) perform type-related checking...
	switch(ptype->GetType())
	{
		case ptWorkspace:
			return false; // TODO
			break;

		//4.2) Now check if the target is a project that it's only a project
		//   that's being dropped.
		case ptProject:
			{
				return true;
			}
			break;

		//4.3) If we're dropping onto a folder, then we can drop folders and files...
		case ptFolder:
			{
				ProjectType* ptSelItem;
				for(i = dropSelectedItems.begin(); i != dropSelectedItems.end(); ++i)
				{
					ptSelItem = reinterpret_cast<ProjectType*>( GetItemData((*i)) );
					if(ptSelItem->GetType() != ptFile &&
						ptSelItem->GetType() != ptFolder)
						return false;
				}
			}
			break;

		//4.4) If it's a file, check that it's only files being dropped.
		//   Also, only support re-ordering files in the same folder.
		// TODO: In fact, for now we don't support re-ordering...
		case ptFile:
			{
				/*ProjectType* ptSelItem;
				HTREEITEM parent = GetParentItem(hDropTargetItem);
				for(i = dropSelectedItems.begin(); i != dropSelectedItems.end(); ++i)
				{
					ptSelItem = reinterpret_cast<ProjectType*>( GetItemData((*i)) );
					if(ptSelItem->GetType() != ptFile)
						return false;
					if( GetParentItem((*i)) != parent )
						return false;
				}*/
				return false;
			}
			break;
	}

	return true;
}

bool CProjectTreeCtrl::dropSelectionContainsItem(HTREEITEM item)
{
	std::list<HTREEITEM>::const_iterator i;
	for(i = dropSelectedItems.begin(); i != dropSelectedItems.end(); i++)
	{
		if( (*i) == item )
			return true;
	}

	return false;
}

bool CProjectTreeCtrl::dropSelectionContainsParent(HTREEITEM item)
{
	HTREEITEM parent = GetParentItem(item);
	while(parent)
	{
		if(dropSelectionContainsItem(parent))
			return true;
		parent = GetParentItem(parent);
	}

	return false;
}

bool CProjectTreeCtrl::handleDrop()
{
	//1) Move the individual file items first.
	//2) Move the folders after.
	// - this makes sure we don't try to move subitems of folders once 
	//   they've already been moved thus invalidating the HTREEITEMs.

	ProjectType* pDropTargetType = reinterpret_cast<ProjectType*>( GetItemData(hDropTargetItem) );

	switch(pDropTargetType->GetType())
	{
		case ptWorkspace:
			break;
		
		case ptProject:
			{
				Project* target = static_cast<Project*>( pDropTargetType );
				return handleProjectDrop(target);
			}
			break;

		case ptFolder:
			{
				Projects::Folder* target = static_cast<Projects::Folder*>( pDropTargetType );
				return handleFolderDrop(target);
			}
			break;

		case ptFile:
			break;
	}

	return false;
}

/**
 * Handles a drag->drop for the specific case of items dropped on
 * a project.
 *
 * If the items dropped are not projects, then we call through to
 * handleFolderDrop.
 * 
 * @param target The project the items were dropped on.
 */
bool CProjectTreeCtrl::handleProjectDrop(Projects::Project* target)
{
	std::list<HTREEITEM>::const_iterator i;
	ProjectType* ptSelItem;
	
	bool folderDrop = false;

	// If there're any non-project items in the selection, then we do this
	// as a folder drop...
	for(i = dropSelectedItems.begin(); i != dropSelectedItems.end(); ++i)
	{
		ptSelItem = reinterpret_cast<ProjectType*>( GetItemData((*i)) );
		if(ptSelItem->GetType() != ptProject)
		{
			folderDrop = true;
			break;
		}
	}

	Projects::Project* pProject;

	if(folderDrop)
	{
		handleFolderDrop(target);
	}
	else
	{
		PROJECT_LIST projects;
		
		// We move all project items to after the item that was selected.
		// First remove all the projects from the tree...
		for(i = dropSelectedItems.begin(); i != dropSelectedItems.end(); ++i)
		{
			pProject = reinterpret_cast<Project*>( GetItemData((*i)) );
			projects.insert(projects.end(), pProject);
			DeleteItem( (*i) );
		}

		Project* pLast = target;

		// Then re-add them, and move them in the projects list at the same time.
		for(PROJECT_LIST::iterator j = projects.begin(); j != projects.end(); ++j)
		{
			workspace->MoveProject((*j), pLast);
			pLast = (*j);
		}
	}

	return true;
}

/**
 * Handles a drag->drop for the specific case of items dropped on
 * a folder.
 * 
 * @param target The folder the items were dropped on.
 */
bool CProjectTreeCtrl::handleFolderDrop(Projects::Folder* target)
{
	std::list<HTREEITEM> queue;
	ProjectType* ptSelItem;
	std::list<HTREEITEM>::const_iterator i;
	for(i = dropSelectedItems.begin(); i != dropSelectedItems.end(); ++i)
	{
		ptSelItem = reinterpret_cast<ProjectType*>( GetItemData((*i)) );
		
		if(ptSelItem->GetType() == ptFile)
		{
			// Move the file object...
			File* pTheFile = static_cast<File*>( ptSelItem );
			Projects::Folder::MoveFile(pTheFile, target);
		}
		else if(ptSelItem->GetType() == ptFolder)
		{
			// queue folders for moving afterwards...
			queue.insert(queue.begin(), (*i));
		}
	}

	FOLDER_LIST folders;
	ProjectViewState viewState;

	// Now move any queued folders...
	for(i = queue.begin(); i != queue.end(); i++)
	{
		ptSelItem = reinterpret_cast<ProjectType*>( GetItemData((*i)) );
		if(ptSelItem->GetType() == ptFolder)
		{
			// Move the folder object...
			Projects::Folder* pFolder = static_cast<Projects::Folder*>( ptSelItem );
			Projects::Folder::MoveChild(pFolder, target);

			// Move the tree item(s)...
			DeleteItem( (*i) );
		}
	}

	Expand( hDropTargetItem, TVE_EXPAND );

	return true;
}

/**
 * Clean up after a drag->drop operation.
 */
void CProjectTreeCtrl::handleEndDrag()
{
	// Stop the imagelist drag thing...
	ImageList_DragLeave(m_hWnd);
	ImageList_EndDrag();
    
	// Release the mouse capture.
	ReleaseCapture();

	// Kill the drag timer.
	KillTimer(dragTimer);
	
	// Reset the mouse cursor.
	::SetCursor( ::LoadCursor(NULL, IDC_ARROW) );

	// Clear the drop target selection.
	TreeView_SelectDropTarget(m_hWnd, NULL);

	// Destroy the imagelist used for dragging.
	ImageList_Destroy(hDragImageList);

	dragging = false;
}

LRESULT CProjectTreeCtrl::handleSystemContextMenuMessage(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam, LRESULT& lResult)
{
	return m_explorerMenu->ProcessWindowMessage(hWnd, uMsg, wParam, lParam, lResult);
}

int CProjectTreeCtrl::CompareWorker(LPARAM lParam1, LPARAM lParam2, LPARAM caseSensitive, bool sortAll)
{
	ProjectType* pt1 = reinterpret_cast<ProjectType*>(lParam1);
	ProjectType* pt2 = reinterpret_cast<ProjectType*>(lParam2);

	PROJECT_TYPE t1 = pt1->GetType();
	PROJECT_TYPE t2 = pt2->GetType();

	if(t1 == ptFolder && t2 == ptFile)
	{
		return -1;
	}
	else if(t1 == ptFile && t2 == ptFolder)
	{
		return 1;
	}
	else if(t1 == ptFile && t2 == ptFile)
	{
		File* pF1 = reinterpret_cast<File*>(pt1);
		File* pF2 = reinterpret_cast<File*>(pt2);
		
		if(caseSensitive)
			return _tcscmp(pF1->GetDisplayName(), pF2->GetDisplayName());
		else
			return _tcsicmp(pF1->GetDisplayName(), pF2->GetDisplayName());
	}
	else if(sortAll && t1 == ptFolder && t2 == ptFolder)
	{
		Projects::Folder* pF1 = reinterpret_cast<Projects::Folder*>(pt1);
		Projects::Folder* pF2 = reinterpret_cast<Projects::Folder*>(pt2);

		if(caseSensitive)
			return _tcscmp(pF1->GetName(), pF2->GetName());
		else
			return _tcsicmp(pF1->GetName(), pF2->GetName());
	}

	return 0;
}

int CALLBACK CProjectTreeCtrl::CompareItem(LPARAM lParam1, LPARAM lParam2, LPARAM caseSensitive)
{
	return CompareWorker(lParam1, lParam2, caseSensitive, false);
}

int CALLBACK CProjectTreeCtrl::CompareItemSortAll(LPARAM lParam1, LPARAM lParam2, LPARAM caseSensitive)
{
	return CompareWorker(lParam1, lParam2, caseSensitive, true);
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
	m_hWndClient = m_view.Create(m_hWnd, rc, _T("ProjectTree"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | TVS_HASBUTTONS | TVS_HASLINES | TVS_EDITLABELS | TVS_SHOWSELALWAYS, 0, 100);

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
	//Hide();

	return 0;
}

LRESULT CProjectDocker::OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{

	bHandled = FALSE;

	return 0;
}

LRESULT CProjectDocker::OnGetMinMaxInfo(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	LPMINMAXINFO mmi = reinterpret_cast<LPMINMAXINFO>(lParam);
	mmi->ptMinTrackSize.x = 80;
	mmi->ptMinTrackSize.y = 100;
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
	if(pnmh->code == NM_DBLCLK || pnmh->code == NM_RETURN)
	{
		File* file = m_view.GetSelectedFile();
		if(file != NULL)
		{
			if( !g_Context.m_frame->CheckAlreadyOpen(file->GetFileName(), eSwitch) )
			{
				g_Context.m_frame->Open(file->GetFileName(), true);
				HWND hWndEditor = GetCurrentEditor();
				::PostMessage(hWndEditor, WM_SETFOCUS, NULL, NULL);
			}
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