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

using namespace Projects;

//////////////////////////////////////////////////////////////////////////////
// CProjectTreeCtrl
//////////////////////////////////////////////////////////////////////////////

CProjectTreeCtrl::CProjectTreeCtrl()
{
	lastFile = NULL;
}

void CProjectTreeCtrl::SetWorkspace(Projects::Workspace* ws)
{
	workspace = ws;

	clearTree();
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
	HTREEITEM hTopItem = InsertItem( workspace->GetName(), 0, 0, NULL, NULL );
	SetItemData(hTopItem, reinterpret_cast<DWORD_PTR>( workspace ));
	const PROJECT_LIST& projects = workspace->GetProjects();

	for(PROJECT_LIST::const_iterator i = projects.begin(); i != projects.end(); ++i)
	{
		buildProject(hTopItem, (*i));
	}

	Expand(hTopItem);
}

void CProjectTreeCtrl::buildProject(HTREEITEM hParentNode, Projects::Project* pj)
{
	HTREEITEM hProject = InsertItem( pj->GetName(), 0, 0, hParentNode, NULL );
	ProjectType* pPT = static_cast<ProjectType*>(pj);
	SetItemData(hProject, reinterpret_cast<DWORD_PTR>( pPT ));

	if(pj->Exists())
	{
		const FOLDER_LIST& folders = pj->GetFolders();
		if( folders.size() > 0 )
			buildFolders(hProject, folders);
	}
	else
	{
		HTREEITEM hni = InsertItem( _T("Could not load project..."), 0, 0, hProject, NULL );
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

		Expand(hFolder);
	}

	return hFolder;
}

HTREEITEM CProjectTreeCtrl::buildFiles(HTREEITEM hParentNode, HTREEITEM hInsertAfter, const FILE_LIST& files)
{
	HTREEITEM hFile = hInsertAfter;

	for(FILE_LIST::const_iterator i = files.begin(); i != files.end(); ++i)
	{
		hFile = InsertItem( (*i)->GetDisplayName(), 0, 0, hParentNode, hFile );
		SetItemData(hFile, reinterpret_cast<DWORD_PTR>( (*i) ));
	}

	return hFile;
}

void CProjectTreeCtrl::clearTree()
{

}

LRESULT CProjectTreeCtrl::OnSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMTREEVIEW n = (LPNMTREEVIEW)pnmh;

	ProjectType* pt = reinterpret_cast<ProjectType*>( GetItemData( n->itemNew.hItem ) );
	if(!pt)
		return 0;

	switch( pt->GetType() )
	{
	case ptFile:
		{
			File* file = static_cast<File*>( pt );
			tstring s(_T("Project file: "));
			s += file->GetFileName();
			g_Context.m_frame->SetStatusText(s.c_str());
		}
		break;

	case ptFolder:
		{
			Projects::Folder* folder = static_cast<Projects::Folder*>( pt );
			tstring s(_T("Folder root: "));
			s += folder->GetBasePath();
			g_Context.m_frame->SetStatusText(s.c_str());
		}
		break;

	case ptProject:
		{
		g_Context.m_frame->SetStatusText(_T("Project selected."));
		//Project* pProject = static_cast<Project*>( pt );
		}
		break;

	case ptWorkspace:
		g_Context.m_frame->SetStatusText(_T("Workspace selected."));
		break;
	}

	return 0;
}

LRESULT CProjectTreeCtrl::OnRightClick(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	TV_HITTESTINFO	tvhti;
	memset(&tvhti, 0, sizeof(TV_HITTESTINFO));

	CPoint pt(GetMessagePos());
	CPoint pt2(pt);
	
	if(pt.x == -1)
		return 0;
		
	ScreenToClient(&pt2);

	tvhti.pt = pt2;
    HitTest(&tvhti);

	lastFile = NULL;

	if(tvhti.hItem != NULL)
	{
		if (tvhti.flags & (TVHT_ONITEMLABEL|TVHT_ONITEMICON))
		{
			ProjectType* ptype = reinterpret_cast<ProjectType*>( GetItemData(tvhti.hItem) );
			switch(ptype->GetType())
			{
				case ptFile:
				{
					lastFile = static_cast<File*>( ptype );

					CSPopupMenu popup(IDR_POPUP_PROJECTFILE);

					CMenuItemInfo mii;
					mii.fMask = MIIM_STATE;
					mii.fState = MFS_DEFAULT;
					
					///@todo This doesn't work, but I'll leave it in to remind me to try
					// and fix it sometime. Stupid menus.
					::SetMenuItemInfo(popup, ID_PROJECT_OPEN, FALSE, &mii);
					
					g_Context.m_frame->TrackPopupMenu(popup, 0, pt.x, pt.y, NULL, m_hWnd);
				}
				break;
			}
		}
	}

	return 0;
}

LRESULT CProjectTreeCtrl::OnOpenFile(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(lastFile != NULL)
	{
		if( !g_Context.m_frame->CheckAlreadyOpen(lastFile->GetFileName(), eSwitch) )
			g_Context.m_frame->OpenFile(lastFile->GetFileName(), true);
	}

	return 0;
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
	//HICON hIconSmall = (HICON)::LoadImage(_Module.GetResourceInstance(), MAKEINTRESOURCE(m_dwIcon), 
	//		IMAGE_ICON, ::GetSystemMetrics(SM_CXSMICON), ::GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);
	//SetIcon(hIconSmall, FALSE);

	RECT rc;
	GetClientRect(&rc);
	m_view.Create(m_hWnd, rc, _T("ProjectTree"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | TVS_HASBUTTONS | TVS_HASLINES, 0, 100);

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

void CProjectDocker::SetWorkspace(Projects::Workspace* ws)
{
	workspace = ws;

	m_view.SetWorkspace(ws);
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
				g_Context.m_frame->OpenFile(file->GetFileName(), true);
		}
	}
	else
		bHandled = false;

	return 0;
}