#if !defined(AFX_SHELLCTRLS_H__20010531_6E5C_D354_4C09_0080AD509054__INCLUDED_)
#define AFX_SHELLCTRLS_H__20010531_6E5C_D354_4C09_0080AD509054__INCLUDED_

#pragma once

/////////////////////////////////////////////////////////////////////////////
// CShellBaseCtrl - A few controls that display shell data
//
// Written by Bjarke Viksoe (bjarke@viksoe.dk)
// Copyright (c) 2001 Bjarke Viksoe.
//
// Thanks to Anatoly Ivasyuk for adding sorting to the controls.
// Idea from a WTL Explorer sample by Leon Finker.
//
// This code may be used in compiled form in any way you desire. This
// source file may be redistributed by any means PROVIDING it is 
// not sold for profit without the authors written consent, and 
// providing that this notice and the authors name is included. 
//
// This file is provided "as is" with no expressed or implied warranty.
// The author accepts no liability if it causes any damage to you or your
// computer whatsoever. It's free, so don't hassle me about it.
//
// Beware of bugs.
//

#ifndef __cplusplus
  #error ATL requires C++ compilation (use a .cpp suffix)
#endif

#ifndef __ATLAPP_H__
  #error ShellCtrls.h requires atlapp.h to be included first
#endif

#ifndef __ATLCTRLS_H__
  #error ShellCtrls.h requires atlctrls.h to be included first
#endif

#ifndef __ATLSHELLEXT_H__
  #error ShellCtrls.h requires atlshellext.h to be included first
#endif

#if (_WIN32_IE < 0x0400)
  #error ShellCtrls.h requires _WIN32_IE >= 0x0400
#endif

#include <shlobj.h>


// The PARAM data of all controls
typedef struct {
   CComPtr<IShellFolder> spFolder;
   CPidl pidlFull;
   CPidl pidlNode;
   DWORD dwAttribs;
} SHELLITEMINFO, *PSHELLITEMINFO;

// Shell styles for all controls
#define SCT_EX_NOFOLDERS      0x00000001
#define SCT_EX_NOFILES        0x00000002
#define SCT_EX_SHOWHIDDEN     0x00000004
#define SCT_EX_NOREADONLY     0x00000008
#define SCT_EX_LOCALCOMPUTER  0x00000010
#define SCT_EX_FILESYSTEMONLY 0x00000020
#define SCT_EX_NOROOT         0x00000040


/////////////////////////////////////////////////////////////////////////////
// Misc Shell methods

inline BOOL AtlGetFilePidl(LPCTSTR pstrFileName, LPITEMIDLIST* pidl)
{
   ATLASSERT(!::IsBadStringPtr(pstrFileName, MAX_PATH));
   ATLASSERT(pidl);
   *pidl = NULL;
   // Make sure the file name is fully qualified and in Unicode format.
   TCHAR szFileName[MAX_PATH];
   ::GetFullPathName(pstrFileName, sizeof(szFileName)/sizeof(TCHAR), szFileName, NULL);
   USES_CONVERSION;
   LPOLESTR pwchPath = const_cast<LPOLESTR>(T2COLE(pstrFileName));
   // Convert the path name into a PIDL relative to the desktop
   CComPtr<IShellFolder> spFolder;
   if( FAILED( ::SHGetDesktopFolder(&spFolder) ) ) return FALSE;
   ULONG ulAttr = 0;
   if( FAILED(spFolder->ParseDisplayName(NULL, NULL, pwchPath, NULL, pidl, &ulAttr)) ) return FALSE;   
   return TRUE;
};

inline BOOL AtlGetFilePidl2(LPCTSTR pstrFileName, LPITEMIDLIST* pidl)
{
	CT2CW wPath(pstrFileName);
	DWORD rgfInOut = 0;

	return SUCCEEDED(SHILCreateFromPath(wPath, pidl, &rgfInOut));
}

inline BOOL AtlGetShellPidl(LPCITEMIDLIST pidl, IShellFolder** ppFolder, LPITEMIDLIST* pidlRel)
{
   ATLASSERT(pidl);
   ATLASSERT(ppFolder);
   ATLASSERT(pidlRel);

   *ppFolder = NULL;
   *pidlRel = NULL;

   // Get the desktop folder as a starting point
   CComPtr<IShellFolder> spFolder;
   if( FAILED( ::SHGetDesktopFolder(&spFolder) ) ) return FALSE;

   CPidl pidlMain;
   pidlMain.Copy(pidl);

   // Traverse each PIDL item and create a new IShellFolder for each of
   // them. Eventually we get to the last IShellFolder object and is left
   // with a simple (as opposed to complex) PIDL.
   int nCount = pidlMain.GetCount();
   while( --nCount > 0 ) {
      // Get the next PIDL entry
      CPidl pidlNext;
      pidlNext.Attach( pidlMain.CopyFirstItem() );
      if( pidlNext.IsEmpty() ) return FALSE;
      // Bind to the folder specified in the new item ID list.
      CComPtr<IShellFolder> spNextFolder;
      if( FAILED( spFolder->BindToObject(pidlNext, NULL, IID_IShellFolder, (LPVOID*) &spNextFolder)) ) return FALSE;
      spFolder = spNextFolder;
      // Strip first PIDL entry and copy remaining
      CPidl temp;
      temp.Copy( pidlMain.GetNextItem() );
      pidlMain.Attach(temp.Detach());
   }

   *ppFolder = spFolder.Detach();
   *pidlRel = pidlMain.Detach();
   return TRUE;
};

/**
 * Get a valid drive letter to use for image retrieval hack
 */
inline TCHAR AtlGetFirstDriveLetter()
{
	DWORD logDrives = ::GetLogicalDrives();
	
	// Skip 'A' and 'B'
	logDrives >>=2;

	TCHAR drive = 'C';
	while(drive < _T('Z'))
	{
		if (logDrives & 1)
		{
			tstring path;
			path += drive;
			path += _T(":\\");
			if (::GetDriveType(path.c_str()) == DRIVE_FIXED)
			{
				break;
			}
		}
		
		drive++;
		logDrives >>=1;
	}
	
	return drive;
}

/////////////////////////////////////////////////////////////////////////////
// CShellBaseCtrl - Common shell control methods and properties

template< class T, typename TItem=int >
class CShellBaseCtrl
{
protected:
   DWORD m_dwShellStyle;

public:
   CShellBaseCtrl() : m_dwShellStyle(0UL)
   {
   }

   void SetShellStyle(DWORD dwStyle)
   {
      m_dwShellStyle = dwStyle;
   }

   DWORD GetShellStyle() const
   {
      return m_dwShellStyle;
   }

   BOOL Populate(int csidl = CSIDL_DESKTOP)
   {
      CPidl pidl;
      if( FAILED( ::SHGetSpecialFolderLocation(NULL, csidl, &pidl) ) ) return FALSE;
      CComPtr<IShellFolder> spDesktop;
      if( FAILED( ::SHGetDesktopFolder(&spDesktop) ) ) return FALSE;
      return Populate(spDesktop, pidl, csidl == CSIDL_DESKTOP ? NULL : (LPCITEMIDLIST)pidl);
   }

   BOOL Populate(LPCTSTR pstrPath)
   {
      ATLASSERT(!::IsBadStringPtr(pstrPath,MAX_PATH));
      USES_CONVERSION;
      CComPtr<IShellFolder> spDesktop;
      if( FAILED( ::SHGetDesktopFolder(&spDesktop) ) ) return FALSE;
      CPidl pidl;
      DWORD dwAttribs = 0;
      DWORD dwEaten = 0;
      LPCOLESTR pwstrPath = T2COLE(pstrPath);
      if( FAILED( spDesktop->ParseDisplayName(NULL, NULL, const_cast<LPOLESTR>(pwstrPath), &dwEaten, &pidl, &dwAttribs) ) ) return FALSE;
      return Populate(pidl);
   }

   BOOL Populate(LPCITEMIDLIST pidl)
   {
      CComPtr<IShellFolder> spFolder;
      CPidl pidlItem;
      if( !AtlGetShellPidl(pidl, &spFolder, &pidlItem) ) return FALSE;
      return Populate(spFolder, pidl, pidlItem);
   }

   BOOL Populate(IShellFolder* pFolder, LPCITEMIDLIST pidlPath, LPCITEMIDLIST pidlNode)
   {
      ATLASSERT(pFolder);
      ATLASSERT(pidlPath);
      CComPtr<IShellFolder> spFolder;
      DWORD dwAttribs = SFGAO_FILESYSANCESTOR | SFGAO_HASSUBFOLDER;
      if( pFolder != NULL && !CPidl::PidlIsEmpty(pidlNode) ) {
         // Get the new IShellFolder object
         if( FAILED( pFolder->BindToObject(pidlNode, NULL, IID_IShellFolder, (LPVOID*)&spFolder) ) ) return 0;
         // Get this folder's attributes
         pFolder->GetAttributesOf(1, &pidlNode, &dwAttribs);
      }
      else {
         // Folder is Desktop
         if( FAILED( ::SHGetDesktopFolder(&spFolder) ) ) return FALSE;
         dwAttribs = SFGAO_HASSUBFOLDER;
      }
      T* pT = static_cast<T*>(this);
      return pT->_Populate(spFolder, pidlPath, dwAttribs);
   }

   BOOL GetItemPidl(TItem hItem, LPITEMIDLIST* pidl)
   {
      T* pT = static_cast<T*>(this);
      pT;
      ATLASSERT(::IsWindow(pT->m_hWnd));
      ATLASSERT(pidl);
      // NOTE: We can't really check the 'hItem' argument here because it may
      //       actually be 0 for some controls (i.e. ListView has 0 as index)
      //       so we need to rely on the argument being passed is a valid item!
      *pidl = NULL;
      DWORD_PTR lParam = pT->GetItemData(hItem);
      if( lParam == NULL || lParam == -1 ) return FALSE;
      PSHELLITEMINFO pItem = reinterpret_cast<PSHELLITEMINFO>(lParam);
      *pidl = CPidl::PidlCopy(pItem->pidlFull);
      return TRUE;
   }

   BOOL GetItemPath(TItem hItem, LPTSTR pstrPath)
   {
      ATLASSERT(!::IsBadWritePtr(pstrPath, MAX_PATH));
      pstrPath[0]=_T('\0');
      CPidl pidl;
      if( !GetItemPidl(hItem, &pidl) ) return FALSE;
      if( !::SHGetPathFromIDList(pidl, pstrPath) ) return FALSE;
      return TRUE;
   }

   // Returns TRUE if PIDL should be filtered out...
   BOOL _FilterItem(IShellFolder* pFolder, LPCITEMIDLIST pidl, DWORD& dwAttribs) const
   {
      ATLASSERT(pFolder);
      ATLASSERT(pidl);

      if( m_dwShellStyle == 0 && dwAttribs == 0 )
	  {
		  return FALSE;
	  }

      dwAttribs |= SFGAO_DISPLAYATTRMASK | SFGAO_FOLDER | SFGAO_FILESYSTEM | SFGAO_FILESYSANCESTOR;

      // Pre-check some attributes as some others cause trouble if these are present:
      DWORD dwRemovable = SFGAO_REMOVABLE | SFGAO_STREAM | SFGAO_FOLDER;
      pFolder->GetAttributesOf(1, &pidl, &dwRemovable);
      
	  // If this is a floppy, we remove ReadOnly checks as this causes disk activity
	  if ((dwRemovable & SFGAO_REMOVABLE) != 0)
	  {
		  dwAttribs &= ~SFGAO_READONLY;
	  }

	  // If this is a zip file, we don't want to look for sub folders - we don't support expansion
	  if ((dwRemovable & (SFGAO_STREAM | SFGAO_FOLDER)) == (SFGAO_STREAM | SFGAO_FOLDER))
	  {
		  dwAttribs &= ~SFGAO_HASSUBFOLDER;
	  }
      
	  pFolder->GetAttributesOf(1, &pidl, &dwAttribs);

      // Filter some items
      if( (m_dwShellStyle & SCT_EX_NOFOLDERS) != 0 && (dwAttribs & SFGAO_FOLDER) != 0 ) return TRUE;
      if( (m_dwShellStyle & SCT_EX_NOFILES) != 0 && ((dwAttribs & SFGAO_FOLDER) == 0) ) return TRUE;
      if( (m_dwShellStyle & SCT_EX_NOREADONLY) != 0 && (dwAttribs & SFGAO_READONLY) != 0 ) return TRUE;
      if( (m_dwShellStyle & SCT_EX_FILESYSTEMONLY) != 0 && ((dwAttribs & (SFGAO_FILESYSTEM | SFGAO_FILESYSANCESTOR)) == 0) ) return TRUE;

      return FALSE;
   }
};


/////////////////////////////////////////////////////////////////////////////
// CShellTreeCtrl

class CShellTreeCtrl :
   public CWindowImpl< CShellTreeCtrl, CTreeViewCtrl, CControlWinTraits >,
   public CShellBaseCtrl< CShellTreeCtrl, HTREEITEM >,
   public CThemeImpl< CShellTreeCtrl >
{
public:
   typedef CWindowImpl< CShellTreeCtrl, CTreeViewCtrl, CControlWinTraits > parentClass;

   BEGIN_MSG_MAP(CShellTreeCtrl)
      MESSAGE_HANDLER(WM_CREATE, OnCreate)
      REFLECTED_NOTIFY_CODE_HANDLER(TVN_GETDISPINFO, OnGetDispInfo)
      REFLECTED_NOTIFY_CODE_HANDLER(TVN_DELETEITEM, OnDeleteItem)
      REFLECTED_NOTIFY_CODE_HANDLER(TVN_ITEMEXPANDING, OnItemExpanding)
      DEFAULT_REFLECTION_HANDLER()
   END_MSG_MAP()

   // Operations

   BOOL SubclassWindow(HWND hWnd)
   {
      ATLASSERT(m_hWnd==NULL);
      ATLASSERT(::IsWindow(hWnd));
      BOOL bRet = parentClass::SubclassWindow(hWnd);
      if( bRet ) _Init();
      return bRet;
   }

   BOOL SelectPidl(LPCITEMIDLIST pidlTarget)
   {
      ATLASSERT(::IsWindow(m_hWnd));
      // BUG: Assumes tree control's root is desktop!
      CPidl pidlItem;
      HTREEITEM hItem = GetRootItem();
      ATLASSERT(hItem!=NULL);
      while( !CPidl::PidlIsEmpty(pidlTarget) ) {
         hItem = GetChildItem(hItem);
         pidlItem.Attach( CPidl::PidlCopyFirstItem(pidlTarget) );
         while( hItem != NULL ) {
            DWORD_PTR lParam = GetItemData(hItem);
            ATLASSERT(lParam>0);
            PSHELLITEMINFO pItem = reinterpret_cast<PSHELLITEMINFO>(lParam);
            if( pItem->spFolder->CompareIDs(0, pidlItem, pItem->pidlNode) == 0 ) {
               if( CPidl::PidlGetCount(pidlTarget)>1 ) Expand(hItem);
               break;
            }
            hItem = GetNextSiblingItem(hItem);
         }
         if( hItem == NULL ) return FALSE; // folder not found?
         pidlTarget = CPidl::PidlGetNextItem(pidlTarget);
      }
      if( hItem != NULL ) SelectItem(hItem);
      return TRUE;
   }

   // Implementation

   BOOL _Init()
   {
		// Try to get a Vista style treeview:
	  if (WTL::RunTimeHelper::IsVista()) 
         SetWindowTheme(L"explorer", NULL);

      // Extract the System Imagelist
      CImageList images;
      SHFILEINFO sfi;

	  TCHAR drive = AtlGetFirstDriveLetter();
      tstring path;
      path += drive;
      path += _T(":\\");

	  images = (HIMAGELIST) ::SHGetFileInfo( path.c_str(), 0, &sfi, sizeof(sfi), SHGFI_SYSICONINDEX | SHGFI_SMALLICON);
      SetImageList(images.Detach(), TVSIL_NORMAL);
      return TRUE;
   }

   BOOL _Populate(IShellFolder* pFolder, LPCITEMIDLIST pidlParent, DWORD dwAttribs)
   {
      ATLASSERT(::IsWindow(m_hWnd));
      ATLASSERT(pidlParent);
      DeleteAllItems();
      HTREEITEM hItem = _InsertItem(pFolder, pidlParent, NULL, dwAttribs, TVI_ROOT);
      SelectItem(hItem);
      Expand(hItem);
      return TRUE;
   }

   HTREEITEM _InsertItem(IShellFolder* pFolder, LPCITEMIDLIST pidlPath, LPCITEMIDLIST pidlNode,
                         DWORD dwAttribs, HTREEITEM hParentItem)
   {
      ATLASSERT(pFolder);
      ATLASSERT(pidlPath);

      // Create PARAM data
      PSHELLITEMINFO pItem(NULL);
      ATLTRY(pItem = new SHELLITEMINFO);
      ATLASSERT(pItem);
      pItem->pidlFull.Copy( pidlPath );
      pItem->pidlFull.Concatenate( pidlNode );
      pItem->pidlNode.Copy( pidlNode );
      pItem->spFolder = pFolder;
      pItem->dwAttribs = dwAttribs;

      // Insert tree item...
      TV_INSERTSTRUCT item = { 0 };
      item.hParent = hParentItem;
      item.hInsertAfter = TVI_LAST;
      item.item.mask = TVIF_TEXT | TVIF_IMAGE | TVIF_SELECTEDIMAGE | TVIF_PARAM | TVIF_CHILDREN;
      item.item.pszText = LPSTR_TEXTCALLBACK;
      item.item.iImage = item.item.iSelectedImage = I_IMAGECALLBACK;
      item.item.lParam= (LPARAM) pItem;

	  if ((m_dwShellStyle & SCT_EX_NOFILES) != 0)
	  {
         item.item.cChildren = (dwAttribs & (SFGAO_HASSUBFOLDER | SFGAO_CONTENTSMASK)) != 0 ? 1 : 0;
	  }
	  else
	  {
         // We can't tell from attributes if a folder contains any files, so just say all can be 
         // enumerated.
		 item.item.cChildren = 
			 // test for zip files first (folder and stream, we don't support expanding them)
			 (dwAttribs & (SFGAO_FOLDER | SFGAO_STREAM)) == (SFGAO_FOLDER | SFGAO_STREAM) ? 0 : 
			 // then test for anything expandable
			 (dwAttribs & (SFGAO_FOLDER | SFGAO_FILESYSANCESTOR | SFGAO_HASSUBFOLDER | SFGAO_CONTENTSMASK)) != 0 ? 1 : 0;
	  }

      if( (dwAttribs & SFGAO_SHARE) != 0 ) {
         item.item.mask |= TVIF_STATE;
         item.item.stateMask |= TVIS_OVERLAYMASK;
         item.item.state |= INDEXTOOVERLAYMASK(1);
      }
      if( (dwAttribs & SFGAO_LINK) != 0 ) {
         item.item.mask |= TVIF_STATE;
         item.item.stateMask |= TVIS_OVERLAYMASK;
         item.item.state |= INDEXTOOVERLAYMASK(2);
      }
      return InsertItem(&item);
   }

   // NOTE: Sorting added by Anatoly Ivasyuk.
   static int CALLBACK _SortFunc(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
   {
      IShellFolder* piSF = reinterpret_cast<IShellFolder*>(lParamSort);
      PSHELLITEMINFO pItem1 = reinterpret_cast<PSHELLITEMINFO>(lParam1);
      PSHELLITEMINFO pItem2 = reinterpret_cast<PSHELLITEMINFO>(lParam2);
      HRESULT Hr = piSF->CompareIDs(0, pItem1->pidlNode, pItem2->pidlNode);
      if( SUCCEEDED(Hr) ) return (SHORT) (Hr & SHCIDS_COLUMNMASK);
      return 0;
   }

   // Message Handlers

   LRESULT OnCreate(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
   {
      LRESULT lRes = DefWindowProc(uMsg, wParam, lParam);
      _Init();
      return lRes;
   }

   LRESULT OnDeleteItem(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
   {
      LPNMTREEVIEW pnmtv = (LPNMTREEVIEW) pnmh;
      PSHELLITEMINFO pItem = reinterpret_cast<PSHELLITEMINFO>(pnmtv->itemOld.lParam);
      ATLASSERT(pItem);
      ATLTRY(delete pItem);
      return 0;
   }

   LRESULT OnGetDispInfo(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
   {
      LPNMTVDISPINFO lpdi = (LPNMTVDISPINFO) pnmh;
      PSHELLITEMINFO pItem = reinterpret_cast<PSHELLITEMINFO>(lpdi->item.lParam);
      SHFILEINFO sfi;
      if( (lpdi->item.mask & TVIF_TEXT) != 0 ) {
         if( ::SHGetFileInfo((LPCTSTR)(LPCITEMIDLIST)pItem->pidlFull, 
                             0, &sfi, sizeof(sfi), 
                             SHGFI_PIDL | SHGFI_DISPLAYNAME) ) {
            ::lstrcpy(lpdi->item.pszText, sfi.szDisplayName);
         }
      }
      if( (lpdi->item.mask & TVIF_IMAGE) != 0 ) {
         if( ::SHGetFileInfo((LPCTSTR)(LPCITEMIDLIST)pItem->pidlFull, 
                             0, &sfi, sizeof(sfi), 
                             SHGFI_PIDL | SHGFI_SYSICONINDEX | SHGFI_SMALLICON | SHGFI_LINKOVERLAY) ) {
            lpdi->item.iImage = sfi.iIcon;
         }
      }
      if( (lpdi->item.mask & TVIF_SELECTEDIMAGE) != 0 ) {
         if( ::SHGetFileInfo((LPCTSTR)(LPCITEMIDLIST)pItem->pidlFull, 
                             0, &sfi, sizeof(sfi), 
                             SHGFI_PIDL | SHGFI_SYSICONINDEX | SHGFI_SMALLICON | SHGFI_OPENICON) ) {
            lpdi->item.iSelectedImage = sfi.iIcon;
         }
      }
      return 0;
   }

   LRESULT OnItemExpanding(int /*idCtrl*/, LPNMHDR pnmh, BOOL& bHandled)
   {
      LPNMTREEVIEW pnmtv = (LPNMTREEVIEW) pnmh;

      if( pnmtv->action == TVE_COLLAPSE ) {
         Expand(pnmtv->itemNew.hItem, TVE_COLLAPSE | TVE_COLLAPSERESET);
         return 0;
      }

#ifdef __ATLCTRLX_H__
      CWaitCursor cursor;
#endif

      PSHELLITEMINFO pFolderItem = reinterpret_cast<PSHELLITEMINFO>(pnmtv->itemNew.lParam);
      CComPtr<IShellFolder> spFolder;
      if( pFolderItem->pidlNode != NULL ) {
         if( FAILED(pFolderItem->spFolder->BindToObject(pFolderItem->pidlNode, NULL, IID_IShellFolder, (LPVOID*)&spFolder)) ) return FALSE;
      }
      else {
         spFolder = pFolderItem->spFolder;
      }

      // Add children
      CComPtr<IEnumIDList> spEnum;
      DWORD dwEnumFlags = SHCONTF_FOLDERS;
      if( (m_dwShellStyle & SCT_EX_SHOWHIDDEN) != 0 ) 
	  {
		  dwEnumFlags |= SHCONTF_INCLUDEHIDDEN;
	  }
	  
	  if ( (m_dwShellStyle & SCT_EX_NOFILES) == 0 )
	  {
		  dwEnumFlags |= SHCONTF_NONFOLDERS;
	  }

      if( SUCCEEDED(spFolder->EnumObjects(NULL, dwEnumFlags, &spEnum)) ) {
         CPidl pidl;
         DWORD  dwFetched;
         while( (spEnum->Next(1, &pidl, &dwFetched) == S_OK) && (dwFetched > 0) ) {
            // Get attributes and filter some items
            DWORD dwAttribs = SFGAO_DISPLAYATTRMASK | SFGAO_HASSUBFOLDER | SFGAO_STREAM;
            if( !_FilterItem(spFolder, pidl, dwAttribs) ) {
               _InsertItem(spFolder, pFolderItem->pidlFull, pidl, dwAttribs, pnmtv->itemNew.hItem);
            }
            pidl.Delete();
         }
      }

      // Sort children
      TVSORTCB tvscb;
      tvscb.hParent = pnmtv->itemNew.hItem;
      tvscb.lpfnCompare = _SortFunc;
      tvscb.lParam = (LPARAM) (IShellFolder*) spFolder;
      SortChildrenCB(&tvscb);

      bHandled = FALSE;
      return 0;
   }
};


/////////////////////////////////////////////////////////////////////////////
// CShellListCtrl

class CShellListCtrl :
   public CWindowImpl< CShellListCtrl, CListViewCtrl, CControlWinTraits >,
   public CShellBaseCtrl< CShellListCtrl >
{
public:
   typedef CWindowImpl< CShellListCtrl, CListViewCtrl, CControlWinTraits > parentClass;

   BEGIN_MSG_MAP(CShellListCtrl)
      MESSAGE_HANDLER(WM_CREATE, OnCreate)
      REFLECTED_NOTIFY_CODE_HANDLER(LVN_GETDISPINFO, OnGetDispInfo)
      REFLECTED_NOTIFY_CODE_HANDLER(LVN_DELETEITEM, OnDeleteItem)
      DEFAULT_REFLECTION_HANDLER()
   END_MSG_MAP()

   // Operations

   BOOL SubclassWindow(HWND hWnd)
   {
      ATLASSERT(m_hWnd==NULL);
      ATLASSERT(::IsWindow(hWnd));
      BOOL bRet = parentClass::SubclassWindow(hWnd);
      if( bRet ) _Init();
      return bRet;
   }

   void SortItems()
   {
      CListViewCtrl::SortItems(_SortFunc, NULL);
   }

   // Implementation

   BOOL _Init()
   {
      // We must not destroy system image list (Q192055)
      ModifyStyle(0, LVS_SHAREIMAGELISTS);
      // Extract the System Imagelists
      CImageList images;
      SHFILEINFO sfi;
      images = (HIMAGELIST)::SHGetFileInfo( _T("C:\\"), 0, &sfi, sizeof(sfi), SHGFI_SYSICONINDEX | SHGFI_SMALLICON);
      SetImageList(images.Detach(), LVSIL_SMALL);
      images = (HIMAGELIST)::SHGetFileInfo( _T("C:\\"), 0, &sfi, sizeof(sfi), SHGFI_SYSICONINDEX | SHGFI_LARGEICON);
      SetImageList(images.Detach(), LVSIL_NORMAL);
      return TRUE;
   }

   BOOL _Populate(IShellFolder* pFolder, LPCITEMIDLIST pidlPath, DWORD /*dwAttribs*/)
   {
      ATLASSERT(::IsWindow(m_hWnd));
      ATLASSERT(pFolder);
      ATLASSERT(pidlPath);

      SetRedraw(FALSE);
      DeleteAllItems();

      CComPtr<IEnumIDList> spEnum;
      DWORD dwEnumFlags = SHCONTF_FOLDERS | SHCONTF_NONFOLDERS;
      if( (m_dwShellStyle & SCT_EX_SHOWHIDDEN) != 0 ) dwEnumFlags |= SHCONTF_INCLUDEHIDDEN;
      if( SUCCEEDED(pFolder->EnumObjects(NULL, dwEnumFlags, &spEnum)) ) {
         CPidl pidl;
         DWORD dwFetched;
         while( (spEnum->Next(1, &pidl, &dwFetched) == S_OK) && (dwFetched > 0) ) {
            // Get attributes and filter some items
            DWORD dwAttribs = SFGAO_DISPLAYATTRMASK;
            if( !_FilterItem(pFolder, pidl, dwAttribs) ) {
               _InsertItem(pFolder, pidlPath, pidl, dwAttribs);            
            }
            pidl.Delete();
         }
      }

      SortItems();

      SetRedraw(TRUE);
      Invalidate();

      return TRUE;
   }

   int _InsertItem(IShellFolder* pFolder, LPCITEMIDLIST pidlPath, LPCITEMIDLIST pidlNode, DWORD dwAttribs)
   {
      ATLASSERT(pFolder);
      ATLASSERT(pidlPath);

      // Create PARAM data
      PSHELLITEMINFO pItem;
      ATLTRY(pItem = new SHELLITEMINFO);
      ATLASSERT(pItem);
      pItem->pidlFull.Copy( pidlPath );
      pItem->pidlFull.Concatenate( pidlNode );
      pItem->pidlNode.Copy( pidlNode );
      pItem->spFolder = pFolder;
      pItem->dwAttribs = dwAttribs;

      // Insert listview item
      LVITEM item = { 0 };
      item.iItem = GetItemCount();
      item.mask = LVIF_TEXT | LVIF_IMAGE | LVIF_PARAM;
      item.pszText = LPSTR_TEXTCALLBACK;
      item.iImage = I_IMAGECALLBACK;
      item.lParam = (LPARAM) pItem;
      if( (dwAttribs & SFGAO_SHARE) != 0 ) {
         item.mask |= LVIF_STATE;
         item.stateMask |= LVIS_OVERLAYMASK;
         item.state |= INDEXTOOVERLAYMASK(1);
      }
      if( (dwAttribs & SFGAO_LINK) != 0 ) {
         item.mask |= LVIF_STATE;
         item.stateMask |= LVIS_OVERLAYMASK;
         item.state |= INDEXTOOVERLAYMASK(2);
      }
      return InsertItem(&item);           
   }

   // NOTE: Sorting added by Anatoly Ivasyuk.
   static int CALLBACK _SortFunc(LPARAM lParam1, LPARAM lParam2, LPARAM /*lParamSort*/)
   {
      PSHELLITEMINFO pItem1 = reinterpret_cast<PSHELLITEMINFO>(lParam1);
      PSHELLITEMINFO pItem2 = reinterpret_cast<PSHELLITEMINFO>(lParam2);
      HRESULT Hr = pItem1->spFolder->CompareIDs(0, pItem1->pidlNode, pItem2->pidlNode);
      if( SUCCEEDED(Hr) ) return (SHORT) (Hr & SHCIDS_COLUMNMASK);
      return 0;
   }

   // Message Handlers

   LRESULT OnCreate(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
   {
      LRESULT lRes = DefWindowProc(uMsg, wParam, lParam);
      _Init();
      return lRes;
   }

   LRESULT OnDeleteItem(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
   {
      LPNMLISTVIEW pnmlv = (LPNMLISTVIEW) pnmh;
      PSHELLITEMINFO pItem = reinterpret_cast<PSHELLITEMINFO>(pnmlv->lParam);
      ATLASSERT(pItem);
      ATLTRY(delete pItem);
      return 0;
   }

   LRESULT OnGetDispInfo(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
   {
      NMLVDISPINFO* lpdi = (NMLVDISPINFO*) pnmh;
      PSHELLITEMINFO pItem = reinterpret_cast<PSHELLITEMINFO>(lpdi->item.lParam);
      SHFILEINFO sfi;
      if( (lpdi->item.mask & LVIF_TEXT) != 0 ) {
         if( ::SHGetFileInfo((LPCTSTR)(LPCITEMIDLIST)pItem->pidlFull, 
                             0, &sfi, sizeof(sfi), 
                             SHGFI_PIDL | SHGFI_DISPLAYNAME) ) {
            ::lstrcpy(lpdi->item.pszText, sfi.szDisplayName);
         }
      }
      if( (lpdi->item.mask & LVIF_IMAGE) != 0 ) {
         if( ::SHGetFileInfo((LPCTSTR)(LPCITEMIDLIST)pItem->pidlFull, 
                             0, &sfi, sizeof(sfi), 
                             SHGFI_PIDL | SHGFI_SYSICONINDEX | SHGFI_SMALLICON | SHGFI_LINKOVERLAY) ) {
            lpdi->item.iImage = sfi.iIcon;
         }
      }
      return 0;
   }
};


/////////////////////////////////////////////////////////////////////////////
// CShellComboCtrl

class CShellComboCtrl :
   public CWindowImpl< CShellComboCtrl, CComboBoxEx, CControlWinTraits >,
   public CShellBaseCtrl< CShellComboCtrl >
{
public:
   typedef CWindowImpl< CShellComboCtrl, CComboBoxEx, CControlWinTraits > parentClass;

   BEGIN_MSG_MAP(CShellComboCtrl)
      MESSAGE_HANDLER(WM_CREATE, OnCreate)
      REFLECTED_NOTIFY_CODE_HANDLER(CBEN_GETDISPINFO, OnGetDispInfo)
      REFLECTED_NOTIFY_CODE_HANDLER(CBEN_DELETEITEM, OnDeleteItem)
      DEFAULT_REFLECTION_HANDLER()
   END_MSG_MAP()

   // Operations

   BOOL SubclassWindow(HWND hWnd)
   {
      ATLASSERT(m_hWnd==NULL);
      ATLASSERT(::IsWindow(hWnd));
      BOOL bRet = parentClass::SubclassWindow(hWnd);
      if( bRet ) _Init();
      return bRet;
   }

   // Implementation

   BOOL _Init()
   {
      // Extract the System Imagelist
      SHFILEINFO sfi;
      CImageList images = (HIMAGELIST) ::SHGetFileInfo( _T("C:\\"), 0, &sfi, sizeof(sfi), SHGFI_SYSICONINDEX | SHGFI_SMALLICON);
      SetImageList(images.Detach());
      return TRUE;
   }

   BOOL _Populate(IShellFolder* pFolder, LPCITEMIDLIST pidlParent, DWORD dwAttribs)
   {
      ATLASSERT(::IsWindow(m_hWnd));
      ATLASSERT(pFolder);
      ATLASSERT(pidlParent);

      ResetContent();

      int iIndent = 0;
      if( (m_dwShellStyle & SCT_EX_NOROOT) == 0 ) {
         _InsertItem(pFolder, pidlParent, NULL, dwAttribs, iIndent++);
      }

      CComPtr<IEnumIDList> spEnum;
      DWORD dwEnumFlags = SHCONTF_FOLDERS | SHCONTF_NONFOLDERS;
      if( (m_dwShellStyle & SCT_EX_SHOWHIDDEN) != 0 ) dwEnumFlags |= SHCONTF_INCLUDEHIDDEN;
      if( SUCCEEDED(pFolder->EnumObjects(NULL, dwEnumFlags, &spEnum)) ) {
         CPidl pidl;
         DWORD dwFetched;
         while( (spEnum->Next(1, &pidl, &dwFetched) == S_OK) && (dwFetched > 0) ) {
            // Get attributes and filter some items
            DWORD dwAttribs = 0;
            if( !_FilterItem(pFolder, pidl, dwAttribs) ) {
               _InsertItem(pFolder, pidlParent, pidl, dwAttribs, iIndent);
            }
            pidl.Delete();
         }
      }

      SetCurSel(0);
      return TRUE;
   }

   int _InsertItem(IShellFolder* pFolder, LPCITEMIDLIST pidlPath, LPCITEMIDLIST pidlNode,
                   DWORD dwAttribs, int iIndent)
   {
      ATLASSERT(pFolder);
      ATLASSERT(pidlPath);

      // Create PARAM data
      PSHELLITEMINFO pItem;
      ATLTRY(pItem = new SHELLITEMINFO);
      ATLASSERT(pItem);
      pItem->pidlFull.Copy( pidlPath );
      pItem->pidlFull.Concatenate( pidlNode );
      pItem->pidlNode.Copy( pidlNode );
      pItem->spFolder = pFolder;
      pItem->dwAttribs = dwAttribs;

      // Insert combobox item
      COMBOBOXEXITEM item = { 0 };
      item.iItem = -1;
      item.mask = CBEIF_TEXT | CBEIF_IMAGE | CBEIF_SELECTEDIMAGE | CBEIF_INDENT | CBEIF_LPARAM;
      item.pszText = LPSTR_TEXTCALLBACK;
      item.iImage = item.iSelectedImage = I_IMAGECALLBACK;
      item.iIndent = iIndent;
      item.lParam = (LPARAM) pItem;
      return InsertItem(&item);
   }

   // Message Handlers

   LRESULT OnCreate(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
   {
      LRESULT lRes = DefWindowProc(uMsg, wParam, lParam);
      _Init();
      return lRes;
   }

   LRESULT OnDeleteItem(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
   {
      NMCOMBOBOXEX* pnmlv = (NMCOMBOBOXEX*) pnmh;
      PSHELLITEMINFO pItem = reinterpret_cast<PSHELLITEMINFO>(pnmlv->ceItem.lParam);
      ATLASSERT(pItem);
      ATLTRY(delete pItem);
      return 0;
   }

   LRESULT OnGetDispInfo(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
   {
      PNMCOMBOBOXEX lpdi = (PNMCOMBOBOXEX) pnmh;
      PSHELLITEMINFO pItem = reinterpret_cast<PSHELLITEMINFO>(lpdi->ceItem.lParam);
      SHFILEINFO sfi;
      if( (lpdi->ceItem.mask & CBEIF_TEXT) != 0 ) {
         if( ::SHGetFileInfo((LPCTSTR)(LPCITEMIDLIST)pItem->pidlFull, 
                             0, &sfi, sizeof(sfi), 
                             SHGFI_PIDL | SHGFI_DISPLAYNAME) ) {
            ::lstrcpy(lpdi->ceItem.pszText, sfi.szDisplayName);
         }
      }
      if( (lpdi->ceItem.mask & CBEIF_IMAGE) != 0 ) {
         if( ::SHGetFileInfo((LPCTSTR)(LPCITEMIDLIST)pItem->pidlFull, 
                             0, &sfi, sizeof(sfi), 
                             SHGFI_PIDL | SHGFI_SYSICONINDEX | SHGFI_SMALLICON | SHGFI_LINKOVERLAY) ) {
            lpdi->ceItem.iImage = sfi.iIcon;
         }
      }
      if( (lpdi->ceItem.mask & CBEIF_SELECTEDIMAGE) != 0 ) {
         if( ::SHGetFileInfo((LPCTSTR)(LPCITEMIDLIST)pItem->pidlFull, 
                             0, &sfi, sizeof(sfi), 
                             SHGFI_PIDL | SHGFI_SYSICONINDEX | SHGFI_SMALLICON | SHGFI_OPENICON) ) {
            lpdi->ceItem.iSelectedImage = sfi.iIcon;
         }
      }
      return 0;
   }
};


/////////////////////////////////////////////////////////////////////////////
// CExplorerMenu

// NOTE: You need to call OleInitialize() first on anything above Win98 to 
//       successfully execute menu commands.
class CExplorerMenu
{
public:
   CComQIPtr<IContextMenu2, &IID_IContextMenu2> m_spCtxMenu2;

   	BOOL TrackPopupMenu(LPCITEMIDLIST pidl, int x, int y, HWND hWnd, UINT idCmdFirst = 1, UINT idCmdLast = 0x7FFF)
	{
		CMenu menu;
		menu.CreatePopupMenu();
		int nCmd;	   
		return CExplorerMenu::TrackPopupMenu(pidl, x, y, hWnd, menu.m_hMenu, menu.m_hMenu, idCmdFirst, idCmdLast, nCmd);
   }

   BOOL TrackPopupMenu(LPCITEMIDLIST pidl, int x, int y, HWND hWnd, HMENU hMenu, HMENU hInsertMenu, UINT idCmdFirst, UINT idCmdLast, int& nCmd)
   {
      ATLASSERT(pidl);
      ATLASSERT(::IsWindow(hWnd));

      CComPtr<IShellFolder> spFolder;
      CPidl pidlItem;
      if( !AtlGetShellPidl(pidl, &spFolder, &pidlItem) ) 
	  {
		  return FALSE;
	  }

      // Get a pointer to the item's IContextMenu interface and call
      // IContextMenu::QueryContextMenu to initialize a context menu.
      BOOL bResult = FALSE;
      CComPtr<IContextMenu> spContextMenu;
      LPCITEMIDLIST lpPidl = pidlItem;
      if( FAILED( spFolder->GetUIObjectOf(hWnd, 1, &lpPidl, IID_IContextMenu, NULL, (LPVOID*) &spContextMenu))) 
	  {
		  return FALSE;
	  }
      
	  nCmd = 0;
      if( SUCCEEDED( spContextMenu->QueryContextMenu(
           hInsertMenu, 
           0, 
           idCmdFirst, 
           idCmdLast, 
           CMF_EXPLORE)) ) 
      {
         m_spCtxMenu2 = spContextMenu;
         
		 // Display the context menu.
         nCmd = DoTrackPopupMenu(hMenu, 
            TPM_LEFTALIGN | TPM_RIGHTBUTTON | TPM_RETURNCMD,
            x, y, 
            0, 
            hWnd, 
            NULL);

         m_spCtxMenu2.Release();
      }

      // If a command is available (from the menu, perhaps), execute it.
      if( (UINT)nCmd >= idCmdFirst && (UINT)nCmd <= idCmdLast ) 
	  {
         CMINVOKECOMMANDINFO ici = { 0 };
         ici.cbSize       = sizeof(CMINVOKECOMMANDINFO);
         ici.fMask        = 0;
         ici.hwnd         = hWnd;
         ici.lpVerb       = MAKEINTRESOURCEA(nCmd - idCmdFirst);
         ici.lpParameters = NULL;
         ici.lpDirectory  = NULL;
         ici.nShow        = SW_SHOWNORMAL;
         ici.dwHotKey     = 0;
         ici.hIcon        = NULL;
         
		 if( SUCCEEDED( spContextMenu->InvokeCommand(&ici)) ) 
			 bResult = TRUE;
      }
	  else
	  {
		  bResult = TRUE;
	  }
      
	  return bResult;
   }

   // To fix the "Send To" problem with Shell Context menus you
   // must chain your window message map with this map.
   // E.g. Use "CHAIN_MSG_MAP_MEMBER(m_menu)" in the owner's map.
   // I guess that subclassing the window would have been a nicer
   // solution.

   BEGIN_MSG_MAP(CExplorerMenu)
      MESSAGE_HANDLER(WM_INITMENUPOPUP, OnShellMenuMsg)
      MESSAGE_HANDLER(WM_DRAWITEM, OnShellMenuMsg)
      MESSAGE_HANDLER(WM_MEASUREITEM, OnShellMenuMsg)
      MESSAGE_HANDLER(WM_MENUCHAR, OnShellMenuMsg)
   END_MSG_MAP()

protected:
	virtual BOOL DoTrackPopupMenu(HMENU hMenu, UINT uFlags, int x, int y, int nReserved, HWND hWnd, CONST RECT *prcRect)
	{
		return ::TrackPopupMenu(hMenu, uFlags, x, y, nReserved, hWnd, prcRect);
	}

private:
   LRESULT OnShellMenuMsg(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
   {
      bHandled = FALSE;
      if( (uMsg == WM_DRAWITEM || uMsg == WM_MEASUREITEM) && (wParam != 0) ) return 0; // Only for menus
      
	  if( m_spCtxMenu2 ) 
	  {
		  m_spCtxMenu2->HandleMenuMsg(uMsg, wParam, lParam);
	  }

      return 0;
   }
};


#endif // !defined(AFX_SHELLCTRLS_H__20010531_6E5C_D354_4C09_0080AD509054__INCLUDED_)
