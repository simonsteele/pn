#ifndef __ATLSHELLEXTBASE_H__
#define __ATLSHELLEXTBASE_H__

#pragma once

///////////////////////////////////////////////////////////////////
// Shell Extension wrappers
//
// Written by Bjarke Viksoe (bjarke@viksoe.dk)
// Copyright (c) 2001-2007 Bjarke Viksoe.
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

#ifndef __ATLBASE_H__
   #error atlshellbase.h requires atlbase.h to be included first
#endif

#include <shlobj.h>
#include <shlguid.h> 
#include <shellapi.h>

#include <commctrl.h>
#include <commoncontrols.h>

#pragma comment(lib, "shell32.lib")
#pragma comment(lib, "comctl32.lib")


class CShellMalloc
{
public:
   LPMALLOC m_pMalloc;
   void Init()
   {
      // It is safe to call SHGetMalloc() / CoGetMalloc() without
      // first calling ::CoInitialize() according to MSDN.
      m_pMalloc = NULL;
      ::SHGetMalloc(&m_pMalloc);
   }
   void Term()
   {
      if( m_pMalloc != NULL ) m_pMalloc->Release();
   }
   operator LPMALLOC() const
   {
      return m_pMalloc;
   }
   LPVOID Alloc(ULONG cb)
   {
      ATLASSERT(m_pMalloc!=NULL);
      ATLASSERT(cb>0);
      return m_pMalloc->Alloc(cb);
   }
   void Free(LPVOID p)
   {
      ATLASSERT(m_pMalloc!=NULL);
      ATLASSERT(p);
      m_pMalloc->Free(p);
   }
};


class CShellImageLists
{
public:
   HIMAGELIST m_hImageListSmall;
   HIMAGELIST m_hImageListLarge;
   HIMAGELIST m_hImageListJumbo;
   CSimpleMap<CComBSTR, int> m_mapFiles;

   CShellImageLists() : m_hImageListSmall(NULL), m_hImageListLarge(NULL), m_hImageListJumbo(NULL)
   {
   }
   virtual ~CShellImageLists()
   {
      if( m_hImageListSmall ) ImageList_Destroy(m_hImageListSmall);
      if( m_hImageListLarge ) ImageList_Destroy(m_hImageListLarge);
      if( m_hImageListJumbo ) ImageList_Destroy(m_hImageListJumbo);
   }

   BOOL Create(HINSTANCE hResource, LPCTSTR RootID, LPCTSTR FolderID)
   {
      // Set the small image list
      if( m_hImageListSmall != NULL ) ImageList_Destroy(m_hImageListSmall);
      int nSmallCx = ::GetSystemMetrics(SM_CXSMICON);
      int nSmallCy = ::GetSystemMetrics(SM_CYSMICON);
      m_hImageListSmall = ImageList_Create(nSmallCx, nSmallCy, ILC_COLOR32 | ILC_MASK, 4, 0);

      // Set the large image list
      if( m_hImageListLarge != NULL ) ImageList_Destroy(m_hImageListLarge);
      int nLargeCx = ::GetSystemMetrics(SM_CXICON);
      int nLargeCy = ::GetSystemMetrics(SM_CYICON);
      m_hImageListLarge = ImageList_Create(nLargeCx, nLargeCy, ILC_COLOR32 | ILC_MASK, 4, 0);

      // Set the super large image list
      if( m_hImageListJumbo != NULL ) ImageList_Destroy(m_hImageListJumbo);
      m_hImageListJumbo = ImageList_Create(256, 256, ILC_COLOR32 | ILC_MASK, 4, 0);

      if( m_hImageListSmall != NULL ) {
         AddFileIcon(m_hImageListSmall, hResource, MAKEINTRESOURCE(RootID), nSmallCx, nSmallCy);
         AddFileIcon(m_hImageListSmall, hResource, MAKEINTRESOURCE(FolderID), nSmallCx, nSmallCy);
      }      
      if( m_hImageListLarge != NULL ) {
         AddFileIcon(m_hImageListLarge, hResource, MAKEINTRESOURCE(RootID), nLargeCx, nLargeCy);
         AddFileIcon(m_hImageListLarge, hResource, MAKEINTRESOURCE(FolderID), nLargeCx, nLargeCy);
      }
      if( m_hImageListJumbo != NULL ) {
         AddFileIcon(m_hImageListJumbo, hResource, MAKEINTRESOURCE(RootID), 256, 256);
         AddFileIcon(m_hImageListJumbo, hResource, MAKEINTRESOURCE(FolderID), 256, 256);
      }

      return TRUE;
   }

   static BOOL AddShellIcon(HIMAGELIST hImageList, LPCTSTR pstrPath, UINT uFlags)
   {
      ATLASSERT(hImageList);
      SHFILEINFO sfi = { 0 };
      ::SHGetFileInfo(pstrPath, 0, &sfi, sizeof(sfi), uFlags);
      ATLASSERT(sfi.hIcon);
      return ImageList_AddIcon(hImageList, sfi.hIcon) >= 0;
   }
   static BOOL AddFileIcon(HIMAGELIST hImageList, HINSTANCE hResource, LPCTSTR pstrFile, int cx, int cy)
   {
      ATLASSERT(hImageList);
      ATLASSERT(pstrFile);
      HICON hIcon = (HICON) ::LoadImage(hResource, pstrFile, IMAGE_ICON, cx, cy, LR_DEFAULTCOLOR);
      ATLASSERT(hIcon);
      return ImageList_AddIcon(hImageList, hIcon) >= 0;
   }
   static BOOL AddJumboIcon(HIMAGELIST hImageList, LPCTSTR pstrPath, UINT uFlags)
   {
      ATLASSERT(hImageList);
      HICON hIcon = NULL;
#if _WIN32_WINNT >= 0x0600
	  OSVERSIONINFO ovi = { 0 };
      ovi.dwOSVersionInfoSize = sizeof(ovi);
      ::GetVersionEx(&ovi);
      if( ovi.dwMajorVersion >= 6 ) {
         HINSTANCE hShell32 = ::LoadLibrary(_T("shell32.dll"));
         typedef HRESULT (WINAPI *PFNSHGETIMAGELIST)(int,REFIID,LPVOID*);
         PFNSHGETIMAGELIST fnSHGetImageList = (PFNSHGETIMAGELIST) ::GetProcAddress(hShell32, "SHGetImageList");
         if( fnSHGetImageList != NULL ) {
#ifndef SHIL_JUMBO
            const int SHIL_JUMBO = 4;
#endif // SHIL_JUMBO            
            CComPtr<IImageList> spImageList;
            fnSHGetImageList(SHIL_JUMBO, IID_IImageList, (LPVOID*) &spImageList);
            if( spImageList != NULL ) {
               SHFILEINFO sfi = { 0 };
               ::SHGetFileInfo(pstrPath, 0, &sfi, sizeof(sfi), SHGFI_SYSICONINDEX);
               if( sfi.iIcon != 0 ) spImageList->GetIcon(sfi.iIcon, ILD_TRANSPARENT, &hIcon);
            }
         }
         ::FreeLibrary(hShell32);
      }
#endif // _WIN32_WINNT
      if( hIcon == NULL ) {
         SHFILEINFO sfi = { 0 };
         ::SHGetFileInfo(pstrPath, 0, &sfi, sizeof(sfi), uFlags);
         hIcon = sfi.hIcon;
      }
      ATLASSERT(hIcon);
      return ImageList_AddIcon(hImageList, hIcon) >= 0;
   }
   static SIZE GetShellIconSize(UINT uType)
   {
      ATLASSERT(uType==SHGFI_LARGEICON || uType==SHGFI_SMALLICON);
      SHFILEINFO sfi = { 0 };
      ::SHGetFileInfo(_T("temp.txt"), 0, &sfi, sizeof(sfi), SHGFI_ICON | SHGFI_USEFILEATTRIBUTES | SHGFI_SHELLICONSIZE | uType);
      if( sfi.hIcon != NULL ) {
         ICONINFO ii = { 0 };
         ::GetIconInfo(sfi.hIcon, &ii);
         BITMAP bm = { 0 };
         BOOL bRes = ::GetObject(ii.hbmColor, sizeof(BITMAP), &bm);
         ::DeleteObject(sfi.hIcon);
         if( bRes ) {
            SIZE sz = { bm.bmWidth, abs(bm.bmHeight) };
            return sz;
         }
      }
      int ix = SM_CXSMICON, iy = SM_CYSMICON;
      if( uType == SHGFI_LARGEICON ) ix = SM_CXICON, iy = SM_CYICON;
      SIZE sz = { ::GetSystemMetrics(ix), ::GetSystemMetrics(iy) };
      return sz;
   }
};


class CShellModule : public CComModule
{
public:
   HRESULT Init(_ATL_OBJMAP_ENTRY* p, HINSTANCE hInstance, const GUID* plibid = NULL)
   {
      ::OleInitialize(NULL);
#ifdef INITCOMMONCONTROLSEX
      INITCOMMONCONTROLSEX iccex;
      iccex.dwSize = sizeof(INITCOMMONCONTROLSEX);
      iccex.dwICC = ICC_LISTVIEW_CLASSES;
      ::InitCommonControlsEx(&iccex);
#else
      ::InitCommonControls();
#endif // INITCOMMONCONTROLSEX

#ifndef _NO_CLIPFORMATS
   #define CFSTR_OLECLIPBOARDPERSISTONFLUSH    TEXT("OleClipboardPersistOnFlush")
   #define CFSTR_DRAGIMAGEBITS                 TEXT("DragImageBits")
#ifndef CFSTR_LOGICALPERFORMEDDROPEFFECT 
   #define CFSTR_LOGICALPERFORMEDDROPEFFECT    TEXT("Logical Performed DropEffect")
#endif // CFSTR_LOGICALPERFORMEDDROPEFFECT
      m_CFSTR_FILEDESCRIPTOR             = (CLIPFORMAT) ::RegisterClipboardFormat(CFSTR_FILEDESCRIPTOR);
      m_CFSTR_FILECONTENTS               = (CLIPFORMAT) ::RegisterClipboardFormat(CFSTR_FILECONTENTS);
      m_CFSTR_PASTESUCCEEDED             = (CLIPFORMAT) ::RegisterClipboardFormat(CFSTR_PASTESUCCEEDED);
      m_CFSTR_LOGICALPERFORMEDDROPEFFECT = (CLIPFORMAT) ::RegisterClipboardFormat(CFSTR_LOGICALPERFORMEDDROPEFFECT);
      m_CFSTR_PERFORMEDDROPEFFECT        = (CLIPFORMAT) ::RegisterClipboardFormat(CFSTR_PERFORMEDDROPEFFECT);
      m_CFSTR_PREFERREDDROPEFFECT        = (CLIPFORMAT) ::RegisterClipboardFormat(CFSTR_PREFERREDDROPEFFECT);
      m_CFSTR_SHELLIDLIST                = (CLIPFORMAT) ::RegisterClipboardFormat(CFSTR_SHELLIDLIST);
      m_CFSTR_OLECLIPBOARDPERSISTONFLUSH = (CLIPFORMAT) ::RegisterClipboardFormat(CFSTR_OLECLIPBOARDPERSISTONFLUSH);
#if (_WIN32_WINNT >= 0x0500)
      m_CFSTR_TARGETCLSID                = (CLIPFORMAT) ::RegisterClipboardFormat(CFSTR_TARGETCLSID);
#endif // WIN32_WINNT
#endif // _NO_CLIPFORMATS

      // Get Shell allocator
      m_Allocator.Init();
      // Get Windows version
      ::ZeroMemory(&m_WinVer, sizeof(m_WinVer));
      m_WinVer.dwOSVersionInfoSize = sizeof(m_WinVer);
      ::GetVersionEx(&m_WinVer);
      // Plain ATL initialization
      return CComModule::Init(p, hInstance, plibid);
   }
   void Term()
   {
      m_Allocator.Term();
      CComModule::Term();
   }
   // Shell Allocator
   CShellMalloc m_Allocator;
   OSVERSIONINFO m_WinVer;
#ifndef _NO_CLIPFORMATS
   // Clipboard formats
   CLIPFORMAT m_CFSTR_SHELLIDLIST;
   CLIPFORMAT m_CFSTR_FILECONTENTS;
   CLIPFORMAT m_CFSTR_PASTESUCCEEDED;
   CLIPFORMAT m_CFSTR_FILEDESCRIPTOR;
   CLIPFORMAT m_CFSTR_PERFORMEDDROPEFFECT;
   CLIPFORMAT m_CFSTR_PREFERREDDROPEFFECT;
   CLIPFORMAT m_CFSTR_LOGICALPERFORMEDDROPEFFECT;
   CLIPFORMAT m_CFSTR_OLECLIPBOARDPERSISTONFLUSH;
#if (_WIN32_WINNT >= 0x0500)
   CLIPFORMAT m_CFSTR_TARGETCLSID;
#endif // _WIN32_WINNT
#endif // _NO_CLIPFORMATS
};


#endif // __ATLSHELLEXTBASE_H__

