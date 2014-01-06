/**
 * @file pntabs.h
 * @brief Tab customisation
 * @author Simon Steele
 * @note Copyright (c) 2007-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef pntabs_h__included
#define pntabs_h__included

#if (_ATL_VER >= 0x0700)
	#include <atlcoll.h>
#endif

#define WTL_TABBED_MDI_SAVE_IMPLEMENTATION
#define USE_BOOST

#include <atlgdix.h>
#include <CustomTabCtrl.h>
#include <DotNetTabCtrl.h>
#include <TabbedFrame.h>
#include <TabbedMDISave.h>
#include <TabbedMDI.h>

#include "controls/pntabcontrol.h"

class CFindBar;

template< class TTabCtrl >
class CPNMDITabOwner :
	public CMDITabOwnerImpl<CPNMDITabOwner<TTabCtrl>, TTabCtrl>
{
	typedef CMDITabOwnerImpl<CPNMDITabOwner, TTabCtrl> baseClass;

public:
	BEGIN_MSG_MAP(CPNMDITabOwner)
		NOTIFY_CODE_HANDLER(CTCN_MCLICK, OnMClick)
		NOTIFY_CODE_HANDLER(NM_DBLCLK, OnDblClick)
		NOTIFY_CODE_HANDLER(NM_CLICK, OnClick)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	/// Get the index of a tab for a child window
	int GetTabIndex(HWND hWndChild)
	{
		TTabCtrl::TItem tcItem;
		tcItem.SetTabView(hWndChild);

		return m_TabCtrl.FindItem(&tcItem, CTFI_TABVIEW);
	}

	/**
	 * Double-click behaviours for tabs:
	 * On a tab: Close (same as middle-click)
	 * In the clear: New tab
	 */
	LRESULT OnDblClick(WPARAM wParam, LPNMHDR lParam, BOOL& bHandled)
	{
		NMCTCITEM* pHdr = (NMCTCITEM*)lParam;
		
		if (pHdr->iItem == -1)
		{
			::SendMessage(GetParent(), WM_COMMAND, ID_FILE_NEW, 0);
		}
		else
		{
			return OnMClick(wParam, lParam, bHandled);
		}
		
		return 0;
	}

	LRESULT OnMClick(WPARAM wParam, LPNMHDR lParam, BOOL& bHandled)
	{
		//We want middle-click to signal a close.
		NMCTCITEM* pHdr = (NMCTCITEM*)lParam;
		
		NMCTCITEM nmh = {{ m_TabCtrl, GetDlgCtrlID(), CTCN_CLOSE }, pHdr->iItem, {pHdr->pt.x, pHdr->pt.y}};
		SendMessage(WM_NOTIFY, nmh.hdr.idFrom, (LPARAM)&nmh);

		return 0;
	}

	LRESULT OnClick(WPARAM wParam, LPNMHDR lParam, BOOL& bHandled)
	{
		::LockWindowUpdate(m_hWndMDIClient);
		LRESULT ret = baseClass::OnClick(wParam, lParam, bHandled);
		::LockWindowUpdate(NULL);
		return ret;
	}

	void ForceHideMDITabControl()
	{
		if(m_hWnd && this->IsWindowVisible())
		{
			RECT rcTabs;
			m_TabCtrl.GetWindowRect(&rcTabs);
			::MapWindowPoints(NULL, m_TabCtrl.GetParent(), (LPPOINT)&rcTabs, 2);

			this->ShowWindow(SW_HIDE);

			// Get the position of the MDI Client including the size of the find bar
			// if it's visible, translated into the correct window co-ordinates.
			RECT rcMDIClient;
			::SendMessage(m_hWndMDIClient, PN_GETMDICLIENTRECT, 0, (LPARAM)&rcMDIClient);

			// the MDI client resizes and shows our window when
			//  handling messages related to SetWindowPos

			// TODO: Is there a better way to do this?
			//  We're basically hiding the tabs and
			//  resizing the MDI client area to "cover up"
			//  where the tabs were
			DWORD dwStyle = m_TabCtrl.GetStyle();
			if(CTCS_BOTTOM == (dwStyle & CTCS_BOTTOM))
			{
				::SetWindowPos(
					m_hWndMDIClient, NULL,
					rcMDIClient.left, rcMDIClient.top,
					(rcMDIClient.right - rcMDIClient.left),
					(rcMDIClient.bottom - rcMDIClient.top) + (rcTabs.bottom - rcTabs.top),
					SWP_NOZORDER);
			}
			else
			{
				::SetWindowPos(
					m_hWndMDIClient, NULL,
					rcMDIClient.left, rcMDIClient.top - (rcTabs.bottom - rcTabs.top),
					(rcMDIClient.right - rcMDIClient.left),
					(rcMDIClient.bottom - rcMDIClient.top) + (rcTabs.bottom - rcTabs.top),
					SWP_NOZORDER);
			}
		}
	}
};

/**
 * @class CPNMDIClient
 * @brief Add extra MDI plumbing to the TabbedMDIClient Framework.
 *
 * In order to maintain other lists of windows such as one in the
 * windows list control, what better way than to use the framework
 * provided in tabbed MDI code.
 */
class CPNMDIClient : public CTabbedMDIClient< CPNTabControl, CPNMDITabOwner< CPNTabControl > >
{
	typedef CTabbedMDIClient< CPNTabControl, CPNMDITabOwner< CPNTabControl > > baseClass;

public:
	explicit CPNMDIClient();
	~CPNMDIClient();

	BEGIN_MSG_MAP(CPNMDIClient)
		MESSAGE_HANDLER(WM_SIZE, OnSize)
		MESSAGE_HANDLER(UWM_MDICHILDACTIVATIONCHANGE, OnChildActivationChange)
		MESSAGE_HANDLER(UWM_MDICHILDTABTEXTCHANGE, OnChildTabTextChange)
		MESSAGE_HANDLER(WM_MDIDESTROY, OnMDIDestroy)
		MESSAGE_HANDLER(WM_LBUTTONDBLCLK, OnDblClick)
		MESSAGE_HANDLER(WM_WINDOWPOSCHANGING, OnWindowPosChanging)
		MESSAGE_HANDLER(PN_ESCAPEPRESSED, OnEscapePressed)
		MESSAGE_HANDLER(PN_GETMDICLIENTRECT, OnGetMdiClientRect)
		
		MESSAGE_HANDLER(WM_MDIACTIVATE, OnMDIActivate)
		MESSAGE_HANDLER(WM_MDINEXT, OnMDINext)
		MESSAGE_HANDLER(WM_MDISETMENU, OnMDISetMenu)

		MESSAGE_HANDLER(PN_NOTIFY, OnPNNotify)

		MESSAGE_HANDLER(WM_THEMECHANGED, OnThemeChanged)

		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	BOOL SubclassWindow(HWND hWnd);

	void ControlUp();

	void ShowFindBar(bool bShow);

	int GetTabIndex(HWND hWndChild);

private:
	LRESULT OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnMDISetMenu(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnMDINext(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnMDIActivate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnMDIDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);
	LRESULT OnChildActivationChange(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);
	LRESULT OnChildTabTextChange(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);
	LRESULT OnWindowPosChanging(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
	LRESULT OnEscapePressed(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
	LRESULT OnGetMdiClientRect(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);
	LRESULT OnThemeChanged(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled);

	LRESULT OnDblClick(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/);

	LRESULT OnPNNotify(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled);

	LRESULT mruMdiSwitch(bool forwards);
	LRESULT tabOrderMdiSwitch(bool forwards);

	typedef std::list<HWND> CHILD_STACK;
	bool					m_bMoving;
	CHILD_STACK				m_children;
	CHILD_STACK::iterator	m_moveIt;
	CFindBar*				m_findBar;
};

/**
 * @brief Special MDI command bar control for PN2 implementing special functionality.
 *
 * The reason for this class is to get a chance to capture WM_MDISETMENU so
 * that PN can change menus when MDI children are activated etc.
 */
template<class TPNFrame>
class CPNTabbedMDICommandBarCtrl : public CTabbedMDICommandBarCtrlImpl< CPNTabbedMDICommandBarCtrl<TPNFrame> >
{
	protected:
		typedef CPNTabbedMDICommandBarCtrl thisClass;
		typedef CTabbedMDICommandBarCtrlImpl<thisClass> baseClass;
		typedef void (TPNFrame::*MDISetMenuFn)(HMENU hOld, HMENU hNew);

	public:
		DECLARE_WND_SUPERCLASS(_T("WTL_PNTabbedMDICommandBar"), GetWndClassName())

		BEGIN_MSG_MAP(thisClass)
			CHAIN_MSG_MAP(baseClass)

		ALT_MSG_MAP(1)		// Parent window messages
			CHAIN_MSG_MAP_ALT(baseClass, 1)

		ALT_MSG_MAP(2)		// MDI client window messages
			MESSAGE_HANDLER(WM_MDISETMENU, OnMDISetMenu)
			CHAIN_MSG_MAP_ALT(baseClass, 2)

		ALT_MSG_MAP(3)		// Message hook messages
			CHAIN_MSG_MAP_ALT(baseClass, 3)
		END_MSG_MAP()

		CPNTabbedMDICommandBarCtrl() : baseClass()
		{
			m_pF = NULL;
			m_pFrame = NULL;
			m_hDisabledImages = NULL;
		}

		~CPNTabbedMDICommandBarCtrl()
		{
			if(m_hDisabledImages != NULL)
				::ImageList_Destroy(m_hDisabledImages);
		}

		LRESULT OnMDISetMenu(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
		{
			bHandled = TRUE;
			m_wndMDIClient.DefWindowProc(uMsg, NULL, lParam);
			HMENU hOldMenu = GetMenu();
			BOOL bRet = AttachMenu((HMENU)wParam);

			// PN Specific Code
			if(m_pFrame)
				(m_pFrame->*m_pF)(hOldMenu, (HMENU)wParam);

			bRet;
			ATLASSERT(bRet);
			return (LRESULT)hOldMenu;
		}

		void SetCallback(TPNFrame* pFrame, MDISetMenuFn pFn)
		{
			m_pFrame = pFrame;
			m_pF = pFn;
		}

		bool CreateInternalImageList(int cImages)
		{
			//UINT uFlags = (m_bAlphaImages ? ILC_COLOR32 : ILC_COLOR) | ILC_MASK;
			UINT uFlags = ILC_COLOR32 | ILC_MASK;
			m_hImageList = ::ImageList_Create(m_szBitmap.cx, m_szBitmap.cy, uFlags, cImages, 1);
			m_hDisabledImages = ::ImageList_Create(m_szBitmap.cx, m_szBitmap.cy, uFlags, cImages, 1);
			ATLASSERT(m_hImageList != NULL);
			ATLASSERT(m_hDisabledImages != NULL);
			return (m_hImageList != NULL);
		}

		BOOL LoadDisabledImages(ATL::_U_STRINGorID image, ATL::_U_STRINGorID disImage)
		{
			return loadImages(image, disImage, m_hDisabledImages, NULL, m_arrDisabledCommands, false);
		}

		BOOL LoadDisabledImages(ATL::_U_STRINGorID image, ATL::_U_STRINGorID disImage, HBITMAP hBitmap)
		{
			return loadImages(image, disImage, m_hDisabledImages, hBitmap, m_arrDisabledCommands, false);
		}

		void DrawBitmapDisabled(CDCHandle& dc, int nImage, POINT point,
			HBRUSH hBrushBackground = ::GetSysColorBrush(COLOR_3DFACE),
			HBRUSH hBrush3DEffect = ::GetSysColorBrush(COLOR_3DHILIGHT),
			HBRUSH hBrushDisabledImage = ::GetSysColorBrush(COLOR_3DSHADOW))
		{
			int nCmd = m_arrCommand[nImage];
			int iButton = -1;

			for(int i = 0; i < m_arrDisabledCommands.GetSize(); i++)
			{
				if(m_arrDisabledCommands[i] == nCmd)
				{
					iButton = i;
					break;
				}
			}

			if(iButton != -1)
			{
				::ImageList_Draw(m_hDisabledImages, iButton, dc, point.x, point.y, ILD_TRANSPARENT);
			}
			else
			{
				baseClass::DrawBitmapDisabled(dc, nImage, point, 
					hBrushBackground, hBrush3DEffect, hBrushDisabledImage);
			}
		}

	protected:
		BOOL loadImages(ATL::_U_STRINGorID image, ATL::_U_STRINGorID disImage, HIMAGELIST hImageList, HBITMAP hBitmap,
			ATL::CSimpleValArray<WORD>& arrCommand,
			bool bMapped, UINT nFlags = 0, LPCOLORMAP lpColorMap = NULL, int nMapSize = 0)
		{
			ATLASSERT(::IsWindow(m_hWnd));
			#if (_ATL_VER >= 0x0700)
					HINSTANCE hInstance = ATL::_AtlBaseModule.GetResourceInstance();
			#else //!(_ATL_VER >= 0x0700)
					HINSTANCE hInstance = _Module.GetResourceInstance();
			#endif //!(_ATL_VER >= 0x0700)

			HRSRC hRsrc = ::FindResource(hInstance, image.m_lpstr, (LPTSTR)RT_TOOLBAR);
			if(hRsrc == NULL)
				return FALSE;

			HGLOBAL hGlobal = ::LoadResource(hInstance, hRsrc);
			if(hGlobal == NULL)
				return FALSE;

			_ToolBarData* pData = (_ToolBarData*)::LockResource(hGlobal);
			if(pData == NULL)
				return FALSE;
			ATLASSERT(pData->wVersion == 1);

			WORD* pItems = pData->items();
			int nItems = pData->wItemCount;

			// Set internal data
			SetImageSize(pData->wWidth, pData->wHeight);

			// Create image list if needed
			if(hImageList == NULL)
			{
				// Check if the bitmap is 32-bit (alpha channel) bitmap (valid for Windows XP only)
				//T* pT = static_cast<T*>(this);
				m_bAlphaImages = AtlIsAlphaBitmapResource(image);

				if(!CreateInternalImageList(pData->wItemCount))
					return FALSE;
			}

			CBitmap bmp;

			if(hBitmap != NULL)
			{
				// Add bitmap to our image list
				ATL::_U_STRINGorID imgId = (disImage.m_lpstr != NULL) ? disImage : image;
			
				if(bMapped)
				{
					ATLASSERT(HIWORD(PtrToUlong(imgId.m_lpstr)) == 0);   // if mapped, must be a numeric ID
					int nIDImage = (int)(short)LOWORD(PtrToUlong(imgId.m_lpstr));
					bmp.LoadMappedBitmap(nIDImage, (WORD)nFlags, lpColorMap, nMapSize);
				}
				else
				{
					if(m_bAlphaImages)
	#if (_ATL_VER >= 0x0700)
						bmp = (HBITMAP)::LoadImage(ATL::_AtlBaseModule.GetResourceInstance(), imgId.m_lpstr, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION | LR_DEFAULTSIZE);
	#else //!(_ATL_VER >= 0x0700)
						bmp = (HBITMAP)::LoadImage(_Module.GetResourceInstance(), imgId.m_lpstr, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION | LR_DEFAULTSIZE);
	#endif //!(_ATL_VER >= 0x0700)
					else
						bmp.LoadBitmap(imgId.m_lpstr);
				}
			}
			else
			{
				bmp.Attach(hBitmap);
			}

			ATLASSERT(bmp.m_hBitmap != NULL);
			if(bmp.m_hBitmap == NULL)
				return FALSE;
			if(::ImageList_AddMasked(hImageList, bmp, m_clrMask) == -1)
				return FALSE;

			if(hBitmap != NULL)
				bmp.Detach();

			// Fill the array with command IDs
			for(int i = 0; i < nItems; i++)
			{
				if(pItems[i] != 0)
					arrCommand.Add(pItems[i]);
			}

			ATLASSERT(::ImageList_GetImageCount(hImageList) == arrCommand.GetSize());
			if(::ImageList_GetImageCount(hImageList) != arrCommand.GetSize())
				return FALSE;

			return TRUE;
		}

	protected:
		ATL::CSimpleValArray<WORD> m_arrDisabledCommands;
		TPNFrame* m_pFrame;
		MDISetMenuFn m_pF;
		HIMAGELIST m_hDisabledImages;
};

#endif //#ifndef pntabs_h__included