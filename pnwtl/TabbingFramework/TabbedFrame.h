/////////////////////////////////////////////////////////////////////////////
// TabbedFrame.h - Base template class for supporting a frame
//   window with multiple views that you switch between using
//   a "CoolTabCtrl" (such as CDotNetTabCtrl)
//
// Written by Daniel Bowen (dbowen@es.com)
// Copyright (c) 2002 Daniel Bowen.
//
// Depends on CoolTabCtrls.h written by Bjarke Viksoe (bjarke@viksoe.dk)
//
// CTabbedFrameImpl -
//   Base template to derive your specialized class from to get
//   a frame window with multiple "view" child windows that you
//   switch between using a "CoolTabCtrl" (such as CDotNetTabCtrl).
//   CTabbedPopupFrame is an example of a simple popup tool window
//   frame derived from CTabbedFrameImpl.
// CCoolTabOwnerImpl -
//   Helps implement the parent of the actual tab window.
//   The class doesn't have a message map itself, and is meant
//   to be inherited from along-side a CWindowImpl derived class.
//   The class inheriting from this should be sure to reflect
//   notifications (since that's what the tab window expects).
//   This class handles creation of the tab window as well as
//   adding, removing, switching and renaming tabs based on an HWND.
//   
//
// This code may be used in compiled form in any way you desire. This
// file may be redistributed by any means PROVIDING it is 
// not sold for profit without the authors written consent, and 
// providing that this notice and the authors name is included.
//
// This file is provided "as is" with no expressed or implied warranty.
// The author accepts no liability if it causes any damage to you or your
// computer whatsoever.
//
// If you find bugs, have suggestions for improvements, etc.,
// please contact the author.
//
// History (Date/Author/Description):
// ----------------------------------
//
// 2002/04/23: Daniel Bowen (DDB)
// - New "CCoolTabItem" class used instead of TCITEM.
//   See "CoolTabCtrls.h" for an explanation.
// - New CCoolTabOwnerImpl::UpdateTabToolTip
// - New CCoolTabOwnerImpl::UpdateTabImage
// - ATL 7 compatibility issues pointed out to me by
//   Ramon Casellas [casellas@infres.enst.fr]
//
// 2002/04/22: Daniel Bowen (DDB)
// - CCoolTabOwnerImpl: Add several more methods for getting an image
//   into the image list and associated with a tab.  With the updates,
//   you can now more easily add images independant of adding a tab.
//   IMPORTANT: If you've used this code in the past, its important
//    to use "AddTabWithIcon" instead of "AddTab" if you are adding
//    a tab with a resource identifier of an icon.
// - CTabbedFrameImpl: Make the tab area height dependant on
//   the same metrics that the font of the tabs depends on.
//
// 2002/04/05: Daniel Bowen (DDB)
// - Update CTabbedFrameImpl::OnSetFocus to not check for
//   IsWindowVisible(m_hWndActive).  When the frame is maximized,
//   ::IsWindowVisible(m_hWndActive) will return FALSE,
//   which results in the focus not being forwarded
//   to the active tab window.
//   See http://www.codeproject.com/useritems/WTLMDIChildMax.asp
//   for an explanation.

// 2002/03/13: Daniel Bowen (DDB)
// - Remove notification handler for NM_RCLICK from CTabbedFrameImpl.
//   The implementation is correct when a tree view is the active child,
//   but not necessarily for other windows (most notibly a list view).
//   Since notifications are getting reflected back anyway, let the
//   child deal with NM_RCLICK as appropriate

// 2002/03/11: Daniel Bowen (DDB)
// - CTabbedFrameImpl:
//   * Added handler for WM_SETFOCUS to set focus to active child
//     (similar to how CFrameWindowImplBase::OnSetFocus does it
//      in the normal case when there's a child/client window)
//   * For key messages, if the baseClass message map hasn't
//      handled it, re-send the message to the active child window

// 2002/02/07: Daniel Bowen (DDB)
// - New CCoolTabOwnerImpl MI class resulting from
//   refactoring due to commonality that existed between
//   CTabbedFrameImpl and CMDITabOwner.  Use this
//   class to help implement a windowing class that
//   will contain a CoolTabCtrl.
// 
// 2002/01/31: Daniel Bowen (DDB)
// - Original Release
//

#ifndef __WTL_TABBED_FRAME_H__
#define __WTL_TABBED_FRAME_H__

#pragma once

#ifndef __cplusplus
	#error TabbedFrame.h requires C++ compilation
#endif

#ifndef __ATLAPP_H__
	#error TabbedFrame.h requires atlapp.h to be included first
#endif

#ifndef __ATLWIN_H__
	#error TabbedFrame.h requires atlwin.h to be included first
#endif

#ifndef __ATLFRAME_H__
	#error TabbedFrame.h requires atlframe.h to be included first
#endif

#ifndef __COOLTABCTRLS_H__
	#error TabbedFrame.h requires CoolTabCtrls.h to be included first
#endif


/////////////////////////////////////////////////////////////////////////////
//
// CCoolTabOwnerImpl
//  an MI template to help implement the owner window that uses CoolTabs
//  to switch between windows / views
//
/////////////////////////////////////////////////////////////////////////////

template <class T, class TTab>
class CCoolTabOwnerImpl
{
// Member variables
protected:
	TTab m_tabs;
	WTL::CImageList m_ImageList;
	bool m_bTabAreaOnTop;
	int m_cxImage, m_cyImage;

// Constructors
public:
	CCoolTabOwnerImpl() :
		m_bTabAreaOnTop(true),
		m_cxImage(16),
		m_cyImage(16)
	{
	}

// Overrideables
public:

	void ShowTabs()
	{
	}

	void HideTabs()
	{
	}

// Methods
public:
	TTab& GetTabs()
	{
		return m_tabs;
	}

	void SetTabAreaOnTop(bool bTabsOnTop = true)
	{
		m_bTabAreaOnTop = bTabsOnTop;
	}

	bool IsTabAreaOnTop() const
	{
		return m_bTabAreaOnTop;
	}

	void CreateTabWindow(HWND hWndTabParent, RECT rcTab)
	{
		if(m_tabs.IsWindow())
		{
			m_tabs.DestroyWindow();
		}

		BOOL bCreate = FALSE;
		bCreate = m_ImageList.Create(m_cxImage, m_cyImage, ILC_COLOR32 | ILC_MASK, 4, 4);
		if(bCreate)
		{
			m_tabs.SetImageList(m_ImageList);
		}

		DWORD dwStyle = WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_CLIPCHILDREN | TCS_TOOLTIPS;
		if(!m_bTabAreaOnTop)
		{
			dwStyle |= TCS_BOTTOM;
		}

		m_tabs.Create(hWndTabParent, rcTab, NULL, dwStyle);
	}

	BOOL DestroyTabWindow()
	{
		return m_ImageList.Destroy();
	}

	int GetCurSel() const
	{
		return m_tabs.GetCurSel();
	}

	// AddBitmap (with a couple of overloaded versions)
	int AddBitmap(HBITMAP hBitmap, HBITMAP hBitmapMask = NULL)
	{
		return m_ImageList.Add(hBitmap, hBitmapMask);
	}

	int AddBitmap(HBITMAP hBitmap, COLORREF crMask)
	{
		return m_ImageList.Add(hBitmap, crMask);
	}

	int AddBitmap(WTL::_U_STRINGorID bitmap, COLORREF crMask, HMODULE hModule = _Module.GetResourceInstance())
	{
		HBITMAP hBitmap = (HBITMAP)::LoadImage(
			hModule,
			bitmap.m_lpstr,
			IMAGE_BITMAP, 0, 0, LR_SHARED);
		return hBitmap ? m_ImageList.Add(hBitmap, crMask) : -1;
	}

	int AddBitmap(WTL::_U_STRINGorID bitmap, HBITMAP hBitmapMask = NULL, HMODULE hModule = _Module.GetResourceInstance())
	{
		HBITMAP hBitmap = (HBITMAP)::LoadImage(
			hModule,
			bitmap.m_lpstr,
			IMAGE_BITMAP, 0, 0, LR_SHARED);
		return hBitmap ? m_ImageList.Add(hBitmap, hBitmapMask) : -1;
	}

	// AddIcon (with a couple of overloaded versions)
	int AddIcon(HICON hIcon)
	{
		return m_ImageList.AddIcon(hIcon);
	}

	int AddIcon(WTL::_U_STRINGorID icon, HMODULE hModule = _Module.GetResourceInstance())
	{
		HICON hIcon = (HICON)::LoadImage(
			hModule,
			icon.m_lpstr,
			IMAGE_ICON, m_cxImage, m_cyImage, LR_SHARED);
		return hIcon ? m_ImageList.AddIcon(hIcon) : -1;
	}

	// AddTabWithBitmap (with a couple of overloaded versions)
	int AddTabWithBitmap(HWND hWnd, LPCTSTR sTabText, HBITMAP hBitmap, HBITMAP hBitmapMask = NULL)
	{
		if(hWnd == NULL)
		{
			return -1;
		}

		int nImageIndex = this->AddBitmap(hBitmap, hBitmapMask);

		return this->AddTab(hWnd, sTabText, nImageIndex);
	}

	int AddTabWithBitmap(HWND hWnd, LPCTSTR sTabText, HBITMAP hBitmap, COLORREF crMask)
	{
		if(hWnd == NULL)
		{
			return -1;
		}

		int nImageIndex = this->AddBitmap(hBitmap, crMask);

		return this->AddTab(hWnd, sTabText, nImageIndex);
	}

	int AddTabWithBitmap(HWND hWnd, LPCTSTR sTabText, WTL::_U_STRINGorID bitmap, HBITMAP hBitmapMask = NULL, HMODULE hModule = _Module.GetResourceInstance())
	{
		if(hWnd == NULL)
		{
			return -1;
		}

		int nImageIndex = this->AddBitmap(bitmap, hBitmapMask, hModule);

		return this->AddTab(hWnd, sTabText, nImageIndex);
	}

	int AddTabWithBitmap(HWND hWnd, LPCTSTR sTabText, WTL::_U_STRINGorID bitmap, COLORREF crMask, HMODULE hModule = _Module.GetResourceInstance())
	{
		if(hWnd == NULL)
		{
			return -1;
		}

		int nImageIndex = this->AddBitmap(bitmap, crMask, hModule);

		return this->AddTab(hWnd, sTabText, nImageIndex);
	}

	// AddTabWithIcon (with a couple of overloaded versions)
	int AddTabWithIcon(HWND hWnd, LPCTSTR sTabText, HICON hIcon)
	{
		if(hWnd == NULL)
		{
			return -1;
		}

		int nImageIndex = this->AddIcon(hIcon);

		return this->AddTab(hWnd, sTabText, nImageIndex);
	}

	int AddTabWithIcon(HWND hWnd, LPCTSTR sTabText, WTL::_U_STRINGorID icon, HMODULE hModule = _Module.GetResourceInstance())
	{
		if(hWnd == NULL)
		{
			return -1;
		}

		int nImageIndex = this->AddIcon(icon, hModule);

		return this->AddTab(hWnd, sTabText, nImageIndex);
	}

	// AddTab - either referencing an image in the image list, or no image used
	int AddTab(HWND hWnd, LPCTSTR sTabText, int nImageIndex = -1)
	{
		if(hWnd == NULL)
		{
			return -1;
		}

		return m_tabs.InsertItem(m_tabs.GetItemCount(), sTabText, nImageIndex, hWnd);
	}

	int DisplayTab(HWND hWnd, BOOL bAddIfNotFound = TRUE)
	{
		int nTab = -1;
		if(hWnd)
		{
			CCoolTabItem tcItem;
			tcItem.SetTabView(hWnd);

			nTab = m_tabs.FindItem(&tcItem, CCoolTabItem::eCoolTabItem_TabView);
			if((bAddIfNotFound == TRUE) && (nTab < 0))
			{
				// The corresponding tab doesn't exist yet. Create it.

				LPTSTR sWindowText = NULL;
				size_t cchWindowText = ::GetWindowTextLength(hWnd);
				if(cchWindowText > 0)
				{
					sWindowText = new TCHAR[cchWindowText + 1];
					if(sWindowText != NULL)
					{
						::GetWindowText(hWnd, sWindowText, cchWindowText+1);

						nTab = AddTab(hWnd, sWindowText);

						delete [] sWindowText;

						if(nTab >= 0)
						{
							T* pT = static_cast<T*>(this);
							pT->ShowTabs();
						}
					}
				}

				if(nTab < 0)
				{
					// We had trouble getting the window text
					// TODO: What should we put for the text and/or icon
					//  in this case?
					ATLASSERT(0 && "Adding a tab where no name was provided");
					nTab = AddTab(hWnd, _T("Document"));
				}
			}


			if(nTab >= 0)
			{
				if(m_tabs.GetCurSel() != nTab)
				{
					m_tabs.SetCurSel(nTab);
				}
			}

		}

		return nTab;
	}

	BOOL RemoveTab(HWND hWnd)
	{
		BOOL bSuccess = FALSE;

		CCoolTabItem tcItem;
		tcItem.SetTabView(hWnd);

		int nTab = m_tabs.FindItem(&tcItem, CCoolTabItem::eCoolTabItem_TabView);
		if(nTab >= 0)
		{
			bSuccess = m_tabs.DeleteItem(nTab);

			if(m_tabs.GetItemCount() < 1)
			{
				T* pT = static_cast<T*>(this);
				pT->HideTabs();
			}
		}

		return bSuccess;
	}

	BOOL UpdateTabText(HWND hWnd, LPCTSTR sText = NULL)
	{
		BOOL bSuccess = FALSE;

		CCoolTabItem tcItem;
		tcItem.SetTabView(hWnd);

		int nTab = m_tabs.FindItem(&tcItem, CCoolTabItem::eCoolTabItem_TabView);
		if(nTab >= 0)
		{
			CCoolTabItem* pItem = m_tabs.GetItem(nTab);
			CString sCurrentTabText = pItem->GetText();

			if(sText != NULL)
			{
				if(sCurrentTabText != sText)
				{
					bSuccess = pItem->SetText(sText);
					m_tabs.UpdateLayout();
					m_tabs.Invalidate();
				}
			}
			else
			{
				LPTSTR sWindowText = NULL;
				size_t cchWindowText = ::GetWindowTextLength(hWnd);
				if(cchWindowText > 0)
				{
					sWindowText = new TCHAR[cchWindowText + 1];
					if(sWindowText != NULL)
					{
						::GetWindowText(hWnd, sWindowText, cchWindowText+1);

						if(sWindowText != NULL &&
							sCurrentTabText != sWindowText)
						{
							bSuccess = pItem->SetText(sWindowText);
							m_tabs.UpdateLayout();
							m_tabs.Invalidate();
						}

						delete [] sWindowText;
					}
				}
			}
		}

		return bSuccess;
	}

	BOOL UpdateTabImage(HWND hWnd, int nImageIndex = -1)
	{
		BOOL bSuccess = FALSE;

		CCoolTabItem tcItem;
		tcItem.SetTabView(hWnd);

		int nTab = m_tabs.FindItem(&tcItem, CCoolTabItem::eCoolTabItem_TabView);
		if(nTab >= 0)
		{
			CCoolTabItem* pItem = m_tabs.GetItem(nTab);
			int nCurrentImageIndex = pItem->GetImageIndex();
			if(nCurrentImageIndex != nImageIndex)
			{
				bSuccess = pItem->SetImageIndex(nImageIndex);
				m_tabs.UpdateLayout();
				m_tabs.Invalidate();
			}
		}

		return bSuccess;
	}

	BOOL UpdateTabToolTip(HWND hWnd, LPCTSTR sToolTip = NULL)
	{
		BOOL bSuccess = FALSE;

		CCoolTabItem tcItem;
		tcItem.SetTabView(hWnd);

		int nTab = m_tabs.FindItem(&tcItem, CCoolTabItem::eCoolTabItem_TabView);
		if(nTab >= 0)
		{
			CCoolTabItem* pItem = m_tabs.GetItem(nTab);
			CString sCurrentToolTip = pItem->GetToolTip();
			if(sCurrentToolTip != sToolTip)
			{
				bSuccess = pItem->SetToolTip(sToolTip);
			}
		}

		return bSuccess;
	}
};

/////////////////////////////////////////////////////////////////////////////
//
// CTabbedFrameImpl
//
/////////////////////////////////////////////////////////////////////////////

template <
	class T,
	class TTab = CDotNetTabCtrl,
	class TBase = CFrameWindowImpl<T, CWindow, CFrameWinTraits> >
class CTabbedFrameImpl :
	public TBase,
	public CCoolTabOwnerImpl< CTabbedFrameImpl<T, TTab, TBase>, TTab>
{
protected:
	typedef CTabbedFrameImpl<T, TTab, TBase> thisClass;
	typedef TBase baseClass;

// Member variables
protected:
	HWND m_hWndActive;
	BYTE m_nTabAreaHeight;

// Constructors
public:
	CTabbedFrameImpl() :
		m_hWndActive(NULL),
		m_nTabAreaHeight(24)
	{
		// Initialize members from CCoolTabOwnerImpl
		m_bTabAreaOnTop = false;
		//m_tabs.SetBoldSelectedTab(false);
	}

// Methods
public:
	void UpdateTabAreaHeight()
	{
		const int nNominalHeight = 24;
		const int nNominalFontLogicalUnits = 11;	// 8 point Tahoma with 96 DPI

		// Initialize nFontLogicalUnits to the typical case
		// appropriate for CDotNetTabCtrl
		LOGFONT lfIcon = { 0 };
		::SystemParametersInfo(SPI_GETICONTITLELOGFONT, sizeof(lfIcon), &lfIcon, 0);
		int nFontLogicalUnits = -lfIcon.lfHeight;

		// Use the actual font of the tab control
		TTab& tabs = this->GetTabs();
		if(tabs.IsWindow())
		{
			HFONT hFont = tabs.GetFont();
			if(hFont != NULL)
			{
				CDC dc = tabs.GetDC();
				CFontHandle hFontOld = dc.SelectFont(hFont);
				TEXTMETRIC tm = {0};
				dc.GetTextMetrics(&tm);
				nFontLogicalUnits = tm.tmAscent;
				dc.SelectFont(hFontOld);
			}
		}

		BYTE nOldTabAreaHeight = m_nTabAreaHeight;
		m_nTabAreaHeight = (BYTE)(nNominalHeight + (nNominalHeight * ((double)nFontLogicalUnits / (double)nNominalFontLogicalUnits) - nNominalHeight) / 2);
		if(nOldTabAreaHeight != m_nTabAreaHeight)
		{
			Invalidate();
		}
	}

	virtual void OnFinalMessage(HWND /*hWnd*/)
	{
		// TODO: Have support both for "new"ing an
		//  instance of this class, or having
		//  a member variable of this class.
		//  Currently, we don't support deleting our
		//  instance because someone created us with "new"
		//delete this;
	}

// Message Handling
public:
	DECLARE_FRAME_WND_CLASS(_T("TabbedFrame"), 0)

	BEGIN_MSG_MAP(thisClass)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy)
		MESSAGE_HANDLER(WM_SETTINGCHANGE, OnSettingChange)
		MESSAGE_HANDLER(WM_ERASEBKGND, OnEraseBackground)
		MESSAGE_HANDLER(WM_SETFOCUS, OnSetFocus)
//		MESSAGE_HANDLER(WM_FORWARDMSG, OnForwardMsg)

		NOTIFY_CODE_HANDLER(TCN_DELETEITEM, OnDeleteItem)
		NOTIFY_CODE_HANDLER(TCN_SELCHANGING, OnSelChanging)
		NOTIFY_CODE_HANDLER(TCN_SELCHANGE, OnSelChange)

		CHAIN_MSG_MAP(baseClass)

		// If there are key messages that haven't been handled yet,
		// pass those along to the active child window
		if(uMsg >= WM_KEYFIRST && uMsg <= WM_KEYLAST)
		{
			if(m_hWndActive != NULL && ::IsWindow(m_hWndActive))
			{
				lResult = ::SendMessage(m_hWndActive, uMsg, wParam, lParam);

				return TRUE;
			}
		}
		

		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	LRESULT OnCreate(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		// "baseClass::OnCreate()"
		LRESULT lRet = DefWindowProc(uMsg, wParam, lParam);
		bHandled = TRUE;

		CreateTabWindow(m_hWnd, rcDefault);

		UpdateTabAreaHeight();
		return 0;
	}

	LRESULT OnDestroy(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		DestroyTabWindow();

		// Say that we didn't handle it so that anyone else
		//  interested gets to handle the message
		bHandled = FALSE;
		return 0;
	}

	LRESULT OnSettingChange(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		UpdateTabAreaHeight();

		bHandled = FALSE;
		return 0;
	}

	LRESULT OnEraseBackground(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		// Let the active view and the tabs do all the drawing
		// as flicker-free as possible.
		return 1;
	}

	LRESULT OnSetFocus(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		// NOTE: ::IsWindowVisible(m_hWndActive) will be false if
		//  the frame is maximized.  So just use "IsWindow" instead.
		if(m_hWndActive != NULL && ::IsWindow(m_hWndActive))
		{
			::SetFocus(m_hWndActive);
		}

		bHandled = FALSE;
		return 1;
	}

// TODO: Call PreTranslateMessage for the active view,
//    or maybe just send it WM_FORWARDMSG
//	LRESULT OnForwardMsg(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
//	{
//		LPMSG pMsg = (LPMSG)lParam;
//
//		if(baseClass::PreTranslateMessage(pMsg))
//			return TRUE;
//
//		return m_view.PreTranslateMessage(pMsg);
//	}

	LRESULT OnDeleteItem(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& bHandled)
	{
		bHandled = FALSE;
		return 0;
	}

	LRESULT OnSelChanging(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& bHandled)
	{
		bHandled = FALSE;
		return 0;
	}

	LRESULT OnSelChange(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& bHandled)
	{
		int nNewTab = m_tabs.GetCurSel();

		if(nNewTab >= 0)
		{
			CCoolTabItem* pItem = m_tabs.GetItem(nNewTab);
			if(pItem->UsingTabView())
			{
				HWND hWndNew = pItem->GetTabView();
				HWND hWndOld = m_hWndActive;
				if( hWndNew != hWndOld )
				{
					m_hWndActive = hWndNew;

					//UpdateLayout is going to essentially do a
					//  "ShowWindow(hWndNew, SW_SHOW)" for us
					// (Call the most derived class's version of UpdateLayout)
					T* pT = static_cast<T*>(this);
					pT->UpdateLayout();

					if(hWndOld)
					{
						::ShowWindow(hWndOld, SW_HIDE);
					}

					::SetFocus(hWndNew);
				}
			}
		}

		bHandled = FALSE;
		return 0;
	}

// Overrides from TBase
public:
	void UpdateLayout(BOOL bResizeBars = TRUE)
	{
		RECT rect;
		GetClientRect(&rect);

		// position bars and offset their dimensions
		T* pT = static_cast<T*>(this);
		pT->UpdateBarsPosition(rect, bResizeBars);

		/*
		// resize client window
		if(m_hWndClient != NULL)
			::SetWindowPos(m_hWndClient, NULL, rect.left, rect.top,
				rect.right - rect.left, rect.bottom - rect.top,
				SWP_NOZORDER | SWP_NOACTIVATE);
		*/

		int nWindowPosCount=0;
		if(m_tabs) nWindowPosCount++;
		if(m_hWndActive) nWindowPosCount++;

		if(nWindowPosCount > 0)
		{
			HDWP hdwp = BeginDeferWindowPos(nWindowPosCount);
			DWORD dwStyle = (DWORD)m_tabs.GetWindowLong(GWL_STYLE);
			BOOL bHasBottomStyle = dwStyle & TCS_BOTTOM;
			if(bHasBottomStyle)
			{
				if(m_tabs)
				{
					::DeferWindowPos(
						hdwp,
						m_tabs,
						NULL,
						rect.left, rect.bottom - m_nTabAreaHeight,
						rect.right - rect.left, m_nTabAreaHeight,
						SWP_NOZORDER | SWP_NOACTIVATE);
				}
				if(m_hWndActive)
				{
					::DeferWindowPos(
						hdwp,
						m_hWndActive,
						NULL,
						rect.left, rect.top,
						rect.right - rect.left, (rect.bottom-m_nTabAreaHeight) - rect.top,
						SWP_NOZORDER | SWP_SHOWWINDOW);
				}
			}
			else
			{
				if(m_tabs)
				{
					::DeferWindowPos(
						hdwp,
						m_tabs,
						NULL,
						rect.left, rect.top,
						rect.right-rect.left, m_nTabAreaHeight,
						SWP_NOZORDER | SWP_NOACTIVATE);
				}
				if(m_hWndActive)
				{
					::DeferWindowPos(
						hdwp,
						m_hWndActive,
						NULL,
						rect.left, rect.top + m_nTabAreaHeight,
						rect.right - rect.left,
						rect.bottom - (rect.top+m_nTabAreaHeight),
						SWP_NOZORDER | SWP_SHOWWINDOW);
				}
			}
			EndDeferWindowPos(hdwp);
		}

		m_tabs.UpdateLayout();
	}
};

/////////////////////////////////////////////////////////////////////////////
//
// CTabbedPopupFrame
//
/////////////////////////////////////////////////////////////////////////////

typedef CWinTraits<WS_POPUP | WS_CAPTION | WS_VISIBLE | WS_SYSMENU | WS_THICKFRAME, WS_EX_TOOLWINDOW> TabbedPopupFrameWinTraits;

template <class TTab = CDotNetTabCtrl, bool _Top = true>
class CTabbedPopupFrame :
	public CTabbedFrameImpl<CTabbedPopupFrame, TTab, CFrameWindowImpl<CTabbedPopupFrame, CWindow, TabbedPopupFrameWinTraits> >
{
protected:
	typedef CTabbedPopupFrame<TTab, _Top> thisClass;
	typedef CTabbedFrameImpl<CTabbedPopupFrame, TTab, CFrameWindowImpl<CTabbedPopupFrame, CWindow, TabbedPopupFrameWinTraits> > baseClass;

// Constructors
public:
	CTabbedPopupFrame()
	{
		m_bTabAreaOnTop = _Top;
	}

// Message Handling
public:
	DECLARE_FRAME_WND_CLASS(_T("TabbedPopupFrame"), 0)

	BEGIN_MSG_MAP(thisClass)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()
};


#endif // __WTL_TABBED_FRAME_H__
