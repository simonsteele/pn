/////////////////////////////////////////////////////////////////////////////
// TabbedMDI.h - Classes that help implement a "Tabbed MDI" interface.
//
// Classes:
//   CTabbedMDIFrameWindowImpl - 
//      Instead of having CMainFrame inherit from
//      CMDIFrameWindowImpl, you can have it inherit from
//      CTabbedMDIFrameWindowImpl. For an out-of-the box WTL MDI
//      application, there are 3 instances of CMDIFrameWindowImpl
//      to replace with CTabbedMDIFrameWindowImpl.
//   CTabbedMDIChildWindowImpl - 
//      If you want your MDI child window to have a corresponding
//      tab in the MDI tab window, inherit from this class instead
//      of from CMDIChildWindowImpl.
//   CTabbedMDIClient - 
//      The CTabbedMDIFrameWindowImpl contains CTabbedMDIClient,
//      which subclasses the "MDI Client" window
//      (from the OS, that manages the MDI child windows).
//      It handles sizing/positioning the tab window,
//      calling the appropriate Display, Remove, UpdateText
//      for the tabs with the HWND of the active child,
//      etc.  You can use CTabbedMDIClient without using
//      CTabbedMDIFrameWindowImpl. To do so, simply call
//      SetTabOwnerParent(m_hWnd) then SubclassWindow(m_hWndMDIClient)
//      on a CTabbedMDIClient member variable after calling
//      CreateMDIClient in your main frame class.
//   CMDITabOwner -
//      The MDITabOwner is the parent of the actual tab window
//      (such as CDotNetTabCtrl), and sibling to the "MDI Client" window.
//      The tab owner tells the MDI child when to display a context
//      menu for the tab (the default menu is the window's system menu).
//      The tab owner changes the active MDI child when the
//      active tab changes.  It also does the real work of
//      hiding and showing the tabs.  It also handles adding,
//      removing, and renaming tabs based on an HWND.
//   CTabbedMDICommandBarCtrl/CTabbedMDICommandBarCtrlImpl -
//      In your MDI application, instead of using CMDICommandBarCtrl,
//      use CTabbedMDICommandBarCtrl.  It addresses a couple of bugs
//      in WTL 7.0's CMDICommandBarCtrl, and allows you to enable
//      or disable whether you want to see the document icon
//      and min/max/close button in the command bar when the
//      child is maximized.  To add additional functionality,
//      derive your own class from CTabbedMDICommandBarCtrlImpl.
//      
//     
//
// Written by Daniel Bowen (dbowen@es.com)
// Copyright (c) 2002 Daniel Bowen.
//
// Depends on CustomTabCtrl.h written by Bjarke Viksoe (bjarke@viksoe.dk)
//  with the modifications by Daniel Bowen
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
// 2002/11/21: Daniel Bowen
// - CMDITabOwner - 
//   * ModifyTabStyles for use before CMDITabOwner is created as a window
// - CTabbedMDIClient -
//   * Expose SetDrawFlat and GetDrawFlat
//   * Updates so that drawing flat draws correctly
// - CTabbedMDIChildWindowImpl - 
//   * Handle WM_MOUSEACTIVATE, and call MDIActivate if
//     MDI child is not already active.  This solves the problem
//     where you have a dialog window as the view of the MDI child,
//     and clicking on a child control (edit box, etc.) doesn't
//     give focus or activate the MDI child (or activate the app
//     if its not active).  This code will ideally make its
//     way into future versions of WTL.
//
// 2002/09/25: Daniel Bowen
// - CTabbedMDICommandBarCtrl -
//   * Break out CTabbedMDICommandBarCtrl into CTabbedMDICommandBarCtrlImpl
//     and CTabbedMDICommandBarCtrl (just like CMDICommandBarCtrlImpl
//     and CMDICommandBarCtrl).
//     You can derive from CTabbedMDICommandBarCtrlImpl
//     if you would like to extend functionality (such as providing
//     your own handling of WM_MDISETMENU).  See the commented out
//     sample class after CTabbedMDICommandBarCtrl.
// - CMDITabOwner -
//   * Expose "SetTabStyles" and "GetTabStyles" so that you can change
//     the tab related styles to something different than the default
// - UWM_MDI... messages -
//   * The TabbedMDI related classes use a handful of custom window
//     messages.  These messages are guaranteed to be unique across
//     all windows for a particular windows session by using
//     RegisterWindowMessage
//
//     Initially, these message IDs were declared as static variables
//     and initialized here in the header.
//     However, that gave them "internal linkeage".  Essentially,
//     this meant that there were multiple copies of these variables.
//     In Visual C++ 6, there was also a bug that caused the variables
//     not to be initialized properly in release mode. So the class
//     CTabbedMDIChildWindowImpl ensured their initialization in its
//     constructor.  The problem was, only the version of the variables
//     in the same translation unit got initialized by doing it this way.
//
//     These variables are now declared using __declspec(selectany)
//     so that there will not be multiple copies.  RegisterWindowMessage
//     for each message ID is now called in the constructor of the
//     struct "RegisterTabbedMDIMessages". If you are using _ATL_MIN_CRT
//     or define _TABBEDMDI_MESSAGES_EXTERN_REGISTER, then you must
//     have an instance of the "RegisterTabbedMDIMessages" struct in
//     one .cpp file.  Otherwise, a global instance of the struct will
//     be declared in this header file (whose constructor will be called
//     by the CRT at load time).  If you are not referencing
//     TabbedMDI.h in stdafx.h, and have multiple translation units
//     including it, then you'll need to do it the 
//     _TABBEDMDI_MESSAGES_EXTERN_REGISTER way.  Also, if you do
//     use _ATL_MIN_CRT, you will get a warning unless you define
//     _TABBEDMDI_MESSAGES_NO_WARN_ATL_MIN_CRT
//
// 2002/08/26: Daniel Bowen
// - CTabbedMDIClient -
//   * Add new template parameter "TTabOwner" to allow easily
//     changing the tab owner class used
//
// 2002/06/26: Daniel Bowen
// - CTabbedMDIFrameWindowImpl - Expose "TClient" and "TTabCtrl".
// - CMDITabOwner - Expose "TTabCtrl".
// - CTabbedMDIClient -
//   * Expose "TTabCtrl"
//   * New method "GetTabOwner" to get a reference to the C++ class
//     instance implementing the MDI tab owner window.
//   * New method "UseMDIChildIcon" to specify that you want the MDI
//     tabs to include the document icon for the MDI child on the
//     corresponding tab
// 
// 2002/06/12: Daniel Bowen
// - Publish codeproject article.  For history prior
//   to the release of the article, please see the article
//   and the section "Note to previous users"

#ifndef __WTL_TABBED_MDI_H__
#define __WTL_TABBED_MDI_H__

#pragma once

#ifndef __cplusplus
	#error Tabbed MDI requires C++ compilation (use a .cpp suffix)
#endif

#ifndef __ATLAPP_H__
	#error TabbedMDI.h requires atlapp.h to be included first
#endif

#ifndef __ATLWIN_H__
	#error TabbedMDI.h requires atlwin.h to be included first
#endif

#ifndef __ATLFRAME_H__
	#error TabbedFrame.h requires atlframe.h to be included first
#endif

#ifndef __ATLCTRLW_H__
	#error TabbedFrame.h requires atlctrlw.h to be included first
#endif

#ifndef __CUSTOMTABCTRL_H__
	#error TabbedMDI.h requires CustomTabCtrl.h to be included first
#endif

#ifndef __WTL_TABBED_FRAME_H__
	#error TabbedMDI.h requires TabbedFrame.h to be included first
#endif

#if _WTL_VER < 0x0700
	#error TabbedMDI.h requires WTL 7.0 or higher
#endif

#define UWM_MDICHILDTABTEXTCHANGE_MSG      _T("UWM_MDICHILDTABTEXTCHANGE_MSG-5DAD28E1-C961-11d5-8BDA-00500477589F")
#define UWM_MDICHILDTABTOOLTIPCHANGE_MSG   _T("UWM_MDICHILDTABTOOLTIPCHANGE_MSG-5DAD28E3-C961-11d5-8BDA-00500477589F")
#define UWM_MDICHILDACTIVATIONCHANGE_MSG   _T("UWM_MDICHILDACTIVATIONCHANGE_MSG-5DAD28E5-C961-11d5-8BDA-00500477589F")
#define UWM_MDICHILDMAXIMIZED_MSG          _T("UWM_MDICHILDMAXIMIZED_MSG-5DAD28E7-C961-11d5-8BDA-00500477589F")
#define UWM_MDICHILDUNMAXIMIZED_MSG        _T("UWM_MDICHILDUNMAXIMIZED_MSG-5DAD28E9-C961-11d5-8BDA-00500477589F")
#define UWM_MDICHILDSHOWTABCONTEXTMENU_MSG _T("UWM_MDICHILDSHOWTABCONTEXTMENU_MSG-5DAD28EB-C961-11d5-8BDA-00500477589F")

__declspec(selectany) UINT UWM_MDICHILDTABTEXTCHANGE = 0;
__declspec(selectany) UINT UWM_MDICHILDTABTOOLTIPCHANGE = 0;
__declspec(selectany) UINT UWM_MDICHILDACTIVATIONCHANGE = 0;
__declspec(selectany) UINT UWM_MDICHILDMAXIMIZED = 0;
__declspec(selectany) UINT UWM_MDICHILDUNMAXIMIZED = 0;
__declspec(selectany) UINT UWM_MDICHILDSHOWTABCONTEXTMENU = 0;

struct RegisterTabbedMDIMessages
{
	RegisterTabbedMDIMessages()
	{
		UWM_MDICHILDTABTEXTCHANGE =      ::RegisterWindowMessage(UWM_MDICHILDTABTEXTCHANGE_MSG);
		UWM_MDICHILDTABTOOLTIPCHANGE =   ::RegisterWindowMessage(UWM_MDICHILDTABTOOLTIPCHANGE_MSG);
		UWM_MDICHILDACTIVATIONCHANGE =   ::RegisterWindowMessage(UWM_MDICHILDACTIVATIONCHANGE_MSG);
		UWM_MDICHILDMAXIMIZED =          ::RegisterWindowMessage(UWM_MDICHILDMAXIMIZED_MSG);
		UWM_MDICHILDUNMAXIMIZED =        ::RegisterWindowMessage(UWM_MDICHILDUNMAXIMIZED_MSG);
		UWM_MDICHILDSHOWTABCONTEXTMENU = ::RegisterWindowMessage(UWM_MDICHILDSHOWTABCONTEXTMENU_MSG);
	}
};

#if defined(_ATL_MIN_CRT) || defined(_TABBEDMDI_MESSAGES_EXTERN_REGISTER)
	// With _ATL_MIN_CRT, we don't get global constructors and destructors,
	// which is the out-of-the-box way we register the tabbed MDI window messages,
	// so the client needs to register these messages.
	// _TABBEDMDI_MESSAGES_EXTERN_REGISTER also skips declaring a global
	// right here - see the note below.

	#if defined(_ATL_MIN_CRT) && !defined(_TABBEDMDI_MESSAGES_NO_WARN_ATL_MIN_CRT)
		#pragma message("By defining _ATL_MIN_CRT, you are responsible for registering the custom TabbedMDI window messages")
		#pragma message(" (Define _TABBEDMDI_MESSAGES_NO_WARN_ATL_MIN_CRT to not see this message again)")
	#endif
#else
	// Global struct, whose constructor will get called when the executable image gets loaded
	// (the CRT makes sure global objects get constructed and destructed)

	// If you are getting "already defined" errors because of including TabbedMDI.h
	// in multiple translation units, you can either change it so that you
	// reference this file only from stdafx.h, or you can declare
	// "_TABBEDMDI_MESSAGES_EXTERN_REGISTER" before including TabbedMDI.h
	// and then have an instance of the "RegisterTabbedMDIMessages"
	// structure in a translation unit that has reference to this file.
	RegisterTabbedMDIMessages g_RegisterTabbedMDIMessages;
#endif


/////////////////////////////////////////////////////////////////////////////
//
// CTabbedMDIFrameWindowImpl
//
/////////////////////////////////////////////////////////////////////////////

template <
	class T,
	class TClient = CTabbedMDIClient< CDotNetTabCtrl<CTabViewTabItem> >,
	class TBase = CMDIWindow,
	class TWinTraits = CFrameWinTraits>
class ATL_NO_VTABLE CTabbedMDIFrameWindowImpl :
	public CMDIFrameWindowImpl<T, TBase, TWinTraits >
{
public:
	// Expose the type of MDI client
	typedef TClient TClient;
	// Expose the type of tab control
	typedef TClient::TTabCtrl TTabCtrl;
	
// Member variables
protected:
	TClient m_tabbedClient;

// Methods
public:
	// Either call the normal "CreateMDIClient"

	HWND CreateMDIClient(HMENU hWindowMenu = NULL, UINT nID = ATL_IDW_CLIENT, UINT nFirstChildID = ATL_IDM_FIRST_MDICHILD)
	{
		HWND hWndClient = baseClass::CreateMDIClient(hWindowMenu, nID, nFirstChildID);

		this->SubclassMDIClient();

		return hWndClient;
	}

	// Or, after creating the client, call SubclassMDIClient
	BOOL SubclassMDIClient()
	{
		ATLASSERT(m_hWndMDIClient && ::IsWindow(m_hWndMDIClient));

		return m_tabbedClient.SubclassWindow(m_hWndMDIClient);
	}

	void SetTabOwnerParent(HWND hWndTabOwnerParent)
	{
		m_tabbedClient.SetTabOwnerParent(hWndTabOwnerParent);
	}

	HWND GetTabOwnerParent() const
	{
		return m_tabbedClient.GetTabOwnerParent();
	}

// Message Handling
public:
	typedef CTabbedMDIFrameWindowImpl< T, TBase, TWinTraits >	thisClass;
	typedef CMDIFrameWindowImpl<T, TBase, TWinTraits >	baseClass;
	BEGIN_MSG_MAP(thisClass)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()
};

template <class T, class TBase = CMDIWindow, class TWinTraits = CMDIChildWinTraits>
class ATL_NO_VTABLE CTabbedMDIChildWindowImpl : public CMDIChildWindowImpl<T, TBase, TWinTraits>
{
// Member variables
protected:
	bool m_bMaximized;

// Constructors
public:
	CTabbedMDIChildWindowImpl() :
		m_bMaximized(false)
	{
		ATLASSERT(UWM_MDICHILDACTIVATIONCHANGE != 0 && "The TabbedMDI Messages didn't get registered properly");
	}

// Overrides ofCMDIChildWindowImpl
public:

	// NOTE: CreateEx also calls this (through T*)
	HWND Create(HWND hWndParent, WTL::_U_RECT rect = NULL, LPCTSTR szWindowName = NULL,
			DWORD dwStyle = 0U, DWORD dwExStyle = 0U,
			UINT nMenuID = 0U, LPVOID lpCreateParam = NULL)
	{
		// NOTE: hWndParent is going to become m_hWndMDIClient
		//  in CMDIChildWindowImpl::Create
		ATLASSERT(::IsWindow(hWndParent));

		BOOL bMaximizeNewChild = (WS_MAXIMIZE == (T::GetWndStyle(dwStyle) & WS_MAXIMIZE));
		if(bMaximizeNewChild == FALSE)
		{
			// If WS_MAXIMIZE wasn't requested, check if the currently
			//  active MDI child (if there is one) is maximized.  If so,
			//  maximize the new child window to match.
			::SendMessage(hWndParent, WM_MDIGETACTIVE, 0, (LPARAM)&bMaximizeNewChild);
		}

		if(bMaximizeNewChild == TRUE)
		{
			::SendMessage(hWndParent, WM_SETREDRAW, FALSE, 0);

			// We'll ShowWindow(SW_SHOWMAXIMIZED) instead of using
			//  WS_MAXIMIZE (which would cause visual anomolies in some cases)
			dwStyle = (T::GetWndStyle(dwStyle) & ~WS_MAXIMIZE);
		}


		HWND hWnd = baseClass::Create(hWndParent, rect, szWindowName, dwStyle, dwExStyle, nMenuID, lpCreateParam);

		if(bMaximizeNewChild == TRUE)
		{
			::ShowWindow(hWnd, SW_SHOWMAXIMIZED);

			::SendMessage(hWndParent, WM_SETREDRAW, TRUE, 0);
			::RedrawWindow(hWndParent, NULL, NULL,
				//A little more forceful if necessary: RDW_ERASE | RDW_INVALIDATE | RDW_UPDATENOW | RDW_ALLCHILDREN);
				RDW_INVALIDATE | RDW_ALLCHILDREN);
		}

		if(hWnd != NULL && ::IsWindow(m_hWnd) && !::IsChild(hWnd, ::GetFocus()))
			::SetFocus(hWnd);

		return hWnd;
	}

// Methods
public:
	void SetTitle(LPCTSTR sNewTitle, BOOL bUpdateTabText = TRUE)
	{
		this->SetWindowText(sNewTitle);

		if(bUpdateTabText)
		{
			::SendMessage(m_hWndMDIClient, UWM_MDICHILDTABTEXTCHANGE, (WPARAM)m_hWnd, (LPARAM)sNewTitle);
		}
	}

	void SetTabText(LPCTSTR sNewTabText)
	{
		::SendMessage(m_hWndMDIClient, UWM_MDICHILDTABTEXTCHANGE, (WPARAM)m_hWnd, (LPARAM)sNewTabText);
	}

	void SetTabToolTip(LPCTSTR sNewToolTip)
	{
		::SendMessage(m_hWndMDIClient, UWM_MDICHILDTABTOOLTIPCHANGE, (WPARAM)m_hWnd, (LPARAM)sNewToolTip);
	}

// Message Handling
public:

	typedef CTabbedMDIChildWindowImpl< T, TBase, TWinTraits >	thisClass;
	typedef CMDIChildWindowImpl<T, TBase, TWinTraits >	baseClass;
	BEGIN_MSG_MAP(thisClass)
		MESSAGE_HANDLER(WM_MDIACTIVATE, OnMDIActivate)
		MESSAGE_HANDLER(WM_SETTEXT, OnSetText)
		MESSAGE_HANDLER(WM_SIZE, OnSize)
		MESSAGE_HANDLER(WM_SETFOCUS, OnSetFocus)
		MESSAGE_HANDLER(WM_MOUSEACTIVATE, OnMouseActivate)
		MESSAGE_HANDLER(UWM_MDICHILDSHOWTABCONTEXTMENU, OnShowTabContextMenu)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	LRESULT OnMDIActivate(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		HWND hWndDeactivating = (HWND)wParam;
		HWND hWndActivating = (HWND)lParam;

		if(m_hWnd == hWndActivating)
		{
			::SendMessage(m_hWndMDIClient, UWM_MDICHILDACTIVATIONCHANGE, (WPARAM)m_hWnd, 0);
		}

		bHandled = FALSE;
		return 0;
	}

	LRESULT OnSetText(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		//::SendMessage(m_hWndMDIClient, UWM_MDICHILDTABTEXTCHANGE, (WPARAM)m_hWnd, lParam);

		bHandled = FALSE;
		return 0;
	}

	LRESULT OnSize(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		if(wParam == SIZE_MAXIMIZED && m_bMaximized == false)
		{
			m_bMaximized = true;
			::SendMessage(m_hWndMDIClient, UWM_MDICHILDMAXIMIZED, (WPARAM)m_hWnd, lParam);
		}
		else if(wParam != SIZE_MAXIMIZED && m_bMaximized == true)
		{
			m_bMaximized = false;
			::SendMessage(m_hWndMDIClient, UWM_MDICHILDUNMAXIMIZED, (WPARAM)m_hWnd, lParam);
		}

		bHandled = FALSE;
		return 0;
	}

	LRESULT OnMouseActivate(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		HWND hWndActive = this->MDIGetActive();
		if(m_hWnd != hWndActive)
		{
			this->MDIActivate(m_hWnd);
		}

		return MA_ACTIVATE;
	}

	LRESULT OnSetFocus(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		// NOTE: ::IsWindowVisible(m_hWndClient) will be false if
		//  the frame is maximized.  So just use "IsWindow" instead.
		if(m_hWndClient != NULL && ::IsWindow(m_hWndClient))
		{
			::SetFocus(m_hWndClient);
		}

		bHandled = FALSE;
		return 1;
	}

	LRESULT OnShowTabContextMenu(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		bHandled = TRUE;

		// NOTE: Your deriving class doesn't have to do the context menu
		//  this way (show the system menu, use TPM_RETURNCMD, etc.)
		//  It can do whatever it wants.  This is just the "fall through"
		//  implementation if you don't want to specialize.

		// NOTE: The sender of the message has already handled the case
		//  when launching the context menu from the keyboard
		//  (translating -1,-1 into a usable position)

		// We use essentially the same code as CMDICommandBarCtrl::OnNcLButtonDown
		//  (for when it handles the menu for the maximized child when clicking
		//  on the document icon to the left of the menus).
		// NOTE: On Windows 2000 and 98 and later, we'll get bitmaps in the menu.
		//  Also note that when running on NT 4 or Win 95, CMDICommandBarCtrl::OnNcLButtonDown
		//  will fail to show the system menu at all because it doesn't like
		//  the real definition of TPM_VERPOSANIMATION.  To avoid that
		//  problem, we won't even try to use TPM_VERPOSANIMATION.
		CMenuHandle menu = this->GetSystemMenu(FALSE);

		UINT uRet = (UINT)menu.TrackPopupMenu(TPM_LEFTBUTTON | TPM_VERTICAL | TPM_LEFTALIGN | TPM_TOPALIGN | TPM_RETURNCMD, 
			GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam), m_hWnd);

		// See MSDN about "GetSystemMenu".  Returns codes greater than
		//  0xF000 (which happens to be SC_SIZE) are sent with WM_SYSCOMMAND
		if(uRet >= SC_SIZE)
		{
			::PostMessage(m_hWnd, WM_SYSCOMMAND, uRet, 0L);
		}

		return 0;
	}
};


/////////////////////////////////////////////////////////////////////////////
//
// CMDITabOwner
//
/////////////////////////////////////////////////////////////////////////////

template< class TTabCtrl >
class CMDITabOwner :
	public CWindowImpl<CMDITabOwner>,
	public CCustomTabOwnerImpl<CMDITabOwner, TTabCtrl>
{
public:
	// Expose the type of tab control
	typedef TTabCtrl TTabCtrl;

// Member variables
protected:
	HWND m_hWndMDIClient;
	DWORD m_nTabStyles;

// Constructors
public:
	CMDITabOwner() :
		m_hWndMDIClient(NULL),
		m_nTabStyles(CTCS_TOOLTIPS | CTCS_BOLDSELECTEDTAB | CTCS_SCROLL | CTCS_CLOSEBUTTON)
	{
		ATLASSERT(UWM_MDICHILDACTIVATIONCHANGE != 0 && "The TabbedMDI Messages didn't get registered properly");
	}

// Methods
public:
	void SetMDIClient(HWND hWndMDIClient)
	{
		m_hWndMDIClient = hWndMDIClient;
	}

	void SetTabStyles(DWORD nTabStyles)
	{
		m_nTabStyles = nTabStyles;
	}

	bool GetTabStyles(void) const
	{
		return m_nTabStyles;
	}

	void ModifyTabStyles(DWORD dwRemove, DWORD dwAdd)
	{
		DWORD dwNewStyle = (m_nTabStyles & ~dwRemove) | dwAdd;
		if(m_nTabStyles != dwNewStyle)
		{
			m_nTabStyles = dwNewStyle;
		}
	}

// Message Handling
public:
	DECLARE_WND_CLASS(_T("MdiTabOwner"))

	BEGIN_MSG_MAP(CMDITabOwner)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy)
		MESSAGE_HANDLER(WM_SIZE, OnSize)
		MESSAGE_HANDLER(WM_ERASEBKGND, OnEraseBackground)
		MESSAGE_HANDLER(WM_CONTEXTMENU, OnContextMenu)
		NOTIFY_CODE_HANDLER(CTCN_DELETEITEM, OnDeleteItem)
		NOTIFY_CODE_HANDLER(CTCN_SELCHANGING, OnSelChanging)
		NOTIFY_CODE_HANDLER(CTCN_SELCHANGE, OnSelChange)
		NOTIFY_CODE_HANDLER(CTCN_CLOSE, OnTabClose)

		// NOTE: CCustomTabCtrl derived classes no longer
		//  need notifications reflected.
		// REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	LRESULT OnCreate(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		// "baseClass::OnCreate()"
		LRESULT lRet = DefWindowProc(uMsg, wParam, lParam);
		bHandled = TRUE;

		CreateTabWindow(m_hWnd, rcDefault, m_nTabStyles);

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

	LRESULT OnEraseBackground(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		if(m_TabCtrl)
		{
			// Let the tabs do all the drawing as flicker-free as possible
			return 1;
		}

		bHandled = FALSE;
		return 0;
	}

	LRESULT OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
	{
		RECT rect = {0};
		GetClientRect(&rect);

		m_TabCtrl.SetWindowPos(NULL, &rect, SWP_NOZORDER | SWP_NOACTIVATE);
		m_TabCtrl.UpdateLayout();

		bHandled = TRUE;

		return 0;
	}

	LRESULT OnContextMenu(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		bHandled = TRUE;

		int nIndex = -1;

		POINT ptPopup = {GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam)};
		if(ptPopup.x == -1 && ptPopup.y == -1)
		{
			nIndex = m_TabCtrl.GetCurSel();
			RECT rect = {0};
			if(nIndex >= 0)
			{
				// If there is a selected item, popup the menu under the node,
				// if not, pop it up in the top left of the tree view
				m_TabCtrl.GetItemRect(nIndex, &rect);
			}
			::MapWindowPoints(m_hWnd, NULL, (LPPOINT)&rect, 2);
			ptPopup.x = rect.left;
			ptPopup.y = rect.bottom;
		}
		else
		{
			POINT ptClient = ptPopup;
			::MapWindowPoints(NULL, m_hWnd, &ptClient, 1);
			CTCHITTESTINFO tchti = { 0 };
			tchti.pt = ptClient;
			//If we become templated, pT->HitTest(&tchti);
			nIndex = m_TabCtrl.HitTest(&tchti);
		}

		if( nIndex >= 0 )
		{
			TTabCtrl::TItem* pItem = m_TabCtrl.GetItem(nIndex);

			HWND hWndChild = pItem->GetTabView();
			if(hWndChild != NULL)
			{
				::SendMessage(hWndChild, UWM_MDICHILDSHOWTABCONTEXTMENU, wParam, MAKELPARAM(ptPopup.x, ptPopup.y));
			}
		}

		return 0;
	}

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
		int nNewTab = m_TabCtrl.GetCurSel();

		if(nNewTab >= 0)
		{
			TTabCtrl::TItem* pItem = m_TabCtrl.GetItem(nNewTab);
			if(pItem->UsingTabView())
			{
				HWND hWndNew = pItem->GetTabView();
				HWND hWndOld = (HWND)::SendMessage(m_hWndMDIClient, WM_MDIGETACTIVE, 0, NULL);
				if( hWndNew != hWndOld )
				{
					// We don't want any flickering when switching the active child
					//  when the child is maximized (when its not maximized, there's no flicker).
					//  There's probably more than one way to do this, but how we do
					//  it is to turn off redrawing for the MDI client window,
					//  activate the new child window, turn redrawing back on for
					//  the MDI client window, and force a redraw (not just a simple
					//  InvalidateRect, but an actual RedrawWindow to include
					//  all the child windows ).
					//  It might be nice to just turn off/on drawing for the window(s)
					//  that need it, but if you watch the messages in Spy++,
					//  the default implementation of the MDI client is forcing drawing
					//  to be on for the child windows involved.  Turning drawing off
					//  for the MDI client window itself seems to solve this problem.
					//

					LRESULT nResult = 0;

					WINDOWPLACEMENT wpOld = {0};
					wpOld.length = sizeof(WINDOWPLACEMENT);
					::GetWindowPlacement(hWndOld, &wpOld);
					if(wpOld.showCmd == SW_SHOWMAXIMIZED)
					{
						nResult = ::SendMessage(m_hWndMDIClient, WM_SETREDRAW, FALSE, 0);
					}

					nResult = ::SendMessage(m_hWndMDIClient, WM_MDIACTIVATE, (LPARAM)hWndNew, 0);

					WINDOWPLACEMENT wpNew = {0};
					wpNew.length = sizeof(WINDOWPLACEMENT);
					::GetWindowPlacement(hWndNew, &wpNew);
					if(wpNew.showCmd == SW_SHOWMINIMIZED)
					{
						::ShowWindow(hWndNew, SW_RESTORE);
					}

					if(wpOld.showCmd == SW_SHOWMAXIMIZED)
					{
						nResult = ::SendMessage(m_hWndMDIClient, WM_SETREDRAW, TRUE, 0);
						::RedrawWindow(m_hWndMDIClient, NULL, NULL,
							//A little more forceful if necessary: RDW_ERASE | RDW_INVALIDATE | RDW_UPDATENOW | RDW_ALLCHILDREN);
							RDW_INVALIDATE | RDW_ALLCHILDREN);
					}
				}
			}
		}

		bHandled = FALSE;
		return 0;
	}

	LRESULT OnTabClose(int /*idCtrl*/, LPNMHDR pnmh, BOOL& bHandled)
	{
		LPNMCTCITEM pnmCustomTab = (LPNMCTCITEM)pnmh;
		if(pnmCustomTab)
		{
			if(pnmCustomTab->iItem >= 0)
			{
				TTabCtrl::TItem* pItem = m_TabCtrl.GetItem(pnmCustomTab->iItem);
				if(pItem)
				{
					::PostMessage(pItem->GetTabView(), WM_SYSCOMMAND, SC_CLOSE, 0L);
				}
			}
		}
		bHandled = FALSE;
		return 0;
	}

// Overrides from CCustomTabOwnerImpl
public:

	void OnAddFirstTab()
	{
		if(this->IsWindowVisible() == FALSE)
		{
			RECT rcMDIClient;
			::GetWindowRect(m_hWndMDIClient, &rcMDIClient);
			::MapWindowPoints(NULL, ::GetParent(m_hWndMDIClient), (LPPOINT)&rcMDIClient, 2);

			this->ShowWindow(SW_SHOW);

			// the MDI client resizes and shows our window when
			//  handling messages related to SetWindowPos
			::SetWindowPos(
				m_hWndMDIClient, NULL,
				rcMDIClient.left, rcMDIClient.top,
				(rcMDIClient.right - rcMDIClient.left),(rcMDIClient.bottom - rcMDIClient.top),
				SWP_NOZORDER);
		}
	}

	void OnRemoveLastTab()
	{
		if(this->IsWindowVisible() == TRUE)
		{
			RECT rcTabs;
			m_TabCtrl.GetWindowRect(&rcTabs);
			::MapWindowPoints(NULL, m_TabCtrl.GetParent(), (LPPOINT)&rcTabs, 2);

			this->ShowWindow(SW_HIDE);

			RECT rcMDIClient;
			::GetWindowRect(m_hWndMDIClient, &rcMDIClient);
			::MapWindowPoints(NULL, ::GetParent(m_hWndMDIClient), (LPPOINT)&rcMDIClient, 2);

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

	void SetTabAreaHeight(int nNewTabAreaHeight)
	{
		if(m_nTabAreaHeight != nNewTabAreaHeight)
		{
			int nOldTabAreaHeight = m_nTabAreaHeight;

			m_nTabAreaHeight = nNewTabAreaHeight;

			//T* pT = static_cast<T*>(this);
			//pT->UpdateLayout();
			//Invalidate();

			if(this->IsWindowVisible() == TRUE)
			{

				RECT rcMDIClient;
				::GetWindowRect(m_hWndMDIClient, &rcMDIClient);
				::MapWindowPoints(NULL, this->GetParent(), (LPPOINT)&rcMDIClient, 2);

				// Don't ask me why these two lines are necessary.
				// Take these lines out if you want to
				// convince yourself that they are :-)
				rcMDIClient.top -= nOldTabAreaHeight;
				rcMDIClient.bottom += nOldTabAreaHeight;

				// The tab resize/reposition logic happens when handling WM_WINDOWPOSCHANGING.
				// If that ever changes, make the appropriate change here.
				::SetWindowPos(
					m_hWndMDIClient, NULL,
					rcMDIClient.left, rcMDIClient.top,
					(rcMDIClient.right - rcMDIClient.left),(rcMDIClient.bottom - rcMDIClient.top),
					SWP_NOZORDER | SWP_NOACTIVATE);
			}
		}
	}
};


/////////////////////////////////////////////////////////////////////////////
//
// CTabbedMDIClient
//
/////////////////////////////////////////////////////////////////////////////

template< class TTabCtrl = CDotNetTabCtrl<CTabViewTabItem>, class TTabOwner = CMDITabOwner<TTabCtrl> >
class CTabbedMDIClient : public CWindowImpl<CTabbedMDIClient, CWindow>
{
public:
	// Expose the type of tab control and tab owner
	typedef TTabCtrl TTabCtrl;
	typedef TTabOwner TTabOwner;

protected:
	typedef CWindowImpl<CTabbedMDIClient, CWindow> baseClass;
	typedef CTabbedMDIClient< TTabCtrl, TTabOwner > thisClass;

// Member variables
protected:
	HWND m_hWndTabOwnerParent;
	TTabOwner m_MdiTabOwner;
	BOOL m_bUseMDIChildIcon;
	bool m_bSubclassed;
	bool m_bDrawFlat;

// Constructors
public:
	CTabbedMDIClient() :
		m_hWndTabOwnerParent(NULL),
		m_bUseMDIChildIcon(FALSE),
		m_bSubclassed(false),
		m_bDrawFlat(false)
	{
		ATLASSERT(UWM_MDICHILDACTIVATIONCHANGE != 0 && "The TabbedMDI Messages didn't get registered properly");
		if(m_bDrawFlat)
		{
			m_MdiTabOwner.ModifyTabStyles(0,CTCS_FLATEDGE);
		}
	}

	virtual ~CTabbedMDIClient()
	{
		if(this->IsWindow() && m_bSubclassed)
		{
			this->UnsubclassWindow(TRUE);
		}
	}

// Methods
public:
	void SetTabOwnerParent(HWND hWndTabOwnerParent)
	{
		m_hWndTabOwnerParent = hWndTabOwnerParent;
	}

	HWND GetTabOwnerParent(void) const
	{
		return m_hWndTabOwnerParent;
	}

	TTabOwner& GetTabOwner(void)
	{
		return m_MdiTabOwner;
	}

	void UseMDIChildIcon(BOOL bUseMDIChildIcon = TRUE)
	{
		m_bUseMDIChildIcon = bUseMDIChildIcon;
	}

	void SetDrawFlat(bool bDrawFlat = true)
	{
		if(m_bDrawFlat!=bDrawFlat)
		{
			//ATLASSERT((m_hWnd==NULL) && "Please call SetDrawFlat before CreateWindow or SubclassWindow");
			m_bDrawFlat = bDrawFlat;
			if(m_bDrawFlat)
			{
				m_MdiTabOwner.ModifyTabStyles(0,CTCS_FLATEDGE);
			}
			else
			{
				m_MdiTabOwner.ModifyTabStyles(CTCS_FLATEDGE,0);
			}
		}
	}

	bool GetDrawFlat(void) const
	{
		return m_bDrawFlat;
	}

	BOOL SubclassWindow(HWND hWnd)
	{
		BOOL bSuccess = baseClass::SubclassWindow(hWnd);

		m_bSubclassed = true;

		InitTabs();

		m_MdiTabOwner.CalcTabAreaHeight();

		return bSuccess;
	}

	HWND UnsubclassWindow(BOOL bForce = FALSE)
	{
		m_bSubclassed = false;

		return baseClass::UnsubclassWindow(bForce);
	}

protected:
	void InitTabs()
	{
		if( !m_MdiTabOwner.IsWindow() )
		{
			if(m_hWndTabOwnerParent == NULL)
			{
				// If the tab owner's parent is not specified,
				// have the tabs as a sibling
				m_hWndTabOwnerParent = this->GetParent();
			}

			m_MdiTabOwner.Create(
				m_hWndTabOwnerParent, 
				rcDefault, NULL,
				WS_CHILD | WS_CLIPSIBLINGS | WS_CLIPCHILDREN // start out not visible
				);

			m_MdiTabOwner.SetMDIClient(m_hWnd);
		}
	}

// Message Handling
public:
	DECLARE_WND_SUPERCLASS(_T("TabbedMDIClient"), _T("MDIClient"))

	BEGIN_MSG_MAP(CTabbedMDIClient)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy)
		MESSAGE_HANDLER(WM_SETTINGCHANGE, OnSettingChange)
		MESSAGE_HANDLER(WM_WINDOWPOSCHANGING, OnWindowPosChanging)
		MESSAGE_HANDLER(WM_NCPAINT, OnNcPaint)
		//MESSAGE_HANDLER(WM_MDICREATE, OnMDICreate)
		MESSAGE_HANDLER(WM_MDIDESTROY, OnMDIDestroy)
		MESSAGE_HANDLER(UWM_MDICHILDACTIVATIONCHANGE, OnChildActivationChange)
		MESSAGE_HANDLER(UWM_MDICHILDTABTEXTCHANGE, OnChildTabTextChange)
		MESSAGE_HANDLER(UWM_MDICHILDTABTOOLTIPCHANGE, OnChildTabToolTipChange)
	END_MSG_MAP()

	LRESULT OnCreate(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		// "base::OnCreate()"
		LRESULT lRet = DefWindowProc(uMsg, wParam, lParam);
		bHandled = TRUE;

		InitTabs();

		m_MdiTabOwner.CalcTabAreaHeight();

		return lRet;
	}

	LRESULT OnDestroy(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		// Say that we didn't handle it so that anyone else
		//  interested gets to handle the message
		bHandled = FALSE;
		return 0;
	}

	LRESULT OnSettingChange(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		// Be sure tab gets message before we recalculate the tab area height
		//  so that it can adjust its font metrics first.
		// NOTE: This causes the tab to get the WM_SETTINGCHANGE message twice,
		//  but that's OK.
		m_MdiTabOwner.GetTabCtrl().SendMessage(uMsg, wParam, lParam);

		m_MdiTabOwner.CalcTabAreaHeight();

		bHandled = FALSE;
		return 0;
	}

	LRESULT OnWindowPosChanging(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		LPWINDOWPOS pWinPos = reinterpret_cast<LPWINDOWPOS>(lParam);
		if(pWinPos)
		{
			if( m_MdiTabOwner.IsWindow() )
			{
				//ATLTRACE(_T("Resizing MDI tab and MDI client\n"));
				int nTabAreaHeight = (m_MdiTabOwner.IsWindowVisible()) ? m_MdiTabOwner.GetTabAreaHeight() : 0;

				TTabCtrl& TabCtrl = m_MdiTabOwner.GetTabCtrl();
				DWORD dwStyle = TabCtrl.GetStyle();
				if(CTCS_BOTTOM == (dwStyle & CTCS_BOTTOM))
				{
					m_MdiTabOwner.SetWindowPos(
						NULL,
						pWinPos->x, pWinPos->y + (pWinPos->cy - nTabAreaHeight),
						pWinPos->cx, nTabAreaHeight,
						(pWinPos->flags & SWP_NOMOVE) | (pWinPos->flags & SWP_NOSIZE) | SWP_NOZORDER | SWP_NOACTIVATE);

					if((pWinPos->flags & SWP_NOSIZE) == 0)
					{
						pWinPos->cy -= nTabAreaHeight;
					}
				}
				else
				{
					m_MdiTabOwner.SetWindowPos(
						NULL,
						pWinPos->x, pWinPos->y,
						pWinPos->cx, nTabAreaHeight,
						(pWinPos->flags & SWP_NOMOVE) | (pWinPos->flags & SWP_NOSIZE) | SWP_NOZORDER | SWP_NOACTIVATE);

					if((pWinPos->flags & SWP_NOMOVE) == 0)
					{
						pWinPos->y += nTabAreaHeight;
					}
					if((pWinPos->flags & SWP_NOSIZE) == 0)
					{
						pWinPos->cy -= nTabAreaHeight;
					}
				}
			}
		}

		// "base::OnWindowPosChanging()"
		LRESULT lRet = DefWindowProc(uMsg, wParam, lParam);
		bHandled = TRUE;

		return lRet;
	}

	LRESULT OnNcPaint(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{

		if(m_bDrawFlat &&
			WS_EX_CLIENTEDGE == (this->GetExStyle() & WS_EX_CLIENTEDGE))
		{
			// When we have WS_EX_CLIENTEDGE and drawing "flat",
			// we'll paint the non-client edges ourself with a more flat look.
			// NOTE: If WS_EX_CLIENTEDGE ever takes up more than 2 pixels
			// on each edge, update the drawing code.

			CDC dc(this->GetWindowDC());
			if(dc)
			{
				RECT rcWindow;
				this->GetWindowRect(&rcWindow);
				::OffsetRect(&rcWindow, -rcWindow.left, -rcWindow.top);
				dc.DrawEdge(&rcWindow, EDGE_ETCHED, BF_FLAT|BF_RECT);
			}

			/*
			// Note: The documentation says the flags should be
			// DCX_WINDOW|DCX_INTERSECTRGN
			// but that wasn't working.
			// On http://freespace.virgin.net/james.brown7/tutorials/tips.htm
			// they mention you also need to OR in the flag "0x10000".
			CDC dc(this->GetDCEx((HRGN)wParam, DCX_WINDOW|DCX_INTERSECTRGN | 0x10000));
			if(dc)
			{
				RECT rcWindow;
				this->GetWindowRect(&rcWindow);
				::OffsetRect(&rcWindow, -rcWindow.left, -rcWindow.top);
				dc.DrawEdge(&rcWindow, EDGE_ETCHED, BF_FLAT|BF_RECT);
			}
			*/
			bHandled = TRUE;
		}
		else
		{
			bHandled = FALSE;
		}

		return 0;
	}

	LRESULT OnMDIDestroy(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		// NOTE: For symmetry, we could try to handle WM_MDICREATE
		//  for tab creation.  There are 2 reasons we don't:
		//   1. With WTL, the implementation doesn't use WM_MDICREATE,
		//      so it wouldn't be reliable to rely on it
		//   2. We don't need to.  Handling the change in child
		//      activation to display the tab, if its not there,
		//      creates, DisplayTab will create the corresponding tab. 

		// Remove the tab for the child being destroyed.
		//  Before removing the tab, we want the default happen,
		//  so that the MDI Client figures out who to activate next.
		//  If we removed the tab first, the "DeleteItem" on
		//  the tab wouldn't know the next best tab to select,
		//  and so if it's removing the tab that's currently selected,
		//  it changes the selection to 0.
		//  But by having the MDI client activate the child
		//  "next in line" (Z-Order), that child will be activated,
		//  and its tab will be selected, so that by the time we remove
		//  the tab for this child, the tab won't be selected.
		LRESULT lRet = DefWindowProc(uMsg, wParam, lParam);
		bHandled = TRUE;

		if(wParam != NULL)
		{
			m_MdiTabOwner.RemoveTab((HWND)wParam);
		}

		return 0;
	}

	LRESULT OnChildActivationChange(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		// NOTE: We'd like to just handle WM_MDIACTIVATE when sent to
		//  the MDI client to know when the active MDI child has changed.
		//  Unfortunately, you don't HAVE to send WM_MDIACTIVATE to the
		//  MDI Client to change the active child (such as clicking
		//  a different MDI child to activate it).
		//  However, the MDI *child* will *always* receive WM_MDIACTIVATE.
		//  In fact, the child losing focus gets WM_MDIACTIVATE, 
		//  and then the child gaining focus gets WM_MDIACTIVATE
		//  (the "deactivating" and "activating" windows are sent as
		//  the WPARAM and LPARAM both times).
		//  So we'll make the "activating" child window responsible for
		//  sending us a message (UWM_MDICHILDACTIVATIONCHANGE)
		//  when it gets a WM_MDIACTIVATE message.  We'll use this
		//  to display the tab (switching the selected tab to the
		//  corresponding tab, or creating a tab for the child and
		//  setting it as selected)

		if(wParam != NULL)
		{
			m_MdiTabOwner.DisplayTab((HWND)wParam, TRUE, m_bUseMDIChildIcon);
		}

		return 0;
	}

	LRESULT OnChildTabTextChange(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
	{
		// NOTE: Our direct children are Frame windows.
		//  We make the MDI child responsible for sending us a
		//  message (UWM_MDICHILDTABTEXTCHANGE) if they want to
		//  update the text of the corresponding tab.

		if(wParam != NULL)
		{
			m_MdiTabOwner.UpdateTabText((HWND)wParam, (LPCTSTR)lParam);
		}

		return 0;
	}

	LRESULT OnChildTabToolTipChange(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
	{
		//  We make the MDI child responsible for sending us a
		//  message (UWM_MDICHILDTABTOOLTIPCHANGE) with the tooltip
		//  if that tooltip is something different than the tab text.

		if(wParam != NULL)
		{
			m_MdiTabOwner.UpdateTabToolTip((HWND)wParam, (LPCTSTR)lParam);
		}

		return 0;
	}
};


template <class T, class TBase = CCommandBarCtrlBase, class TWinTraits = CControlWinTraits>
class ATL_NO_VTABLE CTabbedMDICommandBarCtrlImpl : public CMDICommandBarCtrlImpl<T, TBase, TWinTraits>
{
protected:
	typedef CTabbedMDICommandBarCtrlImpl thisClass;
	typedef CMDICommandBarCtrlImpl<T, TBase, TWinTraits> baseClass;
	typedef CCommandBarCtrlImpl<T, TBase, TWinTraits> grandparentClass;

// Extended data
protected:
	bool m_bUseMaxChildDocIconAndFrameCaptionButtons:1;

// Constructors
public:
	CTabbedMDICommandBarCtrlImpl() :
		m_bUseMaxChildDocIconAndFrameCaptionButtons(true)
	{
	}

// Public methods
public:
	void UseMaxChildDocIconAndFrameCaptionButtons(bool bUseMaxChildDocIconAndFrameCaptionButtons = true)
	{
		m_bUseMaxChildDocIconAndFrameCaptionButtons = bUseMaxChildDocIconAndFrameCaptionButtons;
	}

// Overrides
public:
	void GetSystemSettings()
	{
		baseClass::GetSystemSettings();
		m_cxLeft += 4;
	}

// Message Handling
public:
	BEGIN_MSG_MAP(thisClass)

		if(m_bUseMaxChildDocIconAndFrameCaptionButtons)
		{
			CHAIN_MSG_MAP(baseClass)
		}
		else
		{
			CHAIN_MSG_MAP(grandparentClass)
		}

	ALT_MSG_MAP(1)		// Parent window messages

		if(m_bUseMaxChildDocIconAndFrameCaptionButtons)
		{
			CHAIN_MSG_MAP_ALT(baseClass, 1)
		}
		else
		{
			CHAIN_MSG_MAP_ALT(grandparentClass, 1)
		}

	ALT_MSG_MAP(2)		// MDI client window messages

		MESSAGE_HANDLER(WM_MDISETMENU, OnMDISetMenu)

		if(m_bUseMaxChildDocIconAndFrameCaptionButtons)
		{
			MESSAGE_HANDLER(WM_MDIDESTROY, OnMDIDestroy)
			MESSAGE_HANDLER(UWM_MDICHILDMAXIMIZED, OnChildMaximized)
			MESSAGE_HANDLER(UWM_MDICHILDUNMAXIMIZED, OnChildUnMaximized)
		}
		// NOTE: If future implementations of CMDICommandBarCtrlImpl
		//  add MDI client related handlers for ALT_MSG_MAP(2),
		//  either add them here or chain to the base
	ALT_MSG_MAP(3)		// Message hook messages

		// NOTE: We don't want to depend on the command bar's
		//  hooked messages for telling us about maximization
		//  changes.  We can do the job with the normal
		//  MDI messages and our special MDI messages
		//MESSAGE_RANGE_HANDLER(0, 0xFFFF, OnAllHookMessages)
		CHAIN_MSG_MAP_ALT(grandparentClass, 3)

	END_MSG_MAP()

// MDI client window message handlers
	LRESULT OnMDISetMenu(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
	{
		m_wndMDIClient.DefWindowProc(uMsg, NULL, lParam);
		HMENU hOldMenu = GetMenu();
		BOOL bRet = AttachMenu((HMENU)wParam);
		bRet;
		ATLASSERT(bRet);
		return (LRESULT)hOldMenu;
	}

	LRESULT OnMDIDestroy(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		HWND hWndChild = (HWND)wParam;

		if(m_hWndChildMaximized == hWndChild)
		{
			bool bMaxOld = m_bChildMaximized;
			HWND hWndChildMaximizedOld = m_hWndChildMaximized;

			m_bChildMaximized = false;
			m_hWndChildMaximized = NULL;
			m_hIconChildMaximized = NULL;

			RefreshMaximizedState((bMaxOld != m_bChildMaximized), (hWndChildMaximizedOld != m_hWndChildMaximized));
		}

		bHandled = FALSE;
		return 0;
	}

	LRESULT OnChildMaximized(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		HWND hWndChild = (HWND)wParam;

		bool bMaxOld = m_bChildMaximized;
		HWND hWndChildMaximizedOld = m_hWndChildMaximized;

		m_bChildMaximized = true;

		if(m_hWndChildMaximized != hWndChild)
		{
			CWindow wnd = m_hWndChildMaximized = hWndChild;
			m_hIconChildMaximized = wnd.GetIcon(FALSE);
			if(m_hIconChildMaximized == NULL)	// no icon set with WM_SETICON, get the class one
			{
				m_hIconChildMaximized = (HICON) ::GetClassLong(wnd, GCL_HICONSM);
				if(m_hIconChildMaximized == NULL)
				{
					m_hIconChildMaximized = (HICON) ::GetClassLong(wnd, GCL_HICON);
				}
			}
		}

		RefreshMaximizedState((bMaxOld != m_bChildMaximized), (hWndChildMaximizedOld != m_hWndChildMaximized));

		bHandled = FALSE;
		return 0;
	}

	LRESULT OnChildUnMaximized(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		HWND hWndChild = (HWND)wParam;

		if(m_hWndChildMaximized == hWndChild)
		{
			bool bMaxOld = m_bChildMaximized;
			HWND hWndChildMaximizedOld = m_hWndChildMaximized;

			m_bChildMaximized = false;
			m_hWndChildMaximized = NULL;
			m_hIconChildMaximized = NULL;

			RefreshMaximizedState((bMaxOld != m_bChildMaximized), (hWndChildMaximizedOld != m_hWndChildMaximized));
		}

		bHandled = FALSE;
		return 0;
	}

	void RefreshMaximizedState(bool bMaximizeChanged, bool bChildChanged)
	{
		// NOTE: This code comes out of CMDICommandBarCtrlImpl::OnAllHookMessages.
		//  If the base implementation changes, reflect those changes here.
		if(bMaximizeChanged)
		{
#ifdef _CMDBAR_EXTRA_TRACE
			ATLTRACE2(atlTraceUI, 0, "MDI CmdBar - All messages hook change: m_bChildMaximized = %s\n", m_bChildMaximized ? "true" : "false");
#endif
			// assuming we are in a rebar, change our size to accomodate new state
			// we hope that if we are not in a rebar, nCount will be 0
			int nCount = (int)::SendMessage(GetParent(), RB_GETBANDCOUNT, 0, 0L);
			int cxDiff = (m_bChildMaximized ? 1 : -1) * (m_cxLeft + m_cxRight);
			for(int i = 0; i < nCount; i++)
			{ 
#if (_WIN32_IE >= 0x0500)
				REBARBANDINFO rbi = { sizeof(REBARBANDINFO), RBBIM_CHILD | RBBIM_CHILDSIZE | RBBIM_IDEALSIZE | RBBIM_STYLE };
				::SendMessage(GetParent(), RB_GETBANDINFO, i, (LPARAM)&rbi);
				if(rbi.hwndChild == m_hWnd)
				{
					if((rbi.fStyle & RBBS_USECHEVRON) != 0)
					{
						rbi.fMask = RBBIM_CHILDSIZE | RBBIM_IDEALSIZE;
						rbi.cxMinChild += cxDiff;
						rbi.cxIdeal += cxDiff;
						::SendMessage(GetParent(), RB_SETBANDINFO, i, (LPARAM)&rbi);
					}
					break;
				}
#else //!(_WIN32_IE >= 0x0500)
				REBARBANDINFO rbi = { sizeof(REBARBANDINFO), RBBIM_CHILD | RBBIM_CHILDSIZE };
				::SendMessage(GetParent(), RB_GETBANDINFO, i, (LPARAM)&rbi);
				if(rbi.hwndChild == m_hWnd)
				{
					rbi.fMask = RBBIM_CHILDSIZE;
					rbi.cxMinChild += cxDiff;
					::SendMessage(GetParent(), RB_SETBANDINFO, i, (LPARAM)&rbi);
					break;
				}
#endif //!(_WIN32_IE >= 0x0500)
			}
		}

		if(bMaximizeChanged || bChildChanged)
		{
			// force size change and redraw everything
			RECT rect;
			GetWindowRect(&rect);
			::MapWindowPoints(NULL, GetParent(), (LPPOINT)&rect, 2);
			SetRedraw(FALSE);
			SetWindowPos(NULL, 0, 0, 1, 1, SWP_NOZORDER | SWP_NOMOVE);
			SetWindowPos(NULL, &rect, SWP_NOZORDER | SWP_NOMOVE);
			SetRedraw(TRUE);
			RedrawWindow(NULL, NULL, RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW);
		}
	}
};

class CTabbedMDICommandBarCtrl : public CTabbedMDICommandBarCtrlImpl<CTabbedMDICommandBarCtrl>
{
public:
	DECLARE_WND_SUPERCLASS(_T("WTL_TabbedMDICommandBar"), GetWndClassName())
};

/*
class CAnotherTabbedMDICommandBarCtrl : public CTabbedMDICommandBarCtrlImpl<CAnotherTabbedMDICommandBarCtrl>
{
protected:
	typedef CAnotherTabbedMDICommandBarCtrl thisClass;
	typedef CTabbedMDICommandBarCtrlImpl<CAnotherTabbedMDICommandBarCtrl> baseClass;

public:
	DECLARE_WND_SUPERCLASS(_T("WTL_AnotherTabbedMDICommandBar"), GetWndClassName())

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

	LRESULT OnMDISetMenu(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		bHandled = TRUE;
		m_wndMDIClient.DefWindowProc(uMsg, NULL, lParam);
		HMENU hOldMenu = GetMenu();
		BOOL bRet = AttachMenu((HMENU)wParam);

		// Other stuff

		bRet;
		ATLASSERT(bRet);
		return (LRESULT)hOldMenu;
	}
};
*/

#endif // __WTL_TABBED_MDI_H__
