/////////////////////////////////////////////////////////////////////////////
// TabbedMDI.h - Classes that help implement a "Tabbed MDI" interface.
//
// Classes:
//   CTabbedMDIFrameWindowImpl - 
//      Instead of having CMainFrame inherit from
//      CMDIFrameWindowImpl, have it inherit from
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
//      etc.
//   CMDITabOwner -
//      The MDITabOwner is the parent of the actual tab window
//      (CDotNetTabCtrl), and sibling to the "MDI Client" window.
//      The tab window expects its parent to reflect notifications,
//      so the MDITabOwner does.  The tab owner also handles
//      displaying a context menu appropriate for the tab,
//      as well as changing the active MDI child when the
//      active tab changes.  It also does the real work of
//      hiding and showing the tabs.  It also handles adding,
//      removing, and renaming tabs based on an HWND.
//   CTabbedMDICommandBarCtrl -
//      In your MDI application, instead of using CMDICommandBarCtrl,
//      use CTabbedMDICommandBarCtrl.
//      
//     
//
// Written by Daniel Bowen (dbowen@es.com)
// Copyright (c) 2002 Daniel Bowen.
//
// Depends on CoolTabCtrls.h written by Bjarke Viksoe (bjarke@viksoe.dk)
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
// 2002/04/24: Daniel Bowen (DDB)
// - Rename:
//    UWM_MDICHILDTOOLTIPCHANGE => UWM_MDICHILDTABTOOLTIPCHANGE
//    UWM_MDICHILDTITLECHANGE => UWM_MDICHILDTABTEXTCHANGE
// - Ramon Casellas [casellas@infres.enst.fr] suggested
//    that the caption / window text / title of the MDI child
//    might not always be exactly what you want for the
//    corresponding tab's text.  I like that idea, so the
//    caption and tab text are no longer tied at the hip.
//    Changes to accomodate this:
//     * The tab text still starts out life with the
//       caption / window text of the frame (but it doesn't
//       have to stay that way).
//     * New "SetTabText" that the deriving MDI child can
//       call at anytime to update the text of the corresponding
//       MDI tab.
//     * SetTitle now takes an additional BOOL argument of
//       whether to update the tab text as well as set
//       the title / caption / window text
//     * CTabbedMDIChildWindowImpl::OnSetText doesn't send
//       UWM_MDICHILDTABTEXTCHANGE (UWM_MDICHILDTITLECHANGE) anymore.
//       You have to call SetTitle(text, TRUE) or SetTabText
//        explicitly now.
// - New "UWM_MDICHILDSHOWTABCONTEXTMENU" message sent to
//    the MDI *child* to request that the tab context menu be shown.
//    The default handler in CTabbedMDIChildWindowImpl simply
//    shows the "system menu" (GetSystemMenu).
//
// 2002/04/23: Daniel Bowen (DDB)
// - Tooltips:
//    * New UWM_MDICHILDTOOLTIPCHANGE message from MDI child to MDI client
//    * New "SetTabToolTip" for MDI child
// - New "CCoolTabItem" class used instead of TCITEM.
//   See "CoolTabCtrls.h" for an explanation.
// - ATL 7 compatibility issues pointed out to me by
//   Ramon Casellas [casellas@infres.enst.fr]
//
// 2002/04/22: Daniel Bowen (DDB)
// - Make the tab area height of the MDI tabs dependant on
//   the same metrics that the font of the tabs depends on.
// - Simon Steele (Simon Steele [s.steele@pnotepad.org]) made
//   an excellent suggestion about not hardcoding "CTabbedMDIClient"
//   in CTabbedMDIFrameWindowImpl.  Its now a template parameter.
// - CTabbedMDIClient and CMDITabOwner are now also templatized,
//   with a single template parameter specifying the the Tab Control type
//
// 2002/04/16: Daniel Bowen (DDB)
// - Enhanced CTabbedMDICommandBarCtrl. 
//   CTabbedMDICommandBarCtrl now gives you the option of whether
//   to use the "Document Icon" and frame caption buttons (min, close, etc,)
//   or not.  Simple call the cleverly named 
//   "UseMaxChildDocIconAndFrameCaptionButtons" method before calling
//   "SetMDIClient".
//      Background: In WTL 3.1, there wasn't any CMDICommandBarCtrl,
//   there was just CCommandBarCtrl.  With CCommandBarCtrl,
//   when the MDI child was maximized there was no document icon
//   in the menu or min/restore/close frame caption buttons in the
//   command bar on the right.  If you used this tabbed MDI stuff,
//   that was fine, because you could use the context menu for the
//   MDI tab to restore, minimize, etc.
//   With WTL 7.0, it forcefully asserts that you can't use
//   CCommandBarCtrl in an MDI application like you could in
///  WTL 3.1, and tells you to use CMDICommandBarCtrl.
//   CMDICommandBarCtrl adds the document icon and min/restore/close
//   buttons for maximized children.  CTabbedMDICommandBarCtrl
//   now gives you the choice of which style you want.

// 2002/04/15: Daniel Bowen (DDB)
// - CTabbedMDICommandBarCtrl
//    In case the frame window is living in a different image
//    (such as another DLL), instead of:
//      ::GetClassInfoEx(_Module.GetModuleInstance(), szClass, &wc);
//    use
//		m_hIconChildMaximized = (HICON) ::GetClassLong(wnd, GCL_HICONSM);
//    or
//		m_hIconChildMaximized = (HICON) ::GetClassLong(wnd, GCL_HICON);
// - Update comment at top of this file to mention CTabbedMDIChildWindowImpl
//    and CTabbedMDICommandBarCtrl.
// - Comment out a couple ATLTRACE statements
//
// 2002/04/10: Daniel Bowen (DDB)
// - Context Menu for MDI tab:
//     Now uses "GetSystemMenu" to get the window menu for the MDI child.
//     This is the same approach that CMDICommandBarCtrl::OnNcLButtonDown
//     uses when the MDI child is maximized and the user has clicked
//     on the document icon to the left of the menus.
//     If you run the application on Windows 2000 or 98 or greater,
//     there will be bitmaps in the menu.
//
//     Also note that when running on NT 4 or Win 95,
//     CMDICommandBarCtrl::OnNcLButtonDown will fail to show the
//     system menu at all because it doesn't like what it thinks is an
//     unknown flag - TPM_VERPOSANIMATION. To avoid that problem,
//     we won't even try to use TPM_VERPOSANIMATION.
//
// 2002/04/05: Daniel Bowen (DDB)
// - Updates related to WTL 7.0.
//   * WTL 7.0 wants you to use CMDICommandBarCtrl instead
//     of CCommandBarCtrl for MDI applications.
//     CMDICommandBarCtrl is decent, but there's some
//     anomolies even when its used in a plain vanilla app
//     (such as when the document icon doesn't update
//      depending on the active child).
//     Because we're subclassing the MDIClient and requiring
//     the MDI child frames to inherit from a special class,
//     there is now a "CTabbedMDICommandBarCtrl" that you
//     should use instead of CMDICommandBarCtrl.
// - CTabbedMDIChildWindowImpl:
//   * Add more messages from MDI child to MDI client:
//      UWM_MDICHILDMAXIMIZED
//      UWM_MDICHILDUNMAXIMIZED
//   * Override Create so that when a new child is created,
//     if the previous child was maximized, have the new
//     child be maximized as well (and prevent seeing a flickering
//     glimpse of the restored state of the new child)
//     See http://www.codeproject.com/useritems/WTLMDIChildMax.asp
//     for an explanation.
//   * Handle WM_SETFOCUS instead of depending on how
//     CFrameWindowImplBase handles it.  When the child is
//     maximized, ::IsWindowVisible(m_hWndClient) will return FALSE,
//     which results in the focus not being forwarded
//     to the "view" window.
//     See http://www.codeproject.com/useritems/WTLMDIChildMax.asp
//     for an explanation.
//
// 2002/02/07: Daniel Bowen (DDB)
// - New CCoolTabOwnerImpl MI class resulting from
//   refactoring due to commonality that existed between
//   CTabbedFrameImpl and CMDITabOwner.  Use this
//   class to help implement a windowing class that
//   will contain a CoolTabCtrl. This class lives in
//   TabbedFrame.h for now (see the header for more
//   or a description).
// - MDI child windows are now responsible for
//   a couple of things.  To have your MDI child windows
//   show up with corresponding tabs, be sure to
//   inherit from CTabbedMDIChildWindowImpl
//   instead of CMDIChildWindowImpl.
// - Redo a couple of implementation details of
//   how the MDI client keeps the MDI tabs and the
//   MDI child windows in synch.  See
//     OnMDIDestroy
//     OnChildActivationChange
//     OnChildTabTextChange
//   for details
// 
// 2002/01/31: Daniel Bowen (DDB)
// - Original Release
//

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

#ifndef __COOLTABCTRLS_H__
	#error TabbedMDI.h requires CoolTabCtrls.h to be included first
#endif

#ifndef __WTL_TABBED_FRAME_H__
	#error TabbedMDI.h requires TabbedFrame.h to be included first
#endif

#if _WTL_VER < 0x0700
	#error TabbedMDI.h requires WTL 7.0 or higher
#endif

#define UWM_MDICHILDTABTEXTCHANGE_MSG _T("UWM_MDICHILDTABTEXTCHANGE_MSG-5DAD28E1-C961-11d5-8BDA-00500477589F")
#define UWM_MDICHILDTABTOOLTIPCHANGE_MSG _T("UWM_MDICHILDTABTOOLTIPCHANGE_MSG-5DAD28E3-C961-11d5-8BDA-00500477589F")
#define UWM_MDICHILDACTIVATIONCHANGE_MSG _T("UWM_MDICHILDACTIVATIONCHANGE_MSG-5DAD28E5-C961-11d5-8BDA-00500477589F")
#define UWM_MDICHILDMAXIMIZED_MSG _T("UWM_MDICHILDMAXIMIZED_MSG-5DAD28E7-C961-11d5-8BDA-00500477589F")
#define UWM_MDICHILDUNMAXIMIZED_MSG _T("UWM_MDICHILDUNMAXIMIZED_MSG-5DAD28E9-C961-11d5-8BDA-00500477589F")
#define UWM_MDICHILDSHOWTABCONTEXTMENU_MSG _T("UWM_MDICHILDSHOWTABCONTEXTMENU_MSG-5DAD28EB-C961-11d5-8BDA-00500477589F")

// NOTE: In release mode, these static variables aren't
//  getting initialized by the following.  We'll guarantee they
//  get registered by doing it in the constructor of
//  CTabbedMDIChildWindowImpl if this initialization doesn't work
//  (at least in release mode it initializes it to 0)
static UINT UWM_MDICHILDTABTEXTCHANGE = ::RegisterWindowMessage(UWM_MDICHILDTABTEXTCHANGE_MSG);
static UINT UWM_MDICHILDTABTOOLTIPCHANGE = ::RegisterWindowMessage(UWM_MDICHILDTABTOOLTIPCHANGE_MSG);
static UINT UWM_MDICHILDACTIVATIONCHANGE = ::RegisterWindowMessage(UWM_MDICHILDACTIVATIONCHANGE_MSG);
static UINT UWM_MDICHILDMAXIMIZED = ::RegisterWindowMessage(UWM_MDICHILDMAXIMIZED_MSG);
static UINT UWM_MDICHILDUNMAXIMIZED = ::RegisterWindowMessage(UWM_MDICHILDUNMAXIMIZED_MSG);
static UINT UWM_MDICHILDSHOWTABCONTEXTMENU = ::RegisterWindowMessage(UWM_MDICHILDSHOWTABCONTEXTMENU_MSG);

/////////////////////////////////////////////////////////////////////////////
//
// CTabbedMDIFrameWindowImpl
//
/////////////////////////////////////////////////////////////////////////////

template <
	class T,
	class TClient = CTabbedMDIClient< CDotNetTabCtrl >,
	class TBase = CMDIWindow,
	class TWinTraits = CFrameWinTraits>
class ATL_NO_VTABLE CTabbedMDIFrameWindowImpl :
	public CMDIFrameWindowImpl<T, TBase, TWinTraits >
{
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
		// NOTE: In release mode, these static variables aren't
		//  getting initialized.  We'll guarantee they get registered
		//  by doing it here if they weren't properly initialized.
		//  (at least in release mode it initializes it to 0)

		if(UWM_MDICHILDTABTEXTCHANGE == 0)
		{
			UWM_MDICHILDTABTEXTCHANGE = ::RegisterWindowMessage(UWM_MDICHILDTABTEXTCHANGE_MSG);
		}
		if(UWM_MDICHILDTABTOOLTIPCHANGE == 0)
		{
			UWM_MDICHILDTABTOOLTIPCHANGE = ::RegisterWindowMessage(UWM_MDICHILDTABTOOLTIPCHANGE_MSG);
		}
		if(UWM_MDICHILDACTIVATIONCHANGE == 0)
		{
			UWM_MDICHILDACTIVATIONCHANGE = ::RegisterWindowMessage(UWM_MDICHILDACTIVATIONCHANGE_MSG);
		}
		if(UWM_MDICHILDMAXIMIZED == 0)
		{
			UWM_MDICHILDMAXIMIZED = ::RegisterWindowMessage(UWM_MDICHILDMAXIMIZED_MSG);
		}
		if(UWM_MDICHILDUNMAXIMIZED == 0)
		{
			UWM_MDICHILDUNMAXIMIZED = ::RegisterWindowMessage(UWM_MDICHILDUNMAXIMIZED_MSG);
		}
		if(UWM_MDICHILDSHOWTABCONTEXTMENU == 0)
		{
			UWM_MDICHILDSHOWTABCONTEXTMENU = ::RegisterWindowMessage(UWM_MDICHILDSHOWTABCONTEXTMENU_MSG);
		}
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
		BOOL bMaximized = FALSE;
		HWND hWndOld = (HWND)::SendMessage(hWndParent, WM_MDIGETACTIVE, 0, (LPARAM)&bMaximized);

		if(bMaximized == TRUE)
		{
			::SendMessage(hWndParent, WM_SETREDRAW, FALSE, 0);
		}

		HWND hWnd = baseClass::Create(hWndParent, rect, szWindowName, dwStyle, dwExStyle, nMenuID, lpCreateParam);

		if(bMaximized == TRUE)
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

		CPoint ptPopup(lParam);
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
			ptPopup.x, ptPopup.y, m_hWnd);

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

template< class TTab >
class CMDITabOwner :
	public CWindowImpl<CMDITabOwner>,
	public CCoolTabOwnerImpl<CMDITabOwner, TTab>
{
// Member variables
protected:
	HWND m_hWndMDIClient;

// Constructors
public:
	CMDITabOwner() :
		m_hWndMDIClient(NULL)
	{
		// Initialize members from CCoolTabOwnerImpl
		m_bTabAreaOnTop = true;
		m_tabs.SetBoldSelectedTab(true);
	}

// Methods
public:
	void SetMDIClient(HWND hWndMDIClient)
	{
		m_hWndMDIClient = hWndMDIClient;
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
		NOTIFY_CODE_HANDLER(TCN_DELETEITEM, OnDeleteItem)
		NOTIFY_CODE_HANDLER(TCN_SELCHANGING, OnSelChanging)
		NOTIFY_CODE_HANDLER(TCN_SELCHANGE, OnSelChange)
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	LRESULT OnCreate(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		// "baseClass::OnCreate()"
		LRESULT lRet = DefWindowProc(uMsg, wParam, lParam);
		bHandled = TRUE;

		CreateTabWindow(m_hWnd, rcDefault);

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
		if(m_tabs)
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

		m_tabs.SetWindowPos(NULL, &rect, SWP_NOZORDER | SWP_NOACTIVATE);
		m_tabs.UpdateLayout();

		bHandled = TRUE;

		return 0;
	}

	LRESULT OnContextMenu(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		bHandled = TRUE;

		int nIndex = -1;

		CPoint ptPopup(lParam);
		if(ptPopup.x == -1 && ptPopup.y == -1)
		{
			nIndex = m_tabs.GetCurSel();
			RECT rect = {0};
			if(nIndex >= 0)
			{
				// If there is a selected item, popup the menu under the node,
				// if not, pop it up in the top left of the tree view
				m_tabs.GetItemRect(nIndex, &rect);
			}
			this->ClientToScreen(&rect);
			ptPopup.x = rect.left;
			ptPopup.y = rect.bottom;
		}
		else
		{
			CPoint ptClient(ptPopup);
			this->ScreenToClient(&ptClient);
			TCHITTESTINFO tchti = { 0 };
			tchti.pt = ptClient;
			//If we become templated, pT->HitTest(&tchti);
			nIndex = m_tabs.HitTest(&tchti);
		}

		if( nIndex >= 0 )
		{
			CCoolTabItem* pItem = m_tabs.GetItem(nIndex);

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
		int nNewTab = m_tabs.GetCurSel();

		if(nNewTab >= 0)
		{
			CCoolTabItem* pItem = m_tabs.GetItem(nNewTab);
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

// Overrides from CCoolTabOwnerImpl
public:

	void ShowTabs()
	{
		if(this->IsWindowVisible() == FALSE)
		{
			CRect rect;
			::GetWindowRect(m_hWndMDIClient, &rect);

			this->ShowWindow(SW_SHOW);

			CPoint topLeft(rect.left, rect.top);
			::ScreenToClient(::GetParent(m_hWndMDIClient), &topLeft);


			// the MDI client resizes and shows our window when
			//  handling messages related to SetWindowPos
			::SetWindowPos(
				m_hWndMDIClient, NULL,
				topLeft.x,topLeft.y,
				rect.Width(),rect.Height(),
				SWP_NOZORDER);
		}
	}

	void HideTabs()
	{
		if(this->IsWindowVisible() == TRUE)
		{
			CRect rcTabs;
			m_tabs.GetClientRect(&rcTabs);

			this->ShowWindow(SW_HIDE);

			CRect rect;
			::GetWindowRect(m_hWndMDIClient, &rect);
			CPoint topLeft(rect.left, rect.top);
			::ScreenToClient(::GetParent(m_hWndMDIClient), &topLeft);

			// the MDI client resizes and shows our window when
			//  handling messages related to SetWindowPos

			// TODO: Is there a better way to do this?
			//  We're basically hiding the tabs and
			//  resizing the MDI client area to "cover up"
			//  where the tabs were
			if(m_bTabAreaOnTop)
			{
				::SetWindowPos(
					m_hWndMDIClient, NULL,
					topLeft.x, topLeft.y - rcTabs.Height(),
					rect.Width(), rect.Height() + rcTabs.Height(),
					SWP_NOZORDER);
			}
			else
			{
				::SetWindowPos(
					m_hWndMDIClient, NULL,
					topLeft.x, topLeft.y,
					rect.Width(), rect.Height() + rcTabs.Height(),
					SWP_NOZORDER);
			}
		}
	}
};


/////////////////////////////////////////////////////////////////////////////
//
// CTabbedMDIClient
//
/////////////////////////////////////////////////////////////////////////////

template< class TTab = CDotNetTabCtrl >
class CTabbedMDIClient : public CWindowImpl<CTabbedMDIClient, CWindow>
{
protected:
	typedef CWindowImpl<CTabbedMDIClient, CWindow> baseClass;

// Member variables
protected:
	HWND m_hWndTabOwnerParent;
	CMDITabOwner<TTab> m_MdiTabOwner;
	CRect m_rectTabs;
	bool m_bDrawFlat;
	BYTE m_nTabAreaHeight;

// Constructors
public:
	CTabbedMDIClient() :
		m_hWndTabOwnerParent(NULL),
		m_bDrawFlat(false),
		m_nTabAreaHeight(24)
	{
	}

// Methods
public:
	void SetTabOwnerParent(HWND hWndTabOwnerParent)
	{
		m_hWndTabOwnerParent = hWndTabOwnerParent;
	}

	HWND GetTabOwnerParent() const
	{
		return m_hWndTabOwnerParent;
	}

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
		TTab& tabs = m_MdiTabOwner.GetTabs();
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

	BOOL SubclassWindow(HWND hWnd)
	{
		BOOL bSuccess = baseClass::SubclassWindow(hWnd);

		InitTabs();

		UpdateTabAreaHeight();

		return bSuccess;
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
		MESSAGE_HANDLER(WM_STYLECHANGING, OnStyleChanging)
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

		UpdateTabAreaHeight();

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
		UpdateTabAreaHeight();

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
				int nTabAreaHeight = (m_MdiTabOwner.IsWindowVisible()) ? m_nTabAreaHeight : 0;

				if(m_MdiTabOwner.IsTabAreaOnTop())
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
				else
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
			}
		}

		// "base::OnWindowPosChanging()"
		LRESULT lRet = DefWindowProc(uMsg, wParam, lParam);
		bHandled = TRUE;

		return lRet;
	}

	LRESULT OnNcPaint(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{

		if(m_bDrawFlat)
		{
			// The MDI Client is going to continually set the Ex Style
			//  WS_EX_CLIENTEDGE
			// which we don't want.  Instead of constantly resetting this
			// (in WM_SIZE or WM_WINDOWPOSCHANGING), we'll just handle
			// WM_NCPAINT ourself and draw the flat frame we want.

			CDC dc(this->GetWindowDC());
			if(dc)
			{
				CRect rect;
				this->GetWindowRect(&rect);
				rect.OffsetRect(-rect.left, -rect.top);
				dc.FrameRect(&rect, GetSysColorBrush(COLOR_BTNSHADOW));
				rect.InflateRect(-1, -1);
				dc.FrameRect(&rect, GetSysColorBrush(COLOR_BTNFACE));
			}

			/*
			CRgn rgn(reinterpret_cast<HRGN>(wParam));

			// Note: The documentation says the flags should be
			// DCX_WINDOW|DCX_INTERSECTRGN
			// but that wasn't working.
			// On http://freespace.virgin.net/james.brown7/tutorials/tips.htm
			// they mention you also need to OR in the flag "0x10000".

			CDC dc(this->GetDCEx(rgn, DCX_WINDOW|DCX_INTERSECTRGN | 0x10000));
			if(dc)
			{
				CRect rc;
				rgn.GetRgnBox(&rc);
				dc.FillSolidRect(&rc, RGB(255,0,0));//GetSysColor(COLOR_BTNFACE));
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

	LRESULT OnStyleChanging(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		if(m_bDrawFlat)
		{
			LPSTYLESTRUCT ss = reinterpret_cast<LPSTYLESTRUCT>(lParam);
			if(ss)
			{
				switch(wParam)
				{
				case GWL_EXSTYLE:
					if( (ss->styleOld & WS_EX_CLIENTEDGE) == 0 &&
						(ss->styleNew & WS_EX_CLIENTEDGE) == WS_EX_CLIENTEDGE )
					{
						//ATLTRACE(_T("Resisting ExStyle change to WS_EX_CLIENTEDGE\n"));
						ss->styleNew &= ~(WS_EX_CLIENTEDGE);
					}
					break;
				case GWL_STYLE:
					break;
				}
			}
		}

		bHandled = FALSE;
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
			m_MdiTabOwner.DisplayTab((HWND)wParam);
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


class CTabbedMDICommandBarCtrl : public CMDICommandBarCtrlImpl<CTabbedMDICommandBarCtrl>
{
protected:
	typedef CTabbedMDICommandBarCtrl thisClass;
	typedef CMDICommandBarCtrlImpl<CTabbedMDICommandBarCtrl> baseClass;
	typedef CCommandBarCtrlImpl<CTabbedMDICommandBarCtrl> grandparentClass;

// Extended data
protected:
	bool m_bUseMaxChildDocIconAndFrameCaptionButtons:1;

// Constructors
public:
	CTabbedMDICommandBarCtrl() :
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
	DECLARE_WND_SUPERCLASS(_T("WTL_TabbedMDICommandBar"), baseClass::GetWndClassName())

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

#endif // __WTL_TABBED_MDI_H__
