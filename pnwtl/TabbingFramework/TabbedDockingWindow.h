// TabbedDockingWindow.h: interface for the CTabbedDockingWindow class.
//
// NOTE: This class depends on Sergey Klimov's docking window framework
//  and "TabbedFrame.h"
//
//////////////////////////////////////////////////////////////////////

#ifndef __TABBED_DOCKING_WINDOW_H__
#define __TABBED_DOCKING_WINDOW_H__

#pragma once

#ifndef __WTL_DW__EXTDOCKINGWINDOW_H__
	#error TabbedDockingWindow.h requires ExtDockingWindow.h to be included first
#endif
#ifndef __WTL_TABBED_FRAME_H__
	#error TabbedDockingWindow.h requires TabbedFrame.h to be included first
#endif

class CTabbedDockingWindow :
	public CTabbedFrameImpl<CTabbedDockingWindow, CDotNetTabCtrl<CTabViewTabItem>, dockwins::CTitleDockingWindowImpl< CTabbedDockingWindow,CWindow,dockwins::COutlookLikeTitleDockingWindowTraits> >
{
protected:
	typedef CTabbedDockingWindow thisClass;
	typedef CTabbedFrameImpl<CTabbedDockingWindow, CDotNetTabCtrl<CTabViewTabItem>, dockwins::CTitleDockingWindowImpl< CTabbedDockingWindow,CWindow,dockwins::COutlookLikeTitleDockingWindowTraits> > baseClass;

// Constructors
public:
	CTabbedDockingWindow(bool bReflectNotifications = true) :
		baseClass(bReflectNotifications)
	{
	}

// Message Handling
public:
	DECLARE_WND_CLASS(_T("CTabbedDockingWindow"))

	BOOL PreTranslateMessage(MSG* pMsg)
	{
		//if(baseClass::PreTranslateMessage(pMsg))
		//	return TRUE;

		//return m_view.PreTranslateMessage(pMsg);

		HWND hWndFocus = ::GetFocus();
		if(m_hWndActive != NULL && ::IsWindow(m_hWndActive) &&
			(m_hWndActive == hWndFocus || ::IsChild(m_hWndActive, hWndFocus)))
		{
			//active.PreTranslateMessage(pMsg);
			if(::SendMessage(m_hWndActive, WM_FORWARDMSG, 0, (LPARAM)pMsg))
			{
				return TRUE;
			}
		}

		return FALSE;
	}

	BEGIN_MSG_MAP(thisClass)
		MESSAGE_HANDLER(WM_SIZE, OnSize)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	LRESULT OnSize(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
	{
		if(wParam != SIZE_MINIMIZED)
		{
			//T* pT = static_cast<T*>(this);
			//pT->UpdateLayout();
			UpdateLayout();
		}
		bHandled = FALSE;
		return 1;
	}

// Overrideables
public:
	void UpdateBarsPosition(RECT& rect, BOOL bResizeBars = TRUE)
	{
	}
};

#ifdef DF_AUTO_HIDE_FEATURES

class CTabbedAutoHideDockingWindow :
	public dockwins::CBoxedDockingWindowImpl< CTabbedAutoHideDockingWindow,CWindow,dockwins::COutlookLikeExBoxedDockingWindowTraits>
{
protected:
	typedef CTabbedAutoHideDockingWindow	thisClass;
	typedef dockwins::CBoxedDockingWindowImpl< CTabbedAutoHideDockingWindow,CWindow,dockwins::COutlookLikeExBoxedDockingWindowTraits> baseClass;

// Member variables
protected:
	HWND m_hWndClient;
	bool m_bReflectNotifications;
	bool m_bClientFlatOutline;

// Constructors
public:
	CTabbedAutoHideDockingWindow(HWND hWndClient = NULL) : 
		m_hWndClient(hWndClient),
		m_bReflectNotifications(false),
		m_bClientFlatOutline(false)
	{
	}

// static public Methods:
public:
	static CTabbedAutoHideDockingWindow* CreateInstance(void)
	{
		return new CTabbedAutoHideDockingWindow;
	}

// Public Methods
public:
	void SetClient(HWND hWndClient)
	{
		m_hWndClient = hWndClient;

		if(	m_hWndClient && ::IsWindow(m_hWndClient) &&
			m_hWnd && ::IsWindow(m_hWnd))
		{
			// Set our small icon to the small icon of the client
			HICON hIcon = (HICON)::SendMessage(m_hWndClient, WM_GETICON, ICON_SMALL, 0L);
			if(hIcon==NULL)
			{
				hIcon = (HICON)::GetClassLongPtr(m_hWndClient, GCLP_HICONSM);
			}
			if(hIcon)
			{
				this->SetIcon(hIcon, ICON_SMALL);
			}

			if(m_bClientFlatOutline)
			{
				DWORD dwExStyle = (DWORD)::GetWindowLong(m_hWndClient, GWL_EXSTYLE);
				dwExStyle &= ~(WS_EX_CLIENTEDGE);
				::SetWindowLong(m_hWndClient, GWL_EXSTYLE, dwExStyle);
			}

			// Resize the client to fill our client area
			RECT rect;
			this->GetClientRect(&rect);

			if(m_bClientFlatOutline)
			{
				::SetWindowPos(m_hWndClient, NULL, rect.left+1, rect.top+1,
					rect.right - rect.left-2, rect.bottom - rect.top-2,
					SWP_NOZORDER | SWP_NOACTIVATE);
			}
			else
			{
				::SetWindowPos(m_hWndClient, NULL, rect.left, rect.top,
					rect.right - rect.left, rect.bottom - rect.top,
					SWP_NOZORDER | SWP_NOACTIVATE);
			}
		}
	}

	HWND GetClient(void)
	{
		return m_hWndClient;
	}

	void SetReflectNotifications(bool bReflectNotifications = true)
	{
		m_bReflectNotifications = bReflectNotifications;
	}

	bool GetReflectNotifications(void) const
	{
		return m_bReflectNotifications;
	}

	void SetClientFlatOutline(bool bFlat = true)
	{
		if(m_bClientFlatOutline!=bFlat)
		{
			ATLASSERT((m_hWndClient==NULL) && "Please call SetClientFlatOutline before setting client");
			m_bClientFlatOutline = bFlat;
		}
	}

	bool GetClientFlatOutline(void) const
	{
		return m_bClientFlatOutline;
	}

// Message Handling
public:
    DECLARE_WND_CLASS(_T("CTabbedAutoHideDockingWindow"))

	virtual void OnFinalMessage(HWND /*hWnd*/)
	{
		// NOTE: This class is meant to be created with "new"
		delete this;
	}

	BOOL PreTranslateMessage(MSG* pMsg)
	{
		//if(baseClass::PreTranslateMessage(pMsg))
		//	return TRUE;

		//return m_view.PreTranslateMessage(pMsg);

		HWND hWndFocus = ::GetFocus();
		if(m_hWndClient != NULL && ::IsWindow(m_hWndClient) &&
			(m_hWndClient == hWndFocus || ::IsChild(m_hWndClient, hWndFocus)))
		{
			//active.PreTranslateMessage(pMsg);
			if(::SendMessage(m_hWndClient, WM_FORWARDMSG, 0, (LPARAM)pMsg))
			{
				return TRUE;
			}
		}

		return FALSE;
	}

	BEGIN_MSG_MAP(thisClass)	
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_SIZE, OnSize)		
		MESSAGE_HANDLER(WM_ERASEBKGND, OnEraseBackground)
		MESSAGE_HANDLER(WM_SETFOCUS, OnSetFocus)
		CHAIN_MSG_MAP(baseClass)
		if(m_bReflectNotifications)
		{
			REFLECT_NOTIFICATIONS()
		}
	END_MSG_MAP()

	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
	{
		bHandled = FALSE;
		return 0;
	}

	LRESULT OnSize(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
	{
		if(wParam != SIZE_MINIMIZED )
		{
			// resize client window
			if(m_hWndClient != NULL)
			{
				RECT rect;
				this->GetClientRect(&rect);

				if(m_bClientFlatOutline)
				{
					::SetWindowPos(m_hWndClient, NULL, rect.left+1, rect.top+1,
						rect.right - rect.left-2, rect.bottom - rect.top-2,
						SWP_NOZORDER | SWP_NOACTIVATE);
				}
				else
				{
					::SetWindowPos(m_hWndClient, NULL, rect.left, rect.top,
						rect.right - rect.left, rect.bottom - rect.top,
						SWP_NOZORDER | SWP_NOACTIVATE);
				}
			}
		}
		bHandled = FALSE;
		return 1;
	}

	LRESULT OnEraseBackground(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
	{
		if(m_hWndClient != NULL)
		{
			if(m_bClientFlatOutline)
			{
				// Paint a flat outline
				HDC hdc = (HDC)wParam;
				if(hdc != NULL)
				{
					RECT rcClient={0};
					this->GetClientRect(&rcClient);
					::FrameRect(hdc,&rcClient,::GetSysColorBrush(COLOR_BTNSHADOW));
				}
			}

			// view will paint itself
			return 1;
		}

		// Else no client view is set, so at least fill in the background with something

		HDC hdc = (HDC)wParam;
		if(hdc != NULL)
		{
			RECT rect;
			::GetClipBox(hdc, &rect);
			::SetBkColor(hdc, ::GetSysColor(COLOR_APPWORKSPACE));
			::ExtTextOut(hdc, 0, 0, ETO_OPAQUE, &rect, NULL, 0, NULL);
		}
		return 1;
	}

	LRESULT OnSetFocus(UINT, WPARAM, LPARAM, BOOL& bHandled)
	{
		if(m_hWndClient != NULL && ::IsWindowVisible(m_hWndClient))
			::SetFocus(m_hWndClient);

		bHandled = FALSE;
		return 1;
	}

// "Overridden" from base class
public:
	void OnDocked(HDOCKBAR hBar,bool bHorizontal)
	{
		DWORD dwStyle = GetWindowLong(GWL_STYLE)&(~WS_SIZEBOX);		
		SetWindowLong( GWL_STYLE, dwStyle);

		baseClass::OnDocked(hBar,bHorizontal);
	}
	void OnUndocked(HDOCKBAR hBar)
	{
		DWORD dwStyle = GetWindowLong(GWL_STYLE) | WS_SIZEBOX;
		SetWindowLong( GWL_STYLE , dwStyle);
		
		baseClass::OnUndocked(hBar);
	}
};

#endif //DF_AUTO_HIDE_FEATURES

#endif
