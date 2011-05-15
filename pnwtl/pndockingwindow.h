#ifndef pndockingwindow_h__included
#define pndockingwindow_h__included

class CPNDockingWindow : public CPNDockingWindowT<CPNDockingWindow>
{
	typedef CPNDockingWindowT<CPNDockingWindow> baseClass;
public:
	CPNDockingWindow(LPCTSTR title) : baseClass(title)
	{}

	DECLARE_WND_CLASS_EX(_T("PNDockingWindow"), CS_DBLCLKS, COLOR_APPWORKSPACE)

	BEGIN_MSG_MAP(CPNDockingWindow)
		MESSAGE_HANDLER(WM_SIZE, OnSize)		
		MESSAGE_HANDLER(WM_ERASEBKGND, OnEraseBackground)
		MESSAGE_HANDLER(WM_SETFOCUS, OnSetFocus)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

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
					SWP_NOZORDER | SWP_NOACTIVATE | SWP_FRAMECHANGED);
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

	/*void SetMenuID(int nMenuID)
	{
		m_nMenuID = nMenuID;
	}

	int GetMenuID(void) const
	{
		return m_nMenuID;
	}*/

	virtual void OnFinalMessage(HWND /*hWnd*/)
	{
		// NOTE: This class is meant to be created with "new"
		delete this;
	}

protected:

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

	LRESULT OnSetFocus(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
	{
		if (m_hWndClient != NULL)
		{
			::SetFocus(m_hWndClient);
		}
		else
		{
			bHandled = FALSE;
		}

		return 0;
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

		// Else no client view is set, so let the default erase happen
		// (which will use the brush of the window class)
		bHandled = FALSE;
		return 0;

		//HDC hdc = (HDC)wParam;
		//if(hdc != NULL)
		//{
		//	RECT rect;
		//	::GetClipBox(hdc, &rect);
		//	::SetBkColor(hdc, ::GetSysColor(COLOR_APPWORKSPACE));
		//	::ExtTextOut(hdc, 0, 0, ETO_OPAQUE, &rect, NULL, 0, NULL);
		//}
		//return 1;
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

protected:
	bool m_bClientFlatOutline;
};
#endif // #ifndef pndockingwindow_h__included