/**
 * @file pndocking.h
 * @brief Docking Windows implementation for Programmers Notepad 2.
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef pndocking_h__included
#define pndocking_h__included

//#define DF_AUTO_HIDE_FEATURES

#ifdef DF_AUTO_HIDE_FEATURES
	#include <DWAutoHide.h>
#endif

#include <DockMisc.h>
#include <DockingFocus.h>
#include <DockingFrame.h>
#include <DockingBox.h>
#include <TabDockingBox.h>
#include <stg.h>

//#include <VC7LikeCaption.h>
//typedef dockwins::CVC7LikeBoxedDockingWindowTraits	CPNBoxedDockingWindowTraits;
typedef dockwins::COutlookLikeBoxedDockingWindowTraits	CPNBoxedDockingWindowTraits;

// These includes are to enable the state manager stuff.
#include <sstate.h>

class CPNWindowStateStorage : public sstate::IStorge
{
public:
	CPNWindowStateStorage()
	{
		m_path = PNSK_INTERFACE;
		m_path += PNSK_DEFGUI;
	}

	CPNWindowStateStorage(const tstring& configName)
	{
		m_path = PNSK_INTERFACE;
		if(configName.size())
		{
			m_path += _T("\\");
			m_path += configName;
		}
		else
		{
			m_path += PNSK_DEFGUI;
		}
	}

	virtual ~CPNWindowStateStorage(void)
	{
	}

	virtual long Create(IStorge& parent, LPCTSTR name, Modes mode)
	{
		m_path = static_cast<CPNWindowStateStorage&>(parent).m_path + _T("\\");
		m_path += name;
		return ERROR_SUCCESS;
	}

	virtual long Open(IStorge& parent,LPCTSTR name,Modes mode)
	{
		m_path = static_cast<CPNWindowStateStorage&>(parent).m_path + _T("\\");
		m_path += name;
		return ERROR_SUCCESS;
	}

	virtual long SetString(LPCTSTR name,LPCTSTR data)
	{
		assert(data);
		OPTIONS->Set(m_path.c_str(), name, data);
		return ERROR_SUCCESS;
	}

	virtual long GetString(LPCTSTR name, LPTSTR data, size_t& size)
	{
		if(size == 0)
		{
			tstring val = OPTIONS->Get(m_path.c_str(), name, _T(""));
			if(val.size())
			{
				size = val.size();
				return ERROR_MORE_DATA;
			}
			else
			{
				return ERROR_NO_DATA;
			}
		}
		else
		{
			tstring val = OPTIONS->Get(m_path.c_str(), name, _T(""));
			_tcsncpy(data, val.c_str(), size);
			data[size-1] = _T('\0');
			
			return ERROR_SUCCESS;
		}
	}

private:
	CPNWindowStateStorage(const CPNWindowStateStorage&);
	CPNWindowStateStorage& operator=(const CPNWindowStateStorage&);
private:
	//HKEY m_key;
	tstring m_path;
};


/**
 * @brief Customised CWindowStateMgr allowing named configs.
 */
class CPNStateManager : public sstate::CWindowStateMgr<CPNWindowStateStorage>
{
	public:	
		void Initialize(const tstring& strMainKey, HWND hWnd, int nDefCmdShow = SW_SHOWNOACTIVATE)
		{
			m_pImpl->SetWindow(hWnd, nDefCmdShow);
			//m_strMainKey = strMainKey;
		}

		/*void Store(const tstring* strKey = NULL)
		{
			sstate::CMainState mstate(strKey ? *strKey : m_strMainKey);
			if(mstate.Store())
			{
				CRegKey key;
				DWORD dwDisposition;
				if(key.Create(mstate.MainKey(), sstate::ctxtMainWindow, REG_NONE,
					REG_OPTION_NON_VOLATILE, KEY_WRITE | KEY_READ, 
					NULL, &dwDisposition) == ERROR_SUCCESS)
					m_pImpl->Store(&mstate, key);
			}
		}

		bool Restore(const tstring* strKey = NULL)
		{
			sstate::CMainState mstate(strKey ? *strKey : m_strMainKey);
			CRegKey key;
			if(mstate.Restore() && (key.Open(mstate.MainKey(), 
				sstate::ctxtMainWindow, KEY_READ) == ERROR_SUCCESS))
			{
				m_pImpl->Restore(&mstate, key);
				return true;
			}
			else
			{
				m_pImpl->RestoreDefault();
				return false;
			}
		}
		*/
};

// 1 
//	public CTabbedFrameImpl<CPNDockingWindow, CDotNetTabCtrl<CTabViewTabItem>, 
		//dockwins::CTitleDockingWindowImpl< CPNDockingWindow, CWindow, dockwins::COutlookLikeTitleDockingWindowTraits> >

// 2
	//public dockwins::CBoxedDockingWindowImpl<T, CWindow, dockwins::CVC7LikeBoxedDockingWindowTraits >

/**
 * @brief This is the base class for all docking windows in Programmers Notepad 2.
 */
template <class T>
class /*ATL_NO_VTABLE*/ CPNDockingWindowT : public dockwins::CBoxedDockingWindowImpl<T,
                CWindow, CPNBoxedDockingWindowTraits >
{
	typedef dockwins::CBoxedDockingWindowImpl<T, CWindow, CPNBoxedDockingWindowTraits> baseClass;
	typedef CPNDockingWindowT<T> thisClass;

	public:
		CPNDockingWindowT(LPCTSTR name)
		{
			m_hWndClient = NULL;
			m_name = name;
		}

		HWND Create(HWND hDockingFrameWnd, RECT& rcPos, LPCTSTR szWindowName = NULL,
			DWORD dwStyle = 0, DWORD dwExStyle = 0,
			UINT nID = 0, LPVOID lpCreateParam = NULL)
		{
			// Set up a default size, thus fixing bug #1065665
			m_rcUndock.CopyRect(&rcPos);

			return baseClass::Create(hDockingFrameWnd, rcPos, szWindowName, 
				dwStyle, dwExStyle, nID, lpCreateParam);
		}

		// Prevent Hide from de-activating the current window. This is a bit kludgy.
		virtual bool Hide()
		{
			bool bRes = true;
			HWND hWndActive = ::GetFocus();
			
			if(IsDocking())
			{
				bRes=GetDockingPosition(&m_pos);
				assert(bRes);
				if(bRes)
					bRes=Float(&m_rcUndock, SWP_HIDEWINDOW | SWP_NOACTIVATE); // Added SWP_NOACTIVATE
				assert(bRes);
			}
			else
				m_pos.hdr.hBar=HNONDOCKBAR;

			::SetFocus(hWndActive);

			return (bRes && ShowWindow(SW_HIDE));
		}

		BEGIN_MSG_MAP(thisClass)
			MESSAGE_HANDLER(WM_SETFOCUS, OnSetFocus)
			CHAIN_MSG_MAP(baseClass)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()

	protected:
		LRESULT OnSetFocus(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
		{
			bHandled = FALSE;
			HWND hWndParent = GetParent();

			::SetWindowPos(hWndParent, m_hWnd, 0,0,0,0, SWP_NOSIZE | SWP_NOACTIVATE | SWP_NOMOVE);

			if(m_hWndClient != NULL && ::IsWindowVisible(m_hWndClient))
				::SetFocus(m_hWndClient);

			return 0;
		}

		HWND m_hWndClient;
		tstring m_name;
};

// Get a slightly slimmer splitter - much nicer.
#ifdef DF_AUTO_HIDE_FEATURES
	typedef dockwins::CDockingFrameTraitsT< dockwins::COutlookLikeAutoHidePaneTraits,dockwins::CSimpleSplitterBar<3>,
			WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN | WS_CLIPSIBLINGS,
			WS_EX_APPWINDOW | WS_EX_WINDOWEDGE> CPNDockingFrameTraits;
#else
	typedef dockwins::CDockingFrameTraitsT< dockwins::CSimpleSplitterBar<3>,
			WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN | WS_CLIPSIBLINGS,
			WS_EX_APPWINDOW | WS_EX_WINDOWEDGE> CPNDockingFrameTraits;
#endif

/**
 * @brief This class combines the Tabbed MDI framework with the docking windows framework
 */
template <class T, 
		  class TBase = CMDIWindow, 		  
		  class TWinTraits = CPNDockingFrameTraits >
class ATL_NO_VTABLE CPNDockingTabbedMDIFrameWindow : 
	public dockwins::CDockingFrameImplBase< T, CTabbedMDIFrameWindowImpl< T , CPNMDIClient, TBase, TWinTraits> ,TWinTraits >
{
public:
	DECLARE_WND_CLASS(_T("CPNDockingTabbedMDIFrameWindow"))
};

#include "pndockingwindow.h"

#endif //#ifndef pndocking_h__included