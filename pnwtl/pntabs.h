#ifndef pntabs_h__included
#define pntabs_h__included

#if (_ATL_VER >= 0x0700)
	#include <atlcoll.h>
#endif

#include "TabbingFramework\atlgdix.h"
#include "TabbingFramework\CustomTabCtrl.h"
#include "TabbingFramework\DotNetTabCtrl.h"
#include "TabbingFramework\TabbedFrame.h"
#include "TabbingFramework\TabbedMDI.h"

/**
 * @class CPNMDIClient
 * @brief Add extra MDI plumbing to the TabbedMDIClient Framework.
 *
 * In order to maintain other lists of windows such as one in the
 * windows list control, what better way than to use the framework
 * provided in tabbed MDI code.
 */
class CPNMDIClient : public CTabbedMDIClient< CDotNetTabCtrl<CTabViewTabItem> >
{
	typedef CTabbedMDIClient< CDotNetTabCtrl<CTabViewTabItem> > baseClass;

public:
	BEGIN_MSG_MAP(CPNMDIClient)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(UWM_MDICHILDACTIVATIONCHANGE, OnChildActivationChange)
		MESSAGE_HANDLER(UWM_MDICHILDTABTEXTCHANGE, OnChildTabTextChange)
		MESSAGE_HANDLER(WM_MDIDESTROY, OnMDIDestroy)
		MESSAGE_HANDLER(WM_LBUTTONDBLCLK, OnDblClick)		
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	BOOL SubclassWindow(HWND hWnd)
	{
		BOOL bSuccess = baseClass::SubclassWindow(hWnd);
		
		if(bSuccess)
			SetClassLong(m_hWnd, GCL_STYLE,
				GetClassLong(m_hWnd, GCL_STYLE) | CS_DBLCLKS);	

		return bSuccess;
	}

	LRESULT OnDblClick(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
	{
		// Forward the message
		SendMessage(GetParent(), WM_LBUTTONDBLCLK, wParam, lParam);
		return 0;
	}

	LRESULT OnMDIDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
	{
		SendMessage(GetParent(), PN_NOTIFY, 0, SCN_UPDATEUI);
		bHandled = FALSE;

		return 0;
	}

	LRESULT OnChildActivationChange(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
	{
		SendMessage(GetParent(), PN_NOTIFY, 0, SCN_UPDATEUI);

		bHandled = FALSE;
		
		return 0;
	}

	LRESULT OnChildTabTextChange(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
	{
		SendMessage(GetParent(), PN_NOTIFY, 0, SCN_UPDATEUI);

		bHandled = FALSE;

		return 0;
	}

};

template<class TPNFrame>
class CPNTabbedMDICommandBarCtrl : public CTabbedMDICommandBarCtrlImpl<CPNTabbedMDICommandBarCtrl>
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

protected:
		TPNFrame* m_pFrame;
		MDISetMenuFn m_pF;
};

#endif //#ifndef pntabs_h__included