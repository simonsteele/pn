/**
 * @file ScintillaWTL.h
 * @brief Windows Template Library implementation of a Scintilla window.
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef scintillawtl_h__included
#define scintillawtl_h__included

#include "scintillaif.h"

/** 
 * Whether we're a full unicode build or not, we now need to force Scintilla to be
 * a unicode window to correctly support international character input.
 *
 * This is an almost exact copy of DECLARE_WND_SUPERCLASS with the type changed to
 * always be wide.
 */
#define DECLARE_WND_SUPERCLASSW(WndClassName, OrigWndClassName) \
static ATL::CWndClassInfoW& GetWndClassInfo() \
{ \
	static ATL::CWndClassInfoW wc = \
	{ \
		{ sizeof(WNDCLASSEX), 0, StartWindowProc, \
		  0, 0, NULL, NULL, NULL, NULL, NULL, WndClassName, NULL }, \
		OrigWndClassName, NULL, NULL, TRUE, 0, L"" \
	}; \
	return wc; \
}


/**
 * @class CScintillaWindow
 * @brief WTL Window implementation for Scintilla...
 */
template <class T, class TBase = CScintilla>
class CScintillaWindowImpl : public CWindowImpl< CScintillaWindowImpl<T,TBase> >, public TBase
{
public:
	CScintillaWindowImpl() : m_ctrlid(0) {}
	virtual ~CScintillaWindowImpl() {}

	DECLARE_WND_SUPERCLASSW(L"ScintillaWindowImpl", L"Scintilla")

	BOOL PreTranslateMessage(MSG* pMsg)
	{
		pMsg;
		return FALSE;
	}

	/**
	 * This is almost an exact copy of the base class Create method, altered only to ensure
	 * that all the window class registration and window creation is done in Unicode regardless
	 * of our build type.
	 */
	HWND Create(HWND hWndParent, _U_RECT rect = NULL, LPCTSTR szWindowName = NULL,
			DWORD dwStyle = 0, DWORD dwExStyle = 0,
			_U_MENUorID MenuOrID = 0U, LPVOID lpCreateParam = NULL)
	{
		if (T::GetWndClassInfo().m_lpszOrigName == NULL)
			T::GetWndClassInfo().m_lpszOrigName = L"ScintillaWindowImpl";
		ATOM atom = T::GetWndClassInfo().Register(&m_pfnSuperWindowProc);

		dwStyle = T::GetWndStyle(dwStyle);
		dwExStyle = T::GetWndExStyle(dwExStyle);

		// set caption
		if (szWindowName == NULL)
			szWindowName = T::GetWndCaption();

		//return CWindowImplBaseT< TBase, TWinTraits >::Create(hWndParent, rect, szWindowName,
		//	dwStyle, dwExStyle, MenuOrID, atom, lpCreateParam);
		BOOL result;
		ATLASSUME(m_hWnd == NULL);

		// Allocate the thunk structure here, where we can fail gracefully.
		result = m_thunk.Init(NULL,NULL);
		if (result == FALSE) {
			SetLastError(ERROR_OUTOFMEMORY);
			return NULL;
		}

		if(atom == 0)
			return NULL;

		_AtlWinModule.AddCreateWndData(&m_thunk.cd, this);

		if(MenuOrID.m_hMenu == NULL && (dwStyle & WS_CHILD))
			MenuOrID.m_hMenu = (HMENU)(UINT_PTR)this;
		if(rect.m_lpRect == NULL)
			rect.m_lpRect = &CScintillaWindowImpl<T,TBase>::rcDefault;

		m_ctrlid = HandleToLong(MenuOrID.m_hMenu);

		HWND hWnd = ::CreateWindowExW(dwExStyle, (LPCWSTR)MAKEINTATOM(atom), L"Scintilla",
			dwStyle, rect.m_lpRect->left, rect.m_lpRect->top, rect.m_lpRect->right - rect.m_lpRect->left,
			rect.m_lpRect->bottom - rect.m_lpRect->top, hWndParent, MenuOrID.m_hMenu,
			_AtlBaseModule.GetModuleInstance(), lpCreateParam);

		m_hWnd = hWnd;

		ATLASSUME(m_hWnd == hWnd);

		return hWnd;
	}

	BEGIN_MSG_MAP(CScintillaWindowImpl)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_CONTEXTMENU, OnContextMenu)
		MESSAGE_HANDLER(WM_SHOWWINDOW, OnShowWindow)
		// MESSAGE_HANDLER(OCM_NOTIFY, OnNotify)
		
		REFLECTED_NOTIFY_ID_HANDLER(m_ctrlid, OnNotify)
	END_MSG_MAP()

	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
	{
		m_scihWnd = m_hWnd;

		bHandled = FALSE;

		return 0;
	}

	LRESULT OnShowWindow(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
	{
		if((BOOL)wParam && (!Perform))
		{
			// Setup direct scintilla messages stuff...
			m_Pointer = (void *)SPerform(SCI_GETDIRECTPOINTER);
			Perform = (scmsgfn)SPerform(SCI_GETDIRECTFUNCTION);
			
			T* pT = static_cast<T*>(this);
			pT->OnFirstShow();
		}

		bHandled = FALSE;

		return 0;
	}

	LRESULT OnNotify(int idCtrl, LPNMHDR pnmh, BOOL& bHandled)
	{
		if (IsScintillaNotify((LPARAM)pnmh))
		{
			HandleNotify((LPARAM)pnmh);
			bHandled = TRUE;
		}
		else
		{
			bHandled = FALSE;
		}

		return 0;
	}

	HWND GetHwnd()
	{
		return m_hWnd;
	}

	LRESULT OnContextMenu(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
	{
		T* pT = static_cast<T*>(this);

		CPoint pt;
		pt.x = GET_X_LPARAM(lParam);
		pt.y = GET_Y_LPARAM(lParam); 

		if ((pt.x == (LONG)-1) && (pt.y == (LONG)-1)) 
		{
			// Caused by keyboard so display menu near caret
			int position = GetCurrentPos();

			pt.x = PointXFromPosition(position);
			pt.y = PointYFromPosition(position);

			ClientToScreen(&pt);

			pT->DoContextMenu(&pt);
		} 
		else 
		{
			CRect rcEditor;
			GetWindowRect(rcEditor);

			if (!rcEditor.PtInRect(pt)) 
			{
				bHandled = FALSE;
			}
			else
			{
				pT->DoContextMenu(&pt);
			}
		}
		
		return 0;
	}

protected:
	void DoContextMenu(CPoint* point)
	{
		// User Implementable...
	}

	void OnFirstShow()
	{
		// User Implementable through template type.
	}

private:
	DWORD m_ctrlid;
};

//template <class TBase = CScintilla>
//class CScintillaWindow : public CScintillaWindowImpl<CScintillaWindow<TBase>, TBase>
//{
//};

#endif