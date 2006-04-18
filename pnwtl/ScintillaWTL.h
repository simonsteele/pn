/**
 * @file ScintillaWTL.h
 * @brief Windows Template Library implementation of a Scintilla window.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * These classes rely on my more generic Scintilla wrapper class which can
 * be obtained from:
 *
 * http://www.pnotepad.org/scintilla/
 *
 * At some point, this may be made into a standalone module.
 */

#ifndef scintillawtl_h__included
#define scintillawtl_h__included

#include "scintillaif.h"
//#include "schemes.h"

/**
 * @class CScintillaWindow
 * @brief WTL Window implementation for Scintilla...
 */
template <class T, class TBase = CScintilla>
class CScintillaWindowImpl : public CWindowImpl< CScintillaWindowImpl >, public TBase
{
public:

	DECLARE_WND_SUPERCLASS("ScintillaWindowImpl", "Scintilla")

	BOOL PreTranslateMessage(MSG* pMsg)
	{
		pMsg;
		return FALSE;
	}

	BEGIN_MSG_MAP(CScintillaWindowImpl)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_CONTEXTMENU, OnContextMenu)
		MESSAGE_HANDLER(WM_SHOWWINDOW, OnShowWindow)
		MESSAGE_HANDLER(OCM_NOTIFY, OnNotify)
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
			OnFirstShow();
		}

		bHandled = FALSE;

		return 0;
	}

	LRESULT OnNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
	{
		if(IsScintillaNotify(lParam))
		{
			HandleNotify(lParam);
			bHandled = TRUE;
		}
		else
			bHandled = FALSE;

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

	void DoContextMenu(CPoint* point)
	{
		// User Implementable...
	}

protected:
	virtual void OnFirstShow(){}
};

template <class TBase = CScintilla>
class CScintillaWindow : public CScintillaWindowImpl<CScintillaWindow, TBase>
{
};

#endif