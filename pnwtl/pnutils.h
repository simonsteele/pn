/**
 * @file pnutils.h
 * @brief Utility classes such as MRU Lists etc.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef pnutils_h__included
#define pnutils_h__included

#include "ssmenus.h"

/**
 * @class CMRUList
 * Some inspiration taken from CRecentDocumentList <atlmisc.h>
 */
class CMRUList
{
	protected:
		struct _entry
		{
			TCHAR* pszData;

			_entry()
			{
				pszData = NULL;
			}

			_entry(const _entry& e)
			{
				pszData = NULL;
				*this = e;
			}

			~_entry()
			{
				if(pszData)
					delete [] pszData;
			}

			bool operator==(const _entry& e) const
			{ return (lstrcmpi(pszData, e.pszData) == 0); }

			_entry& operator = (const _entry& e)
			{
				if(pszData)
					delete [] pszData;
				pszData = new TCHAR[_tcslen(e.pszData)+1];
				_tcscpy(pszData, e.pszData);
				return *this;
			}
		};

	public:
		CMRUList(int size = 10);
		~CMRUList();

		void SetSize(int size);

		void AddEntry(LPCTSTR data);

		LPCTSTR GetEntry(int index);

		void SetRegistryKey(LPCTSTR key, bool load = true);

		void SaveToRegistry();
		void LoadFromRegistry();

	protected:		
		void Resize();

		CSimpleArray<_entry>	m_entries;
		CString					m_regkey;
		int						m_iMaxSize;
		UINT					m_iBase;
};

/**
 * @class CMRUMenu
 * A menu version of CMRUList
 */
class CMRUMenu : public CMRUList
{
	public:
		CMRUMenu(UINT baseCmd, int size = 10);
		~CMRUMenu();

		void UpdateMenu();

		operator HMENU();

	protected:
		UINT		m_iBase;
		TCHAR*		m_szEmpty;
		CSPopupMenu	m_Menu;
};

/**
 * @class CDropDownButton
 * Most of the credit for this class goes to the collective authors of CColorButton
 * which can be found at http://www.codeproject.com/wtl/wtlcolorbutton.asp
 * Who'd have thought that to have a themed button with a drop-down arrow on it would
 * require such a ridiculous amount of code. Gotta love Windows programming.
 */
class CDropDownButton : public CWindowImpl <CDropDownButton>,  public CThemeImpl <CDropDownButton>
{
	public:
		BEGIN_MSG_MAP(CDropDownButton)
			CHAIN_MSG_MAP (CThemeImpl <CDropDownButton>)
			MESSAGE_HANDLER (OCM__BASE + WM_DRAWITEM, OnDrawItem)
			MESSAGE_HANDLER (WM_MOUSEMOVE, OnMouseMove)
			MESSAGE_HANDLER (WM_MOUSELEAVE, OnMouseLeave)
		END_MSG_MAP()

		CDropDownButton()
		{
			m_bMouseOver = false;
		}

		BOOL SubclassWindow (HWND hWnd)
		{
			CWindowImpl <CDropDownButton>::SubclassWindow (hWnd);
			ModifyStyle (0, BS_OWNERDRAW);
			OpenThemeData (L"Button");
			return TRUE;
		}

	protected:
		CDC		m_MemDC;
		HBITMAP m_hOldBmp;
		bool	m_bMouseOver;

		LRESULT OnMouseLeave (UINT uMsg, WPARAM wParam, 
			LPARAM lParam, BOOL &bHandled) 
		{
			if (m_bMouseOver)
			{
				m_bMouseOver = false;
				InvalidateRect (NULL);
			}
			bHandled = FALSE;
			return FALSE;
		}

		LRESULT OnMouseMove (UINT uMsg, WPARAM wParam, 
			LPARAM lParam, BOOL &bHandled) 
		{
			if (!m_bMouseOver)
			{
				m_bMouseOver = true;
				TRACKMOUSEEVENT tme;
				tme .cbSize = sizeof (tme);
				tme .dwFlags = TME_LEAVE;
				tme .hwndTrack = m_hWnd;
				_TrackMouseEvent (&tme);
				InvalidateRect (NULL);
			}
			bHandled = FALSE;
			return FALSE;
		}

		LRESULT OnDrawItem(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL &bHandled)
		{
			LPDRAWITEMSTRUCT lpItem = (LPDRAWITEMSTRUCT) lParam;
			CDC dc (lpItem ->hDC);

			// Get data about the request
			UINT uState = lpItem ->itemState;
			CRect rcDraw = lpItem ->rcItem;

			if (m_hTheme != NULL && IsAppThemed())
			{
				// Draw the outer edge
				UINT uFrameState = 0;
				if ((uState & ODS_SELECTED) != 0)
					uFrameState |= PBS_PRESSED;
				if ((uState & ODS_DISABLED) != 0)
					uFrameState |= PBS_DISABLED;
				if ((uState & ODS_HOTLIGHT) != 0 || m_bMouseOver)
					uFrameState |= PBS_HOT;
				else if ((uState & ODS_DEFAULT) != 0)
					uFrameState |= PBS_DEFAULTED;
				DrawThemeBackground (dc, BP_PUSHBUTTON, 
					uFrameState, &rcDraw, NULL);
				GetThemeBackgroundContentRect (dc, BP_PUSHBUTTON, 
					uFrameState, &rcDraw, &rcDraw);
			}

			else
			{
				// Draw the outer edge

				UINT uFrameState = DFCS_BUTTONPUSH | DFCS_ADJUSTRECT;
				if ((uState & ODS_SELECTED) != 0 /*|| m_fPopupActive*/)
					uFrameState |= DFCS_PUSHED;
				if ((uState & ODS_DISABLED) != 0)
					uFrameState |= DFCS_INACTIVE;
				dc .DrawFrameControl (&rcDraw, DFC_BUTTON, uFrameState);

				// Adjust the position if we are selected (gives a 3d look)
				if ((uState & ODS_SELECTED) != 0 /*|| m_fPopupActive*/)
					rcDraw .OffsetRect (1, 1);
			}

			// Draw the arrow
			{
				CRect rcArrow;
				
				rcArrow.left   = rcDraw.left + (rcDraw.Width() / 2 - 2);
				rcArrow.top    = (rcDraw.bottom + rcDraw.top) / 2 - 2 / 2;
				rcArrow.right  = rcArrow.left + 4;
				rcArrow.bottom = (rcDraw .bottom + rcDraw .top) / 2 + 2 / 2;

				DrawArrow (dc, rcArrow, 0, 
					(uState & ODS_DISABLED) ? ::GetSysColor (COLOR_GRAYTEXT) : RGB (0,0,0));
			}

			if ((uState & ODS_FOCUS) != 0)
			{
				CRect rcFocus (rcDraw.left, rcDraw.top, 
					rcDraw.right - 1, rcDraw.bottom);
				dc .DrawFocusRect(&rcFocus);
			}

			return 1;
		}

		void DrawArrow (CDC &dc, const RECT &rect, 
			int iDirection, COLORREF clrArrow)
		{
			POINT ptsArrow[3];
			
			switch (iDirection)
			{
				case 0 : // Down
					{
						ptsArrow [0] .x = rect .left;
						ptsArrow [0] .y = rect .top;
						ptsArrow [1] .x = rect .right;
						ptsArrow [1] .y = rect .top;
						ptsArrow [2] .x = (rect .left + rect .right) / 2;
						ptsArrow [2] .y = rect .bottom;
						break;
					}
					
				case 1 : // Up
					{
						ptsArrow [0] .x = rect .left;
						ptsArrow [0] .y = rect .bottom;
						ptsArrow [1] .x = rect .right;
						ptsArrow [1] .y = rect .bottom;
						ptsArrow [2] .x = (rect .left + rect .right) / 2;
						ptsArrow [2] .y = rect .top;
						break;
					}
					
				case 2 : // Left
					{
						ptsArrow [0] .x = rect .right;
						ptsArrow [0] .y = rect .top;
						ptsArrow [1] .x = rect .right;
						ptsArrow [1] .y = rect .bottom;
						ptsArrow [2] .x = rect .left;
						ptsArrow [2] .y = (rect .top + rect .bottom) / 2;
						break;
					}
					
				case 3 : // Right
					{
						ptsArrow [0] .x = rect .left;
						ptsArrow [0] .y = rect .top;
						ptsArrow [1] .x = rect .left;
						ptsArrow [1] .y = rect .bottom;
						ptsArrow [2] .x = rect .right;
						ptsArrow [2] .y = (rect .top + rect .bottom) / 2;
						break;
					}
			}
			
			CBrush brArrow;
			brArrow .CreateSolidBrush (clrArrow);
			CPen penArrow;
			penArrow .CreatePen (PS_SOLID, 0, clrArrow);

			HBRUSH hbrOld = dc .SelectBrush (brArrow);
			HPEN hpenOld = dc .SelectPen (penArrow);

			dc .SetPolyFillMode (WINDING);
			dc .Polygon (ptsArrow, 3);

			dc .SelectBrush (hbrOld);
			dc .SelectPen (hpenOld);
			return;
		}
};


#endif //#ifndef pnutils_h__included