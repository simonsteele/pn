/**
 * @file pnutils.h
 * @brief Utility classes such as MRU Lists etc.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * Classes in this file:
 *		CContainedPropSheet	- Create a propsheet as a child window.
 *		CMRUList			- "Most Recently Used" list container.
 *		CMRUMenu			- "Most Recently Used" menu.
 *		CDropDownButton		- XP Themes friendly drop-down arrow button.
 *		CNumberCombo		- ComboBox derivative simplifying number display.
 */

#ifndef pnutils_h__included
#define pnutils_h__included

#include "ssmenus.h"

/**
 * @class CContainedPropSheet
 * @brief Create a propsheet as a child window.
 * 
 * This class is basically a version of the code in the Code Project article by
 * Jay Giganti called "Owner Drawn WTL Controls". I removed his application specific
 * code and created this generic class. All credit goes to Jay <JGiganti@hotmail.com>
 * link: http://www.codeproject.com/wtl/ownerdrawn.asp
 */
class CContainedPropSheet : public CPropertySheetImpl<CContainedPropSheet>
{
	BEGIN_MSG_MAP(CContainedPropSheet)
		NOTIFY_CODE_HANDLER(TCN_SELCHANGE, OnSelChange)
		CHAIN_MSG_MAP(CPropertySheetImpl<CContainedPropSheet>)
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	HWND Create(const HWND hWndParent, const int nStartPage, const CRect & rc)
	{
		ATLASSERT(m_hWnd == NULL);

		m_psh.dwFlags	    = PSH_NOAPPLYNOW | PSH_MODELESS | PSH_USECALLBACK;
		m_psh.hwndParent	= hWndParent;
		m_psh.phpage		= (HPROPSHEETPAGE*)m_arrPages.GetData();
		m_psh.nPages		= m_arrPages.GetSize();
		m_psh.pfnCallback	= CContainedPropSheet::PropSheetCallback;
			
		_Module.AddCreateWndData(&m_thunk.cd, this);

		HWND hWnd = (HWND)::PropertySheet(&m_psh);
		_CleanUpPages();

		if (m_hWnd)
		{		
			HWND hWndBtn = GetDlgItem(IDCANCEL);
			if (hWndBtn)
			{
				::ShowWindow(hWndBtn, SW_HIDE);
			}

			hWndBtn = GetDlgItem(IDOK);
			if (hWndBtn)
			{
				::ShowWindow(hWndBtn, SW_HIDE);
			}

			int nAdjX = GetSystemMetrics(SM_CXDLGFRAME) * 2;
			int nAdjY = GetSystemMetrics(SM_CYDLGFRAME) * 2;
					
			SetWindowPos(NULL, rc.left - nAdjX, rc.top - nAdjY, rc.Width(), rc.Height(), 
							SWP_NOZORDER|SWP_NOACTIVATE);	
		}

		SetActivePage(nStartPage);
		return hWnd;
	}

	LRESULT OnSelChange(WPARAM wParam, LPNMHDR pnmHdr, BOOL & bHandled)
	{
		return  DefWindowProc(WM_NOTIFY, wParam, (LPARAM)pnmHdr);
	}
	
	static int CALLBACK PropSheetCallback(HWND hWnd, UINT uMsg, LPARAM lParam)
	{
		int nRetVal = 0;

		if (uMsg == PSCB_INITIALIZED)
		{		
			ATLASSERT(hWnd != NULL);
			CContainedPropSheet* pT = (CContainedPropSheet*)_Module.ExtractCreateWndData();		
			pT->SubclassWindow(hWnd);	// subclass the sheet window		
			pT->_CleanUpPages();		// remove page handles array
		}
		else if (uMsg == PSCB_PRECREATE)
		{
			LPDLGTEMPLATE pTemplate = (LPDLGTEMPLATE)lParam;
			ATLASSERT(pTemplate);
			
			DWORD dwRemove	= WS_POPUP|WS_SYSMENU|WS_CAPTION|DS_MODALFRAME;
			DWORD dwAdd		= WS_CHILD|WS_VISIBLE|WS_TABSTOP|DS_CONTEXTHELP|DS_3DLOOK;//|DS_CONTROL;

			pTemplate->style			 = (pTemplate->style & ~dwRemove) | dwAdd;
			pTemplate->dwExtendedStyle	|= WS_EX_CONTROLPARENT;		
			nRetVal = 1;
		}
			
		return nRetVal;
	}
};

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

/**
 * @class CNumberComboBox
 * @author Simon Steele
 * @brief A simple class to store numbers in a combobox, with support for sorting.
 */
class CNumberCombo : public CComboBox
{
	public:
		void Add(int n)
		{
			_itot(n, buf, 10);
			InternalAdd(n, buf);
		}

		void AddSorted(int n)
		{
			_itot(n, buf, 10);
			InternalAdd(n, buf, GetInsertPos(n));
		}

		void Select(int n)
		{
			_itot(n, buf, 10);

			if(SelectString(0, buf) == CB_ERR)
				SetCurSel(InternalAdd(n, buf, GetInsertPos(n)));
		}

		int GetSelection()
		{
			int i = GetCurSel();
			return GetItemData(i);			
		}

	protected:
		TCHAR buf[10];
		inline int InternalAdd(int n, LPCTSTR s, int index = -1)
		{
			int i = InsertString(index, s);
			SetItemData(i, n);
			return i;
		}

		inline int GetInsertPos(int n)
		{
			int count = GetCount();
			int data;
			for(int i = 0; i < count; i++)
			{
				data = GetItemData(i);
				if(n < data)
					return i;
			}
			
			return -1;
		}
};

///@todo this should probably go in a pnstrings.h file or something similar...

#include <vector>
using std::vector;

#include <string>
typedef basic_string<TCHAR> tstring;

template <typename TStringType>
static void StringTokenise(const TStringType& str,
                      vector<TStringType>& tokens,
                      const TStringType& delimiters = _T(" "))
{
    // Skip delimiters at beginning.
    TStringType::size_type lastPos = str.find_first_not_of(delimiters, 0);
    // Find first "non-delimiter".
    TStringType::size_type pos     = str.find_first_of(delimiters, lastPos);

    while (TStringType::npos != pos || TStringType::npos != lastPos)
    {
        // Found a token, add it to the vector.
        tokens.push_back(str.substr(lastPos, pos - lastPos));
        // Skip delimiters.  Note the "not_of"
        lastPos = str.find_first_not_of(delimiters, pos);
        // Find next "non-delimiter"
        pos = str.find_first_of(delimiters, lastPos);
    }
}

template <class T>
class CustomFormatStringBuilder
{
	public:
		const tstring& Build(LPCTSTR str)
		{
			T* pT = static_cast<T*>(this);
			int len = _tcslen(formatstr);

			for(int i = 0; i < len; i++)
			{
				if(str[i] != _T('%'))
				{
					m_string += str[i];
				}
				else
				{
					TCHAR next = SafeGetNextChar(str, i, len);
					
					if(i == NULL)
					{
						m_string += str[i];
					}
					else if(next == _T('%'))
					{
						m_string += next;
						// Push past the next %
						i++;
					}
					else
					{
						pT->OnFormatChar(next);
						i++;
					}
				}
			}

			return m_string;
		}

		void OnFormatChar(TCHAR thechar){}
		
		//void OnFormatComplex(LPCTSTR complex){}

	protected:
		TCHAR SafeGetNextChar(LPCTSTR str, int i, int len)
		{
			PNASSERT(i < len);
			PNASSERT(i > 0);

			if(i = (len-1))
				return NULL;
            
			return str[i+1];
		}

		tstring	m_string;
};

#endif //#ifndef pnutils_h__included