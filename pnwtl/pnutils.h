/**
 * @file pnutils.h
 * @brief Utility classes such as MRU Lists etc.
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * Classes in this file:
 *		CWindowText			- Get the text from a window.
 *		CContainedPropSheet	- Create a propsheet as a child window.
 *		CMRUList			- "Most Recently Used" list container.
 *		CMRUMenu			- "Most Recently Used" menu.
  *		CXPButton			- XP Theme friendly standard button.
 *		CArrowButton		- XP Themes friendly drop-down arrow button.
 *		CNumberCombo		- ComboBox derivative simplifying number display.
 */

#ifndef pnutils_h__included
#define pnutils_h__included

#ifdef _MSC_VER
	#pragma once
#endif

#include "ssmenus.h"
#include <list>

class CWindowText
{
	public:
		CWindowText(HWND hWnd) : m_buffer(NULL)
		{
			PNASSERT(::IsWindow(hWnd));

			int len = ::GetWindowTextLength(hWnd);
			if (len > 0)
			{
				len++;
				m_buffer = new TCHAR[len];
				::GetWindowText(hWnd, m_buffer, len);
			}
		}

		~CWindowText()
		{
			if (m_buffer)
				delete [] m_buffer;
		}

		operator LPCTSTR ()
		{
			return m_buffer;
		}

		std::string GetA()
		{
			if (!m_buffer)
			{
				return std::string();
			}

			CT2CA conv(m_buffer);
			return std::string(conv);
		}

	private:
		TCHAR* m_buffer;
};

BOOL PNCenterWindow(HWND hWnd, HWND hWndCenter = NULL) throw();

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
public:
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
			int nAdjY = GetSystemMetrics(SM_CYDLGFRAME) * 1;

			SetWindowPos(NULL, rc.left - nAdjX, rc.top - nAdjY, rc.Width() + nAdjX, rc.Height() + nAdjY,
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

		int GetCount();

		void AddEntry(LPCTSTR data);
		bool MoveToTop(int index);
		bool RemoveEntry(int index);

		LPCTSTR GetEntry(int index);

		void Save(extensions::IOptions* options, LPCTSTR key);
		void Load(extensions::IOptions* options, LPCTSTR key);

	protected:		
		void Resize();

		CSimpleArray<_entry>	m_entries;
		int						m_iMaxSize;
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

		UINT base() const;
		UINT last() const;
		
		void MoveToTop(int index);
		void RemoveEntry(int index);

	protected:
		UINT		m_iBase;
		tstring		m_strEmpty;
		CSPopupMenu	m_Menu;
};

template <class T>
class CXPButton : public CWindowImpl <T>,  public CThemeImpl < CXPButton<T> >
{
	typedef CXPButton<T> thisClass;
	public:
		BEGIN_MSG_MAP(thisClass)
			CHAIN_MSG_MAP (CThemeImpl <CXPButton>)
			MESSAGE_HANDLER (OCM__BASE + WM_DRAWITEM, OnDrawItem)
			MESSAGE_HANDLER (WM_MOUSEMOVE, OnMouseMove)
			MESSAGE_HANDLER (WM_MOUSELEAVE, OnMouseLeave)
		END_MSG_MAP()

		CXPButton()
		{
			m_bMouseOver = false;
			m_hTheme = NULL;
		}

		HWND Create(HWND hWndParent, _U_RECT rect = NULL, LPCTSTR szWindowName = NULL,
			DWORD dwStyle = 0, DWORD dwExStyle = 0,
			_U_MENUorID MenuOrID = 0U, LPVOID lpCreateParam = NULL)
		{
			dwStyle |= BS_OWNERDRAW;
			HWND hWndResult = CWindowImpl<T>::Create(hWndParent, rect, szWindowName, dwStyle,
				dwExStyle, MenuOrID, lpCreateParam);
			
			if(hWndResult)
			{
				OpenThemeData (L"Button");
			}

			return hWndResult;
		}

		BOOL SubclassWindow (HWND hWnd)
		{
			CWindowImpl <T>::SubclassWindow (hWnd);
			ModifyStyle (0, BS_OWNERDRAW);
			OpenThemeData (L"Button");
			return TRUE;
		}

	protected:
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
			T* pT = static_cast<T*>(this);
			return pT->DrawItem(uMsg, wParam, lParam, bHandled);
		}

		LRESULT DrawItem(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL &bHandled)
		{
			LPDRAWITEMSTRUCT lpItem = (LPDRAWITEMSTRUCT) lParam;
			CDCHandle dc (lpItem ->hDC);

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

			// Draw the content
			{
				CRect rcContent;
				
				// This may be too arrow specific, it may be refined in the future...
				rcContent.left   = rcDraw.left + (rcDraw.Width() / 2 - 2);
				rcContent.top    = (rcDraw.bottom + rcDraw.top) / 2 - 2 / 2;
				rcContent.right  = rcContent.left + 4;
				rcContent.bottom = (rcDraw .bottom + rcDraw .top) / 2 + 2 / 2;

				T* pT = static_cast<T*>(this);
				pT->DrawContent (dc, rcContent, uState);
			}

			if ((uState & ODS_FOCUS) != 0)
			{
				CRect rcFocus (rcDraw.left, rcDraw.top, 
					rcDraw.right - 1, rcDraw.bottom);
				dc .DrawFocusRect(&rcFocus);
			}

			return 1;
		}
};

/**
 * @class CArrowButton
 * Most of the credit for this class goes to the collective authors of CColorButton
 * which can be found at http://www.codeproject.com/wtl/wtlcolorbutton.asp
 * Who'd have thought that to have a themed button with a drop-down arrow on it would
 * require such a ridiculous amount of code. Gotta love Windows programming.
 */
class CArrowButton : public CXPButton <CArrowButton>
{
	public:
		typedef enum {abdDown = 0, abdUp, abdLeft, abdRight} EABDirection;

		CArrowButton()
		{
			m_direction = abdDown;
		}

		CArrowButton(EABDirection direction)
		{
			SetDirection(direction);
		}

		void SetDirection(EABDirection direction)
		{
			m_direction = direction;
		}

		void DrawContent(CDCHandle &dc, const RECT &rect, UINT uState)
		{
			DrawArrow(dc, rect, m_direction, 
				(uState & ODS_DISABLED) ? ::GetSysColor (COLOR_GRAYTEXT) : RGB (0,0,0) );
		}

	protected:
		void DrawArrow (CDCHandle &dc, const RECT &rect, 
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

			penArrow.DeleteObject();
			brArrow.DeleteObject();
			return;
		}

	protected:
		EABDirection m_direction;
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

		int GetSelection(bool bTryWindowText = true)
		{
			if(bTryWindowText)
			{
				CWindowText wt(m_hWnd);
				if((LPCTSTR)wt && _tcslen((LPCTSTR)wt) > 0)
				{
					int size = _ttol((LPCTSTR)wt);
					if( size != 0 )
						return size;
				}
			}
			
			// fallback to the last proper selection.
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

#ifdef _DEBUG

/**
 * @brief operation timer class - allows simple timing within scope operators.
 *
 * note: Only available in debug builds.
 */
class OpTimer
{
	public:
		OpTimer()
		{
			dwTicksStart = GetTickCount();
		}

		~OpTimer()
		{
			DWORD diffTicks = GetTickCount() - dwTicksStart;
			TCHAR buffer[50];
			_sntprintf(buffer, 50, _T("OpTimer recorded %d Milliseconds.\n"), diffTicks);
			::OutputDebugString(buffer);
		}

	protected:
		DWORD dwTicksStart;
};

#endif

class FileInformation
{
public:
	FileInformation(LPCTSTR filePath)
	{
		set(filePath);
	}

	tstring FileDate;
	tstring FileTime;
	tstring FileAttr;

protected:

	void set(LPCTSTR filePath)
	{
		const int TEMP_LEN = 100;
		TCHAR temp[TEMP_LEN];

		HANDLE hf = ::CreateFile(filePath, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
		if (hf != INVALID_HANDLE_VALUE) {
			FILETIME ft;
			::GetFileTime(hf, NULL, NULL, &ft);
			::CloseHandle(hf);
			FILETIME lft;
			::FileTimeToLocalFileTime(&ft, &lft);
			SYSTEMTIME st;
			::FileTimeToSystemTime(&lft, &st);
			::GetTimeFormat(LOCALE_SYSTEM_DEFAULT,
							0, &st,
							NULL, temp, TEMP_LEN);
			FileTime = temp;

			::GetDateFormat(LOCALE_SYSTEM_DEFAULT,
							DATE_SHORTDATE, &st,
							NULL, temp, TEMP_LEN);
			
			FileDate = temp;

			DWORD attr = ::GetFileAttributes(filePath);
			tstring fa;
			if (attr & FILE_ATTRIBUTE_READONLY)
				fa += _T("R");
			if (attr & FILE_ATTRIBUTE_HIDDEN)
				fa += _T("H");
			if (attr & FILE_ATTRIBUTE_SYSTEM)
				fa += _T("S");

			FileAttr = fa.c_str();
		}
		else 
		{
			/* Reset values for no file */
			FileTime = _T("");
			FileDate = _T("");
			FileAttr = _T("");
		}


	}
};

class DateTimeInformation
{
public:
	DateTimeInformation()
	{
		set();
	}

	tstring CurrentDate;
	tstring CurrentTime;

protected:
	void set()
	{
		const int TEMP_LEN = 100;
		TCHAR temp[TEMP_LEN];

		::GetDateFormat(LOCALE_SYSTEM_DEFAULT,
						DATE_SHORTDATE, NULL,    	// Current date
						NULL, temp, TEMP_LEN);
		CurrentDate = temp;

		::GetTimeFormat(LOCALE_SYSTEM_DEFAULT,
						0, NULL,    	// Current time
						NULL, temp, TEMP_LEN);
		CurrentTime = temp;
	}
};

class UserInformation
{
public:
	UserInformation()
	{
		set();
	}

	tstring UserName;
	//tstring DisplayName;

protected:
	void set()
	{
		const int TEMP_LEN = 100;
		TCHAR temp[TEMP_LEN];

		//if( ::GetUserNameEx(NameDisplay, temp, TEMP_LEN) == 0 )
		//	DisplayName = temp;

		DWORD len = TEMP_LEN;
		if( ::GetUserName/*Ex*/(/*NameSamCompatible, */temp, &len) != 0 )
			UserName = temp;
	}
};

std::list<tstring> GetCommandLineArgs();

bool IsXPOrLater();

#endif //#ifndef pnutils_h__included