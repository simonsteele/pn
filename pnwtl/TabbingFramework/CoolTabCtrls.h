#ifndef __COOLTABCTRLS_H__
#define __COOLTABCTRLS_H__

#pragma once

/////////////////////////////////////////////////////////////////////////////
// CCoolTabCtrls - A set of Tab Controls with different appearances
//
// Written by Bjarke Viksoe (bjarke@viksoe.dk)
// Copyright (c) 2001 Bjarke Viksoe.
//
// Modifications by Daniel Bowen (dbowen@es.com)
// Copyright (c) 2002 Daniel Bowen
//
// Add the following macro to the parent's message map:
//   REFLECT_NOTIFICATIONS()
// (to handle NM_CUSTOMDRAW and possibly others)
//
// CFolderTabCtrl code based on a Paul DiLascia MSJ 1999 article.
//
// CDotNetTabCtrl and CDotNetButtonTabCtrl provided by Daniel Bowen.
//
// This code may be used in compiled form in any way you desire. This
// file may be redistributed by any means PROVIDING it is 
// not sold for profit without the authors written consent, and 
// providing that this notice and the authors name is included. 
//
// This file is provided "as is" with no expressed or implied warranty.
// The author accepts no liability if it causes any damage to you or your
// computer whatsoever. It's free, so don't hassle me about it.
//
// Beware of bugs.
//
// History (Date/Author/Description):
// ----------------------------------
// 
// 2002/04/24: Daniel Bowen (DDB)
// - CDotNetTabCtrl and CDotNetButtonTabCtrl -
//   Owen Gunter [OwenG@d-graphic.co.uk] pointed out that
//   the tabs were "shifting" when the selection changed
//   (in tab controls that aren't bolding the selected tab).
//   This was due to using "SelMargin" in some recent updates.
//   Now, SelMargin is not used, just "Margin" - and the
//   tabs shouldn't shift like they did.
//
// 2002/04/23: Daniel Bowen (DDB)
// - Tooltips. The way we implement tooltips is to have as many
//   "tools" as there are tabs.  The relationship of
//    tool ID => tab index is:
//    tool ID = tab index + 1     (to avoid 0 as an ID)
//   Adding and removing tabs adds and removes the "last" tooltip tool.
//   In UpdateLayout, we set the RECTs for the tooltip tools,
//   and we have it ask us for the text all the time,
//   and give back the text for the corresponding tab.
// - New "CCoolTabItem" class that is used instead of TCITEM.
// - CCustomTabCtrl now has another template parameter - 
//   the items "array" type.  Its default is "CCoolTabItem".
//   If you want, you could use your own class that inherits
//   from CCoolTabItem when creating a new type of tab control.
// - The "sizes" parallel array is now obsoleted, because
//   the RECT for the tab is kept in CCoolTabItem.
// - Several of the public "Interfaces" have been modified. 
//   This was done in conjunction with what would make more
//   sense for CCoolTabItem. Here an attempt at the list:
//    * GetItem - Now returns a pointer to the corresponding tab item
//    * SetItem - I've obsoleted it for now. It can come back
//       if someone really wants it.  Now, instead of calling SetItem,
//       call GetItem to get the pointer to the TItem, update
//       what you want through the methods on TItem (i.e., CCoolTabItem),
//       then call UpdateLayout and Invalidate if appropriate.
//    * FindItem - Since TItem (CCoolTabItem) doesn't have a "mask"
//       guy anymore, FindItem has a new parameter to specify
//       which fields to care about in the search.  These flags
//       are an enumeration in CCoolTabItem.
//    * InsertItem - You can build a TItem instance and pass
//       a pointer to that in, or you can use the new version
//       of InsertItem where you can just pass function arguments.
// - New "CreateNewItem" and "DeleteItem" that manage the memory
//   allocation/deallocation of the TItem;  These are overrideable.
// - Call SetCurSel and SetFocus so that they are overrideable.
// - Some variable renaming.(idx => nIndex, cnt => nCount, etc.)
// - Some format changes in CCustomTabCtrl (tabs, brace placement, etc.)
//
// 2002/04/22: Daniel Bowen (DDB)
// - Changed "struct TC_SIZES" from using int's to using char's.
//   This makes an instance take up 4 bytes instead of 16 bytes.
// - Move the Image List and "bold selected tab" stuff to the
//   base "CCustomTabCtrl" class.  In a future revision, I'll
//   probably update the base class to honor these, but for
//   now, its up to the deriving class whether or not to use
//   these things.  Having them there makes it easier to use
//   any CCustomTabCtrl derived class where there's a template
//   parameter for the tab class, and where it might want to
//   use the image list or bold the selected tab.
// - When the tabs are not a subclassed static control,
//   the "GetFont" and "SetFont" calls were going in the bit
//   bucket.  CCustomTabCtrl now has a member variable
//   "CFont m_font", and all the derived classes (with one
//   exception) now use the base class version instead of their own.
//   If a derived class has a bold font, they still declare
//   that themselves.  Because the base class "m_font" is
//   managing the lifetime of the font (i.e., it calls DeleteObject
//   in the destructor), the handler for WM_SETFONT makes
//   a copy of the font instead of "pointing" to it.
//   Typically, no one should be sending us WM_SETFONT.
//   CFlatTabCtrl is the one deriving class that has its
//   own version of m_font, because it wants to do a
//   SetFont(m_fontBold) - which with the new way of things
//   would cause CCustomTabCtrl::m_font and CFlatTabCtrl::m_fontBold
//   to be the same.
// - More updates for CDotNetTabCtrl and CDotNetButtonTabCtrl
//   to accomodate the current DPI and system metrics.
//   The updates were focused on getting the padding and margin
//   for the active and inactive tabs to be a better match
//   for what VS.Net does under various DPI and font settings.
//   It turned out to be quite difficult to get a pixel-for-pixel
//   match to VS.Net under various font sizes and DPI settings.
//   What's there now is not a perfect match, but its pretty good.
// - Change the logic for CDotNetTabCtrl and CDotNetButtonTabCtrl
//   when they don't get all the real estate their tabs want.
//   When the desired widths of the tabs would be too big for
//   the client area:
//    * First, the inactive tabs are proportionally reduced,
//      and the selected tab is given its desired width
//    * Once the smallest inactive tab would be too small,
//      the inactive tabs are no longer made proportional to
//      their desired size, but are all made the same size.
//      The selected tab is still given its desired width.
//    * Once there's not enough room to show the selected tab
//      at its desired width and all the inactive tabs at
//      the same minimum width, all of the tabs are made
//      the same width (both the selected and inactive tabs)
//   NOTE: This is not the approach VS.Net takes.  VS.Net
//    first gives away the real estate for the widest tabs
//    until all the tabs are the same width.  If you'd rather
//    have that approach, create a new class that inherits
//    from CDotNetTabCtrl/CDotNetButtonTabCtrl and override
//    UpdateLayout with your implementation.
//   
// 2002/04/17: Daniel Bowen (DDB)
// - Owen Gunter [OwenG@d-graphic.co.uk] pointed out that the
//   tab background and font height of CDotNetTabCtrl didn't match
//   what he saw in the tabs under his version of Visual Studio.
//   When I originally chose those, I was running Visual Studio
//   RC1 under Windows XP with the Windows XP theme and the
//   silver color scheme and large fonts.  It wasn't clear to me
//   at the time if the tab background color was dependant on a
//   system color (it didn't seem to be) or if the font was dependant
//   on a system metric (I had changed the caption font and saw
//   it didn't change).
//
//   I've now revisited the issue, and there's now a much better
//   approximation of what Visual Studio.Net is doing.
//   * The tab font is the "ICON" font, which can be retrieved
//     by calling SystemParametersInfo with SPI_GETICONMETRICS.
//     CDotNetTabCtrl and CDotNetButtonTabCtrl now use this
//     font instead of a hardcoded 9 point Tahoma.
//   * The tab background is a "lightened" version of COLOR_BTNFACE.
//     The formula used in CDotNetTabCtrl isn't exactly the same
//     formula that VS.Net uses, but from my experiments, its
//     never more than 1 off for red, green, or blue
//     (with each of these being 0-255)
//   * The "inactive tab" text color is a "darkened" version of
//     COLOR_GRAYTEXT.  Again, the formula is an approximation
//     to what VS.Net is doing, but its seems to be very close.
//   * CDotNetTabCtrl was doing some outlines with a black ben
//     and a white pen.  After some experiments, I found that
//     VS.Net was actually using COLOR_BTNTEXT and COLOR_BTNHIGHLIGHT.
//     I've updated CDotNetTabCtrl to do the same.
//
// 2002/01/30: Daniel Bowen (DDB)
// - Rename Bjarke's original CDotNetTabCtrl to CFlatTabCtrl
// - Added my version of CDotNetTabCtrl based on Pascal's
//   CDotNetTabCtrl2:
//    * Added support for divider lines between tabs
//    * Closer match to look of tabs used in VS.Net
//    * Support for bolding text of selected tab
// - Added CDotNetButtonTabCtrl (to look like VS.Net
//    view of HTML with the Design/HTML buttons)
// - Utilize COffscreenDrawRect for flicker-free drawing
//   (requires updated atlgdix.h containing COffscreenDrawRect)
// - Handle WM_RBUTTONDOWN to allow switching tabs,
//   and fire NM_RCLICK notification
// - When changing the selection, redraw entire client area
//   to avoid visual artifacts in case an implementation
//   doesn't keep each tab the same size always
//   (such as if the selected tab is bolded and slightly bigger,
//   or if there are divider lines between tabs).
// - Added method "FindItem" that let's you search for an item
//   with some or all the members that have the requested values.
//   It is meant to work similar to CListViewCtrl::FindItem and
//   LVM_FINDITEM (since there are no similar messages for tab controls)
// - Updated "DeleteItem" to better handle the case when the
//   deleted item is before the selected item.  It now keeps the
//   same item selected that was selected before, and adjusts
//   m_iCurSel as appropriate (this was needed for doing TabbedMDI.h)
// - Updated "GetItem" so that if bits in the mask are requested
//   that aren't acutally in the stored item, those bits are cleared,
//   so that the caller of "GetItem" can determine which bits
//   it got, compared to which it asked for.
//   This update also allows you to let some tabs have images and
//   others not have images, and have it work properly.
// 
// 11/06/2001: Pascal Binggeli (PBI)
// - Added support for ImageList
// - Added support for not subclassing from CStatic.
// - Added CDotNetTabCtrl2 class.
//             

#ifndef __cplusplus
  #error ATL requires C++ compilation (use a .cpp suffix)
#endif

#ifndef __ATLAPP_H__
  #error CoolTabCtrls.h requires atlapp.h to be included first
#endif

#ifndef __ATLCTRLS_H__
  #error CoolTabCtrls.h requires atlctrls.h to be included first
#endif

#ifndef __ATLMISC_H__
  #error CoolTabCtrls.h requires atlmisc.h to be included first (before atlwin.h)
#endif

#if (_WIN32_IE < 0x0400)
  #error CoolTabCtrls.h requires _WIN32_IE >= 0x0400
#endif


#define TCN_INITIALIZE TCN_FIRST-10
#define TCN_INSERTITEM TCN_FIRST-11
#define TCN_DELETEITEM TCN_FIRST-12

class CCoolTabItem
{
// Member variables
protected:
	CRect m_rcItem;
	CString m_sText;
	int m_nImage;
	HWND m_hWndTabView;
	CString m_sToolTip;

public:
	typedef enum FieldFlags
	{
		eCoolTabItem_None = 0x0000,
		eCoolTabItem_Rect = 0x0001,
		eCoolTabItem_Text = 0x0002,
		eCoolTabItem_Image = 0x0004,
		eCoolTabItem_TabView = 0x0008,
		eCoolTabItem_ToolTip = 0x0010,
		//eCoolTabItem_Reserved1 = 0x0020,
		//eCoolTabItem_Reserved2 = 0x0040,
		eCoolTabItem_All = 0x00FF,
	};

// Constructors/Destructors
public:
	CCoolTabItem() :
		m_rcItem(0,0,0,0),
		m_nImage(-1),
		m_hWndTabView(NULL)
	{
	}

	CCoolTabItem(const CCoolTabItem& rhs)
	{
		*this = rhs;
	}

	virtual ~CCoolTabItem()
	{
	}

	const CCoolTabItem& operator=(const CCoolTabItem& rhs)
	{
		if(&rhs != this)
		{
			m_rcItem		= rhs.m_rcItem;
			m_sText			= rhs.m_sText;
			m_nImage		= rhs.m_nImage;
			m_hWndTabView	= rhs.m_hWndTabView;
			m_sToolTip		= rhs.m_sToolTip;
		}
		return *this;
	}

// Accessors
public:

	CRect GetRect() const
	{
		return m_rcItem;
	}
	LPCRECT GetRectRef() const
	{
		return &m_rcItem;
	}
	bool SetRect(CRect rcItem)
	{
		m_rcItem = rcItem;
		return true;
	}

	CString GetText() const
	{
		return m_sText;
	}
	LPCTSTR GetTextRef() const
	{
		return (LPCTSTR)m_sText;
	}
	bool SetText(LPCTSTR sNewText)
	{
		m_sText = sNewText;
		return true;
	}

	int GetImageIndex() const
	{
		return m_nImage;
	}
	bool SetImageIndex(int nImage = -1)
	{
		m_nImage = nImage;
		return true;
	}

	HWND GetTabView() const
	{
		return m_hWndTabView;
	}
	bool SetTabView(HWND hWnd = NULL)
	{
		m_hWndTabView = hWnd;
		return true;
	}

	CString GetToolTip() const
	{
		return m_sToolTip;
	}
	LPCTSTR GetToolTipRef() const
	{
		return (LPCTSTR)m_sToolTip;
	}
	bool SetToolTip(LPCTSTR sNewText)
	{
		m_sToolTip = sNewText;
		return true;
	}
	

// Methods:
public:
	bool UsingText() const
	{
		return (m_sText.GetLength() > 0);
	}
	bool UsingImage() const
	{
		return (m_nImage >= 0);
	}
	bool UsingTabView() const
	{
		return (m_hWndTabView != NULL);
	}
	bool UsingToolTip() const
	{
		return (m_sToolTip.GetLength() > 0);
	}

	BOOL InflateRect(int dx, int dy)
	{
		return ::InflateRect(&m_rcItem, dx, dy);
	}

// Overrideables
public:
	bool MatchItem(CCoolTabItem* pItem, FieldFlags eFlags) const
	{
		bool bMatch = true;
		if(bMatch && (eFlags & eCoolTabItem_Rect) == eCoolTabItem_Rect)
		{
			bMatch = (TRUE == ::EqualRect(&m_rcItem, &pItem->m_rcItem));
		}
		if(bMatch && (eFlags & eCoolTabItem_Text) == eCoolTabItem_Text)
		{
			bMatch = (m_sText == pItem->m_sText);
		}
		if(bMatch && (eFlags & eCoolTabItem_Image) == eCoolTabItem_Image)
		{
			bMatch = (m_nImage == pItem->m_nImage);
		}
		if(bMatch && (eFlags & eCoolTabItem_TabView) == eCoolTabItem_TabView)
		{
			bMatch = (m_hWndTabView == pItem->m_hWndTabView);
		}
		if(bMatch && (eFlags & eCoolTabItem_ToolTip) == eCoolTabItem_ToolTip)
		{
			bMatch = (m_sToolTip == pItem->m_sToolTip);
		}

		if(bMatch)
		{
			*pItem = *this;
		}

		return bMatch;
	}
};

struct TC_SIZES
{
	signed char iPadding;
	signed char iMargin;
	signed char iSelMargin;
	signed char iIndent;
};

template <class T, class TItem = CCoolTabItem, class TBase = CWindow, class TWinTraits = CControlWinTraits>
class ATL_NO_VTABLE CCustomTabCtrl : 
	public CWindowImpl< T, TBase, TWinTraits >,
	public COffscreenDrawRect< T >
{
// Member variables
protected:
	int m_iCurSel;
	TC_SIZES m_settings;
	CSimpleArray< TItem* > m_Items;
	UINT m_idDlgCtrl;
	CFont m_font;
	CFont m_fontBold;
	bool m_bBoldSelectedTab;
	CImageList m_imageList;
	CToolTipCtrl m_tooltip;

// Constructors
public:
	CCustomTabCtrl() :
		m_iCurSel(-1),
		m_idDlgCtrl(0),
		m_bBoldSelectedTab(false)
	{
		::ZeroMemory(&m_settings, sizeof(TC_SIZES));
	}

// Message Handling
public:
	BEGIN_MSG_MAP(CCustomTabCtrl)
		CHAIN_MSG_MAP(COffscreenDrawRect< T >)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy)
		MESSAGE_RANGE_HANDLER(WM_MOUSEFIRST, WM_MOUSELAST, OnMouseMessage)
		MESSAGE_HANDLER(WM_LBUTTONDOWN, OnLButtonClick)
		MESSAGE_HANDLER(WM_RBUTTONDOWN, OnRButtonClick)
		MESSAGE_HANDLER(WM_SETTINGCHANGE, OnSettingChange)
		MESSAGE_HANDLER(WM_GETDLGCODE, OnGetDlgCode)
		MESSAGE_HANDLER(WM_KEYDOWN, OnKeyDown)
		MESSAGE_HANDLER(WM_GETFONT, OnGetFont)
		MESSAGE_HANDLER(WM_SETFONT, OnSetFont)
		NOTIFY_CODE_HANDLER(TTN_GETDISPINFO, OnGetToolTipInfo)
	END_MSG_MAP()

	// Operations

	BOOL SubclassWindow(HWND hWnd)
	{
		ATLASSERT(m_hWnd == NULL);
		ATLASSERT(::IsWindow(hWnd));
		BOOL bRet = CWindowImpl< T, TBase, TWinTraits >::SubclassWindow(hWnd);
		if( bRet )
		{
			_Init();
		}
		return bRet;
	}

	CImageList SetImageList(HIMAGELIST hImageList)
	{
		CImageList& oldImageList = m_imageList;
		m_imageList = hImageList;
		return oldImageList;
	}
	CImageList& GetImageList() const
	{
		return m_imageList;
	}

	void SetBoldSelectedTab(bool bBoldSelectedTab = true)
	{
		m_bBoldSelectedTab = bBoldSelectedTab;
	}
	bool GetBoldSelectedTab() const
	{
		return m_bBoldSelectedTab;
	}

   // Implementation

	void _Init()
	{
		ATLASSERT(::IsWindow(m_hWnd));

		// PBI: Modified to allow class not derived from CStatic.
		// Check class
		TCHAR szBuffer[20];
		if( ::GetClassName(m_hWnd, szBuffer, sizeof(szBuffer)/sizeof(TCHAR)) )
		{
			if (::lstrcmpi(szBuffer, CStatic::GetWndClassName())==0)
			{
				ATLASSERT(GetStyle() & WS_CHILD);
				ModifyStyle(0, SS_NOTIFY); // We need this for mouse-clicks
			}
		}

		m_idDlgCtrl = GetDlgCtrlID();

		T* pT = static_cast<T*>(this);
		BOOL bDummy;
		pT->OnSettingChange(0,0,0,bDummy);

		// This is a little WTL subclass helper notification
		NMHDR nmh = { m_hWnd, m_idDlgCtrl, TCN_INITIALIZE };
		::SendMessage(GetParent(), WM_NOTIFY, nmh.idFrom, (LPARAM)&nmh);

		// Be sure InitCommonControlsEx is called before this, with one of
		//  the flags that includes the tooltip control

		m_tooltip.Create(m_hWnd, NULL, NULL, WS_POPUP | TTS_NOPREFIX | TTS_ALWAYSTIP /* | TTS_BALLOON */, WS_EX_TOOLWINDOW);
		if(m_tooltip.IsWindow())
		{
			m_tooltip.SetWindowPos(HWND_TOPMOST, 0, 0, 0, 0,
				 SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE);

			m_tooltip.Activate(TRUE);
			m_tooltip.SetDelayTime(TTDT_INITIAL, ::GetDoubleClickTime());
			m_tooltip.SetDelayTime(TTDT_AUTOPOP, ::GetDoubleClickTime() * 20);
			m_tooltip.SetDelayTime(TTDT_RESHOW, ::GetDoubleClickTime() / 5);
		}
	}

	// Message Handlers

	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		_Init();
		return 0;
	}

	LRESULT OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		DeleteAllItems();
		return 0;
	}

	LRESULT OnSettingChange(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		// Empty - but might get overloaded by derived classes.
		Invalidate();
		return 0;
	}

	LRESULT OnGetDlgCode(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
	{
		return DefWindowProc(uMsg, wParam, lParam) | DLGC_WANTARROWS;
	}

	LRESULT OnMouseMessage(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		bHandled = FALSE;
		if(m_tooltip.IsWindow())
		{
			MSG msg = { m_hWnd, uMsg, wParam, lParam };
			m_tooltip.RelayEvent(&msg);
		}
		return 1;
	}

	LRESULT OnLButtonClick(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
	{
		NMHDR nmh = { m_hWnd, m_idDlgCtrl, NM_CLICK };
		::SendMessage(GetParent(), WM_NOTIFY, nmh.idFrom, (LPARAM)&nmh);

		// Search for a tab
		T* pT = static_cast<T*>(this);
		TCHITTESTINFO tchti = { 0 };
		tchti.pt = CPoint(lParam);
		int nIndex = pT->HitTest(&tchti);
		if( nIndex!=-1 )
		{
			pT->SetFocus();
			pT->SetCurSel(nIndex);
		}
		return 0;
	}

	LRESULT OnRButtonClick(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
	{
		NMHDR nmh = { m_hWnd, m_idDlgCtrl, NM_RCLICK };
		::SendMessage(GetParent(), WM_NOTIFY, nmh.idFrom, (LPARAM)&nmh);

		// Search for a tab
		T* pT = static_cast<T*>(this);
		TCHITTESTINFO tchti = { 0 };
		tchti.pt = CPoint(lParam);
		int nIndex = pT->HitTest(&tchti);
		if( nIndex!=-1 )
		{
			pT->SetFocus();
			pT->SetCurSel(nIndex);
		}
		return 0;
	}

	LRESULT OnKeyDown(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
	{
		T* pT = static_cast<T*>(this);
		switch( wParam )
		{
		case VK_LEFT:
			if( m_iCurSel>0 )
			{
				pT->SetCurSel(m_iCurSel-1);
			}
			return 0;
		case VK_RIGHT:
			if( m_iCurSel<m_Items.GetSize()-1 )
			{
				pT->SetCurSel(m_iCurSel+1);
			}
			return 0;
		}
		bHandled = FALSE;
		return 0;
	}

	LRESULT OnGetFont(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		// DDB: 2002/04/22
		// The code was doing GetFont and SetFont, but wasn't actually
		//  properly dealing with implementing it if the window
		//  was not a subclassed static control.
		return (LRESULT)(HFONT)m_font;
	}

	LRESULT OnSetFont(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
	{
		// DDB: 2002/04/22
		// The code was doing GetFont and SetFont, but wasn't actually
		//  properly dealing with implementing it if the window
		//  was not a subclassed static control.
		//
		// Also, we're managing the lifetime of our font
		//  (i.e., we're going to DeleteObject it in our destructor),
		//  so if someone calls SetFont, keep a copy of the
		//  font instead of just "pointing" to it
		LOGFONT lfCopy = {0};
		::GetObject((HFONT)wParam, sizeof(LOGFONT), &lfCopy);

		if(!m_font.IsNull()) m_font.DeleteObject();

		m_font.CreateFontIndirect(&lfCopy);

		if(LOWORD(lParam))
		{
			Invalidate();
		}

		return 0;
	}

	LRESULT OnGetToolTipInfo(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
	{
		LPNMTTDISPINFO pToolTipInfo = (LPNMTTDISPINFO)pnmh;
		if(pToolTipInfo)
		{
			// The way we implement tooltips is to have as many
			// "tools" as there are tabs.  The relationship of
			// tool ID => tab index is:
			// tool ID = tab index + 1	(to avoid 0 as an ID)
			//
			// We supply the RECT elsewhere and the text here
			UINT id = pToolTipInfo->hdr.idFrom;
			if(id > 0 && id <= (UINT)m_Items.GetSize())
			{
				TItem* pItem = m_Items[id-1];
				if(pItem)
				{
					if(pItem->UsingToolTip())
					{
						pToolTipInfo->lpszText = const_cast<LPTSTR>(pItem->GetToolTipRef());
					}
					else if(pItem->UsingText())
					{
						pToolTipInfo->lpszText = const_cast<LPTSTR>(pItem->GetTextRef());
					}
				}
			}
		}

		return 0;
	}

	// Overridables

	TItem* CreateNewItem(void* pInitData = NULL)
	{
#if defined (_CPPUNWIND) & (defined(_ATL_EXCEPTIONS) | defined(_AFX))
		TItem* pNewItem = NULL;
		try { pNewItem = new TItem; }
		catch (...) { ATLTRACE(_T("!! Exception thrown in CCustomTabCtrl::CreateNewItem\r\n")); }
#else
		TItem* pNewItem = new TItem;
#endif
		return pNewItem;
	}

	void DeleteItem(TItem* pOldItem)
	{
#if defined (_CPPUNWIND) & (defined(_ATL_EXCEPTIONS) | defined(_AFX))
		try { delete pOldItem; }
		catch (...) { ATLTRACE(_T("!! Exception thrown in CCustomTabCtrl::DeleteItem\r\n")); }
#else
		delete pOldItem;
#endif
	}

	void UpdateLayout()
	{
		CClientDC dc(m_hWnd);
		HFONT hOldFont = (HFONT)::SelectObject(dc, GetFont());    

		RECT rcClient;
		GetClientRect(&rcClient);

		// Reposition buttons
		int nCount = m_Items.GetSize();
		int xpos = m_settings.iIndent;
		for( int i=0; i<nCount; ++i )
		{
			TItem* pItem = m_Items[i];
			RECT rc;
			rc.left = rc.right = xpos;
			if( pItem->UsingText() )
			{
				RECT rcText = { 0 };
				CString sText = pItem->GetText();
				dc.DrawText(sText, sText.GetLength(), &rcText, DT_SINGLELINE|DT_CALCRECT);
				rc.right += (rcText.right-rcText.left) + (m_settings.iPadding*2);
			}
			rc.top = 0;
			rc.bottom = rcClient.bottom-rcClient.top;
			pItem->SetRect(rc);
			xpos += (rc.right-rc.left) + m_settings.iMargin;
		}
		if( m_iCurSel!=-1 )
		{
			m_Items[m_iCurSel]->InflateRect(m_settings.iSelMargin, 0);
		}

		::SelectObject(dc, hOldFont);

		// The way we implement tooltips is to have as many
		// "tools" as there are tabs.  The relationship of
		// tool ID => tab index is:
		// tool ID = tab index + 1	(to avoid 0 as an ID)
		//
		// We supply the RECT here and text elsewhere.
		if(m_tooltip.IsWindow())
		{
			for(i=0; i<nCount; ++i )
			{
				TItem* pItem = m_Items[i];
				m_tooltip.SetToolRect(m_hWnd, i+1, pItem->GetRectRef());
			}
		}
	}

	void DoPaint(CDCHandle hdc, RECT &rcClip)
	{
		// NOTE: The handling of NM_CUSTOMDRAW is probably not entirely correct
		//       in the code below. But at least it makes a brave attempt to
		//       implement all the states described in MSDN.

		// Save current DC selections
		int nSave = ::SaveDC(hdc);
		ATLASSERT(nSave!=0);

		// Make sure we don't paint outside client area (possible with paint dc)
		RECT rcClient;
		GetClientRect(&rcClient);
		::IntersectClipRect(hdc, rcClient.left, rcClient.top, rcClient.right, rcClient.bottom);

		// Prepare DC
		::SelectObject(hdc, GetFont());

		T* pT = static_cast<T*>(this);
		LRESULT lResStage;
		NMCUSTOMDRAW nmc = { 0 };
		nmc.hdr.hwndFrom = m_hWnd;
		nmc.hdr.idFrom = m_idDlgCtrl;
		nmc.hdr.code = NM_CUSTOMDRAW;
		nmc.hdc = hdc;
		nmc.uItemState = 0;

		nmc.dwDrawStage = CDDS_PREPAINT;
		lResStage = ::SendMessage(GetParent(), WM_NOTIFY, nmc.hdr.idFrom, (LPARAM)&nmc);
		if( lResStage==CDRF_NOTIFYITEMDRAW || lResStage==CDRF_DODEFAULT )
		{
			int nCount = m_Items.GetSize();
			// Draw the list items, except the selected one. It is drawn last
			// so it can cover the tabs below it.
			RECT rcIntersect;
			for( int i=0; i<nCount; ++i )
			{
				TItem* pItem = m_Items[i];
				CRect rcItem = pItem->GetRect();
				if( i!=m_iCurSel )
				{
					if( ::IntersectRect(&rcIntersect, &rcItem, &rcClip) )
					{
						nmc.dwItemSpec = i;
						nmc.uItemState = 0;
						nmc.rc = rcItem;
						pT->ProcessItem(lResStage, nmc);
					}
				}
			}
			if( m_iCurSel!=-1 )
			{
				TItem* pItem = m_Items[m_iCurSel];
				CRect rcItem = pItem->GetRect();
				if( ::IntersectRect(&rcIntersect, &rcItem, &rcClip) )
				{
					nmc.dwItemSpec = m_iCurSel;
					nmc.uItemState = CDIS_SELECTED;
					nmc.rc = rcItem;
					pT->ProcessItem(lResStage, nmc);
					nmc.uItemState = 0;
				}
			}
			nmc.dwItemSpec = 0;
		}

		if( lResStage==CDRF_NOTIFYPOSTPAINT )
		{
			nmc.dwDrawStage = CDDS_POSTPAINT;
			::SendMessage(GetParent(), WM_NOTIFY, nmc.hdr.idFrom, (LPARAM)&nmc);
		}

		::RestoreDC(hdc, nSave);
	}

	void ProcessItem(LRESULT lResStage, NMCUSTOMDRAW &nmc)
	{
		LRESULT lResItem = CDRF_DODEFAULT;
		if( lResStage==CDRF_NOTIFYITEMDRAW )
		{
			nmc.dwDrawStage = CDDS_ITEMPREPAINT;
			lResItem = ::SendMessage(GetParent(), WM_NOTIFY, nmc.hdr.idFrom, (LPARAM)&nmc);
		}
		if( lResItem!=CDRF_SKIPDEFAULT )
		{
			// Do default item-drawing
			T* pT = static_cast<T*>(this);
			pT->DoItemPaint(nmc);
		}
		if( lResStage==CDRF_NOTIFYITEMDRAW && lResItem==CDRF_NOTIFYPOSTPAINT )
		{
			nmc.dwDrawStage = CDDS_ITEMPOSTPAINT;
			::SendMessage(GetParent(), WM_NOTIFY, nmc.hdr.idFrom, (LPARAM)&nmc);
		}
	}

	void DoItemPaint(NMCUSTOMDRAW &/*nmc*/)
	{
	}

	// Operations

#define CHECK_ITEM(nItem) \
	ATLASSERT(nItem>=0 && nItem<m_Items.GetSize()); \
	if( nItem<0 || nItem>=m_Items.GetSize() ) return 0

	int InsertItem(int nItem, LPCTSTR sText = NULL, int nImage = -1, HWND hWndTabView = NULL, LPCTSTR sToolTip = NULL)
	{
		T* pT = static_cast<T*>(this);
		TItem* pItem = pT->CreateNewItem();
		if(pItem)
		{
			pItem->SetText(sText);
			pItem->SetImageIndex(nImage);
			pItem->SetTabView(hWndTabView);
			pItem->SetToolTip(sToolTip);

			return InsertItem(nItem, pItem);
		}
		return -1;
	}

	int InsertItem(int nItem, TItem* pItem)
	{
		T* pT = static_cast<T*>(this);

		ATLASSERT(::IsWindow(m_hWnd));
		ATLASSERT(pItem);
		ATLASSERT(nItem<0 || nItem>=m_Items.GetSize()); // We only support appending right now
		if(!::IsWindow(m_hWnd) || pItem == NULL)
		{
			return -1;
		}

		if( nItem < 0 || nItem>m_Items.GetSize() )
		{
			nItem = m_Items.GetSize();
		}

		RECT rc = { 0 };
		m_Items.Add(pItem);
		int nIndex = m_Items.GetSize()-1;
		// Send notification
		NMHDR nmh = { m_hWnd, m_idDlgCtrl, TCN_INSERTITEM };
		::SendMessage(GetParent(), WM_NOTIFY, nmh.idFrom, (LPARAM)&nmh);
		// Select if first tab
		if( m_Items.GetSize()==1 )
		{
			pT->SetCurSel(0);
		}

		// The way we implement tooltips is to have as many
		// "tools" as there are tabs.  The relationship of
		// tool ID => tab index is:
		// tool ID = tab index + 1	(to avoid 0 as an ID)
		//
		// We supply the RECT and text elsewhere.
		if(m_tooltip.IsWindow())
		{
			m_tooltip.AddTool(m_hWnd, LPSTR_TEXTCALLBACK, &rcDefault, m_Items.GetSize());
		}

		pT->UpdateLayout();
		Invalidate();
		return nIndex;
	}

	BOOL DeleteItem(int nItem)
	{
		T* pT = static_cast<T*>(this);
		ATLASSERT(::IsWindow(m_hWnd));
		if( nItem<0 || nItem>=m_Items.GetSize() )
		{
			return FALSE;
		}

		// If currently selected, select something else
		if( nItem < m_iCurSel )
		{
			// The item being removed is before the current selection.
			// We still want the same item to be selected, but
			// the index needs to be adjusted to account for the missing item
			m_iCurSel--;
		}
		else if( nItem == m_iCurSel )
		{
			pT->SetCurSel( (m_Items.GetSize() > 1) ? 0 : -1 );
		}

		// Remove from structures

		// The way we implement tooltips is to have as many
		// "tools" as there are tabs.  The relationship of
		// tool ID => tab index is:
		// tool ID = tab index + 1	(to avoid 0 as an ID)
		//
		// We supply the RECT and text elsewhere.
		if(m_tooltip.IsWindow())
		{
			m_tooltip.DelTool(m_hWnd, m_Items.GetSize());
		}

		TItem* pItem = m_Items[nItem];
		m_Items.RemoveAt(nItem);

		pT->DeleteItem(pItem);
		// Send notification
		NMHDR nmh = { m_hWnd, m_idDlgCtrl, TCN_DELETEITEM };
		::SendMessage(GetParent(), WM_NOTIFY, nmh.idFrom, (LPARAM)&nmh);

		// Repaint
		pT->UpdateLayout();
		Invalidate();
		return TRUE;
	}

	BOOL DeleteAllItems()
	{
		ATLASSERT(::IsWindow(m_hWnd));
		while( GetItemCount()>0 ) DeleteItem(0); // Slooow!!!
		return TRUE;
	}

	// NOTE: Now, instead of calling SetItem, call
	//   GetItem to get the pointer to the TItem, update
	//   what you want, then call UpdateLayout and Invalidate
	//   if appropriate.
	/*
	BOOL SetItem(int nItem, TItem* pItem)
	{
		ATLASSERT(::IsWindow(m_hWnd));
		ATLASSERT(!::IsBadReadPtr(pItem,sizeof(TItem)));
		CHECK_ITEM(nItem);

		// Copy caller's data to the internal structure
		TItem* pItemT = m_Items[nItem];
		UINT mask = pItem->mask;
		if( mask & TCIF_TEXT )
		{
			pItemT->SetText(pItem->pszText);
		}
		// PBI: Added support for ImageList.
		if( mask & TCIF_IMAGE )
		{
			pItemT->iImage = pItem->iImage;
			pItemT->mask |= TCIF_IMAGE;
		}
		if( mask & TCIF_PARAM )
		{
			pItemT->lParam = pItem->lParam;
			pItemT->mask |= TCIF_PARAM;
		}

		// Repaint control
		T* pT = static_cast<T*>(this);
		pT->UpdateLayout();
		Invalidate();
		return TRUE;
	}
	*/

	TItem* GetItem(int nItem) const
	{
		ATLASSERT(::IsWindow(m_hWnd));
		CHECK_ITEM(nItem);

		return m_Items[nItem];
	}

	int SetCurSel(int nItem)
	{
		ATLASSERT(::IsWindow(m_hWnd));
		// Selecting same tab? Not worth it.
		if( nItem==m_iCurSel ) return m_iCurSel;
		if( nItem<-1 || nItem>=m_Items.GetSize() )
		{
			return m_iCurSel;
		}

		RECT rc = { 0 };
		int iOldSel = m_iCurSel;
		// Send notification
		NMHDR nmh = { m_hWnd, m_idDlgCtrl, TCN_SELCHANGING };
		if( ::SendMessage(GetParent(), WM_NOTIFY, nmh.idFrom, (LPARAM)&nmh)==TRUE ) return -1;
		// Repaint old button area
		GetItemRect(nItem, &rc); InvalidateRect(&rc);
		GetItemRect(iOldSel, &rc); InvalidateRect(&rc);

		// Change tab
		m_iCurSel = nItem;
		// Recalc new positions
		T* pT = static_cast<T*>(this);
		pT->UpdateLayout();

		//      // Repaint new button area with new button positions
		//      GetItemRect(nItem, &rc); InvalidateRect(&rc);
		//      GetItemRect(iOldSel, &rc); InvalidateRect(&rc);

		// DDB: Have the whole tab area redraw to avoid any visual artifacts
		// if implementations do anything more than keep each
		// tab the same size always (such as if the selected tab
		// is bolded and slightly bigger, or if there are divider lines
		// between tabs)
		GetClientRect(&rc); InvalidateRect(&rc);

		// Send notification
		nmh.code = TCN_SELCHANGE;
		::SendMessage(GetParent(), WM_NOTIFY, nmh.idFrom, (LPARAM)&nmh);
		return iOldSel;
	}

	int GetCurSel() const
	{
		ATLASSERT(::IsWindow(m_hWnd));
		return m_iCurSel;
	}

	int GetItemCount() const
	{
		ATLASSERT(::IsWindow(m_hWnd));
		return m_Items.GetSize();
	}

	int HitTest(LPTCHITTESTINFO pinfo) const
	{
		ATLASSERT(!::IsBadWritePtr(pinfo,sizeof(TCHITTESTINFO)));
		RECT rc;
		int nCount = m_Items.GetSize();
		for( int i=0; i<nCount; ++i )
		{
			GetItemRect(i, &rc);
			if( ::PtInRect(&rc, pinfo->pt) )
			{
				pinfo->flags = TCHT_ONITEM;
				return i;
			}
		}
		return -1;
	}

	int GetRowCount() const
	{
		return 1;
	}

	DWORD SetItemSize(int nItem, int cx, int cy)
	{
		ATLASSERT(::IsWindow(m_hWnd));
		CHECK_ITEM(nItem);

		TItem* pItem = m_Items[nItem];
		RECT rcItem = pItem->GetRect();
		pItem->SetRect(CRect(rcItem.left, rcItem.top, rcItem.left + cx, rcItem.top + cy));
		T* pT = static_cast<T*>(this);
		pT->UpdateLayout();
		Invalidate();
		return MAKELONG(rcItem.right-rcItem.left, rcItem.bottom-rcItem.top);
	}

	BOOL GetItemRect(int nItem, RECT *prcItem) const
	{
		ATLASSERT(::IsWindow(m_hWnd));
		ATLASSERT(prcItem);
		if( prcItem==NULL ) return FALSE;
		if( nItem<0 || nItem>=m_Items.GetSize() )
		{
			::SetRectEmpty(prcItem);
			return FALSE;
		}
		*prcItem = m_Items[nItem]->GetRect();
		return TRUE;
	}

	void SetPadding(int iPadding) 
	{ 
		m_iPadding = iPadding; 
		T* pT = static_cast<T*>(this);
		pT->UpdateLayout();
		Invalidate();
	};

	void GetSizeSettings(TC_SIZES *pSizes) const
	{
		ATLASSERT(!::IsBadWritePtr(pSizes,sizeof(TC_SIZES)));
		*pSizes = m_settings;
	}

	void SetSizeSettings(const TC_SIZES *pSizes)
	{
		ATLASSERT(!::IsBadReadPtr(pSizes,sizeof(TC_SIZES)));
		m_settings = *pSizes;
		T* pT = static_cast<T*>(this);
		pT->UpdateLayout();
		Invalidate();
	}

	// FindItem:    Find the next tab item matching the search criteria
	//              The functions is meant to mimic how
	//              CListViewCtrl::FindItem and LVM_FINDITEM work,
	//              since there are no comparable messages or functions
	//              for a tab control
	//
	//  pFindInfo should specify a mask of things to check for,
	//   and have the corresponding fields set (used in the check).
	//   For example, set the mask to TCIF_PARAM and set .lParam to
	//   what you want to find.
	//  If nStart is -1, the search begins from the beginning.
	//   If nStart is not -1, the search begins with the item
	//   just after nStart (like with LVM_FINDITEM).
	//  If a matching item is found, its index is returned.
	//   Otherwise -1 is returned.
	int FindItem(TItem* pFindItem, TItem::FieldFlags eFlags, int nStart = -1) const
	{
		ATLASSERT(::IsWindow(m_hWnd));
		if(nStart < 0)
		{
			nStart = -1;
		}

		// Find the next item matching the criteria specified
		int nCount = m_Items.GetSize();
		for( int i=(nStart+1); i < nCount; ++i )
		{
			if(m_Items[i]->MatchItem(pFindItem, eFlags))
			{
				return i;
			}
		}

		return -1;
	}

};


/////////////////////////////////////////////////////////////////////////////
//
// The sample tab controls
//
// The follwing samples derive directly from CCustomTabCtrl.
// This means that they can actually use the internal members
// of this class. But they will not! To keep the code clean, I'm only
// using public member methods to access all variables.
//
// You need to add the...
//   REFLECT_NOTIFICATIONS()
// macro to the parent's message map.
// (to handle NM_CUSTOMDRAW and possibly others)
//

class CButtonTabCtrl : 
   public CCustomTabCtrl<CButtonTabCtrl, CCoolTabItem>,
   public CCustomDraw<CButtonTabCtrl>
{
protected:
    typedef CCustomTabCtrl<CButtonTabCtrl, CCoolTabItem> coolTabClass;
    typedef CCustomDraw<CButtonTabCtrl> customDrawClass;

public:
   DECLARE_WND_CLASS(_T("WTL_CoolButtonTabCtrl"))

   BEGIN_MSG_MAP(CButtonTabCtrl)
      CHAIN_MSG_MAP(coolTabClass)
      CHAIN_MSG_MAP_ALT(customDrawClass, 1)
      REFLECTED_NOTIFY_CODE_HANDLER(TCN_INITIALIZE, OnInitialize)
      DEFAULT_REFLECTION_HANDLER()
   END_MSG_MAP()

   LRESULT OnInitialize(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& /*bHandled*/)
   {
      TC_SIZES sizes;
      GetSizeSettings(&sizes);
      sizes.iPadding = 10;
      sizes.iMargin = 3;
      SetSizeSettings(&sizes);
      return 0;
   }

   DWORD OnPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW /*lpNMCustomDraw*/)
   {
      return CDRF_NOTIFYITEMDRAW;   // We need per-item notifications
   }

   DWORD OnItemPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW lpNMCustomDraw)
   {
      ::SetBkMode(lpNMCustomDraw->hdc, TRANSPARENT);
      ::SetTextColor(lpNMCustomDraw->hdc, ::GetSysColor(COLOR_BTNTEXT));
      
      UINT state = 0;
      if( lpNMCustomDraw->uItemState & CDIS_SELECTED )
      {
          state |= DFCS_PUSHED;
      }
      if( lpNMCustomDraw->uItemState & CDIS_DISABLED )
      {
          state |= DFCS_INACTIVE;
      }
      ::DrawFrameControl(lpNMCustomDraw->hdc, &lpNMCustomDraw->rc, DFC_BUTTON, DFCS_BUTTONPUSH | state );
      
      CCoolTabItem* pItem = this->GetItem(lpNMCustomDraw->dwItemSpec);
      if( lpNMCustomDraw->uItemState & CDIS_SELECTED )
      {
         lpNMCustomDraw->rc.left += 2;
         lpNMCustomDraw->rc.top += 2;
      }
      CString sText = pItem->GetText();
      ::DrawText(lpNMCustomDraw->hdc, sText, sText.GetLength(), &lpNMCustomDraw->rc, DT_SINGLELINE|DT_CENTER|DT_VCENTER);

      return CDRF_SKIPDEFAULT;
   }
};

class CFolderTabCtrl : 
   public CCustomTabCtrl<CFolderTabCtrl, CCoolTabItem>,
   public CCustomDraw<CFolderTabCtrl>
{
protected:
    typedef CCustomTabCtrl<CFolderTabCtrl, CCoolTabItem> coolTabClass;
    typedef CCustomDraw<CFolderTabCtrl> customDrawClass;

public:
   DECLARE_WND_CLASS(_T("WTL_CoolFolderTabCtrl"))

   enum { CXOFFSET = 8 };     // defined pitch of trapezoid slant
   enum { CXMARGIN = 2 };     // left/right text margin
   enum { CYMARGIN = 1 };     // top/bottom text margin
   enum { CYBORDER = 1 };     // top border thickness
 
   BEGIN_MSG_MAP(CFolderTabCtrl)
      MESSAGE_HANDLER(WM_SETTINGCHANGE, OnSettingChange)
      CHAIN_MSG_MAP(coolTabClass)
      CHAIN_MSG_MAP_ALT(customDrawClass, 1)
      DEFAULT_REFLECTION_HANDLER()
   END_MSG_MAP()

   DWORD OnPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW /*lpNMCustomDraw*/)
   {
      return CDRF_NOTIFYITEMDRAW;   // We need per-item notifications
   }

   DWORD OnItemPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW lpNMCustomDraw)
   {
      CDCHandle dc( lpNMCustomDraw->hdc );
      bool bSelected = lpNMCustomDraw->uItemState & CDIS_SELECTED;

      COLORREF bgColor = ::GetSysColor(bSelected ? COLOR_WINDOW     : COLOR_3DFACE);
      COLORREF fgColor = ::GetSysColor(bSelected ? COLOR_WINDOWTEXT : COLOR_BTNTEXT);

      CBrush brush;
      brush.CreateSolidBrush(bgColor);     // background brush
      dc.SetBkColor(bgColor);              // text background
      dc.SetTextColor(fgColor);            // text color = fg color

      CPen shadowPen;
      shadowPen.CreatePen(PS_SOLID, 1, ::GetSysColor(COLOR_3DSHADOW));

      // Fill trapezoid
      POINT pts[4];
      RECT rc = lpNMCustomDraw->rc;
      _GetTrapezoid(rc, pts);
      CPenHandle hOldPen = dc.SelectStockPen(BLACK_PEN);   
      CRgn rgn;
      rgn.CreatePolygonRgn(pts, 4, WINDING);
      dc.FillRgn(rgn, brush);

      // Draw edges. This requires two corrections:
      // 1) Trapezoid dimensions don't include the right and bottom edges,
      //    so must use one pixel less on bottom (cybottom)
      // 2) the endpoint of LineTo is not included when drawing the line, so
      //    must add one pixel (cytop)
      pts[1].y--;       // correction #1: true bottom edge y-coord
      pts[2].y--;       // ...ditto
      pts[3].y--;       // correction #2: extend final LineTo
      dc.MoveTo(pts[0]);              // upper left
      dc.LineTo(pts[1]);              // bottom left
      dc.SelectPen(shadowPen);        // bottom line is shadow color
      dc.MoveTo(pts[1]);              // line is inside trapezoid bottom
      dc.LineTo(pts[2]);              // ...
      dc.SelectStockPen(BLACK_PEN);   // upstroke is black
      dc.LineTo(pts[3]);              // y-1 to include endpoint
      if( !bSelected ) {
         // If not highlighted, upstroke has a 3D shadow, one pixel inside
         pts[2].x--;    // offset left one pixel
         pts[3].x--;    // ...ditto
         dc.SelectPen(shadowPen);
         dc.MoveTo(pts[2]);
         dc.LineTo(pts[3]);
      }
      dc.SelectPen(hOldPen);

      CCoolTabItem* pItem = this->GetItem(lpNMCustomDraw->dwItemSpec);

      // Draw text
      CString sText = pItem->GetText();
      ::InflateRect(&rc, -(CXOFFSET + CXMARGIN), -CYMARGIN);
      dc.DrawText(sText, sText.GetLength(), &rc, DT_CENTER|DT_VCENTER|DT_SINGLELINE);

      return CDRF_SKIPDEFAULT;
   }

   void _GetTrapezoid(const RECT& rc, POINT* pts) const
   {
      pts[0].x = rc.left;
      pts[0].y = rc.top;
      pts[1].x = rc.left + CXOFFSET;
      pts[1].y = rc.bottom;
      pts[2].x = rc.right - CXOFFSET - 1;
      pts[2].y = rc.bottom;
      pts[3].x = rc.right - 1, rc.top;
      pts[3].y = rc.top;
   }

   LRESULT OnSettingChange(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
   {
      // Initialize font
      if( !m_font.IsNull() ) m_font.DeleteObject();
      LOGFONT lf = { 0 };      
      lf.lfHeight = ::GetSystemMetrics(SM_CYHSCROLL) - CYMARGIN;
      lf.lfWeight = FW_NORMAL;
      lf.lfCharSet = DEFAULT_CHARSET;
      ::lstrcpy(lf.lfFaceName, _T("Arial"));
      m_font.CreateFontIndirect(&lf);
 /*     
      NONCLIENTMETRICS ncm = { 0 };
      ncm.cbSize = sizeof(ncm);
      ::SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, &ncm, 0);
      ncm.lfSmCaptionFont.lfWeight = FW_NORMAL;
      m_font.CreateFontIndirect(&ncm.lfSmCaptionFont);
 */

      TC_SIZES sizes;
      GetSizeSettings(&sizes);
      sizes.iPadding = CXOFFSET + 3;
      sizes.iMargin = -CXOFFSET;
      SetSizeSettings(&sizes);

      Invalidate();
      return 0;
   }

};

class CFlatTabCtrl : 
   public CCustomTabCtrl<CFlatTabCtrl>,
   public CCustomDraw<CFlatTabCtrl>
{
protected:
    typedef CCustomTabCtrl<CFlatTabCtrl, CCoolTabItem> coolTabClass;
    typedef CCustomDraw<CFlatTabCtrl> customDrawClass;

public:
   DECLARE_WND_CLASS(_T("WTL_CoolFlatTabCtrl"))

   CFont m_font;  // DDB 2002/04/22: Leave this here even though the
                  //  base class now has CFont m_font, because there's
                  //  a "SetFont(m_fontBold)" that would cause
                  //  m_font and m_fontBold to be m_fontBold.
                  //  Keeping a version in this class here
                  //  will have the base class version
                  //  keep a copy of bold, but not cause
                  //  us to lose m_font.
   CBrush m_hbrBack;

   BEGIN_MSG_MAP(CFlatTabCtrl)
      MESSAGE_HANDLER(WM_ERASEBKGND, OnEraseBkgnd)
      CHAIN_MSG_MAP(coolTabClass)
      CHAIN_MSG_MAP_ALT(customDrawClass, 1)
      REFLECTED_NOTIFY_CODE_HANDLER(TCN_INITIALIZE, OnInitialize)
      DEFAULT_REFLECTION_HANDLER()
   END_MSG_MAP()

   LRESULT OnEraseBkgnd(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
   {
      CDCHandle dc((HDC)wParam);
      RECT rc;
      GetClientRect(&rc);
      HBRUSH hOldBrush = dc.SelectBrush(m_hbrBack);
      dc.PatBlt(rc.left, rc.top, rc.right-rc.left, rc.bottom-rc.top, PATCOPY);
      dc.SelectBrush(hOldBrush);
      return 1;
   }

   LRESULT OnInitialize(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& /*bHandled*/)
   {
      TC_SIZES sizes;
      GetSizeSettings(&sizes);
      sizes.iIndent = 6;
      sizes.iPadding = 0;
      sizes.iMargin = 2;
      sizes.iSelMargin = 4;
      SetSizeSettings(&sizes);
      return 0;
   }

   DWORD OnPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW lpNMCustomDraw)
   {
      CDCHandle dc( lpNMCustomDraw->hdc );
      dc.SetTextColor(::GetSysColor(COLOR_BTNTEXT));
      ::SetBkMode(lpNMCustomDraw->hdc, TRANSPARENT);

      return CDRF_NOTIFYITEMDRAW;   // We need per-item notifications
   }

   DWORD OnItemPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW lpNMCustomDraw)
   {
      CDCHandle dc( lpNMCustomDraw->hdc );
      bool bSelected = lpNMCustomDraw->uItemState & CDIS_SELECTED;
      RECT &rc = lpNMCustomDraw->rc;

      dc.FillRect(&rc, m_hbrBack);
      if( bSelected ) {
         // Tab is selected, so paint tab folder
         RECT rcTab = rc;
         rcTab.top += 5;
         rcTab.right--;
         dc.FillRect(&rcTab, (HBRUSH)(COLOR_BTNFACE+1));
         dc.SelectStockPen(WHITE_PEN);
         dc.MoveTo(rcTab.left, rcTab.bottom);
         dc.LineTo(rcTab.left, rcTab.top);
         dc.LineTo(rcTab.right, rcTab.top);
         dc.SelectStockPen(BLACK_PEN);
         dc.LineTo(rcTab.right, rcTab.bottom);
      }

      CCoolTabItem* pItem = this->GetItem(lpNMCustomDraw->dwItemSpec);
      TC_SIZES sizes;
      GetSizeSettings(&sizes);

      // Draw text
      HFONT hOldFont = dc.SelectFont(bSelected ? m_fontBold : m_font);
      RECT rcText = rc;
      ::InflateRect(&rcText, -sizes.iPadding, 0);
      rcText.bottom -= 3;

      CString sText = pItem->GetText();
      dc.DrawText(sText, sText.GetLength(), &rcText, DT_CENTER|DT_BOTTOM|DT_SINGLELINE);
      dc.SelectFont(hOldFont);

      return CDRF_SKIPDEFAULT;
   }

   LRESULT OnSettingChange(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
   {
      // Initialize font
      if( !m_font.IsNull() ) m_font.DeleteObject();
      NONCLIENTMETRICS ncm = { 0 };
      ncm.cbSize = sizeof(ncm);
      ::SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, &ncm, 0);
      ncm.lfSmCaptionFont.lfWeight = FW_NORMAL;
      m_font.CreateFontIndirect(&ncm.lfSmCaptionFont);
      ncm.lfSmCaptionFont.lfWeight = FW_BOLD;
      m_fontBold.CreateFontIndirect(&ncm.lfSmCaptionFont);
      SetFont(m_fontBold); // Bold font scales tabs correctly

      // Background brush
      if( !m_hbrBack.IsNull() ) m_hbrBack.DeleteObject();
      CWindowDC dc(NULL);
      int nBitsPerPixel = dc.GetDeviceCaps(BITSPIXEL);
      if( nBitsPerPixel > 8 ) {
         COLORREF clrBtnHilite = ::GetSysColor(COLOR_BTNHILIGHT);
         COLORREF clrBtnFace = ::GetSysColor(COLOR_BTNFACE);
         COLORREF clrLight = 
            RGB( GetRValue(clrBtnFace) + ((GetRValue(clrBtnHilite) - GetRValue(clrBtnFace)) / 2),
                 GetGValue(clrBtnFace) + ((GetGValue(clrBtnHilite) - GetGValue(clrBtnFace)) / 2),
                 GetBValue(clrBtnFace) + ((GetBValue(clrBtnHilite) - GetBValue(clrBtnFace)) / 2),
         );
         m_hbrBack.CreateSolidBrush(clrLight);
      }
      else {
         m_hbrBack =  CDCHandle::GetHalftoneBrush();
      }
      Invalidate();
      return 0;
   }
};


class CDotNetTabCtrl : 
	public CCustomTabCtrl<CDotNetTabCtrl>,
	public CCustomDraw<CDotNetTabCtrl>
{
protected:
	typedef CCustomTabCtrl<CDotNetTabCtrl, CCoolTabItem> coolTabClass;
	typedef CCustomDraw<CDotNetTabCtrl> customDrawClass;

protected:
	CBrush m_hbrBackground;
	COLORREF m_clrBackground, m_clrInactiveTab;

	signed char m_nFontSizeTextTopOffset;

	const signed char m_nMinWidthToDisplayText;

// Constructor
public:
	CDotNetTabCtrl() :
		m_clrBackground(::GetSysColor(COLOR_APPWORKSPACE)),
		m_clrInactiveTab(::GetSysColor(COLOR_GRAYTEXT)),
		m_nFontSizeTextTopOffset(0),
		m_nMinWidthToDisplayText(12)
	{
	}

// Methods
public:

// Message Handling
public:
	DECLARE_WND_CLASS(_T("WTL_CoolDotNetTabCtrl"))

	BEGIN_MSG_MAP(CDotNetTabCtrl)
		MESSAGE_HANDLER(WM_SETTINGCHANGE, OnSettingChange)
		CHAIN_MSG_MAP(coolTabClass)
		CHAIN_MSG_MAP_ALT(customDrawClass, 1)
		DEFAULT_REFLECTION_HANDLER()
	END_MSG_MAP()

	DWORD OnPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW lpNMCustomDraw)
	{
		CDCHandle dc( lpNMCustomDraw->hdc );

		// Erase Background
		//  (do it here instead of a handler for WM_ERASEBKGND
		//   so that we can do flicker-free drawing with the help
		//   of COffscreenDrawRect that's in the base class)
		RECT rc = {0};
		GetClientRect(&rc);

		// TODO: Don't "erase" entire client area.
		//  Do a smarter erase of just what needs it

		HBRUSH hOldBrush = dc.SelectBrush(m_hbrBackground);
		dc.PatBlt(rc.left, rc.top, rc.right-rc.left, rc.bottom-rc.top, PATCOPY);
		dc.SelectBrush(hOldBrush);

		// Connect with the client area.
		DWORD dwStyle = (DWORD)::GetWindowLong(m_hWnd, GWL_STYLE);
		BOOL bHasBottomStyle = dwStyle & TCS_BOTTOM;

		if (bHasBottomStyle)
		{
			rc.bottom = rc.top + 3;
			dc.FillSolidRect(&rc, GetSysColor(COLOR_BTNFACE));

			CPen penText;
			penText.CreatePen(PS_SOLID, 1, ::GetSysColor(COLOR_BTNTEXT));
			CPenHandle penOld = dc.SelectPen(penText);

			dc.MoveTo(rc.left, rc.bottom);
			dc.LineTo(rc.right, rc.bottom);

			dc.SelectPen(penOld);
		}
		else
		{
			int nOrigTop = rc.top;
			rc.top = rc.bottom - 2;
			dc.FillSolidRect(&rc, GetSysColor(COLOR_BTNFACE));

			CPen penHilight;
			penHilight.CreatePen(PS_SOLID, 1, ::GetSysColor(COLOR_BTNHIGHLIGHT));
			CPenHandle penOld = dc.SelectPen(penHilight);

			dc.MoveTo(rc.left, rc.top-1);
			dc.LineTo(rc.right, rc.top-1);

			rc.top = nOrigTop;

			CPen penShadow;
			penShadow.CreatePen(PS_SOLID, 1, ::GetSysColor(COLOR_BTNSHADOW));
			dc.SelectPen(penShadow);
			dc.MoveTo(rc.left, rc.bottom);
			dc.LineTo(rc.left, rc.top);
			dc.LineTo(rc.right-1, rc.top);
			dc.LineTo(rc.right-1, rc.bottom);

			dc.SelectPen(penOld);
		}

		// Set up the text color and background mode
		dc.SetTextColor(::GetSysColor(COLOR_BTNTEXT));
		::SetBkMode(lpNMCustomDraw->hdc, TRANSPARENT);

		return CDRF_NOTIFYITEMDRAW;   // We need per-item notifications
	}

	DWORD OnItemPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW lpNMCustomDraw)
	{
		CDCHandle dc( lpNMCustomDraw->hdc );
		bool bSelected = lpNMCustomDraw->uItemState & CDIS_SELECTED;
		RECT &rc = lpNMCustomDraw->rc;

		DWORD dwStyle = (DWORD)::GetWindowLong(m_hWnd, GWL_STYLE);
		BOOL bHasBottomStyle = dwStyle & TCS_BOTTOM;

		int nIconVerticalCenter = 0;

		CRect rcTab(rc);
		CRect rcText(rc);

		if (bHasBottomStyle)
		{
			rcTab.top += 3;
			rcTab.bottom -= 2;

			rcText.top = rcTab.top+1 + m_nFontSizeTextTopOffset;
			rcText.bottom = rc.bottom;
			//nIconVerticalCenter = rcTab.top + (rc.bottom - rcTab.top) / 2;
			//nIconVerticalCenter = rcTab.top + rcText.Height() / 2;
			nIconVerticalCenter = (rc.bottom + rc.top) / 2 + rcTab.top / 2;
		}
		else
		{
			rcTab.top += 3;
			rcTab.bottom -= 2;

			rcText.top = rc.top+1 + m_nFontSizeTextTopOffset;
			rcText.bottom = rc.bottom;
			nIconVerticalCenter = (rc.bottom + rc.top) / 2 + rcTab.top / 2;
		}

		if( bSelected )
		{
			// Tab is selected, so paint tab folder

			rcTab.right--;
			dc.FillRect(&rcTab, (HBRUSH)(COLOR_BTNFACE+1));

			CPen penText, penHilight;
			penText.CreatePen(PS_SOLID, 1, ::GetSysColor(COLOR_BTNTEXT));
			penHilight.CreatePen(PS_SOLID, 1, ::GetSysColor(COLOR_BTNHIGHLIGHT));

			if (bHasBottomStyle)
			{
				CPenHandle penOld = dc.SelectPen(penText);

				dc.MoveTo(rcTab.right, rcTab.top);
				dc.LineTo(rcTab.right, rcTab.bottom);
				dc.LineTo(rcTab.left, rcTab.bottom);
				dc.SelectPen(penHilight);
				dc.LineTo(rcTab.left, rcTab.top-1);

				dc.SelectPen(penOld);
			}
			else
			{
				CPenHandle penOld = dc.SelectPen(penHilight);

				dc.MoveTo(rcTab.left, rcTab.bottom-1);
				dc.LineTo(rcTab.left, rcTab.top);
				dc.LineTo(rcTab.right, rcTab.top);
				dc.SelectPen(penText);
				dc.LineTo(rcTab.right, rcTab.bottom);

				dc.SelectPen(penOld);
			}
		}
		else
		{
			// Tab is not selected

			// Draw division line on right, unless we're the item
			// on the left of the selected tab
			if(lpNMCustomDraw->dwItemSpec + 1 == (DWORD)m_iCurSel)
			{
				// Item just left of selected tab
			}
			else
			{
				CPen pen;
				pen.CreatePen(PS_SOLID, 1, ::GetSysColor(COLOR_BTNSHADOW));
				CPenHandle penOld = dc.SelectPen(pen);
				if(bHasBottomStyle)
				{
					// Important!  Be sure and keep within "our" tab area horizontally
					dc.MoveTo(rcTab.right-1, rcTab.top + 3);
					dc.LineTo(rcTab.right-1, rcTab.bottom - 1);
				}
				else
				{
					// Important!  Be sure and keep within "our" tab area horizontally
					dc.MoveTo(rcTab.right-1, rcTab.top + 2);
					dc.LineTo(rcTab.right-1, rcTab.bottom - 2);
				}
				dc.SelectPen(penOld);
			}
		}

		CCoolTabItem* pItem = this->GetItem(lpNMCustomDraw->dwItemSpec);

		TC_SIZES sizes;
		GetSizeSettings(&sizes);

		HFONT hOldFont = dc.SelectFont((bSelected && m_bBoldSelectedTab) ? m_fontBold : m_font);
		COLORREF crPrevious = dc.GetTextColor();
		if( !bSelected )
		{
			crPrevious = dc.SetTextColor(m_clrInactiveTab);
		}
		else
		{
			crPrevious = dc.SetTextColor(GetSysColor(COLOR_BTNTEXT));
		}

		//--------------------------------------------
		// This is how CDotNetTabCtrl interprets padding, margin, etc.:
		//
		//  M - Margin
		//  P - Padding
		//  I - Image
		//  Text - Tab Text
		//
		// With image:
		//     __________________________
		//
		//    | M | I | P | Text | P | M |
		//     --------------------------
		//
		// Without image:
		//     ______________________
		//
		//    | M | P | Text | P | M |
		//     ----------------------

		//rcText.left += (bSelected ? sizes.iSelMargin : sizes.iMargin);
		rcText.left += sizes.iMargin;
		rcText.right -= sizes.iMargin;
		if (pItem->UsingImage() && !m_imageList.IsNull())
		{
			// Draw the image.
			IMAGEINFO ii = {0};
			int nImageIndex = pItem->GetImageIndex();
			m_imageList.GetImageInfo(nImageIndex, &ii);

			if( (ii.rcImage.right - ii.rcImage.left) < (rcTab.right - rcTab.left) )
			{
				int nImageHalfHeight = (ii.rcImage.bottom - ii.rcImage.top) / 2;
				m_imageList.Draw(dc, nImageIndex, rcText.left, nIconVerticalCenter - nImageHalfHeight + m_nFontSizeTextTopOffset, ILD_NORMAL);
			}

			// Offset on the right of the image.
			rcText.left += (ii.rcImage.right - ii.rcImage.left);
		}

		if (rcText.left + m_nMinWidthToDisplayText < rcText.right)
		{
			rcText.InflateRect(-sizes.iPadding, 0);

			CString sText = pItem->GetText();
			dc.DrawText(sText, sText.GetLength(), &rcText, DT_LEFT | DT_VCENTER | DT_SINGLELINE | DT_END_ELLIPSIS);
		}

		dc.SetTextColor(crPrevious);
		dc.SelectFont(hOldFont);

		return CDRF_SKIPDEFAULT;
	}

	LRESULT OnSettingChange(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		// Initialize/Reinitialize font

		// Visual Studio.Net seems to use the "icon" font for the tabs
		LOGFONT lfIcon = { 0 };
		::SystemParametersInfo(SPI_GETICONTITLELOGFONT, sizeof(lfIcon), &lfIcon, 0);

		bool bResetFont = true;
		if( !m_font.IsNull() )
		{
			LOGFONT lf = {0};
			if(m_font.GetLogFont(&lf))
			{
				if(lstrcmpi(lf.lfFaceName, lfIcon.lfFaceName) == 0 &&
					lf.lfHeight == lfIcon.lfHeight)
				{
					bResetFont = false;
				}
			}
		}

		if(bResetFont)
		{
			if(m_font) m_font.DeleteObject();

			HFONT font = m_font.CreateFontIndirect(&lfIcon);
			if(font==NULL)
			{
				m_font.Attach(AtlGetStockFont(DEFAULT_GUI_FONT));
			}

			if(m_bBoldSelectedTab)
			{
				lfIcon.lfWeight = FW_BOLD;
				font = m_fontBold.CreateFontIndirect(&lfIcon);
				if(font==NULL)
				{
					m_fontBold.Attach(AtlGetStockFont(DEFAULT_GUI_FONT));
				}
			}
		}

		// Background brush
		if( !m_hbrBackground.IsNull() ) m_hbrBackground.DeleteObject();
		CWindowDC dcWindow(NULL);
		int nBitsPerPixel = dcWindow.GetDeviceCaps(BITSPIXEL);
		if( nBitsPerPixel > 8 )
		{
			COLORREF clrBtnHilite = ::GetSysColor(COLOR_BTNHILIGHT);
			COLORREF clrBtnFace = ::GetSysColor(COLOR_BTNFACE);
			COLORREF clrLight = 
				RGB( GetRValue(clrBtnFace) + ((GetRValue(clrBtnHilite) - GetRValue(clrBtnFace)) / 2),
					GetGValue(clrBtnFace) + ((GetGValue(clrBtnHilite) - GetGValue(clrBtnFace)) / 2),
					GetBValue(clrBtnFace) + ((GetBValue(clrBtnHilite) - GetBValue(clrBtnFace)) / 2),
					);
			//m_hbrBackground.CreateSolidBrush(clrLight);

			// This is a brave attempt to mimic the algorithm that Visual Studio.Net
			// uses to calculate the tab's background color and inactive tab color.
			// The other colors that VS.Net uses seems to be standard ones,
			// but these two colors are calculated.
			BYTE nRed = 0, nGreen = 0, nBlue = 0, nMax = 0;

			// Early experiments seemed to reveal that the background color is dependant
			// on COLOR_BTNFACE.  The following algorithm is just an attempt
			// to match several empirical results.  I tested with 20 variations
			// on COLOR_BTNFACE and kept track of what the tab background became.
			// I then brought the numbers into Excel, and started crunching on the numbers
			// until I came up with a formula that seems to pretty well match.

			nRed = GetRValue(clrBtnFace);
			nGreen = GetGValue(clrBtnFace);
			nBlue = GetBValue(clrBtnFace);

			nMax = (nRed > nGreen) ? ((nRed > nBlue) ? nRed : nBlue) : ((nGreen > nBlue) ? nGreen : nBlue);
			const BYTE nMagicBackgroundOffset = (nMax > (0xFF - 35)) ? (0xFF - nMax) : 35;
			nRed +=		(nMax==0) ? nMagicBackgroundOffset : (BYTE)(nMagicBackgroundOffset*(nRed/(double)nMax) + 0.5);
			nGreen +=	(nMax==0) ? nMagicBackgroundOffset : (BYTE)(nMagicBackgroundOffset*(nGreen/(double)nMax) + 0.5);
			nBlue +=	(nMax==0) ? nMagicBackgroundOffset : (BYTE)(nMagicBackgroundOffset*(nBlue/(double)nMax) + 0.5);

			m_clrBackground = RGB(nRed, nGreen, nBlue);
			m_hbrBackground.CreateSolidBrush(m_clrBackground);


			// The inactive tab color seems to be calculated in a similar way to
			// the tab background, only instead of lightening BNTFACE, it darkens GRAYTEXT.
			COLORREF clrGrayText = ::GetSysColor(COLOR_GRAYTEXT);

			nRed = GetRValue(clrGrayText);
			nGreen = GetGValue(clrGrayText);
			nBlue = GetBValue(clrGrayText);

			nMax = (nRed > nGreen) ? ((nRed > nBlue) ? nRed : nBlue) : ((nGreen > nBlue) ? nGreen : nBlue);
			const BYTE nMagicInactiveOffset = 43;
			nRed -=		(nMax==0) ? 0 : ((nRed < nMagicInactiveOffset) ? (nRed/2) :		(BYTE)(nMagicInactiveOffset*(nRed/(double)nMax) + 0.5));
			nGreen -=	(nMax==0) ? 0 : ((nGreen < nMagicInactiveOffset) ? (nGreen/2) :	(BYTE)(nMagicInactiveOffset*(nGreen/(double)nMax) + 0.5));
			nBlue -=	(nMax==0) ? 0 : ((nBlue < nMagicInactiveOffset) ? (nBlue/2) :	(BYTE)(nMagicInactiveOffset*(nBlue/(double)nMax) + 0.5));

			m_clrInactiveTab = RGB(nRed, nGreen, nBlue);
		}
		else
		{
			m_hbrBackground =  CDCHandle::GetHalftoneBrush();
			m_clrInactiveTab = ::GetSysColor(COLOR_GRAYTEXT);
		}

		TC_SIZES sizes = {0};
		GetSizeSettings(&sizes);
		sizes.iIndent = 5;
		sizes.iPadding = 4;
		sizes.iMargin = 3;
		sizes.iSelMargin = 3;
		SetSizeSettings(&sizes);

		int nHeightLogicalUnits = -lfIcon.lfHeight;
		// In MSDN for "LOGFONT", they give the following formula for calculating
		// the log font height given a point size.
		//long lfHeight = -MulDiv(PointSize, GetDeviceCaps(hDC, LOGPIXELSY), 72);

		const int nNominalFontLogicalUnits = 11;	// 8 point Tahoma with 96 DPI
		m_nFontSizeTextTopOffset = (nHeightLogicalUnits - nNominalFontLogicalUnits) / 2;

		Invalidate();
		return 0;
	}


	// Override standard behavior while resizing the control.

	void UpdateLayout()
	{
		if(!m_hWnd || !::IsWindow(m_hWnd))
		{
			return;
		}

		LONG nMinInactiveWidth = 0x7FFFFFFF;
		LONG nMaxInactiveWidth = 0;

		CClientDC dc(m_hWnd);
		HFONT hOldFont = dc.SelectFont(m_font);

		CRect rcClient;
		GetClientRect(&rcClient);

		CRect rcItem = rcClient;
		// rcItem.top and rcItem.bottom aren't really going to change

		// Recalculate tab positions and widths
		// See OnItemPrePaint for a discussion of how CDotNetTabCtrl
		//  interprets margin, padding, etc.
		int nCount = m_Items.GetSize();
		int xpos = m_settings.iIndent;
		HFONT hRestoreNormalFont = NULL;
		for( int i=0; i<nCount; ++i )
		{
			bool bSelected = (i == m_iCurSel);
			if(m_bBoldSelectedTab && bSelected)
			{
				hRestoreNormalFont = dc.SelectFont(m_fontBold);
			}

			CCoolTabItem* pItem = m_Items[i];
			rcItem.left = rcItem.right = xpos;
			//rcItem.right += ((bSelected ? m_settings.iSelMargin : m_settings.iMargin));
			rcItem.right += m_settings.iMargin;
			if( pItem->UsingImage() && !m_imageList.IsNull())
			{
				IMAGEINFO ii = {0};
				int nImageIndex = pItem->GetImageIndex();
				m_imageList.GetImageInfo(nImageIndex, &ii);
				rcItem.right += (ii.rcImage.right - ii.rcImage.left);
			}
			if( pItem->UsingText() )
			{
				CRect rcText;
				CString sText = pItem->GetText();
				dc.DrawText(sText, sText.GetLength(), &rcText, DT_SINGLELINE | DT_CALCRECT);
				rcItem.right += (rcText.Width()) + (m_settings.iPadding * 2);
			}
			rcItem.right += m_settings.iMargin;
			pItem->SetRect(rcItem);
			xpos += (rcItem.Width());

			if(hRestoreNormalFont != NULL)
			{
				dc.SelectFont(hRestoreNormalFont);
				hRestoreNormalFont = NULL;
			}

			if(!bSelected)
			{
				if(rcItem.Width() < nMinInactiveWidth)
				{
					nMinInactiveWidth = rcItem.Width();
				}
				if(rcItem.Width() > nMaxInactiveWidth)
				{
					nMaxInactiveWidth = rcItem.Width();
				}
			}
		}
		xpos += m_settings.iIndent;

		if(xpos > rcClient.Width() && nCount > 0 && m_iCurSel >= 0)
		{
			// Our desired widths are more than the width of the client area.
			// We need to have some or all of the tabs give up some real estate

			// We'll try to let the selected tab have its fully desired width.
			// If it can't, we'll make all the tabs the same width.

			CRect rcSelected = m_Items[m_iCurSel]->GetRect();

			int cxClientInactiveTabs = rcClient.Width() - (m_settings.iIndent * 2) - rcSelected.Width();

			double nRatioWithSelectionFullSize = (double) (cxClientInactiveTabs) / (double)(xpos - (m_settings.iIndent * 2) - rcSelected.Width());

			long nInactiveSameSizeWidth = (m_nMinWidthToDisplayText + (m_settings.iMargin*2) + (m_settings.iPadding));

			if(cxClientInactiveTabs > (nInactiveSameSizeWidth * (nCount-1)))
			{
				//  There should be enough room to display the entire contents of
				//  the selected tab plus something for the inactive tabs

				bool bMakeInactiveSameSize = ((nMinInactiveWidth * nRatioWithSelectionFullSize) < nInactiveSameSizeWidth);

				xpos = m_settings.iIndent;
				for(i=0; i<nCount; ++i )
				{
					CCoolTabItem* pItem = m_Items[i];
					CRect rcItemDesired = pItem->GetRect();
					rcItem.left = rcItem.right = xpos;
					if(i == m_iCurSel)
					{
						rcItem.right += rcItemDesired.Width();
					}
					else
					{
						if(bMakeInactiveSameSize)
						{
							rcItem.right += (LONG)((cxClientInactiveTabs / (nCount-1)) + 0.5);
						}
						else
						{
							rcItem.right += (LONG)((rcItemDesired.Width() * nRatioWithSelectionFullSize) + 0.5);
						}
					}
					pItem->SetRect(rcItem);
					xpos += (rcItem.right-rcItem.left);
				}
			}
			else
			{
				// We're down pretty small, so just make all the tabs the same width
				int cxItem = (rcClient.Width() - (m_settings.iIndent*2)) / nCount;

				xpos = m_settings.iIndent;

				for(i=0; i<nCount; ++i)
				{
					rcItem.left = rcItem.right = xpos;
					rcItem.right += cxItem;
					m_Items[i]->SetRect(rcItem);
					xpos += (rcItem.right-rcItem.left);
				}
			}
		}

		dc.SelectFont(hOldFont);

		// The way we implement tooltips is to have as many
		// "tools" as there are tabs.  The relationship of
		// tool ID => tab index is:
		// tool ID = tab index + 1	(to avoid 0 as an ID)
		//
		// We supply the RECT here and text elsewhere.
		if(m_tooltip.IsWindow())
		{
			for(i=0; i<nCount; ++i )
			{
				CCoolTabItem* pItem = m_Items[i];
				m_tooltip.SetToolRect(m_hWnd, i+1, pItem->GetRectRef());
			}
		}
	}

};


class CDotNetButtonTabCtrl : 
	public CCustomTabCtrl<CDotNetButtonTabCtrl>,
	public CCustomDraw<CDotNetButtonTabCtrl>
{
protected:
	typedef CCustomTabCtrl<CDotNetButtonTabCtrl, CCoolTabItem> coolTabClass;
	typedef CCustomDraw<CDotNetButtonTabCtrl> customDrawClass;

protected:
	CBrush m_hbrBackground;

	const signed char m_nMinWidthToDisplayText;

// Constructor
public:
	CDotNetButtonTabCtrl() :
		m_nMinWidthToDisplayText(12)
	{
	}

// Message Handling
public:
	DECLARE_WND_CLASS(_T("WTL_CoolDotNetButtonTabCtrl"))

	BEGIN_MSG_MAP(CDotNetButtonTabCtrl)
		MESSAGE_HANDLER(WM_SETTINGCHANGE, OnSettingChange)
		CHAIN_MSG_MAP(coolTabClass)
		CHAIN_MSG_MAP_ALT(customDrawClass, 1)
		DEFAULT_REFLECTION_HANDLER()
	END_MSG_MAP()

	DWORD OnPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW lpNMCustomDraw)
	{
		CDCHandle dc( lpNMCustomDraw->hdc );

		// Erase Background
		//  (do it here instead of a handler for WM_ERASEBKGND
		//   so that we can do flicker-free drawing with the help
		//   of COffscreenDrawRect that's in the base class)
		// Note: Because the "erase" part is very simple, and only coloring
		//  it with the background color, we can do a smarter erase.
		//  Instead of erasing the whole client area (which might be clipped),
		//  We'll just ask the HDC for the clip box.

		RECT rc = {0};
		//GetClientRect(&rc);
		dc.GetClipBox(&rc);


		HBRUSH hOldBrush = dc.SelectBrush(m_hbrBackground);
		dc.PatBlt(rc.left, rc.top, rc.right-rc.left, rc.bottom-rc.top, PATCOPY);
		dc.SelectBrush(hOldBrush);


		// Set up the text color and background mode
		dc.SetTextColor(::GetSysColor(COLOR_BTNTEXT));
		::SetBkMode(lpNMCustomDraw->hdc, TRANSPARENT);

		return CDRF_NOTIFYITEMDRAW;   // We need per-item notifications
	}

	DWORD OnItemPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW lpNMCustomDraw)
	{
		CDCHandle dc( lpNMCustomDraw->hdc );
		bool bSelected = lpNMCustomDraw->uItemState & CDIS_SELECTED;
		RECT &rc = lpNMCustomDraw->rc;

		DWORD dwStyle = (DWORD)::GetWindowLong(m_hWnd, GWL_STYLE);
		BOOL bHasBottomStyle = dwStyle & TCS_BOTTOM;

		int nIconVerticalCenter = (rc.bottom + rc.top) / 2;

		CRect rcTab(rc);
		CRect rcText(rc);

		rcTab.top += 3;
		rcTab.bottom -= 3;

		if( bSelected )
		{
			// Tab is selected, so paint as select

			CPen penOutline;
			penOutline.CreatePen(PS_SOLID, 1, ::GetSysColor(COLOR_HIGHLIGHT));

			CBrush brushSelected;
			brushSelected.CreateSysColorBrush(COLOR_WINDOW);

			HPEN hOldPen = dc.SelectPen(penOutline);
			HBRUSH hOldBrush = dc.SelectBrush(brushSelected);

			dc.Rectangle(&rcTab);

			dc.SelectPen(hOldPen);
			dc.SelectBrush(hOldBrush);
		}
		else
		{
			// Tab is not selected

			// Draw division line on right, unless we're the item
			// on the left of the selected tab
			if(lpNMCustomDraw->dwItemSpec + 1 == (DWORD)m_iCurSel)
			{
				// Item just left of selected tab
			}
			else
			{
				CPen pen;
				pen.CreatePen(PS_SOLID, 1, ::GetSysColor(COLOR_BTNSHADOW));
				CPenHandle penOld = dc.SelectPen(pen);
				if(bHasBottomStyle)
				{
					// Important!  Be sure and keep within "our" tab area horizontally
					dc.MoveTo(rcTab.right-1, rcTab.top + 3);
					dc.LineTo(rcTab.right-1, rcTab.bottom - 1);
				}
				else
				{
					// Important!  Be sure and keep within "our" tab area horizontally
					dc.MoveTo(rcTab.right-1, rcTab.top + 2);
					dc.LineTo(rcTab.right-1, rcTab.bottom - 2);
				}
				dc.SelectPen(penOld);
			}
		}

		CCoolTabItem* pItem = this->GetItem(lpNMCustomDraw->dwItemSpec);

		TC_SIZES sizes;
		GetSizeSettings(&sizes);

		HFONT hOldFont = dc.SelectFont((bSelected && m_bBoldSelectedTab) ? m_fontBold : m_font);
		COLORREF crPrevious = dc.SetTextColor(GetSysColor(COLOR_BTNTEXT));


		//--------------------------------------------
		// This is how CDotNetButtonTabCtrl interprets padding, margin, etc.:
		//
		//  M - Margin
		//  P - Padding
		//  I - Image
		//  Text - Tab Text
		//
		// With image:
		//     __________________________
		//
		//    | M | I | P | Text | P | M |
		//     --------------------------
		//
		// Without image:
		//     ______________________
		//
		//    | M | P | Text | P | M |
		//     ----------------------

		//rcText.left += (bSelected ? sizes.iSelMargin : sizes.iMargin);
		rcText.left += sizes.iMargin;
		rcText.right -= sizes.iMargin;
		if (pItem->UsingImage() && !m_imageList.IsNull())
		{
			// Draw the image.
			IMAGEINFO ii = {0};
			int nImageIndex = pItem->GetImageIndex();
			m_imageList.GetImageInfo(nImageIndex, &ii);

			if( (ii.rcImage.right - ii.rcImage.left) < (rcTab.right - rcTab.left) )
			{
				int nImageHalfHeight = (ii.rcImage.bottom - ii.rcImage.top) / 2;
				m_imageList.Draw(dc, nImageIndex, rcText.left, nIconVerticalCenter - nImageHalfHeight, ILD_NORMAL);
			}

			// Offset on the right of the image.
			rcText.left += (ii.rcImage.right - ii.rcImage.left);
		}

		if (rcText.left + m_nMinWidthToDisplayText < rcText.right)
		{
			rcText.InflateRect(-sizes.iPadding, 0);

			CString sText = pItem->GetText();
			dc.DrawText(sText, sText.GetLength(), &rcText, DT_LEFT | DT_VCENTER | DT_SINGLELINE | DT_PATH_ELLIPSIS);
		}

		dc.SetTextColor(crPrevious);
		dc.SelectFont(hOldFont);

		return CDRF_SKIPDEFAULT;
	}

	LRESULT OnSettingChange(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		// Initialize/Reinitialize font

		// Visual Studio.Net seems to use the "icon" font for the tabs
		LOGFONT lfIcon = { 0 };
		::SystemParametersInfo(SPI_GETICONTITLELOGFONT, sizeof(lfIcon), &lfIcon, 0);

		bool bResetFont = true;
		if( !m_font.IsNull() )
		{
			LOGFONT lf = {0};
			if(m_font.GetLogFont(&lf))
			{
				if(lstrcmpi(lf.lfFaceName, lfIcon.lfFaceName) == 0 &&
					lf.lfHeight == lfIcon.lfHeight)
				{
					bResetFont = false;
				}
			}
		}

		if(bResetFont)
		{
			if(m_font) m_font.DeleteObject();

			HFONT font = m_font.CreateFontIndirect(&lfIcon);
			if(font==NULL)
			{
				m_font.Attach(AtlGetStockFont(DEFAULT_GUI_FONT));
			}

			if(m_bBoldSelectedTab)
			{
				lfIcon.lfWeight = FW_BOLD;
				font = m_fontBold.CreateFontIndirect(&lfIcon);
				if(font==NULL)
				{
					m_fontBold.Attach(AtlGetStockFont(DEFAULT_GUI_FONT));
				}
			}
		}

		// Background brush
		if( !m_hbrBackground.IsNull() ) m_hbrBackground.DeleteObject();

		m_hbrBackground.CreateSysColorBrush(COLOR_BTNFACE);

		TC_SIZES sizes = {0};
		GetSizeSettings(&sizes);
		sizes.iIndent = 5;
		sizes.iPadding = 4;
		sizes.iMargin = 3;
		sizes.iSelMargin = 3;
		SetSizeSettings(&sizes);

		Invalidate();
		return 0;
	}


	// Override standard behavior while resizing the control.

	void UpdateLayout()
	{
		if(!m_hWnd || !::IsWindow(m_hWnd))
		{
			return;
		}

		LONG nMinInactiveWidth = 0x7FFFFFFF;
		LONG nMaxInactiveWidth = 0;

		CClientDC dc(m_hWnd);
		HFONT hOldFont = dc.SelectFont(m_font);

		CRect rcClient;
		GetClientRect(&rcClient);

		CRect rcItem = rcClient;
		// rcItem.top and rcItem.bottom aren't really going to change

		// Reposition buttons
		// See OnItemPrePaint for a discussion of how CDotNetButtonTabCtrl
		//  interprets margin, padding, etc.
		int nCount = m_Items.GetSize();
		int xpos = m_settings.iIndent;
		HFONT hRestoreNormalFont = NULL;
		for( int i=0; i<nCount; ++i )
		{
			bool bSelected = (i == m_iCurSel);
			if(m_bBoldSelectedTab && bSelected)
			{
				hRestoreNormalFont = dc.SelectFont(m_fontBold);
			}

			CCoolTabItem* pItem = m_Items[i];
			rcItem.left = rcItem.right = xpos;
			//rcItem.right += ((bSelected ? m_settings.iSelMargin : m_settings.iMargin));
			rcItem.right += m_settings.iMargin;
			if( pItem->UsingImage() && !m_imageList.IsNull())
			{
				IMAGEINFO ii = {0};
				m_imageList.GetImageInfo(pItem->GetImageIndex(), &ii);
				rcItem.right += (ii.rcImage.right - ii.rcImage.left);
			}
			if( pItem->UsingText() )
			{
				CRect rcText;
				CString sText = pItem->GetText();
				dc.DrawText(sText, sText.GetLength(), &rcText, DT_SINGLELINE | DT_CALCRECT);
				rcItem.right += (rcText.Width()) + (m_settings.iPadding * 2);
			}
			rcItem.right += m_settings.iMargin;
			pItem->SetRect(rcItem);
			xpos += (rcItem.Width());

			if(hRestoreNormalFont != NULL)
			{
				dc.SelectFont(hRestoreNormalFont);
				hRestoreNormalFont = NULL;
			}

			if(!bSelected)
			{
				if(rcItem.Width() < nMinInactiveWidth)
				{
					nMinInactiveWidth = rcItem.Width();
				}
				if(rcItem.Width() > nMaxInactiveWidth)
				{
					nMaxInactiveWidth = rcItem.Width();
				}
			}
		}
		xpos += m_settings.iIndent;

		if(xpos > rcClient.Width() && nCount > 0 && m_iCurSel >= 0)
		{
			// Our desired widths are more than the width of the client area.
			// We need to have some or all of the tabs give up some real estate

			// We'll try to let the selected tab have its fully desired width.
			// If it can't, we'll make all the tabs the same width.

			CRect rcSelected = m_Items[m_iCurSel]->GetRect();

			int cxClientInactiveTabs = rcClient.Width() - (m_settings.iIndent * 2) - rcSelected.Width();

			double nRatioWithSelectionFullSize = (double) (cxClientInactiveTabs) / (double)(xpos - (m_settings.iIndent * 2) - rcSelected.Width());

			long nInactiveSameSizeWidth = (m_nMinWidthToDisplayText + (m_settings.iMargin*2) + (m_settings.iPadding));

			if(cxClientInactiveTabs > (nInactiveSameSizeWidth * (nCount-1)))
			{
				//  There should be enough room to display the entire contents of
				//  the selected tab plus something for the inactive tabs

				bool bMakeInactiveSameSize = ((nMinInactiveWidth * nRatioWithSelectionFullSize) < nInactiveSameSizeWidth);

				xpos = m_settings.iIndent;
				for(i=0; i<nCount; ++i )
				{
					CCoolTabItem* pItem = m_Items[i];
					CRect rcItemDesired = pItem->GetRect();
					rcItem.left = rcItem.right = xpos;
					if(i == m_iCurSel)
					{
						rcItem.right += rcItemDesired.Width();
					}
					else
					{
						if(bMakeInactiveSameSize)
						{
							rcItem.right += (LONG)((cxClientInactiveTabs / (nCount-1)) + 0.5);
						}
						else
						{
							rcItem.right += (LONG)((rcItemDesired.Width() * nRatioWithSelectionFullSize) + 0.5);
						}
					}
					pItem->SetRect(rcItem);
					xpos += (rcItem.right-rcItem.left);
				}
			}
			else
			{
				// We're down pretty small, so just make all the tabs the same width
				int cxItem = (rcClient.Width() - (m_settings.iIndent*2)) / nCount;

				xpos = m_settings.iIndent;

				for(i=0; i<nCount; ++i)
				{
					rcItem.left = rcItem.right = xpos;
					rcItem.right += cxItem;
					m_Items[i]->SetRect(rcItem);
					xpos += (rcItem.right-rcItem.left);
				}
			}
		}

		dc.SelectFont(hOldFont);

		// The way we implement tooltips is to have as many
		// "tools" as there are tabs.  The relationship of
		// tool ID => tab index is:
		// tool ID = tab index + 1	(to avoid 0 as an ID)
		//
		// We supply the RECT here and text elsewhere.
		if(m_tooltip.IsWindow())
		{
			for(i=0; i<nCount; ++i )
			{
				CCoolTabItem* pItem = m_Items[i];
				m_tooltip.SetToolRect(m_hWnd, i+1, pItem->GetRectRef());
			}
		}
	}

};


#endif // __COOLTABCTRLS_H__

