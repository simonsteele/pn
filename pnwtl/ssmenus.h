/**
 * @file ssmenus.h
 * @brief Interface for the menu functionality classes.
 * @author Simon Steele
 * @note copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 * 
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef ssmenus_h__included
#define ssmenus_h__included

const long mfchecked[] = {MF_UNCHECKED, MF_CHECKED};
const long mfenabled[] = {MF_DISABLED | MF_GRAYED, MF_ENABLED};
const long mfdefault[] = {0, MFS_DEFAULT};

template <bool t_bManaged> class CSMenuT;
typedef CSMenuT<true> CSMenu;
typedef CSMenuT<false> CSMenuHandle;

#include "include/sscontainers.h"

/** 
 * @class CSMenu
 * @brief Class to ease working with menus
 */
template <bool t_bManaged>
class CSMenuT
{
	public:
		explicit CSMenuT() : m_hMenu(NULL)
		{
		}

		explicit CSMenuT(HMENU hMenu) : m_hMenu(hMenu)
		{
		}

		virtual ~CSMenuT()
		{
			if (t_bManaged)
			{
				Destroy();
			}
		}

		void Destroy()
		{
			if (m_hMenu)
			{
				::DestroyMenu(m_hMenu);
				m_hMenu = NULL;
			}
		}

		void LoadMenu(int resource)
		{
			::LoadMenu(GetAppInstance(), MAKEINTRESOURCE(resource));
		}

		int AddItem(LPCTSTR Caption, int iID, bool bChecked = false, bool bDefault = false, bool bEnabled = true)
		{
			if (iID == 0)
			{
				// Assign an ID!
				throw std::exception("No menu ID provided");
			}
			
			if (_tcslen(Caption) == 0)
			{
				::AppendMenu(GetHandle(), MF_SEPARATOR, 0, _T(""));
			}
			else 
			{
				int iFlags = MF_STRING | mfchecked[bChecked] | mfdefault[bDefault] | mfenabled[bEnabled];
				::AppendMenu(GetHandle(), iFlags, iID, Caption);
			}

			return iID;
		}

		void InsertItemAtPosition(LPCTSTR caption, int iID, int position, bool bChecked = false, bool bDefault = false, bool bEnabled = true)
		{
			if (iID == 0)
			{
				// Assign an ID!
				throw std::exception("No menu ID provided");
			}

			MENUITEMINFO mii = {sizeof(MENUITEMINFO), 0};
			
			if (_tcslen(caption) == 0)
			{
				mii.fMask = MIIM_FTYPE;
				mii.fType = MFT_SEPARATOR;
			}
			else 
			{
				mii.fMask = MIIM_FTYPE | MIIM_ID | MIIM_STATE | MIIM_STRING;
				mii.fType = MFT_STRING;
				mii.fState = 0 | 
					(bChecked ? MFS_CHECKED : 0) |
					(bDefault ? MFS_DEFAULT : 0) |
					(!bEnabled ? MFS_DISABLED : 0);
				mii.wID = iID;
				mii.dwTypeData = const_cast<TCHAR*>(caption);
			}

			::InsertMenuItem(GetHandle(), position, TRUE, &mii);
		}

		void InsertSubMenuAtPosition(LPCTSTR caption, int position, HMENU subMenu)
		{
			MENUITEMINFO mii = {sizeof(MENUITEMINFO), 0};
		
			mii.fMask = MIIM_FTYPE | MIIM_SUBMENU | MIIM_STRING;
			mii.fType = MFT_STRING;
			mii.hSubMenu = subMenu;
			mii.dwTypeData = const_cast<TCHAR*>(caption);

			::InsertMenuItem(GetHandle(), position, TRUE, &mii);
		}

		void AddSeparator()
		{
			AddItem(_T(""), -1);
		}

		void CheckMenuItem(int uIDCheckItem, bool bChecked)
		{
			::CheckMenuItem(GetHandle(), uIDCheckItem, mfchecked[bChecked]);
		}

		void EnableMenuItem(int uIDEnableItem, bool bEnabled)
		{
			::EnableMenuItem(GetHandle(), uIDEnableItem, mfenabled[bEnabled]);
		}

		int GetCount()
		{
			return ::GetMenuItemCount(GetHandle());
		}

		BOOL GetItemInfo(int item, LPMENUITEMINFO pItemInfo, BOOL byPosition = TRUE)
		{
			return ::GetMenuItemInfo(GetHandle(), item, byPosition, pItemInfo);
		}

		unsigned int GetItemID(int position) const
		{
			return ::GetMenuItemID(SafeGetHandle(), position);
		}

		virtual HMENU GetHandle()
		{
			if(m_hMenu == NULL)
			{
				m_hMenu = ::CreateMenu();
			}

			return m_hMenu;
		}

		HMENU SafeGetHandle() const
		{
			return m_hMenu;
		}

		void Attach(HMENU hMenu)
		{
			m_hMenu = hMenu;
		}

		HMENU Detach()
		{
			HMENU ret = m_hMenu;
			m_hMenu = NULL;
			return ret;
		}

		CSMenuHandle GetSubMenu(int nPos)
		{
			return CSMenuHandle( ::GetSubMenu(SafeGetHandle(), nPos) );
		}

		CSMenuHandle GetSubMenu(LPCTSTR name)
		{
			tstring sname(name);			
			tstring buffer;
			CMenuItemInfo mii;
			mii.fMask = MIIM_STRING;

			for (int i = 0; i < GetCount(); i++)
			{
				mii.dwTypeData = NULL;
				mii.cch = 0;

				::GetMenuItemInfo(SafeGetHandle(), i, MF_BYPOSITION, &mii);

				if (mii.cch != sname.length())
				{
					continue;
				}

				mii.cch++;
				buffer.resize(mii.cch);
				mii.dwTypeData = &buffer[0];

				::GetMenuItemInfo(SafeGetHandle(), i, MF_BYPOSITION, &mii);

				buffer.resize(mii.cch);

				if (buffer == sname)
				{
					return GetSubMenu(i);
				}
			}
			
			return CSMenuHandle(NULL);
		}

		void RemoveItemByPosition(int position)
		{
			::RemoveMenu(SafeGetHandle(), position, MF_BYPOSITION);	
		}

		void RemoveItemByCommand(UINT cmd)
		{
			::RemoveMenu(SafeGetHandle(), cmd, MF_BYCOMMAND);	
		}

		const CSMenuT& operator = (const CSMenuT& copy)
		{
			m_hMenu = copy.m_hMenu;
			return *this;
		}

		operator HMENU ()
		{
			return m_hMenu;
		}
	
	protected:
		HMENU m_hMenu;

		HINSTANCE GetAppInstance()
		{
			return _Module.GetModuleInstance();
		}
};

/**
 * @class CSPopupMenu
 * @brief Class to ease working with popup menus
 */
class CSPopupMenu : public CSMenu
{
	public:
		explicit CSPopupMenu();
		explicit CSPopupMenu(int resource, int index = 0);
		virtual ~CSPopupMenu();

		virtual HMENU GetHandle();

		int TrackPopupMenu(LPPOINT pt, HWND hWnd);

		operator HMENU ()
		{
			return GetHandle();
		}

	protected:
		HMENU m_hSubMenu;
};

#endif