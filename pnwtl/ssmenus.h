/**
 * @file ssmenus.h
 * @brief Interface for the menu functionality classes.
 * @author Simon Steele
 * @note copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 * 
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

/**
 * @todo Make CSMenuManager a proper static class,
 *
 * @todo get rid of static class minders - use some kind of
 * exit function ???
 *
 * @todo Implement some kind of list of registered command
 * handlers.
 *
 * @todo Command ID Stack? Examine TBits from Delphi more closely.
 */

#ifndef ssmenus_h__included
#define ssmenus_h__included

const long mfchecked[] = {MF_UNCHECKED, MF_CHECKED};
const long mfenabled[] = {MF_DISABLED | MF_GRAYED, MF_ENABLED};
const long mfdefault[] = {0, MFS_DEFAULT};

#include <map>
using namespace std;

//#include "callback.h"
#include "include/sscontainers.h"

typedef struct
{
	int start;
	int end;
	int current;
} menu_id_range;

/**
 * @class CSMenuEventHandler
 * @brief Mixin class for implementing custom menu generation and handling...
 */
class CSMenuEventHandler
{
	public:
		virtual void SHandleMenuCommand(int iCommand, LPVOID data) = 0;
};

typedef struct
{
	CSMenuEventHandler*	pHandler;
	int					iID;
	
	LPVOID				data;
} menu_event_handler;

typedef map<int, menu_event_handler*> MAP_HANDLERS;
typedef MAP_HANDLERS::value_type MH_VT;
typedef MAP_HANDLERS::iterator MH_IT;
typedef MAP_HANDLERS::const_iterator MH_CI;

/**
 * @brief Stack for command IDs.
 */
class MenuCommandCache
{
	public:
		MenuCommandCache(int initialStack);

	public:
		void push(int iCommand);
		bool canPop();
		int pop();

	protected:
		int				m_index;
		GArray<DWORD>	m_stack;
};

/**
 * @class CSMenuManager
 * @brief This class basically manages resource ID allocation.
 */
class CSMenuManager
{
	public:
		~CSMenuManager();

		static CSMenuManager* GetInstance();
		static void ReleaseInstance();

		int RegisterCallback(CSMenuEventHandler* pHandler, int iCommand, LPVOID data = NULL);
		int RegisterCallback(int iRealCommand, CSMenuEventHandler* pHandler, int iMappedCommand, LPVOID data = NULL);
		void UnRegisterCallback(int iID);

		int GetNextID();
		void ReturnID(int id);

		bool HandleCommand(int iID);
		bool LocalHandleCommand(int iID, int iCommand, CSMenuEventHandler* pHandler);

	protected:
		CSMenuManager();

		// Static Members:
		static menu_id_range* s_IDs[];
		static CSMenuManager* s_pTheInstance;

		// Members:
		int m_iRanges;
		menu_id_range* m_pRange;

		MenuCommandCache	m_IDCache;
		MAP_HANDLERS m_Handlers;
};

template <bool t_bManaged> class CSMenuT;
typedef CSMenuT<true> CSMenu;
typedef CSMenuT<false> CSMenuHandle;

/** 
 * @class CSMenu
 * @brief Class to ease working with menus
 */
template <bool t_bManaged>
class CSMenuT
{
	public:
		CSMenuT()
		{
			m_hMenu = NULL;
		}

		CSMenuT(HMENU hMenu)
		{
			m_hMenu = hMenu;
		}

		virtual ~CSMenuT()
		{
			if(t_bManaged)
				Destroy();
		}

		void Destroy()
		{
			if(m_hMenu)
			{
				::DestroyMenu(m_hMenu);
				m_hMenu = NULL;
			}
		}

		void LoadMenu(int resource)
		{
			::LoadMenu(GetAppInstance(), MAKEINTRESOURCE(resource));
		}

		int AddItem(LPCTSTR Caption, int iID = 0, bool bChecked = false, bool bDefault = false, bool bEnabled = true)
		{
			if(iID == 0)
			{
				// Assign an ID!
			}
			
			if (_tcslen(Caption) == 0)
				::AppendMenu(GetHandle(), MF_SEPARATOR, 0, _T(""));
			else 
			{
				int iFlags = MF_STRING | mfchecked[bChecked] | mfdefault[bDefault] | mfenabled[bEnabled];
				::AppendMenu(GetHandle(), iFlags, iID, Caption);
			}

			return iID;
		}

		void AddSeparator()
		{
			AddItem(_T(""));
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

		virtual HMENU GetHandle()
		{
			if(m_hMenu == NULL)
			{
				m_hMenu = ::CreateMenu();
			}

			return m_hMenu;
		}

		HMENU SafeGetHandle()
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

		const CSMenuT& operator = (const CSMenuT& copy)
		{
			m_hMenu = copy.m_hMenu;
			return *this;
		}

		operator HMENU ()
		{
			return m_hMenu;
		}

		static const TCHAR szPlus[];

		static tstring GetShortcutText(int wCode, int wModifiers)
		{
			tstring strKeyName;

			if (wCode != 0 || wModifiers != 0)
			{
				if (wModifiers & HOTKEYF_CONTROL)
				{
					strKeyName += GetKeyName(VK_CONTROL, false);
					strKeyName += _T("+");
				}

				if (wModifiers & HOTKEYF_SHIFT)
				{
					strKeyName += GetKeyName(VK_SHIFT, false);
					strKeyName += _T("+");
				}

				if (wModifiers & HOTKEYF_ALT)
				{
					strKeyName += GetKeyName(VK_MENU, false);
					strKeyName += _T("+");
				}

				strKeyName += GetKeyName(wCode, (wModifiers & HOTKEYF_EXT) != 0);
			}

			return strKeyName;
		}

		static tstring GetKeyName(UINT vk, bool extended)
		{
			LONG lScan = MapVirtualKey(vk, 0) << 16;

			// if it's an extended key, add the extended flag
			if (extended)
				lScan |= 0x01000000L;

			tstring str;
			GArray<TCHAR> tcbuf;
			
			int nBufferLen = 64;
			int nLen;
			do
			{
				nBufferLen *= 2;
				tcbuf.grow(nBufferLen);
				nLen = ::GetKeyNameText(lScan, &tcbuf[0], nBufferLen + 1);
			}
			while (nLen == nBufferLen);
			return tstring(&tcbuf[0]);
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
		CSPopupMenu();
		CSPopupMenu(int resource, int index = 0);
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

#define ROUTE_MENUCOMMANDS() \
	if(uMsg == WM_COMMAND) \
	{ \
		bHandled = TRUE; \
		if( CSMenuManager::GetInstance()->HandleCommand(LOWORD(wParam)) ) \
			return TRUE; \
		else \
			bHandled = FALSE; \
	}

#define LOCAL_MENUCOMMAND(id) \
	if(uMsg == WM_COMMAND) \
	{ \
		bHandled = TRUE; \
		if( CSMenuManager::GetInstance()->LocalHandleCommand(LOWORD(wParam), id, this) ) \
			return TRUE; \
		else \
			bHandled = FALSE; \
	}

#define BEGIN_MENU_HANDLER_MAP() \
	void SHandleMenuCommand(int iCommand, LPVOID data) \
	{


#define HANDLE_MENU_COMMAND(id, handler) \
	if(id == iCommand) \
	{ \
		handler(data); \
		return; \
	}

#define END_MENU_HANDLER_MAP() \
	}

#endif