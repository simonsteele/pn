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

#include "callback.h"

typedef struct
{
	int start;
	int end;
	int current;
} menu_id_range;

typedef map<int, CallbackBase*> MAP_HANDLERS;
typedef MAP_HANDLERS::value_type MH_VT;
typedef MAP_HANDLERS::const_iterator MH_CI;

/**
 * @class CMenuManager
 * @brief This class basically manages resource ID allocation.
 */
class CSMenuManager
{
	public:
		~CSMenuManager();

		static CSMenuManager* GetInstance();
		static void ReleaseInstance();

		void RegisterCallback(int iID, CallbackBase* pHandler);
		void UnRegisterCallback(int iID);

		bool HandleCommand(int iID);

	protected:
		CSMenuManager();

		// Static Members:
		static menu_id_range* s_IDs[];
		static CSMenuManager* s_pTheInstance;

		// Members:
		int m_iRanges;
		menu_id_range* m_pRange;

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
				::AppendMenu(GetHandle(), MF_SEPARATOR, 0, "");
			else 
			{
				int iFlags = MF_STRING | mfchecked[bChecked] | mfdefault[bDefault] | mfenabled[bEnabled];
				::AppendMenu(GetHandle(), iFlags, iID, Caption);
			}

			return iID;
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

	protected:
		HMENU m_hSubMenu;
};

#endif