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


#endif //#ifndef pnutils_h__included