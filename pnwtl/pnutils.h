#ifndef pnutils_h__included
#define pnutils_h__included

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

		void SetSize(int size);

		void AddEntry(LPCTSTR data);

	protected:		
		void Resize();

		CSimpleArray<_entry>	m_entries;
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