#ifndef pnutils_h__included
#define pnutils_h__included

/**
 * @class MRUManager
 * Some inspiration taken from CRecentDocumentList <atlmisc.h>
 *
 * @parm t_MaxSize	- Maximum number of MRU entries.
 * @parm t_Base		- Base resource ID for the menu (i.e. Range must be t_Base -> t_Base + (t_MaxSize - 1)
 */
class MRUManager
{
	struct _entry
	{
		TCHAR* pszFilename;

		bool operator==(const _entry& e) const
		{ return (lstrcmpi(pszFilename, e.pszFilename) == 0); }

		_entry& operator = (const _entry& e)
		{
			if(pszFilename)
				delete [] pszFilename;
			pszFilename = new TCHAR[_tcslen(e.pszFilename)+1];
			_tcscpy(pszFilename, e.pszFilename);
			return *this;
		}
	};

	public:
		MRUManager(UINT baseCmd, int size = 10);
		~MRUManager();

		void SetSize(int size);

		void UpdateMenu(HMENU hMenu);

		void AddFile(LPCTSTR filename);

	protected:		
		void BuildMenu(HMENU hMenu, int iCommand);
		void Resize();

		CSimpleArray<_entry>	m_entries;
		int						m_iMaxSize;
		UINT					m_iBase;
		TCHAR*					m_szEmpty;
};


#endif //#ifndef pnutils_h__included