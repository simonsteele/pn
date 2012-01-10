#ifndef PN_SCHEMEMENU_H_
#define PN_SCHEMEMENU_H_

typedef struct 
{
	Scheme* pScheme;
	int iCommand;
} menuid_scheme_pair;

typedef std::list<menuid_scheme_pair> MISCHEMELIST;

class CSchemeSwitcher
{
	public:
		CSchemeSwitcher();
		~CSchemeSwitcher();

		void Reset(CommandDispatch* pDispatch, int iCommand = SCHEMEMANAGER_SELECTSCHEME);

		void SetActiveScheme(Scheme* pCurrent);

		operator HMENU ();

	protected:
		void BuildMenu(int iCommand, CommandDispatch* dispatch);

		MISCHEMELIST	m_list;
		CSPopupMenu		m_menu;
};

#endif
