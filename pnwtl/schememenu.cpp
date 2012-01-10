#include "stdafx.h"
#include "schemes.h"
#include "schememenu.h"

///////////////////////////////////////////////////////////
// CSchemeSwitcher
///////////////////////////////////////////////////////////

CSchemeSwitcher::CSchemeSwitcher()
{
	
}

CSchemeSwitcher::~CSchemeSwitcher()
{

}

void CSchemeSwitcher::BuildMenu(int iCommand, CommandDispatch* dispatch)
{
	menuid_scheme_pair x;
	SchemeManager& sm = SchemeManager::GetInstanceRef();
	SCHEME_LIST* pSchemes = sm.GetSchemesList();
	
	m_menu.AddItem( sm.GetDefaultScheme()->GetTitle(), dispatch->RegisterCallback(NULL, iCommand, (LPVOID)sm.GetDefaultScheme()) );

	for(SCIT i = pSchemes->begin(); i != pSchemes->end(); ++i)
	{
		if( !(*i).IsInternal() )
		{
			x.pScheme = &(*i);
			x.iCommand = dispatch->RegisterCallback(NULL, iCommand, (LPVOID)x.pScheme);

			m_menu.AddItem( x.pScheme->GetTitle(), x.iCommand );
			m_list.insert(m_list.end(), x);
		}
	}
}

void CSchemeSwitcher::Reset(CommandDispatch* pDispatch, int iCommand)
{
	m_list.clear();
	BuildMenu(iCommand, pDispatch);
}

void CSchemeSwitcher::SetActiveScheme(Scheme* pCurrent)
{
	for(MISCHEMELIST::iterator i = m_list.begin(); i != m_list.end(); ++i)
	{
		if((*i).pScheme == pCurrent)
			::CheckMenuItem((HMENU)m_menu, (*i).iCommand, MF_BYCOMMAND | MF_CHECKED);
		else
			::CheckMenuItem((HMENU)m_menu, (*i).iCommand, MF_BYCOMMAND | MF_UNCHECKED);
	}
}

CSchemeSwitcher::operator HMENU ()
{
	return (HMENU)m_menu;
}
