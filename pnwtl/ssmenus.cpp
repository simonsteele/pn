/**
 * @file ssmenus.cpp
 * @brief Implementation of the menu functionality classes.
 * @author Simon Steele
 * @note copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 * 
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "ssmenus.h"

menu_id_range menu_id_range1 = {1,2,0};
//menu_id_range menu_id_range2 = {6,8,0};

// NULL is important here for counting the available IDs.
menu_id_range* CSMenuManager::s_IDs[] = {&menu_id_range1, NULL};

CSMenuManager* CSMenuManager::s_pTheInstance = NULL;

CSMenuManager::CSMenuManager()
{
	int nRanges = 0;

	while(s_IDs[nRanges] != NULL)
	{
		nRanges++;
	}
	
	m_iRanges	= nRanges;
	m_pRange	= s_IDs[0];
}

CSMenuManager::~CSMenuManager()
{

}

CSMenuManager* CSMenuManager::GetInstance()
{
	if(!s_pTheInstance)
	{
		s_pTheInstance = new CSMenuManager;	
	}

	return s_pTheInstance;
}

void CSMenuManager::ReleaseInstance()
{
	if(s_pTheInstance)
	{
		delete s_pTheInstance;
		s_pTheInstance = NULL;
	}
}

void CSMenuManager::RegisterCallback(int iID, CallbackBase* pHandler)
{
	m_Handlers.insert(m_Handlers.begin(), MH_VT(iID, pHandler));
}

void CSMenuManager::UnRegisterCallback(int iID)
{
	m_Handlers.erase(iID);
}

bool CSMenuManager::HandleCommand(int iID)
{
	bool bHandled = false;

	MH_CI i = m_Handlers.find(iID);
	if(i != m_Handlers.end())
	{
		CallbackBase* pBase = (*i).second;
		(*pBase)();
		bHandled = true;
	}

	return bHandled;
}

/////////////////////////////////////////////////////////////////////////////
// CSMenu
/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// CSPopupMenu
/////////////////////////////////////////////////////////////////////////////

CSPopupMenu::CSPopupMenu() : CSMenu()
{
	m_hSubMenu = NULL;
}

CSPopupMenu::CSPopupMenu(int resource, int index) : CSMenu()
{
	m_hMenu = ::LoadMenu(GetAppInstance(), MAKEINTRESOURCE(resource));
	m_hSubMenu = ::GetSubMenu(m_hMenu, index);
}

CSPopupMenu::~CSPopupMenu()
{

}

HMENU CSPopupMenu::GetHandle()
{
	if(m_hMenu == NULL)
	{
		m_hMenu = ::CreatePopupMenu();
	}

	if(m_hSubMenu)
		return m_hSubMenu;
	else
		return m_hMenu;
}


int CSPopupMenu::TrackPopupMenu(LPPOINT pt, HWND hWnd)
{
	//theApp.GetContextMenuManager()->ShowPopupMenu(GetHandle(), pt->x, pt->y, pWnd/*, BOOL bOwnMessage=FALSE,BOOL bAutoDestroy=TRUE*/);
	::TrackPopupMenu(SafeGetHandle(), 0, pt->x, pt->y, 0, hWnd, NULL);
	return 0;
}
