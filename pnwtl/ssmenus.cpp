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

menu_id_range menu_id_range1 = {20000,21000,0};

// NULL is important here for counting the available IDs.
menu_id_range* CSMenuManager::s_IDs[] = {&menu_id_range1, NULL};

CSMenuManager* CSMenuManager::s_pTheInstance = NULL;

///////////////////////////////////////////////////////////////
// MenuCommandCache
///////////////////////////////////////////////////////////////

MenuCommandCache::MenuCommandCache(int initialStack)
{
	m_index = 0;
	m_stack.grow(initialStack);
}

void MenuCommandCache::push(int iCommand)
{
	if(m_index == m_stack.size())
		m_stack.grow((m_stack.size() + 1) * 2); // add one incase size is 0.

	m_stack[m_index] = iCommand;

	m_index++;
}

bool MenuCommandCache::canPop()
{
	return (m_index > 0);
}

int MenuCommandCache::pop()
{
	if(m_index > 0)
	{
		m_index--;
		return m_stack[m_index];
	}
	else
		return 0;
}

///////////////////////////////////////////////////////////////
// CSMenuManager
///////////////////////////////////////////////////////////////

CSMenuManager::CSMenuManager() : m_IDCache(10)
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
	for(MH_IT i = m_Handlers.begin(); i != m_Handlers.end(); ++i)
	{
		delete (*i).second;
	}
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

/**
 * @return int The actual ID of the registered command
 */
int CSMenuManager::RegisterCallback(CSMenuEventHandler *pHandler, int iCommand, LPVOID data)
{
	return RegisterCallback(-1, pHandler, iCommand, data);
}

/**
 * @return int The actual ID of the registered command
 * @param iRealCommand set to -1 to generate a command id.
 */
int CSMenuManager::RegisterCallback(int iRealCommand, CSMenuEventHandler* pHandler, int iMappedCommand, LPVOID data)
{
	if(iRealCommand == -1)
		iRealCommand = GetNextID();

	menu_event_handler* pRecord = new menu_event_handler;
	pRecord->iID = iMappedCommand;
	pRecord->pHandler = pHandler;
	pRecord->data = data;

	m_Handlers.insert(m_Handlers.begin(), MH_VT(iRealCommand, pRecord));

	return iRealCommand;
}

void CSMenuManager::UnRegisterCallback(int iID)
{
	MH_IT i = m_Handlers.find(iID);
	if(i != m_Handlers.end())
	{
		delete (*i).second;
		m_Handlers.erase(i);
		m_IDCache.push(iID);
	}
}

int CSMenuManager::GetNextID()
{
	if(m_IDCache.canPop())
		return m_IDCache.pop();

	if(m_pRange->current == 0)
	{
		m_pRange->current = m_pRange->start;
	}

	int ret = m_pRange->current;

	if(++m_pRange->current > m_pRange->end)
	{
		m_pRange++;
	}
	
	return ret;
}

/**
 * This method calls the predefined handler for a command.
 */
bool CSMenuManager::HandleCommand(int iID)
{
	bool bHandled = false;

	MH_CI i = m_Handlers.find(iID);
	if(i != m_Handlers.end())
	{
		menu_event_handler* pRecord = (*i).second;
		if(pRecord->pHandler)
		{
			pRecord->pHandler->SHandleMenuCommand(pRecord->iID, pRecord->data);
			bHandled = true;
		}
	}

	return bHandled;
}

/**
 * This method specifies the handler for the command in the call, 
 * and doesn't look it up from the handler map.
 */
bool CSMenuManager::LocalHandleCommand(int iID, int iCommand, CSMenuEventHandler* pHandler)
{
	bool bHandled = false;

	MH_CI i = m_Handlers.find(iID);
	if(i != m_Handlers.end())
	{
		menu_event_handler* pRecord = (*i).second;
		if(pRecord->iID == iCommand)
		{
			pHandler->SHandleMenuCommand(pRecord->iID, pRecord->data);

			bHandled = true;
		}
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
		return m_hMenu;}


int CSPopupMenu::TrackPopupMenu(LPPOINT pt, HWND hWnd)
{
	::TrackPopupMenu(GetHandle(), 0, pt->x, pt->y, 0, hWnd, NULL);
	
	return 0;
}