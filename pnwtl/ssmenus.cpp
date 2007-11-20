/**
 * @file ssmenus.cpp
 * @brief Implementation of the menu functionality classes.
 * @author Simon Steele
 * @note copyright (c) 2002 Simon Steele - http://untidy.net/
 * 
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "ssmenus.h"

/////////////////////////////////////////////////////////////////////////////
// CSMenu
/////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////
// CSPopupMenu
/////////////////////////////////////////////////////////////////////////////

CSPopupMenu::CSPopupMenu() : 
	CSMenu(), 
	m_hSubMenu(NULL)
{
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