/**
 * @file ExplorerMenu.h
 * @brief File Browser View
 * @author Simon Steele
 * @note Copyright (c) 2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef ExplorerMenu_h__included
#define ExplorerMenu_h__included

#include "include/atlshellext.h"
#include "include/ShellCtrls.h"

class ShellContextMenu : public CExplorerMenu
{
public:

protected:
	virtual BOOL DoTrackPopupMenu(HMENU hMenu, UINT uFlags, int x, int y, int nReserved, HWND hWnd, CONST RECT *prcRect)
	{
		//return g_Context.m_frame->TrackPopupMenu(hMenu, uFlags, x, y, NULL, hWnd);
		return CExplorerMenu::DoTrackPopupMenu(hMenu, uFlags, x, y, NULL, hWnd, prcRect);
	}
};

#endif // #ifndef ExplorerMenu_h__included