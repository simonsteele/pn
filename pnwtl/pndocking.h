/**
 * @file pndocking.h
 * @brief Docking Windows implementation for Programmers Notepad 2.
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef pndocking_h__included
#define pndocking_h__included

#include "pntabs.h"

#include <DockMisc.h>
#include <DockingFrame.h>
#include <DockingBox.h>
#include <TabDockingBox.h>

/**
 * @brief This is the base class for all docking windows in Programmers Notepad 2.
 */
template <class T>
class ATL_NO_VTABLE CPNDockingWindow : public dockwins::CBoxedDockingWindowImpl<T,
                CWindow, dockwins::COutlookLikeExBoxedDockingWindowTraits >
{
};

/**
 * @brief This class combines the Tabbed MDI framework with the docking windows framework
 */
template <class T, 
		  class TBase = CMDIWindow, 		  
		  class TWinTraits = dockwins::CDockingFrameTraits >
class ATL_NO_VTABLE CPNDockingTabbedMDIFrameWindow : 
	public dockwins::CDockingFrameImplBase< T, CTabbedMDIFrameWindowImpl< T , CPNMDIClient, TBase, TWinTraits> ,TWinTraits >
{
public:
	DECLARE_WND_CLASS(_T("CPNDockingTabbedMDIFrameWindow"))
};


#endif //#ifndef pndocking_h__included