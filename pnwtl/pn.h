/**
 * @file pn.h
 * @brief Main Header File for Programmers Notepad 2, defines the application object.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "pntypes.h"
#include "schemes.h"
#include "optionsmanager.h"

#define PN_NOTIFY (WM_USER+37)

class CPNAppState
{
	public:
		CPNAppState();
		~CPNAppState();

		CSchemeManager& GetSchemes(){return m_Schemes;}
		COptionsManager& GetOptionsManager(){return m_Options;}

		SFindOptions*		GetFindOptions(){return &m_FindOptions;}
		SReplaceOptions*	GetReplaceOptions(){return &m_ReplaceOptions;}
	protected:

		CSchemeManager	m_Schemes;
		COptionsManager	m_Options;

		SFindOptions	m_FindOptions;
		SReplaceOptions	m_ReplaceOptions;
};

HWND GetCurrentEditor(CWindow* pMDIFrameWnd);
