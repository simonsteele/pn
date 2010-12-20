/**
 * @file commandbaredit.h
 * @brief Command Bar Edit Control
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef COMMANDBAREDIT_H_INCLUDED
#define COMMANDBAREDIT_H_INCLUDED

/**
 * Edit control for the command bar
 */
class CCommandBarEdit : public CWindowImpl<CCommandBarEdit, CEdit, CControlWinTraits>
{
	typedef CWindowImpl<CCommandBarEdit, CEdit, CControlWinTraits> baseClass;

public:
	DECLARE_WND_SUPERCLASS(_T("PN_COMMANDBAREDIT"), _T("EDIT"));

	BEGIN_MSG_MAP(CCommandBarEdit)
		MESSAGE_HANDLER(WM_CHAR, OnKeyDown)
		MESSAGE_HANDLER(WM_SETFOCUS, OnSetFocus)
		MESSAGE_HANDLER(WM_KILLFOCUS, OnKillFocus)
		DEFAULT_REFLECTION_HANDLER()
	END_MSG_MAP()

private:
	LRESULT OnKeyDown(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		if (wParam == VK_RETURN && GetFocus() == m_hWnd)
		{
			::SendMessage(GetParent(), WM_COMMAND, MAKEWPARAM(GetWindowLong(GWL_ID), BN_CLICKED), NULL);
		}
		else
		{
			bHandled = FALSE;
		}

		return 0;
	}

	LRESULT OnSetFocus(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		bHandled = FALSE;
		::SendMessage(GetParent(), WM_COMMAND, MAKEWPARAM(GetWindowLong(GWL_ID), BN_SETFOCUS), NULL);
		return 0;
	}

	LRESULT OnKillFocus(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		bHandled = FALSE;
		::SendMessage(GetParent(), WM_COMMAND, MAKEWPARAM(GetWindowLong(GWL_ID), BN_KILLFOCUS), NULL);
		return 0;
	}
};

#endif // COMMANDBAREDIT_H_INCLUDED