#ifndef scintillawtl_h__included
#define scintillawtl_h__included

#include "scintillaif.h"
#include "schemes.h"

/**
 * @class CScintillaWindow
 * @brief WTL Window implementation for Scintilla...
 */
template <class TBase = CScintilla>
class CScintillaWindow : public CWindowImpl< CScintillaWindow >, public TBase
{
public:

	DECLARE_WND_SUPERCLASS(NULL, "Scintilla")

	BOOL PreTranslateMessage(MSG* pMsg)
	{
		pMsg;
		return FALSE;
	}

	BEGIN_MSG_MAP(CScintillaWindow)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(OCM_NOTIFY, OnNotify)
	END_MSG_MAP()

	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
	{
		m_scihWnd = m_hWnd;

		bHandled = FALSE;

		return 0;
	}

	LRESULT OnNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
	{
		if(IsScintillaNotify(lParam))
		{
			HandleNotify(lParam);
			bHandled = TRUE;
		}
		else
			bHandled = FALSE;

		return 0;
	}

	HWND GetHwnd()
	{
		return m_hWnd;
	}
};


#endif