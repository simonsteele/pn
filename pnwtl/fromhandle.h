/**
 * @file fromhandle.h
 * @brief ::FromHandle mixin implementation for WTL
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele - http://untidy.net/
 *
 * Simply inherit from CFromHandle< CMyWndClass > and add 
 * IMPLEMENT_FROMHANDLE() to your message map. To get a pointer to 
 * the class instance, you now simply need to call:
 * CMyCWndClass::FromHandle().
 *
 * This uses SendMessage to retrieve a pointer back, so no nasty
 * handle maps.
 */
/* 
 * Boilerplate:
 * This code may be used in compiled form in any way you desire. This
 * file may be redistributed by any means PROVIDING it is 
 * not sold for profit without the authors written consent, and 
 * providing that this notice and the authors name is included.
 *
 * This file is provided "as is" with no expressed or implied warranty.
 * The author accepts no liability if it causes any damage to you or your
 * computer whatsoever.
 * 
 * If you find bugs, have suggestions for improvements, etc.,
 * please contact the author.
 */

#ifndef fromhandle_h__included
#define fromhandle_h__included

#pragma once

#define UWM_WNDGETCLASSPTR_MSG _T("UWM_WNDGETCLASSPTRB871792E-E058-428c-B4B3-A3243900C82A")
static UINT UWM_WNDGETCLASSPTR = ::RegisterWindowMessage(UWM_WNDGETCLASSPTR_MSG);

#define IMPLEMENT_FROMHANDLE() \
	MESSAGE_HANDLER(UWM_WNDGETCLASSPTR, OnGetClassInstance)

template <class T>
class CFromHandle
{
public:

	CFromHandle()
	{
		if(UWM_WNDGETCLASSPTR == 0)
			UWM_WNDGETCLASSPTR = ::RegisterWindowMessage(UWM_WNDGETCLASSPTR_MSG);
	}

	static T* FromHandle(HWND hWnd)
	{
		return hWnd != NULL ? (T*)::SendMessage(hWnd, UWM_WNDGETCLASSPTR, 0, 0) : NULL;
	}

protected:
	LRESULT OnGetClassInstance(WORD /*wNotifyCode*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		return (long)((T*)this);
	}
};

#endif //#ifndef fromhandle_h__included