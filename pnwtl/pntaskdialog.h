/**
 * @file pn.h
 * @brief Main Header File for Programmers Notepad 2, defines the application level services.
 * @author Simon Steele
 * @note Copyright (c) 2008-2011 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * Thanks to the author of kPad for the g_Context here!
 */

#ifndef pntaskdialog_h__included
#define pntaskdialog_h__included

#include "include/TaskDialog.h"

/**
 * Wrap TaskDialogIndirect to use the CommCtrl one on Vista+ and the 
 * Viksoe one below
 */
inline int PNTaskDialogIndirect(TASKDIALOGCONFIG* pTask, int* pnRadioButton = NULL, 
								BOOL* pfVerificationFlagChecked = NULL)
{
	int nButton;
	if (WTL::RunTimeHelper::IsVista())
	{
		DWORD hRet = AtlTaskDialogIndirect(pTask, &nButton, pnRadioButton, pfVerificationFlagChecked);
		ATLVERIFY(SUCCEEDED(hRet));
	}
	else
	{
		DWORD hRet = Task98DialogIndirect(pTask, &nButton, pnRadioButton, pfVerificationFlagChecked);
		ATLVERIFY(SUCCEEDED(hRet));
	}
	
	return nButton;
}

/**
 * Wrap TaskDialog to use the CommCtrl one on Vista+ and the Viksoe one below
 */
inline int PNTaskDialog(HWND hWndParent, ATL::_U_STRINGorID WindowTitle, ATL::_U_STRINGorID MainInstructionText, 
						ATL::_U_STRINGorID ContentText, TASKDIALOG_COMMON_BUTTON_FLAGS dwCommonButtons = 0U, 
						ATL::_U_STRINGorID Icon = (LPCTSTR)NULL)
{
	if (WTL::RunTimeHelper::IsVista())
	{
		return AtlTaskDialog(hWndParent, WindowTitle, MainInstructionText, ContentText, dwCommonButtons, Icon);
	}
	else
	{
		USES_CONVERSION;
		int nRet;
		DWORD hRet = Task98Dialog(hWndParent, _Module.GetResourceInstance(), 
			WindowTitle.m_lpstr,
            MainInstructionText.m_lpstr,
            ContentText.m_lpstr,
			dwCommonButtons,
			Icon.m_lpstr,
            &nRet);
		ATLVERIFY(SUCCEEDED(hRet));
		return nRet;
	}
}

/**
 * Extend PNTaskDialog signature with options for default button and flags
 */
inline int PNTaskDialogEx(HWND hWndParent, ATL::_U_STRINGorID WindowTitle, ATL::_U_STRINGorID MainInstructionText,
						ATL::_U_STRINGorID ContentText, TASKDIALOG_COMMON_BUTTON_FLAGS dwCommonButtons = 0U,
						ATL::_U_STRINGorID Icon = (LPCTSTR)NULL,
						DWORD defaultButton = 0,
						DWORD flags = 0)
{
	TASKDIALOGCONFIG tdc = {sizeof(TASKDIALOGCONFIG), 0};
	tdc.hInstance = _Module.GetResourceInstance();
	tdc.hwndParent = hWndParent;
	tdc.pszWindowTitle = WindowTitle.m_lpstr;
	tdc.pszMainInstruction = MainInstructionText.m_lpstr;
	tdc.pszContent = ContentText.m_lpstr;
	tdc.dwCommonButtons = dwCommonButtons;
	tdc.dwFlags = flags;
	tdc.nDefaultButton = defaultButton;
		
	if (Icon.m_lpstr)
	{
		tdc.pszMainIcon = MAKEINTRESOURCEW(Icon.m_lpstr);
	}

	return PNTaskDialogIndirect(&tdc);
}

#endif