/**
 * @file textclipeditor.h
 * @brief Text Clip Editing Dialog for Programmer's Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2007-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef textclipeditor_h__included
#define textclipeditor_h__included

#include "optionscontrols.h"

#pragma once

/**
 * Editor window for text clips
 */
class CTextClipEditor : public CDialogImpl<CTextClipEditor>,
						public CDialogResize<CTextClipEditor>
{
public:
	CTextClipEditor(std::string shortcut, std::string text, tstring hint) : m_shortcut(shortcut), m_text(text), m_hint(hint)
	{
	}

	enum { IDD = IDD_TEXTCLIPEDITOR };

	BEGIN_MSG_MAP(CTextClipEditor)
		MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
		COMMAND_ID_HANDLER(IDOK, OnCloseCmd)
		COMMAND_ID_HANDLER(IDCANCEL, OnCloseCmd)
		CHAIN_MSG_MAP( CDialogResize<CTextClipEditor> )
	END_MSG_MAP()

	enum { IDC_EDITOR = 102 };

	BEGIN_DLGRESIZE_MAP(CTextClipEditor)
		DLGRESIZE_CONTROL(IDC_EDITOR, DLSZ_SIZE_Y | DLSZ_SIZE_X)
		DLGRESIZE_CONTROL(IDOK, DLSZ_MOVE_X | DLSZ_MOVE_Y)
		DLGRESIZE_CONTROL(IDCANCEL, DLSZ_MOVE_X | DLSZ_MOVE_Y)
    END_DLGRESIZE_MAP()

	LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		CRect rcScintilla;
		::GetWindowRect(GetDlgItem(IDC_PLACEHOLDER), rcScintilla);
		ScreenToClient(rcScintilla);
		m_scintilla.SetWantAll(true);
		m_scintilla.Create(m_hWnd, rcScintilla, _T("EditClipText"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_TABSTOP, WS_EX_STATICEDGE, IDC_EDITOR);
		::SetWindowPos(m_scintilla, GetDlgItem(IDC_PLACEHOLDER), 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
		m_scintilla.SetWrapMode(SC_WRAP_WORD);
		m_scintilla.AssignCmdKey(SCK_HOME, SCI_HOMEDISPLAY);
		m_scintilla.AssignCmdKey(SCK_END, SCI_LINEENDDISPLAY);
		m_scintilla.SetMarginWidthN(1, 0);
		m_scintilla.SetEOLMode(SC_EOL_LF);
		
		// Stop scintilla from capturing the escape and tab keys...
		m_scintilla.ClearCmdKey(SCK_ESCAPE);
		//m_scintilla.ClearCmdKey(SCK_TAB);
		
		SchemeManager::GetInstance()->GetDefaultScheme()->Load(m_scintilla);

		m_scintilla.SetText(m_text.c_str());
	
		CA2CT scconv(m_shortcut.c_str());

		GetDlgItem(IDC_SHORTCUT_EDIT).SetWindowText(scconv);
		GetDlgItem(IDC_HINT_EDIT).SetWindowText(m_hint.c_str());

		CenterWindow(GetParent());

		DlgResize_Init();

		return 0;
	}

	LRESULT OnCloseCmd(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
	{
		if (wID == IDOK)
		{
			CWindowText wt(GetDlgItem(IDC_SHORTCUT_EDIT).m_hWnd);
			m_shortcut = wt.GetA();

			CWindowText wt2(GetDlgItem(IDC_HINT_EDIT).m_hWnd);
			if ((LPCTSTR)wt2)
			{
				m_hint = wt2;
			}

			int len = m_scintilla.GetTextLength();
			char* buffer = new char[len+1];
			m_scintilla.GetText(len+1, buffer);
			buffer[len] = 0;
			m_text = buffer;
			delete [] buffer;
		}

		EndDialog(wID);
		return 0;
	}

	const char* GetShortcut() const
	{
		return m_shortcut.c_str();
	}

	const char* GetText() const
	{
		return m_text.c_str();
	}

	LPCTSTR GetHint() const
	{
		return m_hint.c_str();
	}

private:
	CScintillaDialogWnd m_scintilla;
	std::string m_shortcut;
	std::string m_text;
	tstring m_hint;
};

#endif // #ifndef textclipeditor_h__included