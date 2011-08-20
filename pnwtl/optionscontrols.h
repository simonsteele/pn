/**
 * @file optionscontrols.h
 * @brief Controls for options dialogs (and the like).
 * @author Simon Steele
 * @note Copyright (c) 2002-2005 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef optionscontrols_h__included
#define optionscontrols_h__included

#include "include/fontcombo.h"
#include "include/ColorButton.h"
#include "outputscintilla.h"
#include "ScintillaWTL.h"

class SchemeDetails;
class SchemeConfigParser;

namespace PCRE {
	class RegExp;
}

/**
 * @brief Static control to display text styles.
 */
class CStyleDisplay : public CWindowImpl<CStyleDisplay>
{
	public:
		CStyleDisplay();
		~CStyleDisplay();

		BEGIN_MSG_MAP(CStyleDisplay)
			MESSAGE_HANDLER(WM_PAINT, OnPaint)
			MESSAGE_HANDLER(WM_ERASEBKGND, OnEraseBkgnd)
		END_MSG_MAP()

		void SetBold(bool bold);
		void SetItalic(bool italic);
		void SetUnderline(bool underline);
		void SetFontName(LPCTSTR fontname);
		void SetSize(int size, bool bInvalidate = true);
		void SetFore(COLORREF fore);
		void SetBack(COLORREF back);
		void SetStyle(LPCTSTR fontname, int fontsize, COLORREF fore, COLORREF back, LPCTSTR name, bool bold, bool italic, bool underline);
		
	protected:
		CString		m_Name;
		LOGFONT		m_lf;
		CFont*		m_Font;
		COLORREF	m_Fore;
		COLORREF	m_Back;

		LRESULT OnPaint(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnEraseBkgnd(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

		void UpdateFont();
};

/**
 * A Scintilla window to go in a dialog
 * - should probably move this into the scintilla wrappers. It basically
 * ensures that dialog keys can move around properly.
 */
class CScintillaDialogWnd : public CScintillaWindowImpl< CScintillaDialogWnd, CScintilla>
{
public:
	CScintillaDialogWnd() : m_wantall(false){}

	typedef CScintillaWindowImpl< CScintillaDialogWnd, CScintilla> baseClass;
	BEGIN_MSG_MAP(CScintillaDialogWnd)
		MESSAGE_HANDLER(WM_GETDLGCODE, OnGetDlgCode)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	void SetWantAll(bool value)
	{
		m_wantall = value;
	}

	LRESULT OnGetDlgCode(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		if (m_wantall)
		{
			return DLGC_WANTARROWS | DLGC_WANTCHARS | DLGC_WANTALLKEYS | DLGC_WANTTAB;
		}
		else
		{
			return DLGC_HASSETSEL | DLGC_WANTARROWS | DLGC_WANTCHARS;
		}
	}

private:
	bool m_wantall;
};

class CScintillaREDialogWnd : public CScintillaWindowImpl< CScintillaREDialogWnd, REScintilla >
{
	typedef CScintillaWindowImpl< CScintillaREDialogWnd, REScintilla > baseClass;

public:
	BEGIN_MSG_MAP(CScintillaREDialogWnd)
		MESSAGE_HANDLER(WM_GETDLGCODE, OnGetDlgCode)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	virtual int HandleNotify(LPARAM lParam);

protected:
	LRESULT OnGetDlgCode(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		return DLGC_HASSETSEL | DLGC_WANTARROWS | DLGC_WANTCHARS;
	}
};

/**
 * @brief Provide better default colour functionality in CColorButton.
 */
class CPNColorButton : public CColorButton
{
	public:
		COLORREF SafeGetColor (void) const
		{
			if(m_clrCurrent != CLR_DEFAULT)
				return m_clrCurrent;
			else
				return GetDefaultColor();
		}
};

/**
 * @brief Wrap scheme-choosing combo-box functionality.
 */
class CSchemeCombo : public CComboBox
{
	public:
		/// Add a scheme to the combo box
		int AddScheme(LPCTSTR title, SchemeDetails* pScheme);
		
		/// Load schemes from a SchemeConfigParser instance into the combo box.
		void Load(SchemeConfigParser* pConfig, LPCSTR selectScheme = NULL, bool bIncludePlainText = true, bool bIncludeInternal = false);
		
		/// Return a SchemeConfig* for the item 'index'.
		SchemeDetails* GetItemScheme(int index);
};

/**
 * @brief Override the hotkey control to support the Delete key
 */
class CPNHotkeyCtrl : public CWindowImpl<CPNHotkeyCtrl, CHotKeyCtrl>
{
public:
	BEGIN_MSG_MAP(CPNHotkeyCtrl)
		MESSAGE_HANDLER(WM_KEYUP, OnKeyUp)
		MESSAGE_HANDLER(WM_KEYDOWN, OnKeyDown)
		MESSAGE_HANDLER(WM_CHAR, OnChar)
		MESSAGE_HANDLER(WM_GETDLGCODE, OnGetDlgCode)
	END_MSG_MAP()

private:
	LRESULT OnKeyUp(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnKeyDown(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnChar(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnGetDlgCode(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
};

#endif