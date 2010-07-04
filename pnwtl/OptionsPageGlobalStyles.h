/**
 * @file OptionsPageGlobalStyles.h
 * @brief Options Dialog Global Styles Page for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2007-2009 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef OptionsPageGlobalStyles_h__included
#define OptionsPageGlobalStyles_h__included

#include "include/optionsdialog.h"
#include "controls/OptionsBlockHeader.h"

class SchemeConfigParser;

class COptionsPageGlobalStyles : public COptionsPageImpl<COptionsPageGlobalStyles>
{
	public:
		enum {IDD = IDD_PAGE_STYLECLASSES};

		COptionsPageGlobalStyles(SchemeConfigParser* pSchemes);

		BEGIN_MSG_MAP(CTabPageStyles)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_HANDLER(IDC_STYLE_BOLDCHECK, BN_CLICKED, OnBoldClicked)
			COMMAND_HANDLER(IDC_STYLE_ITALICCHECK, BN_CLICKED, OnItalicClicked)
			COMMAND_HANDLER(IDC_STYLE_UNDERLINECHECK, BN_CLICKED, OnUnderlineClicked)
			COMMAND_HANDLER(IDC_STYLE_EOLFILLEDCHECK, BN_CLICKED, OnEOLFilledClicked)
			COMMAND_HANDLER(IDC_STYLE_FONTCOMBO, CBN_SELCHANGE, OnFontChanged)
			COMMAND_HANDLER(IDC_STYLE_SIZECOMBO, CBN_SELCHANGE, OnSizeChanged)
			COMMAND_HANDLER(IDC_STYLE_SIZECOMBO, CBN_EDITCHANGE, OnSizeChanged)
			COMMAND_HANDLER(IDC_STYLE_RESETBTN, BN_CLICKED, OnResetClicked)
			COMMAND_HANDLER(IDC_STYLE_RESETALLBTN, BN_CLICKED, OnResetAllClicked)
			COMMAND_HANDLER(IDC_STYLE_LOADPRESETBUTTON, BN_CLICKED, OnLoadPresetClicked)
			COMMAND_HANDLER(IDC_STYLE_SAVEPRESETBUTTON, BN_CLICKED, OnSavePresetClicked)
			NOTIFY_HANDLER(IDC_STYLE_FOREBUTTON, CPN_SELCHANGE, OnForeChanged)
			NOTIFY_HANDLER(IDC_STYLE_BACKBUTTON, CPN_SELCHANGE, OnBackChanged)
			NOTIFY_HANDLER(IDC_STYLES_LIST, LVN_ITEMCHANGED, OnListSelChanged)
			REFLECT_NOTIFICATIONS()
			
		END_MSG_MAP()

		virtual void OnOK();
		virtual void OnInitialise();
		virtual tstring GetTreePosition();
		virtual void OnCancel();

		bool IsDirty();

	private:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnForeChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnBackChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnFontChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnSizeChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnBoldClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnItalicClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnUnderlineClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnEOLFilledClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnResetClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnResetAllClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnListSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

		LRESULT OnLoadPresetClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnSavePresetClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		void OnPresetFound(const wchar_t* path, FileFinderData& file, bool& /*shouldContinue*/);

		void onChange();
		void storeChanges();
		void updateSel();
		void updateDisplay();
		void loadPreset(LPCTSTR path);
		void savePreset();

	private:
		CFontCombo			m_FontCombo;
		CNumberCombo		m_SizeCombo;

		CPNColorButton		m_fore;
		CPNColorButton		m_back;

		CButton				m_bold;
		CButton				m_italic;
		CButton				m_underline;
		CButton				m_eolfilled;

		CStyleDisplay		m_sd;

		CListViewCtrl		m_list;

		CComboBox			m_presets;

		StyleDetails		m_style;
		NamedStyleDetails*	m_pStyle;

		bool				m_dirty;

		SchemeConfigParser*	m_pSchemes;

		COptionsBlockHeader m_settingsHeader;
		COptionsBlockHeader m_presetsHeader;
};

#endif