/**
 * @file styletabpages.h
 * @brief Options Styles Tab for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef styletabpages_h__included
#define styletabpages_h__included

#include "SchemeConfig.h"

class CTabPageKeywords : public CPropertyPageImpl<CTabPageKeywords>
{
	public:
		CTabPageKeywords();

		enum {IDD = IDD_TAB_KEYWORDS};

		BEGIN_MSG_MAP(CTabPageKeywords)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_HANDLER(IDC_KEYWORDS_RESETBUTTON, BN_CLICKED, OnResetClicked)
			COMMAND_HANDLER(IDC_KEYWORDS_SORTBUTTON, BN_CLICKED, OnSortClicked)
			//NOTIFY_HANDLER(IDC_KEYWORDS_LIST, LVN_KEYDOWN, OnListSelChanged)
			//NOTIFY_HANDLER(IDC_KEYWORDS_LIST, NM_CLICK, OnListSelChanged)

			NOTIFY_HANDLER(IDC_KEYWORDS_LIST, LVN_ITEMCHANGED, OnListSelChanged)

			REFLECT_NOTIFICATIONS()
			CHAIN_MSG_MAP(CPropertyPageImpl<CTabPageKeywords>)
		END_MSG_MAP()

	void SetScheme(SchemeDetails* pScheme);
	void Finalise();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnResetClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnListSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnSortClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		void SetItem();
		void UpdateSel();
		void EnableControls(BOOL bEnable = TRUE);
		void DoSetScheme();

	protected:
		CEdit				m_Text;
		CButton				m_ResetBtn;
		CButton				m_SortBtn;
		bool				m_bChanging;
		SchemeDetails*		m_pScheme;
		CustomKeywordSet*	m_pSet;
		CListViewCtrl		m_list;
		CScintillaDialogWnd	m_scintilla;
};

class CTabPageStyles : public CPropertyPageImpl<CTabPageStyles>
{
	public:	
		enum {IDD = IDD_TAB_STYLES};

		CTabPageStyles(SchemeConfigParser* pSchemes);

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
			NOTIFY_HANDLER(IDC_STYLE_FOREBUTTON, CPN_SELCHANGE, OnForeChanged)
			NOTIFY_HANDLER(IDC_STYLE_BACKBUTTON, CPN_SELCHANGE, OnBackChanged)
			NOTIFY_HANDLER(IDC_STYLES_TREE, TVN_SELCHANGED, OnTreeSelChanged)
			CHAIN_MSG_MAP(CPropertyPageImpl<CTabPageStyles>)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()

		void SetScheme(SchemeDetails* pScheme);
		void Finalise();
		void UpdateDisplay();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnForeChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnBackChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnFontChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnSizeChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnTreeSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnBoldClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnItalicClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnUnderlineClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnEOLFilledClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnResetClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnResetAllClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		
		void UpdateSel();
		void SetItem();
		void EnableButtons(bool bEnable);
		void UpdateGroup();
		//void UpdateGroupChildren(StyleDetails* pUpdatedClass, CustomStyleCollection* pColl = NULL);
		void UpdateStyle();
		void DisableNonColourItems();

	protected:
		StyleDetails		m_Style;
		SchemeDetails*		m_pScheme;
		SchemeConfigParser*	m_pSchemes;
		CTreeViewCtrl		m_tree;
		CStyleDisplay		m_sd;

		FullStyleDetails*	m_pStyle;

		CFontCombo			m_FontCombo;
		CNumberCombo		m_SizeCombo;

		CPNColorButton		m_fore;
		CPNColorButton		m_back;

		CButton				m_bold;
		CButton				m_italic;
		CButton				m_underline;
		CButton				m_eolfilled;

		HTREEITEM			m_lastTreeItem;

		bool				m_bGroup;
		bool				m_bChanging;
};

class CTabPageMisc : public CPropertyPageImpl<CTabPageMisc>, CWinDataExchange<CTabPageMisc>
{
	public:	
		enum {IDD = IDD_TAB_MISC};

		CTabPageMisc();

		BEGIN_MSG_MAP(CTabPageMisc)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			
			NOTIFY_HANDLER(IDC_STYLE_SELFOREBUTTON, CPN_SELCHANGE, OnValueChanged)
			NOTIFY_HANDLER(IDC_STYLE_SELBACKBUTTON, CPN_SELCHANGE, OnValueChanged)
			NOTIFY_HANDLER(IDC_STYLE_CURCOLBUTTON, CPN_SELCHANGE, OnValueChanged)
			NOTIFY_HANDLER(IDC_STYLE_IGCOLBUTTON, CPN_SELCHANGE, OnValueChanged)

			COMMAND_HANDLER(IDC_STYLE_SELUSEFORE, BN_CLICKED, OnSelUseForeClicked)

			COMMAND_HANDLER(IDC_TAB_NOORRADIO, BN_CLICKED, OnValueChanged)
			COMMAND_HANDLER(IDC_TAB_SPACESRADIO, BN_CLICKED, OnValueChanged)
			COMMAND_HANDLER(IDC_TAB_TABSRADIO, BN_CLICKED, OnValueChanged)

			CHAIN_MSG_MAP(CPropertyPageImpl<CTabPageMisc>)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()

		BEGIN_DDX_MAP(COptionsPageConf)
			DDX_RADIO(IDC_TAB_NOORRADIO, m_iTabOverride)
		END_DDX_MAP()

		bool IsDirty();

		void SetScheme(SchemeDetails* pScheme);
		void Finalise();
		void UpdateDisplay();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		
		LRESULT OnValueChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnValueChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnSelUseForeClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		void SetValues();
		void EnableButtons();

	protected:
		SchemeDetails*	m_pScheme;
		
		CPNColorButton	m_selFore;
		CPNColorButton	m_selBack;
		CPNColorButton	m_cursorCol;
		CPNColorButton	m_igCol;
		CButton			m_selUseExistingFore;

		bool			m_bChanging;
		bool			m_bDirty;
		int				m_iTabOverride;
};

#endif //#ifndef styletabpages_h__included