/**
 * @file optionspages.h
 * @brief Options Dialog Pages (1) for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef optionspages_h__included
#define optionspages_h__included

#include "include/optionsdialog.h"
#include "include/sslistctrl.h"
#include "optionscontrols.h"
#include "SchemeConfig.h"
#include "tools.h"

class COptionsPageGeneral : public COptionsPageImpl<COptionsPageGeneral>,
							public CWinDataExchange<COptionsPageGeneral>
{
	public:
		BEGIN_MSG_MAP(COptionsPageGeneral)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()
		enum { IDD = IDD_PAGE_GENERAL };

		BEGIN_DDX_MAP(COptionsPageGeneral)
			DDX_CHECK(IDC_OPT_INDENTGUIDESCHECK, m_bIndentGuides)
			DDX_CHECK(IDC_OPT_USETABSCHECK, m_bUseTabs)
			DDX_CHECK(IDC_OPT_LINENOSCHECK, m_bLineNos)
			DDX_UINT(IDC_OPT_TABWIDTHEDIT, m_iTabWidth)
			DDX_UINT(IDC_OPT_MRUCOUNT, m_iMRUSize)
			DDX_CHECK(IDC_OPT_MAXCHECK, m_bMaximise)
			DDX_CHECK(IDC_OPT_FULLPATHCHECK, m_bFullPath)
		END_DDX_MAP()

		virtual void OnOK();
		virtual void OnInitialise();
		virtual LPCTSTR GetTreePosition();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		
		BOOL m_bUseTabs;
		BOOL m_bIndentGuides;
		BOOL m_bLineNos;
		BOOL m_bMaximise;
		BOOL m_bFullPath;
		UINT m_iTabWidth;
		UINT m_iMRUSize;
		EPNSaveFormat m_SaveFormat;
};

class COptionsPageStyle : public COptionsPageImpl<COptionsPageStyle>
{
	public:
		COptionsPageStyle(SchemeConfigParser* pSchemes) : m_pSchemes(pSchemes), m_bDirty(false) {}

		BEGIN_MSG_MAP(COptionsPageStyle)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()
		enum { IDD = IDD_PAGE_STYLE };

		virtual void OnOK();
		virtual void OnCancel();
		virtual void OnInitialise();
		virtual LPCTSTR GetTreePosition();

		bool IsDirty();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		StyleDetails* GetDefault(bool& bIsCustom);
	
	protected:
		CFontCombo		m_FontCombo;
		CNumberCombo	m_SizeCombo;

		CPNColorButton	m_fore;
		CPNColorButton	m_back;
		
		CButton			m_bold;
		CButton			m_italic;
		CButton			m_underline;

		bool			m_bDirty;

		SchemeConfigParser* m_pSchemes;
};

class CTabPageKeywords : public CPropertyPageImpl<CTabPageKeywords>
{
	public:
		CTabPageKeywords();

		enum {IDD = IDD_TAB_KEYWORDS};

		BEGIN_MSG_MAP(CTabPageKeywords)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_HANDLER(IDC_KEYWORDS_RESETBUTTON, BN_CLICKED, OnResetClicked)
			COMMAND_HANDLER(IDC_KEYWORDS_SORTBUTTON, BN_CLICKED, OnSortClicked)
			NOTIFY_HANDLER(IDC_KEYWORDS_LIST, LVN_KEYDOWN, OnListSelChanged)
			NOTIFY_HANDLER(IDC_KEYWORDS_LIST, NM_CLICK, OnListSelChanged)
			REFLECT_NOTIFICATIONS()
			CHAIN_MSG_MAP(CPropertyPageImpl<CTabPageKeywords>)
		END_MSG_MAP()

	void SetScheme(SchemeConfig* pScheme);
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
		SchemeConfig*		m_pScheme;
		CustomKeywordSet*	m_pSet;
		CListViewCtrl		m_list;
		CScintillaDialogWnd	m_scintilla;
};

class CTabPageStyles : public CPropertyPageImpl<CTabPageStyles>
{
	public:	
		enum {IDD = IDD_TAB_STYLES};

		CTabPageStyles();

		BEGIN_MSG_MAP(CTabPageStyles)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_HANDLER(IDC_STYLE_BOLDCHECK, BN_CLICKED, OnBoldClicked)
			COMMAND_HANDLER(IDC_STYLE_ITALICCHECK, BN_CLICKED, OnItalicClicked)
			COMMAND_HANDLER(IDC_STYLE_UNDERLINECHECK, BN_CLICKED, OnUnderlineClicked)
			COMMAND_HANDLER(IDC_STYLE_EOLFILLEDCHECK, BN_CLICKED, OnEOLFilledClicked)
			COMMAND_HANDLER(IDC_STYLE_FONTCOMBO, CBN_SELCHANGE, OnFontChanged)
			COMMAND_HANDLER(IDC_STYLE_SIZECOMBO, CBN_SELCHANGE, OnSizeChanged)
			COMMAND_HANDLER(IDC_STYLE_RESETBTN, BN_CLICKED, OnResetClicked)
			COMMAND_HANDLER(IDC_STYLE_RESETALLBTN, BN_CLICKED, OnResetAllClicked)
			NOTIFY_HANDLER(IDC_STYLE_FOREBUTTON, CPN_SELCHANGE, OnForeChanged)
			NOTIFY_HANDLER(IDC_STYLE_BACKBUTTON, CPN_SELCHANGE, OnBackChanged)
			NOTIFY_HANDLER(IDC_STYLES_TREE, TVN_SELCHANGED, OnTreeSelChanged)
			CHAIN_MSG_MAP(CPropertyPageImpl<CTabPageStyles>)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()

		void SetScheme(SchemeConfig* pScheme);
		void Finalise();

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
		void UpdateGroupChildren(StyleDetails* pUpdatedClass, CustomStyleCollection* pColl = NULL);
		void UpdateStyle();

	protected:
		StyleDetails	m_Style;
		StyleDetails*	m_pStyle;
		SchemeConfig*	m_pScheme;
		CTreeViewCtrl	m_tree;
		CStyleDisplay	m_sd;

		CFontCombo		m_FontCombo;
		CNumberCombo	m_SizeCombo;

		CPNColorButton	m_fore;
		CPNColorButton	m_back;

		CButton			m_bold;
		CButton			m_italic;
		CButton			m_underline;
		CButton			m_eolfilled;

		HTREEITEM		m_lastTreeItem;

		bool			m_bGroup;
		bool			m_bChanging;
};

class COptionsPageSchemes : public COptionsPageImpl<COptionsPageSchemes>
{
	public:
		enum {IDD = IDD_PAGE_SCHEMES};

		COptionsPageSchemes(SchemeConfigParser* pSchemes);

		BEGIN_MSG_MAP(COptionsPageSchemes)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_HANDLER(IDC_SCHEMECOMBO, CBN_SELCHANGE, OnComboChange)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()

		virtual void OnInitialise();
		virtual void OnOK();
		virtual LPCTSTR GetTreePosition();

		bool IsDirty();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		void Update();

	protected:
		CSchemeCombo		m_combo;
		CContainedPropSheet	m_props;
		SchemeConfigParser* m_pSchemes;
		CTabPageStyles		m_stylestab;
		CTabPageKeywords	m_keywordstab;
		bool				m_bDirty;
};

/**
 * @brief Custom Tools page for the options dialog.
 */
class COptionsPageTools : public COptionsPageImpl<COptionsPageTools>
{
	public:
		enum {IDD = IDD_PAGE_TOOLS};

		COptionsPageTools(SchemeConfigParser* pSchemes);
		~COptionsPageTools();

		BEGIN_MSG_MAP(COptionsPageTools)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_HANDLER(IDC_SCHEMECOMBO, CBN_SELCHANGE, OnComboChange)
			COMMAND_HANDLER(IDC_TOOLS_EDITBUTTON, BN_CLICKED, OnEditClicked)
			COMMAND_HANDLER(IDC_TOOLS_REMOVEBUTTON, BN_CLICKED, OnRemoveClicked)
			COMMAND_HANDLER(IDC_TOOLS_ADDBUTTON, BN_CLICKED, OnAddClicked)
			COMMAND_HANDLER(IDC_TOOLS_MOVEUPBUTTON, BN_CLICKED, OnUpClicked)
			COMMAND_HANDLER(IDC_TOOLS_MOVEDOWNBUTTON, BN_CLICKED, OnDownClicked)
			NOTIFY_HANDLER(IDC_LIST, LVN_KEYDOWN, OnListKeyDown);
			NOTIFY_HANDLER(IDC_LIST, NM_CLICK, OnListClicked)
			NOTIFY_HANDLER(IDC_LIST, NM_DBLCLK, OnListDblClicked)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()

		virtual void OnInitialise();
		virtual void OnOK();
		virtual LPCTSTR GetTreePosition();

	protected:
		void AddDefinition(ToolDefinition* pDef);

		void EnableButtons();
		void Update();
		void SetItem();

		SchemeTools* GetTools();
	
	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnEditClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnUpClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnDownClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnListKeyDown(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnListClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnListDblClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	protected:
		bool				m_bChanging;
		CSchemeCombo		m_combo;
		//CListViewCtrl		m_list;
		CSSListCtrl		m_list;
		SchemeConfigParser* m_pSchemes;
		SchemeConfig*		m_pScheme;
		SchemeTools*		m_pCurrent;

		SchemeToolsManager	m_toolstore;

		CArrowButton		m_btnMoveUp;
		CArrowButton		m_btnMoveDown;
};

class COptionsPageNewFiles : public COptionsPageImpl<COptionsPageNewFiles>
{
	public:
		COptionsPageNewFiles(SchemeConfigParser* pSchemes);

		enum {IDD = IDD_PAGE_NEWFILES};

		BEGIN_MSG_MAP(COptionsPageNewFiles)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_HANDLER(IDC_NEW_SCHEMECOMBO, CBN_SELCHANGE, OnComboChange)
			COMMAND_HANDLER(IDC_SMARTSTART_EDITBUTTON, BN_CLICKED, OnEditClicked)
			COMMAND_HANDLER(IDC_SMARTSTART_REMOVEBUTTON, BN_CLICKED, OnRemoveClicked)
			COMMAND_HANDLER(IDC_SMARTSTART_ADDBUTTON, BN_CLICKED, OnAddClicked)
			COMMAND_HANDLER(IDC_SMARTSTART_ENABLECHECK, BN_CLICKED, OnEnabledChanged)
			NOTIFY_HANDLER(IDC_SMARTSTART_LIST, LVN_KEYDOWN, OnListKeyDown);
			NOTIFY_HANDLER(IDC_SMARTSTART_LIST, NM_CLICK, OnListClicked)
			NOTIFY_HANDLER(IDC_SMARTSTART_LIST, NM_DBLCLK, OnListDblClicked)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()

		virtual void OnInitialise();
		virtual void OnOK();
		virtual void OnCancel();
		virtual LPCTSTR GetTreePosition();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnEditClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnEnabledChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnListKeyDown(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnListClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnListDblClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

		void AddItem(LPCTSTR key, LPCTSTR schemetitle);
		void EnableButtons();
		void FreeResources();

		bool				m_bDirty;
		CListViewCtrl		m_list;
		SchemeConfigParser*	m_pSchemes;
		CSchemeCombo		m_combo;
		CButton				m_ssCheck;
};

#endif