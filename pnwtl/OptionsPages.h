/**
 * @file optionspages.h
 * @brief Options Dialog Pages (1) for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef optionspages_h__included
#define optionspages_h__included

#include "include/optionsdialog.h"
#include "include/fontcombo.h"
#include "include/ColorButton.h"

#include "SchemeConfig.h"
#include "tools.h"

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
		END_DDX_MAP()

		virtual void OnOK();
		virtual void OnInitialise();
		virtual LPCTSTR GetTreePosition();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		
		BOOL m_bUseTabs;
		BOOL m_bIndentGuides;
		BOOL m_bLineNos;
		UINT m_iTabWidth;
		EPNSaveFormat m_SaveFormat;
};

class COptionsPageStyle : public COptionsPageImpl<COptionsPageStyle>
{
	public:
		COptionsPageStyle(SchemeConfigParser* pSchemes) : m_pSchemes(pSchemes){}

		BEGIN_MSG_MAP(COptionsPageStyle)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()
		enum { IDD = IDD_PAGE_STYLE };

		virtual void OnOK();
		virtual void OnCancel();
		virtual void OnInitialise();
		virtual LPCTSTR GetTreePosition();

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

		SchemeConfigParser* m_pSchemes;
};

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

#include "ScintillaWTL.h"

class CScintillaDialogWnd : public CScintillaWindowImpl< CScintillaDialogWnd, CScintilla>
{
public:
	typedef CScintillaWindowImpl< CScintillaDialogWnd, CScintilla> baseClass;
	BEGIN_MSG_MAP(CScintillaDialogWnd)
		MESSAGE_HANDLER(WM_GETDLGCODE, OnGetDlgCode)
		CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	LRESULT OnGetDlgCode(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		return DLGC_HASSETSEL | DLGC_WANTARROWS;
		//return 0;
	}
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

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		void Update();

	protected:
		CComboBox			m_combo;
		CContainedPropSheet	m_props;
		SchemeConfigParser* m_pSchemes;
		CTabPageStyles		m_stylestab;
		CTabPageKeywords	m_keywordstab;
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
			NOTIFY_HANDLER(IDC_LIST, LVN_KEYDOWN, OnListKeyDown);
			NOTIFY_HANDLER(IDC_LIST, NM_CLICK, OnListClicked)
			NOTIFY_HANDLER(IDC_LIST, NM_DBLCLK, OnListDblClicked)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()

		virtual void OnInitialise();
		virtual void OnOK();
		virtual LPCTSTR GetTreePosition();

	protected:
		void AddDefinition(SToolDefinition* pDef);

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

		LRESULT OnListKeyDown(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnListClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnListDblClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	protected:
		bool				m_bChanging;
		CComboBox			m_combo;
		CListViewCtrl		m_list;
		SchemeConfigParser* m_pSchemes;
		SchemeConfig*		m_pScheme;
		SchemeTools*		m_pCurrent;

		SchemeToolsManager	m_toolstore;

		CArrowButton		m_btnMoveUp;
		CArrowButton		m_btnMoveDown;
};

class CToolEditorDialog : public CDialogImpl<CToolEditorDialog>,
							public CWinDataExchange<CToolEditorDialog>
{
	public:
		CToolEditorDialog();
        
		enum {IDD = IDD_TOOLEDITOR};

		BEGIN_MSG_MAP(CToolEditorDialog)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_ID_HANDLER(IDOK, OnOK)
			COMMAND_ID_HANDLER(IDCANCEL, OnCancel)
			COMMAND_ID_HANDLER(IDC_TE_COMMANDBUTTON, OnBrowseCommand);
			COMMAND_ID_HANDLER(IDC_TE_DIRBUTTON, OnBrowseDir);
		END_MSG_MAP()

		BEGIN_DDX_MAP(CToolEditorDialog)
			DDX_TEXT(IDC_TE_NAMEEDIT,		m_csName)
			DDX_TEXT(IDC_TE_CMDEDIT,		m_csCommand)
			DDX_TEXT(IDC_TE_FOLDEREDIT,		m_csFolder)
			DDX_TEXT(IDC_TE_PARAMSEDIT,		m_csParams)
			DDX_TEXT(IDC_TE_SHORTCUTEDIT,	m_csShortcut)
			DDX_CHECK(IDC_TE_CAPTURECHECK,	m_bCapture)
			DDX_CHECK(IDC_TE_FILTERCHECK,	m_bFilter)
			DDX_CHECK(IDC_TE_SAVEALLCHECK,	m_bSaveAll)
		END_DDX_MAP()

		CString m_csDisplayTitle;

		void GetValues(SToolDefinition* pDefinition);
		void SetValues(SToolDefinition* pDefinition);

		void SetTitle(LPCTSTR title);

	protected:
		class CInfoLabel : public CWindowImpl<CInfoLabel>
		{
			public:
				CInfoLabel();
				~CInfoLabel();

				BEGIN_MSG_MAP(CInfoLabel)
					MESSAGE_HANDLER(WM_PAINT, OnPaint);
				END_MSG_MAP()

			protected:
				void MakeFonts(HDC hDC);
				LRESULT OnPaint(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

				CFont*	m_pTitleFont;
				TCHAR	strbuf[200];
		};

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnBrowseDir(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnBrowseCommand(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	protected:
		CString	m_csName;
		CString	m_csCommand;
		CString	m_csFolder;
		CString	m_csParams;
		CString	m_csShortcut;

		BOOL	m_bCapture;
		BOOL	m_bFilter;
		BOOL	m_bSaveAll;

		CInfoLabel m_infolabel;
};

#endif