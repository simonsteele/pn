/**
 * @file optionspages.h
 * @brief Options Dialog Pages for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef optionspages_h__included
#define optionspages_h__included

#include "include/optionsdialog.h"
#include "include/sslistctrl.h"
#include "controls/OptionsBlockHeader.h"
#include "optionscontrols.h"
#include "StyleTabPages.h"
#include "SchemeConfig.h"
#include "tools.h"

namespace Projects
{
	class ProjectTemplate;
}

class ToolsManager;

class COptionsPageEditDefaults : public COptionsPageImpl<COptionsPageEditDefaults>,
								public CWinDataExchange<COptionsPageEditDefaults>
{
	public:
		BEGIN_MSG_MAP(COptionsPageEditDefaults)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()
		enum { IDD = IDD_PAGE_EDITDEFS };

		BEGIN_DDX_MAP(COptionsPageEditDefaults)
			DDX_CHECK(IDC_OPT_USETABSCHECK,		m_bUseTabs)
			DDX_CHECK(IDC_OPT_LINENOSCHECK,		m_bLineNos)
			DDX_CHECK(IDC_OPT_WORDWRAPCHECK,	m_bWrap)
			DDX_UINT(IDC_OPT_TABWIDTHEDIT,		m_iTabWidth)
			DDX_CHECK(IDC_OPT_LINEENDINGSCHECK, m_bLineEndings)
			DDX_CHECK(IDC_OPT_WHITESPACECHECK,	m_bWhiteSpace)
		END_DDX_MAP()

		virtual void OnOK();
		virtual void OnInitialise();
		virtual tstring GetTreePosition();

	private:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

		BOOL			m_bUseTabs;
		BOOL			m_bLineNos;
		BOOL			m_bWrap;
		BOOL			m_bLineEndings;
		BOOL			m_bWhiteSpace;
		UINT			m_iTabWidth;
		EPNSaveFormat	m_SaveFormat;
		ECodePage		m_AnsiCodePage;
		EPNEncoding		m_DefaultEncoding;

		COptionsBlockHeader m_defaultsHeader;
};

class COptionsPageConf : public COptionsPageImpl<COptionsPageConf>,
							public CWinDataExchange<COptionsPageConf>
{
	public:
		BEGIN_MSG_MAP(COptionsPageConf)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()
		enum { IDD = IDD_PAGE_CONF };

		BEGIN_DDX_MAP(COptionsPageConf)
			DDX_RADIO(IDC_REOPEN_DOIT, m_iReOpen)
			DDX_RADIO(IDC_REDROP_DOIT, m_iReDrop)
		END_DDX_MAP()

		virtual void OnOK();
		virtual void OnInitialise();
		virtual tstring GetTreePosition();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

		int m_iReOpen;
		int m_iReDrop;

		COptionsBlockHeader m_settingsHeader;
};

/**
 * "Interface" options page, was once "Dialogs". Options here for turning on and off
 * bits of the UI.
 */
class COptionsPageDialogs : public COptionsPageImpl<COptionsPageDialogs>,
							public CWinDataExchange<COptionsPageDialogs>
{
	public:
		BEGIN_MSG_MAP(COptionsPageDialogs)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()
		
		enum { IDD = IDD_PAGE_DIALOGS };

		BEGIN_DDX_MAP(COptionsPageDialogs)
			DDX_CHECK(IDC_OPENCURFILEDIRCHECK,	m_bOpenCurFileDir)
			DDX_CHECK(IDC_FINDALPHACHECK,		m_bFindAlpha)
			DDX_CHECK(IDC_CLOSEONFINDNEXTCHECK,	m_bCloseFindNext)
			DDX_CHECK(IDC_SHOWEDITORTOOLBARCHECK, m_bShowEditorToolbar)
			DDX_CHECK(IDC_ENABLECOMMANDBAR_CHECK, m_bEnableCmdBar)
		END_DDX_MAP()

		virtual void OnOK();
		virtual void OnInitialise();
		virtual tstring GetTreePosition();

	private:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		
		BOOL m_bOpenCurFileDir;
		BOOL m_bFindAlpha;
		BOOL m_bCloseFindNext;
		BOOL m_bShowEditorToolbar;
		BOOL m_bEnableCmdBar;

		std::map<int, tstring> m_lcid_map;

		COptionsBlockHeader m_languageHeader;
		COptionsBlockHeader m_dialogsHeader;
		COptionsBlockHeader m_findHeader;
		COptionsBlockHeader m_settingsHeader;
};

class COptionsPageSchemes : public COptionsPageImpl<COptionsPageSchemes>
{
	public:
		enum {IDD = IDD_PAGE_SCHEMES};

		COptionsPageSchemes(SchemeConfigParser* pSchemes);

		BEGIN_MSG_MAP(COptionsPageSchemes)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			MESSAGE_HANDLER(PN_NOTIFY, OnNotify)
			COMMAND_HANDLER(IDC_SCHEMECOMBO, CBN_SELCHANGE, OnComboChange)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()

		virtual void OnInitialise();
		virtual void OnOK();
		virtual tstring GetTreePosition();

		bool IsDirty();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		void Update();

	protected:
		CSchemeCombo		m_combo;
		CContainedPropSheet	m_props;
		SchemeConfigParser* m_pSchemes;
		CTabPageStyles		m_stylestab;
		CTabPageKeywords	m_keywordstab;
		CTabPageMisc		m_misctab;
		bool				m_bDirty;
};

/**
 * @brief Custom Tools page for the options dialog.
 */
class COptionsPageTools : public COptionsPageImpl<COptionsPageTools>
{
	public:
		enum {IDD = IDD_PAGE_TOOLS};

		COptionsPageTools(SchemeConfigParser* pSchemes, ToolsManager* pToolManager);
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
		virtual tstring GetTreePosition();

	protected:
		COptionsPageTools(ToolsManager* pToolManager);

		void AddDefinition(ToolDefinition* pDef);

		virtual void EnableButtons();
		void Update();
		void UpdateIndexes();
		void SetItem();

		virtual SchemeTools* GetTools();

		bool doToolEditDlg(ToolDefinition* in, ToolDefinition* out);

		virtual CComboBox* getCombo();
		void enableButtons(bool bEnable);
		virtual void updateFromSel(int iSel);
	
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

		void initControls(LPCTSTR itemTitle);

	protected:
		bool				m_bChanging;
		CSSListCtrl			m_list;
		SchemeConfigParser* m_pSchemes;
		SchemeDetails*		m_pScheme;
		SchemeTools*		m_pCurrent;

		ToolsManager*		m_toolstore;

		CArrowButton		m_btnMoveUp;
		CArrowButton		m_btnMoveDown;

	private:
		CSchemeCombo		m_combo;
};

class COptionsPageProjectTools : public COptionsPageTools
{
	public:
		COptionsPageProjectTools(ToolsManager* pToolManager);
		~COptionsPageProjectTools();

		BEGIN_MSG_MAP(COptionsPageProjectTools)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			CHAIN_MSG_MAP(COptionsPageTools)
		END_MSG_MAP()

		virtual void OnInitialise();
		virtual void OnOK();
		virtual tstring GetTreePosition();

	protected:
		virtual void EnableButtons();

		virtual SchemeTools* GetTools();

		virtual CComboBox* getCombo();
		virtual void updateFromSel(int iSel);

		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	private:
		CComboBox	m_combo;
		Projects::ProjectTemplate* m_pTemplate;
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
		virtual tstring GetTreePosition();

	private:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnEditClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnEnabledChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnListKeyDown(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnListClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnListDblClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

		void AddItem(LPCSTR key, LPCSTR schemename);
		void EnableButtons();
		void FreeResources();

		bool				m_bDirty;
		CListViewCtrl		m_list;
		SchemeConfigParser*	m_pSchemes;
		CSchemeCombo		m_combo;
		CButton				m_ssCheck;

		COptionsBlockHeader m_settingsHeader;
		COptionsBlockHeader m_smartStartHeader;
};

class SetsList;
class AlternateFileSet;

class COptionsPageAFiles : public COptionsPageImpl<COptionsPageAFiles>
{
	public:
		COptionsPageAFiles();
		~COptionsPageAFiles();

		enum {IDD = IDD_PAGE_AFILES};

		BEGIN_MSG_MAP(COptionsPageAFiles)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)

			COMMAND_HANDLER(IDC_AFILES_ADDBUTTON, BN_CLICKED, OnAddClicked)
			COMMAND_HANDLER(IDC_AFILES_EDITBUTTON, BN_CLICKED, OnEditClicked)
			COMMAND_HANDLER(IDC_AFILES_REMOVEBUTTON, BN_CLICKED, OnRemoveClicked)
			NOTIFY_HANDLER(IDC_AFILES_LIST, NM_DBLCLK, OnListDblClicked)
		END_MSG_MAP()

		virtual void OnInitialise();
		virtual void OnOK();
		virtual void OnCancel();
		virtual tstring GetTreePosition();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

		LRESULT OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnEditClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnListDblClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

		void addItem(LPCTSTR set1, LPCTSTR set2, AlternateFileSet* lpData);

	private:
		CListViewCtrl	m_list;
		SetsList*		sets;
		bool			m_bDirty;

		COptionsBlockHeader m_settingsHeader;
};

#include "FileAssoc.h"

class COptionsPageFileAssoc : public COptionsPageImpl<COptionsPageFileAssoc>,
	public CCustomDraw<COptionsPageFileAssoc>
{
	enum Mode
	{
		ModeNone,
		ModeEdit,
		ModeAdd,
		ModeChanging,
	};
	enum Columns
	{
		ColConflict,
		ColExtension,
		ColMethod,
		ColTypeName,
	};

	typedef CSimpleMap<CString, CString> StringMap;

	public:
		COptionsPageFileAssoc();

		enum {IDD = IDD_PAGE_FILEASSO};

		BEGIN_MSG_MAP(COptionsPageFileAssoc)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_HANDLER(IDC_EXTENSION, EN_CHANGE, OnExtensionChange)
			COMMAND_HANDLER(IDC_ADD, BN_CLICKED, OnAddClicked)
			COMMAND_HANDLER(IDC_REMOVE, BN_CLICKED, OnRemoveClicked)
			COMMAND_HANDLER(IDC_CHECKNOW, BN_CLICKED, OnCheckNowClicked)
			COMMAND_HANDLER(IDC_ENABLEEXPLORERCONTEXT, BN_CLICKED, OnEnableExplorerContextMenu)
			NOTIFY_HANDLER(IDC_LIST, LVN_ITEMCHANGED, OnListItemChanged)
			NOTIFY_HANDLER(IDC_LIST, NM_SETFOCUS, OnListSetFocus)
			NOTIFY_HANDLER(IDC_LIST, LVN_GETINFOTIP, OnListGetInfoTip)
			NOTIFY_HANDLER(IDC_LIST, LVN_KEYDOWN, OnListKeyDown)
			CHAIN_MSG_MAP(CCustomDraw<COptionsPageFileAssoc>)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()

		virtual void OnInitialise();
		virtual void OnOK();
		virtual void OnCancel();
		virtual tstring GetTreePosition();

		DWORD OnPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW /*lpNMCustomDraw*/);
		DWORD OnItemPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW /*lpNMCustomDraw*/);
		DWORD OnSubItemPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW /*lpNMCustomDraw*/);

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

		LRESULT OnExtensionChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnCheckNowClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnEnableExplorerContextMenu(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnListItemChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnListSetFocus(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnListGetInfoTip(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnListKeyDown(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

		void SetMode(Mode mode, bool setExt = true, LPCTSTR extension = NULL);
		void ListItemToFileAssoc(int index, FileAssoc& fa);
		void RemoveExtension(int index);

		bool checkExplorerContextMenu();
		void removeContextMenu();
		void addContextMenu();

		bool			m_bDirty;
		bool			m_bKeyChange;
		Mode			m_mode;
		CComboBox		m_combo;
		CButton			m_buttonAddEdit;
		CButton			m_buttonRemove;
		CListViewCtrl	m_list;
		FileAssocManager m_fam;
		StringMap		m_conflicts;
		CFont			m_boldFont;
		COLORREF		m_colors[2];
		bool			m_bExplorerMenuRegistered;
};

class COptionsPageFileTypes : public COptionsPageImpl<COptionsPageFileTypes>,
						      public CWinDataExchange<COptionsPageFileTypes>
{
	public:
		COptionsPageFileTypes(SchemeConfigParser* schemes);
		~COptionsPageFileTypes();

		enum {IDD = IDD_PAGE_FILETYPES};

		BEGIN_DDX_MAP(COptionsPageNewFiles)
			DDX_CHECK(IDC_STRIPBEFORESAVE_CHECK, m_bStripTrailingOnSave)
			DDX_CHECK(IDC_ENSUREBLANKLINE_CHECK, m_bEnsureBlankLine)
		END_DDX_MAP()

		BEGIN_MSG_MAP(COptionsPageFileTypes)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)

			COMMAND_HANDLER(IDC_FILETYPES_ADDBUTTON, BN_CLICKED, OnAddClicked)
			COMMAND_HANDLER(IDC_FILETYPES_EDITBUTTON, BN_CLICKED, OnEditClicked)
			COMMAND_HANDLER(IDC_FILETYPES_REMOVEBUTTON, BN_CLICKED, OnRemoveClicked)
			NOTIFY_HANDLER(IDC_LIST, NM_DBLCLK, OnListDblClicked)
		END_MSG_MAP()

		virtual void OnInitialise();
		virtual void OnOK();
		virtual void OnCancel();
		virtual tstring GetTreePosition();

	private:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

		LRESULT OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnEditClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnListDblClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	private:
		void addItem(int index, LPCTSTR ext, Scheme* pScheme, bool isFilename);
		void clear();

		SchemeConfigParser* m_schemes;
		CListViewCtrl		m_list;
		SCHEME_MAP*			m_pExtMap;
		SCHEME_MAP*			m_pFilenameMap;
		bool				m_bDirty;
		BOOL				m_bStripTrailingOnSave;
		BOOL				m_bEnsureBlankLine;

		COptionsBlockHeader m_settingsHeader;
		COptionsBlockHeader m_optionsHeader;
};

#include "OptionsPageGeneral.h"
#include "OptionsPageKeyboard.h"
#include "OptionsPageAutocomplete.h"
#include "OptionsPageGlobalStyles.h"
#include "OptionsPageExtensions.h"
#include "OptionsPageEditing.h"
#include "OptionsPageVisual.h"
#include "OptionsPageStyle.h"

#endif