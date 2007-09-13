/**
 * @file optionspages.h
 * @brief Options Dialog Pages (1) for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002-2007 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef optionspages_h__included
#define optionspages_h__included

#include "include/optionsdialog.h"
#include "include/sslistctrl.h"
#include "optionscontrols.h"
#include "StyleTabPages.h"
#include "SchemeConfig.h"
#include "tools.h"

namespace Projects
{
	class ProjectTemplate;
}

class ToolsManager;

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
			DDX_UINT(IDC_OPT_MRUCOUNT,			m_iMRUSize)
			DDX_CHECK(IDC_OPT_MAXCHECK,			m_bMaximise)
			DDX_CHECK(IDC_OPT_FULLPATHCHECK,	m_bFullPath)
			DDX_CHECK(IDC_OPT_NEWFILEONSTART,	m_bNewOnStart)
			DDX_CHECK(IDC_MULTIINSTANCECHECK,	m_bMultiInstanceOk)
			DDX_CHECK(IDC_OPT_SHOWTABSCHECK,	m_bShowTabs)
			//DDX_CHECK(IDC_OPT_HIDEONETABCHECK,	m_bHideSingleTab)
			DDX_CHECK(IDC_OPT_TABSBOTTOMCHECK,	m_bTabsOnBottom)
			DDX_CHECK(IDC_OPT_MAXTABSONLY,		m_bTabsOnlyMax)
			DDX_CHECK(IDC_OPT_TABORDERCHECK,	m_bManageTabOrder)
			DDX_CHECK(IDC_OPT_SAVEWORKSPACE,	m_bSaveWorkspace)
		END_DDX_MAP()

		virtual void OnOK();
		virtual void OnInitialise();
		virtual LPCTSTR GetTreePosition();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		
		BOOL			m_bNewOnStart;
		BOOL			m_bMaximise;
		BOOL			m_bFullPath;
		BOOL			m_bMultiInstanceOk;
		UINT			m_iMRUSize;
		BOOL			m_bShowTabs;
		//BOOL			m_bHideSingleTab;
		BOOL			m_bTabsOnBottom;
		BOOL			m_bTabsOnlyMax;
		BOOL			m_bManageTabOrder;
		BOOL			m_bSaveWorkspace;
};

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
		virtual LPCTSTR GetTreePosition();

	private:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

		BOOL			m_bUseTabs;
		BOOL			m_bLineNos;
		BOOL			m_bWrap;
		BOOL			m_bLineEndings;
		BOOL			m_bWhiteSpace;
		UINT			m_iTabWidth;
		EPNSaveFormat	m_SaveFormat;
		ECodePage		m_CodePage;
		int				m_CharSet;
};

class COptionsPageVisual : public COptionsPageImpl<COptionsPageVisual>,
							public CWinDataExchange<COptionsPageVisual>
{
	public:
		BEGIN_MSG_MAP(COptionsPageVisual)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()
		enum { IDD = IDD_PAGE_VISUALHELP };

		BEGIN_DDX_MAP(COptionsPageVisual)
			DDX_CHECK(IDC_OPT_INDENTGUIDESCHECK, m_bIndentGuides)
			DDX_CHECK(IDC_OPT_FOLDINGCHECK, m_bFolding)
			DDX_CHECK(IDC_OPT_LINELIGHTCHECK, m_bLineHighlight)
			DDX_RADIO(IDC_OPT_NOLLHELPRADIO, m_iLongLineHelp)
			DDX_UINT(IDC_OPT_LLCOLUMNEDIT, m_iRightColumn)
		END_DDX_MAP()

		virtual void OnOK();
		virtual void OnInitialise();
		virtual LPCTSTR GetTreePosition();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		
		BOOL m_bIndentGuides;
		BOOL m_bFolding;
		BOOL m_bLineHighlight;
		int m_iLongLineHelp;
		UINT m_iRightColumn;

		CPNColorButton	m_btnLineCol;
		CPNColorButton	m_btnLLCol;
		CTrackBarCtrl	m_trackerHighlight;
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
		virtual LPCTSTR GetTreePosition();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

		int m_iReOpen;
		int m_iReDrop;
};

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
		END_DDX_MAP()

		virtual void OnOK();
		virtual void OnInitialise();
		virtual LPCTSTR GetTreePosition();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		
		BOOL m_bOpenCurFileDir;
		BOOL m_bFindAlpha;
		BOOL m_bCloseFindNext;
};

class COptionsPageStyle : public COptionsPageImpl<COptionsPageStyle>
{
	public:
		COptionsPageStyle(SchemeConfigParser* pSchemes) : m_pSchemes(pSchemes), m_bDirty(false) {}

		BEGIN_MSG_MAP(COptionsPageStyle)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			MESSAGE_HANDLER(PN_NOTIFY, OnNotify)
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
		LRESULT OnNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	
	protected:
		CFontCombo		m_FontCombo;
		CNumberCombo	m_SizeCombo;

		CPNColorButton	m_fore;
		CPNColorButton	m_back;
		
		CButton			m_bold;
		CButton			m_italic;
		CButton			m_underline;

		CPNColorButton	m_cur;
		CPNColorButton	m_indentGuides;
		CPNColorButton	m_selFore;
		CPNColorButton	m_selBack;

		bool			m_bDirty;

		SchemeConfigParser* m_pSchemes;
		StylePtr		m_defclass;
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
		virtual LPCTSTR GetTreePosition();

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
		virtual LPCTSTR GetTreePosition();

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

	protected:
		bool				m_bChanging;
		CSchemeCombo		m_combo;
		CSSListCtrl			m_list;
		SchemeConfigParser* m_pSchemes;
		SchemeDetails*		m_pScheme;
		SchemeTools*		m_pCurrent;

		ToolsManager*		m_toolstore;

		CArrowButton		m_btnMoveUp;
		CArrowButton		m_btnMoveDown;
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
		virtual LPCTSTR GetTreePosition();

	protected:
		virtual void EnableButtons();

		virtual SchemeTools* GetTools();

		virtual CComboBox* getCombo();
		virtual void updateFromSel(int iSel);

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	protected:
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
		virtual LPCTSTR GetTreePosition();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

		LRESULT OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnEditClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnListDblClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

		void addItem(LPCTSTR set1, LPCTSTR set2, AlternateFileSet* lpData);

	protected:
		CListViewCtrl	m_list;
		SetsList*		sets;
		bool			m_bDirty;
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
		virtual LPCTSTR GetTreePosition();

		DWORD OnPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW /*lpNMCustomDraw*/);
		DWORD OnItemPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW /*lpNMCustomDraw*/);
		DWORD OnSubItemPrePaint(int /*idCtrl*/, LPNMCUSTOMDRAW /*lpNMCustomDraw*/);

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

		LRESULT OnExtensionChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnCheckNowClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnListItemChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnListSetFocus(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnListGetInfoTip(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
		LRESULT OnListKeyDown(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

		void SetMode(Mode mode, bool setExt = true, LPCTSTR extension = NULL);
		void ListItemToFileAssoc(int index, FileAssoc& fa);
		void RemoveExtension(int index);

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
};

class COptionsPageFileTypes : public COptionsPageImpl<COptionsPageFileTypes>
{
	public:
		COptionsPageFileTypes(SchemeConfigParser* schemes);
		~COptionsPageFileTypes();

		enum {IDD = IDD_PAGE_FILETYPES};

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
		virtual LPCTSTR GetTreePosition();

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

		LRESULT OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnEditClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnListDblClicked(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	protected:
		void addItem(int index, LPCTSTR ext, Scheme* pScheme, bool isFilename);
		void clear();

	protected:
		SchemeConfigParser* m_schemes;
		CListViewCtrl		m_list;
		SCHEME_MAP*			m_pExtMap;
		SCHEME_MAP*			m_pFilenameMap;
		bool				m_bDirty;
};

#include "OptionsPageKeyboard.h"
#include "OptionsPageAutocomplete.h"
#include "OptionsPageGlobalStyles.h"
#include "OptionsPageClips.h"

#endif