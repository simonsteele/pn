/**
 * @file findex.h
 * @brief Find and Replace dialogs for PN 2
 * @author Simon Steele
 * @note Copyright (c) 2004-2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef findex_h__included_8B16FC2D_D4A3_4d2c_ACA1_4A80DE51B836
#define findex_h__included_8B16FC2D_D4A3_4d2c_ACA1_4A80DE51B836

class CChildFrame;
class CTextView;

typedef BOOL (__stdcall *PFNSetLayeredWindowAttributes)(HWND hwnd, COLORREF crKey, BYTE bAlpha, DWORD dwFlags);

class CFindExDialog : public CDialogImpl<CFindExDialog, CWindow>,
						public CWinDataExchange<CFindExDialog>,
						public CUpdateUI<CFindExDialog>,
						public CDialogResize<CFindExDialog>,
						public CMessageFilter
{
public:
	CFindExDialog();
	enum {IDD = IDD_FINDEX};

	virtual BOOL PreTranslateMessage(MSG* pMsg);

	void Show(EFindDialogType type = eftFind, LPCTSTR findText = NULL);

protected:

	BEGIN_MSG_MAP(CFindExDialog)
        MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
		MESSAGE_HANDLER(WM_SHOWWINDOW, OnShowWindow)
		MESSAGE_HANDLER(WM_CLOSE, OnCloseWindow)
		MESSAGE_HANDLER(WM_ACTIVATE, OnActivate)
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy)

		COMMAND_ID_HANDLER(IDOK, OnCloseCmd)
		COMMAND_ID_HANDLER(IDCANCEL, OnCloseCmd)
		COMMAND_ID_HANDLER(IDC_FINDNEXT_BUTTON, OnFindNext)
		COMMAND_ID_HANDLER(IDC_MARKALL_BUTTON, OnMarkAll)
		COMMAND_HANDLER(IDC_REPLACE_BUTTON, BN_CLICKED, OnReplaceClicked)
		COMMAND_HANDLER(IDC_REPLACEALL_BUTTON, BN_CLICKED, OnReplaceAllClicked)
		COMMAND_HANDLER(IDC_BROWSE_BUTTON, BN_CLICKED, OnBrowseClicked)

		// Regex helper stuff...
		COMMAND_ID_HANDLER(IDC_REHELPER_BUTTON, OnReHelperClicked)
		COMMAND_ID_HANDLER(IDC_RHELPER_BUTTON, OnReHelperClicked)
		COMMAND_RANGE_HANDLER(ID_REGEXP_ANYCHARACTER, ID_REGEXP_GROUP, OnReInsertClicked)
		COMMAND_RANGE_HANDLER(ID_REGEXP_TAGGEDEXPRESSION1, ID_REGEXP_TAGGEDEXPRESSION9, OnReMatchesMenuItemClicked)
		COMMAND_ID_HANDLER(IDC_REGEXP_CHECK, OnUseRegExpClicked)
		COMMAND_RANGE_CODE_HANDLER(IDC_CURRENTDOC_RADIO, IDC_INSELECTION_RADIO, BN_CLICKED, OnRadioClicked)

		NOTIFY_CODE_HANDLER(CTCN_SELCHANGE, OnSelChange)
		

        CHAIN_MSG_MAP(CDialogResize<CFindExDialog>)
		REFLECT_NOTIFICATIONS ()
    END_MSG_MAP()

	BEGIN_UPDATE_UI_MAP(CFindExDialog)
		UPDATE_ELEMENT(IDC_REHELPER_BUTTON, UPDUI_CHILDWINDOW)
		UPDATE_ELEMENT(IDC_RHELPER_BUTTON, UPDUI_CHILDWINDOW)
	END_UPDATE_UI_MAP()

	BEGIN_DDX_MAP(CFindExDialog)
		DDX_TEXT(IDC_FINDTEXT_COMBO, m_FindText)
		DDX_TEXT(IDC_REPLACETEXT_COMBO, m_ReplaceText)
		DDX_TEXT(IDC_FINDWHERE_COMBO, m_FindWhereText)
		DDX_TEXT(IDC_FINDTYPE_COMBO, m_FindTypeText)
		DDX_CHECK(IDC_MATCHCASE_CHECK, m_bMatchCase)
		DDX_CHECK(IDC_MATCHWHOLE_CHECK, m_bMatchWhole)
		DDX_CHECK(IDC_REGEXP_CHECK, m_bRegExp)
		DDX_CHECK(IDC_BACKSLASHES_CHECK, m_bUseSlashes)
		DDX_CHECK(IDC_SUBDIRS_CHECK, m_bSearchSubdirs)
		DDX_CHECK(IDC_INCLUDEHIDDEN_CHECK, m_bIncludeHidden)
		DDX_RADIO(IDC_CURRENTDOC_RADIO, m_SearchWhere)
		DDX_CHECK(IDC_SEARCHUP_CHECK, m_bSearchUp)
		//DDX_CHECK(IDC_SEARCHALL_CHECK, m_bSearchAll)
	END_DDX_MAP()

    BEGIN_DLGRESIZE_MAP(CFindExDialog)
		DLGRESIZE_CONTROL(IDC_FINDTEXT_COMBO, DLSZ_SIZE_X)
		DLGRESIZE_CONTROL(IDC_FINDEX_LINE, DLSZ_SIZE_X)
		DLGRESIZE_CONTROL(IDC_FINDEX_TABS, DLSZ_SIZE_X)
		DLGRESIZE_CONTROL(IDC_REHELPER_BUTTON, DLSZ_MOVE_X)
		DLGRESIZE_CONTROL(IDC_REPLACETEXT_COMBO, DLSZ_SIZE_X)
		DLGRESIZE_CONTROL(IDC_RHELPER_BUTTON, DLSZ_MOVE_X)
		DLGRESIZE_CONTROL(IDC_FINDWHERE_COMBO, DLSZ_SIZE_X)
		DLGRESIZE_CONTROL(IDC_FINDTYPE_COMBO, DLSZ_SIZE_X)
		DLGRESIZE_CONTROL(IDC_BROWSE_BUTTON, DLSZ_MOVE_X)
		DLGRESIZE_CONTROL(IDC_FINDNEXT_BUTTON, DLSZ_MOVE_X)
		DLGRESIZE_CONTROL(IDC_MARKALL_BUTTON, DLSZ_MOVE_X)
		DLGRESIZE_CONTROL(IDC_REPLACE_BUTTON, DLSZ_MOVE_X)
		DLGRESIZE_CONTROL(IDC_REPLACEALL_BUTTON, DLSZ_MOVE_X)
		DLGRESIZE_CONTROL(IDCANCEL, DLSZ_MOVE_X)
    END_DLGRESIZE_MAP()

	// Messages
	LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnShowWindow(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnCloseWindow(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnActivate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	
	// Commands
	LRESULT OnCloseCmd(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnFindNext(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnMarkAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnReHelperClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnReHelper2Clicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnReInsertClicked(WORD /*wNotifyCode*/, WORD nID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnReMatchesMenuItemClicked(WORD /*wNotifyCode*/, WORD nID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnReplaceClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnReplaceAllClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnBrowseClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnUseRegExpClicked(WORD /*wNotifyCode*/, WORD /*nID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnRadioClicked(WORD /*wNotifyCode*/, WORD /*nID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	// Notifications
	LRESULT OnSelChange(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& bHandled);

protected:
	int addTab(LPCTSTR name, int iconIndex);
	int calcTabAreaHeight();
	void doRegExpHelperMenu(LPRECT rc, bool bDoMatches = false);
	int doRegExpInsert(BXT::CComboBoxAC* pCB, LPCTSTR insert, CString& str, int offset);
	bool editorChanged();
	void enableButtons();
	FindNextResult findNext();
	void findInFiles();
	CChildFrame* getCurrentEditorWnd();
	CSize getGUIFontSize();
	SearchOptions* getOptions();
	int getRegExpString(int nID, CString& Text);
	void moveUp(int offset, CWindow& ctrl);
	void placeWindow(const POINT& pt, int lineHeight = 10);
	int positionChecks(int top, const UINT* checkboxIDs, int nCheckboxIDs);
	void updateLayout();
	bool selectionIsWholeLine(CTextView* textView);
	BOOL setLayeredWindowAttributes(HWND hwnd, COLORREF crKey, BYTE bAlpha, DWORD dwFlags);

protected:
	typedef enum { fwCurrentFile = 1, fwCurrentFolder = 2, fwCurrentProjectFiles = 3, fwOpenDocs = 4, fwUser = 5 } EFIFWhere;

	EFIFWhere				m_lastFifLocation;
	EFindDialogType			m_type;
	EFindDialogType			m_lastType;
	CDotNetButtonTabCtrl<>	m_tabControl;
	CImageList				m_imageList;
	BXT::CComboBoxAC		m_FindTextCombo;
	BXT::CComboBoxAC		m_ReplaceTextCombo;
	BXT::CComboBoxAC		m_FindWhereCombo;
	BXT::CComboBoxAC		m_FindTypeCombo;
	CArrowButton			m_ReHelperBtn;
	CArrowButton			m_ReHelperBtn2;
	CChildFrame*			m_pLastEditor;
	bool					m_bInitialising;
	bool					m_bHiding;

	// Positional information
	int		m_group2Top;
	int		m_group1Bottom;
	int		m_group2Bottom;
	int		m_comboDistance;
	int		m_group3Bottom;
	int		m_lastVisibleCB;
	int		m_checkDist;
	int		m_bottom;
	
	// Find/Replace Information
	CString		m_FindText;
	CString		m_ReplaceText;
	CString		m_FindWhereText;
	CString		m_FindTypeText;
	int			m_SearchWhere;
	BOOL		m_bMatchCase;
	BOOL		m_bMatchWhole;
	BOOL		m_bRegExp;
	BOOL		m_bUseSlashes;
	BOOL		m_bSearchSubdirs;
	BOOL		m_bIncludeHidden;
	BOOL		m_bSearchUp;

	PFNSetLayeredWindowAttributes	m_pFnSLWA;
};

#endif //#ifndef findex_h__included_8B16FC2D_D4A3_4d2c_ACA1_4A80DE51B836