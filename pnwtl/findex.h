#ifndef findex_h__included_8B16FC2D_D4A3_4d2c_ACA1_4A80DE51B836
#define findex_h__included_8B16FC2D_D4A3_4d2c_ACA1_4A80DE51B836

class CFindExDialog : public CDialogImpl<CFindExDialog, CWindow>,
						public CWinDataExchange<CFindExDialog>,
						public CUpdateUI<CFindExDialog>,
						public CDialogResize<CFindExDialog>
{
public:
	CFindExDialog();
	enum {IDD = IDD_FINDEX};

protected:

	BEGIN_MSG_MAP(CFindExDialog)
        MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
        ///...
        CHAIN_MSG_MAP(CDialogResize<CFindExDialog>)
		REFLECT_NOTIFICATIONS ()
    END_MSG_MAP()

	BEGIN_UPDATE_UI_MAP(CReplaceDlg)
		UPDATE_ELEMENT(IDC_REHELPER_BUTTON, UPDUI_CHILDWINDOW)
		UPDATE_ELEMENT(IDC_RHELPER_BUTTON, UPDUI_CHILDWINDOW)
	END_UPDATE_UI_MAP()

	BEGIN_DDX_MAP(CReplaceDlg)
		DDX_TEXT(IDC_FINDTEXT_COMBO, m_FindText)
		DDX_TEXT(IDC_REPLACETEXT_COMBO, m_ReplaceText)
		DDX_CHECK(IDC_MATCHCASE_CHECK, m_bMatchCase)
		DDX_CHECK(IDC_MATCHWHOLE_CHECK, m_bMatchWhole)
		DDX_CHECK(IDC_REGEXP_CHECK, m_bRegExp)
		DDX_CHECK(IDC_BACKSLASH_CHECK, m_bUseSlashes)
		DDX_RADIO(IDC_UP_RADIO, m_Direction)
		//DDX_CHECK(IDC_SEARCHALL_CHECK, m_bSearchAll)
	END_DDX_MAP()

	/*    DLGRESIZE_CONTROL(ControlID, Flags)

ControlID is the ID of the dialog control. The possible flags and their meanings are:

    * DLSZ_SIZE_X: Resize the width of the control as the dialog resizes horizontally.
    * DLSZ_SIZE_Y: Resize the height of the control as the dialog resizes vertically.
    * DLSZ_MOVE_X: Move the control horizontally as the dialog resizes horizontally.
    * DLSZ_MOVE_Y: Move the control vertically as the dialog resizes vertically.
    * DLSZ_REPAINT: Invalidate the control after every move/resize so it repaints every time.

	*/

    BEGIN_DLGRESIZE_MAP(CFindExDialog)
		DLGRESIZE_CONTROL(IDC_FINDTEXT_COMBO, DLSZ_SIZE_X)
		DLGRESIZE_CONTROL(IDC_FINDEX_LINE, DLSZ_SIZE_X)
		DLGRESIZE_CONTROL(IDC_FINDEX_TABS, DLSZ_SIZE_X)
		DLGRESIZE_CONTROL(IDC_REHELPER_BUTTON, DLSZ_MOVE_X)
		DLGRESIZE_CONTROL(IDC_FINDNEXT_BUTTON, DLSZ_MOVE_X)
		DLGRESIZE_CONTROL(IDC_MARKALL_BUTTON, DLSZ_MOVE_X)
		DLGRESIZE_CONTROL(IDCANCEL, DLSZ_MOVE_X)
    END_DLGRESIZE_MAP()


	LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

protected:
	CSize GetGUIFontSize()
	{
		CClientDC dc(m_hWnd);
		dc.SelectFont((HFONT) GetStockObject( DEFAULT_GUI_FONT ));		
		TEXTMETRIC tm;
		dc.GetTextMetrics( &tm );
		//int cxChar = tm.tmAveCharWidth;
		//int cyChar = tm.tmHeight + tm.tmExternalLeading;

		return CSize( tm.tmAveCharWidth, tm.tmHeight + tm.tmExternalLeading);
	}

	//void resizeControls();

	int calcTabAreaHeight();
	int addTab(LPCTSTR name, int iconIndex);
	void setupTabs();

	int positionChecks(int top);
	void updateLayout();

protected:
	typedef enum { eftFind, eftReplace, eftFindInFiles } EFindDialogType;
	EFindDialogType			m_type;
	CDotNetButtonTabCtrl<>	m_tabControl;
	CImageList				m_imageList;
	BXT::CComboBoxAC		m_FindTextCombo;
	BXT::CComboBoxAC		m_ReplaceTextCombo;
	BXT::CComboBoxAC		m_FindWhereCombo;
	CComboBox				m_FindTypeCombo;
	CArrowButton			m_ReHelperBtn;
	CArrowButton			m_ReHelperBtn2;

	int		m_group2Top;
	int		m_group1Bottom;
	int		m_group2Bottom;
	int		m_comboDistance;
	int		m_group3Bottom;

	CString	m_FindText;
	CString m_ReplaceText;
	int		m_Direction;
	BOOL	m_bMatchCase;
	BOOL	m_bMatchWhole;
	BOOL	m_bRegExp;
	BOOL	m_bUseSlashes;
};

#endif //#ifndef findex_h__included_8B16FC2D_D4A3_4d2c_ACA1_4A80DE51B836