/**
 * @file OptionsDialogs.h
 * @brief Dialogs used to edit settings from the Options dialog.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef optionsdialogs_h__included
#define optionsdialogs_h__included

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
			COMMAND_ID_HANDLER(IDC_TE_COMMANDBUTTON, OnBrowseCommand)
			COMMAND_ID_HANDLER(IDC_TE_DIRBUTTON, OnBrowseDir)
			COMMAND_ID_HANDLER(IDC_TE_CAPTURECHECK, OnCaptureChanged)
			COMMAND_ID_HANDLER(IDC_TE_ABOUTBUILTIN, OnAboutBuiltin)
			COMMAND_ID_HANDLER(IDC_TE_CLEARBUTTON, OnClearShortcut)
			COMMAND_ID_HANDLER(IDC_TE_BUILTIN, OnWindowStateChanged)
			COMMAND_ID_HANDLER(IDC_TE_CUSTOMPARSE, OnWindowStateChanged)
		END_MSG_MAP()

		BEGIN_DDX_MAP(CToolEditorDialog)
			DDX_TEXT(IDC_TE_NAMEEDIT,		m_csName)
			DDX_TEXT(IDC_TE_CMDEDIT,		m_csCommand)
			DDX_TEXT(IDC_TE_FOLDEREDIT,		m_csFolder)
			DDX_TEXT(IDC_TE_PARAMSEDIT,		m_csParams)
			DDX_TEXT(IDC_TE_CUSTOMTEXT,		m_csCustomPattern)
			DDX_CHECK(IDC_TE_CAPTURECHECK,	m_bCapture)
			DDX_CHECK(IDC_TE_FILTERCHECK,	m_bFilter)
			//DDX_CHECK(IDC_TE_SAVEALLCHECK,	m_bSaveAll)
			DDX_RADIO(IDC_TE_BUILTIN,		m_iBuiltIn)
			DDX_CHECK(IDC_TE_CLEARCHECK,	m_bClear)
		END_DDX_MAP()

		CString m_csDisplayTitle;

		void GetValues(ToolDefinition* pDefinition);
		void SetValues(ToolDefinition* pDefinition);

		void SetTitle(LPCTSTR title);

	protected:
		class CInfoLabel : public CWindowImpl<CInfoLabel>
		{
			public:
				CInfoLabel(LPCTSTR title, DWORD StringID);
				~CInfoLabel();

				BEGIN_MSG_MAP(CInfoLabel)
					MESSAGE_HANDLER(WM_PAINT, OnPaint);
				END_MSG_MAP()

			protected:
				void MakeFonts(HDC hDC);
				LRESULT OnPaint(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

				CFont*	m_pTitleFont;
				TCHAR	strbuf[200];
				tstring m_title;
		};

	protected:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnBrowseDir(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnBrowseCommand(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnCaptureChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnAboutBuiltin(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnWindowStateChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnClearShortcut(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		void EnableButtons();

	protected:
		CString		m_csName;
		CString		m_csCommand;
		CString		m_csFolder;
		CString		m_csParams;
		CString		m_csCustomPattern;
		CHotKeyCtrl	m_HotKeyCtrl;

		BOOL	m_bCapture;
		BOOL	m_bFilter;
		int		m_iSaveStyle;
		BOOL	m_bClear;
		WORD	m_wHotKey;

		int		m_iBuiltIn;

		bool	m_bGlobal;

		CInfoLabel	m_infolabel;
		CInfoLabel	m_infolabel2;
		CComboBox	m_outputcombo;
		CComboBox	m_saveCombo;
};

// pre-declare SchemeConfig.
class SchemeConfigParser;

class CSmartStartEditorDialog : public CDialogImpl<CSmartStartEditorDialog>
{
	public:
		CSmartStartEditorDialog(SchemeConfigParser* pSchemes);

		enum {IDD = IDD_SMARTSTARTEDITOR};

		BEGIN_MSG_MAP(CSmartStartEditorDialog)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_ID_HANDLER(IDOK, OnOK)
			COMMAND_ID_HANDLER(IDCANCEL, OnCancel)
		END_MSG_MAP()

		void GetValues(tstring& startPhrase, tstring& schemeName);
		void SetValues(LPCTSTR startPhrase, LPCTSTR schemeName);

	protected:
		LRESULT OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	protected:
		SchemeConfigParser*	m_pSchemes;
		tstring				m_startPhrase;
		tstring				m_schemeName;
};

class CAFileEditorDialog : public CDialogImpl<CAFileEditorDialog>
{
	public:
		CAFileEditorDialog(){}

		enum {IDD = IDD_AFILEEDITOR};

		BEGIN_MSG_MAP(CAFileEditorDialog)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_ID_HANDLER(IDOK, OnOK)
			COMMAND_ID_HANDLER(IDCANCEL, OnCancel)
			
		END_MSG_MAP()

		void GetValues(tstring& set1, tstring& set2);
		void SetValues(LPCTSTR set1, LPCTSTR set2);

	protected:
		LRESULT OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	protected:
		tstring setFrom;
		tstring setTo;
};

#endif