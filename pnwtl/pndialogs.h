/**
 * @file pndialogs.h
 * @brief Assorted Dialogs for Programmers Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef pndialogs_h__included
#define pndialogs_h__included

#include <list>
using std::list;

#include "resource.h"

/**
 * @class CPNFileDialogImpl
 * @brief Very simply adds handling for pipe (|) separated filters.
 */
template <class T>
class CPNFileDialogImpl : public CFileDialogImpl<T>
{
	public:
		CPNFileDialogImpl(BOOL bOpenFileDialog, // TRUE for FileOpen, FALSE for FileSaveAs
			LPCTSTR lpszDefExt = NULL,
			LPCTSTR lpszFileName = NULL,
			DWORD dwFlags = OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
			LPCTSTR lpszFilter = NULL,
			HWND hWndParent = NULL,
			bool bUsePipeChar = true)
			: CFileDialogImpl<T>(bOpenFileDialog, lpszDefExt, lpszFileName, dwFlags, lpszFilter, hWndParent)
		{
			if(lpszFilter != NULL && bUsePipeChar)
			{
				m_szFilter = new TCHAR[_tcslen(lpszFilter)+1];
				_tcscpy(m_szFilter, lpszFilter);
				
				LPTSTR pch = m_szFilter;
				while ((pch = _tcschr(pch, '|')) != NULL)
					*pch++ = '\0';

				m_ofn.lpstrFilter = m_szFilter;
			}
			else
			{
				m_szFilter = NULL;
				m_ofn.lpstrFilter = lpszFilter;
			}				
		}

		~CPNFileDialogImpl()
		{
			if(m_szFilter != NULL)
				delete [] m_szFilter;
		}

		BEGIN_MSG_MAP(CPNFileDialogImpl)
		END_MSG_MAP()

	protected:
		LPTSTR m_szFilter;
};

class CPNFileDialog : public CPNFileDialogImpl<CPNFileDialog>
{
public:
	CPNFileDialog(BOOL bOpenFileDialog, // TRUE for FileOpen, FALSE for FileSaveAs
		LPCTSTR lpszDefExt = NULL,
		LPCTSTR lpszFileName = NULL,
		DWORD dwFlags = OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		LPCTSTR lpszFilter = NULL,
		HWND hWndParent = NULL)
		: CPNFileDialogImpl<CPNFileDialog>(bOpenFileDialog, lpszDefExt, lpszFileName, dwFlags, lpszFilter, hWndParent)
	{ }

	// override base class map and references to handlers
	DECLARE_EMPTY_MSG_MAP()
};

/**
 * @class CPNSaveDialog
 * @brief Save dialog with added controls for save type (e.g. LF, CRLF, etc.)
 *
 * Thanks to Tim Smith for his excellent Code Project article which taught me
 * how to do this without tearing my hair out.
 * link: http://www.codeproject.com/useritems/uoth.asp
 */
class CPNSaveDialog : public CPNFileDialogImpl<CPNSaveDialog>
{
	typedef CPNFileDialogImpl<CPNSaveDialog> baseClass;
	public:
		CPNSaveDialog(LPCTSTR szFilter);

		BEGIN_MSG_MAP (CPNSaveDialog)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			MESSAGE_HANDLER(WM_SIZE, OnSize)

			COMMAND_HANDLER(IDC_PNSAVE_TYPECOMBO, CBN_SELCHANGE, OnComboSelChange) 

			CHAIN_MSG_MAP (baseClass);
		END_MSG_MAP ()

		EPNSaveFormat GetSaveFormat();

	protected:
		LRESULT OnComboSelChange(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnSize (UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled);
		LRESULT OnInitDialog (UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL &bHandled);

		void RepositionControls();
		void RepositionControl(CWindow &wnd, UINT nID, bool fSize);
		void RepositionPlacesBar(CWindow &bottomwnd);

		CComboBox		m_SaveTypeCombo;
		CStatic			m_SaveTypeLabel;
		EPNSaveFormat	m_Format;
};

class CGotoDialog : public CDialogImpl<CGotoDialog>
{
	public:
		enum { IDD = IDD_GOTO };

		BEGIN_MSG_MAP(CGotoDialog)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_ID_HANDLER(IDOK, OnOK)
			COMMAND_ID_HANDLER(IDCANCEL, OnCancel)
		END_MSG_MAP()

		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		
		LRESULT OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		LRESULT OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		int GetLineNo();
	
	protected:
		int lineno;
};

class COptionsDialog;

class COptionsPage
{
	friend class COptionsDialog;

public:
	COptionsPage()
	{
		m_bCreated = false;
		m_bOrphan = false;
	}

	// Methods called by COptionsDialog when dealing with COptionsPage.
	virtual void OnOK(){};
	virtual void OnCancel(){};
	virtual void OnInitialise(){};

	// Return a tree position like "Schemes\AddOn Options"
	virtual LPCTSTR GetTreePosition() = 0;

	virtual HWND CreatePage(HWND hOwner, LPRECT rcPos, HWND hInsertAfter = NULL) = 0;
	virtual void ClosePage() = 0;
	virtual void ShowPage(int showCmd) = 0;

protected:
	bool m_bCreated;
	bool m_bOrphan;
};

template <class T>
class COptionsPageImpl : public CDialogImpl<T>, public COptionsPage
{
	public:
		virtual ~COptionsPageImpl(){}

		virtual HWND CreatePage(HWND hParent, LPRECT rcPos, HWND hInsertAfter = NULL)
		{
			Create(hParent);
			SetWindowPos(hInsertAfter, rcPos, SWP_NOACTIVATE);
			MoveWindow(rcPos);

			return m_hWnd;
		}

		virtual void ClosePage()
		{
			DestroyWindow();
			if(m_bOrphan)
				delete this;
		}

		virtual void ShowPage(int showCmd)
		{
			ShowWindow(showCmd);
		}
};

class COptionsDialog : public CDialogImpl<COptionsDialog>
{
	typedef list<COptionsPage*> PAGEPTRLIST;
	typedef CDialogImpl<COptionsDialog> baseClass;

	public:

		COptionsDialog();

		enum { IDD = IDD_OPTIONS };

		BEGIN_MSG_MAP(COptionsDialog)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_ID_HANDLER(IDOK, OnOK)
			COMMAND_ID_HANDLER(IDCANCEL, OnCancel)
			NOTIFY_ID_HANDLER(IDC_TREE, OnTreeNotify)
		END_MSG_MAP()

		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnTreeNotify(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

		void AddPage(COptionsPage* pPage);

		BOOL EndDialog(int nRetCode);

	protected:
		PAGEPTRLIST		m_Pages;
		COptionsPage*	m_pCurrentPage;
		CTreeViewCtrl	m_tree;

		void InitialisePages();
		void ClosePages();
		void SelectPage(COptionsPage* pPage);
		HTREEITEM FindAtThisLevel(LPCTSTR title, HTREEITEM context);
		HTREEITEM AddTreeEntry(const char* title, HTREEITEM context);
};

#include "include/fontcombo.h"

class COptionsPageStyle : public COptionsPageImpl<COptionsPageStyle>
{
	public:
		BEGIN_MSG_MAP(COptionsPageStyle)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()
		enum { IDD = IDD_PAGE_STYLE };

		virtual void OnOK();
		virtual void OnCancel();
		virtual void OnInitialise();
		virtual LPCTSTR GetTreePosition();
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	protected:
		CFontCombo	m_FontCombo;
		CComboBox	m_SizeCombo;
};

#include "SchemeConfig.h"
#include "pnutils.h"

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

class CKeywordsTabPage : public CPropertyPageImpl<CKeywordsTabPage>
{
	public:
		enum {IDD = IDD_TAB_KEYWORDS};

		BEGIN_MSG_MAP(CKeywordsTabPage)
			CHAIN_MSG_MAP(CPropertyPageImpl<CKeywordsTabPage>)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()
};

#include "Include/ColorButton.h"

class CStylesTabPage : public CPropertyPageImpl<CStylesTabPage>
{
	public:	
		enum {IDD = IDD_TAB_STYLES};

		CStylesTabPage();

		BEGIN_MSG_MAP(CStylesTabPage)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_HANDLER(IDC_STYLE_BOLDCHECK, BN_CLICKED, OnBoldClicked)
			COMMAND_HANDLER(IDC_STYLE_ITALICCHECK, BN_CLICKED, OnItalicClicked)
			COMMAND_HANDLER(IDC_STYLE_UNDERLINECHECK, BN_CLICKED, OnUnderlineClicked)
			COMMAND_HANDLER(IDC_STYLE_FONTCOMBO, CBN_SELCHANGE, OnFontChanged)
			COMMAND_HANDLER(IDC_STYLE_SIZECOMBO, CBN_SELCHANGE, OnSizeChanged)
			NOTIFY_HANDLER(IDC_STYLE_FOREBUTTON, CPN_SELCHANGE, OnForeChanged)
			NOTIFY_HANDLER(IDC_STYLE_BACKBUTTON, CPN_SELCHANGE, OnBackChanged)
			NOTIFY_HANDLER(IDC_STYLES_TREE, TVN_SELCHANGED, OnTreeSelChanged)
			CHAIN_MSG_MAP(CPropertyPageImpl<CStylesTabPage>)
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
		void AddFontSize(int size, LPCTSTR sizestr);
		void SetItem();

	protected:
		StyleDetails	m_Style;
		StyleDetails*	m_pStyle;
		SchemeConfig*	m_pScheme;
		CTreeViewCtrl	m_tree;
		CStyleDisplay	m_sd;

		CFontCombo		m_FontCombo;
		CComboBox		m_SizeCombo;

		CColorButton	m_fore;
		CColorButton	m_back;

		CButton			m_bold;
		CButton			m_italic;
		CButton			m_underline;
		CButton			m_eolfilled;
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

		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		virtual void OnInitialise();
		virtual void OnOK();
		virtual LPCTSTR GetTreePosition();
		LRESULT OnComboChange(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled);

	protected:
		CComboBox			m_combo;
		CContainedPropSheet	m_props;
		SchemeConfigParser* m_pSchemes;
		CStylesTabPage		m_stylestab;
		CKeywordsTabPage	m_keywordstab;
};

#endif