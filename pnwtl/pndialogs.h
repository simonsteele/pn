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

		virtual void OnOK()
		{
			::OutputDebugString(_T("COptionsPageStyle::OnOK\n"));
		}

		virtual void OnCancel()
		{
			::OutputDebugString(_T("COptionsPageStyle::OnCancel\n"));
		}
		
		virtual void OnInitialise()
		{
			::OutputDebugString(_T("COptionsPageStyle::OnInitialise\n"));
			m_FontCombo.SelectString(0, _T("Lucida Console"));
			m_SizeCombo.SelectString(0, _T("10"));
		}

		virtual LPCTSTR GetTreePosition()
		{
			return _T("Style");
		}

		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
		{
			m_FontCombo.SubclassWindow(GetDlgItem(IDC_FONT_COMBO));
			m_SizeCombo = GetDlgItem(IDC_FONTSIZE_COMBO);
			
			m_SizeCombo.AddString(_T("6"));
			m_SizeCombo.AddString(_T("8"));
			m_SizeCombo.AddString(_T("10"));
			m_SizeCombo.AddString(_T("12"));
			m_SizeCombo.AddString(_T("14"));
			m_SizeCombo.AddString(_T("16"));
			m_SizeCombo.AddString(_T("18"));
			

			return 0;
		}

	protected:
		CFontCombo	m_FontCombo;
		CComboBox	m_SizeCombo;
};

#include "SchemeConfig.h"

template <class T>
class CTabbedDialogPageImpl : public CDialogImpl<T>
{
	BEGIN_MSG_MAP(CTabbedDialogPageImpl)
		MESSAGE_HANDLER(WM_CTLCOLORDLG, OnCtlColor)
	END_MSG_MAP()

	LRESULT OnCtlColor(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
	{
		if((HWND)lParam == m_hWnd)
		{
			return reinterpret_cast<INT_PTR>(::GetStockObject(NULL_BRUSH));

		}
		return 0;
	}
};

class CStylesTabDialog : public CTabbedDialogPageImpl<CStylesTabDialog>
{
	public:	
		enum {IDD = IDD_TAB_STYLES};

		BEGIN_MSG_MAP(CStylesTabDialog)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			CHAIN_MSG_MAP(CTabbedDialogPageImpl<CStylesTabDialog>)
		END_MSG_MAP()

		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
		{
			CRect rc;
			
			m_tree.Attach(GetDlgItem(IDC_STYLES_TREE));

			return 0;
		}

		void SetScheme(SchemeConfig* pScheme)
		{
			m_tree.DeleteAllItems();

			CustomStyleCollection* pColl = static_cast<CustomStyleCollection*>(pScheme);
			HTREEITEM insertunder = TVI_ROOT;
			
			while(pColl)
			{
				for(SL_IT i = pScheme->m_Styles.begin(); i != pScheme->m_Styles.end(); ++i)
				{
					HTREEITEM hi = m_tree.InsertItem((*i)->name.c_str(), insertunder, TVI_LAST);
					m_tree.SetItemData(hi, reinterpret_cast<DWORD_PTR>(*i));
				}
				if(insertunder != TVI_ROOT)
					m_tree.Expand(insertunder, TVE_EXPAND);

				pColl = pColl->GetNext();
				if(pColl)
				{
					insertunder = m_tree.InsertItem(pColl->GetName(), NULL, NULL);
					m_tree.SetItemData(insertunder, reinterpret_cast<DWORD_PTR>(pColl));
				}
			}

			m_tree.EnsureVisible(m_tree.GetRootItem());
		}

	protected:
		CTreeViewCtrl	m_tree;
};

class COptionsPageSchemes : public COptionsPageImpl<COptionsPageSchemes>
{
	public:
		BEGIN_MSG_MAP(COptionsPageSchemes)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_HANDLER(IDC_SCHEMECOMBO, CBN_SELCHANGE, OnComboChange)
			NOTIFY_HANDLER(IDC_TAB, TCN_SELCHANGE, OnTabChange)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()
		enum {IDD = IDD_PAGE_SCHEMES};

		COptionsPageSchemes(SchemeConfigParser* pSchemes) : COptionsPageImpl<COptionsPageSchemes>()
		{
			m_pSchemes = pSchemes;
		}

		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
		{
			m_tabs.Attach(GetDlgItem(IDC_TAB));

			TCITEM tci;
			tci.mask = TCIF_TEXT | TCIF_IMAGE;

			tci.pszText = _T("Styles");
			tci.iImage = 0;
			m_tabs.InsertItem(0, &tci);
			
			tci.pszText = _T("Keywords");
			tci.iImage = 1;
			m_tabs.InsertItem(1, &tci);

			CWindow label;
			CSize s;
			CRect rc;

			label.Attach(GetDlgItem(IDC_SCHEMELABEL));
			
			CDC dc(label.GetDC());
			dc.GetTextExtent(_T("Scheme:"), 7, &s);
			
			label.GetWindowRect(rc);
			ScreenToClient(rc);
			rc.right = rc.left + s.cx;
			label.SetWindowPos(HWND_TOP, &rc, 0);

			CRect rcCombo;

			m_combo.Attach(GetDlgItem(IDC_SCHEMECOMBO));

			m_combo.GetWindowRect(rcCombo);
			ScreenToClient(rcCombo);
			rcCombo.left = rc.right + 5;
			m_combo.SetWindowPos(HWND_TOP, &rcCombo, 0);

			m_stylestab.Create(m_tabs.m_hWnd);
			m_tabs.GetWindowRect(rc);
			m_tabs.ScreenToClient(rc);
			m_tabs.AdjustRect(FALSE, rc);
			m_stylestab.SetWindowPos(HWND_TOP, rc, SWP_SHOWWINDOW);
			m_stylestab.ShowWindow(SW_SHOW);
			return 0;
		}

		virtual void OnInitialise()
		{
			for(SCF_IT i = m_pSchemes->GetSchemes().begin(); i != m_pSchemes->GetSchemes().end(); ++i)
			{
				int index = m_combo.AddString((*i)->m_Title);
				m_combo.SetItemDataPtr(index, (*i));
			}
		}

		virtual LPCTSTR GetTreePosition()
		{
			return _T("Style\\Schemes");
		}

		LRESULT OnComboChange(WORD wNotifyCode, WORD wID, HWND hWndCtl, BOOL& bHandled)
		{
			int i = m_combo.GetCurSel();
			SchemeConfig* pScheme = static_cast<SchemeConfig*>(m_combo.GetItemDataPtr(i));
			m_stylestab.SetScheme(pScheme);
			return 0;
		}

		LRESULT OnTabChange(int idCtrl, LPNMHDR pnmh, BOOL& bHandled)
		{
			int iTab = m_tabs.GetCurSel();
			if(iTab == 0)
			{
				m_stylestab.ShowWindow(SW_SHOW);
			}
			else
			{
				m_stylestab.ShowWindow(SW_HIDE);
			}
			return 0;
		}

	protected:
		CComboBox			m_combo;
		CTabCtrlT<CWindow>	m_tabs;
		SchemeConfigParser* m_pSchemes;
		CStylesTabDialog	m_stylestab;
};

#endif