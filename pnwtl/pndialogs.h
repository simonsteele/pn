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
#include "pnutils.h"

class CStyleDisplay : public CWindowImpl<CStyleDisplay>
{
	public:
		CStyleDisplay()
		{
			m_Font = NULL;
		}

		~CStyleDisplay()
		{
			if(m_Font)
				delete m_Font;
		}

		BEGIN_MSG_MAP(CStyleDisplay)
			MESSAGE_HANDLER(WM_PAINT, OnPaint)
			MESSAGE_HANDLER(WM_ERASEBKGND, OnEraseBkgnd)
		END_MSG_MAP()

		void SetStyle(LPCTSTR fontname, int fontsize, COLORREF fore, COLORREF back, LPCTSTR name, bool bold, bool italic, bool underline)
		{
			m_Name = name;
			m_Fore = fore;
			m_Back = back;

			if(m_Font)
				delete m_Font;

			HDC dc = GetDC();

			LOGFONT lf;
			memset(&lf, 0, sizeof(LOGFONT));
			lf.lfHeight = -MulDiv(fontsize, GetDeviceCaps(dc, LOGPIXELSY), 72);
			lf.lfWeight = (bold ? FW_BOLD : FW_NORMAL);
			if(underline)
				lf.lfUnderline = TRUE;
			if(italic)
				lf.lfItalic = TRUE;
			_tcscpy(lf.lfFaceName, fontname);

			ReleaseDC(dc);
			
			m_Font = new CFont;
			m_Font->CreateFontIndirect(&lf);

			Invalidate();
		}

	protected:
		CString		m_Name;
		CFont*		m_Font;
		COLORREF	m_Fore;
		COLORREF	m_Back;

		LRESULT OnPaint(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
		{
			PAINTSTRUCT ps;
			BeginPaint(&ps);

			CDC dc(ps.hdc);
			
			CRect rc;
			GetClientRect(rc);

			HBRUSH light = ::CreateSolidBrush(::GetSysColor(COLOR_3DSHADOW));

			dc.FillRect(rc, (HBRUSH)::GetStockObject(WHITE_BRUSH));
			dc.FrameRect(rc, light);

			if(m_Font)
			{
				HFONT hOldFont = dc.SelectFont(m_Font->m_hFont);
				dc.SetBkColor(m_Back);
				dc.SetTextColor(m_Fore);
				dc.DrawText(m_Name, m_Name.GetLength(), rc, DT_CENTER | DT_VCENTER | DT_SINGLELINE);
				dc.SelectFont(hOldFont);
			}

			::DeleteObject(light);
			
			EndPaint(&ps);
			return 0;
		}

		LRESULT OnEraseBkgnd(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
		{
			return 1;
		}
};

class CStylesTabDialog : public CPropertyPageImpl<CStylesTabDialog>
{
	public:	
		enum {IDD = IDD_TAB_STYLES};

		BEGIN_MSG_MAP(CStylesTabDialog)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			NOTIFY_HANDLER(IDC_STYLES_TREE, TVN_SELCHANGED, OnTreeSelChanged)
			CHAIN_MSG_MAP(CPropertyPageImpl<CStylesTabDialog>)
		END_MSG_MAP()

		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
		{
			CRect rc;
			
			m_tree.Attach(GetDlgItem(IDC_STYLES_TREE));

			CWindow placeholder(GetDlgItem(IDC_STYLE_EXAMPLE));
			placeholder.GetWindowRect(rc);
			ScreenToClient(rc);
			m_sd.Create(m_hWnd, rc, _T("Style Display"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS);

			return 0;
		}

		LRESULT OnTreeSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
		{
			HTREEITEM item = m_tree.GetSelectedItem();

			if(item)
			{
				if(m_tree.GetParentItem(item) == NULL)
				{
					StyleDetails* pStyle = reinterpret_cast<StyleDetails*>(m_tree.GetItemData(item));
					if(pStyle)
					{
						m_sd.SetStyle(pStyle->FontName.c_str(), pStyle->FontSize, pStyle->ForeColor, pStyle->BackColor, pStyle->name.c_str(), pStyle->Bold, pStyle->Italic, pStyle->Underline);
						CheckDlgButton(IDC_STYLE_BOLDCHECK, pStyle->Bold ? BST_CHECKED : BST_UNCHECKED);
						CheckDlgButton(IDC_STYLE_ITALICCHECK, pStyle->Italic ? BST_CHECKED : BST_UNCHECKED);
						CheckDlgButton(IDC_STYLE_UNDERLINECHECK, pStyle->Underline ? BST_CHECKED : BST_UNCHECKED);
						CheckDlgButton(IDC_STYLE_EOLFILLEDCHECK, pStyle->EOLFilled ? BST_CHECKED : BST_UNCHECKED);
					}
				}
			}

			return 0;
		}

		void SetScheme(SchemeConfig* pScheme)
		{
			m_tree.DeleteAllItems();

			CustomStyleCollection* pColl = static_cast<CustomStyleCollection*>(pScheme);
			HTREEITEM insertunder = TVI_ROOT;
			
			while(pColl)
			{
				for(SL_IT i = pColl->m_Styles.begin(); i != pColl->m_Styles.end(); ++i)
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
		CStyleDisplay	m_sd;
};

class COptionsPageSchemes : public COptionsPageImpl<COptionsPageSchemes>
{
	public:
		BEGIN_MSG_MAP(COptionsPageSchemes)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_HANDLER(IDC_SCHEMECOMBO, CBN_SELCHANGE, OnComboChange)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()
		enum {IDD = IDD_PAGE_SCHEMES};

		COptionsPageSchemes(SchemeConfigParser* pSchemes) : COptionsPageImpl<COptionsPageSchemes>()
		{
			m_pSchemes = pSchemes;
		}

		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
		{
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

			
			CRect rcPH;
			GetDlgItem(IDC_PS_PLACEHOLDER).GetWindowRect(rcPH);
			ScreenToClient(rcPH);
			m_stylestab.SetTitle(_T("Styles"));
			m_props.AddPage(m_stylestab);
			m_props.Create(m_hWnd, 0, rcPH);

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

	protected:
		CComboBox			m_combo;
		CContainedPropSheet	m_props;
		SchemeConfigParser* m_pSchemes;
		CStylesTabDialog	m_stylestab;
};

#endif