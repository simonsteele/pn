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
			memset(&m_lf, 0, sizeof(LOGFONT));
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

		void SetBold(bool bold)
		{
			m_lf.lfWeight = (bold ? FW_BOLD : FW_NORMAL);
			UpdateFont();
		}

		void SetItalic(bool italic)
		{
			m_lf.lfItalic = italic;
			UpdateFont();
		}

		void SetUnderline(bool underline)
		{
			m_lf.lfUnderline = underline;
			UpdateFont();
		}

		void SetFontName(LPCTSTR fontname)
		{
			_tcscpy(m_lf.lfFaceName, fontname);
			UpdateFont();
		}

		void SetSize(int size, bool bInvalidate = true)
		{
			HDC dc = GetDC();			
			m_lf.lfHeight = -MulDiv(size, GetDeviceCaps(dc, LOGPIXELSY), 72);
			ReleaseDC(dc);

			if(bInvalidate)
				UpdateFont();
		}

		void SetFore(COLORREF fore)
		{
			m_Fore = fore;
			Invalidate();
		}

		void SetBack(COLORREF back)
		{
			m_Back = back;
			Invalidate();
		}

		void UpdateFont()
		{
			if(m_Font)
				delete m_Font;

			m_Font = new CFont;
			m_Font->CreateFontIndirect(&m_lf);

			Invalidate();
		}

		void SetStyle(LPCTSTR fontname, int fontsize, COLORREF fore, COLORREF back, LPCTSTR name, bool bold, bool italic, bool underline)
		{
			m_Name = name;
			m_Fore = fore;
			m_Back = back;

			SetSize(fontsize, false);
			
			m_lf.lfWeight = (bold ? FW_BOLD : FW_NORMAL);
			m_lf.lfUnderline = underline;
			m_lf.lfItalic = italic;
			_tcscpy(m_lf.lfFaceName, fontname);

			UpdateFont();
		}

	protected:
		CString		m_Name;
		LOGFONT		m_lf;
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

#include "Include/ColorButton.h"

class CStylesTabDialog : public CPropertyPageImpl<CStylesTabDialog>
{
	public:	
		enum {IDD = IDD_TAB_STYLES};

		CStylesTabDialog()
		{
			m_pStyle = NULL;
		}

		BEGIN_MSG_MAP(CStylesTabDialog)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			COMMAND_HANDLER(IDC_STYLE_BOLDCHECK, BN_CLICKED, OnBoldClicked)
			COMMAND_HANDLER(IDC_STYLE_ITALICCHECK, BN_CLICKED, OnItalicClicked)
			COMMAND_HANDLER(IDC_STYLE_UNDERLINECHECK, BN_CLICKED, OnUnderlineClicked)
			COMMAND_HANDLER(IDC_STYLE_FONTCOMBO, CBN_SELCHANGE, OnFontChanged)
			COMMAND_HANDLER(IDC_STYLE_SIZECOMBO, CBN_SELCHANGE, OnSizeChanged)
			NOTIFY_HANDLER(IDC_STYLE_FOREBUTTON, CPN_SELCHANGE, OnForeChanged)
			NOTIFY_HANDLER(IDC_STYLE_BACKBUTTON, CPN_SELCHANGE, OnBackChanged)
			NOTIFY_HANDLER(IDC_STYLES_TREE, TVN_SELCHANGED, OnTreeSelChanged)
			CHAIN_MSG_MAP(CPropertyPageImpl<CStylesTabDialog>)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()

		void SetScheme(SchemeConfig* pScheme)
		{
			m_pScheme = pScheme;

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

		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
		{
			CRect rc;
			
			m_tree.Attach(GetDlgItem(IDC_STYLES_TREE));

			CWindow placeholder(GetDlgItem(IDC_STYLE_EXAMPLE));
			placeholder.GetWindowRect(rc);
			ScreenToClient(rc);
			m_sd.Create(m_hWnd, rc, _T("Style Display"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS);

			//IDC_FORE_PLACEHOLDER, IDC_BACK_PLACEHOLDER
			//IDC_STYLE_FONTCOMBO, IDC_STYLE_SIZECOMBO

			m_FontCombo.SubclassWindow(GetDlgItem(IDC_STYLE_FONTCOMBO));
			m_SizeCombo.Attach(GetDlgItem(IDC_STYLE_SIZECOMBO));

			m_fore.SubclassWindow(GetDlgItem(IDC_STYLE_FOREBUTTON));
			m_back.SubclassWindow(GetDlgItem(IDC_STYLE_BACKBUTTON));
			
			AddFontSize(6, _T("6"));
			AddFontSize(8, _T("8"));
			AddFontSize(10, _T("10"));
			AddFontSize(12, _T("12"));
			AddFontSize(14, _T("14"));
			AddFontSize(16, _T("16"));
			AddFontSize(18, _T("18"));

			m_bold.Attach(GetDlgItem(IDC_STYLE_BOLDCHECK));
			m_italic.Attach(GetDlgItem(IDC_STYLE_ITALICCHECK));
			m_underline.Attach(GetDlgItem(IDC_STYLE_UNDERLINECHECK));
			m_eolfilled.Attach(GetDlgItem(IDC_STYLE_EOLFILLEDCHECK));

			return 0;
		}

		LRESULT OnForeChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
		{
			NMCOLORBUTTON* pN = reinterpret_cast<NMCOLORBUTTON*>(pnmh);
			m_sd.SetFore(pN->clr);
			m_Style.ForeColor = pN->clr;
			return 0;
		}

		LRESULT OnBackChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
		{
			NMCOLORBUTTON* pN = reinterpret_cast<NMCOLORBUTTON*>(pnmh);
			m_sd.SetBack(pN->clr);
			m_Style.BackColor = pN->clr;
			return 0;
		}

		LRESULT OnFontChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
		{			
			if(m_pStyle)
			{
				int i = m_FontCombo.GetCurSel();
				CString str;
				m_FontCombo.GetLBText(i, str);
				m_sd.SetFontName(str);
				m_Style.FontName = (LPCTSTR)str;
			}
			return 0;
		}

		LRESULT OnSizeChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
		{
			if(m_pStyle)
			{
				int i = m_SizeCombo.GetCurSel();
				i = m_SizeCombo.GetItemData(i);
				m_sd.SetSize(i);
				m_Style.FontSize = i;
			}
			return 0;
		}

		LRESULT OnTreeSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
		{
			SetItem();

			HTREEITEM item = m_tree.GetSelectedItem();

			if(item)
			{
				if(m_tree.GetChildItem(item) == NULL)
				{
					StyleDetails* pS = reinterpret_cast<StyleDetails*>(m_tree.GetItemData(item));
					if(pS)
					{
						StyleDetails* existing = m_pScheme->m_customs.GetStyle(pS->Key);
						if(existing)
							m_Style = *existing;
						else
                            m_Style = *pS;
						
						m_pStyle = pS;
						m_sd.SetStyle(m_Style.FontName.c_str(), m_Style.FontSize, m_Style.ForeColor, m_Style.BackColor, m_Style.name.c_str(), m_Style.Bold, m_Style.Italic, m_Style.Underline);
						m_bold.SetCheck(m_Style.Bold ? BST_CHECKED : BST_UNCHECKED);
						m_italic.SetCheck(m_Style.Italic ? BST_CHECKED : BST_UNCHECKED);
						m_underline.SetCheck(m_Style.Underline ? BST_CHECKED : BST_UNCHECKED);
						m_eolfilled.SetCheck(m_Style.EOLFilled ? BST_CHECKED : BST_UNCHECKED);
						m_fore.SetColor(m_Style.ForeColor);
						m_back.SetColor(m_Style.BackColor);

						m_FontCombo.SelectString(0, m_Style.FontName.c_str());
						TCHAR buf[10];
						_itot(m_Style.FontSize, buf, 10);
						if (m_SizeCombo.SelectString(0, buf) == CB_ERR)
						{
							int idx = m_SizeCombo.AddString(buf);
							m_SizeCombo.SetCurSel(idx);
						}
					}
				}
				else
				{
					///@todo Disable everything
				}
			}
			else
				m_pStyle = NULL;

			return 0;
		}

		LRESULT OnBoldClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
		{
			if(m_pStyle)
			{
				bool bC = m_bold.GetCheck() == BST_CHECKED;
				m_Style.Bold = bC;
				m_sd.SetBold(bC);
			}

			return 0;
		}

		LRESULT OnItalicClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
		{
			if(m_pStyle)
			{
				bool bC = m_italic.GetCheck() == BST_CHECKED;
				m_Style.Italic = bC;
				m_sd.SetItalic(bC);
			}
			return 0;
		}

		LRESULT OnUnderlineClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
		{
			if(m_pStyle)
			{
				bool bC = m_underline.GetCheck() == BST_CHECKED;
				m_Style.Underline = bC;
				m_sd.SetUnderline(bC);
			}
			return 0;
		}

		LRESULT OnEOLFilledClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
		{
			if(m_pStyle)
			{
				bool bC = m_eolfilled.GetCheck() == BST_CHECKED;
				m_Style.EOLFilled = bC;
				//m_sd.SetEOLFilled(bC);
			}
			return 0;
		}

		void AddFontSize(int size, LPCTSTR sizestr)
		{
			int idx = m_SizeCombo.AddString(sizestr);
			m_SizeCombo.SetItemData(idx, size);
		}

		void SetItem()
		{
			if(m_pStyle)
			{
				int mask = 0;

				if(m_Style != *m_pStyle)
				{
					StyleDetails* existing = m_pScheme->m_customs.GetStyle(m_Style.Key);
					if(existing)
					{
						*existing = m_Style;
					}
					else
					{
						existing = new StyleDetails;
						*existing = m_Style;
						m_pScheme->m_customs.AddStyle(existing);
					}
				}
			}
		}

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