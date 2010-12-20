/**
 * @file FontCombo.h
 * @author Girish Bharadwaj <mailto:Girish_Bharadwaj@smtpnotes.pictel.com>
 * @author Norm Almond <mailto:nalmond@hotmail.com> 
 * @author Konstantin Boukreev <mailto:konstantin@mail.primorye.ru>
 * @author Simon Steele <mailto:ss a.t. pnotepad.org>
 *
 * This class is mostly an exact copy from kFontCombo by Konstantin Boukreev, 
 * Norm Almond and Girish Bharadwaj. The reasons for the change are
 * to remove the dependence on the flat combo box class and to simplify.
 *
 * CodeGuru Articles:	
 *		- http://codeguru.earthweb.com/combobox/font_selection_combo.shtml
 *		- http://codeguru.earthweb.com/combobox/fontcombo.shtml
 * CodeProject Article:
 *		- http://www.codeproject.com/wtl/kpad.asp
 */

#ifndef fontcombo_h__included
#define fontcombo_h__included

#define FIXEDPITCH_FONTTYPE 0x0008

class CFontCombo : public CWindowImpl<CFontCombo, CComboBox, CControlWinTraits>
{
	typedef CWindowImpl<CFontCombo, CComboBox, CControlWinTraits> baseClass;
public:	
	CFontCombo() 
	{ 
		m_cyItem = ITEM_DEFAULT_HEIGHT; 
	}

	BEGIN_MSG_MAP(CFontCombo)
		MESSAGE_HANDLER(WM_CREATE, OnCreate);
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy);
        MESSAGE_HANDLER(WM_SETFONT, OnSetFont);
		MESSAGE_HANDLER(OCM_DRAWITEM, OnDrawItem);
		MESSAGE_HANDLER(OCM_MEASUREITEM, OnMeasureItem);
	END_MSG_MAP()

	HWND CreateEx(HWND hWnd, RECT& rc, int cyItem = -1, int ID = 0)
	{
		m_cyItem = cyItem;
		return Create(hWnd, rc, 0, WS_CHILD|WS_VSCROLL|WS_TABSTOP|WS_VISIBLE|CBS_DROPDOWN|CBS_OWNERDRAWFIXED|CBS_HASSTRINGS | CBS_SORT,
				0, ID, 0);		
	}

	BOOL SubclassWindow (HWND hWnd)
	{
		baseClass::SubclassWindow (hWnd);
		Init();
		return TRUE;
	}

	const TCHAR* GetSelFontName()
	{
		static TCHAR szFaceName[LF_FACESIZE];		
		GetLBText(GetCurSel(), szFaceName);
		return szFaceName;
	}

protected:
	CImageList m_img;
    CFontHandle m_fontNormal;
    CFont m_fontBold;
	int m_cyItem;
	
	enum 
	{
		ITEM_DEFAULT_HEIGHT = 15,
			GLYPH_WIDTH = 15
	};

	LRESULT OnCreate(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		Init();

		return 0;
	}
	
	LRESULT OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		return 0;
	}

	LRESULT OnDrawItem(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
	{
		DRAWITEMSTRUCT *lpDIS = reinterpret_cast<DRAWITEMSTRUCT *>(lParam);

		ATLASSERT(lpDIS->CtlType == ODT_COMBOBOX); 
	
		CDCHandle dc = lpDIS->hDC;		
		RECT rc = lpDIS->rcItem;
	
		if (lpDIS->itemState & ODS_FOCUS)
			dc.DrawFocusRect(&rc);
			
		int nIndexDC = dc.SaveDC();

		CBrush br;
		
		if (lpDIS->itemState & ODS_SELECTED)
		{
			br.CreateSolidBrush(::GetSysColor(COLOR_HIGHLIGHT));
			dc.SetTextColor(::GetSysColor(COLOR_HIGHLIGHTTEXT));
		}
		else
		{
			br.CreateSolidBrush(dc.GetBkColor());
		}

		dc.SetBkMode(TRANSPARENT);
		dc.FillRect(&rc, br);
		
		// ss 02/12/2002
		if(lpDIS->itemID != -1)
		{
			DWORD dwData = GetItemData(lpDIS->itemID);

			int nLen = GetLBTextLen(lpDIS->itemID);
			TCHAR* psFont = (TCHAR *)_alloca(sizeof TCHAR * (nLen + 1));
			GetLBText(lpDIS->itemID, psFont);
			
			SIZE sz;

			if (dwData & TRUETYPE_FONTTYPE)
			{
				m_img.GetIconSize(sz);
				m_img.Draw(dc, 0, rc.left + 2, rc.top + ((rc.bottom - rc.top - sz.cy) / 2) ,ILD_TRANSPARENT);
			}

            if (dwData & FIXEDPITCH_FONTTYPE)
            {
                dc.SelectFont(m_fontBold);
            }
		
			rc.left += GLYPH_WIDTH + 2;
			dc.GetTextExtent(psFont, nLen, &sz);
			dc.TextOut(rc.left, rc.top + ((rc.bottom - rc.top - sz.cy) / 2), psFont);
		}

		dc.RestoreDC(nIndexDC);
		return 0;
	}

	LRESULT OnMeasureItem(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
	{
		MEASUREITEMSTRUCT* pmis = reinterpret_cast<MEASUREITEMSTRUCT*>(lParam);
		ATLASSERT(pmis->CtlType == ODT_COMBOBOX);

		if (m_cyItem == -1)
		{
			// calculate height
			CClientDC dc(m_hWnd);
			HFONT hFontDefault = dc.SelectFont(m_fontNormal); //GetFont()
			TEXTMETRIC tm;
			dc.GetTextMetrics(&tm); 

			m_cyItem = tm.tmHeight + tm.tmInternalLeading;

			dc.SelectFont(hFontDefault); // reselect the default font
		}

		pmis->itemHeight = m_cyItem;
		return TRUE;			
	}

	LRESULT OnSetFont(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
        bHandled = FALSE;

        if(!m_fontNormal.IsNull())
            m_fontNormal.DeleteObject();

        if(!m_fontBold.IsNull())
            m_fontBold.DeleteObject();

        m_fontNormal = (HFONT)wParam;
        CLogFont lf;
        m_fontNormal.GetLogFont(&lf);
        lf.SetBold();

        m_fontBold.CreateFontIndirect(&lf);

		return 0;
	}
	
	void Init ()
	{		
		HFONT hFont = ((HFONT)GetStockObject( DEFAULT_GUI_FONT ));
		SetFont(hFont);

		m_img.CreateFromImage(IDB_TRUETYPE_FONTTYPE, GLYPH_WIDTH, 1, RGB(255,255,255), IMAGE_BITMAP);
		ATLASSERT(m_img.GetImageCount() == 1);
		CClientDC dc(m_hWnd);		

		EnumFonts (dc, 0,(FONTENUMPROC) EnumFontProc,(LPARAM)this); //Enumerate font
	}

	static BOOL CALLBACK EnumFontProc (LPLOGFONT lplf, LPTEXTMETRIC /*lptm*/, DWORD dwType, LPARAM lpData)	
	{	
		CFontCombo *pThis = reinterpret_cast<CFontCombo*>(lpData);
		int index = pThis->AddString(lplf->lfFaceName);

        if(lplf->lfPitchAndFamily & FIXED_PITCH)
        {
            // fixed pitch 
            dwType |= FIXEDPITCH_FONTTYPE;
        }
		pThis->SetItemData (index, dwType);
		return TRUE;
	}

};

#endif