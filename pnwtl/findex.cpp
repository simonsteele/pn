#include "stdafx.h"
#include "resource.h"
#include "CustomAutoComplete.h"
#include "include/accombo.h"
#include "findex.h"

class ATL_NO_VTABLE ClientRect : public CRect
{
public:
	ClientRect(CWindow& wnd, CWindow& owner)
	{
		set(wnd, owner);
	}

	void set(CWindow& wnd, CWindow& owner)
	{
		wnd.GetWindowRect(this);
		owner.ScreenToClient(this);
	}
};

CFindExDialog::CFindExDialog()
{
	m_Direction = 1;
	m_type = eftFind;
}

// A derived class might not need to override this although they can.
// (but they will probably need to specialize SetTabAreaHeight)
int CFindExDialog::calcTabAreaHeight(void)
{
	// Dynamically figure out a reasonable tab area height
	// based on the tab's font metrics

	const int nNominalHeight = 28;
	const int nNominalFontLogicalUnits = 11;	// 8 point Tahoma with 96 DPI

	// Initialize nFontLogicalUnits to the typical case
	// appropriate for CDotNetTabCtrl
	LOGFONT lfIcon = { 0 };
	::SystemParametersInfo(SPI_GETICONTITLELOGFONT, sizeof(lfIcon), &lfIcon, 0);
	int nFontLogicalUnits = -lfIcon.lfHeight;

	// Use the actual font of the tab control
	if(m_tabControl.IsWindow())
	{
		HFONT hFont = m_tabControl.GetFont();
		if(hFont != NULL)
		{
			CDC dc = m_tabControl.GetDC();
			CFontHandle hFontOld = dc.SelectFont(hFont);
			TEXTMETRIC tm = {0};
			dc.GetTextMetrics(&tm);
			nFontLogicalUnits = tm.tmAscent;
			dc.SelectFont(hFontOld);
		}
	}

	int nNewTabAreaHeight = nNominalHeight + ( ::MulDiv(nNominalHeight, nFontLogicalUnits, nNominalFontLogicalUnits) - nNominalHeight ) / 2;
	return nNewTabAreaHeight;
}

LRESULT CFindExDialog::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CSize size = GetGUIFontSize();

	ClientRect rc(GetDlgItem(IDC_FINDTEXT_DUMMY), *this);
	rc.bottom = rc.top + (size.cy * 10);

	m_FindTextCombo.Create(m_hWnd, rc, _T("FINDTEXTCOMBO"), CBS_DROPDOWN | CBS_AUTOHSCROLL | WS_CHILD | WS_VSCROLL | WS_VISIBLE | WS_TABSTOP, 0, IDC_FINDTEXT_COMBO,
		_T("Software\\Echo Software\\PN2\\AutoComplete\\Find"), IDC_FINDTEXT_DUMMY);

	rc.set(GetDlgItem(IDC_REPLACETEXT_DUMMY), *this);
	rc.bottom = rc.top + (size.cy * 10);

	m_ReplaceTextCombo.Create(m_hWnd, rc, _T("REPLACETEXTCOMBO"), CBS_DROPDOWN | CBS_AUTOHSCROLL | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 0, IDC_REPLACETEXT_COMBO,
		_T("Software\\Echo Software\\PN2\\AutoComplete\\Replace"), IDC_REPLACETEXT_DUMMY);

	// Store the position of the second combo.
	m_group2Top = rc.top;

	rc.set(GetDlgItem(IDC_FINDWHERE_DUMMY), *this);
	rc.bottom = rc.top + (size.cy * 10);

	m_FindWhereCombo.Create(m_hWnd, rc, _T("FINDWHERECOMBO"), CBS_DROPDOWN | CBS_AUTOHSCROLL | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 0, IDC_REPLACETEXT_COMBO,
		IDC_FINDWHERE_DUMMY);

	rc.set(GetDlgItem(IDC_FINDTYPE_DUMMY), *this);
	rc.bottom = rc.top + (size.cy * 10);

	m_FindTypeCombo.Create(m_hWnd, rc, _T("FINDTYPECOMBO"), CBS_DROPDOWN | CBS_AUTOHSCROLL | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 0);
	m_FindTypeCombo.SetFont(GetFont());

	m_ReHelperBtn.SubclassWindow(GetDlgItem(IDC_REHELPER_BUTTON));
	m_ReHelperBtn2.SubclassWindow(GetDlgItem(IDC_RHELPER_BUTTON));

	UIEnable(IDC_REHELPER_BUTTON, true);

	UIAddChildWindowContainer(m_hWnd);

	CenterWindow(GetParent());

	// Set up the tab control images.
	m_imageList.Create(16, 16, ILC_COLOR32 | ILC_MASK, 4, 4);
	
	HBITMAP hBitmap = (HBITMAP)::LoadImage(
		_Module.m_hInst,
		MAKEINTRESOURCE(IDB_FINDTOOLBAR),
		IMAGE_BITMAP, 0, 0, LR_SHARED);

	m_imageList.Add(hBitmap, RGB(255,0,255));
	m_tabControl.SetImageList(m_imageList.m_hImageList);
	
	// Position and create the tab control.
	DWORD dwStyle = WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_CLIPCHILDREN | CTCS_HOTTRACK | CTCS_FLATEDGE;

	CRect rcTabs;
	GetClientRect(rcTabs);
	rcTabs.top = 2;
	rcTabs.left = 2;
	rcTabs.bottom = calcTabAreaHeight();

	m_tabControl.Create(m_hWnd, rcTabs, NULL, dwStyle, 0, IDC_FINDEX_TABS);

	// Move the line that goes beneath the tab control.
	CStatic s(GetDlgItem(IDC_FINDEX_LINE));
	rc.set(s, *this);
	int height = rc.Height();
	rc.top = rcTabs.bottom + 1;
	s.SetWindowPos(HWND_TOP, rc, SWP_NOZORDER | SWP_NOSIZE);

	// Setup find where autocomplete.
	//m_FindWhereAC

	ClientRect rcCombo(m_FindWhereCombo, *this);

	m_comboDistance = rcCombo.top - m_group2Top;
	m_group3Bottom = rcCombo.bottom;

	rcCombo.set(m_ReplaceTextCombo, *this);
	
	m_group2Bottom = rcCombo.bottom;
	m_group1Bottom = m_group2Bottom - m_comboDistance;

	setupTabs();

	updateLayout();

	DlgResize_Init();

	return TRUE;
}

int CFindExDialog::addTab(LPCTSTR name, int iconIndex)
{
	CDotNetButtonTabCtrl<>::TItem* pItem = m_tabControl.CreateNewItem();
	if(pItem)
	{
		pItem->SetText(name);
		pItem->SetImageIndex(iconIndex);

		// The tab control takes ownership of the new item
		return m_tabControl.InsertItem(m_tabControl.GetItemCount(), pItem);
	}

	return -1;
}

void CFindExDialog::setupTabs()
{
	addTab(_T("Find"), 2);
	addTab(_T("Replace"), 3);
	addTab(_T("Find in Files"), 0);
}

#define SWP_SIMPLEMOVE (SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE)

int CFindExDialog::positionChecks(int top)
{
	UINT checkboxIDs [] = {
		IDC_MATCHWHOLE_CHECK, 
		IDC_MATCHCASE_CHECK, 
		IDC_REGEXP_CHECK, 
		IDC_SEARCHALL_CHECK, 
		IDC_BACKSLASHES_CHECK
	};
	
	int nCheckboxes = 5;

	CRect rc;
	int chkDist;
	CButton cb1(GetDlgItem(checkboxIDs[0]));
	cb1.GetWindowRect(rc);
	ScreenToClient(rc);
	chkDist = rc.top;
	CButton cb2(GetDlgItem(checkboxIDs[1]));
	cb2.GetWindowRect(rc);
	ScreenToClient(rc);
	chkDist = rc.top - chkDist;

	rc.top = top;

	for(int i = 0; i < nCheckboxes; i++)
	{
		CButton cb(GetDlgItem(checkboxIDs[i]));
		cb.SetWindowPos(HWND_TOP, rc, SWP_SIMPLEMOVE);
		rc.top += chkDist;
	}

	return rc.top;
}

void CFindExDialog::updateLayout()
{
	int restTop;
	ClientRect rcLastCheck(GetDlgItem(IDC_BACKSLASHES_CHECK), *this);
	int oldBottom = rcLastCheck.bottom;

	switch(m_type)
	{
	case eftFind:
		{
			CStatic(GetDlgItem(IDC_REPLACETEXT_LABEL)).ShowWindow(SW_HIDE);
			m_ReplaceTextCombo.ShowWindow(SW_HIDE);
			CStatic(GetDlgItem(IDC_FINDWHERE_LABEL)).ShowWindow(SW_HIDE);
			m_FindWhereCombo.ShowWindow(SW_HIDE);
			CStatic(GetDlgItem(IDC_FINDTYPE_LABEL)).ShowWindow(SW_HIDE);
			m_FindTypeCombo.ShowWindow(SW_HIDE);
			m_ReHelperBtn2.ShowWindow(SW_HIDE);
			
			restTop = m_group1Bottom + 12;
		}
		break;
	}

	int bottomChecks = positionChecks(restTop);

	CStatic dirgroup(GetDlgItem(IDC_FINDEX_DIRGROUP));
	ClientRect rc(dirgroup, *this);
	int dy = rc.top - (restTop);
	rc.top = restTop;
	dirgroup.SetWindowPos(HWND_TOP, rc, SWP_SIMPLEMOVE);
	
	CButton up(GetDlgItem(IDC_UP_RADIO));
	rc.set(up, *this);
	rc.top -= dy;
	up.SetWindowPos(HWND_TOP, rc, SWP_SIMPLEMOVE);
	
	CButton down(GetDlgItem(IDC_DOWN_RADIO));
	rc.set(down, *this);
	rc.top -= dy;
	down.SetWindowPos(HWND_TOP, rc, SWP_SIMPLEMOVE);

	//int height = bottomChecks + gap;
	CRect rcMe;
	GetWindowRect(rcMe);
	rcMe.bottom -= (oldBottom - bottomChecks);
	SetWindowPos(HWND_TOP, rcMe, SWP_NOZORDER | SWP_NOMOVE | SWP_NOACTIVATE);
}