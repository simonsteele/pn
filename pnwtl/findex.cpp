#include "stdafx.h"
#include "resource.h"
#include "CustomAutoComplete.h"
#include "include/accombo.h"
#include "findex.h"

#define SWP_SIMPLEMOVE (SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE)

const UINT CHECKBOX_CONTROL_IDS [] = {
	IDC_MATCHWHOLE_CHECK, 
	IDC_MATCHCASE_CHECK, 
	IDC_REGEXP_CHECK, 
	IDC_BACKSLASHES_CHECK,
	IDC_SEARCHUP_CHECK,
	IDC_SUBDIRS_CHECK
};

const UINT RADIO_CONTROL_IDS [] = {
	IDC_CURRENTDOC_RADIO,
	IDC_ALLOPEN_RADIO,
	IDC_CURRENTPROJ_RADIO,
	IDC_INSELECTION_RADIO
};

const int CHECKBOX_CONTROL_COUNT = 6;

const UINT FIND_CHECKBOXES [] = {
	IDC_MATCHWHOLE_CHECK, 
	IDC_MATCHCASE_CHECK, 
	IDC_REGEXP_CHECK, 
	IDC_BACKSLASHES_CHECK,
	IDC_SEARCHUP_CHECK
};

const int nFIND_CHECKBOXES = 5;

const UINT REPLACE_CHECKBOXES [] = {
	IDC_MATCHWHOLE_CHECK, 
	IDC_MATCHCASE_CHECK, 
	IDC_REGEXP_CHECK,
	IDC_BACKSLASHES_CHECK,
	IDC_SEARCHUP_CHECK
};

const int nREPLACE_CHECKBOXES = 4;

const UINT FINDINFILES_CHECKBOXES [] = {
	//IDC_MATCHWHOLE_CHECK, 
	IDC_MATCHCASE_CHECK, 
	//IDC_REGEXP_CHECK, 
	//IDC_SEARCHUP_CHECK, 
	//IDC_BACKSLASHES_CHECK
	IDC_SUBDIRS_CHECK
};

int nFINDINFILES_CHECKBOXES = 2;

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
	m_SearchWhere = 0;
	m_type = eftFind;

	m_lastVisibleCB = -1;
	m_bottom = -1;
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

void CFindExDialog::moveUp(int offset, CWindow& ctrl)
{
	ClientRect rc(ctrl, *this);
	rc.top -= offset;
	ctrl.SetWindowPos(HWND_TOP, rc, SWP_SIMPLEMOVE);
}

LRESULT CFindExDialog::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	// Set up the tab control images.
	m_imageList.Create(16, 16, ILC_COLOR32 | ILC_MASK, 4, 4);
	
	HBITMAP hBitmap = (HBITMAP)::LoadImage(
		_Module.m_hInst,
		MAKEINTRESOURCE(IDB_FINDTOOLBAR),
		IMAGE_BITMAP, 0, 0, LR_SHARED);

	m_imageList.Add(hBitmap, RGB(255,0,255));
	m_tabControl.SetImageList(m_imageList.m_hImageList);
	
	// Position and create the tab control.
	DWORD dwStyle = WS_TABSTOP | WS_CHILD | WS_CLIPSIBLINGS | WS_CLIPCHILDREN | CTCS_HOTTRACK | CTCS_FLATEDGE;

	CRect rcTabs;
	GetClientRect(rcTabs);
	rcTabs.top = 2;
	rcTabs.left = 2;
	rcTabs.bottom = calcTabAreaHeight();

	m_tabControl.Create(m_hWnd, rcTabs, NULL, dwStyle, 0, IDC_FINDEX_TABS);
	m_tabControl.SetWindowPos(GetDlgItem(IDC_FINDEXTABS_PLACEHOLDER), 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_SHOWWINDOW | SWP_NOACTIVATE);

	// Move the line that goes beneath the tab control.
	CStatic s(GetDlgItem(IDC_FINDEX_LINE));
	ClientRect rc(s, *this);
	int height = rc.Height();
	rc.top = rcTabs.bottom + 1;
	s.SetWindowPos(HWND_TOP, rc, SWP_SIMPLEMOVE);

	// Sort out the combo boxes.
	CSize size = GetGUIFontSize();
	
	rc.set(GetDlgItem(IDC_FINDTEXT_DUMMY), *this);
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

	m_FindWhereCombo.Create(m_hWnd, rc, _T("FINDWHERECOMBO"), CBS_DROPDOWN | CBS_AUTOHSCROLL | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 0, IDC_FINDWHERE_COMBO,
		IDC_FINDWHERE_DUMMY);

	rc.set(GetDlgItem(IDC_FINDTYPE_DUMMY), *this);
	rc.bottom = rc.top + (size.cy * 10);

	m_FindTypeCombo.Create(m_hWnd, rc, _T("FINDTYPECOMBO"), CBS_DROPDOWN | CBS_AUTOHSCROLL | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 0, IDC_FINDTYPE_COMBO);
	m_FindTypeCombo.SetFont(GetFont());
	m_FindTypeCombo.SetWindowPos(GetDlgItem(IDC_FINDTYPE_DUMMY), 0,0,0,0, SWP_NOMOVE | SWP_NOSIZE | SWP_SHOWWINDOW | SWP_NOACTIVATE);

	m_ReHelperBtn.SubclassWindow(GetDlgItem(IDC_REHELPER_BUTTON));
	m_ReHelperBtn2.SubclassWindow(GetDlgItem(IDC_RHELPER_BUTTON));

	UIEnable(IDC_REHELPER_BUTTON, true);

	UIAddChildWindowContainer(m_hWnd);

	CenterWindow(GetParent());

	// Work out some combo box vertical positions.
	ClientRect rcCombo(m_FindWhereCombo, *this);

	m_comboDistance = rcCombo.top - m_group2Top;
	m_group3Bottom = rcCombo.bottom;

	// Move all the combo boxes into position.
	moveUp(m_comboDistance, m_FindWhereCombo);
	moveUp(m_comboDistance, m_FindTypeCombo);
	moveUp(m_comboDistance, CStatic(GetDlgItem(IDC_FINDWHERE_LABEL)));
	moveUp(m_comboDistance, CStatic(GetDlgItem(IDC_FINDTYPE_LABEL)));

	rcCombo.set(m_ReplaceTextCombo, *this);
	
	m_group2Bottom = rcCombo.bottom;
	m_group1Bottom = m_group2Bottom - m_comboDistance;

	// Add tabs
	addTab(_T("Find"), 2);
	addTab(_T("Replace"), 3);
	addTab(_T("Find in Files"), 0);

	updateLayout();

	DlgResize_Init();

	m_FindTextCombo.SetFocus();

	return TRUE;
}

LRESULT CFindExDialog::OnShowWindow(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	if((BOOL)wParam)
	{
		SFindOptions* pOptions = OPTIONS->GetFindOptions();
		//SetFindText(m_FindText);
		if(m_FindText.GetLength() == 0 && pOptions->FindText.GetLength())
			m_FindText = pOptions->FindText;
		m_bSearchUp = pOptions->Direction;
		m_bMatchCase = pOptions->MatchCase;
		m_bMatchWhole = pOptions->MatchWholeWord;
		//m_bSearchAll = pOptions->SearchAll;
		m_bRegExp = pOptions->UseRegExp;
		
		// Do the funky DDX thang...
		DoDataExchange(FALSE);

		m_FindTextCombo.SetFocus();

		UIEnable(IDC_REHELPER_BUTTON, m_bRegExp);
		UIUpdateChildWindows();
	}

	return 0;
}

LRESULT CFindExDialog::OnCloseCmd(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	/*CChildFrame* pChild = GetCurrentEditorWnd();
	if(pChild != NULL)
		pChild->SetFocus();
	else
		GetWindow(GW_OWNER).SetFocus();*/

	ShowWindow(SW_HIDE);
	
	return 0;
}

LRESULT CFindExDialog::OnSelChange(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& bHandled)
{
	int nNewTab = m_tabControl.GetCurSel();
	m_type = (EFindDialogType)nNewTab;
	updateLayout();
	return 0;
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

int CFindExDialog::positionChecks(int top, const UINT* checkboxIDs, int nCheckboxIDs)
{
	int chkDist = m_checkDist;

	ClientRect rc(CButton(GetDlgItem(checkboxIDs[0])), *this);
	rc.top = top;

	for(int j = 0; j < CHECKBOX_CONTROL_COUNT; j++)
	{
		CButton(GetDlgItem(CHECKBOX_CONTROL_IDS[j])).ShowWindow(SW_HIDE);
	}

	for(int i = 0; i < nCheckboxIDs; i++)
	{
		CButton cb(GetDlgItem(checkboxIDs[i]));
		cb.SetWindowPos(HWND_TOP, rc, SWP_SIMPLEMOVE);
		rc.top += chkDist;
		cb.ShowWindow(SW_SHOW);
	}

	m_lastVisibleCB = checkboxIDs[nCheckboxIDs-1];

	return rc.top;
}

void CFindExDialog::updateLayout()
{
	int restTop;
	const UINT* checkboxes = NULL;
	int nCheckboxes = 0;

	if(m_lastVisibleCB == -1)
	{
		// First time through, we calculate the gap between checkboxes
		// and work out where the initial last-visible checkbox is.
		int chkDist;
		CButton cb1(GetDlgItem(CHECKBOX_CONTROL_IDS[0]));
		ClientRect rc(cb1, *this);
		
		chkDist = rc.top;
		
		CButton cb2(GetDlgItem(CHECKBOX_CONTROL_IDS[1]));
		rc.set(cb2, *this);
		
		m_checkDist = rc.top - chkDist;

		m_lastVisibleCB = CHECKBOX_CONTROL_IDS[CHECKBOX_CONTROL_COUNT-1];

		ClientRect rcLastCheck(GetDlgItem(CHECKBOX_CONTROL_IDS[CHECKBOX_CONTROL_COUNT-1]), *this);
		m_bottom = rcLastCheck.bottom;
	}
	
	int oldBottom = m_bottom;

	switch(m_type)
	{
	case eftFind:
		{
			checkboxes = FIND_CHECKBOXES;
			nCheckboxes = nFIND_CHECKBOXES;
			
			CStatic(GetDlgItem(IDC_REPLACETEXT_LABEL)).ShowWindow(SW_HIDE);
			m_ReplaceTextCombo.ShowWindow(SW_HIDE);
			m_ReHelperBtn2.ShowWindow(SW_HIDE);

			CStatic(GetDlgItem(IDC_FINDWHERE_LABEL)).ShowWindow(SW_HIDE);
			m_FindWhereCombo.ShowWindow(SW_HIDE);
			CStatic(GetDlgItem(IDC_FINDTYPE_LABEL)).ShowWindow(SW_HIDE);
			m_FindTypeCombo.ShowWindow(SW_HIDE);

			CButton(GetDlgItem(IDC_REPLACE_BUTTON)).ShowWindow(SW_HIDE);
			CButton(GetDlgItem(IDC_REPLACEALL_BUTTON)).ShowWindow(SW_HIDE);

			CButton(GetDlgItem(IDC_MARKALL_BUTTON)).ShowWindow(SW_SHOW);
			
			restTop = m_group1Bottom + 12;
		}
		break;

	case eftReplace:
		{
			checkboxes = REPLACE_CHECKBOXES;
			nCheckboxes = nREPLACE_CHECKBOXES;
			
			CStatic(GetDlgItem(IDC_FINDWHERE_LABEL)).ShowWindow(SW_HIDE);
			m_FindWhereCombo.ShowWindow(SW_HIDE);
			CStatic(GetDlgItem(IDC_FINDTYPE_LABEL)).ShowWindow(SW_HIDE);
			m_FindTypeCombo.ShowWindow(SW_HIDE);

			CButton(GetDlgItem(IDC_MARKALL_BUTTON)).ShowWindow(SW_SHOW);

			CStatic(GetDlgItem(IDC_REPLACETEXT_LABEL)).ShowWindow(SW_SHOW);
			m_ReplaceTextCombo.ShowWindow(SW_SHOW);
			m_ReHelperBtn2.ShowWindow(SW_SHOW);

			CButton(GetDlgItem(IDC_REPLACE_BUTTON)).ShowWindow(SW_SHOW);
			CButton(GetDlgItem(IDC_REPLACEALL_BUTTON)).ShowWindow(SW_SHOW);

			restTop = m_group2Bottom + 12;
		}
		break;

	case eftFindInFiles:
		{
			checkboxes = FINDINFILES_CHECKBOXES;
			nCheckboxes = nFINDINFILES_CHECKBOXES;

			int nCheckboxes = 2;

			CStatic(GetDlgItem(IDC_REPLACETEXT_LABEL)).ShowWindow(SW_HIDE);
			m_ReplaceTextCombo.ShowWindow(SW_HIDE);
			m_ReHelperBtn2.ShowWindow(SW_HIDE);

			CButton(GetDlgItem(IDC_MARKALL_BUTTON)).ShowWindow(SW_HIDE);
			CButton(GetDlgItem(IDC_REPLACE_BUTTON)).ShowWindow(SW_HIDE);
			CButton(GetDlgItem(IDC_REPLACEALL_BUTTON)).ShowWindow(SW_HIDE);

			CStatic(GetDlgItem(IDC_FINDWHERE_LABEL)).ShowWindow(SW_SHOW);
			m_FindWhereCombo.ShowWindow(SW_SHOW);
			CStatic(GetDlgItem(IDC_FINDTYPE_LABEL)).ShowWindow(SW_SHOW);
			m_FindTypeCombo.ShowWindow(SW_SHOW);

			restTop = m_group3Bottom + 12;
		}
		break;
	}

	int bottomChecks = positionChecks(restTop, checkboxes, nCheckboxes);

	CStatic dirgroup(GetDlgItem(IDC_SEARCHIN_GROUP));
	ClientRect rc(dirgroup, *this);
	int dy = rc.top - (restTop);
	rc.top = restTop;
	dirgroup.SetWindowPos(HWND_TOP, rc, SWP_SIMPLEMOVE);

	for(int i = 0; i < 4; i++)
	{
		CButton up(GetDlgItem(RADIO_CONTROL_IDS[i]));
		rc.set(up, *this);
		rc.top -= dy;
		up.SetWindowPos(HWND_TOP, rc, SWP_SIMPLEMOVE);
	}

	// Get the new position of the bottom of the window.
	CRect rcMe;
	GetWindowRect(rcMe);
	int newBottom = rcMe.bottom - (oldBottom - bottomChecks);
	
	// Check whether it's below the groupbox.
	CRect rcGroup;
	dirgroup.GetWindowRect(rcGroup);
	newBottom = max(newBottom, rcGroup.bottom+12);

	// Get that difference and offset our minmaxinfo
	dy = rcMe.bottom - newBottom;
	m_ptMinTrackSize.y -= dy;

	m_bottom = rcMe.bottom = newBottom;
	//m_bottom = rcMe.bottom;

	SetWindowPos(HWND_TOP, rcMe, SWP_NOZORDER | SWP_NOMOVE | SWP_NOACTIVATE);	
}