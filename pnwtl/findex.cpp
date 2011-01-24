/**
 * @file findex.cpp
 * @brief Find and Replace dialogs for PN 2
 * @author Simon Steele
 * @note Copyright (c) 2004-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "CustomAutoComplete.h"
#include "include/accombo.h"
#include "findex.h"
#include "childfrm.h"
#include "pndialogs.h"
#include "project.h"

#define SWP_SIMPLEMOVE (SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE)

const UINT CHECKBOX_CONTROL_IDS [] = {
	IDC_MATCHWHOLE_CHECK, 
	IDC_MATCHCASE_CHECK, 
	IDC_REGEXP_CHECK, 
	IDC_BACKSLASHES_CHECK,
	IDC_SEARCHUP_CHECK,
	IDC_SUBDIRS_CHECK,
	IDC_INCLUDEHIDDEN_CHECK
};

const UINT RADIO_CONTROL_IDS [] = {
	IDC_CURRENTDOC_RADIO,
	IDC_ALLOPEN_RADIO,
	IDC_CURRENTPROJ_RADIO,
	IDC_INSELECTION_RADIO
};

const int CHECKBOX_CONTROL_COUNT = 7;

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

const int nREPLACE_CHECKBOXES = 5;

const UINT FINDINFILES_CHECKBOXES [] = {
	IDC_MATCHWHOLE_CHECK, 
	IDC_MATCHCASE_CHECK, 
	//IDC_REGEXP_CHECK, 
	//IDC_SEARCHUP_CHECK, 
	//IDC_BACKSLASHES_CHECK
	IDC_SUBDIRS_CHECK,
	IDC_INCLUDEHIDDEN_CHECK
};

int nFINDINFILES_CHECKBOXES = 4;

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

CFindExDialog::CFindExDialog() : 
	m_SearchWhere(0),
	m_type(eftFind),
	m_lastType(eftInvalid),
	m_lastFifLocation(fwUser),
	m_FindWhereCombo(false), 
	m_lastVisibleCB(-1),
	m_bottom(-1),
	m_pFnSLWA(NULL),
	m_bInitialising(false),
	m_bHiding(false)
{
	if(g_Context.OSVersion.dwMajorVersion >= 5)
	{
		HMODULE hUser32 = ::GetModuleHandleA("USER32.DLL");
		m_pFnSLWA = (PFNSetLayeredWindowAttributes)::GetProcAddress(hUser32, "SetLayeredWindowAttributes");
	}
}

BOOL CFindExDialog::PreTranslateMessage(MSG* pMsg)
{
	return IsDialogMessage(pMsg);
}

void CFindExDialog::Show(EFindDialogType type, LPCTSTR findText)
{
	m_type = type;
	m_tabControl.SetCurSel((int)type, false);
	updateLayout();

	DoDataExchange(FALSE);

	// Place the dialog so it doesn't hide the text under the caret
	CChildFrame* editor = getCurrentEditorWnd();
	if(editor)
	{
		// Get the position on screen of the caret.
		CTextView* textView = editor->GetTextView();
		long pos = textView->GetCurrentPos();
		POINT pt = {
			textView->PointXFromPosition(pos),
			textView->PointYFromPosition(pos)
		};
		int lineHeight = textView->TextHeight(0);
		editor->ClientToScreen(&pt);
		
		// Place the window away from the caret.
		placeWindow(pt, lineHeight);

		// Need to decide if we want to put the dialog into "Replace In Selection" mode
		// and whether to include the selected text as the find text.

		// Rules:
		// There is a selection AND
		// (The selection runs from the first character on the line to the last OR
		// The selection spans multiple lines OR
		// The selection is longer than 1024 characters)
		bool bigSelection = selectionIsWholeLine(textView);

		if(type == eftReplace)
		{
			if(bigSelection)
			{
				m_SearchWhere = extensions::elwSelection;
			}
			else
			{
				m_SearchWhere = extensions::elwCurrentDoc;
			}
		}

		if(!bigSelection)
			m_FindText = findText;
	}

	if(IsWindowVisible())
	{
		// We do this to update the find text when the window is 
		// re-activated without being closed first.
		DoDataExchange(FALSE);
	}
	
	ShowWindow(SW_SHOW);
	
	m_FindTextCombo.SetFocus();
}

LRESULT CFindExDialog::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	CMessageLoop* pLoop = _Module.GetMessageLoop();
	ATLASSERT(pLoop != NULL);
	pLoop->AddMessageFilter(this);

	// Set up the tab control images.
	bool lowColour = !IsXPOrLater() || OPTIONS->Get(PNSK_INTERFACE, _T("LowColourToolbars"), false);
	HBITMAP hBitmap;
	if (lowColour)
	{
		m_imageList.Create(16, 16, ILC_COLOR24 | ILC_MASK, 4, 1);
		hBitmap = static_cast<HBITMAP>(::LoadImage(ATL::_AtlBaseModule.GetResourceInstance(), MAKEINTRESOURCE(IDB_TBFIND24), IMAGE_BITMAP, 0, 0, LR_SHARED));
	}
	else
	{
		m_imageList.Create(16, 16, ILC_COLOR32 | ILC_MASK, 4, 1);
		hBitmap = static_cast<HBITMAP>(::LoadImage(ATL::_AtlBaseModule.GetResourceInstance(), MAKEINTRESOURCE(IDB_TBFIND), IMAGE_BITMAP, 0, 0, LR_SHARED | LR_CREATEDIBSECTION));
	}

	m_imageList.Add(hBitmap, RGB(255, 0, 255));
	m_tabControl.SetImageList(m_imageList.m_hImageList);
	
	// Position and create the tab control.
	DWORD dwStyle = WS_TABSTOP | WS_CHILD | WS_CLIPSIBLINGS | WS_CLIPCHILDREN | CTCS_HOTTRACK | CTCS_FLATEDGE;

	CRect rcTabs;
	GetClientRect(rcTabs);
	rcTabs.top = 2;
	rcTabs.left = 2;
	rcTabs.bottom = calcTabAreaHeight();

	m_tabControl.Create(m_hWnd, rcTabs, NULL, dwStyle, 0, IDC_FINDEX_TABS);
	m_tabControl.SetWindowPos(GetDlgItem(IDC_FINDEXTABS_PLACEHOLDER).m_hWnd, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_SHOWWINDOW | SWP_NOACTIVATE);

	// Move the line that goes beneath the tab control.
	CStatic s(GetDlgItem(IDC_FINDEX_LINE));
	ClientRect rc(s, *this);

	rc.top = rcTabs.bottom + 1;
	s.SetWindowPos(HWND_TOP, rc, SWP_SIMPLEMOVE);

	// Sort out the combo boxes.
	CSize size = getGUIFontSize();

	CWindow findTextDummy(GetDlgItem(IDC_FINDTEXT_DUMMY));
	rc.set(findTextDummy, *this);
	rc.bottom = rc.top + (size.cy * 10);

	m_FindTextCombo.Create(m_hWnd, rc, _T("FINDTEXTCOMBO"), CBS_DROPDOWN | CBS_AUTOHSCROLL | WS_CHILD | WS_VSCROLL | WS_VISIBLE | WS_TABSTOP, 0, IDC_FINDTEXT_COMBO, _T("Find"), IDC_FINDTEXT_DUMMY);
	m_FindTextCombo.SetWindowPos(GetDlgItem(IDC_FINDTEXT_LABEL).m_hWnd, rc, SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);

	CWindow replaceTextDummy(GetDlgItem(IDC_REPLACETEXT_DUMMY));
	rc.set(replaceTextDummy, *this);
	rc.bottom = rc.top + (size.cy * 10);

	m_ReplaceTextCombo.Create(m_hWnd, rc, _T("REPLACETEXTCOMBO"), CBS_DROPDOWN | CBS_AUTOHSCROLL | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 0, IDC_REPLACETEXT_COMBO, _T("Replace"), IDC_REPLACETEXT_DUMMY);
	m_ReplaceTextCombo.SetWindowPos(GetDlgItem(IDC_REPLACETEXT_LABEL).m_hWnd, rc, SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);

	// Store the position of the second combo.
	m_group2Top = rc.top;

	CWindow findWhereDummy(GetDlgItem(IDC_FINDWHERE_DUMMY));
	rc.set(findWhereDummy, *this);
	rc.bottom = rc.top + (size.cy * 10);

	m_FindWhereCombo.Create(m_hWnd, rc, _T("FINDWHERECOMBO"), CBS_DROPDOWN | CBS_AUTOHSCROLL | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 0, IDC_FINDWHERE_COMBO,
		IDC_FINDWHERE_DUMMY);
	m_FindWhereCombo.SetWindowPos(GetDlgItem(IDC_FINDWHERE_LABEL).m_hWnd, rc, SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE);

	m_FindWhereCombo.InsertString(0, LS(IDS_CURRENTFILE));
	m_FindWhereCombo.InsertString(1, LS(IDS_CURRENTFOLDER));
	m_FindWhereCombo.InsertString(2, LS(IDS_CURRENTPROJECTFILES));
	m_FindWhereCombo.InsertString(3, LS(IDS_ALLOPENFILES));
	m_FindWhereCombo.SetItemData(0, fwCurrentFile);
	m_FindWhereCombo.SetItemData(1, fwCurrentFolder);
	m_FindWhereCombo.SetItemData(2, fwCurrentProjectFiles);
	m_FindWhereCombo.SetItemData(3, fwOpenDocs);

	CWindow findTypeDummy(GetDlgItem(IDC_FINDTYPE_DUMMY));
	rc.set(findTypeDummy, *this);
	rc.bottom = rc.top + (size.cy * 10);

	m_FindTypeCombo.Create(m_hWnd, rc, _T("FINDTYPECOMBO"), CBS_DROPDOWN | CBS_AUTOHSCROLL | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 0, IDC_FINDTYPE_COMBO,
		_T("FindType"), IDC_FINDTYPE_DUMMY);
	m_FindTypeCombo.SetFont(GetFont());
	m_FindTypeCombo.SetWindowPos(GetDlgItem(IDC_FINDTYPE_DUMMY).m_hWnd, 0,0,0,0, SWP_NOMOVE | SWP_NOSIZE | SWP_SHOWWINDOW | SWP_NOACTIVATE);

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

	CStatic findWhereLabel(GetDlgItem(IDC_FINDWHERE_LABEL).m_hWnd);
	moveUp(m_comboDistance, findWhereLabel);
	
	CStatic findTypeLabel(GetDlgItem(IDC_FINDTYPE_LABEL).m_hWnd);
	moveUp(m_comboDistance, findTypeLabel);

	rcCombo.set(m_ReplaceTextCombo, *this);
	
	m_group2Bottom = rcCombo.bottom;
	m_group1Bottom = m_group2Bottom - m_comboDistance;

	m_bInitialising = true;

	// Add tabs
	addTab(LS(IDS_FIND), 2);
	addTab(LS(IDS_REPLACE), 3);
	addTab(LS(IDS_FINDINFILES), 0);

	m_bInitialising = false;

	updateLayout();

	DlgResize_Init();

	m_FindTextCombo.SetFocus();

	return TRUE;
}

LRESULT CFindExDialog::OnShowWindow(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	if((BOOL)wParam)
	{
		SearchOptions* pOptions = reinterpret_cast<SearchOptions*>( OPTIONS->GetSearchOptions() );
		if(m_FindText.GetLength() == 0 && _tcslen(pOptions->GetFindText()) > 0)
		{
			m_FindText = pOptions->GetFindText();
		}
		
		m_ReplaceText = pOptions->GetReplaceText();
		m_FindTypeText = pOptions->GetFileExts();

		switch(m_lastFifLocation)
		{
			case fwUser:
				m_FindWhereText = pOptions->GetSearchPath();
				break;
			case fwCurrentFile:
				m_FindWhereCombo.SetCurSel(0);
				break;
			case fwCurrentFolder:
				m_FindWhereCombo.SetCurSel(1);
				break;
			case fwCurrentProjectFiles:
				m_FindWhereCombo.SetCurSel(2);
				break;
			case fwOpenDocs:
				m_FindWhereCombo.SetCurSel(3);
				break;
		}

		m_bMatchCase = pOptions->GetMatchCase();
		m_bMatchWhole = pOptions->GetMatchWholeWord();
		m_bSearchUp = pOptions->GetSearchBackwards();
		m_bRegExp = pOptions->GetUseRegExp();
		m_bUseSlashes = pOptions->GetUseSlashes();
		m_bSearchSubdirs = pOptions->GetRecurse();
		m_bIncludeHidden = pOptions->GetIncludeHidden();

		//m_bSearchAll = pOptions->SearchAll;
		
		// Do the funky DDX thang...
		DoDataExchange(FALSE);

		m_FindTextCombo.SetFocus();

		UIEnable(IDC_REHELPER_BUTTON, m_bRegExp);
		UIEnable(IDC_RHELPER_BUTTON, m_bRegExp);
		UIUpdateChildWindows();
	}

	return 0;
}

LRESULT CFindExDialog::OnCloseWindow(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	GetWindow(GW_OWNER).SetFocus();
	ShowWindow(SW_HIDE);
	return 0;
}

LRESULT CFindExDialog::OnCloseCmd(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	/*CChildFrame* pChild = GetCurrentEditorWnd();
	if(pChild != NULL)
		pChild->SetFocus();
	else*/
	GetWindow(GW_OWNER).SetFocus();

	ShowWindow(SW_HIDE);
	
	return 0;
}

LRESULT CFindExDialog::OnActivate(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	if(LOWORD(wParam) == WA_INACTIVE && OPTIONS->GetCached(Options::OFindAlphaEnabled) && !m_bHiding)
	{
		SetWindowLong(GWL_EXSTYLE, GetWindowLong(GWL_EXSTYLE) | WS_EX_LAYERED);
		int tspct = OPTIONS->GetCached(Options::OFindAlphaPercent);
		setLayeredWindowAttributes(m_hWnd, NULL, (255 * tspct) / 100, LWA_ALPHA);
	}
	else
	{
		// Remove WS_EX_LAYERED from this window styles
		SetWindowLong(GWL_EXSTYLE, GetWindowLong(GWL_EXSTYLE) & ~WS_EX_LAYERED);

		// Ask the window and its children to repaint
		RedrawWindow(NULL, NULL, RDW_ERASE | RDW_INVALIDATE | RDW_FRAME | RDW_ALLCHILDREN);
	}

	return 0;
}

LRESULT CFindExDialog::OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = FALSE;

	CMessageLoop* pLoop = _Module.GetMessageLoop();
	ATLASSERT(pLoop != NULL);
	pLoop->RemoveMessageFilter(this);

	return 0;
}

LRESULT CFindExDialog::OnFindNext(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if (m_type == eftFindInFiles)
	{
		findInFiles();
		return TRUE;
	}

	switch (findNext())
	{
	case fnFound:
		{
			if (m_type != eftReplace && !OPTIONS->Get(PNSK_INTERFACE, _T("FindStaysOpen"), false))
			{
				// Default Visual C++, old PN and others behaviour:
				// We disable the alpha behaviour because of a Win7 bug with hiding layered toolwindows
				m_bHiding = true;
				ShowWindow(SW_HIDE);
				m_bHiding = false;
			}
		}
		break;

	case fnNotFound:
	case fnInvalidSearch:
		{
			CString strTextToFind = m_FindText;
			CString strMsg;

			if (strTextToFind.IsEmpty())
				strTextToFind = _T("(empty)");

			strMsg.Format(IDS_FINDNOTFOUND, strTextToFind);
			MessageBox((LPCTSTR)strMsg, LS(IDR_MAINFRAME), MB_OK | MB_ICONINFORMATION);

			m_FindTextCombo.SetFocus();
		}
		break;

	case fnInvalidRegex:
		{
			MessageBox(LS(IDS_INVALIDREGEX), LS(IDR_MAINFRAME), MB_OK | MB_ICONINFORMATION);

			m_FindTextCombo.SetFocus();
		}
		break;

	default:
		LOG(_T("Unexpected Find Result"));
	}

	return TRUE;
}

LRESULT CFindExDialog::OnMarkAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CChildFrame* editor = getCurrentEditorWnd();

	if(editor != NULL)
	{
		SearchOptions* pOptions = getOptions();
		editor->MarkAll(pOptions);
		g_Context.m_frame->GetWindow()->SendMessage(PN_UPDATEFINDTEXT,0,0);
	}
	
	return TRUE;
}

LRESULT CFindExDialog::OnReHelperClicked(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CRect rc;
	GetDlgItem(wID).GetWindowRect(&rc);
	doRegExpHelperMenu(rc, wID == IDC_RHELPER_BUTTON);

	return TRUE;
}

LRESULT CFindExDialog::OnReInsertClicked(WORD /*wNotifyCode*/, WORD nID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CString Text;
	int offset = getRegExpString(nID, Text);

	// DoDataExchange(TRUE);
	doRegExpInsert(&m_FindTextCombo, Text, m_FindText, offset);

	return TRUE;
}

LRESULT CFindExDialog::OnReplaceClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	// Call this before GetCurrentEditorWnd - it will check if the editor
	// has changed and if so set Found to false.
	SearchOptions* pOptions = getOptions();

	CChildFrame* pEditor = getCurrentEditorWnd();		
		
	if(pEditor)
		pEditor->Replace(pOptions);

	return 0;
}

LRESULT CFindExDialog::OnReplaceAllClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	SearchOptions* pOptions = getOptions();

	CChildFrame* pEditor = getCurrentEditorWnd();
		
	if(pEditor)
	{
		int count = pEditor->ReplaceAll(pOptions);
		CString s;
		s.Format(IDS_NREPLACEMENTS, count);
		CString sTitle;
		sTitle.LoadString(IDR_MAINFRAME);
		MessageBox((LPCTSTR)s, sTitle, MB_OK | MB_ICONINFORMATION);
	}

	return 0;
}

LRESULT CFindExDialog::OnBrowseClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CString str;
	m_FindWhereCombo.GetWindowText(str);
	if(!DirExists(str))
		str = _T("");

	CString strTitle;
	strTitle.LoadString(IDS_BROWSEFINDROOT);

	CPNFolderDialog fd(m_hWnd, (LPCTSTR)str, strTitle, BIF_RETURNONLYFSDIRS | BIF_USENEWUI | BIF_NONEWFOLDERBUTTON);
	if(fd.DoModal() == IDOK)
	{
		m_FindWhereCombo.SetCurSel(-1);
		m_FindWhereCombo.SetWindowText( fd.GetFolderPath() );
	}

	return 0;
}

LRESULT CFindExDialog::OnReMatchesMenuItemClicked(WORD /*wNotifyCode*/, WORD nID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int matchno = (nID - ID_REGEXP_TAGGEDEXPRESSION1) + 1;
	CString Text;
	Text.Format(_T("\\%d"), matchno);

	// DoDataExchange(TRUE);
	doRegExpInsert(&m_ReplaceTextCombo, Text, m_ReplaceText, 0);

	return 0;
}

LRESULT CFindExDialog::OnUseRegExpClicked(WORD /*wNotifyCode*/, WORD /*nID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_bRegExp = !m_bRegExp;
	UIEnable(IDC_REHELPER_BUTTON, m_bRegExp);
	UIEnable(IDC_RHELPER_BUTTON, m_bRegExp);
	UIUpdateChildWindows();
	return TRUE;
}

LRESULT CFindExDialog::OnSelChange(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& bHandled)
{
	if(m_bInitialising)
		return 0;

	HWND hWndCur = GetCurrentEditor();

	int nNewTab = m_tabControl.GetCurSel();

	if(!hWndCur && nNewTab != eftFindInFiles)
	{
		m_type = eftFindInFiles;
		m_tabControl.SetCurSel((int)m_type);
	}
	else
	{
		m_type = (EFindDialogType)nNewTab;
	}
	
	updateLayout();

	return 0;
}

LRESULT CFindExDialog::OnRadioClicked(WORD /*wNotifyCode*/, WORD /*nID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	enableButtons();
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

void CFindExDialog::doRegExpHelperMenu(LPRECT rc, bool bDoMatches)
{
	HMENU hRegExpMenu;
	if(bDoMatches)
	{
		hRegExpMenu = ::LoadMenu(_Module.m_hInst, MAKEINTRESOURCE(IDR_POPUP_REGEXPM));
	}
	else
	{
		hRegExpMenu = ::LoadMenu(_Module.m_hInst, MAKEINTRESOURCE(IDR_POPUP_REGEXP));
	}

	HMENU hPopup = ::GetSubMenu(hRegExpMenu, 0);

	::TrackPopupMenu(hPopup, 0, rc->right, rc->top, 0, m_hWnd, NULL);

	::DestroyMenu(hRegExpMenu);
}

int CFindExDialog::doRegExpInsert(BXT::CComboBoxAC* pCB, LPCTSTR insert, CString& str, int offset)
{
	int selStart, selEnd;
	WTL::BXT::CComboBoxAC::CComboBoxEdit& edit = pCB->GetEditCtrl();
	
	// Selection is lost when the combo loses focus, we get the last stored
	// selection range and reset it.
	edit.GetStoredSel(selStart, selEnd);
	edit.SetSel(selStart, selEnd);
	edit.ReplaceSel(insert);

	if (offset != 0)
	{
		selStart += offset;
	}
	else
	{
		selStart += _tcslen(insert);
	}

	edit.SetFocus();
	edit.SetSel(selStart, selStart);

	return selStart;
}

bool CFindExDialog::editorChanged()
{
	return m_pLastEditor != getCurrentEditorWnd();
}

void CFindExDialog::enableButtons()
{
	CButton inSelRadio(GetDlgItem(IDC_INSELECTION_RADIO));
	CButton(GetDlgItem(IDC_REPLACE_BUTTON)).EnableWindow( inSelRadio.GetCheck() != BST_CHECKED );
	CButton(GetDlgItem(IDC_FINDNEXT_BUTTON)).EnableWindow( inSelRadio.GetCheck() != BST_CHECKED );
}

FindNextResult CFindExDialog::findNext()
{
	FindNextResult found = fnNotFound;

	CChildFrame* editor = getCurrentEditorWnd();

	if(editor != NULL)
	{
		SearchOptions* pOptions = getOptions();
		found = editor->FindNext(pOptions);
		g_Context.m_frame->GetWindow()->SendMessage(PN_UPDATEFINDTEXT,0,0);
	}

	return found;
}

void CFindExDialog::findInFiles()
{
	SearchOptions* pOptions = getOptions();

	if (pOptions->GetFileSet() == extensions::fifPath)
	{
		if (!DirExists(pOptions->GetSearchPath()))
		{
			CString sError;
			sError.Format(IDS_INVALIDFIFPATH, m_FindWhereText);
			::PNTaskDialog(m_hWnd, IDR_MAINFRAME, _T(""), (LPCTSTR)sError, TDCBF_OK_BUTTON, TDT_WARNING_ICON);
			return;
		}
	}

	g_Context.m_frame->FindInFiles(pOptions);

	// Reset the FileExtensions member incase we did find in current file...
	pOptions->SetFileExts(m_FindTypeText);

	// Disable layered window for hiding to avoid Win7 bug.
	m_bHiding = true;
	ShowWindow(SW_HIDE);
	m_bHiding = false;
}

int CFindExDialog::getRegExpString(int nID, CString& Text)
{
	int offset = 0;

	switch(nID)
	{
		case ID_REGEXP_ANYCHARACTER:
			Text = _T(".");
			break;
		case ID_REGEXP_CHARACTERINRANGE:
			Text = _T("[]");
			offset = 1;
			break;
		case ID_REGEXP_CHARACTERNOTINRANGE:
			Text = _T("[^]");
			offset = 2;
			break;
		case ID_REGEXP_BEGINNINGOFLINE:
			Text = _T("^");
			break;
		case ID_REGEXP_ENDOFLINE:
			Text = _T("$");
			break;
		case ID_REGEXP_TAGGEDEXPRESSSION:
			Text = _T("()");
			break;
		case ID_REGEXP_NOT:
			Text = _T("~");
			break;
		case ID_REGEXP_OR:
			Text = _T("\\!");
			break;
		case ID_REGEXP_0ORMOREMATCHES:
			Text = _T("*");
			break;
		case ID_REGEXP_1ORMOREMATCHES:
			Text = _T("+");
			break;
		case ID_REGEXP_GROUP:
			Text = _T("{}");
			break;
	};

	return offset;
}

CChildFrame* CFindExDialog::getCurrentEditorWnd()
{
	m_pLastEditor = CChildFrame::FromHandle(GetCurrentEditor());
	return m_pLastEditor;
}

CSize CFindExDialog::getGUIFontSize()
{
	CClientDC dc(m_hWnd);
	dc.SelectFont((HFONT) GetStockObject( DEFAULT_GUI_FONT ));		
	TEXTMETRIC tm;
	dc.GetTextMetrics( &tm );

	return CSize( tm.tmAveCharWidth, tm.tmHeight + tm.tmExternalLeading);
}

SearchOptions* CFindExDialog::getOptions()
{
	DoDataExchange(TRUE);

	SearchOptions* pOptions = reinterpret_cast<SearchOptions*>( OPTIONS->GetSearchOptions() );

	// If the user has changed to a different scintilla window
	// then Found is no longer necessarily true.
	if(editorChanged())
		pOptions->SetFound(false);

	extensions::EFindWhere lookWhere = (extensions::EFindWhere)m_SearchWhere;

	pOptions->SetFindText			( m_FindText );
	pOptions->SetReplaceText		( m_ReplaceText );
	pOptions->SetSearchBackwards	( m_bSearchUp == TRUE );
	pOptions->SetMatchCase			( m_bMatchCase == TRUE );
	pOptions->SetMatchWholeWord		( m_bMatchWhole == TRUE );
	pOptions->SetUseRegExp			( m_bRegExp == TRUE );
	pOptions->SetUseSlashes		    ( m_bUseSlashes == TRUE );
	pOptions->SetReplaceInSelection	( lookWhere == extensions::elwSelection );
	pOptions->SetFindTarget			( lookWhere );
	pOptions->SetRecurse			( m_bSearchSubdirs == TRUE );
	pOptions->SetIncludeHidden		( m_bIncludeHidden == TRUE );
	pOptions->SetFileExts			( m_FindTypeText );
	
	///@todo Add a user interface counterpart for the loop search option.
	pOptions->SetLoopOK(true);

	// Where are we going to look?
	int findType(0);
	if(m_FindWhereCombo.GetCurSel() != -1)
	{
		findType = (EFIFWhere)m_FindWhereCombo.GetItemData(m_FindWhereCombo.GetCurSel());
	}

	switch(findType)
	{
		case fwCurrentFile:
		{
			if(m_pLastEditor != NULL && m_pLastEditor->CanSave())
			{
				pOptions->SetFileSet(extensions::fifSingleFile);
			}
			else
			{
				// No document! Can't search current file, leave as previous...
				pOptions->SetFileSet(extensions::fifPath);
				pOptions->SetSearchPath(_T(""));
			}
		}
		break;

		case fwCurrentFolder:
		{
			pOptions->SetFileSet(extensions::fifPath);

			if(m_pLastEditor != NULL && m_pLastEditor->CanSave())
			{
				pOptions->SetSearchPath(m_pLastEditor->GetFileName(FN_PATH).c_str());
			}
			else
			{
				// No document! Can't search current file, leave as previous...
				pOptions->SetSearchPath(L"");
			}
		}
		break;

		case fwCurrentProjectFiles:
		{
			pOptions->SetFileSet(extensions::fifActiveProjectFiles);
			pOptions->SetSearchPath(L"");
		}
		break;

		case fwOpenDocs:
		{
			pOptions->SetFileSet(extensions::fifOpenFiles);
			pOptions->SetSearchPath(L"");
		}
		break;
		
		default:
		{
			pOptions->SetFileSet(extensions::fifPath);
			pOptions->SetSearchPath(m_FindWhereText);
		}
		break;
	}
	
	if ((findType == 0 || findType > fwOpenDocs) && wcscmp(pOptions->GetSearchPath(), L"") != -1) 
	{
		if(DirExists(pOptions->GetSearchPath()))
		{
			m_FindWhereCombo.AddString(pOptions->GetSearchPath(),4);
		}
		else
		{
			pOptions->SetSearchPath(L"");
		}
	}

	m_lastFifLocation = (EFIFWhere)m_FindWhereCombo.GetItemData(m_FindWhereCombo.GetCurSel());

	m_FindTextCombo.AddString( m_FindText );
	m_ReplaceTextCombo.AddString( m_ReplaceText );
//	m_FindWhereCombo.AddString( m_FindWhereText,3 );

	return pOptions;
}

void CFindExDialog::moveUp(int offset, CWindow& ctrl)
{
	ClientRect rc(ctrl, *this);
	rc.top -= offset;
	ctrl.SetWindowPos(HWND_TOP, rc, SWP_SIMPLEMOVE);
}

/**
 * @brief Places the window so that it does not cover the cursor.
 * @param pt constant reference to the point that should not be covered by the dialog
 * @param lineHeight line height as specified by Scintilla.
 */
void CFindExDialog::placeWindow(const POINT& pt, int lineHeight)
{
	// Get current pos and size
	RECT rc;
	GetWindowRect(&rc);
	POINT winPos = { rc.left, rc.top };
	SIZE winSize = { rc.right - rc.left, rc.bottom - rc.top };

	// Default to place the dialog above the line
	POINT pos = winPos;
	if(rc.bottom >= pt.y && rc.top <= pt.y)
		pos.y = pt.y - winSize.cy - lineHeight;
	if(pos.y < 0)
		pos.y = max(0, pt.y + lineHeight);

	::GetWindowRect(GetWindow(GW_OWNER), &rc);
	// TODO: 
	pos.x = rc.right - winSize.cx - ::GetSystemMetrics(SM_CXVSCROLL);

	// Ensure that the dialog is inside the work area
	RECT rcWa;
	
	if(g_Context.OSVersion.dwMajorVersion >= 5) // support multiple monitors on 2k+
	{
		rcWa.top = ::GetSystemMetrics(SM_YVIRTUALSCREEN);
		rcWa.left = ::GetSystemMetrics(SM_XVIRTUALSCREEN);
		rcWa.right = rcWa.left + ::GetSystemMetrics(SM_CXVIRTUALSCREEN);
		rcWa.bottom = rcWa.top + ::GetSystemMetrics(SM_CYVIRTUALSCREEN);
	}
	else
	{
		// On older systems we rely on GetWorkArea which doesn't support 
		// multiple monitors.
		::SystemParametersInfo(SPI_GETWORKAREA, NULL, &rcWa, NULL);
	}

	if(pos.x + winSize.cx > rcWa.right - rcWa.left)
		pos.x = rcWa.right - rcWa.left - winSize.cx;
	if(pos.x < rcWa.left)
		pos.x = rcWa.left;
	if(pos.y + winSize.cy > rcWa.bottom - rcWa.top)
		pos.y = rcWa.bottom - rcWa.top - winSize.cy;
	if(pos.y < rcWa.top)
		pos.y = rcWa.top;

	// Move when the pos changed only
	if(winPos.x != pos.x || winPos.y != pos.y)
		// note no SWP_SHOWWINDOW - this causes OnShowWindow not to be called on creation.
		SetWindowPos(NULL, pos.x, pos.y, 0, 0, SWP_NOSIZE | SWP_NOZORDER /*| SWP_SHOWWINDOW*/);
}

int CFindExDialog::positionChecks(int top, const UINT* checkboxIDs, int nCheckboxIDs)
{
	int chkDist = m_checkDist;

	CButton checkbox(GetDlgItem(checkboxIDs[0]).m_hWnd);
	ClientRect rc(checkbox, *this);
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
	DoDataExchange(TRUE);

	int restTop(0);
	const UINT* checkboxes(NULL);
	int nCheckboxes(0);

	if(m_type == m_lastType)
		return;

	m_lastType = m_type;

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

		CWindow prevCheckBox(GetDlgItem(CHECKBOX_CONTROL_IDS[CHECKBOX_CONTROL_COUNT-1]));
		ClientRect rcLastCheck(prevCheckBox, *this);
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
			CButton(GetDlgItem(IDC_BROWSE_BUTTON)).ShowWindow(SW_HIDE);

			CButton(GetDlgItem(IDC_MARKALL_BUTTON)).ShowWindow(SW_SHOW);

			CButton(GetDlgItem(IDC_CURRENTDOC_RADIO)).EnableWindow(TRUE);
			CButton(GetDlgItem(IDC_INSELECTION_RADIO)).EnableWindow(FALSE);
			CButton(GetDlgItem(IDC_ALLOPEN_RADIO)).EnableWindow(TRUE);
			
			restTop = m_group1Bottom + 12;

			if(m_SearchWhere != extensions::elwCurrentDoc && m_SearchWhere != extensions::elwAllDocs)
				m_SearchWhere = extensions::elwCurrentDoc;
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
			CButton(GetDlgItem(IDC_BROWSE_BUTTON)).ShowWindow(SW_HIDE);

			CButton(GetDlgItem(IDC_CURRENTDOC_RADIO)).EnableWindow(TRUE);
			CButton(GetDlgItem(IDC_INSELECTION_RADIO)).EnableWindow(TRUE);
			CButton(GetDlgItem(IDC_ALLOPEN_RADIO)).EnableWindow(FALSE);

			restTop = m_group2Bottom + 12;

			if(m_SearchWhere != extensions::elwCurrentDoc && m_SearchWhere != extensions::elwSelection)
				m_SearchWhere = extensions::elwCurrentDoc;
		}
		break;

	case eftFindInFiles:
		{
			checkboxes = FINDINFILES_CHECKBOXES;
			nCheckboxes = nFINDINFILES_CHECKBOXES;

			CStatic(GetDlgItem(IDC_REPLACETEXT_LABEL)).ShowWindow(SW_HIDE);
			m_ReplaceTextCombo.ShowWindow(SW_HIDE);
			m_ReHelperBtn2.ShowWindow(SW_HIDE);

			CButton(GetDlgItem(IDC_MARKALL_BUTTON)).ShowWindow(SW_HIDE);
			CButton(GetDlgItem(IDC_REPLACE_BUTTON)).ShowWindow(SW_HIDE);
			CButton(GetDlgItem(IDC_REPLACEALL_BUTTON)).ShowWindow(SW_HIDE);
			CButton(GetDlgItem(IDC_BROWSE_BUTTON)).ShowWindow(SW_SHOW);

			CStatic(GetDlgItem(IDC_FINDWHERE_LABEL)).ShowWindow(SW_SHOW);
			m_FindWhereCombo.ShowWindow(SW_SHOW);
			CStatic(GetDlgItem(IDC_FINDTYPE_LABEL)).ShowWindow(SW_SHOW);
			m_FindTypeCombo.ShowWindow(SW_SHOW);

			CButton(GetDlgItem(IDC_CURRENTDOC_RADIO)).EnableWindow(FALSE);
			CButton(GetDlgItem(IDC_INSELECTION_RADIO)).EnableWindow(FALSE);
			CButton(GetDlgItem(IDC_ALLOPEN_RADIO)).EnableWindow(FALSE);

			restTop = m_group3Bottom + 12;

			if(m_SearchWhere != extensions::elwCurrentDoc)
				m_SearchWhere = extensions::elwCurrentDoc;
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

	// Put settings back normalizing radio selections...
	DoDataExchange();

	// Make sure the right buttons are enabled
	enableButtons();
}

bool CFindExDialog::selectionIsWholeLine(CTextView* textView)
{
	int selLength = textView->GetSelLength();
	bool bShouldReplaceInSel = false;
	if(selLength > 1024)
	{
		bShouldReplaceInSel = true;
	}
	else if(selLength > 0)
	{
		Scintilla::CharacterRange cr;
		textView->GetSel(cr);

		int lineStart = textView->LineFromPosition(cr.cpMin);
		int lineEnd = textView->LineFromPosition(cr.cpMax);

		int posLineStart = textView->PositionFromLine(lineStart);

		if(lineStart == lineEnd)
		{
			if(textView->GetLineEndPosition(lineStart) == cr.cpMax &&
				posLineStart == cr.cpMin)
				bShouldReplaceInSel = true;
			else
			{
				// The selection is inside one line, but does it span from the first
				// non-whitespace char to the last?
				int lineLength = textView->LineLength(lineStart);
				char* lineBuf = new char[lineLength+1];
				lineBuf[lineLength] = '\0';
				textView->GetLine(lineStart, lineBuf);

				int firstNonWSChar = strFirstNonWS(lineBuf);
				if(cr.cpMin <= (firstNonWSChar + posLineStart))
				{
					int lastNonWSChar = strLastNonWSChar(lineBuf, lineLength);
					if(cr.cpMax >= (lastNonWSChar + posLineStart))
						bShouldReplaceInSel = true;
				}

				delete [] lineBuf;
			}
		}
		else
		{
			// Sel spans multiple lines
			bShouldReplaceInSel = true;
		}
	}
	
	return bShouldReplaceInSel;
}

// Stub for dynamically loaded SetLayered...
BOOL CFindExDialog::setLayeredWindowAttributes(HWND hwnd, COLORREF crKey, BYTE bAlpha, DWORD dwFlags)
{
	if(m_pFnSLWA != NULL)
	{
		return m_pFnSLWA(hwnd, crKey, bAlpha, dwFlags);
	}
	else
		return TRUE;
}