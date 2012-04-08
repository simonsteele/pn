/**
 * @file textclipsview.cpp
 * @brief View to display text clips.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"

#include <algorithm>

#include "resource.h"
#include "textclips.h"
#include "textclips/clipmanager.h"
#include "textclipsview.h"
#include "textclipeditor.h"
#include "childfrm.h"
#include "pndialogs.h"
#include "include/encoding.h"
#include "textclips/variables.h"
#include "scriptregistry.h"

///////////////////////////////////////////////////////////////////////////////////////////////
// Our Toolbar Bits

#define TOOLBAR_HEIGHT 22
#define TOOLBAR_BUTTON_COUNT 5
#define TOOLBAR_BUTTON_SIZE 16
#define TOOLBAR_WIDTH TOOLBAR_BUTTON_SIZE * TOOLBAR_BUTTON_COUNT

namespace {

// TODO: Can we store the tooltip text with the same ids as the buttons?

TBBUTTON TOOLBAR_BUTTONS[TOOLBAR_BUTTON_COUNT] = 
{
	{ 0, ID_CLIPS_ADD, TBSTATE_ENABLED, BTNS_BUTTON, 0, 0, 0 },
	{ 4, ID_CLIPS_EDIT, 0, BTNS_BUTTON, 0, 0, 0 },
	{ 1, ID_CLIPS_REMOVE, 0, BTNS_BUTTON, 0, 0, 0 },
	{ 2, ID_CLIPS_ADDSET, TBSTATE_ENABLED, BTNS_BUTTON, 0, 0, 0 },
	{ 3, ID_CLIPS_REMOVESET, 0, BTNS_BUTTON, 0, 0, 0 },
};

}

///////////////////////////////////////////////////////////////////////////////////////////////
// CClipsDocker

CClipsDocker::CClipsDocker(TextClips::TextClipsManager* manager) : m_hWndToolBar(NULL), m_pTheClips(manager), m_hImgList(NULL), m_hLastItem(NULL)
{
}

CClipsDocker::~CClipsDocker()
{
	if (m_hImgList)
	{
		ImageList_Destroy(m_hImgList);
	}
}

LRESULT CClipsDocker::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	HICON hIconSmall = (HICON)::LoadImage(_Module.GetResourceInstance(), MAKEINTRESOURCE(IDI_TEXTCLIPS), 
			IMAGE_ICON, ::GetSystemMetrics(SM_CXSMICON), ::GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);
	SetIcon(hIconSmall, FALSE);

	RECT rc;
	TEXTMETRIC tm;

	GetClientRect(&rc);

	HDC hdc = GetDC();
	HFONT hFont = ((HFONT)GetStockObject( DEFAULT_GUI_FONT ));
	HFONT hOldFont = static_cast<HFONT>( ::SelectObject(hdc, hFont) );
	
	GetTextMetrics(hdc, &tm);
	::SelectObject(hdc, hOldFont);
	
	int fontHeight = tm.tmHeight;
	m_comboHeight = MulDiv(12, fontHeight, 8); // 12 dialog units - default height.
	
	CRect rcCombo(rc);
	rcCombo.bottom = rcCombo.top + (m_comboHeight * 8); // what value here?
	rc.top += m_comboHeight;
	rc.bottom -= TOOLBAR_HEIGHT;

	m_tv.Create(m_hWnd, rc, _T("ClipsTree"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | TVS_FULLROWSELECT | TVS_NOHSCROLL, 0, IDC_CLIPSLIST);
	m_tv.SetIndent(0);
	m_tv.ShowWindow(SW_SHOW);
	m_tv.SetExtendedStyle(TVS_EX_DOUBLEBUFFER, TVS_EX_DOUBLEBUFFER);
	m_tv.SetItemHeight(m_tv.GetItemHeight() + 4);

	m_combo.Create(m_hWnd, rcCombo, _T("ClipsCombo"), WS_CHILD | WS_VISIBLE | WS_VSCROLL | WS_TABSTOP | CBS_DROPDOWNLIST | CBS_SORT, 0, IDC_CLIPSCOMBO);
	m_combo.SetFont( static_cast<HFONT> (GetStockObject( DEFAULT_GUI_FONT )) );
	
	setupToolbar();
	setupView(); // Fill Combo

	return 0;
}

LRESULT CClipsDocker::OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = FALSE;

	saveView();

	return 0;
}

LRESULT CClipsDocker::OnSize(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	if(wParam != SIZE_MINIMIZED )
	{
		RECT rc;
		GetClientRect(&rc);
		CRect rcCombo(rc);
		rcCombo.bottom = rcCombo.top + m_comboHeight;
		rc.top += m_comboHeight + 1;
		rc.bottom -= TOOLBAR_HEIGHT;

		m_tv.SetWindowPos(NULL, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top, SWP_NOZORDER | SWP_NOACTIVATE);
		m_combo.SetWindowPos(NULL, rcCombo.left, rcCombo.top, rcCombo.right - rcCombo.left, rcCombo.bottom - rcCombo.top, SWP_NOZORDER | SWP_NOACTIVATE);
		::SetWindowPos(m_hWndToolBar, NULL, rc.left, rc.bottom, rc.right - rc.left, rc.bottom + TOOLBAR_HEIGHT, SWP_NOZORDER | SWP_NOACTIVATE);
	}

	bHandled = FALSE;

	return 0;
}

/**
 * For some reason the WM_CTLCOLORLISTBOX message from the combo is not getting
 * a satisfactory result somewhere resulting in a black background. This fixes that.
 */
LRESULT CClipsDocker::OnCtlColor(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	/*COMBOBOXINFO cbi;
	cbi.cbSize = sizeof(COMBOBOXINFO);
	m_combo.GetComboBoxInfo(&cbi);
	if((HWND)lParam == cbi.hwndList)*/
	
	CDCHandle dc( (HDC) wParam );

	dc.SetTextColor( ::GetSysColor(COLOR_WINDOWTEXT) );
	dc.SetBkColor( ::GetSysColor(COLOR_WINDOW) );
	
	return (LRESULT)::GetSysColorBrush( COLOR_WINDOW );
}

LRESULT CClipsDocker::OnGetMinMaxInfo(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	LPMINMAXINFO mmi = reinterpret_cast<LPMINMAXINFO>(lParam);
	mmi->ptMinTrackSize.x = 80;
	mmi->ptMinTrackSize.y = 100;
	return 0;
}

/**
 * This is used to avoid the focus capture that happens in NM_DBLCLK, we post PN_SETFOCUS to ourselves
 * and set the focus from there.
 */
LRESULT CClipsDocker::OnSetEditorFocus(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	::LockWindowUpdate(::GetParent((HWND)lParam));
	::SetFocus((HWND)lParam);
	::LockWindowUpdate(NULL);
	return 0;
}

LRESULT	CClipsDocker::OnContextMenu(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	// If this is from a keyboard press...
	if(GET_X_LPARAM(lParam) == -1 && GET_Y_LPARAM(lParam) == -1)
	{
		CRect rc;
		m_hLastItem = m_tv.GetSelectedItem();
		m_tv.GetItemRect(m_hLastItem, &rc, TRUE);
		CPoint pt(rc.right, rc.top);
		m_tv.ClientToScreen(&pt);
		doContextMenu(&pt);
	}
	else
	{
		CPoint pt(GetMessagePos());
		handleRightClick(&pt);
	}

	return 0;
}

LRESULT CClipsDocker::OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	//Hide();

	return 0;
}

LRESULT CClipsDocker::OnComboSelChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int index = m_combo.GetCurSel();
	Scheme* pSet = reinterpret_cast<Scheme*>( m_combo.GetItemDataPtr(index) );

	if( pSet != NULL )
	{
		LoadSet(pSet);
	}

	return 0;
}

/**
 * Insert a selected clip into the current text
 */
LRESULT CClipsDocker::OnClipSelected(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	HTREEITEM hSel = m_tv.GetSelectedItem();
	if (hSel == NULL)
	{
		return 0;
	}

	TextClips::Clip* clip = m_tv.GetClip(hSel);
	
	if(clip != NULL)
	{
		InsertClip(clip);
		return 1;
	}

	return 0;
}

/**
 * Insert a selected clip into the current text, keyboard selection.
 */
LRESULT CClipsDocker::OnClipEnterPressed(int idCtrl, LPNMHDR pnmh, BOOL& bHandled)
{
	return OnClipSelected(idCtrl, pnmh, bHandled);
}

LRESULT CClipsDocker::OnClipGetInfoTip(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	// LPNMLVGETINFOTIP pGetInfoTip = (LPNMLVGETINFOTIP)pnmh;
	
	/*TextClips::Clip* clip = reinterpret_cast<TextClips::Clip*>( m_view.GetItemData(pGetInfoTip->iItem) );
	if(clip)
	{
		tstring str;

		CA2CT clipText(clip->Text.c_str());

		if(pGetInfoTip->dwFlags == 0)
		{
			str = pGetInfoTip->pszText;
			str += _T(":\n");
			str += clipText;
		}
		else
		{
			str = clipText;
		}

		if( str.size() >= (size_t)(pGetInfoTip->cchTextMax - 3) )
		{
			str.resize(pGetInfoTip->cchTextMax-4);
			str += _T("...");
		}
		
		_tcscpy(pGetInfoTip->pszText, str.c_str());
	}*/

	return 0;
}

LRESULT CClipsDocker::OnClipSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMTREEVIEW pnmtv = reinterpret_cast<LPNMTREEVIEW>(pnmh);

	bool haveClipSelected = pnmtv->itemNew.hItem != NULL && pnmtv->itemNew.lParam != 0;
	bool haveSetSelected = !haveClipSelected && pnmtv->itemNew.hItem != NULL;
	::SendMessage(m_hWndToolBar, TB_ENABLEBUTTON, ID_CLIPS_EDIT, haveClipSelected);
	::SendMessage(m_hWndToolBar, TB_ENABLEBUTTON, ID_CLIPS_REMOVE, haveClipSelected);
	::SendMessage(m_hWndToolBar, TB_ENABLEBUTTON, ID_CLIPS_REMOVESET, haveSetSelected);
	return 0;
}

/**
 * Get the tooltip text for each item.
 */
LRESULT CClipsDocker::OnToolbarGetInfoTip(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMTBGETINFOTIP pGetInfoTip = (LPNMTBGETINFOTIP)pnmh;

	tstring str = LS(pGetInfoTip->iItem);

	if (str.size() >= static_cast<size_t>(pGetInfoTip->cchTextMax))
	{
		str.resize(pGetInfoTip->cchTextMax - 4);
		str += _T("...");
	}

	_tcscpy(pGetInfoTip->pszText, str.c_str());

	return 0;
}

LRESULT CClipsDocker::OnAdd(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CTextClipEditor dlg(std::string(""), std::string(""), tstring(_T("")));
	if (dlg.DoModal() != IDOK)
	{
		return 0;
	}

	TextClips::Clip* clip = new TextClips::Clip(dlg.GetHint(), dlg.GetShortcut(), dlg.GetText());

	TextClips::TextClipSet* set(NULL);
	HTREEITEM hSelected = m_tv.GetSelectedItem();
	HTREEITEM hParent = TVI_ROOT;
	if (hSelected == NULL)
	{
		// No selection, add to root.
		set = getOrCreateSet(NULL);
	}

	if (m_tv.GetItemData(hSelected) == NULL)
	{
		// Set selected
		set = getSetFromSetItem(hSelected);
		hParent = hSelected;
	}
	else
	{
		// Item, get the set and find the parent item (or TVI_ROOT)
		set = getSetForItem(hSelected, hParent);
	}

	set->Add(clip);
	set->Save();

	m_tv.AddClip(clip, hParent);

	if (hParent != TVI_ROOT)
	{
		m_tv.Expand(hParent, TVE_EXPAND);
	}

	m_tv.SortSets();

	return 0;
}

LRESULT CClipsDocker::OnEdit(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	HTREEITEM hSelected = m_hLastItem != NULL ? m_hLastItem : m_tv.GetSelectedItem();
	
	if (hSelected == NULL)
	{
		return 0;
	}

	TextClips::Clip* clip = m_tv.GetClip(hSelected);

	if (clip == NULL)
	{
		return 0;
	}

	CTextClipEditor dlg(clip->Shortcut, clip->Text, clip->Name);
	if (dlg.DoModal() != IDOK)
	{
		return 0;
	}

	clip->Shortcut = dlg.GetShortcut();
	clip->Text = dlg.GetText();
	clip->Name = dlg.GetHint();

	TextClips::TextClipSet* set = getSetForItem(hSelected);
	set->Save();

	m_tv.SetItemText(hSelected, clip->Name.c_str());

	return 0;
}

LRESULT CClipsDocker::OnRemove(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	HTREEITEM hSelected = m_hLastItem != NULL ? m_hLastItem : m_tv.GetSelectedItem();
	
	if (hSelected == NULL)
	{
		return 0;
	}

	TextClips::Clip* clip(m_tv.GetClip(hSelected));
	if (clip == NULL)
	{
		return 0;
	}

	TextClips::TextClipSet* set = getSetForItem(hSelected);
	set->Remove(clip);
	delete clip;
	set->Save();
	m_tv.DeleteItem(hSelected);

	return 0;
}

LRESULT CClipsDocker::OnAddSet(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CInputDialog ib(LS(IDS_ADDCLIPSETTITLE), LS(IDS_ADDCLIPSETCAPTION));
	if (ib.DoModal(g_Context.m_frame->GetWindow()->m_hWnd) == IDOK)
	{
		if(ib.GetInput() == NULL)
		{
			return 0;
		}

		// Create the set, ignoring this if the name is a duplicate:
		getOrCreateSet(ib.GetInput());
	}
	
	return 0;
}

LRESULT CClipsDocker::OnRemoveSet(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	HTREEITEM hSelected = m_tv.GetSelectedItem();

	if (hSelected == NULL)
	{
		return 0;
	}

	// Item data must be null for a set.
	TextClips::Clip* clip = m_tv.GetClip(hSelected);
	if (clip != NULL)
	{
		return 0;
	}

	TextClips::TextClipSet* set = getSetFromSetItem(hSelected);
	m_tv.RemoveSet(set, hSelected);
	m_pTheClips->Delete(set);

	return 0;
}

void CClipsDocker::Reset()
{
	saveView();

	m_tv.DeleteAllItems();
	m_combo.Clear();

	setupView();
}

void CClipsDocker::LoadSet(Scheme* scheme)
{
	m_tv.DeleteAllItems();
	const TextClips::LIST_CLIPSETS& sets = m_pTheClips->GetClips(scheme->GetName());

	BOOST_FOREACH(TextClips::TextClipSet* set, sets)
	{
		LPCTSTR setName(set->GetName());
		HTREEITEM parent;
		if (setName && setName[0])
		{
			parent = m_tv.AddSet(set);
		}
		else
		{
			parent = TVI_ROOT;
		}

		const TextClips::LIST_CLIPS& clips = set->GetClips();
			
		for (TextClips::LIST_CLIPS::const_iterator i = clips.begin(); i != clips.end(); ++i)
		{
			m_tv.AddClip((*i), parent);
		}

		if (parent != TVI_ROOT)
		{
			m_tv.Expand(parent);
		}
	}

	m_tv.SortSets();
}

void CClipsDocker::InsertClip(TextClips::Clip* tc)
{
	CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor());
	
	if(pChild)
	{
		CTextView* pS = pChild->GetTextView();
		if(!pS)
		{
			return;
		}

		pS->InsertClip(tc);
		
		PostMessage(PN_SETFOCUS, 0, reinterpret_cast<LPARAM>(pS->m_hWnd));
	}
}

void CClipsDocker::saveView()
{
	int index = m_combo.GetCurSel();
	if (index == -1)
	{
		return;
	}

	Scheme* pScheme = reinterpret_cast<Scheme*>( m_combo.GetItemDataPtr(index) );
	Windows1252_Tcs schemeName(pScheme->GetName());
	OPTIONS->Set(PNSK_INTERFACE, _T("LastClipsScheme"), schemeName);
}

void CClipsDocker::setupView()
{
	SchemeManager* pM = SchemeManager::GetInstance();
	SCHEME_LIST* pSchemes = pM->GetSchemesList();

	// Text to select after populating combo:
	Tcs_Windows1252 lastClipsScheme(OPTIONS->Get(PNSK_INTERFACE, _T("LastClipsScheme"), _T("")).c_str());
	std::string schemeToSelect(lastClipsScheme);
	tstring selectText;

	// Add the schemes:
	int index = m_combo.AddString(pM->GetDefaultScheme()->GetTitle());
	m_combo.SetItemDataPtr(index, pM->GetDefaultScheme());

	for(SCIT i = pSchemes->begin(); i != pSchemes->end(); ++i)
	{
		index = m_combo.AddString((*i).GetTitle());
		m_combo.SetItemDataPtr(index, &(*i));

		// See if this was the previous selection:
		if(schemeToSelect == (*i).GetName())
		{
			selectText = (*i).GetTitle();
		}
	}

	if (selectText.size())
	{
		m_combo.SelectString(0, selectText.c_str());
	}
	else
	{
		// Default:
		m_combo.SelectString(0, pM->GetDefaultScheme()->GetTitle());
	}

	BOOL ignored;
	OnComboSelChange(0, 0, 0, ignored);
}

void CClipsDocker::setupToolbar()
{
	CToolBarCtrl toolbar;

	bool lowColour = !IsXPOrLater() || OPTIONS->Get(PNSK_INTERFACE, _T("LowColourToolbars"), false);

	CImageList imglist;
	HBITMAP bmp;
	
	if (lowColour)
	{
		imglist.Create(16, 16, ILC_COLOR24 | ILC_MASK, TOOLBAR_BUTTON_COUNT, 1);
		bmp = static_cast<HBITMAP>(::LoadImage(ATL::_AtlBaseModule.GetResourceInstance(), MAKEINTRESOURCE(IDB_TBCLIPS24), IMAGE_BITMAP, TOOLBAR_WIDTH, TOOLBAR_BUTTON_SIZE, LR_SHARED));
	}
	else
	{
		imglist.Create(16, 16, ILC_COLOR32 | ILC_MASK, TOOLBAR_BUTTON_COUNT, 1);
		bmp = static_cast<HBITMAP>(::LoadImage(ATL::_AtlBaseModule.GetResourceInstance(), MAKEINTRESOURCE(IDB_TBCLIPS), IMAGE_BITMAP, TOOLBAR_WIDTH, TOOLBAR_BUTTON_SIZE, LR_SHARED | LR_CREATEDIBSECTION));
	}

	imglist.Add(bmp, RGB(255, 0, 255));

	m_hImgList = imglist.Detach();

	CRect rc;
	GetClientRect(rc);
	rc.top = rc.bottom - TOOLBAR_HEIGHT;

	// We fix the size of the mini toolbar to make it suitably small (see TOOLBAR_HEIGHT).
	DWORD dwStyle = WS_CHILD | WS_VISIBLE | CCS_BOTTOM | CCS_NODIVIDER | TBSTYLE_TOOLTIPS | CCS_NORESIZE | TBSTYLE_FLAT;

	toolbar.Create(m_hWnd, rc, NULL, dwStyle, 0, IDC_CLIPSTOOLBAR);
	toolbar.SetButtonStructSize();
	toolbar.SetBitmapSize(CSize(TOOLBAR_BUTTON_SIZE, TOOLBAR_BUTTON_SIZE));
	toolbar.SetImageList(m_hImgList);

	toolbar.AddButtons(TOOLBAR_BUTTON_COUNT, &TOOLBAR_BUTTONS[0]);
	toolbar.SetButtonSize(CSize(20, 20));

	m_hWndToolBar = toolbar.Detach();
}

/**
 * Get a clip set for the selected clip tree item.
 */
TextClips::TextClipSet* CClipsDocker::getSetForItem(HTREEITEM item)
{
	HTREEITEM hParent(NULL);
	return getSetForItem(item, hParent);
}

/**
 * Get a clip set for the selected clip tree item, and also return the 
 * HTREEITEM for the set where it exists.
 */
TextClips::TextClipSet* CClipsDocker::getSetForItem(HTREEITEM item, HTREEITEM& hParent)
{
	hParent = m_tv.GetParentItem(item);
	if (hParent == NULL)
	{
		hParent = TVI_ROOT;
		return getOrCreateSet(NULL);
	}
	else
	{
		return getSetFromSetItem(hParent);
	}	
}

/**
 * Get a clip set from the set tree item
 */
TextClips::TextClipSet* CClipsDocker::getSetFromSetItem(HTREEITEM setItem)
{
	CString str;
	m_tv.GetItemText(setItem, str);
	return getOrCreateSet(str);
}

/**
 * Get a set by name, and if it doesn't exist create it.
 */
TextClips::TextClipSet* CClipsDocker::getOrCreateSet(LPCTSTR title)
{
	int index = m_combo.GetCurSel();
	if (index == -1)
	{
		RETURN_UNEXPECTED(_T("GetOrCreateSet called with no scheme"), NULL);
	}

	Scheme* scheme = reinterpret_cast<Scheme*>( m_combo.GetItemDataPtr(index) );
	
	const TextClips::LIST_CLIPSETS& sets = m_pTheClips->GetClips(scheme->GetName());

	BOOST_FOREACH(TextClips::TextClipSet* set, sets)
	{
		LPCTSTR setName = set->GetName();

		if (title == NULL)
		{
			if (setName == NULL || setName[0] == NULL)
			{
				return set;
			}
		}
		else
		{
			if (_tcscmp(setName, title) == 0)
			{
				return set;
			}
		}
	}

	// We didn't find the set:
	TextClips::TextClipSet* newSet = new TextClips::TextClipSet(_T(""), title, scheme->GetName(), false);
	m_pTheClips->Add(newSet);
	if (title != NULL && title[0])
	{
		// This is a named Clip Set, add an item for it
		m_tv.AddSet(newSet);
		m_tv.SortSets();
	}

	return newSet;
}

void CClipsDocker::handleRightClick(LPPOINT pt)
{
	//CPoint pt(GetMessagePos());
	CPoint pt2(*pt);

	// Test for keyboard right-click...
	if(pt->x != -1)
	{
		m_tv.ScreenToClient(&pt2);

		TVHITTESTINFO tvhti;
		memset(&tvhti, 0, sizeof(TV_HITTESTINFO));
		
		tvhti.pt = pt2;
		m_tv.HitTest(&tvhti);

		m_hLastItem = NULL;

		if(tvhti.hItem != NULL)
		{
			if (tvhti.flags & (TVHT_ONITEM|TVHT_ONITEMRIGHT))
			{
				m_hLastItem = tvhti.hItem;
			}
		}
	}

	doContextMenu(pt);
}

void CClipsDocker::doContextMenu(LPPOINT pt)
{
	if (m_hLastItem != NULL)
	{
		CSPopupMenu popup(IDR_POPUP_TEXTCLIPS);
		DeleteMenu(popup.GetHandle(), ID_DUMMY_EXPLORER, MF_BYCOMMAND);

		g_Context.m_frame->TrackPopupMenu(popup, 0, pt->x, pt->y, NULL, m_hWnd);

		m_hLastItem = NULL;
	}
}