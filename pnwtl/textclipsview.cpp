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
#include "resource.h"
#include "textclips.h"
#include "textclips/clipmanager.h"
#include "textclipsview.h"
#include "childfrm.h"
#include <algorithm>

CClipsDocker::CClipsDocker(TextClips::TextClipsManager* manager)
{
	m_pTheClips = manager;
}

CClipsDocker::~CClipsDocker()
{
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

	m_tv.Create(m_hWnd, rc, _T("ClipsTree"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | TVS_FULLROWSELECT | TVS_NOHSCROLL, 0, IDC_CLIPSLIST);
	m_tv.SetIndent(0);
	m_tv.ShowWindow(SW_SHOW);
	m_tv.SetExtendedStyle(TVS_EX_DOUBLEBUFFER, TVS_EX_DOUBLEBUFFER);

	m_combo.Create(m_hWnd, rcCombo, _T("ClipsCombo"), WS_CHILD | WS_VISIBLE | WS_VSCROLL | WS_TABSTOP | CBS_DROPDOWNLIST | CBS_SORT, 0, IDC_CLIPSCOMBO);
	m_combo.SetFont( static_cast<HFONT> (GetStockObject( DEFAULT_GUI_FONT )) );
	
	// Fill the combo box.
	setupView();

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

		m_tv.SetWindowPos(NULL, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top, SWP_NOZORDER | SWP_NOACTIVATE);
		m_combo.SetWindowPos(NULL, rcCombo.left, rcCombo.top, rcCombo.right - rcCombo.left, rcCombo.bottom - rcCombo.top, SWP_NOZORDER | SWP_NOACTIVATE);
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

LRESULT CClipsDocker::OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	//Hide();

	return 0;
}

LRESULT CClipsDocker::OnComboSelChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int index = m_combo.GetCurSel();
	//TextClips::TextClipSet* pSet = reinterpret_cast<TextClips::TextClipSet*>( m_combo.GetItemDataPtr(index) );
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

	TextClips::Clip* clip = reinterpret_cast<TextClips::Clip*>( m_tv.GetItemData(hSel));
	
	if(clip != NULL)
	{
		InsertClip(clip);
	}

	return 0;
}

LRESULT CClipsDocker::OnClipEnterPressed(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	/*int selIndex = m_view.GetSelectedIndex();
	if(selIndex >= 0)
	{
		TextClips::Clip* clip = reinterpret_cast<TextClips::Clip*>( m_view.GetItemData(selIndex) );
		
		if(clip)
			InsertClip(clip);
	}*/

	return 0;
}

LRESULT CClipsDocker::OnClipGetInfoTip(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMLVGETINFOTIP pGetInfoTip = (LPNMLVGETINFOTIP)pnmh;
	//pGetInfoTip->
	
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
			parent = m_tv.InsertItem(set->GetName(), TVI_ROOT, NULL);
		}
		else
		{
			parent = TVI_ROOT;
		}

		const TextClips::LIST_CLIPS& clips = set->GetClips();
			
		for (TextClips::LIST_CLIPS::const_iterator i = clips.begin(); i != clips.end(); ++i)
		{
			HTREEITEM clipItem = m_tv.InsertItem((*i)->Name.c_str(), parent, NULL);
			m_tv.SetItemData(clipItem, reinterpret_cast<DWORD_PTR>((*i)));
		}

		if (parent != TVI_ROOT)
		{
			m_tv.Expand(parent);
		}
	}
}

inline void CClipsDocker::AddClip(TextClips::Clip* tc)
{
	//int iIndex = m_view.InsertItem(m_view.GetItemCount(), tc->Name.c_str());
	//m_view.SetItemData(iIndex, reinterpret_cast<DWORD_PTR>(tc));
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

		std::vector<TextClips::Chunk> chunks;
		tc->GetChunks(chunks, pS);
		
		pS->SendMessage(PN_INSERTCLIP, 0, reinterpret_cast<LPARAM>(&chunks));
		
		::SetFocus(pS->m_hWnd);
	}
}

void CClipsDocker::saveView()
{
	/*CWindowText wt(m_combo.m_hWnd);
	LPCTSTR sztw = (LPCTSTR)wt;
	if(sztw && _tcslen(sztw) > 0)
		OPTIONS->Set(PNSK_INTERFACE, _T("LastClipSet"), sztw);*/
}

void CClipsDocker::setupView()
{
	SchemeManager* pM = SchemeManager::GetInstance();
	SCHEME_LIST* pSchemes = pM->GetSchemesList();

	int index = m_combo.AddString(pM->GetDefaultScheme()->GetTitle());
	m_combo.SetItemDataPtr( index, pM->GetDefaultScheme() );

	for(SCIT i = pSchemes->begin(); i != pSchemes->end(); ++i)
	{
		index = m_combo.AddString((*i).GetTitle());
		m_combo.SetItemDataPtr(index, &(*i));
	}
}