/**
 * @file textclipsview.cpp
 * @brief View to display text clips.
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "textclips.h"
#include "textclipsview.h"
#include "childfrm.h"
#include <algorithm>

CClipsDocker::CClipsDocker()
{
	m_pTheClips = new TextClips::TextClipsManager();
}

CClipsDocker::~CClipsDocker()
{
	delete m_pTheClips;
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

	m_view.Create(m_hWnd, rc, _T("ClipsList"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | LVS_REPORT | LVS_NOCOLUMNHEADER | LVS_SINGLESEL, 0, IDC_CLIPSLIST);
	m_view.ShowWindow(SW_SHOW);
	m_view.SetExtendedListViewStyle(LVS_EX_FULLROWSELECT);

	m_view.InsertColumn(0, _T("Name"), LVCFMT_LEFT, rc.right - rc.left, 0);

	m_combo.Create(m_hWnd, rcCombo, _T("ClipsCombo"), WS_CHILD | WS_VISIBLE | WS_VSCROLL | WS_TABSTOP | CBS_DROPDOWNLIST | CBS_SORT, 0, IDC_CLIPSCOMBO);
	m_combo.SetFont( static_cast<HFONT> (GetStockObject( DEFAULT_GUI_FONT )) );
	
	// Fill the combo box.

	const TextClips::LIST_CLIPSETS& sets = m_pTheClips->GetClipSets();
	
	tstring lastSet = OPTIONS->Get(PNSK_INTERFACE, _T("LastClipSet"), _T(""));

	int selindex = 0;
	int index;
	for(TextClips::LIST_CLIPSETS::const_iterator i = sets.begin();
		i != sets.end();
		++i)
	{
		index = m_combo.InsertString(m_combo.GetCount(), (*i)->GetName());
		m_combo.SetItemDataPtr(index, *i);
		if(lastSet == (*i)->GetName())
			selindex = index;
	}

	if( sets.size() > 0 )
	{
		m_combo.SetCurSel(selindex);
		BOOL unneeded;
		OnComboSelChange(0, 0, NULL, unneeded);
	}

	return 0;
}

LRESULT CClipsDocker::OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = FALSE;

	CWindowText wt(m_combo.m_hWnd);
	LPCTSTR sztw = (LPCTSTR)wt;
	if(sztw && _tcslen(sztw) > 0)
		OPTIONS->Set(PNSK_INTERFACE, _T("LastClipSet"), sztw);

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

		m_view.SetWindowPos(NULL, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top ,SWP_NOZORDER | SWP_NOACTIVATE);
		m_view.SetColumnWidth(0, rc.right - rc.left - ::GetSystemMetrics(SM_CXVSCROLL) - 1);
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

	/*bHandled = FALSE;
	return 0;*/
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
	TextClips::TextClipSet* pSet = reinterpret_cast<TextClips::TextClipSet*>( 
		m_combo.GetItemDataPtr(index) );

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
	if( ((LPNMITEMACTIVATE)pnmh)->iItem == -1 )
		return 0;

	TextClips::Clip* clip = reinterpret_cast<TextClips::Clip*>( m_view.GetItemData(((LPNMITEMACTIVATE)pnmh)->iItem));
	
	CChildFrame* pChild = CChildFrame::FromHandle( GetCurrentEditor() );
	
	if(pChild)
	{
		CTextView* pS = pChild->GetTextView();
		if(!pS)
			return 0;

		tstring clipstr;
		// clip->Text is in Unix EOL mode (LF only), convert it to target EOL mode
		switch(pS->GetEOLMode())
		{
		case PNSF_Unix:
			// no conversion needed, just copy
			clipstr = clip->Text;
			break;

		case PNSF_Windows:
			{
				// heuristically reserve size for the target string
				// and copy the string by inserting '\r' where appropriate
				clipstr.reserve(clip->Text.size() + (clip->Text.size() / 16));
				tstring::const_iterator it = clip->Text.begin();
				tstring::const_iterator end = clip->Text.end();
				for(; it != end; ++it)
				{
					if(*it == '\n')
						clipstr += '\r';
					clipstr += *it;
				}
			}
			break;

		case PNSF_Mac:
			// reserve size for the target string and use standard algorithm
			clipstr.reserve(clip->Text.size());
			std::replace_copy(clip->Text.begin(), clip->Text.end(),
				std::back_inserter(clipstr), '\n', '\r');
			break;
		}
		
		size_t offset = clipstr.find(_T('|'));
		if(offset != clipstr.npos)
			clipstr.erase(offset, 1);
		else
			offset = 0;
		
		int curPos = pS->GetCurrentPos();
	
		int length = pS->GetSelLength();
		if(length != 0)
		{
			// Get the selected text from Scintilla.
			char* selData = new char[length+1];
			int rxlen = pS->GetSelText(selData);
			PNASSERT(rxlen == length+1);
			
			// Insert the text into the buffer.
			clipstr.insert(offset, selData);
			delete [] selData;
			
			// Adjust the offset to place the cursor after the selected text.
			offset += length;
		}

		// Wrap everything in an undo block.
		pS->BeginUndoAction();
		if(length)
			pS->DeleteBack(); // kill the selection text, we're inserting it again.
		pS->InsertText(curPos, clipstr.c_str());
		pS->SetCurrentPos(curPos + offset);
		pS->SetSel(curPos + offset, curPos + offset);
		pS->EndUndoAction();
		::SetFocus(pS->m_hWnd);
	}

	return 0;
}

void CClipsDocker::LoadSet(TextClips::TextClipSet* set)
{
	m_view.DeleteAllItems();

	const TextClips::LIST_CLIPS& clips = set->GetClips();
		
	for(TextClips::LIST_CLIPS::const_iterator i = clips.begin(); i != clips.end(); ++i)
	{
		AddClip(*i);
	}
}

inline void CClipsDocker::AddClip(TextClips::Clip* tc)
{
	int iIndex = m_view.InsertItem(m_view.GetItemCount(), tc->Name.c_str());
	m_view.SetItemData(iIndex, reinterpret_cast<DWORD_PTR>(tc));
}