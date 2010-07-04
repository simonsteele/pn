/**
 * @file optionspageclips.cpp
 * @brief Options Dialog Clips Page for Programmer's Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2007-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "OptionsDialogs.h"
#include "OptionsPageClips.h"
#include "textclips.h"
#include "textclips/clipmanager.h"
#include "textclipeditor.h"
#include "schemeconfig.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

COptionsPageClips::COptionsPageClips(SchemeConfigParser* pSchemes, TextClips::TextClipsManager* pClipManager) : 
	m_pSchemes(pSchemes),
	m_pOriginalClips(pClipManager),
	m_dirty(false),
	m_pCurSet(NULL)
{
	m_pClips = new TextClips::TextClipsManager(*m_pOriginalClips);
}

COptionsPageClips::~COptionsPageClips()
{
	delete m_pClips;
}

void COptionsPageClips::OnOK()
{
	if(m_dirty)
	{
		m_pOriginalClips->Reset(*m_pClips);
		m_pOriginalClips->Save();
	}
}

void COptionsPageClips::OnInitialise()
{
	
}

tstring COptionsPageClips::GetTreePosition()
{
	return MAKE_OPTIONSTREEPATH(IDS_OPTGROUP_GENERAL, IDS_OPTPAGE_CODETEMPLATES);
}

void COptionsPageClips::OnCancel()
{
}

bool COptionsPageClips::IsDirty() const
{
	return m_dirty;
}

LRESULT COptionsPageClips::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_combo.Attach(GetDlgItem(IDC_SCHEMECOMBO));
	m_list.Attach(GetDlgItem(IDC_CLIPLIST));

	m_combo.Load(m_pSchemes);

	CRect rcScintilla;
	::GetWindowRect(GetDlgItem(IDC_PLACEHOLDER), rcScintilla);
	ScreenToClient(rcScintilla);
	m_scintilla.Create(m_hWnd, rcScintilla, _T("ClipText"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_TABSTOP, WS_EX_STATICEDGE);
	::SetWindowPos(m_scintilla, GetDlgItem(IDC_PLACEHOLDER), 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
	m_scintilla.SetWrapMode(SC_WRAP_WORD);
	m_scintilla.AssignCmdKey(SCK_HOME, SCI_HOMEDISPLAY);
	m_scintilla.AssignCmdKey(SCK_END, SCI_LINEENDDISPLAY);
	m_scintilla.SetMarginWidthN(1, 0);
	
	// Stop scintilla from capturing the escape and tab keys...
	m_scintilla.ClearCmdKey(SCK_ESCAPE);
	m_scintilla.ClearCmdKey(SCK_TAB);

	SchemeManager::GetInstance()->GetDefaultScheme()->Load(m_scintilla);

	m_scintilla.SetReadOnly(true);

	CRect rc;
	m_list.GetClientRect(&rc);
	int wCol = rc.right - rc.left - 20 - 80;
	m_list.InsertColumn(0, LS(IDS_HDR_CLIPS_SHORTCUT), LVCFMT_LEFT, 80, 0);
	m_list.InsertColumn(1, LS(IDS_HDR_CLIPS_HINT), LVCFMT_LEFT, wCol, 0);
	m_list.SetExtendedListViewStyle(LVS_EX_FULLROWSELECT, LVS_EX_FULLROWSELECT);

	updateSel();

	return 0;
}

LRESULT COptionsPageClips::OnClipSelChanged(int /*idCtrl*/, LPNMHDR pNMHDR, BOOL& /*bHandled*/)
{
	updateSelectedClip();
	
	return 0;
}

LRESULT COptionsPageClips::OnSchemeComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	updateSel();

	return 0;
}

LRESULT COptionsPageClips::OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CTextClipEditor editor(std::string(""), std::string(""), tstring(_T("")));
	if (editor.DoModal() == IDOK)
	{
		TextClips::Clip* clip = new TextClips::Clip(editor.GetHint(), editor.GetShortcut(), editor.GetText());
		
		if(m_pCurSet == NULL)
		{
			SchemeDetails* sc = m_combo.GetItemScheme( m_combo.GetCurSel() );
			m_pCurSet = new TextClips::TextClipSet(NULL, _T(""), sc->Name.c_str(), false);
			m_pClips->Add(m_pCurSet);
			//TODO: Check this!
		}

		m_pCurSet->Add(clip);

		CA2CT shortcut(clip->Shortcut.c_str());

		int index = m_list.AddItem(m_list.GetItemCount(), 0, shortcut);
		m_list.SetItemData(index, reinterpret_cast<DWORD_PTR>(clip));

		m_list.SetItemText(index, 1, clip->Name.c_str());

		m_dirty = true;

		updateSelectedClip();
	}

	return 0;
}

LRESULT COptionsPageClips::OnEditClipClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if (m_list.GetSelectedIndex() == -1)
	{
		return 0;
	}

	TextClips::Clip* clip = reinterpret_cast<TextClips::Clip*>(m_list.GetItemData(m_list.GetSelectedIndex()));
	CTextClipEditor editor(clip->Shortcut, clip->Text, clip->Name);
	if (editor.DoModal() == IDOK)
	{
		clip->Shortcut = editor.GetShortcut();
		clip->Text = editor.GetText();
		clip->Name = editor.GetHint();

		CA2CT scconv(clip->Shortcut.c_str());
		
		m_list.SetItemText(m_list.GetSelectedIndex(), 0, scconv);
		m_list.SetItemText(m_list.GetSelectedIndex(), 1, clip->Name.c_str());

		m_dirty = true;

		updateSelectedClip();
	}

	return 0;
}

LRESULT COptionsPageClips::OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if (m_list.GetSelectedIndex() == -1)
	{
		return 0;
	}

	// Remove any selected items
	for(int i = m_list.GetItemCount() - 1; i >= 0; --i)
	{
		if (m_list.GetItemState(i, LVIS_SELECTED) != 0)
		{
			TextClips::Clip* clip = reinterpret_cast<TextClips::Clip*>(m_list.GetItemData(i));
			m_pCurSet->Remove(clip);
			delete clip;
			m_list.DeleteItem(i);
		}
	}

	m_dirty = true;

	updateSelectedClip();

	return 0;
}

void COptionsPageClips::updateSel()
{
	m_list.DeleteAllItems();
	m_pCurSet = NULL;

	SchemeDetails* pCurScheme = reinterpret_cast<SchemeDetails*>(m_combo.GetItemData(m_combo.GetCurSel()));
	if(pCurScheme != NULL)
	{
		TextClips::TextClipSet* schemeClips = m_pClips->GetClips(pCurScheme->Name.c_str()).front();
		if (schemeClips != NULL)
		{
			const TextClips::LIST_CLIPS clips = schemeClips->GetClips();
			for(TextClips::LIST_CLIPS::const_iterator i = clips.begin();
				i != clips.end();
				++i)
			{
				CA2CT shortcut((*i)->Shortcut.c_str());
				int index = m_list.AddItem(m_list.GetItemCount(), 0, shortcut);
				m_list.SetItemData(index, reinterpret_cast<DWORD_PTR>(*i));
				
				m_list.SetItemText(index, 1, (*i)->Name.c_str());
			}

			m_pCurSet = schemeClips;
		}
	}
}

void COptionsPageClips::updateSelectedClip()
{
	int sel = m_list.GetSelectedIndex();
	
	m_scintilla.SetReadOnly(false);
	
	if (sel == -1)
	{
		//GetDlgItem(IDC_SHORTCUT_STATIC).SetWindowText(_T(""));
		m_scintilla.SetText("");
	}
	else
	{
		TextClips::Clip* clip = reinterpret_cast<TextClips::Clip*>(m_list.GetItemData(sel));
		//GetDlgItem(IDC_SHORTCUT_STATIC).SetWindowText(clip->Shortcut.c_str());
		m_scintilla.SetText(clip->Text.c_str());
	}
	
	m_scintilla.SetReadOnly(true);
}
