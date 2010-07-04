/**
 * @file optionspageclips.h
 * @brief Options Dialog Clips Page for Programmer's Notepad 2
 * @author Simon Steele
 * @note Copyright (c) 2007 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef optionspageclips_h__included
#define optionspageclips_h__included

#include "include/optionsdialog.h"

namespace TextClips { class TextClipsManager; class TextClipSet; }

/**
 * Autocomplete options page
 */
class COptionsPageClips : public COptionsPageImpl<COptionsPageClips>
{
	public:
		COptionsPageClips(SchemeConfigParser* pSchemes, TextClips::TextClipsManager* pClipManager);
		~COptionsPageClips();

		BEGIN_MSG_MAP(COptionsPageClips)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			NOTIFY_HANDLER(IDC_CLIPLIST, LVN_ITEMCHANGED, OnClipSelChanged)
			COMMAND_HANDLER(IDC_SCHEMECOMBO, CBN_SELCHANGE, OnSchemeComboChange)
			COMMAND_HANDLER(IDC_CLIPS_ADDBUTTON, BN_CLICKED, OnAddClicked)
			COMMAND_HANDLER(IDC_CLIPS_EDITBUTTON, BN_CLICKED, OnEditClipClicked)
			COMMAND_HANDLER(IDC_CLIPS_REMOVEBUTTON, BN_CLICKED, OnRemoveClicked)
			REFLECT_NOTIFICATIONS()
		END_MSG_MAP()

		enum { IDD = IDD_PAGE_KEYWORDTEXTCLIPS };

		virtual void OnOK();
		virtual void OnInitialise();
		virtual tstring GetTreePosition();
		virtual void OnCancel();

		bool IsDirty() const;

	private:
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
		LRESULT OnClipSelChanged(int /*idCtrl*/, LPNMHDR pNMHDR, BOOL& /*bHandled*/);
		LRESULT OnSchemeComboChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnAddClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnEditClipClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
		LRESULT OnRemoveClicked(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

		void updateSel();
		void updateSelectedClip();

		bool							m_dirty;
		CSchemeCombo					m_combo;
		CListViewCtrl					m_list;
		CScintillaDialogWnd				m_scintilla;
		SchemeConfigParser*				m_pSchemes;
		TextClips::TextClipsManager*	m_pClips;
		TextClips::TextClipsManager*	m_pOriginalClips;
		TextClips::TextClipSet*			m_pCurSet;
};

#endif optionspageclips_h__included