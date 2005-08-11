/**
 * @file textclipsview.h
 * @brief View to display text clips.
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef textclipsview_h__included
#define textclipsview_h__included

namespace TextClips {
	class Clip;
	class TextClipsManager;
	class TextClipSet;
}

class CClipsDocker : public CWindowImpl<CClipsDocker>//CPNDockingWindow<CClipsDocker>
{
	typedef CClipsDocker thisClass;
	//typedef CPNDockingWindow<CClipsDocker> baseClass;
	typedef CWindowImpl<CClipsDocker> baseClass;

public:
	DECLARE_WND_CLASS(_T("CClipsDocker"))

	CClipsDocker();
	~CClipsDocker();

	enum {
		IDC_CLIPSLIST = 100,
		IDC_CLIPSCOMBO = 101,
	};

	BEGIN_MSG_MAP(thisClass)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_SIZE, OnSize)
		MESSAGE_HANDLER(WM_CTLCOLORLISTBOX, OnCtlColor)
		MESSAGE_HANDLER(WM_CTLCOLOREDIT, OnCtlColor)
		COMMAND_ID_HANDLER(ID_OUTPUT_HIDE, OnHide)
		COMMAND_HANDLER(IDC_CLIPSCOMBO, CBN_SELCHANGE, OnComboSelChange)
		NOTIFY_HANDLER(IDC_CLIPSLIST, NM_DBLCLK, OnClipSelected);
		REFLECT_NOTIFICATIONS()
		//CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

protected:
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);
	LRESULT OnCtlColor(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);
	LRESULT OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	
	LRESULT OnComboSelChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnClipSelected(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	void AddClip(TextClips::Clip* tc);
	void LoadSet(TextClips::TextClipSet* set);

	CListViewCtrl	m_view;
	CComboBox		m_combo;
	TextClips::TextClipsManager* m_pTheClips;

	int m_comboHeight;
};

#endif