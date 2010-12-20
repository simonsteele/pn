/**
 * @file textclipsview.h
 * @brief View to display text clips.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef textclipsview_h__included
#define textclipsview_h__included

#include "controls/textclipstree.h"

namespace TextClips {
	class Clip;
	class TextClipsManager;
	class TextClipSet;
}

/**
 * Docking window for Text Clips
 */
class CClipsDocker : public CWindowImpl<CClipsDocker>
{
	typedef CClipsDocker thisClass;
	typedef CWindowImpl<CClipsDocker> baseClass;

public:
	DECLARE_WND_CLASS(_T("CClipsDocker"))

	CClipsDocker(TextClips::TextClipsManager* manager);
	~CClipsDocker();

	enum {
		IDC_CLIPSLIST = 100,
		IDC_CLIPSCOMBO = 101,
		IDC_CLIPSTOOLBAR = 102
	};

	BEGIN_MSG_MAP(thisClass)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_DESTROY, OnClose)
		MESSAGE_HANDLER(WM_SIZE, OnSize)
		MESSAGE_HANDLER(WM_CTLCOLORLISTBOX, OnCtlColor)
		MESSAGE_HANDLER(WM_CTLCOLOREDIT, OnCtlColor)
		MESSAGE_HANDLER(WM_GETMINMAXINFO, OnGetMinMaxInfo)
		MESSAGE_HANDLER(PN_SETFOCUS, OnSetEditorFocus)
		MESSAGE_HANDLER(WM_CONTEXTMENU, OnContextMenu)
		COMMAND_ID_HANDLER(ID_OUTPUT_HIDE, OnHide)
		COMMAND_ID_HANDLER(ID_CLIPS_ADD, OnAdd)
		COMMAND_ID_HANDLER(ID_CLIPS_EDIT, OnEdit)
		COMMAND_ID_HANDLER(ID_CLIPS_REMOVE, OnRemove)
		COMMAND_ID_HANDLER(ID_CLIPS_ADDSET, OnAddSet)
		COMMAND_ID_HANDLER(ID_CLIPS_REMOVESET, OnRemoveSet)
		COMMAND_HANDLER(IDC_CLIPSCOMBO, CBN_SELCHANGE, OnComboSelChange)
		NOTIFY_HANDLER(IDC_CLIPSLIST, NM_DBLCLK, OnClipSelected);
		NOTIFY_HANDLER(IDC_CLIPSLIST, NM_RETURN, OnClipEnterPressed);
		NOTIFY_HANDLER(IDC_CLIPSLIST, LVN_GETINFOTIP, OnClipGetInfoTip);
		NOTIFY_HANDLER(IDC_CLIPSTOOLBAR, TBN_GETINFOTIP, OnToolbarGetInfoTip);
		NOTIFY_HANDLER(IDC_CLIPSLIST, TVN_SELCHANGED, OnClipSelChanged)
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	void Reset();

private:
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);
	LRESULT OnCtlColor(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);
	LRESULT OnGetMinMaxInfo(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);
	LRESULT OnSetEditorFocus(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);
	LRESULT	OnContextMenu(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	
	LRESULT OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnAdd(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnEdit(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnRemove(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnAddSet(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnRemoveSet(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnComboSelChange(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnClipSelected(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
	LRESULT OnClipEnterPressed(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
	LRESULT OnClipGetInfoTip(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
	LRESULT OnClipSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
	LRESULT OnToolbarGetInfoTip(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	void InsertClip(TextClips::Clip* tc);
	void LoadSet(Scheme* scheme);
	void saveView();
	void setupView();
	void setupToolbar();
	void handleRightClick(LPPOINT pt);
	void doContextMenu(LPPOINT pt);

	TextClips::TextClipSet* getSetForItem(HTREEITEM item);
	TextClips::TextClipSet* getSetForItem(HTREEITEM item, HTREEITEM& parent);
	TextClips::TextClipSet* getSetFromSetItem(HTREEITEM setItem);
	TextClips::TextClipSet* getOrCreateSet(LPCTSTR title);

	CTextClipsTreeCtrl m_tv;
	CComboBox		m_combo;
	TextClips::TextClipsManager* m_pTheClips;

	int m_comboHeight;
	HWND m_hWndToolBar;
	HIMAGELIST m_hImgList;
	HTREEITEM m_hLastItem;
};

#endif