/**
 * @file textclipstree.h
 * @brief Tree Control for Text Clips
 * @author Simon Steele
 * @note Copyright (c) 2010+ Simon Steele - http://untidy.net/
 */
#ifndef TEXTCLIPSTREE_H_INCLUDED
#define TEXTCLIPSTREE_H_INCLUDED

namespace TextClips { class Clip; class TextClipSet; }

/**
 * Custom tree control presenting collapsible text clip sets and
 * custom-drawn clip items displaying the shortcut for each clip.
 */
class CTextClipsTreeCtrl : public CWindowImpl<CTextClipsTreeCtrl, CTreeViewCtrl>
{
	typedef CWindowImpl<CTextClipsTreeCtrl, CTreeViewCtrl> baseClass;
public:
	DECLARE_WND_CLASS(_T("TEXTCLIPTREE"));

	CTextClipsTreeCtrl();

	BEGIN_MSG_MAP(CTextClipsTreeCtrl)
		REFLECTED_NOTIFY_CODE_HANDLER(NM_CUSTOMDRAW, OnCustomDraw)
	END_MSG_MAP()

	/**
	 * Sort the root of the tree ensuring that non-grouped clips are at the top.
	 */
	void SortSets();

	void AddClip(TextClips::Clip* clip, HTREEITEM parent);
	HTREEITEM AddSet(TextClips::TextClipSet* set);

	TextClips::Clip* GetClip(HTREEITEM item);

	void RemoveSet(TextClips::TextClipSet* set, HTREEITEM item);

	void DeleteAllItems();

private:
	LRESULT OnCustomDraw(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& bHandled);
	void DrawClipSet(CDCHandle& handle, CRect& rcItem, TVITEMEX& info);
	void DrawClip(CDCHandle& handle, CRect& rcItem, HTREEITEM item, TVITEMEX& info);
	
	CTheme m_HeaderTheme;
	std::map<DWORD_PTR, bool> m_isSet;
};

/**
 * Convert an LPARAM to a Clip pointer.
 */
TextClips::Clip* ClipPtrFromLParam(LPARAM param);

/**
 * Convert an LPARAM to a TextClipSet pointer.
 */
TextClips::TextClipSet* ClipSetPtrFromLParam(LPARAM param);

#endif // TEXTCLIPSTREE_H_INCLUDED