/**
 * @file textclipstree.h
 * @brief Tree Control for Text Clips
 * @author Simon Steele
 * @note Copyright (c) 2010+ Simon Steele - http://untidy.net/
 */
#ifndef TEXTCLIPSTREE_H_INCLUDED
#define TEXTCLIPSTREE_H_INCLUDED

namespace TextClips { class Clip; }

/**
 * Custom tree control presenting collapsible text clip sets and
 * custom-drawn clip items displaying the shortcut for each clip.
 */
class CTextClipsTreeCtrl : public CWindowImpl<CTextClipsTreeCtrl, CTreeViewCtrl>
{
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

private:
	LRESULT OnCustomDraw(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& bHandled);
	void DrawClipSet(CDCHandle& handle, CRect& rcItem, TVITEMEX& info);
	void DrawClip(CDCHandle& handle, CRect& rcItem, HTREEITEM item, TVITEMEX& info);
	CTheme m_HeaderTheme;
};

/**
 * Convert an LPARAM to a Clip pointer.
 */
TextClips::Clip* ClipPtrFromLParam(LPARAM param);

#endif // TEXTCLIPSTREE_H_INCLUDED