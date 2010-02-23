/**
 * @file textclipstree.h
 * @brief Tree Control for Text Clips
 * @author Simon Steele
 * @note Copyright (c) 2010+ Simon Steele - http://untidy.net/
 */
#ifndef TEXTCLIPSTREE_H_INCLUDED
#define TEXTCLIPSTREE_H_INCLUDED

class CTextClipsTreeCtrl : public CWindowImpl<CTextClipsTreeCtrl, CTreeViewCtrl>
{
public:
	DECLARE_WND_CLASS(_T("TEXTCLIPTREE"));

	BEGIN_MSG_MAP(CTextClipsTreeCtrl)
		REFLECTED_NOTIFY_CODE_HANDLER(NM_CUSTOMDRAW, OnCustomDraw)
	END_MSG_MAP()

private:
	LRESULT OnCustomDraw(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& bHandled);
};

#endif // TEXTCLIPSTREE_H_INCLUDED