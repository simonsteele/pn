/**
 * @file textclipstree.cpp
 * @brief Tree Control for Text Clips
 * @author Simon Steele
 * @note Copyright (c) 2010+ Simon Steele - http://untidy.net/
 */
#include "stdafx.h"
#include "textclipstree.h"
#include "../textclips.h"

namespace {

/**
 * Comparison function for sorting items in the clips tree.
 */
int CALLBACK TreeSorter(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
{
	if (lParam1 == 0 && lParam2 != 0)
	{
		return 1;
	}
	else if (lParam2 == 0 && lParam1 != 0)
	{
		return -1;
	}
	else if (lParam1 == 0 && lParam2 == 0)
	{
		return 0;
	}
	
	TextClips::Clip* clip1 = ClipPtrFromLParam(lParam1);
	TextClips::Clip* clip2 = ClipPtrFromLParam(lParam2);
	
	if (clip1->Name < clip2->Name)
	{
		return -1;
	}
	else if (clip1->Name > clip2->Name)
	{
		return 1;
	}

	return 0;
}

} // namespace {

TextClips::Clip* ClipPtrFromLParam(LPARAM param)
{
	return reinterpret_cast<TextClips::Clip*>(param);
}

CTextClipsTreeCtrl::CTextClipsTreeCtrl()
{
	if (m_HeaderTheme.IsThemingSupported())
	{
		m_HeaderTheme.OpenThemeData(NULL, L"ExplorerBar");
	}
}

void CTextClipsTreeCtrl::SortSets()
{
	TVSORTCB sort = {0};
	sort.hParent = TVI_ROOT;
	sort.lParam = 0;
	sort.lpfnCompare = &TreeSorter;

	SortChildrenCB(&sort, false);
}

LRESULT CTextClipsTreeCtrl::OnCustomDraw(int /*idCtrl*/, LPNMHDR pnmh, BOOL& bHandled)
{
	LPNMTVCUSTOMDRAW cdn = reinterpret_cast<LPNMTVCUSTOMDRAW>(pnmh);
	
	switch(cdn->nmcd.dwDrawStage)
	{
		case CDDS_PREPAINT:
			return CDRF_NOTIFYITEMDRAW;

		case CDDS_ITEMPREPAINT:
			{
				cdn->clrText = RGB(255,0,0);
				
				HTREEITEM item = (HTREEITEM)cdn->nmcd.dwItemSpec;
				
				// Get information about the item:
				TCHAR textbuf[256];
				TVITEMEX info = {0};
				info.hItem = item;
				info.mask = TVIF_CHILDREN | TVIF_TEXT | TVIF_PARAM;
				info.pszText = &textbuf[0];
				info.cchTextMax = 256;
				GetItem(&info);
				
				CRect rcItem;
				
				// false indicates to get the full item rect, not just the text:
				GetItemRect(item, rcItem, false);
				CDCHandle dc(cdn->nmcd.hdc);
				
				CBrush itemBrush;
				COLORREF bgColor(info.cChildren ? GetSysColor(COLOR_BTNFACE) : GetSysColor(COLOR_WINDOW));
				if (cdn->nmcd.uItemState & CDIS_SELECTED)
				{
					bgColor = GetSysColor(COLOR_HIGHLIGHT);
				}

				itemBrush.CreateSolidBrush(bgColor);
				
				dc.SetBkColor(bgColor);
				dc.FillRect(rcItem, itemBrush);

				if (info.lParam == 0)
				{
					DrawClipSet(dc, rcItem, info);
				}
				else
				{
					DrawClip(dc, rcItem, item, info);
				}

				return CDRF_SKIPDEFAULT;
			}

		default:
			return CDRF_DODEFAULT;
	}
}

/**
 * Draw a clip set item.
 */
void CTextClipsTreeCtrl::DrawClipSet(CDCHandle& dc, CRect& rcItem, TVITEMEX& info)
{
	//COLORREF btnFace(dc.GetBkColor());
	COLORREF btnFace(GetSysColor(COLOR_BTNHIGHLIGHT));
	COLORREF btnHl(GetSysColor(COLOR_BTNFACE));

	TRIVERTEX        vert[2];
	GRADIENT_RECT    gRect;
	vert [0] .x      = rcItem.left;
	vert [0] .y      = rcItem.top;
	vert [0] .Red    = GetRValue(btnFace) << 8;
	vert [0] .Green  = GetGValue(btnFace) << 8;
	vert [0] .Blue   = GetBValue(btnFace) << 8;
	vert [0] .Alpha  = 0x0000;

	vert [1] .x      = rcItem.right;
	vert [1] .y      = rcItem.bottom;
	vert [1] .Red    = GetRValue(btnHl) << 8;
	vert [1] .Green  = GetGValue(btnHl) << 8;
	vert [1] .Blue   = GetBValue(btnHl) << 8;
	vert [1] .Alpha  = 0x0000;

	gRect.UpperLeft  = 0;
	gRect.LowerRight = 1;
	dc.GradientFill(vert, 2, &gRect, 1, GRADIENT_FILL_RECT_V);
	
	// Top/tail
	CPen borderPen;
	borderPen.CreatePen(PS_SOLID, 1, GetSysColor(COLOR_BTNSHADOW));
	CPenHandle oldPen(dc.SelectPen(borderPen));
	dc.MoveTo(rcItem.left, rcItem.top);
	dc.LineTo(rcItem.right, rcItem.top);
	dc.MoveTo(rcItem.left, rcItem.bottom);
	dc.LineTo(rcItem.right, rcItem.bottom);
	dc.SelectPen(oldPen);

	rcItem.top += 1;
	rcItem.bottom -= 1;

	int oldMode = dc.SetBkMode(TRANSPARENT);
	dc.SetTextColor(GetSysColor(COLOR_WINDOWTEXT));
	rcItem.left += 2;
	dc.DrawText(info.pszText, -1, rcItem, DT_SINGLELINE | DT_HIDEPREFIX | DT_VCENTER);
	dc.SetBkMode(oldMode);

	rcItem.left = rcItem.right - 20;

	if (m_HeaderTheme.IsThemingSupported())
	{
		m_HeaderTheme.DrawThemeBackground(dc, EBP_NORMALGROUPCOLLAPSE, EBNGC_NORMAL, rcItem);
	}
}

/**
 * Draw a clip item.
 */
void CTextClipsTreeCtrl::DrawClip(CDCHandle& dc, CRect& rcItem, HTREEITEM item, TVITEMEX& info)
{
	// Draw an actual clip:
	TextClips::Clip* clip = reinterpret_cast<TextClips::Clip*>(GetItemData(item));
	if (clip != NULL && clip->Shortcut.size())
	{
		// Draw Text:
		dc.DrawText(info.pszText, -1, rcItem, DT_SINGLELINE | DT_HIDEPREFIX | DT_VCENTER);

		// Draw Shortcut:
		CA2CT shortcutText(clip->Shortcut.c_str());
		
		COLORREF shortcutBg(GetSysColor(COLOR_BTNFACE));
		CBrush shortcutBrush;
		shortcutBrush.CreateSolidBrush(shortcutBg);
		CPen shortcutPen;
		shortcutPen.CreatePen(PS_SOLID, 1, shortcutBg);

		// Work out where to draw the shortcut:
		CRect rcRow(rcItem);
		
		dc.DrawText(shortcutText, -1, rcItem, DT_SINGLELINE | DT_CALCRECT | DT_HIDEPREFIX);
		rcItem.MoveToX(rcRow.right - 5 - rcItem.Width());
		rcItem.MoveToY(rcRow.top + ((rcRow.Height() - rcItem.Height()) / 2));
		rcItem.InflateRect(2, 2);

		// And do the drawing:
		dc.SetBkColor(shortcutBg);
		CBrushHandle pOldBrush(dc.SelectBrush(shortcutBrush));
		CPenHandle pOldPen(dc.SelectPen(shortcutPen));
		dc.RoundRect(rcItem, CPoint(2, 2));
		dc.SetTextColor(GetSysColor(COLOR_WINDOWTEXT));
		dc.DrawText(shortcutText, -1, rcItem, DT_SINGLELINE | DT_CENTER | DT_HIDEPREFIX | DT_VCENTER);
		dc.SelectBrush(pOldBrush);
		dc.SelectPen(pOldPen);
	}
}