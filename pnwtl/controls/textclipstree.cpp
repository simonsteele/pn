/**
 * @file textclipstree.cpp
 * @brief Tree Control for Text Clips
 * @author Simon Steele
 * @note Copyright (c) 2010+ Simon Steele - http://untidy.net/
 */
#include "stdafx.h"
#include "textclipstree.h"
#include "../textclips.h"

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
				
				TCHAR textbuf[256];
				TVITEMEX info = {0};
				info.hItem = item;
				info.mask = TVIF_CHILDREN | TVIF_TEXT;
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
					LOG(info.pszText);
					LOG(_T("\n"));
				}

				itemBrush.CreateSolidBrush(bgColor);
				
				dc.SetBkColor(bgColor);
				dc.FillRect(rcItem, itemBrush);
				dc.DrawText(info.pszText, -1, rcItem, DT_SINGLELINE | DT_HIDEPREFIX);

				if (info.cChildren == 0)
				{
					TextClips::Clip* clip = reinterpret_cast<TextClips::Clip*>(GetItemData(item));
					if (clip != NULL && clip->Shortcut.size())
					{
						// Draw Shortcut:
						CA2CT shortcutText(clip->Shortcut.c_str());
						
						COLORREF shortcutBg(GetSysColor(COLOR_BTNFACE));
						CBrush shortcutBrush;
						shortcutBrush.CreateSolidBrush(shortcutBg);
						CPen shortcutPen;
						shortcutPen.CreatePen(PS_SOLID, 1, shortcutBg);

						// Work out where to draw the shortcut:
						CRect rcRow(rcItem);
						
						dc.DrawText(shortcutText, -1, rcItem, DT_CALCRECT);
						rcItem.MoveToX(rcRow.right - 5 - rcItem.Width());
						rcItem.MoveToY(rcRow.top + ((rcRow.Height() - rcItem.Height()) / 2));
						rcItem.InflateRect(2, 2);
						
						dc.SetBkColor(shortcutBg);
						CBrushHandle pOldBrush(dc.SelectBrush(shortcutBrush));
						CPenHandle pOldPen(dc.SelectPen(shortcutPen));
						dc.RoundRect(rcItem, CPoint(2, 2));
						//int oldAlignment = dc.SetTextAlign(TA_CENTER | TA_BASELINE);
						dc.SetTextColor(GetSysColor(COLOR_WINDOWTEXT));
						dc.DrawText(shortcutText, -1, rcItem, DT_SINGLELINE | DT_CENTER | DT_HIDEPREFIX | DT_VCENTER);
						dc.SelectBrush(pOldBrush);
						dc.SelectPen(pOldPen);
						//dc.SetTextAlign(oldAlignment);
					}
				}

				return CDRF_SKIPDEFAULT;
			}

		default:
			return CDRF_DODEFAULT;
	}
}