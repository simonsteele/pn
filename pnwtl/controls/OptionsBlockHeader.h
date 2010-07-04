/**
 * @file OptionsBlockHeader.h
 * @brief Command Bar Edit Control
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef OPTIONSBLOCKHEADER_H__INCLUDED
#define OPTIONSBLOCKHEADER_H__INCLUDED

class COptionsBlockHeader : public CWindowImpl<COptionsBlockHeader, CStatic>
{
public:
	BEGIN_MSG_MAP(COptionsBlockHeader)
		MESSAGE_HANDLER(WM_PAINT, OnPaint)
	END_MSG_MAP()

private:
	LRESULT OnPaint(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		CPaintDC dc(m_hWnd);
		
		CRect rc;
		GetClientRect(rc);
		dc.SetBkColor(GetSysColor(COLOR_BTNFACE));
		dc.SetTextColor(GetSysColor(COLOR_BTNTEXT));
		dc.FillRect(&rc, (HBRUSH)::GetSysColorBrush(COLOR_BTNFACE));

		HFONT hOldFont = dc.SelectFont(GetParent().GetFont());

		CWindowText wt(m_hWnd);

		// Draw the text:
		rc.left += 5;
		dc.DrawText((LPCTSTR)wt, -1, rc, DT_VCENTER | DT_SINGLELINE | DT_CALCRECT | DT_LEFT);
		dc.DrawText((LPCTSTR)wt, -1, rc, DT_VCENTER | DT_SINGLELINE | DT_LEFT);

		dc.SelectFont(hOldFont);

		CRect rcLine;
		GetClientRect(rcLine);
		rcLine.left = rc.right + 5;
		rcLine.top = rcLine.top + ((rcLine.bottom - rcLine.top) / 2) - 1;
		rcLine.bottom = rcLine.top + 2;
		dc.Draw3dRect(rcLine, GetSysColor(COLOR_3DSHADOW), GetSysColor(COLOR_3DHIGHLIGHT));

		return 0;
	}
};

#endif // #ifndef OPTIONSBLOCKHEADER_H__INCLUDED