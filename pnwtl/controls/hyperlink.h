/**
 * @file hyperlink.h
 * @brief Hyperlink Control
 * @author Simon Steele
 * @note Copyright (c) 2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef HYPERLINK_H__INCLUDED
#define HYPERLINK_H__INCLUDED

/**
 * Simple static control to display a hyperlink and allow clicking on it.
 */
class CHyperlink : public CWindowImpl<CHyperlink, CStatic>
{
public:
	DECLARE_WND_CLASS(_T("PNHyperlink"))

	CHyperlink() : m_clientArea(200,20)
	{
		m_hHandCursor = ::LoadCursor(NULL, IDC_HAND);
	}
	
	BEGIN_MSG_MAP(CHyperlink)
		MESSAGE_HANDLER(WM_PAINT, OnPaint)
		MESSAGE_HANDLER(WM_LBUTTONDOWN, OnLButtonDown)
		MESSAGE_HANDLER(WM_NCHITTEST, OnHitTest)
		MESSAGE_HANDLER(WM_SETCURSOR, OnSetCursor)
	END_MSG_MAP()

	/**
	 * Set the URL target for this control.
	 */
	void SetTarget(LPCTSTR url)
	{
		m_url = url;
	}

private:
	/**
	 * Handle button click, navigate to URL.
	 */
	LRESULT OnLButtonDown(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		::ShellExecute(m_hWnd, _T("open"), m_url.c_str(), NULL, NULL, SW_SHOW);

		return 0;
	}

	/**
	 * Need to claim clicks for static controls.
	 */
	LRESULT OnHitTest(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		CPoint point(GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam));
		ScreenToClient(&point);
		
		return (point.x > 0 && point.x <= m_clientArea.x && point.y > 0 && point.y <= m_clientArea.y) ? HTCLIENT : HTTRANSPARENT;
	}

	/**
	 * Hand cursor when you're over the text:
	 */
	LRESULT OnSetCursor(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		if (LOWORD(lParam) == HTCLIENT)
		{
			::SetCursor(m_hHandCursor);
		}

		return TRUE;
	}

	/**
	 * Paint the URL
	 */
	LRESULT OnPaint(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		CPaintDC dc(m_hWnd);
		
		CRect rc;
		GetClientRect(rc);

		CRect rcText(rc);

		COLORREF linkColor(GetSysColor(COLOR_HIGHLIGHT));
		
		dc.SetTextColor(linkColor);
		HFONT hOldFont = dc.SelectFont(GetParent().GetFont());

		CWindowText wt(m_hWnd);

		// Draw the text:
		int oldMode = dc.SetBkMode(TRANSPARENT);
		dc.DrawText((LPCTSTR)wt, -1, rcText, DT_TOP | DT_SINGLELINE | DT_CALCRECT | DT_LEFT);
		dc.DrawText((LPCTSTR)wt, -1, rcText, DT_TOP | DT_SINGLELINE | DT_LEFT);
		dc.SetBkMode(oldMode);

		m_clientArea.x = rcText.Width();
		m_clientArea.y = rcText.Height();

		// Draw the line:
		CPen penLine;
		penLine.CreatePen(PS_SOLID, 1, linkColor);
		CPenHandle penOld = dc.SelectPen(penLine);
		int lineat = __min(rcText.bottom + 1, rc.bottom);
		dc.MoveTo(rcText.left, lineat);
		dc.LineTo(rcText.right, lineat);
		dc.SelectPen(penOld);

		dc.SelectFont(hOldFont);

		return 0;
	}

private:
	tstring m_url;
	CPoint m_clientArea;
	HCURSOR m_hHandCursor;
};

#endif // #ifndef HYPERLINK_H__INCLUDED