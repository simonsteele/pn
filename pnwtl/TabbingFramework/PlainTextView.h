// PlainTextView.h : interface of the CPlainTextView class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_PlainTextView_H__053AD676_0AE2_11D6_8BF1_00500477589F__INCLUDED_)
#define AFX_PlainTextView_H__053AD676_0AE2_11D6_8BF1_00500477589F__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

class CPlainTextView:
	public CWindowImpl<CPlainTextView, CEdit>,
	public CEditCommands<CPlainTextView>
{
protected:
	CFont m_font;

public:
	DECLARE_WND_SUPERCLASS(NULL, CEdit::GetWndClassName())

	BOOL PreTranslateMessage(MSG* pMsg)
	{
		pMsg;
		return FALSE;
	}

	BEGIN_MSG_MAP(CPlainTextView)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy)
		CHAIN_MSG_MAP_ALT(CEditCommands<CPlainTextView>, 1)

		DEFAULT_REFLECTION_HANDLER()
	END_MSG_MAP()

	LRESULT OnCreate(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		// "base::OnCreate()"
		LRESULT lRet = DefWindowProc(uMsg, wParam, lParam);

		// "OnInitialUpdate"

		// Set the font
		LOGFONT lf = {0};                  // Used to create the CFont.
		memset(&lf, 0, sizeof(LOGFONT));   // Initialize everything to 0
		lf.lfHeight = 10;                  // We want a font 10 units high
		::lstrcpy(lf.lfFaceName, _T("Courier")); //    with face name "Courier" (fixed width).
		m_font.CreateFontIndirect(&lf);    // Create the font.

		// Set the font of the edit control.
		this->SetFont(m_font);


		// Set the tab stops to 4 characters

		// get the control's DC
		CDC pDC(this->GetDC());

		// Select the font that the control will use into the DC.
		// We must do this because the control may or may not be using
		// that font at this exact moment
		CFont pOldFont(pDC.SelectFont(m_font));

		// Retreive text metrics for that font and return the previously
		// selected font.
		TEXTMETRIC tm = {0};
		pDC.GetTextMetrics(&tm);
		pDC.SelectFont(pOldFont);

		//this->ReleaseDC(pDC);

		// Tab stops are in dialog units. We'll use a 4 character tab stop
		int nDialogUnitsX = 4*(tm.tmAveCharWidth / LOWORD(GetDialogBaseUnits()));
		int nTabStops = 4*nDialogUnitsX;
		this->SetTabStops(nTabStops);

		bHandled = TRUE;

		return lRet;
	}

	LRESULT OnDestroy(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
	{
		bHandled = FALSE;
		return 0;
	}

};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PlainTextView_H__053AD676_0AE2_11D6_8BF1_00500477589F__INCLUDED_)
