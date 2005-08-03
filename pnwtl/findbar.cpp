#include "stdafx.h"
#include "findbar.h"
#include "resource.h"
#include "childfrm.h"

LRESULT CFindBar::OnTextChanged(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CWindowText wt(m_txtbox.m_hWnd);
	if((LPCTSTR)wt == NULL)
		return 0;

	CChildFrame* pChild = CChildFrame::FromHandle(GetCurrentEditor());
	if(pChild != NULL)
	{
		SearchOptions* so = OPTIONS->GetSearchOptions();
		so->FindText = wt;
		so->MatchWholeWord = false;
		so->Direction = true;

		CTextView* pTV = pChild->GetTextView();
		if(!pTV)
			return 0;

		// Kill the selection, so we re-select if we find good stuff.
		CharacterRange cr;
		pTV->GetSel(cr);
		pTV->SetSel(cr.cpMin, cr.cpMin);

		if(pChild->FindNext(so) == CScintillaImpl::FindNextResults::fnNotFound)
		{
			pTV->SetSel(cr.cpMin, cr.cpMax);
			m_txtbox.SetDoRed(true);
		}
		else
		{
			m_txtbox.SetDoRed(false);
		}
	}
	
	return 0;
}