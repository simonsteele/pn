/**
 * @file OutputView.cpp
 * @brief View to display output from tool calls.
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "outputview.h"

#include "childfrm.h"

/**
 * Finds the full extent of the text which is styled with "style" and
 * contains the position startPos.
 */
void COutputView::ExtendStyleRange(int startPos, int style, TextRange* tr)
{
	// Find the extent of this styled text...
	int docLength = GetLength();
	int startRange = startPos - 1;
	int endRange = startPos + 1;
	
	while( startRange >= 0 )
	{
		if(GetStyleAt(startRange) != style)
			break;
		startRange--;
	}
	
	while( endRange < docLength )
	{
		if(GetStyleAt(endRange) != style)
			break;
		endRange++;
	}

	tr->chrg.cpMin = startRange + 1;
	tr->chrg.cpMax = endRange - 1;
}

/**
 * Parse a simple GCC error string:
 * filename.ext:linenumber: error string/whatever
 */
void COutputView::HandleGCCError(int style, int position)
{
	TextRange tr;
				
	ExtendStyleRange(position, style, &tr);
	char* buf = new char[tr.chrg.cpMax - tr.chrg.cpMin + 1];
	tr.lpstrText = buf;

	GetTextRange(&tr);

	if( strlen(tr.lpstrText) > 0 )
	{
		char* cPos = strchr(buf, ':');
		
		// Skip past a drive letter...
		if( cPos != NULL && !isdigit(*(cPos+1)) )
			cPos = strchr(cPos+1, ':');

		if( cPos != NULL )
		{
			// We found the filename, now find the line number.
			*cPos = '\0';
			char* pLineNo  = cPos + 1;
			cPos = strchr(pLineNo, ':');
			if(cPos != NULL)
			{
				// Got the line number too, it's a valid error string.
				*cPos = '\0';

				int lineno = atoi(pLineNo);

				if(FileExists(buf))
				{
					// If the file's already open, just switch to it, otherwise open it.
					if( !g_Context.m_frame->CheckAlreadyOpen(buf, eSwitch) )
						g_Context.m_frame->OpenFile(buf);

					CChildFrame* pWnd = CChildFrame::FromHandle(GetCurrentEditor());
					CTextView* pView = pWnd->GetTextView();
					if(pView)
					{
						pView->GotoLine(lineno-1);
						::SetFocus(pView->m_hWnd);
					}
				}
			}
		}
	}

	delete [] buf;
}

LRESULT COutputView::OnHotSpotClicked(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	switch(wParam)
	{
		case /*SCE_ERR_GCC*/ 2:
		{
			HandleGCCError(wParam, lParam);
		}
		break;
	}

	return 0;
}

/**
 * In this function we look for a hotspot clicked notification, and if we get
 * one then we post a message to ourselves with the details required to handle
 * it. The reason we don't just do it here is that the notify will come in the
 * middle of Scintilla handling a click - therefore we get window focus problems.
 */
int COutputView::HandleNotify(LPARAM lParam)
{
	SCNotification *scn = (SCNotification*)lParam;
	if( scn->nmhdr.code == SCN_HOTSPOTCLICK )
	{
		int style = GetStyleAt(scn->position);
		PostMessage(PN_HANDLEHSCLICK, style, scn->position);
		return 0;
	}
	else
		return baseClass::HandleNotify(lParam);
}