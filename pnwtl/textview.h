/**
 * @file TextView.h
 * @brief Interface Definition for CTextView, the Scintilla based text-editor view.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#if !defined(AFX_PNVIEW_H__1F3B4A2C_A836_4C30_A47B_6E5A13ED33F2__INCLUDED_)
#define AFX_PNVIEW_H__1F3B4A2C_A836_4C30_A47B_6E5A13ED33F2__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "ScintillaImpl.h"
#include "ScintillaWTL.h"

#include "schemes.h"
#include "pntypes.h"

#define PN_NOTIFY (WM_USER+37)

class CTextView : public CScintillaWindow< CScintillaImpl >
{
public:

	typedef CScintillaWindow< CScintillaImpl > baseClass;

	CTextView() : CScintillaWindow< CScintillaImpl >()
	{
		m_pLastScheme = NULL;
	}

	void SetScheme(CScheme* pScheme)
	{
		pScheme->Load(*this);
		m_pLastScheme = pScheme;
	}

	void Load(LPCTSTR filename, CScheme* pScheme = NULL)
	{
		CScintilla::OpenFile(filename);

		CFileName cfn(filename);
		
		if(NULL == pScheme)
		{
			ctcString ext;
			ext = cfn.GetExtension();

			CScheme* sch = theApp.GetSchemes().SchemeForExt(ext.c_str());
			SetScheme(sch);
		}
		else
		{
			SetScheme(pScheme);
		}
	}

	void Save(LPCTSTR filename, bool bSetScheme = true)
	{
		CScintilla::SaveFile(filename);

		if(bSetScheme)
		{
			// Re-Apply Scheme:
			CFileName cfn(filename);
			ctcString ext;
			ext = cfn.GetExtension();
			if(ext.size() > 0)
			{
				CScheme* sch = theApp.GetSchemes().SchemeForExt(ext.c_str());
				sch->Load(*this);
				m_pLastScheme = sch;
			}
		}
	}

	void EnableHighlighting(bool bEnable)
	{
		if (bEnable)
		{
			if(m_pLastScheme != NULL)
			{
				m_pLastScheme->Load(*this);				
			}
		}
		else
		{
			SetLexer(0);
		}

		ClearDocumentStyle();
		Colourise(0, -1);
	}

	virtual int HandleNotify(LPARAM lParam)
	{
		int msg = baseClass::HandleNotify(lParam);
		
		if(msg == SCN_SAVEPOINTREACHED)
		{
			SendMessage(GetParent(), PN_NOTIFY, 0, SCN_SAVEPOINTREACHED);
			m_Modified = false;
		}
		else if(msg == SCN_SAVEPOINTLEFT)
		{
			SendMessage(GetParent(), PN_NOTIFY, 0, SCN_SAVEPOINTLEFT);
			m_Modified = true;
		}
		else if(msg == SCN_UPDATEUI)
		{
			SendMessage(GetParent(), PN_NOTIFY, 0, SCN_UPDATEUI);
		}
		
		return msg;
	}

	void SetPosStatus(CMultiPaneStatusBarCtrl& stat)
	{
		TCHAR tvstatbuf[30];
		
		long pos = GetCurrentPos();

		_stprintf(tvstatbuf, _T("[%d:%d] : %d"), 
			(LineFromPosition(pos) + 1),	/* row    */
			(GetColumn(pos) + 1),			/* column */
			GetLineCount()					/* lines  */
		);

		stat.SetPaneText(ID_POS_PANE, tvstatbuf);
	}

protected:

	virtual void OnFirstShow()
	{
		theApp.GetSchemes().GetDefaultScheme()->Load(*this);
	}

	CScheme* m_pLastScheme;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PNVIEW_H__1F3B4A2C_A836_4C30_A47B_6E5A13ED33F2__INCLUDED_)
