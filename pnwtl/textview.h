/**
 * @file TextView.h
 * @brief Interface Definition for CTextView, the Scintilla based text-editor view.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
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

	CTextView() : CScintillaWindow< CScintillaImpl >()
	{
		m_pLastScheme = NULL;
	}

	void Load(LPCTSTR filename)
	{
		CScintilla::OpenFile(filename);

		CFileName cfn(filename);
		
		ctcString ext;
		ext = cfn.GetExtension();

		CScheme* sch = theApp.GetSchemes().SchemeForExt(ext.c_str());
		sch->Load(*this);
		m_pLastScheme = sch;
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
		SCNotification *scn = (SCNotification*)lParam;
		switch (scn->nmhdr.code)
		{
			case SCN_SAVEPOINTREACHED :
				SendMessage(GetParent(), PN_NOTIFY, 0, SCN_SAVEPOINTREACHED);
				m_Modified = false;
				break;

			case SCN_SAVEPOINTLEFT :
				SendMessage(GetParent(), PN_NOTIFY, 0, SCN_SAVEPOINTLEFT);
				m_Modified = true;
				break;

			default:
				return CScintilla::HandleNotify(lParam);
		}
			

		return scn->nmhdr.code;
	}

protected:
	CScheme* m_pLastScheme;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PNVIEW_H__1F3B4A2C_A836_4C30_A47B_6E5A13ED33F2__INCLUDED_)
