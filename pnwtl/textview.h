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
		EnsureRangeVisible(0, GetLength());
		ClearDocumentStyle();
		Colourise(0, -1);

		m_pLastScheme = pScheme;
	}

	virtual bool OpenFile(LPCTSTR filename)
	{
		CFile file;		
		if ( file.Open(filename, CFile::modeRead | CFile::modeBinary) ) 
		{
			//fileModTime = GetModTime(fullPath);

			SPerform(SCI_CLEARALL);
			// Disable UNDO
			SPerform(SCI_SETUNDOCOLLECTION, 0);
			char data[blockSize];
			int lenFile = file.Read(data, sizeof(data));
			while (lenFile > 0) 
			{
				SPerform(SCI_ADDTEXT, lenFile, (long)data);
				lenFile = file.Read(data, sizeof(data)); //fread(data, 1, sizeof(data), fp);
			}
			file.Close();
			SPerform(SCI_SETSEL, 0, 0);
			// Re-Enable UNDO
			SPerform(SCI_SETUNDOCOLLECTION, 1);
			SPerform(SCI_SETSAVEPOINT);
			return true;
		}
		else
			return false;
	}

	bool Load(LPCTSTR filename, CScheme* pScheme = NULL)
	{
		if( OpenFile(filename) )
		{

			CFileName cfn(filename);
			
			if(NULL == pScheme)
			{
				ctcString ext;
				ext = cfn.GetExtension();

				CScheme* sch = CSchemeManager::GetInstance()->SchemeForExt(ext.c_str());
				SetScheme(sch);
			}
			else
			{
				SetScheme(pScheme);
			}
			return true;
		}
		else
			return false;
	}

	virtual bool SaveFile(LPCTSTR filename)
	{
		CFile file;
		if( file.Open(filename, CFile::modeWrite | CFile::modeBinary) )
		{
			char data[blockSize + 1];
			int lengthDoc = SPerform(SCI_GETLENGTH);
			for (int i = 0; i < lengthDoc; i += blockSize) 
			{
				int grabSize = lengthDoc - i;
				if (grabSize > blockSize)
					grabSize = blockSize;
				GetRange(i, i + grabSize, data);
				
				/*if (props.GetInt("strip.trailing.spaces"))
					grabSize = StripTrailingSpaces(
								   data, grabSize, grabSize != blockSize);*/
				
				file.Write(data, grabSize);
			}
			file.Close();
			SPerform(SCI_SETSAVEPOINT);
			return true;
		}
		else
			return false;
	}

	bool Save(LPCTSTR filename, bool bSetScheme = true)
	{
		if( SaveFile(filename) )
		{

			if(bSetScheme)
			{
				// Re-Apply Scheme:
				CFileName cfn(filename);
				ctcString ext;
				ext = cfn.GetExtension();
				if(ext.size() > 0)
				{
					CScheme* sch = CSchemeManager::GetInstance()->SchemeForExt(ext.c_str());
					SetScheme(sch);
				}
			}
			return true;
		}
		else
		{
			return false;
		}
	}

	void EnableHighlighting(bool bEnable)
	{
		if (bEnable)
		{
			if(m_pLastScheme != NULL)
			{
				SetScheme(m_pLastScheme);
			}
		}
		else
		{
			SetLexer(0);
			ClearDocumentStyle();
			Colourise(0, -1);
		}
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

	CScheme* GetCurrentScheme()
	{
		return m_pLastScheme;
	}

protected:

	virtual void OnFirstShow()
	{
		CSchemeManager::GetInstance()->GetDefaultScheme()->Load(*this);
	}

	CScheme* m_pLastScheme;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PNVIEW_H__1F3B4A2C_A836_4C30_A47B_6E5A13ED33F2__INCLUDED_)
