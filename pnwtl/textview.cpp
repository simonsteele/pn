/**
 * @file TextView.cpp
 * @brief Implementation of CTextView, the Scintilla based text-editor view.
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "textview.h"

#include "include/utf8_16.h"

CTextView::CTextView() : baseClass()
{
	m_pLastScheme = NULL;
	m_waitOnBookmarkNo = FALSE;
	m_encType = eUnknown;
}

BOOL CTextView::PreTranslateMessage(MSG* pMsg)
{
	if(m_waitOnBookmarkNo && pMsg->message == WM_KEYDOWN)
	{
		if((pMsg->wParam >= 0x30) && (pMsg->wParam <= 0x39))
		{
			ProcessNumberedBookmark(pMsg->wParam - 0x30);
			return TRUE;
		}
		m_waitOnBookmarkNo = FALSE;
	}
	return FALSE;
}

void CTextView::SetScheme(CScheme* pScheme)
{
	pScheme->Load(*this);
	EnsureRangeVisible(0, GetLength());
	ClearDocumentStyle();
	Colourise(0, -1);

	m_pLastScheme = pScheme;

	::SendMessage(GetParent(), PN_SCHEMECHANGED, 0, reinterpret_cast<LPARAM>(pScheme));
}

static string ExtractLine(const char *buf, size_t length) {
	unsigned int endl = 0;
	if (length > 0) {
		while ((endl < length) && (buf[endl] != '\r') && (buf[endl] != '\n')) {
			endl++;
		}
		if (((endl+1) < length) && (buf[endl] == '\r') && (buf[endl+1] == '\n')) {
			endl++;
		}
		if (endl < length) {
			endl++;
		}
	}
	return string(buf, 0, endl);
}

int determineEncoding(unsigned char* pBuf, int nLen, EPNEncoding& eEncoding) {
	eEncoding = eUnknown;

	int nRet = 0;

	if (nLen > 1) {
		if (pBuf[0] == Utf8_16::k_Boms[eUtf16BigEndian][0] && pBuf[1] == Utf8_16::k_Boms[eUtf16BigEndian][1]) {
			eEncoding = eUtf16BigEndian;
			nRet = 2;
		} else if (pBuf[0] == Utf8_16::k_Boms[eUtf16LittleEndian][0] && pBuf[1] == Utf8_16::k_Boms[eUtf16LittleEndian][1]) {
			eEncoding = eUtf16LittleEndian;
			nRet = 2;
		} else if (nLen > 2 && pBuf[0] == Utf8_16::k_Boms[eUtf8][0] && pBuf[1] == Utf8_16::k_Boms[eUtf8][1] && pBuf[2] == Utf8_16::k_Boms[eUtf8][2]) {
			eEncoding = eUtf8;
			nRet = 3;
		}
	}

	return nRet;
}

EPNSaveFormat determineLineEndings(char* pBuf, int nLen)
{
	char c, n, p;
	int linesCRLF, linesCR, linesLF;
	
	linesCRLF = linesCR = linesLF = 0;
	p = NULL;
	
	for(int i = 0; i < nLen; i++)
	{
		c = pBuf[i];
		n = ((i < nLen) ? pBuf[i] : NULL);

        if (c == '\r') 
		{
			if (n == '\n')
			{
				linesCRLF++;
				// Skip the next character (\n).
				n++;
				p = '\n';
				continue;
			}
			else
				linesCR++;
		} 
		else if (c == '\n') 
		{
			linesLF++;
		}
		
		p = c;
	}

	if (((linesLF >= linesCR) && (linesLF > linesCRLF)) || ((linesLF > linesCR) && (linesLF >= linesCRLF)))
		return PNSF_Unix;
	else if (((linesCR >= linesLF) && (linesCR > linesCRLF)) || ((linesCR > linesLF) && (linesCR >= linesCRLF)))
		return PNSF_Mac;
	
	else if (((linesCRLF >= linesLF) && (linesCRLF > linesCR)) || ((linesCRLF > linesLF) && (linesCRLF >= linesCR)))
		return PNSF_Windows;

	// Default
	return COptionsManager::GetInstance()->LineEndings;
}

bool CTextView::OpenFile(LPCTSTR filename)
{
	CFile file;		
	if ( file.Open(filename, CFile::modeRead | CFile::modeBinary) ) 
	{
		SPerform(SCI_CLEARALL);
		// Disable UNDO
		SPerform(SCI_SETUNDOCOLLECTION, 0);
		char data[blockSize];
		int lenFile = file.Read(data, sizeof(data));

		EPNSaveFormat endings = determineLineEndings(data, lenFile);

		///@todo otherwise set user's code page... (int bomLength =)
        determineEncoding(reinterpret_cast<unsigned char*>(data), lenFile, m_encType);
		if(m_encType != eUnknown)
		{
			// We do a Unicode-friendly read for unicode files...
			SetCodePage(SC_CP_UTF8);
			Utf8_16_Read converter;

			while (lenFile > 0)
			{
				lenFile = converter.convert(data, lenFile);
				SPerform(SCI_ADDTEXT, lenFile, (long)converter.getNewBuf());
				lenFile = file.Read(data, sizeof(data));
			}
		}
		else
		{
			// Otherwise we do a simple read.
			while (lenFile > 0) 
			{
				SPerform(SCI_ADDTEXT, lenFile, (long)data);
				lenFile = file.Read(data, sizeof(data));
			}
		}
		file.Close();
		SPerform(SCI_SETSEL, 0, 0);
		
		SetEOLMode(endings);
		
		// Re-Enable UNDO
		SPerform(SCI_SETUNDOCOLLECTION, 1);
		SPerform(SCI_SETSAVEPOINT);
		return true;
	}
	else
		return false;
}

bool CTextView::Load(LPCTSTR filename, CScheme* pScheme)
{
	if( OpenFile(filename) )
	{
		CFileName cfn(filename);
		
		if(NULL == pScheme)
		{
			ctcString ext;
			ext = cfn.GetExtension();

			EPNSaveFormat mode = static_cast<EPNSaveFormat>( GetEOLMode() );
			CScheme* sch = CSchemeManager::GetInstance()->SchemeForExt(ext.c_str());
			SetScheme(sch);
			mode = static_cast<EPNSaveFormat>( GetEOLMode() );
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

bool CTextView::SaveFile(LPCTSTR filename)
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

bool CTextView::Save(LPCTSTR filename, bool bSetScheme)
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

void CTextView::EnableHighlighting(bool bEnable)
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
		EnsureRangeVisible(0, GetLength());
		ClearDocumentStyle();
		Colourise(0, -1);
		StyleClearAll();
	}
}

void CTextView::ShowLineNumbers(bool bShow)
{
	int w = COptionsManager::GetInstance()->Get(PNSK_INTERFACE, _T("LineNumbersWidth"), 4);
	int pixelWidth = 4 + w * SPerform(SCI_TEXTWIDTH, STYLE_LINENUMBER, (LPARAM)"9");
	SPerform(SCI_SETMARGINWIDTHN, 0, bShow ? pixelWidth : 0);
}

int CTextView::HandleNotify(LPARAM lParam)
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

void CTextView::SetPosStatus(CMultiPaneStatusBarCtrl& stat)
{
	TCHAR tvstatbuf[50];
	
	long pos = GetCurrentPos();

	_stprintf(tvstatbuf, _T("[%d:%d] : %d"), 
		(LineFromPosition(pos) + 1),	/* row    */
		(GetColumn(pos) + 1),			/* column */
		GetLineCount()					/* lines  */
	);

	stat.SetPaneText(ID_POS_PANE, tvstatbuf);
	
	if(GetSelLength() > 0)
	{
		if(SelectionIsRectangle())
		{
			_tcscpy(tvstatbuf, _T("Rectangular Selection."));
		}
		else
		{
			_stprintf(tvstatbuf, _T("%d character(s) selected."), GetSelLength());
		}
		g_Context.m_frame->SetStatusText(tvstatbuf);
	}
	else
		g_Context.m_frame->SetStatusText(NULL);
}

CScheme* CTextView::GetCurrentScheme()
{
	return m_pLastScheme;
}

void CTextView::DoContextMenu(CPoint* point)
{
	CSPopupMenu popup(IDR_POPUP_EDITOR);
	g_Context.m_frame->TrackPopupMenu(popup, 0, point->x, point->y, NULL);
}

////////////////////////////////////////////////////////////////
// Command Handlers

LRESULT CTextView::OnIndent(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Tab();
	return 0;
}

LRESULT CTextView::OnUnindent(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	BackTab();
	return 0;
}

LRESULT CTextView::OnSetNumberedBookmark(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_waitOnBookmarkNo = 1;
	return 0;
}

LRESULT CTextView::OnNumberedBookmarkJump(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_waitOnBookmarkNo = 2;
	return 0;
}

LRESULT CTextView::OnToggleBookmark(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	ToggleBookmark();
	return 0;
}

LRESULT CTextView::OnNextBookmark(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	NextBookmark();
	return 0;
}

void CTextView::ProcessNumberedBookmark(int n)
{
	switch(m_waitOnBookmarkNo)
	{
		case 1:
			ToggleNumberedBookmark(n);
			break;

		case 2:
			JumpToNumberedBookmark(n);
			break;
	}
}

void CTextView::OnFirstShow()
{
	CSchemeManager::GetInstance()->GetDefaultScheme()->Load(*this);
}