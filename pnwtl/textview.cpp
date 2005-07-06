/**
 * @file TextView.cpp
 * @brief Implementation of CTextView, the Scintilla based text-editor view.
 * @author Simon Steele
 * @note Copyright (c) 2002-2005 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "textview.h"
#include "scaccessor.h"
#include "smartstart.h"
#include "include/utf8_16.h"

CTextView::CTextView() : baseClass()
{
	m_pLastScheme = NULL;
	m_waitOnBookmarkNo = FALSE;
	m_encType = eUnknown;
	
	m_bSmartStart = OPTIONS->Get(PNSK_EDITOR, _T("SmartStart"), true);

	::InitializeCriticalSection(&m_csMeasure);
	m_hMeasureThread = NULL;
	m_bMeasureCanRun = false;
}

CTextView::~CTextView()
{
	::EnterCriticalSection(&m_csMeasure);
	m_bMeasureCanRun = false;
	::LeaveCriticalSection(&m_csMeasure);

	if(m_hMeasureThread != NULL)
	{
		::WaitForSingleObject(m_hMeasureThread, 10000);
		::CloseHandle(m_hMeasureThread);
	}
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
	if(pScheme != SchemeManager::GetInstance()->GetDefaultScheme())
		m_bSmartStart = false;
	
	EnsureRangeVisible(0, GetLength());
	ClearDocumentStyle(); // zero all style bytes

	// Clear out the keywords or they get carried over.
	for(int i = 0; i < KEYWORDSET_MAX; i++)
	{
		SetKeyWords(i, "");
	}
	
	pScheme->Load(*this);
	
	//EnsureRangeVisible(0, GetLength());
	//ClearDocumentStyle();
	
	Colourise(0, -1);
	InvalidateRect(NULL, FALSE);
	
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
		n = ((i < nLen) ? pBuf[i+1] : NULL);

        if (c == '\r') 
		{
			if (n == '\n')
			{
				linesCRLF++;
				// Skip the next character (\n).
				i++;
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
	return (EPNSaveFormat)OPTIONS->GetCached(Options::OLineEndings);
}

bool CTextView::OpenFile(LPCTSTR filename, EPNEncoding encoding)
{
	// We don't want smart start if we're opening a file...
	m_bSmartStart = false;

#ifdef _DEBUG
	DWORD timeIn = GetTickCount();
#endif

	CFile file;
	if ( file.Open(filename, CFile::modeRead | CFile::modeBinary) ) 
	{
		// Disable UNDO
		SPerform(SCI_SETUNDOCOLLECTION, 0);

		SPerform(SCI_CLEARALL);

		// Pre-Allocate room for file...
		int length = file.GetLength();
		SPerform(SCI_ALLOCATE, length + 1024);
		
		int useBlockSize = blockSize;

		if(length >= 10485760)
		{
			// Optimise for large files. Disable the line layout cache,
			// and read the file in larger chunks...
			SPerform(SCI_SETLAYOUTCACHE, SC_CACHE_NONE);
			useBlockSize = 1048576;
		}
		else
		{
			SPerform(SCI_SETLAYOUTCACHE, OPTIONS->GetCached(Options::ODefaultScintillaCache));
		}
		
		char* data = new char[useBlockSize];
		int lenFile = file.Read(data, useBlockSize);

		EPNSaveFormat endings = determineLineEndings(data, lenFile);

		///See if there's an encoding specified or not...
        if(encoding == eUnknown)
			determineEncoding(reinterpret_cast<unsigned char*>(data), lenFile, m_encType);
		else
			m_encType = encoding;

		if(m_encType != eUnknown)
		{
			// We do a Unicode-friendly read for unicode files...
			SPerform(SCI_SETCODEPAGE, SC_CP_UTF8);
			Utf8_16_Read converter;

			while (lenFile > 0)
			{
				lenFile = converter.convert(data, lenFile);
				SPerform(SCI_ADDTEXT, lenFile, (long)converter.getNewBuf());
				lenFile = file.Read(data, useBlockSize);
			}
		}
		else
		{
			SPerform(SCI_SETCODEPAGE, (long)OPTIONS->GetCached(Options::ODefaultCodePage));

			// Otherwise we do a simple read.
			while (lenFile > 0) 
			{
				SPerform(SCI_ADDTEXT, lenFile, (long)data);
				lenFile = file.Read(data, useBlockSize);
			}
		}
		delete [] data;
		file.Close();
		SPerform(SCI_SETSEL, 0, 0);
		
		SetEOLMode(endings);
		
		// Re-Enable UNDO
		SPerform(SCI_SETUNDOCOLLECTION, 1);
		SPerform(SCI_SETSAVEPOINT);

		SetLineNumberChars();

#ifdef _DEBUG
		DWORD timeTotal = GetTickCount() - timeIn;
		TCHAR outstr[300];
		_stprintf(outstr, _T("File load takes %d milliseconds\n"), timeTotal);
		::OutputDebugString(outstr);
#endif

		return true;
	}
	else
		return false;
}

bool CTextView::Load(LPCTSTR filename, CScheme* pScheme, EPNEncoding encoding)
{
	if( OpenFile(filename, encoding) )
	{
		CScheme* sch = pScheme;
		
		if(NULL == sch)
		{
			sch = SchemeManager::GetInstance()->SchemeForFile(filename);
		}
		else
		{
			sch = pScheme;	
		}

		//EPNSaveFormat mode = static_cast<EPNSaveFormat>( GetEOLMode() ); // not sure what this was here for.

		SetScheme(sch);

		//mode = static_cast<EPNSaveFormat>( GetEOLMode() ); // not sure what this was here for.

		if( (sch == SchemeManager::GetInstance()->GetDefaultScheme()) && OPTIONS->Get(PNSK_EDITOR, _T("SmartStart"), true) )
		{
			// SmartStart is enabled, we'll use it to try and get a scheme if our scheme
			// is the default one...
			SmartStart::GetInstance()->Scan(this);
		}

		if(OPTIONS->Get(PNSK_EDITOR, _T("EnableLongLineThread"), true))
		{
			checkLineLength();
		}

		return true;
	}
	else
		return false;
}

/**
 * Add some extra intelligence to Revert so that we try to keep the cursor
 * in the same position - we're not going to extreme lengths though.
 * 
 * An alternative idea is to store the start and end of any text selection
 * and re-apply that instead of just re-setting the position.
 */
void CTextView::Revert(LPCTSTR filename)
{
	int lastPos = GetCurrentPos();
	int scrollPos = DocLineFromVisible( GetFirstVisibleLine() );

	if( OpenFile(filename, m_encType) )
	{
		if( GetLength() >= lastPos )
		{
			// Make sure the line is in view.
			int selLine = LineFromPosition( lastPos );
			EnsureVisibleEnforcePolicy( selLine );
			
			SetSel(lastPos, lastPos);

            int curTop = GetFirstVisibleLine();
			int lineTop = VisibleFromDocLine( scrollPos );
			LineScroll(0, lineTop - curTop);
			Invalidate();
		}

		if(OPTIONS->Get(PNSK_EDITOR, _T("EnableLongLineThread"), true))
		{
			checkLineLength();
		}
	}
}

bool CTextView::SaveFile(LPCTSTR filename)
{
	char data[blockSize + 1];
	int lengthDoc = SPerform(SCI_GETLENGTH);

	if(m_encType == eUnknown)
	{
		// Standard 8-bit ascii...
		CFile file;
		if( file.Open(filename, CFile::modeWrite | CFile::modeBinary) )
		{
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
		}
		else
			return false;
	}
	else
	{
		// Deal with writing unicode formats here...
		Utf8_16_Write converter;
		converter.setEncoding( static_cast<Utf8_16::encodingType>(m_encType) );

		FILE* fp = converter.fopen(filename, _T("wb"));
		if(fp != NULL)
		{
			for(int i = 0; i < lengthDoc; i += blockSize)
			{
				int grabSize = lengthDoc - i;
				if( grabSize > blockSize )
					grabSize = blockSize;
				GetRange(i, i + grabSize, data);

				converter.fwrite(data, grabSize);
			}
			converter.fclose();
		}
		else
			return false;
	}
	
	SPerform(SCI_SETSAVEPOINT);
	return true;
}

bool CTextView::Save(LPCTSTR filename, bool bSetScheme)
{
	if( SaveFile(filename) )
	{
		if(bSetScheme)
		{
			// Re-Apply Scheme:
			CScheme* sch = SchemeManager::GetInstance()->SchemeForFile(filename);
			SetScheme(sch);
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

/**
 * @brief Intelligently set the number of characters available for line numbers.
 */
void CTextView::SetLineNumberChars(bool bSet)
{
	if( SPerform(SCI_GETMARGINWIDTHN, 0) > 0 || bSet)
	{
		int w = OPTIONS->Get(PNSK_INTERFACE, _T("LineNumbersWidth"), 4);
		
		long lines = SPerform(SCI_GETLINECOUNT);
		char lnbuf[40];
		itoa(lines, lnbuf, 10);
		w = max(w, (int)strlen(lnbuf));
		
		int pixelWidth = 4 + w * SPerform(SCI_TEXTWIDTH, STYLE_LINENUMBER, (LPARAM)"9");
		SPerform(SCI_SETMARGINWIDTHN, 0, pixelWidth);
	}
}

void CTextView::ShowLineNumbers(bool bShow)
{
	m_bLineNos = bShow;
	if(bShow)
		SetLineNumberChars(true);
	else
        SPerform(SCI_SETMARGINWIDTHN, 0, 0);
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
	else if(msg == SCN_CHARADDED)
	{
		if(m_bSmartStart)
			if(SmartStart::GetInstance()->OnChar(this) != SmartStart::eContinue)
				m_bSmartStart = false;
	}
	else if(msg == SCN_MODIFIED)
	{
		if( ( reinterpret_cast<SCNotification*>(lParam))->linesAdded != 0 && m_bLineNos )
			SetLineNumberChars();
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
			_tcscpy(tvstatbuf, LS(IDS_RECTSEL));
		}
		else
		{
			_stprintf(tvstatbuf, LS(IDS_SELLENGTH), GetSelLength());
		}
		g_Context.m_frame->SetStatusText(tvstatbuf, false);
	}
	else
		g_Context.m_frame->SetStatusText(NULL);
}

tstring CTextView::GetCurrentWord()
{
	// VS.NET style find text:
	// 1. check if there is a selection
	// 2. check if the caret is inside a word
	// 3. if neither 1 nor 2 are true, use the previous search text

	CharacterRange cr;
	GetSel(cr);
	int len = cr.cpMax - cr.cpMin;

	char* pStr = NULL;

	//CString sel;
	if(len > 0)
	{
		pStr = new char[len+2];
		GetSelText(pStr);
	}
	else
	{
		TextRange tr;
		long pos = GetCurrentPos();
		tr.chrg.cpMin = WordStartPosition(pos, true);
		tr.chrg.cpMax = WordEndPosition(pos, true);
		len = tr.chrg.cpMax - tr.chrg.cpMin;
		if(len > 0)
		{
			pStr = new char[len + 2];
			tr.lpstrText = pStr;
			GetTextRange(&tr);
		}
	}

	tstring ret;

	if(pStr != NULL)
	{
		USES_CONVERSION;

		ret = A2CT(pStr);
		delete [] pStr;
		pStr = NULL;
	}

	return ret;
}

EPNEncoding CTextView::GetEncoding()
{
	return m_encType;
}

void CTextView::SetEncoding(EPNEncoding encoding)
{
	m_encType = encoding;
}

CScheme* CTextView::GetCurrentScheme()
{
	return m_pLastScheme;
}

#include "project.h"
#include "childfrm.h"

void CTextView::DoContextMenu(CPoint* point)
{
	CSPopupMenu popup(IDR_POPUP_EDITOR);
	CSPopupMenu popupProjects;

	// Build up a menu allowing us to add this file to a current project...
	CChildFrame* pParent = CChildFrame::FromHandle(GetParent());
	
	Projects::Workspace* pWS = g_Context.m_frame->GetActiveWorkspace();
	std::vector<int> menuIDs;

	if(pParent->CanSave() && pWS != NULL)
	{
		const Projects::PROJECT_LIST& projects = pWS->GetProjects();
		int index = 0;
		for(Projects::PL_CIT i = projects.begin(); i != projects.end(); ++i)
		{
			int iCmd = CSMenuManager::GetInstance()->GetNextID();
			popupProjects.AddItem((*i)->GetName(), iCmd);
			menuIDs.push_back(iCmd);
			index++;
		}

		if(popupProjects.GetCount() > 0)
		{
			::InsertMenu(popup.GetHandle(), popup.GetCount()-1, MF_BYPOSITION | MF_POPUP, (UINT)(HMENU)popupProjects.GetHandle(), _T("Add to Project"));
			::InsertMenu(popup.GetHandle(), popup.GetCount()-1, MF_BYPOSITION | MF_SEPARATOR, 0, _T(""));
		}
	}

	BOOL mnuResult = g_Context.m_frame->TrackPopupMenu(popup, TPM_RETURNCMD, point->x, point->y, NULL);

	if(mnuResult != 0)
	{
		bool bFoundOne = false;
		
		if(menuIDs.size())
		{
			const Projects::PROJECT_LIST& projects = pWS->GetProjects();
			Projects::PL_CIT i2 = projects.begin();
			
			for(size_t k = 0; k < menuIDs.size(); ++k)
			{
				if(menuIDs.at(k) == mnuResult)
				{
					// we should add this file to the project at index i.
					bFoundOne = true;
					tstring fn = pParent->GetFileName();
					(*i2)->AddFile(fn.c_str());
				}
				i2++;
			}
		}

		if(!bFoundOne)
		{
			::SendMessage(g_Context.m_frame->GetWindow()->m_hWnd, WM_COMMAND, mnuResult, NULL);
		}
	}

	for(std::vector<int>::iterator i = menuIDs.begin(); i != menuIDs.end(); ++i)
	{
		CSMenuManager::GetInstance()->ReturnID((*i));
	}
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

LRESULT CTextView::OnSelectAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	SelectAll();
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

LRESULT CTextView::OnCollapseAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	FoldAll();
	return 0;
}

LRESULT CTextView::OnExpandAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	UnFoldAll();
	return 0;
}

LRESULT CTextView::OnToggleFold(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	ToggleFold();
	return 0;
}

LRESULT CTextView::OnGotoBrace(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int posBrace = -1;
	if(caretAtBrace(posBrace))
	{
		// Switch to the other, matching brace
		posBrace = BraceMatch(posBrace);
	}
	else
	{
		posBrace = seekBrace();
	}
	
	if(posBrace != -1)
		GotoPos(posBrace);

	return 0;
}

bool CTextView::caretAtBrace(int& posBrace)
{
	///@todo Move to the properties? And should this be scheme-overridable.
	const char* braceset = "{[()]}";
	
	int caret = GetCurrentPos();
	
	posBrace = -1;

	if(caret > 0)
	{
		// brace could be before the caret.
		char before = GetCharAt(caret-1);
		if(before != NULL && strchr(braceset, before))
			posBrace = caret-1;
	}

	if(posBrace < 0)
	{
		// Look the other side of the caret...
		char after = GetCharAt(caret);
		if(after != NULL && strchr(braceset, after))
		{
			posBrace = caret;
			//after = false;
		}
	}

	//OtherBrace = BraceMatch(CaretBrace);

	//return (OtherBrace > CaretBrace) ? After : !After;
	return posBrace != -1;
}

int CTextView::seekBrace()
{
	// Look forwards, and then backwards if unsuccessful.
	int theBrace = seekBrace(true);
	if(theBrace == -1)
		theBrace = seekBrace(false);
	return theBrace;
}

int CTextView::seekBrace(bool forwards)
{
	int direction = forwards ? 1 : -1;

	const char* openset = "{[(";
	const char* closeset = ")]}";

	const char* pOpenSet = forwards ? openset : closeset;
	const char* pCloseSet = forwards ? closeset : openset;

	ScintillaAccessor doc(this);

	// Find out where we are...
	int position = GetCurrentPos() + direction;

	// Limit the seek
	int maxSeek = min(position+25000, doc.Length());
	int minSeek = max(0, position-25000);

	// See comment below for why this isn't necessary atm.
	/*int stylingBits = GetStyleBits();
	int stylingBitsMask = 0;
	for (int bit = 0; bit < stylingBits; bit++) {
		stylingBitsMask <<= 1;
		stylingBitsMask |= 1;
	}*/

	// Code from Scintilla's BraceMatch;
	int depth = 1;
	while ((position >= minSeek) && (position < maxSeek))
	{
		char chAtPos = doc[position];
		
		// Can't check the brace styling as we don't know what style they should be
		// in. This is one to add to the lexer configuration to make this work better.
		/*char styAtPos = static_cast<char>(doc.StyleAt(position) & stylingBitsMask);
		if ((position > GetEndStyled()) || (styAtPos == styBrace))*/
		
		{
			if (strchr(pOpenSet, chAtPos))
				depth++;
			if (strchr(pCloseSet, chAtPos))
				depth--;
			if (depth == 0)
				return position;
		}
		position = position + direction;
	}
	return -1;
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
	m_pLastScheme = SchemeManager::GetInstance()->GetDefaultScheme();
	m_pLastScheme->Load(*this);
	SetEOLMode( OPTIONS->GetCached(Options::OLineEndings) );
}

void CTextView::checkLineLength()
{
	::EnterCriticalSection(&m_csMeasure);
	if(m_bMeasureCanRun)
	{
		::LeaveCriticalSection(&m_csMeasure);
		return;
	}
	
	if(m_hMeasureThread != NULL)
	{
		::LeaveCriticalSection(&m_csMeasure);
		::WaitForSingleObject(m_hMeasureThread, 10000);
		::EnterCriticalSection(&m_csMeasure);
		::CloseHandle(m_hMeasureThread);
	}

	m_bMeasureCanRun = true;

	unsigned int thrdid;
	m_hMeasureThread = (HANDLE)_beginthreadex(NULL, 0, &CTextView::RunMeasureThread, this, 0, &thrdid);
	
	::LeaveCriticalSection(&m_csMeasure);	
}

/**
 * This thread is responsible for trying to make the line length in
 * the scintilla view much more sensible. We optionally whack it up
 * to the maximum limit. This is much more useful for viewing long
 * documents.
 */
UINT __stdcall CTextView::RunMeasureThread(void* pThis)
{
	CTextView* pTextView = static_cast<CTextView*>(pThis);

	pTextView->DisableDirectAccess();

	bool bCanRun = true;
	int maxLines = 0;
	int index = 0;
	int maxLength = pTextView->GetScrollWidth() - 10;
	int endPos;
	int endX;
	int absMaxLength;

	// NT has a 1000000 pixel max, 9x has 30000.
	if(g_Context.OSVersion.dwPlatformId == VER_PLATFORM_WIN32_NT)
		absMaxLength = 1000000;
	else
		absMaxLength = 30000;

	// We add 10 to the length we find as a little buffer, so
	// remove it from the absolute.
	absMaxLength -= 10;

	while(true)
	{
		::EnterCriticalSection(&pTextView->m_csMeasure);
		bCanRun = pTextView->m_bMeasureCanRun;
		::LeaveCriticalSection(&pTextView->m_csMeasure);
		if(!bCanRun)
			break;

		maxLines = pTextView->GetLineCount();
		
		if(index >= maxLines)
			break;
		
		endPos = pTextView->GetLineEndPosition(index);
		endX = pTextView->PointXFromPosition(endPos);
		maxLength = max(endX, maxLength);

		if(maxLength >= absMaxLength)
			break;

		index++;
	}

	// ensure we stay below the absolute maximum...
	maxLength = min(maxLength, absMaxLength);

	TCHAR buf[50];
	_stprintf(buf, _T("Max line length: %d"), maxLength);
	LOG(buf);

	pTextView->SetScrollWidth(maxLength + 10);

	// As long as nothing else wants direct access we're ok!
	pTextView->EnableDirectAccess();

	::EnterCriticalSection(&pTextView->m_csMeasure);
	pTextView->m_bMeasureCanRun = false;
	::LeaveCriticalSection(&pTextView->m_csMeasure);

	_endthreadex(0);

	return 0;
}