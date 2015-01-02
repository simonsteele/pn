/**
 * @file TextView.cpp
 * @brief Implementation of CTextView, the Scintilla based text-editor view.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "textview.h"
#include "scaccessor.h"
#include "smartstart.h"
#include "include/lineendings.h"
#include "scriptregistry.h"
#include "project.h"
#include "childfrm.h"
#include "findinfiles.h"
#include "unicodefilewriter.h"
#include "textclips/variables.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

/**
 * SmartHighlight is fun, but causes a lot of work in huge files. We now
 * limit the scope to +/- 200 lines from the top and bottom of the current view
 * and update as the viewport is moved.
 */
#define MAX_SMARTHIGHLIGHT_LINES 200

#define URL_REGEX "https?://[^ ]+|www\\.[^ ]+"

/////////////////////////////////////////////////////////////////////////////////////////////
// CTextView

CTextView::CTextView(DocumentPtr document, Views::ViewPtr parent, CommandDispatch* commands, AutoCompleteManager* autoComplete) : 
	Views::View(Views::vtText, parent),
	m_pDoc(document),
	m_pCmdDispatch(commands),
	m_pLastScheme(NULL),
	m_waitOnBookmarkNo(FALSE),
	m_encType(eUnknown),
	m_bOverwriteTarget(false),
	m_bInsertClip(false),
	m_bSkipNextChar(false)
{
	m_bSmartStart = OPTIONS->Get(PNSK_EDITOR, _T("SmartStart"), true);
	SetAutoCompleteManager(autoComplete);
}

CTextView::~CTextView()
{
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

void CTextView::SetScheme(Scheme* pScheme, int flags)
{
	if(pScheme != SchemeManager::GetInstance()->GetDefaultScheme())
		m_bSmartStart = false;
	
	EnsureRangeVisible(0, GetLength(), false);
	
	if ((flags & scfNoRestyle) == 0)
	{
		ClearDocumentStyle(); // zero all style bytes
	}

	ClearAutoComplete();

	// Clear out the keywords or they get carried over.
	for(int i = 0; i < KEYWORDSET_MAX; i++)
	{
		SetKeyWords(i, "");
	}
	
	//Pass scheme to base class to set initial words for autocomplete
	InitAutoComplete(pScheme);

	pScheme->Load(*this, (flags & scfNoViewSettings) == 0);
	
	//EnsureRangeVisible(0, GetLength());
	//ClearDocumentStyle();
	
	if ((flags & scfNoRestyle) == 0)
	{
		Colourise(0, -1);
	}

	InvalidateRect(NULL, FALSE);
	
	m_pLastScheme = pScheme;

	::SendMessage(m_pDoc->GetFrame()->m_hWnd, PN_SCHEMECHANGED, 0, reinterpret_cast<LPARAM>(pScheme));
}

static std::string ExtractLine(const char *buf, size_t length) {
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
	return std::string(buf, 0, endl);
}

/**
 * @return number of bytes to skip for BOM
 */
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
		} else {
			int good_cnt = 0;
			int escaped = 0;
			for (int i = 0; i < nLen; ++i)
			{
				unsigned char c = pBuf[i];

				if (((escaped == 1) && !(c >= 0x80 && c <= 0xBF)) ||
					((escaped == 0) && (c >= 0x80 && c <= 0xBF)))
				{
					// "Dead" combination ocuried - it is not UTF8
					good_cnt = -1;
					break;
				}	

				if (c >= 0xC0 && c <= 0xFD)
				{	
					escaped = 1;
				}
				if (c <= 0x7F)
				{
					escaped = 0;
				}
				if (c >= 0x80 && c <= 0xBF)
				{
					escaped++;
					good_cnt++;
				}
			}
			if (good_cnt > 0)
			{
				eEncoding = eUtf8NoBOM;
				nRet = 0; // special case for auto UTF8 - no BOM
			}
		}
	}

	return nRet;
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
		SPerform(SCI_EMPTYUNDOBUFFER);
		//SPerform(SCI_BEGINUNDOACTION);

		SPerform(SCI_CLEARALL);

		// Pre-Allocate room for file...
		int length = file.GetLength();
		try
		{
			SPerform(SCI_ALLOCATE, length + 1024);
		}
		catch(std::bad_alloc& ba)
		{
			// TODO: This now needs to check Scintilla's error handler
			// to find out if there was an exception and then maybe throw.
			TCHAR buf[1024];
			_sntprintf(buf, 1024, LS(IDS_FILETOOBIG), ba.what());
			buf[1023] = NULL;
			tstring msg = L10N::StringLoader::Get(IDS_FILETOOBIG);

			::MessageBox(m_hWnd, buf, LS(IDR_MAINFRAME), MB_ICONERROR | MB_OK);
		
			SPerform(SCI_SETUNDOCOLLECTION, 1);
			return false;
		}
		
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
		
		std::vector<char> data(useBlockSize);
		int lenFile = file.Read(&data[0], useBlockSize);

		///See if there's an encoding specified or not...
        if(encoding == eUnknown)
		{
			determineEncoding(reinterpret_cast<unsigned char*>(&data[0]), lenFile, m_encType);
		}
		else
		{
			m_encType = encoding;
		}

		EPNSaveFormat endings = determineLineEndings(reinterpret_cast<unsigned char*>(&data[0]), lenFile, m_encType);

		if(m_encType != eUnknown)
		{
			// We do a Unicode-friendly read for unicode files...
			SPerform(SCI_SETCODEPAGE, SC_CP_UTF8);
			Utf8_16_Read converter;
			
			// Converter doesn't understand UTF-8 with no BOM
			Utf8_16::encodingType convEncType = (m_encType == eUtf8NoBOM) ? Utf8_16::eUtf8 : (Utf8_16::encodingType)m_encType;
			int nBomSkipBytes( BOMLengthLookup[m_encType] );

			while (lenFile > 0)
			{
				lenFile = converter.convert(&data[0], lenFile, convEncType, nBomSkipBytes);
				SPerform(SCI_ADDTEXT, lenFile, (long)converter.getNewBuf());
				lenFile = file.Read(&data[0], useBlockSize);
			}
		}
		else
		{
			SPerform(SCI_SETCODEPAGE, (long)OPTIONS->GetCached(Options::OMultiByteCodePage));

			// Otherwise we do a simple read.
			while (lenFile > 0) 
			{
				SPerform(SCI_ADDTEXT, lenFile, reinterpret_cast<LPARAM>(&data[0]));
				lenFile = file.Read(&data[0], useBlockSize);
			}
		}

		file.Close();
		SPerform(SCI_SETSEL, 0, 0);
		
		// Re-Enable UNDO (if necessary)
		//SPerform(SCI_ENDUNDOACTION);
		SPerform(SCI_SETUNDOCOLLECTION, 1);

		SetEOLMode(endings);
		SPerform(SCI_SETSAVEPOINT);

		SetLineNumberChars();

		checkDotLogTimestamp();

#ifdef _DEBUG
		DWORD timeTotal = GetTickCount() - timeIn;
		TCHAR outstr[300];
		outstr[299] = NULL;
		_sntprintf(outstr, 299, _T("File load takes %d milliseconds\n"), timeTotal);
		::OutputDebugString(outstr);
#endif

		return true;
	}
	else
		return false;
}

bool CTextView::Load(LPCTSTR filename, Scheme* pScheme, EPNEncoding encoding)
{
	if (OpenFile(filename, encoding))
	{
		// Clear the UNDO buffer
		// EmptyUndoBuffer();

		Scheme* sch(pScheme);
		
		if (NULL == sch)
		{
			sch = SchemeManager::GetInstance()->SchemeForFile(filename);
		}

		SetScheme(sch);

		if ((sch == SchemeManager::GetInstance()->GetDefaultScheme()) && OPTIONS->Get(PNSK_EDITOR, _T("SmartStart"), true))
		{
			// SmartStart is enabled, we'll use it to try and get a scheme if our scheme
			// is the default one...
			SmartStart::GetInstance()->Scan(this);
		}

		return true;
	}

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
	}
}

bool CTextView::SaveFile(IFilePtr file, bool setSavePoint)
{
	char data[blockSize + 1];
	int lengthDoc = SPerform(SCI_GETLENGTH);

	if(m_encType == eUnknown)
	{
		// Standard 8-bit ascii...
		for (int i = 0; i < lengthDoc; i += blockSize) 
		{
			int grabSize = lengthDoc - i;
			if (grabSize > blockSize)
				grabSize = blockSize;
			GetRange(i, i + grabSize, data);

			file->Write(data, grabSize);
		}
	}
	else
	{
		// Deal with writing unicode formats here...
		UnicodeFileWriter converter(file);
		Utf8_16::encodingType convEncType = (m_encType == eUtf8NoBOM) ? Utf8_16::eUtf8 : static_cast<Utf8_16::encodingType>(m_encType);
		converter.SetEncoding( convEncType );
		converter.SetWriteBOM( m_encType != eUtf8NoBOM );

		for(int i = 0; i < lengthDoc; i += blockSize)
		{
			int grabSize = lengthDoc - i;
			if( grabSize > blockSize )
				grabSize = blockSize;
			GetRange(i, i + grabSize, data);

			converter.Write(data, grabSize);
		}
	}

	// We allow saving the file without setting the save point for the UI
	if(setSavePoint)
	{
		SPerform(SCI_SETSAVEPOINT);
	}

	return true;
}

bool CTextView::Save(IFilePtr file, bool bSetScheme)
{
	if (SaveFile(file, bSetScheme))
	{
		if (bSetScheme)
		{
			// Apply scheme if we picked up a filetype
			Scheme* sch = SchemeManager::GetInstance()->SchemeForFile(file->GetFilename().c_str());
			if (sch != NULL && sch != SchemeManager::GetInstance()->GetDefaultScheme() && sch != GetCurrentScheme())
			{
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
		EnsureRangeVisible(0, GetLength(), false);
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
		_itoa(lines, lnbuf, 10);
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

	Scintilla::SCNotification* scn(reinterpret_cast<Scintilla::SCNotification*>(lParam));

	if (scn->nmhdr.hwndFrom != m_hWnd)
	{
		// This is not for us
        return msg;
	}

	if(msg == SCN_SAVEPOINTREACHED)
	{
		SendMessage(m_pDoc->GetFrame()->m_hWnd, PN_NOTIFY, 0, SCN_SAVEPOINTREACHED);
		m_Modified = false;
	}
	else if(msg == SCN_SAVEPOINTLEFT)
	{
		SendMessage(m_pDoc->GetFrame()->m_hWnd, PN_NOTIFY, 0, SCN_SAVEPOINTLEFT);
		m_Modified = true;
	}
	else if(msg == SCN_UPDATEUI)
	{
		if (GetModEventMask() == 0)
		{
			// If we're not notifying of changes, we don't care about updateui at this point either.
			// This resolves a nasty never-ending update loop with multiple views and smartHighlight.
			return msg;
		}

		SendMessage(m_pDoc->GetFrame()->m_hWnd, PN_NOTIFY, 0, SCN_UPDATEUI);

		smartHighlight();
		updateOverwriteTarget();
	}
	else if(msg == SCN_CHARADDED)
	{
		if(m_bSmartStart)
		{
			if(SmartStart::GetInstance()->OnChar(this) != SmartStart::eContinue)
				m_bSmartStart = false;
		}
		
		m_pDoc->OnCharAdded( ( reinterpret_cast<Scintilla::SCNotification*>(lParam))->ch );
	}
	else if(msg == SCN_MODIFIED)
	{
		if( scn->linesAdded != 0 && m_bLineNos )
		{
			SetLineNumberChars();
		}
	}
	else if (msg == SCN_MACRORECORD)
	{
		m_recorder->RecordScintillaAction(scn->message, scn->wParam, scn->lParam);
	}
	
	if (m_bInsertClip)
	{
		handleInsertClipNotify(scn);
	}
	
	return msg;
}

void CTextView::SetPosStatus(CMultiPaneStatusBarCtrl& stat)
{
	TCHAR tvstatbuf[50];
	tvstatbuf[49] = NULL;
	
	long pos = GetCurrentPos();

	_sntprintf(tvstatbuf, 49, _T("[%d:%d] : %d"), 
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
			_sntprintf(tvstatbuf, 49, LS(IDS_SELLENGTH), GetSelLength());
		}
		g_Context.m_frame->SetStatusText(tvstatbuf, false);
	}
	else
		g_Context.m_frame->SetStatusText(NULL);
}

std::string CTextView::GetCurrentWord()
{
	// VS.NET style find text:
	// 1. check if there is a selection
	// 2. check if the caret is inside a word

	if (GetSelections() > 1)
	{
		return "";
	}

	int len = GetSelText(NULL);

	std::vector<char> buffer;

	if(len > 1) // 1 character when there's no selection.
	{
		buffer.resize(len+2);
		GetSelText(&buffer[0]);
	}
	else
	{
		long pos = GetCurrentPos();
		TextRangeEx tr(WordStartPosition(pos, true), WordEndPosition(pos, true));
		len = tr.Length();

		if(len > 0)
		{
			buffer.resize(len+2);
			tr.lpstrText = &buffer[0];
			GetTextRange(&tr);
		}
	}

	std::string ret;

	if(buffer.size())
	{
		ret = &buffer[0];
	}

	return ret;
}

EPNEncoding CTextView::GetEncoding() const
{
	return m_encType;
}

void CTextView::SetEncoding(EPNEncoding encoding)
{
	m_encType = encoding;
	
	if(m_encType != eUnknown)
	{
		// Experimental, 2006-02-12
		// Set the code page to UTF-8 if we're going to do unicode editing.
		SetCodePage(SC_CP_UTF8);
	}
	else
	{
		// We're ANSI, go for our default codepage:
		int ansiCodePage = (long)OPTIONS->GetCached(Options::OMultiByteCodePage);
		SetCodePage(ansiCodePage);
	}
}

Scheme* CTextView::GetCurrentScheme() const
{
	return m_pLastScheme;
}

void HandleFindAllResult(CTextView* parent, FIFSink* sink, LPCTSTR szFilename, int start, int end)
{
	int line = parent->LineFromPosition(start);
	CA2CT lineText(parent->GetLineText(line).c_str());
	sink->OnFoundString(_T(""), szFilename, line+1, lineText);
}

void HandleMarkAllResult(CTextView* parent, int start, int end)
{
	parent->IndicatorFillRange(start, end-start);
}

void CTextView::handleMarkAllResult(int start, int end)
{
	IndicatorFillRange(start, end-start);
	m_findAllResultCount++;
}

void CTextView::FindAll(extensions::ISearchOptions* options, FIFSink* sink, LPCTSTR szFilename)
{
	CScintillaImpl::FindAll(options, boost::bind(HandleFindAllResult, this, sink, szFilename, _1, _2));
}

void CTextView::MarkAll(extensions::ISearchOptions* options)
{
	ClearMarkAll();
	
	SetIndicatorValue(INDIC_ROUNDBOX);
	IndicSetStyle(INDIC_MARKALL, INDIC_ROUNDBOX);
	
	m_findAllResultCount = 0;
	CScintillaImpl::FindAll(options, boost::bind(&CTextView::handleMarkAllResult, this, _1, _2));
	
	CString str;
	str.Format(IDS_MARKALL_COUNT, m_findAllResultCount);
	g_Context.m_frame->SetStatusText((LPCTSTR)str);
}

void CTextView::ClearMarkAll()
{
	SetIndicatorCurrent(INDIC_MARKALL);
	IndicatorClearRange(0, GetLength());
}

/**
 * User wants to start recording.
 */
void CTextView::StartRecording(extensions::IRecorderPtr recorder)
{
	m_recorder = recorder;
	CScintilla::StartRecord();
	recorder->StartRecording();
}

/**
 * User wants to stop recording.
 */
void CTextView::StopRecording()
{
	if (!m_recorder.get())
	{
		return;
	}

	CScintilla::StopRecord();
	m_recorder->StopRecording();
	m_recorder.reset();
}

/**
 * Find out whether a recording is in progress.
 */
bool CTextView::IsRecording() const
{
	return m_recorder.get() != NULL;
}

void CTextView::DoContextMenu(CPoint* point)
{
	CSPopupMenu popup(IDR_POPUP_EDITOR);
	CSPopupMenu popupProjects;

	// Build up a menu allowing us to add this file to a current project...
	Projects::Workspace* pWS = g_Context.m_frame->GetActiveWorkspace();
	std::vector<int> menuIDs;

	if(m_pDoc->GetCanSave() && pWS != NULL)
	{
		const Projects::PROJECT_LIST& projects = pWS->GetProjects();
		int index = 0;
		for(Projects::PL_CIT i = projects.begin(); i != projects.end(); ++i)
		{
			int iCmd = m_pCmdDispatch->GetNextID();
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

	if(!isUrlSelected())
	{
		popup.RemoveItemByCommand(ID_VIEW_OPEN_URL);
	}

	if(!(m_pLastScheme && ScriptRegistry::GetInstanceRef().SchemeScriptsEnabled(m_pLastScheme->GetName())))
	{
		popup.EnableMenuItem(ID_EDITOR_USEASSCRIPT, false);
	}

	std::string wordAtCursor = GetCurrentWord();
	popup.EnableMenuItem(ID_GO_TO_DEF, wordAtCursor.size() != 0);

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
					tstring fn = m_pDoc->GetFileName();
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
		m_pCmdDispatch->ReturnID((*i));
	}
}

/**
 * Enter overwrite target mode from a command.
 */
HRESULT CTextView::OnOverwriteTarget(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	BeginOverwriteTarget();

	return 0;
}

/**
 * Insert a clip.
 */
void CTextView::InsertClip(const TextClips::Clip* clip)
{
	TextClips::DefaultVariableProvider variables(m_pDoc->GetFrame(), g_Context.m_frame->GetActiveWorkspace());
	std::vector<TextClips::Chunk> chunks;
	clip->GetChunks(chunks, this, &variables, ScriptRegistry::GetInstance());

	if (variables.GetSelectionUsed() && GetSelLength())
	{
		DeleteBack();
	}

	BOOL bHandled(FALSE);
	OnInsertClip(0, 0, reinterpret_cast<LPARAM>(&chunks), bHandled);
}

/**
 * Insert a clip based on text passed in, used for extensions.
 */
HRESULT CTextView::OnInsertClipText(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
{
	if (lParam == 0)
	{
		return 0;
	}

	// Make a clip and process the text:
	TextClips::Clip clip(_T(""), "", reinterpret_cast<const char*>(lParam));
	InsertClip(&clip);

	return 0;
}

HRESULT CTextView::OnInsertClip(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
{
	std::vector<TextClips::Chunk>* chunks = reinterpret_cast<std::vector<TextClips::Chunk>*>(lParam);
	if (chunks == NULL)
	{
		return -1;
	}

	beginInsertClip(*chunks);

	return 0;
}

/**
 * Message to set current scheme, using message to avoid revving the
 * extensions interface further in 2.1.
 */
LRESULT CTextView::OnSetSchemeText(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	if (lParam == 0)
	{
		return 0;
	}

	SchemeManager* pSM = SchemeManager::GetInstance();
	Scheme* pScheme(pSM->SchemeByName(reinterpret_cast<const char*>(lParam)));

	if (pScheme != NULL)
	{
		m_pDoc->GetFrame()->SetScheme(pScheme, scfNoViewSettings);
	}

	return 0;
}

HRESULT CTextView::OnKeyDown(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = false;

	if (wParam == VK_TAB && ::GetFocus() == m_hWnd)
	{
		if (m_bInsertClip)
		{
			// Next field in the template:
			if ( GetKeyState( VK_SHIFT ) & 0x8000 )
			{
				prevClipField();
			}
			else
			{
				nextClipField();
			}
			
			bHandled = true;
			m_bSkipNextChar = true;
		}
		else
		{
			if (::SendMessage(m_pDoc->GetFrame()->m_hWnd, PN_COMPLETECLIP, 0, 0))
			{
				bHandled = true;
				m_bSkipNextChar = true;
			}
		}
	}
	
	return 0;
}

HRESULT CTextView::OnKeyUp(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{	
	return 0;
}

HRESULT CTextView::OnChar(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = m_bSkipNextChar;
	m_bSkipNextChar = false;

	return 0;
}

/**
 * Override Vertical Scroll to handle smarthighlight throughout the file.
 */
HRESULT CTextView::OnVScroll(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = true;
	HRESULT ret = DefWindowProc(uMsg, wParam, lParam);
	smartHighlight();

	return ret;
}

/**
 * Override Mouse Wheel to handle smarthighlight throughout the file.
 */
HRESULT CTextView::OnMouseWheel(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	bHandled = true;
	HRESULT ret = DefWindowProc(uMsg, wParam, lParam);
	
	if ((wParam & (MK_CONTROL | MK_SHIFT)) == 0)
	{
		smartHighlight();
	}

	return ret;
}

HRESULT CTextView::OnSetFocus(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = false;
	NotifyGotFocus();
	return 0;
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

LRESULT CTextView::OnPrevBookmark(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	PrevBookmark();
	return 0;
}

LRESULT CTextView::OnClearAllBookmarks(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	ClearAllBookmarks();
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

LRESULT CTextView::OnOpenUrl(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Scintilla::TextToFind ttf;
	ttf.lpstrText = URL_REGEX;
	GetSel(ttf.chrg);
	long pos = FindText(SCFIND_REGEXP, &ttf);
	ATLASSERT(pos >= 0);
	if(pos < 0)
		return 0;

	std::string buf;
	buf.resize(ttf.chrg.cpMax - ttf.chrg.cpMin + 1);

	Scintilla::TextRange tr;
	tr.chrg = ttf.chrg;
	tr.lpstrText = &buf[0];
	GetTextRange(&tr);
	buf.resize(ttf.chrg.cpMax - ttf.chrg.cpMin);

	if(_strnicmp(buf.c_str(), "http://", 7) != 0)
		buf = std::string("http://") + buf;

	::ShellExecute(m_hWnd, _T("open"), CA2CT(buf.c_str()), NULL, NULL, SW_SHOW);
	return 0;
}

bool CTextView::isUrlSelected()
{
	Scintilla::TextToFind ttf;
	ttf.lpstrText = URL_REGEX;
	GetSel(ttf.chrg);

	return FindText(SCFIND_REGEXP, &ttf) >= 0;
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

int CTextView::leastIndentedLine(int startLine, int endLine)
{
	unsigned int indent = 0xFFFFFFFF;
	int line = startLine;
	for(int i = startLine; i <= endLine; ++i)
	{
		unsigned int lineIndent = (unsigned int)GetLineIndentation(i);
		if(lineIndent < indent)
		{
			indent = lineIndent;
			line = i;
		}
	}

	return line;
}

int CTextView::lineTextStartPosition(int line)
{
	int indent = GetLineIndentation(line);
	if(GetUseTabs())
	{
		int tabs = indent / GetTabWidth();
		indent = tabs + (indent % GetTabWidth());
	}

	return indent;
}

LRESULT CTextView::OnCommentLine(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Scheme* scheme = GetCurrentScheme();
	const CommentSpecRec& comments = scheme->GetCommentSpec();

	if( GetSelLength() && comments.CommentLineText[0] != NULL)
	{
		// Comment a block out using line comments
		UndoGroup group(*this);

		// Find the selection
		Scintilla::CharacterRange cr;
		GetSel(cr);
		int selStartLine = LineFromPosition( cr.cpMin );
		int selEndLine = LineFromPosition( cr.cpMax );

		// If the selection is full lines, the cursor will be right at the start of 
		// a line that we don't want to comment:
		if (cr.cpMax == PositionFromLine(selEndLine) && selEndLine > selStartLine)
		{
			selEndLine--;
		}

		// Calculate where the left-most bit of line is, and comment
		// vertically from there.
		int leftmostline = leastIndentedLine(selStartLine, selEndLine);
		int leastindent = lineTextStartPosition(leftmostline);

		// Comment those lines!
		for(int i = selStartLine; i <= selEndLine; ++i)
		{
			int linestart = PositionFromLine(i);
			linestart += leastindent;
			SetTargetStart(linestart);
			SetTargetEnd(linestart);
			ReplaceTarget(strlen(comments.CommentLineText), comments.CommentLineText);
		}
	}
	else if( comments.CommentLineText[0] != NULL )
	{
		// Comment the current line out...
		UndoGroup group(*this);

		int commentLine = LineFromPosition( GetCurrentPos() );
		int commentAt = PositionFromLine( commentLine ) + lineTextStartPosition( commentLine );
		SetTargetStart(commentAt);
		SetTargetEnd(commentAt);
		ReplaceTarget(strlen(comments.CommentLineText), comments.CommentLineText);
	}

	return 0;
}

LRESULT CTextView::OnCommentStream(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Scheme* scheme = GetCurrentScheme();
	const CommentSpecRec& comments = scheme->GetCommentSpec();

	if( GetSelLength() && comments.CommentStreamStart[0] != NULL && comments.CommentStreamEnd[0] != NULL )
	{
		UndoGroup group(*this);
		
		// Find selection
		Scintilla::CharacterRange cr;
		GetSel(cr);
		
		// Insert start of stream comment...
		SetTargetStart(cr.cpMin);
		SetTargetEnd(cr.cpMin);
		ReplaceTarget(strlen(comments.CommentStreamStart), comments.CommentStreamStart);
		cr.cpMax += strlen(comments.CommentStreamStart);

		// Insert end of stream comment...
		SetTargetStart(cr.cpMax);
		SetTargetEnd(cr.cpMax);
		ReplaceTarget(strlen(comments.CommentStreamEnd), comments.CommentStreamEnd);
		cr.cpMax += strlen(comments.CommentStreamEnd);

		// Expand the selection to include the comment chars...
		SetSel(cr.cpMin, cr.cpMax);
	}

	return 0;
}

LRESULT CTextView::OnCommentBlock(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Scheme* scheme = GetCurrentScheme();
	const CommentSpecRec& comments = scheme->GetCommentSpec();

	if( comments.CommentBlockStart[0] != NULL && comments.CommentBlockEnd[0] != NULL &&
		comments.CommentBlockLine[0] != NULL )
	{
		UndoGroup group(*this);

		Scintilla::CharacterRange cr;
		GetSel(cr);

		int selStartLine = LineFromPosition( cr.cpMin );
		int selEndLine = LineFromPosition( cr.cpMax );

		// Calculate where the left-most bit of line is, and comment
		// vertically from there.
		int leftmostline = leastIndentedLine(selStartLine, selEndLine);
		int leastindent = lineTextStartPosition(leftmostline);

		// Insert block start...
		SetTargetStart(PositionFromLine(selStartLine));
		SetTargetEnd(PositionFromLine(selStartLine));

		std::string lineend;
		switch (GetEOLMode())
		{
			case SC_EOL_CRLF:
				lineend = "\r\n";
				break;
			case SC_EOL_CR:
				lineend = "\r";
				break;
			default:
				lineend = "\n";
		};

		std::string indent = MakeIndentText( GetLineIndentation( leftmostline ), GetUseTabs(), GetTabWidth() );
		std::string startline(indent);
		startline += comments.CommentBlockStart;
        startline += lineend;

		ReplaceTarget(startline.size(), startline.c_str());

		for(int i = selStartLine+1; i <= selEndLine+1; ++i)
		{
			int linestart = PositionFromLine(i);
			linestart += leastindent;
			SetTargetStart(linestart);
			SetTargetEnd(linestart);
			ReplaceTarget(strlen(comments.CommentBlockLine), comments.CommentBlockLine);
		}
		
		std::string endline(lineend);
		endline += indent;
		endline += comments.CommentBlockEnd;
		endline += lineend;

		GetSel(cr);
		SetTargetStart(cr.cpMax);
		SetTargetEnd(cr.cpMax);
		ReplaceTarget(endline.size(), endline.c_str());
	}
	
	return 0;
}

LRESULT CTextView::OnUncomment(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Scheme* scheme = GetCurrentScheme();
	const CommentSpecRec& comments = scheme->GetCommentSpec();

	// 1. See if there's no selection, in which case it's a line comment
	// 2. Check start->end and intermediate to see if we have a comment block
	// 3. Else check start->end to see if we have a comment stream
	// 4. Try to un-line-comment the selection...

	Scintilla::CharacterRange cr;
	GetSel(cr);

	int startLine = LineFromPosition(cr.cpMin);
	int endLine = LineFromPosition(cr.cpMax);

	if (comments.CommentLineText[0] != NULL)
	{
		// Look for and try to remove line comments:
		int indentPos = GetLineIndentPosition(startLine);
		ScintillaAccessor sa(this);
		bool match(true);

		for (size_t i(0); i < strlen(comments.CommentLineText); ++i)
		{
			if (sa.SafeGetCharAt(indentPos + i, '\0') != comments.CommentLineText[i])
			{
				match = false;
				break;
			}
		}

		if (match)
		{
			UndoGroup group(*this);

			for (int line = startLine; line <= endLine; line++)
			{
				UnCommentLine(comments, line);
			}

			return 0;
		}
	}

	if(cr.cpMin == cr.cpMax)
		return 0; // nothing else to do here, we can't uncomment this...

	//TODO: Uncomment block...

	// Uncomment stream...
	if(comments.CommentStreamStart[0] != NULL && comments.CommentStreamEnd[0] != NULL)
	{
		if( UnCommentStream(comments) )
			return 0;
	}

	return 0;
}

/**
 * Get document title, used for printing
 */
tstring CTextView::GetDocTitle()
{
	return tstring(m_pDoc->GetTitle());
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
	
	int defaultEncoding = (long)OPTIONS->GetCached(Options::ODefaultEncoding);
	if (defaultEncoding == eUnknown)
	{
		int ansiCodePage = OPTIONS->GetCached(Options::OMultiByteCodePage);
		SPerform(SCI_SETCODEPAGE, ansiCodePage);
	}
	else
	{
		// Any unicode encoding has UTF8 as the Scintilla Code Page.
		SPerform(SCI_SETCODEPAGE, SC_CP_UTF8);	
	}
	
	m_encType = (EPNEncoding)defaultEncoding;
}

/**
 * Insert a timestamp if the file has .LOG in the first line
 */
void CTextView::checkDotLogTimestamp()
{
	int lineLength = min(4, LineLength(0));
	if (lineLength != 4)
	{
		return;
	}

	std::string text;
	text.resize(5);
	GetText(5, &text[0]);
	text.resize(4);

	// If we have .LOG then add a blank line and then the date and time   
	if (text == ".LOG")
	{
		std::string lineend;
		
		switch (GetEOLMode())
		{
		case SC_EOL_CRLF:
			lineend = "\r\n";
			break;
		case SC_EOL_CR:
			lineend = "\r";
			break;
		default:
			lineend = "\n";
		};

		std::string timestr(lineend);
		
		time_t now;
		time(&now);
		struct tm* local(localtime(&now));

		timestr += asctime(local);

		// remove unwanted \n
		timestr.resize(timestr.size() - 1);
		timestr += lineend;

		AppendText(timestr.size(), timestr.c_str());

		DocumentEnd();
	}
}

/**
 * Implement the smart highlight feature seen in Notepad++, this highlights
 * all occurrences of the currently selected word.
 */
void CTextView::smartHighlight()
{
	if (!OPTIONS->GetCached(Options::OSmartHighlight))
	{
		return;
	}

	// Clear Smart Highlights:
	SetIndicatorCurrent(INDIC_SMARTHIGHLIGHT);
	IndicatorClearRange(0, GetLength());

	if (GetSelections() > 1)
	{
		// Smart Highlight not supported with more than 1 selection.
		return;
	}

	Scintilla::CharacterRange cr;
	GetSel(cr);
	int len = cr.cpMax - cr.cpMin;
	if (len > 0)
	{
		// Only do this for single-line selections where there are no spaces (i.e. a single word)
		if (LineFromPosition(cr.cpMin) == LineFromPosition(cr.cpMax))
		{
			std::string buf(GetSelText());

			if (buf.find(' ') == -1 && buf.find('\t') == -1)
			{
				SetIndicatorValue(INDIC_ROUNDBOX);
				IndicSetStyle(INDIC_SMARTHIGHLIGHT, INDIC_ROUNDBOX);
				
				// Get our confining range for Smart Highlight:
				int startAtLine = DocLineFromVisible(GetFirstVisibleLine());
				int endAtLine = min(GetLineCount(), DocLineFromVisible(GetFirstVisibleLine() + LinesOnScreen()) + MAX_SMARTHIGHLIGHT_LINES);
				
				CA2CT findText(buf.c_str());
				SearchOptions opt;
				opt.SetFindText(findText);
				CScintillaImpl::FindAll(PositionFromLine(startAtLine), PositionFromLine(endAtLine), &opt, boost::bind(HandleMarkAllResult, this, _1, _2));
			}
		}
	}
}

/**
 * Override FindNext to provide script/macro recording support.
 */
int CTextView::FindNext(extensions::ISearchOptions* pOptions)
{
	int result = baseClass::FindNext(pOptions);

	if (m_recorder.get())
	{
		m_recorder->RecordSearchAction(stFindNext, pOptions, static_cast<FindNextResult>(result));
	}

	return result;
}

/**
 * Override ReplaceOnce to provide script/macro recording support.
 */
bool CTextView::ReplaceOnce(extensions::ISearchOptions* pOptions)
{
	bool result = baseClass::ReplaceOnce(pOptions);

	if (m_recorder.get())
	{
		m_recorder->RecordSearchAction(stReplace, pOptions, result ? fnFound : fnNotFound);
	}

	return result;
}

/**
 * Override ReplaceAll to provide script/macro recording support.
 */
int CTextView::ReplaceAll(extensions::ISearchOptions* pOptions)
{
	int result = baseClass::ReplaceAll(pOptions);

	if (m_recorder.get())
	{
		m_recorder->RecordSearchAction(stReplaceAll, pOptions, static_cast<FindNextResult>(result));
	}

	return result;
}

/**
 * OverwriteTarget mode works like VIM's "change in" command, we set an end point
 * and if the cursor leaves that point we delete everything from the last cursor
 * pos to the end of the target.
 */
void CTextView::BeginOverwriteTarget()
{
	SetIndicatorCurrent(INDIC_OVERWRITETARGET);
	IndicatorClearRange(0, GetLength());

	SetIndicatorValue(INDIC_BOX);
	IndicSetStyle(INDIC_OVERWRITETARGET, INDIC_BOX);

	int startTarget = GetTargetStart();
	int endTarget = GetTargetEnd();
	IndicatorFillRange(startTarget, endTarget - startTarget);

	SetOvertype(true);

	// Move to the start of the target:
	SetSelectionStart(startTarget);
	SetSelectionEnd(startTarget);

	m_bOverwriteTarget = true;
}

/**
 * We're in overwrite target mode and something has caused a UI update, we need
 * to work out what to do.
 */
void CTextView::updateOverwriteTarget()
{
	if (!m_bOverwriteTarget)
	{
		return;
	}

	// Find the range of the indicator, we start by finding the end because it's the first
	// end we'll find in the document.
	bool onAt0 = SPerform(SCI_INDICATORVALUEAT, INDIC_OVERWRITETARGET, 0) != 0;
	
	int start = onAt0 ? 0 : SPerform(SCI_INDICATOREND, INDIC_OVERWRITETARGET, 0);
	int end = SPerform(SCI_INDICATOREND, INDIC_OVERWRITETARGET, start);
	
	// Find our current position:
	int current = GetCurrentPos();
	
	if (current < start || current > end)
	{
		// User has stepped outside the target, time to clear the remaining target.
		SetTarget(start, end);
		ReplaceTarget(0, "");

		// We're out of this mode now:
		SetOvertype(false);
		m_bOverwriteTarget = false;
	}
	else
	{
		// User is inside the target
		SPerform(SCI_SETINDICATORCURRENT, INDIC_OVERWRITETARGET, 0);
		SPerform(SCI_INDICATORCLEARRANGE, start, current - start);
	}
}

void CTextView::UpdateModifiedState()
{
	m_Modified = GetModify();
}