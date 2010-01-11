/**
 * @file ScintillaIF.cpp
 * @brief Implementation of CScintilla
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "scintillaif.h"

#define SCINTILLA_PIXMAPS

#ifdef SCINTILLA_PIXMAPS
#include "ScintillaPixmaps.h"
#endif

///////////////////////////////////////////////////////////////////////////////
// UndoGroup

UndoGroup::UndoGroup(CScintilla& sci) : m_sci(sci)
{
	m_sci.BeginUndoAction();
}

UndoGroup::~UndoGroup()
{
	m_sci.EndUndoAction();
}

///////////////////////////////////////////////////////////////////////////////
// CScintilla

// Initialise no Scintilla dll on startup...
HMODULE CScintilla::scidll = NULL;
int CScintilla::refs = 0;

CScintilla::CScintilla()
{
	#ifndef SCI_NOLOAD
		#ifndef STATIC_SCILEXER
			if(!scidll)
			{
				scidll = LoadLibrary(_T("SciLexer.dll"));
			}
		#else
			if(!refs)
				Scintilla_RegisterClasses(GetModuleHandle(NULL));
		#endif
		
		refs++;
	#endif

	m_Modified = false;
	Perform = NULL;
	StoredPerform = NULL;
	m_TabWidth = 4;
	m_SelLength = 0;

	// initialise numbered bookmarks...
	for(int i = 0; i < 10; i++)
	{
		m_numberedBookmarks[i] = -1;
	}
}

CScintilla::~CScintilla()
{
	#ifndef SCI_NOLOAD
	refs--;
		#ifndef STATIC_SCILEXER
			if(!refs)
			{
				FreeLibrary(scidll);
				scidll = NULL;
			}
		#else
			if(!refs)
				Scintilla_ReleaseResources();
		#endif
	#endif
}

#ifndef WTL_SCINTILLA

HWND CScintilla::Create(HWND hParent, HINSTANCE hInst)
{
	const int style = WS_CHILD | WS_VSCROLL | WS_HSCROLL | WS_CLIPCHILDREN;
	const int exstyle = 0;

	m_scihWnd = ::CreateWindowEx(exstyle,
			_T("Scintilla"),
			_T("Source"),
			style,
			0, 0,
			100, 100,
			hParent,
			0,
			hInst,
			0);

	m_Pointer = (void *)SPerform(SCI_GETDIRECTPOINTER);
	Perform = (scmsgfn)SPerform(SCI_GETDIRECTFUNCTION);

	SetTabWidth(m_TabWidth);

	return m_scihWnd;
}

#endif

void CScintilla::DisableDirectAccess()
{
	StoredPerform = Perform;
	Perform = NULL;
}

bool CScintilla::EnableDirectAccess()
{
	if(StoredPerform != NULL)
	{
		Perform = StoredPerform;
		return true;
	}
	else
		return false;
}

bool CScintilla::OpenFile(LPCTSTR filename)
{
	FILE *fp = _tfopen(filename, _T("rb"));
	if (fp) 
	{
		//fileModTime = GetModTime(fullPath);

		SPerform(SCI_CLEARALL);
		// Disable UNDO
		SPerform(SCI_SETUNDOCOLLECTION, 0);
		char data[blockSize];
		int lenFile = fread(data, 1, sizeof(data), fp);
		while (lenFile > 0) 
		{
			SPerform(SCI_ADDTEXT, lenFile, (long)data);
			lenFile = fread(data, 1, sizeof(data), fp);
		}
		fclose(fp);
		SPerform(SCI_SETSEL, 0, 0);
		// Re-Enable UNDO
		SPerform(SCI_SETUNDOCOLLECTION, 1);
		SPerform(SCI_SETSAVEPOINT);
		return true;
	}
	return false;
}

void CScintilla::GetRange(int start, int end, char *text) 
{
	Scintilla::TextRange tr;
	tr.chrg.cpMin = start;
	tr.chrg.cpMax = end;
	tr.lpstrText = text;
	SPerform(SCI_GETTEXTRANGE, 0, reinterpret_cast<long>(&tr));
}

bool CScintilla::SaveFile(LPCTSTR filename)
{
	FILE *fp = _tfopen(filename, _T("wb"));
	if (fp) {
		char data[blockSize + 1];
		int lengthDoc = SPerform(SCI_GETLENGTH);
		for (int i = 0; i < lengthDoc; i += blockSize) {
			int grabSize = lengthDoc - i;
			if (grabSize > blockSize)
				grabSize = blockSize;
			GetRange(i, i + grabSize, data);
			/*if (props.GetInt("strip.trailing.spaces"))
				grabSize = StripTrailingSpaces(
					           data, grabSize, grabSize != blockSize);*/
			fwrite(data, grabSize, 1, fp);
		}
		fclose(fp);
		SPerform(SCI_SETSAVEPOINT);
		return true;
	}
	return false;
}

bool CScintilla::IsScintillaNotify(LPARAM lParam)
{
	Scintilla::SCNotification *scn = (Scintilla::SCNotification*)lParam;
	if (scn->nmhdr.hwndFrom == m_scihWnd)
		return true;
	return false;
}

int CScintilla::HandleNotify(LPARAM lParam)
{
	Scintilla::SCNotification *scn = (Scintilla::SCNotification*)lParam;
	switch (scn->nmhdr.code)
	{
		case SCN_SAVEPOINTREACHED :
			m_Modified = false;
			break;

		case SCN_SAVEPOINTLEFT :
			m_Modified = true;
			break;
		
		// Folding Notifications:
		case SCN_MARGINCLICK :
			if (scn->margin == 2) 
			{
				MarginClick(scn->position, scn->modifiers);
			}
			break;
		case SCN_MODIFIED:
			if (0 != (scn->modificationType & SC_MOD_CHANGEFOLD)) 
			{
				FoldChanged(scn->line, scn->foldLevelNow, scn->foldLevelPrev);
			}
			break;
		case SCN_UPDATEUI:
			{
				m_SelLength = SPerform(SCI_GETSELECTIONEND) - SPerform(SCI_GETSELECTIONSTART);
			}
			break;
		case SCN_NEEDSHOWN:
			{
				EnsureRangeVisible(scn->position, scn->position + scn->length, false);
			}
			break;
	}
	return scn->nmhdr.code;
}

bool CScintilla::GetModified()
{
	return m_Modified;
}

/**
 * Function taken from Scite to combine three marker define operations
 */
void CScintilla::DefineMarker(int marker, int markerType, COLORREF fore, COLORREF back)
{
	SPerform(SCI_MARKERDEFINE, marker, markerType);
	SPerform(SCI_MARKERSETFORE, marker, fore);
	SPerform(SCI_MARKERSETBACK, marker, back);
}

void CScintilla::DefineNumberedBookmarks(int base, bool SetDefaultColours)
{
#ifdef SCINTILLA_PIXMAPS
	for(int i = 0; i < 10; i++)
	{
		SPerform(SCI_MARKERDEFINEPIXMAP, base + i, (LPARAM)scpixmap_bookmarks[i]);
	}
#else
	COLORREF fore;
	COLORREF back;

	if(SetDefaultColours)
	{
		fore = ::GetSysColor(COLOR_HIGHLIGHTTEXT /*COLOR_INFOTEXT*/);
		back = ::GetSysColor(COLOR_HIGHLIGHT /*COLOR_INFOBK*/);
	}

	for(int i = 0; i < 10; i++)
	{
		SPerform(SCI_MARKERDEFINE, base + i, SC_MARK_CHARACTER + '0' + i);
		m_numberedBookmarks[i] = -1;
		if(SetDefaultColours)
		{
			SPerform(SCI_MARKERSETFORE, base+i, fore);
			SPerform(SCI_MARKERSETBACK, base+i, back);
		}
	}
#endif
}

void CScintilla::DefineBookmarks()
{
#ifndef SCINTILLA_PIXMAPS
	MarkerSetFore(SC_BOOKMARK, ::GetSysColor(COLOR_HIGHLIGHT));
	MarkerSetBack(SC_BOOKMARK, ::GetSysColor(COLOR_HOTLIGHT));
	MarkerDefine(SC_BOOKMARK, SC_MARK_CIRCLE);
#else
	SPerform(SCI_MARKERDEFINEPIXMAP, SC_BOOKMARK, (LPARAM)scpixmap_bookmark);
#endif
}

void CScintilla::ToggleBookmark(int marker)
{
	int line = LineFromPosition(GetCurrentPos());
	if(MarkerGet(line) & (1 << SC_BOOKMARK))
	{
		MarkerDelete(line, SC_BOOKMARK);
	}
	else
	{
		MarkerAdd(line, SC_BOOKMARK);
	}
}

void CScintilla::NextBookmark()
{
	int line = LineFromPosition(GetCurrentPos());
	int nextLine = MarkerNext(line+1, 1 << SC_BOOKMARK);
	if (nextLine < 0)
		nextLine = MarkerNext(0, 1 << SC_BOOKMARK);
	if (nextLine >= 0)
	{
		GotoLineEnsureVisible(nextLine);
	}
}

void CScintilla::PrevBookmark()
{
	int line = LineFromPosition(GetCurrentPos());
	int prevLine = MarkerPrevious(line-1, 1 << SC_BOOKMARK);
	if (prevLine < 0)
	{
		prevLine = MarkerPrevious(GetLineCount(), 1 << SC_BOOKMARK);
	}

	if (!(prevLine < 0 || prevLine == line))
	{
		GotoLineEnsureVisible(prevLine);
	}
}

void CScintilla::ClearAllBookmarks()
{
	MarkerDeleteAll(SC_BOOKMARK);

	for (int i = 0; i < 10; i++)
	{
		if (m_numberedBookmarks[i] != -1)
		{
			MarkerDeleteHandle(m_numberedBookmarks[i]);
			m_numberedBookmarks[i] = -1;

			MarkerDeleteAll(SC_NUMBERED_BOOKMARK + i);
		}
	}
}

void debugBookmarkLines(int* handles)
{
#ifdef DEBUG_NUMBERED_BOOKMARKS
	std::string dbgout;
	char buf[40];
	for(int i = 0; i < 10; ++i)
	{
		_itoa(handles[i], buf, 10);
		dbgout += buf;
		dbgout += ",";
	}
	dbgout += "\n";
	LOG(dbgout.c_str());
#endif //#ifdef DEBUG_NUMBERED_BOOKMARKS
}

void CScintilla::ToggleNumberedBookmark(int number, int base)
{
	if(number >= 0 && number <= 9)
	{
		int line = LineFromPosition(GetCurrentPos());
		int oldline = -1;

		if(m_numberedBookmarks[number] != -1)
		{
			oldline = MarkerLineFromHandle(m_numberedBookmarks[number]);
			MarkerDeleteHandle(m_numberedBookmarks[number]);
			m_numberedBookmarks[number] = -1;
		}

		int markers = MarkerGet(line);
		for(int i = 0; i < 10; i++)
		{
			if(markers & (1 << (base + i)))
			{
				MarkerDelete(line, base + i);
				m_numberedBookmarks[i] = -1;
			}
		}

		if(oldline != line)
		{
			m_numberedBookmarks[number] = MarkerAdd(line, base + number);
		}
	}

	debugBookmarkLines(m_numberedBookmarks);
}

void CScintilla::JumpToNumberedBookmark(int number, int base)
{
	if(number >= 0 && number <= 9 && m_numberedBookmarks[number] != -1)
	{
		int line = MarkerLineFromHandle(m_numberedBookmarks[number]);
		GotoLineEnsureVisible(line);
	}

	debugBookmarkLines(m_numberedBookmarks);
}

void CScintilla::GetSel(Scintilla::CharacterRange& cr)
{
	cr.cpMin = GetSelectionStart();
	cr.cpMax = GetSelectionEnd();
}

#define min(a, b)  (((a) < (b)) ? (a) : (b))
#define max(a, b)  (((a) > (b)) ? (a) : (b))

void CScintilla::EnsureRangeVisible(int begin, int end, bool enforcePolicy)
{
	int lineStart = LineFromPosition(min(begin, end));
	int lineEnd = LineFromPosition(max(begin, end));

	for(int line = lineStart; line <= lineEnd; line++)
	{
		enforcePolicy ? EnsureVisibleEnforcePolicy(line) : EnsureVisible(line);
	}
}

void CScintilla::GotoLineEnsureVisible(int line)
{
	SPerform(SCI_ENSUREVISIBLEENFORCEPOLICY, line);
	SPerform(SCI_GOTOLINE, line);
}

void CScintilla::SetTarget(int begin, int end)
{
	SPerform(SCI_SETTARGETSTART, begin);
	SPerform(SCI_SETTARGETEND, end);
}

void CScintilla::SetTarget(Scintilla::CharacterRange* cr)
{
	SPerform(SCI_SETTARGETSTART, cr->cpMin);
	SPerform(SCI_SETTARGETEND, cr->cpMax);
}

// Folding Functions...

/**
 * Call SetFoldingMargins to have CScintilla automatically set up 
 * the folding margin indicators in one of several given styles. The
 * function also enables folding by setting the fold and fold.compact
 * properties in Scintilla. Finally, it sets the fold flags to 16.
 *
 * By default, CScintilla handles all notification messages related to
 * folding and implements Scite-Style folding control.
 *
 * @param style Style is one value from the EFoldStyle enumeration
 */
void CScintilla::SetFoldingMargins(EFoldStyle style)
{
	/*SetProperty("fold", "1");
	SetProperty("fold.compact", "1");
	SetFoldFlags(16);*/

	switch (style)
	{
	case efsVSNet:
		{
			DefineMarker(SC_MARKNUM_FOLDEROPEN, SC_MARK_BOXMINUS, RGB(0xff, 0xff, 0xff), RGB(0x80, 0x80, 0x80));
			DefineMarker(SC_MARKNUM_FOLDER, SC_MARK_BOXPLUS, RGB(0xff, 0xff, 0xff), RGB(0x80, 0x80, 0x80));
			DefineMarker(SC_MARKNUM_FOLDERSUB, SC_MARK_VLINE, RGB(0xff, 0xff, 0xff), RGB(0x80, 0x80, 0x80));
			DefineMarker(SC_MARKNUM_FOLDERTAIL, SC_MARK_LCORNER, RGB(0xff, 0xff, 0xff), RGB(0x80, 0x80, 0x80));
			DefineMarker(SC_MARKNUM_FOLDEREND, SC_MARK_BOXPLUSCONNECTED, RGB(0xff, 0xff, 0xff), RGB(0x80, 0x80, 0x80));
			DefineMarker(SC_MARKNUM_FOLDEROPENMID, SC_MARK_BOXMINUSCONNECTED, RGB(0xff, 0xff, 0xff), RGB(0x80, 0x80, 0x80));
			DefineMarker(SC_MARKNUM_FOLDERMIDTAIL, SC_MARK_TCORNER, RGB(0xff, 0xff, 0xff), RGB(0x80, 0x80, 0x80));
		}
		break;
	case efsVSNetR:
		{
			DefineMarker(SC_MARKNUM_FOLDEROPEN, SC_MARK_CIRCLEMINUS, RGB(0xff, 0xff, 0xff), RGB(0x40, 0x40, 0x40));
			DefineMarker(SC_MARKNUM_FOLDER, SC_MARK_CIRCLEPLUS, RGB(0xff, 0xff, 0xff), RGB(0x40, 0x40, 0x40));
			DefineMarker(SC_MARKNUM_FOLDERSUB, SC_MARK_VLINE, RGB(0xff, 0xff, 0xff), RGB(0x40, 0x40, 0x40));
			DefineMarker(SC_MARKNUM_FOLDERTAIL, SC_MARK_LCORNERCURVE, RGB(0xff, 0xff, 0xff), RGB(0x40, 0x40, 0x40));
			DefineMarker(SC_MARKNUM_FOLDEREND, SC_MARK_CIRCLEPLUSCONNECTED, RGB(0xff, 0xff, 0xff), RGB(0x40, 0x40, 0x40));
			DefineMarker(SC_MARKNUM_FOLDEROPENMID, SC_MARK_CIRCLEMINUSCONNECTED, RGB(0xff, 0xff, 0xff), RGB(0x40, 0x40, 0x40));
			DefineMarker(SC_MARKNUM_FOLDERMIDTAIL, SC_MARK_TCORNERCURVE, RGB(0xff, 0xff, 0xff), RGB(0x40, 0x40, 0x40));
		}
		break;
	case efsPlus:
		{
			DefineMarker(SC_MARKNUM_FOLDEROPEN, SC_MARK_MINUS, RGB(0xff, 0xff, 0xff), RGB(0, 0, 0));
			DefineMarker(SC_MARKNUM_FOLDER, SC_MARK_PLUS, RGB(0xff, 0xff, 0xff), RGB(0, 0, 0));
			DefineMarker(SC_MARKNUM_FOLDERSUB, SC_MARK_EMPTY, RGB(0xff, 0xff, 0xff), RGB(0, 0, 0));
			DefineMarker(SC_MARKNUM_FOLDERTAIL, SC_MARK_EMPTY, RGB(0xff, 0xff, 0xff), RGB(0, 0, 0));
			DefineMarker(SC_MARKNUM_FOLDEREND, SC_MARK_EMPTY, RGB(0xff, 0xff, 0xff), RGB(0, 0, 0));
			DefineMarker(SC_MARKNUM_FOLDEROPENMID, SC_MARK_EMPTY, RGB(0xff, 0xff, 0xff), RGB(0, 0, 0));
			DefineMarker(SC_MARKNUM_FOLDERMIDTAIL, SC_MARK_EMPTY, RGB(0xff, 0xff, 0xff), RGB(0, 0, 0));
		}
		break;
	default:
	case efsArrow:
		{
			DefineMarker(SC_MARKNUM_FOLDEROPEN, SC_MARK_ARROWDOWN, RGB(0, 0, 0), RGB(0, 0, 0));
			DefineMarker(SC_MARKNUM_FOLDER, SC_MARK_ARROW, RGB(0, 0, 0), RGB(0, 0, 0));
			DefineMarker(SC_MARKNUM_FOLDERSUB, SC_MARK_EMPTY, RGB(0, 0, 0), RGB(0, 0, 0));
			DefineMarker(SC_MARKNUM_FOLDERTAIL, SC_MARK_EMPTY, RGB(0, 0, 0), RGB(0, 0, 0));
			DefineMarker(SC_MARKNUM_FOLDEREND, SC_MARK_EMPTY, RGB(0xff, 0xff, 0xff), RGB(0, 0, 0));
			DefineMarker(SC_MARKNUM_FOLDEROPENMID, SC_MARK_EMPTY, RGB(0xff, 0xff, 0xff), RGB(0, 0, 0));
			DefineMarker(SC_MARKNUM_FOLDERMIDTAIL, SC_MARK_EMPTY, RGB(0xff, 0xff, 0xff), RGB(0, 0, 0));
		}
		break;
	}
}

bool CScintilla::MarginClick(int position, int modifiers)
{
	int lineClick = LineFromPosition(position);//SendEditor(SCI_LINEFROMPOSITION, position);

	//Platform::DebugPrintf("Margin click %d %d %x\n", position, lineClick,
	//	SendEditor(SCI_GETFOLDLEVEL, lineClick) & SC_FOLDLEVELHEADERFLAG);

	if ((modifiers & SCMOD_SHIFT) && (modifiers & SCMOD_CTRL)) 
	{
		FoldAll();
	}
	else 
	{
		int levelClick = GetFoldLevel(lineClick);
		if (levelClick & SC_FOLDLEVELHEADERFLAG) 
		{
			if (modifiers & SCMOD_SHIFT) 
			{
				// Ensure all children visible
				SetFoldExpanded(lineClick, 1);
				Expand(lineClick, true, true, 100, levelClick);
			} 
			else if (modifiers & SCMOD_CTRL) 
			{
				if (GetFoldExpanded(lineClick)) 
				{
					// Contract this line and all children
					SetFoldExpanded(lineClick, 0);
					Expand(lineClick, false, true, 0, levelClick);
				}
				else 
				{
					// Expand this line and all children
					SetFoldExpanded(lineClick, 1);
					Expand(lineClick, true, true, 100, levelClick);
				}
			} 
			else 
			{
				// Toggle this line
				ToggleFold(lineClick);
			}
		}
	}
	return true;
}

void CScintilla::FoldAll()
{
	Colourise(0, -1);
	int maxLine = GetLineCount();
	bool expanding = true;
	for (int lineSeek = 0; lineSeek < maxLine; lineSeek++) 
	{
		if (GetFoldLevel(lineSeek) & SC_FOLDLEVELHEADERFLAG) 
		{
			expanding = !GetFoldExpanded(lineSeek);
			break;
		}
	}
	for (int line = 0; line < maxLine; line++) 
	{
		int level = GetFoldLevel(line);

		if ((level & SC_FOLDLEVELHEADERFLAG) &&
		        (SC_FOLDLEVELBASE == (level & SC_FOLDLEVELNUMBERMASK))) 
		{
			if (expanding) 
			{
				SetFoldExpanded(line, 1);
				Expand(line, true, false, 0, level);
				line--;
			} 
			else 
			{
				int lineMaxSubord = GetLastChild(line, -1);
				SetFoldExpanded(line, 0);
				if (lineMaxSubord > line)
					HideLines(line+1, lineMaxSubord);
			}
		}
	}
}

void CScintilla::Expand(int &line, bool doExpand, bool force, int visLevels, int level) 
{
	int lineMaxSubord = GetLastChild(line, level & SC_FOLDLEVELNUMBERMASK);
	line++;
	while (line <= lineMaxSubord) 
	{
		if (force) 
		{
			if (visLevels > 0)
				ShowLines(line, line);
			else
				HideLines(line, line);
		} 
		else 
		{
			if (doExpand)
				ShowLines(line, line);
		}
		
		int levelLine = level;

		if (levelLine == -1)
			levelLine = GetFoldLevel(line);

		if (levelLine & SC_FOLDLEVELHEADERFLAG) 
		{
			if (force) 
			{
				if (visLevels > 1)
					SetFoldExpanded(line, 1);
				else
					SetFoldExpanded(line, 0);
				
				Expand(line, doExpand, force, visLevels - 1);
			} 
			else 
			{
				if (doExpand) 
				{
					if (!GetFoldExpanded(line))
						SetFoldExpanded(line, 1);
				
					Expand(line, true, force, visLevels - 1);
				} 
				else 
				{
					Expand(line, false, force, visLevels - 1);
				}
			}
		} 
		else 
		{
			line++;
		}
	}
}

void CScintilla::FoldChanged(int line, int levelNow, int levelPrev) 
{
	//Platform::DebugPrintf("Fold %d %x->%x\n", line, levelPrev, levelNow);
	if (levelNow & SC_FOLDLEVELHEADERFLAG) 
	{
		if (!(levelPrev & SC_FOLDLEVELHEADERFLAG)) 
		{
			// Adding a fold point.
			SetFoldExpanded(line, 1);
			Expand(line, true, false, 0, levelPrev);
		}
	} 
	else if (levelPrev & SC_FOLDLEVELHEADERFLAG) 
	{
		//Platform::DebugPrintf("Fold removed %d-%d\n", line, SendEditor(SCI_GETLASTCHILD, line));
		if (!GetFoldExpanded(line)) 
		{
			// Removing the fold from one that has been contracted so should expand
			// otherwise lines are left invisible with no way to make them visible
			SetFoldExpanded(line, 1);
			Expand(line, true, false, 0, levelPrev);
		}
	}
	
	if (!(levelNow & SC_FOLDLEVELWHITEFLAG) && 
		((levelPrev & SC_FOLDLEVELNUMBERMASK) > (levelNow & SC_FOLDLEVELNUMBERMASK)))
	{
		// See if should still be hidden
		int parentLine = GetFoldParent(line);
		if (parentLine < 0)
		{
			ShowLines(line, line);
		}
		else if (GetFoldExpanded(parentLine) && GetLineVisible(parentLine))
		{
			ShowLines(line, line);
		}
	}
}

// From here on in, we use the generated functions...

//++FuncImp
void CScintilla::AddText(int length, const char* text)
{
	SPerform(SCI_ADDTEXT, (long)length, (long)text);
}

void CScintilla::AddStyledText(int length, char* c)
{
	SPerform(SCI_ADDSTYLEDTEXT, (long)length, (long)c);
}

void CScintilla::InsertText(long pos, const char* text)
{
	SPerform(SCI_INSERTTEXT, pos, (long)text);
}

void CScintilla::ClearAll()
{
	SPerform(SCI_CLEARALL, 0, 0);
}

void CScintilla::ClearDocumentStyle()
{
	SPerform(SCI_CLEARDOCUMENTSTYLE, 0, 0);
}

int CScintilla::GetLength()
{
	return (int)SPerform(SCI_GETLENGTH, 0, 0);
}

int CScintilla::GetCharAt(long pos)
{
	return (int)SPerform(SCI_GETCHARAT, pos, 0);
}

long CScintilla::GetCurrentPos()
{
	return SPerform(SCI_GETCURRENTPOS, 0, 0);
}

long CScintilla::GetAnchor()
{
	return SPerform(SCI_GETANCHOR, 0, 0);
}

int CScintilla::GetStyleAt(long pos)
{
	return (int)SPerform(SCI_GETSTYLEAT, pos, 0);
}

void CScintilla::Redo()
{
	SPerform(SCI_REDO, 0, 0);
}

void CScintilla::SetUndoCollection(bool collectUndo)
{
	SPerform(SCI_SETUNDOCOLLECTION, (long)collectUndo, 0);
}

void CScintilla::SelectAll()
{
	SPerform(SCI_SELECTALL, 0, 0);
}

void CScintilla::SetSavePoint()
{
	SPerform(SCI_SETSAVEPOINT, 0, 0);
}

int CScintilla::GetStyledText(Scintilla::TextRange* tr)
{
	return (int)SPerform(SCI_GETSTYLEDTEXT, 0, (long)tr);
}

bool CScintilla::CanRedo()
{
	return SPerform(SCI_CANREDO, 0, 0) != 0;
}

int CScintilla::MarkerLineFromHandle(int handle)
{
	return (int)SPerform(SCI_MARKERLINEFROMHANDLE, (long)handle, 0);
}

void CScintilla::MarkerDeleteHandle(int handle)
{
	SPerform(SCI_MARKERDELETEHANDLE, (long)handle, 0);
}

bool CScintilla::GetUndoCollection()
{
	return SPerform(SCI_GETUNDOCOLLECTION, 0, 0) != 0;
}

int CScintilla::GetViewWS()
{
	return (int)SPerform(SCI_GETVIEWWS, 0, 0);
}

void CScintilla::SetViewWS(int viewWS)
{
	SPerform(SCI_SETVIEWWS, (long)viewWS, 0);
}

long CScintilla::PositionFromPoint(int x, int y)
{
	return SPerform(SCI_POSITIONFROMPOINT, (long)x, (long)y);
}

long CScintilla::PositionFromPointClose(int x, int y)
{
	return SPerform(SCI_POSITIONFROMPOINTCLOSE, (long)x, (long)y);
}

void CScintilla::GotoLine(int line)
{
	SPerform(SCI_GOTOLINE, (long)line, 0);
}

void CScintilla::GotoPos(long pos)
{
	SPerform(SCI_GOTOPOS, pos, 0);
}

void CScintilla::SetAnchor(long posAnchor)
{
	SPerform(SCI_SETANCHOR, posAnchor, 0);
}

int CScintilla::GetCurLine(int length, char* text)
{
	return (int)SPerform(SCI_GETCURLINE, (long)length, (long)text);
}

long CScintilla::GetEndStyled()
{
	return SPerform(SCI_GETENDSTYLED, 0, 0);
}

void CScintilla::ConvertEOLs(int eolMode)
{
	SPerform(SCI_CONVERTEOLS, (long)eolMode, 0);
}

int CScintilla::GetEOLMode()
{
	return (int)SPerform(SCI_GETEOLMODE, 0, 0);
}

void CScintilla::SetEOLMode(int eolMode)
{
	SPerform(SCI_SETEOLMODE, (long)eolMode, 0);
}

void CScintilla::StartStyling(long pos, int mask)
{
	SPerform(SCI_STARTSTYLING, pos, (long)mask);
}

void CScintilla::SetStyling(int length, int style)
{
	SPerform(SCI_SETSTYLING, (long)length, (long)style);
}

bool CScintilla::GetBufferedDraw()
{
	return SPerform(SCI_GETBUFFEREDDRAW, 0, 0) != 0;
}

void CScintilla::SetBufferedDraw(bool buffered)
{
	SPerform(SCI_SETBUFFEREDDRAW, (long)buffered, 0);
}

void CScintilla::SetTabWidth(int tabWidth)
{
	SPerform(SCI_SETTABWIDTH, (long)tabWidth, 0);
}

int CScintilla::GetTabWidth()
{
	return (int)SPerform(SCI_GETTABWIDTH, 0, 0);
}

void CScintilla::SetCodePage(int codePage)
{
	SPerform(SCI_SETCODEPAGE, (long)codePage, 0);
}

void CScintilla::SetUsePalette(bool usePalette)
{
	SPerform(SCI_SETUSEPALETTE, (long)usePalette, 0);
}

void CScintilla::MarkerDefine(int markerNumber, int markerSymbol)
{
	SPerform(SCI_MARKERDEFINE, (long)markerNumber, (long)markerSymbol);
}

void CScintilla::MarkerSetFore(int markerNumber, COLORREF fore)
{
	SPerform(SCI_MARKERSETFORE, (long)markerNumber, (long)fore);
}

void CScintilla::MarkerSetBack(int markerNumber, COLORREF back)
{
	SPerform(SCI_MARKERSETBACK, (long)markerNumber, (long)back);
}

int CScintilla::MarkerAdd(int line, int markerNumber)
{
	return (int)SPerform(SCI_MARKERADD, (long)line, (long)markerNumber);
}

void CScintilla::MarkerDelete(int line, int markerNumber)
{
	SPerform(SCI_MARKERDELETE, (long)line, (long)markerNumber);
}

void CScintilla::MarkerDeleteAll(int markerNumber)
{
	SPerform(SCI_MARKERDELETEALL, (long)markerNumber, 0);
}

int CScintilla::MarkerGet(int line)
{
	return (int)SPerform(SCI_MARKERGET, (long)line, 0);
}

int CScintilla::MarkerNext(int lineStart, int markerMask)
{
	return (int)SPerform(SCI_MARKERNEXT, (long)lineStart, (long)markerMask);
}

int CScintilla::MarkerPrevious(int lineStart, int markerMask)
{
	return (int)SPerform(SCI_MARKERPREVIOUS, (long)lineStart, (long)markerMask);
}

void CScintilla::MarkerDefinePixmap(int markerNumber, const char* pixmap)
{
	SPerform(SCI_MARKERDEFINEPIXMAP, (long)markerNumber, (long)pixmap);
}

void CScintilla::SetMarginTypeN(int margin, int marginType)
{
	SPerform(SCI_SETMARGINTYPEN, (long)margin, (long)marginType);
}

int CScintilla::GetMarginTypeN(int margin)
{
	return (int)SPerform(SCI_GETMARGINTYPEN, (long)margin, 0);
}

void CScintilla::SetMarginWidthN(int margin, int pixelWidth)
{
	SPerform(SCI_SETMARGINWIDTHN, (long)margin, (long)pixelWidth);
}

int CScintilla::GetMarginWidthN(int margin)
{
	return (int)SPerform(SCI_GETMARGINWIDTHN, (long)margin, 0);
}

void CScintilla::SetMarginMaskN(int margin, int mask)
{
	SPerform(SCI_SETMARGINMASKN, (long)margin, (long)mask);
}

int CScintilla::GetMarginMaskN(int margin)
{
	return (int)SPerform(SCI_GETMARGINMASKN, (long)margin, 0);
}

void CScintilla::SetMarginSensitiveN(int margin, bool sensitive)
{
	SPerform(SCI_SETMARGINSENSITIVEN, (long)margin, (long)sensitive);
}

bool CScintilla::GetMarginSensitiveN(int margin)
{
	return SPerform(SCI_GETMARGINSENSITIVEN, (long)margin, 0) != 0;
}

void CScintilla::StyleClearAll()
{
	SPerform(SCI_STYLECLEARALL, 0, 0);
}

void CScintilla::StyleSetFore(int style, COLORREF fore)
{
	SPerform(SCI_STYLESETFORE, (long)style, (long)fore);
}

void CScintilla::StyleSetBack(int style, COLORREF back)
{
	SPerform(SCI_STYLESETBACK, (long)style, (long)back);
}

void CScintilla::StyleSetBold(int style, bool bold)
{
	SPerform(SCI_STYLESETBOLD, (long)style, (long)bold);
}

void CScintilla::StyleSetItalic(int style, bool italic)
{
	SPerform(SCI_STYLESETITALIC, (long)style, (long)italic);
}

void CScintilla::StyleSetSize(int style, int sizePoints)
{
	SPerform(SCI_STYLESETSIZE, (long)style, (long)sizePoints);
}

void CScintilla::StyleSetFont(int style, const char* fontName)
{
	SPerform(SCI_STYLESETFONT, (long)style, (long)fontName);
}

void CScintilla::StyleSetEOLFilled(int style, bool filled)
{
	SPerform(SCI_STYLESETEOLFILLED, (long)style, (long)filled);
}

void CScintilla::StyleResetDefault()
{
	SPerform(SCI_STYLERESETDEFAULT, 0, 0);
}

void CScintilla::StyleSetUnderline(int style, bool underline)
{
	SPerform(SCI_STYLESETUNDERLINE, (long)style, (long)underline);
}

void CScintilla::StyleSetCase(int style, int caseForce)
{
	SPerform(SCI_STYLESETCASE, (long)style, (long)caseForce);
}

void CScintilla::StyleSetCharacterSet(int style, int characterSet)
{
	SPerform(SCI_STYLESETCHARACTERSET, (long)style, (long)characterSet);
}

void CScintilla::StyleSetHotSpot(int style, bool hotspot)
{
	SPerform(SCI_STYLESETHOTSPOT, (long)style, (long)hotspot);
}

void CScintilla::SetSelFore(bool useSetting, COLORREF fore)
{
	SPerform(SCI_SETSELFORE, (long)useSetting, (long)fore);
}

void CScintilla::SetSelBack(bool useSetting, COLORREF back)
{
	SPerform(SCI_SETSELBACK, (long)useSetting, (long)back);
}

void CScintilla::SetCaretFore(COLORREF fore)
{
	SPerform(SCI_SETCARETFORE, (long)fore, 0);
}

void CScintilla::AssignCmdKey(DWORD km, int msg)
{
	SPerform(SCI_ASSIGNCMDKEY, (long)km, (long)msg);
}

void CScintilla::ClearCmdKey(DWORD km)
{
	SPerform(SCI_CLEARCMDKEY, (long)km, 0);
}

void CScintilla::ClearAllCmdKeys()
{
	SPerform(SCI_CLEARALLCMDKEYS, 0, 0);
}

void CScintilla::SetStylingEx(int length, const char* styles)
{
	SPerform(SCI_SETSTYLINGEX, (long)length, (long)styles);
}

void CScintilla::StyleSetVisible(int style, bool visible)
{
	SPerform(SCI_STYLESETVISIBLE, (long)style, (long)visible);
}

int CScintilla::GetCaretPeriod()
{
	return (int)SPerform(SCI_GETCARETPERIOD, 0, 0);
}

void CScintilla::SetCaretPeriod(int periodMilliseconds)
{
	SPerform(SCI_SETCARETPERIOD, (long)periodMilliseconds, 0);
}

void CScintilla::SetWordChars(const char* characters)
{
	SPerform(SCI_SETWORDCHARS, 0, (long)characters);
}

void CScintilla::BeginUndoAction()
{
	SPerform(SCI_BEGINUNDOACTION, 0, 0);
}

void CScintilla::EndUndoAction()
{
	SPerform(SCI_ENDUNDOACTION, 0, 0);
}

void CScintilla::IndicSetStyle(int indic, int style)
{
	SPerform(SCI_INDICSETSTYLE, (long)indic, (long)style);
}

int CScintilla::IndicGetStyle(int indic)
{
	return (int)SPerform(SCI_INDICGETSTYLE, (long)indic, 0);
}

void CScintilla::IndicSetFore(int indic, COLORREF fore)
{
	SPerform(SCI_INDICSETFORE, (long)indic, (long)fore);
}

COLORREF CScintilla::IndicGetFore(int indic)
{
	return (COLORREF)SPerform(SCI_INDICGETFORE, (long)indic, 0);
}

void CScintilla::SetWhitespaceFore(bool useSetting, COLORREF fore)
{
	SPerform(SCI_SETWHITESPACEFORE, (long)useSetting, (long)fore);
}

void CScintilla::SetWhitespaceBack(bool useSetting, COLORREF back)
{
	SPerform(SCI_SETWHITESPACEBACK, (long)useSetting, (long)back);
}

void CScintilla::SetStyleBits(int bits)
{
	SPerform(SCI_SETSTYLEBITS, (long)bits, 0);
}

int CScintilla::GetStyleBits()
{
	return (int)SPerform(SCI_GETSTYLEBITS, 0, 0);
}

void CScintilla::SetLineState(int line, int state)
{
	SPerform(SCI_SETLINESTATE, (long)line, (long)state);
}

int CScintilla::GetLineState(int line)
{
	return (int)SPerform(SCI_GETLINESTATE, (long)line, 0);
}

int CScintilla::GetMaxLineState()
{
	return (int)SPerform(SCI_GETMAXLINESTATE, 0, 0);
}

bool CScintilla::GetCaretLineVisible()
{
	return SPerform(SCI_GETCARETLINEVISIBLE, 0, 0) != 0;
}

void CScintilla::SetCaretLineVisible(bool show)
{
	SPerform(SCI_SETCARETLINEVISIBLE, (long)show, 0);
}

COLORREF CScintilla::GetCaretLineBack()
{
	return (COLORREF)SPerform(SCI_GETCARETLINEBACK, 0, 0);
}

void CScintilla::SetCaretLineBack(COLORREF back)
{
	SPerform(SCI_SETCARETLINEBACK, (long)back, 0);
}

void CScintilla::StyleSetChangeable(int style, bool changeable)
{
	SPerform(SCI_STYLESETCHANGEABLE, (long)style, (long)changeable);
}

void CScintilla::AutoCShow(int lenEntered, const char* itemList)
{
	SPerform(SCI_AUTOCSHOW, (long)lenEntered, (long)itemList);
}

void CScintilla::AutoCCancel()
{
	SPerform(SCI_AUTOCCANCEL, 0, 0);
}

bool CScintilla::AutoCActive()
{
	return SPerform(SCI_AUTOCACTIVE, 0, 0) != 0;
}

long CScintilla::AutoCPosStart()
{
	return SPerform(SCI_AUTOCPOSSTART, 0, 0);
}

void CScintilla::AutoCComplete()
{
	SPerform(SCI_AUTOCCOMPLETE, 0, 0);
}

void CScintilla::AutoCStops(const char* characterSet)
{
	SPerform(SCI_AUTOCSTOPS, 0, (long)characterSet);
}

void CScintilla::AutoCSetSeparator(int separatorCharacter)
{
	SPerform(SCI_AUTOCSETSEPARATOR, (long)separatorCharacter, 0);
}

int CScintilla::AutoCGetSeparator()
{
	return (int)SPerform(SCI_AUTOCGETSEPARATOR, 0, 0);
}

void CScintilla::AutoCSelect(const char* text)
{
	SPerform(SCI_AUTOCSELECT, 0, (long)text);
}

void CScintilla::AutoCSetCancelAtStart(bool cancel)
{
	SPerform(SCI_AUTOCSETCANCELATSTART, (long)cancel, 0);
}

bool CScintilla::AutoCGetCancelAtStart()
{
	return SPerform(SCI_AUTOCGETCANCELATSTART, 0, 0) != 0;
}

void CScintilla::AutoCSetFillUps(const char* characterSet)
{
	SPerform(SCI_AUTOCSETFILLUPS, 0, (long)characterSet);
}

void CScintilla::AutoCSetChooseSingle(bool chooseSingle)
{
	SPerform(SCI_AUTOCSETCHOOSESINGLE, (long)chooseSingle, 0);
}

bool CScintilla::AutoCGetChooseSingle()
{
	return SPerform(SCI_AUTOCGETCHOOSESINGLE, 0, 0) != 0;
}

void CScintilla::AutoCSetIgnoreCase(bool ignoreCase)
{
	SPerform(SCI_AUTOCSETIGNORECASE, (long)ignoreCase, 0);
}

bool CScintilla::AutoCGetIgnoreCase()
{
	return SPerform(SCI_AUTOCGETIGNORECASE, 0, 0) != 0;
}

void CScintilla::UserListShow(int listType, const char* itemList)
{
	SPerform(SCI_USERLISTSHOW, (long)listType, (long)itemList);
}

void CScintilla::AutoCSetAutoHide(bool autoHide)
{
	SPerform(SCI_AUTOCSETAUTOHIDE, (long)autoHide, 0);
}

bool CScintilla::AutoCGetAutoHide()
{
	return SPerform(SCI_AUTOCGETAUTOHIDE, 0, 0) != 0;
}

void CScintilla::AutoCSetDropRestOfWord(bool dropRestOfWord)
{
	SPerform(SCI_AUTOCSETDROPRESTOFWORD, (long)dropRestOfWord, 0);
}

bool CScintilla::AutoCGetDropRestOfWord()
{
	return SPerform(SCI_AUTOCGETDROPRESTOFWORD, 0, 0) != 0;
}

void CScintilla::RegisterImage(int type, const char* xpmData)
{
	SPerform(SCI_REGISTERIMAGE, (long)type, (long)xpmData);
}

void CScintilla::ClearRegisteredImages()
{
	SPerform(SCI_CLEARREGISTEREDIMAGES, 0, 0);
}

int CScintilla::AutoCGetTypeSeparator()
{
	return (int)SPerform(SCI_AUTOCGETTYPESEPARATOR, 0, 0);
}

void CScintilla::AutoCSetTypeSeparator(int separatorCharacter)
{
	SPerform(SCI_AUTOCSETTYPESEPARATOR, (long)separatorCharacter, 0);
}

void CScintilla::SetIndent(int indentSize)
{
	SPerform(SCI_SETINDENT, (long)indentSize, 0);
}

int CScintilla::GetIndent()
{
	return (int)SPerform(SCI_GETINDENT, 0, 0);
}

void CScintilla::SetUseTabs(bool useTabs)
{
	SPerform(SCI_SETUSETABS, (long)useTabs, 0);
}

bool CScintilla::GetUseTabs()
{
	return SPerform(SCI_GETUSETABS, 0, 0) != 0;
}

void CScintilla::SetLineIndentation(int line, int indentSize)
{
	SPerform(SCI_SETLINEINDENTATION, (long)line, (long)indentSize);
}

int CScintilla::GetLineIndentation(int line)
{
	return (int)SPerform(SCI_GETLINEINDENTATION, (long)line, 0);
}

long CScintilla::GetLineIndentPosition(int line)
{
	return SPerform(SCI_GETLINEINDENTPOSITION, (long)line, 0);
}

int CScintilla::GetColumn(long pos)
{
	return (int)SPerform(SCI_GETCOLUMN, pos, 0);
}

void CScintilla::SetHScrollBar(bool show)
{
	SPerform(SCI_SETHSCROLLBAR, (long)show, 0);
}

bool CScintilla::GetHScrollBar()
{
	return SPerform(SCI_GETHSCROLLBAR, 0, 0) != 0;
}

void CScintilla::SetIndentationGuides(bool show)
{
	SPerform(SCI_SETINDENTATIONGUIDES, (long)show, 0);
}

bool CScintilla::GetIndentationGuides()
{
	return SPerform(SCI_GETINDENTATIONGUIDES, 0, 0) != 0;
}

void CScintilla::SetHighlightGuide(int column)
{
	SPerform(SCI_SETHIGHLIGHTGUIDE, (long)column, 0);
}

int CScintilla::GetHighlightGuide()
{
	return (int)SPerform(SCI_GETHIGHLIGHTGUIDE, 0, 0);
}

int CScintilla::GetLineEndPosition(int line)
{
	return (int)SPerform(SCI_GETLINEENDPOSITION, (long)line, 0);
}

int CScintilla::GetCodePage()
{
	return (int)SPerform(SCI_GETCODEPAGE, 0, 0);
}

COLORREF CScintilla::GetCaretFore()
{
	return (COLORREF)SPerform(SCI_GETCARETFORE, 0, 0);
}

bool CScintilla::GetUsePalette()
{
	return SPerform(SCI_GETUSEPALETTE, 0, 0) != 0;
}

bool CScintilla::GetReadOnly()
{
	return SPerform(SCI_GETREADONLY, 0, 0) != 0;
}

void CScintilla::SetCurrentPos(long pos)
{
	SPerform(SCI_SETCURRENTPOS, pos, 0);
}

void CScintilla::SetSelectionStart(long pos)
{
	SPerform(SCI_SETSELECTIONSTART, pos, 0);
}

long CScintilla::GetSelectionStart()
{
	return SPerform(SCI_GETSELECTIONSTART, 0, 0);
}

void CScintilla::SetSelectionEnd(long pos)
{
	SPerform(SCI_SETSELECTIONEND, pos, 0);
}

long CScintilla::GetSelectionEnd()
{
	return SPerform(SCI_GETSELECTIONEND, 0, 0);
}

void CScintilla::SetPrintMagnification(int magnification)
{
	SPerform(SCI_SETPRINTMAGNIFICATION, (long)magnification, 0);
}

int CScintilla::GetPrintMagnification()
{
	return (int)SPerform(SCI_GETPRINTMAGNIFICATION, 0, 0);
}

void CScintilla::SetPrintColourMode(int mode)
{
	SPerform(SCI_SETPRINTCOLOURMODE, (long)mode, 0);
}

int CScintilla::GetPrintColourMode()
{
	return (int)SPerform(SCI_GETPRINTCOLOURMODE, 0, 0);
}

long CScintilla::FindText(int flags, Scintilla::TextToFind* ft)
{
	return SPerform(SCI_FINDTEXT, (long)flags, (long)ft);
}

long CScintilla::FormatRange(bool draw, long fr)
{
	return SPerform(SCI_FORMATRANGE, (long)draw, fr);
}

int CScintilla::GetFirstVisibleLine()
{
	return (int)SPerform(SCI_GETFIRSTVISIBLELINE, 0, 0);
}

int CScintilla::GetLine(int line, char* text)
{
	return (int)SPerform(SCI_GETLINE, (long)line, (long)text);
}

int CScintilla::GetLineCount()
{
	return (int)SPerform(SCI_GETLINECOUNT, 0, 0);
}

void CScintilla::SetMarginLeft(int pixelWidth)
{
	SPerform(SCI_SETMARGINLEFT, 0, (long)pixelWidth);
}

int CScintilla::GetMarginLeft()
{
	return (int)SPerform(SCI_GETMARGINLEFT, 0, 0);
}

void CScintilla::SetMarginRight(int pixelWidth)
{
	SPerform(SCI_SETMARGINRIGHT, 0, (long)pixelWidth);
}

int CScintilla::GetMarginRight()
{
	return (int)SPerform(SCI_GETMARGINRIGHT, 0, 0);
}

bool CScintilla::GetModify()
{
	return SPerform(SCI_GETMODIFY, 0, 0) != 0;
}

void CScintilla::SetSel(long start, long end)
{
	SPerform(SCI_SETSEL, start, end);
}

int CScintilla::GetSelText(char* text)
{
	return (int)SPerform(SCI_GETSELTEXT, 0, (long)text);
}

std::string CScintilla::GetSelText()
{
	std::string ret;
	
	// First instance, GetSelText returns buffer size required to store text including NULL
	int selLength = GetSelText(NULL);

	if (selLength == 1)
	{
		return "";
	}

	ret.resize(selLength);
	
	// Now GetSelText returns:
	// 1. If the document is empty: 0
	// 2. Otherwise: number of characters written, including NULL
	selLength = GetSelText(&ret[0]);
	if (selLength == 0)
	{
		return "";
	}

	ret.resize(selLength - 1);
	
	return ret;
}

int CScintilla::GetTextRange(Scintilla::TextRange* tr)
{
	return (int)SPerform(SCI_GETTEXTRANGE, 0, (long)tr);
}

std::string CScintilla::GetTextRange(int start, int end)
{
	std::string ret;
	
	// First instance, GetSelText returns buffer size required to store text including NULL
	int selLength = end - start;

	ret.resize(selLength + 1);
	
	Scintilla::Sci_TextRange range;
	range.chrg.cpMin = start;
	range.chrg.cpMax = end;
	range.lpstrText = &ret[0];
	
	int len = GetTextRange(&range);
	ret.resize(len);
	
	return ret;
}

void CScintilla::HideSelection(bool normal)
{
	SPerform(SCI_HIDESELECTION, (long)normal, 0);
}

int CScintilla::PointXFromPosition(long pos)
{
	return (int)SPerform(SCI_POINTXFROMPOSITION, 0, pos);
}

int CScintilla::PointYFromPosition(long pos)
{
	return (int)SPerform(SCI_POINTYFROMPOSITION, 0, pos);
}

int CScintilla::LineFromPosition(long pos)
{
	return (int)SPerform(SCI_LINEFROMPOSITION, pos, 0);
}

long CScintilla::PositionFromLine(int line)
{
	return SPerform(SCI_POSITIONFROMLINE, (long)line, 0);
}

void CScintilla::LineScroll(int columns, int lines)
{
	SPerform(SCI_LINESCROLL, (long)columns, (long)lines);
}

void CScintilla::ScrollCaret()
{
	SPerform(SCI_SCROLLCARET, 0, 0);
}

void CScintilla::ReplaceSel(const char* text)
{
	SPerform(SCI_REPLACESEL, 0, (long)text);
}

void CScintilla::SetReadOnly(bool readOnly)
{
	SPerform(SCI_SETREADONLY, (long)readOnly, 0);
}

void CScintilla::Null()
{
	SPerform(SCI_NULL, 0, 0);
}

bool CScintilla::CanPaste()
{
	return SPerform(SCI_CANPASTE, 0, 0) != 0;
}

bool CScintilla::CanUndo()
{
	return SPerform(SCI_CANUNDO, 0, 0) != 0;
}

void CScintilla::EmptyUndoBuffer()
{
	SPerform(SCI_EMPTYUNDOBUFFER, 0, 0);
}

void CScintilla::Undo()
{
	SPerform(SCI_UNDO, 0, 0);
}

void CScintilla::Cut()
{
	SPerform(SCI_CUT, 0, 0);
}

void CScintilla::Copy()
{
	SPerform(SCI_COPY, 0, 0);
}

void CScintilla::Paste()
{
	SPerform(SCI_PASTE, 0, 0);
}

void CScintilla::Clear()
{
	SPerform(SCI_CLEAR, 0, 0);
}

void CScintilla::SetText(const char* text)
{
	SPerform(SCI_SETTEXT, 0, (long)text);
}

int CScintilla::GetText(int length, char* text)
{
	return (int)SPerform(SCI_GETTEXT, (long)length, (long)text);
}

int CScintilla::GetTextLength()
{
	return (int)SPerform(SCI_GETTEXTLENGTH, 0, 0);
}

int CScintilla::GetDirectFunction()
{
	return (int)SPerform(SCI_GETDIRECTFUNCTION, 0, 0);
}

int CScintilla::GetDirectPointer()
{
	return (int)SPerform(SCI_GETDIRECTPOINTER, 0, 0);
}

void CScintilla::SetOvertype(bool overtype)
{
	SPerform(SCI_SETOVERTYPE, (long)overtype, 0);
}

bool CScintilla::GetOvertype()
{
	return SPerform(SCI_GETOVERTYPE, 0, 0) != 0;
}

void CScintilla::SetCaretWidth(int pixelWidth)
{
	SPerform(SCI_SETCARETWIDTH, (long)pixelWidth, 0);
}

int CScintilla::GetCaretWidth()
{
	return (int)SPerform(SCI_GETCARETWIDTH, 0, 0);
}

void CScintilla::SetTargetStart(long pos)
{
	SPerform(SCI_SETTARGETSTART, pos, 0);
}

long CScintilla::GetTargetStart()
{
	return SPerform(SCI_GETTARGETSTART, 0, 0);
}

void CScintilla::SetTargetEnd(long pos)
{
	SPerform(SCI_SETTARGETEND, pos, 0);
}

long CScintilla::GetTargetEnd()
{
	return SPerform(SCI_GETTARGETEND, 0, 0);
}

int CScintilla::ReplaceTarget(int length, const char* text)
{
	return (int)SPerform(SCI_REPLACETARGET, (long)length, (long)text);
}

int CScintilla::ReplaceTargetRE(int length, const char* text)
{
	return (int)SPerform(SCI_REPLACETARGETRE, (long)length, (long)text);
}

int CScintilla::SearchInTarget(int length, const char* text)
{
	return (int)SPerform(SCI_SEARCHINTARGET, (long)length, (long)text);
}

void CScintilla::SetSearchFlags(int flags)
{
	SPerform(SCI_SETSEARCHFLAGS, (long)flags, 0);
}

int CScintilla::GetSearchFlags()
{
	return (int)SPerform(SCI_GETSEARCHFLAGS, 0, 0);
}

void CScintilla::CallTipShow(long pos, const char* definition)
{
	SPerform(SCI_CALLTIPSHOW, pos, (long)definition);
}

void CScintilla::CallTipCancel()
{
	SPerform(SCI_CALLTIPCANCEL, 0, 0);
}

bool CScintilla::CallTipActive()
{
	return SPerform(SCI_CALLTIPACTIVE, 0, 0) != 0;
}

long CScintilla::CallTipPosStart()
{
	return SPerform(SCI_CALLTIPPOSSTART, 0, 0);
}

void CScintilla::CallTipSetHlt(int start, int end)
{
	SPerform(SCI_CALLTIPSETHLT, (long)start, (long)end);
}

void CScintilla::CallTipSetBack(COLORREF back)
{
	SPerform(SCI_CALLTIPSETBACK, (long)back, 0);
}

void CScintilla::CallTipSetFore(COLORREF fore)
{
	SPerform(SCI_CALLTIPSETFORE, (long)fore, 0);
}

void CScintilla::CallTipSetForeHlt(COLORREF fore)
{
	SPerform(SCI_CALLTIPSETFOREHLT, (long)fore, 0);
}

int CScintilla::VisibleFromDocLine(int line)
{
	return (int)SPerform(SCI_VISIBLEFROMDOCLINE, (long)line, 0);
}

int CScintilla::DocLineFromVisible(int lineDisplay)
{
	return (int)SPerform(SCI_DOCLINEFROMVISIBLE, (long)lineDisplay, 0);
}

void CScintilla::SetFoldLevel(int line, int level)
{
	SPerform(SCI_SETFOLDLEVEL, (long)line, (long)level);
}

int CScintilla::GetFoldLevel(int line)
{
	return (int)SPerform(SCI_GETFOLDLEVEL, (long)line, 0);
}

int CScintilla::GetLastChild(int line, int level)
{
	return (int)SPerform(SCI_GETLASTCHILD, (long)line, (long)level);
}

int CScintilla::GetFoldParent(int line)
{
	return (int)SPerform(SCI_GETFOLDPARENT, (long)line, 0);
}

void CScintilla::ShowLines(int lineStart, int lineEnd)
{
	SPerform(SCI_SHOWLINES, (long)lineStart, (long)lineEnd);
}

void CScintilla::HideLines(int lineStart, int lineEnd)
{
	SPerform(SCI_HIDELINES, (long)lineStart, (long)lineEnd);
}

bool CScintilla::GetLineVisible(int line)
{
	return SPerform(SCI_GETLINEVISIBLE, (long)line, 0) != 0;
}

void CScintilla::SetFoldExpanded(int line, bool expanded)
{
	SPerform(SCI_SETFOLDEXPANDED, (long)line, (long)expanded);
}

bool CScintilla::GetFoldExpanded(int line)
{
	return SPerform(SCI_GETFOLDEXPANDED, (long)line, 0) != 0;
}

void CScintilla::ToggleFold(int line)
{
	SPerform(SCI_TOGGLEFOLD, (long)line, 0);
}

void CScintilla::EnsureVisible(int line)
{
	SPerform(SCI_ENSUREVISIBLE, (long)line, 0);
}

void CScintilla::SetFoldFlags(int flags)
{
	SPerform(SCI_SETFOLDFLAGS, (long)flags, 0);
}

void CScintilla::EnsureVisibleEnforcePolicy(int line)
{
	SPerform(SCI_ENSUREVISIBLEENFORCEPOLICY, (long)line, 0);
}

void CScintilla::SetTabIndents(bool tabIndents)
{
	SPerform(SCI_SETTABINDENTS, (long)tabIndents, 0);
}

bool CScintilla::GetTabIndents()
{
	return SPerform(SCI_GETTABINDENTS, 0, 0) != 0;
}

void CScintilla::SetBackSpaceUnIndents(bool bsUnIndents)
{
	SPerform(SCI_SETBACKSPACEUNINDENTS, (long)bsUnIndents, 0);
}

bool CScintilla::GetBackSpaceUnIndents()
{
	return SPerform(SCI_GETBACKSPACEUNINDENTS, 0, 0) != 0;
}

void CScintilla::SetMouseDwellTime(int periodMilliseconds)
{
	SPerform(SCI_SETMOUSEDWELLTIME, (long)periodMilliseconds, 0);
}

int CScintilla::GetMouseDwellTime()
{
	return (int)SPerform(SCI_GETMOUSEDWELLTIME, 0, 0);
}

int CScintilla::WordStartPosition(long pos, bool onlyWordCharacters)
{
	return (int)SPerform(SCI_WORDSTARTPOSITION, pos, (long)onlyWordCharacters);
}

int CScintilla::WordEndPosition(long pos, bool onlyWordCharacters)
{
	return (int)SPerform(SCI_WORDENDPOSITION, pos, (long)onlyWordCharacters);
}

void CScintilla::SetWrapMode(int mode)
{
	SPerform(SCI_SETWRAPMODE, (long)mode, 0);
}

int CScintilla::GetWrapMode()
{
	return (int)SPerform(SCI_GETWRAPMODE, 0, 0);
}

void CScintilla::SetLayoutCache(int mode)
{
	SPerform(SCI_SETLAYOUTCACHE, (long)mode, 0);
}

int CScintilla::GetLayoutCache()
{
	return (int)SPerform(SCI_GETLAYOUTCACHE, 0, 0);
}

void CScintilla::SetScrollWidth(int pixelWidth)
{
	SPerform(SCI_SETSCROLLWIDTH, (long)pixelWidth, 0);
}

int CScintilla::GetScrollWidth()
{
	return (int)SPerform(SCI_GETSCROLLWIDTH, 0, 0);
}

int CScintilla::TextWidth(int style, const char* text)
{
	return (int)SPerform(SCI_TEXTWIDTH, (long)style, (long)text);
}

void CScintilla::SetEndAtLastLine(bool endAtLastLine)
{
	SPerform(SCI_SETENDATLASTLINE, (long)endAtLastLine, 0);
}

int CScintilla::GetEndAtLastLine()
{
	return (int)SPerform(SCI_GETENDATLASTLINE, 0, 0);
}

int CScintilla::TextHeight(int line)
{
	return (int)SPerform(SCI_TEXTHEIGHT, (long)line, 0);
}

void CScintilla::SetVScrollBar(bool show)
{
	SPerform(SCI_SETVSCROLLBAR, (long)show, 0);
}

bool CScintilla::GetVScrollBar()
{
	return SPerform(SCI_GETVSCROLLBAR, 0, 0) != 0;
}

void CScintilla::AppendText(int length, const char* text)
{
	SPerform(SCI_APPENDTEXT, (long)length, (long)text);
}

bool CScintilla::GetTwoPhaseDraw()
{
	return SPerform(SCI_GETTWOPHASEDRAW, 0, 0) != 0;
}

void CScintilla::SetTwoPhaseDraw(bool twoPhase)
{
	SPerform(SCI_SETTWOPHASEDRAW, (long)twoPhase, 0);
}

void CScintilla::TargetFromSelection()
{
	SPerform(SCI_TARGETFROMSELECTION, 0, 0);
}

void CScintilla::LinesJoin()
{
	SPerform(SCI_LINESJOIN, 0, 0);
}

void CScintilla::LinesSplit(int pixelWidth)
{
	SPerform(SCI_LINESSPLIT, (long)pixelWidth, 0);
}

void CScintilla::SetFoldMarginColour(bool useSetting, COLORREF back)
{
	SPerform(SCI_SETFOLDMARGINCOLOUR, (long)useSetting, (long)back);
}

void CScintilla::SetFoldMarginHiColour(bool useSetting, COLORREF fore)
{
	SPerform(SCI_SETFOLDMARGINHICOLOUR, (long)useSetting, (long)fore);
}

void CScintilla::LineDown()
{
	SPerform(SCI_LINEDOWN, 0, 0);
}

void CScintilla::LineDownExtend()
{
	SPerform(SCI_LINEDOWNEXTEND, 0, 0);
}

void CScintilla::LineUp()
{
	SPerform(SCI_LINEUP, 0, 0);
}

void CScintilla::LineUpExtend()
{
	SPerform(SCI_LINEUPEXTEND, 0, 0);
}

void CScintilla::CharLeft()
{
	SPerform(SCI_CHARLEFT, 0, 0);
}

void CScintilla::CharLeftExtend()
{
	SPerform(SCI_CHARLEFTEXTEND, 0, 0);
}

void CScintilla::CharRight()
{
	SPerform(SCI_CHARRIGHT, 0, 0);
}

void CScintilla::CharRightExtend()
{
	SPerform(SCI_CHARRIGHTEXTEND, 0, 0);
}

void CScintilla::WordLeft()
{
	SPerform(SCI_WORDLEFT, 0, 0);
}

void CScintilla::WordLeftExtend()
{
	SPerform(SCI_WORDLEFTEXTEND, 0, 0);
}

void CScintilla::WordRight()
{
	SPerform(SCI_WORDRIGHT, 0, 0);
}

void CScintilla::WordRightExtend()
{
	SPerform(SCI_WORDRIGHTEXTEND, 0, 0);
}

void CScintilla::Home()
{
	SPerform(SCI_HOME, 0, 0);
}

void CScintilla::HomeExtend()
{
	SPerform(SCI_HOMEEXTEND, 0, 0);
}

void CScintilla::LineEnd()
{
	SPerform(SCI_LINEEND, 0, 0);
}

void CScintilla::LineEndExtend()
{
	SPerform(SCI_LINEENDEXTEND, 0, 0);
}

void CScintilla::DocumentStart()
{
	SPerform(SCI_DOCUMENTSTART, 0, 0);
}

void CScintilla::DocumentStartExtend()
{
	SPerform(SCI_DOCUMENTSTARTEXTEND, 0, 0);
}

void CScintilla::DocumentEnd()
{
	SPerform(SCI_DOCUMENTEND, 0, 0);
}

void CScintilla::DocumentEndExtend()
{
	SPerform(SCI_DOCUMENTENDEXTEND, 0, 0);
}

void CScintilla::PageUp()
{
	SPerform(SCI_PAGEUP, 0, 0);
}

void CScintilla::PageUpExtend()
{
	SPerform(SCI_PAGEUPEXTEND, 0, 0);
}

void CScintilla::PageDown()
{
	SPerform(SCI_PAGEDOWN, 0, 0);
}

void CScintilla::PageDownExtend()
{
	SPerform(SCI_PAGEDOWNEXTEND, 0, 0);
}

void CScintilla::EditToggleOvertype()
{
	SPerform(SCI_EDITTOGGLEOVERTYPE, 0, 0);
}

void CScintilla::Cancel()
{
	SPerform(SCI_CANCEL, 0, 0);
}

void CScintilla::DeleteBack()
{
	SPerform(SCI_DELETEBACK, 0, 0);
}

void CScintilla::Tab()
{
	SPerform(SCI_TAB, 0, 0);
}

void CScintilla::BackTab()
{
	SPerform(SCI_BACKTAB, 0, 0);
}

void CScintilla::NewLine()
{
	SPerform(SCI_NEWLINE, 0, 0);
}

void CScintilla::FormFeed()
{
	SPerform(SCI_FORMFEED, 0, 0);
}

void CScintilla::VCHome()
{
	SPerform(SCI_VCHOME, 0, 0);
}

void CScintilla::VCHomeExtend()
{
	SPerform(SCI_VCHOMEEXTEND, 0, 0);
}

void CScintilla::ZoomIn()
{
	SPerform(SCI_ZOOMIN, 0, 0);
}

void CScintilla::ZoomOut()
{
	SPerform(SCI_ZOOMOUT, 0, 0);
}

void CScintilla::DelWordLeft()
{
	SPerform(SCI_DELWORDLEFT, 0, 0);
}

void CScintilla::DelWordRight()
{
	SPerform(SCI_DELWORDRIGHT, 0, 0);
}

void CScintilla::LineCut()
{
	SPerform(SCI_LINECUT, 0, 0);
}

void CScintilla::LineDelete()
{
	SPerform(SCI_LINEDELETE, 0, 0);
}

void CScintilla::LineTranspose()
{
	SPerform(SCI_LINETRANSPOSE, 0, 0);
}

void CScintilla::LineDuplicate()
{
	SPerform(SCI_LINEDUPLICATE, 0, 0);
}

void CScintilla::LowerCase()
{
	SPerform(SCI_LOWERCASE, 0, 0);
}

void CScintilla::UpperCase()
{
	SPerform(SCI_UPPERCASE, 0, 0);
}

void CScintilla::LineScrollDown()
{
	SPerform(SCI_LINESCROLLDOWN, 0, 0);
}

void CScintilla::LineScrollUp()
{
	SPerform(SCI_LINESCROLLUP, 0, 0);
}

void CScintilla::DeleteBackNotLine()
{
	SPerform(SCI_DELETEBACKNOTLINE, 0, 0);
}

void CScintilla::HomeDisplay()
{
	SPerform(SCI_HOMEDISPLAY, 0, 0);
}

void CScintilla::HomeDisplayExtend()
{
	SPerform(SCI_HOMEDISPLAYEXTEND, 0, 0);
}

void CScintilla::LineEndDisplay()
{
	SPerform(SCI_LINEENDDISPLAY, 0, 0);
}

void CScintilla::LineEndDisplayExtend()
{
	SPerform(SCI_LINEENDDISPLAYEXTEND, 0, 0);
}

void CScintilla::HomeWrap()
{
	SPerform(SCI_HOMEWRAP, 0, 0);
}

void CScintilla::HomeWrapExtend()
{
	SPerform(SCI_HOMEWRAPEXTEND, 0, 0);
}

void CScintilla::LineEndWrap()
{
	SPerform(SCI_LINEENDWRAP, 0, 0);
}

void CScintilla::LineEndWrapExtend()
{
	SPerform(SCI_LINEENDWRAPEXTEND, 0, 0);
}

void CScintilla::VCHomeWrap()
{
	SPerform(SCI_VCHOMEWRAP, 0, 0);
}

void CScintilla::VCHomeWrapExtend()
{
	SPerform(SCI_VCHOMEWRAPEXTEND, 0, 0);
}

void CScintilla::LineCopy()
{
	SPerform(SCI_LINECOPY, 0, 0);
}

void CScintilla::MoveCaretInsideView()
{
	SPerform(SCI_MOVECARETINSIDEVIEW, 0, 0);
}

int CScintilla::LineLength(int line)
{
	return (int)SPerform(SCI_LINELENGTH, (long)line, 0);
}

void CScintilla::BraceHighlight(long pos1, long pos2)
{
	SPerform(SCI_BRACEHIGHLIGHT, pos1, pos2);
}

void CScintilla::BraceBadLight(long pos)
{
	SPerform(SCI_BRACEBADLIGHT, pos, 0);
}

long CScintilla::BraceMatch(long pos)
{
	return SPerform(SCI_BRACEMATCH, pos, 0);
}

bool CScintilla::GetViewEOL()
{
	return SPerform(SCI_GETVIEWEOL, 0, 0) != 0;
}

void CScintilla::SetViewEOL(bool visible)
{
	SPerform(SCI_SETVIEWEOL, (long)visible, 0);
}

int CScintilla::GetDocPointer()
{
	return (int)SPerform(SCI_GETDOCPOINTER, 0, 0);
}

void CScintilla::SetDocPointer(int pointer)
{
	SPerform(SCI_SETDOCPOINTER, 0, (long)pointer);
}

void CScintilla::SetModEventMask(int mask)
{
	SPerform(SCI_SETMODEVENTMASK, (long)mask, 0);
}

int CScintilla::GetEdgeColumn()
{
	return (int)SPerform(SCI_GETEDGECOLUMN, 0, 0);
}

void CScintilla::SetEdgeColumn(int column)
{
	SPerform(SCI_SETEDGECOLUMN, (long)column, 0);
}

int CScintilla::GetEdgeMode()
{
	return (int)SPerform(SCI_GETEDGEMODE, 0, 0);
}

void CScintilla::SetEdgeMode(int mode)
{
	SPerform(SCI_SETEDGEMODE, (long)mode, 0);
}

COLORREF CScintilla::GetEdgeColour()
{
	return (COLORREF)SPerform(SCI_GETEDGECOLOUR, 0, 0);
}

void CScintilla::SetEdgeColour(COLORREF edgeColour)
{
	SPerform(SCI_SETEDGECOLOUR, (long)edgeColour, 0);
}

void CScintilla::SearchAnchor()
{
	SPerform(SCI_SEARCHANCHOR, 0, 0);
}

int CScintilla::SearchNext(int flags, const char* text)
{
	return (int)SPerform(SCI_SEARCHNEXT, (long)flags, (long)text);
}

int CScintilla::SearchPrev(int flags, const char* text)
{
	return (int)SPerform(SCI_SEARCHPREV, (long)flags, (long)text);
}

int CScintilla::LinesOnScreen()
{
	return (int)SPerform(SCI_LINESONSCREEN, 0, 0);
}

void CScintilla::UsePopUp(bool allowPopUp)
{
	SPerform(SCI_USEPOPUP, (long)allowPopUp, 0);
}

bool CScintilla::SelectionIsRectangle()
{
	return SPerform(SCI_SELECTIONISRECTANGLE, 0, 0) != 0;
}

void CScintilla::SetZoom(int zoom)
{
	SPerform(SCI_SETZOOM, (long)zoom, 0);
}

int CScintilla::GetZoom()
{
	return (int)SPerform(SCI_GETZOOM, 0, 0);
}

int CScintilla::CreateDocument()
{
	return (int)SPerform(SCI_CREATEDOCUMENT, 0, 0);
}

void CScintilla::AddRefDocument(int doc)
{
	SPerform(SCI_ADDREFDOCUMENT, 0, (long)doc);
}

void CScintilla::ReleaseDocument(int doc)
{
	SPerform(SCI_RELEASEDOCUMENT, 0, (long)doc);
}

int CScintilla::GetModEventMask()
{
	return (int)SPerform(SCI_GETMODEVENTMASK, 0, 0);
}

void CScintilla::SetEditorFocus(bool focus)
{
	SPerform(SCI_SETFOCUS, (long)focus, 0);
}

bool CScintilla::GetFocus()
{
	return SPerform(SCI_GETFOCUS, 0, 0) != 0;
}

void CScintilla::SetStatus(int statusCode)
{
	SPerform(SCI_SETSTATUS, (long)statusCode, 0);
}

int CScintilla::GetStatus()
{
	return (int)SPerform(SCI_GETSTATUS, 0, 0);
}

void CScintilla::SetMouseDownCaptures(bool captures)
{
	SPerform(SCI_SETMOUSEDOWNCAPTURES, (long)captures, 0);
}

bool CScintilla::GetMouseDownCaptures()
{
	return SPerform(SCI_GETMOUSEDOWNCAPTURES, 0, 0) != 0;
}

void CScintilla::SetCursor(int cursorType)
{
	SPerform(SCI_SETCURSOR, (long)cursorType, 0);
}

int CScintilla::GetCursor()
{
	return (int)SPerform(SCI_GETCURSOR, 0, 0);
}

void CScintilla::SetControlCharSymbol(int symbol)
{
	SPerform(SCI_SETCONTROLCHARSYMBOL, (long)symbol, 0);
}

int CScintilla::GetControlCharSymbol()
{
	return (int)SPerform(SCI_GETCONTROLCHARSYMBOL, 0, 0);
}

void CScintilla::WordPartLeft()
{
	SPerform(SCI_WORDPARTLEFT, 0, 0);
}

void CScintilla::WordPartLeftExtend()
{
	SPerform(SCI_WORDPARTLEFTEXTEND, 0, 0);
}

void CScintilla::WordPartRight()
{
	SPerform(SCI_WORDPARTRIGHT, 0, 0);
}

void CScintilla::WordPartRightExtend()
{
	SPerform(SCI_WORDPARTRIGHTEXTEND, 0, 0);
}

void CScintilla::SetVisiblePolicy(int visiblePolicy, int visibleSlop)
{
	SPerform(SCI_SETVISIBLEPOLICY, (long)visiblePolicy, (long)visibleSlop);
}

void CScintilla::DelLineLeft()
{
	SPerform(SCI_DELLINELEFT, 0, 0);
}

void CScintilla::DelLineRight()
{
	SPerform(SCI_DELLINERIGHT, 0, 0);
}

void CScintilla::SetXOffset(int newOffset)
{
	SPerform(SCI_SETXOFFSET, (long)newOffset, 0);
}

int CScintilla::GetXOffset()
{
	return (int)SPerform(SCI_GETXOFFSET, 0, 0);
}

void CScintilla::ChooseCaretX()
{
	SPerform(SCI_CHOOSECARETX, 0, 0);
}

void CScintilla::GrabFocus()
{
	SPerform(SCI_GRABFOCUS, 0, 0);
}

void CScintilla::SetXCaretPolicy(int caretPolicy, int caretSlop)
{
	SPerform(SCI_SETXCARETPOLICY, (long)caretPolicy, (long)caretSlop);
}

void CScintilla::SetYCaretPolicy(int caretPolicy, int caretSlop)
{
	SPerform(SCI_SETYCARETPOLICY, (long)caretPolicy, (long)caretSlop);
}

void CScintilla::SetPrintWrapMode(int mode)
{
	SPerform(SCI_SETPRINTWRAPMODE, (long)mode, 0);
}

int CScintilla::GetPrintWrapMode()
{
	return (int)SPerform(SCI_GETPRINTWRAPMODE, 0, 0);
}

void CScintilla::SetHotspotActiveFore(bool useSetting, COLORREF fore)
{
	SPerform(SCI_SETHOTSPOTACTIVEFORE, (long)useSetting, (long)fore);
}

void CScintilla::SetHotspotActiveBack(bool useSetting, COLORREF back)
{
	SPerform(SCI_SETHOTSPOTACTIVEBACK, (long)useSetting, (long)back);
}

void CScintilla::SetHotspotActiveUnderline(bool underline)
{
	SPerform(SCI_SETHOTSPOTACTIVEUNDERLINE, (long)underline, 0);
}

void CScintilla::SetHotspotSingleLine(bool singleLine)
{
	SPerform(SCI_SETHOTSPOTSINGLELINE, (long)singleLine, 0);
}

void CScintilla::ParaDown()
{
	SPerform(SCI_PARADOWN, 0, 0);
}

void CScintilla::ParaDownExtend()
{
	SPerform(SCI_PARADOWNEXTEND, 0, 0);
}

void CScintilla::ParaUp()
{
	SPerform(SCI_PARAUP, 0, 0);
}

void CScintilla::ParaUpExtend()
{
	SPerform(SCI_PARAUPEXTEND, 0, 0);
}

long CScintilla::PositionBefore(long pos)
{
	return SPerform(SCI_POSITIONBEFORE, pos, 0);
}

long CScintilla::PositionAfter(long pos)
{
	return SPerform(SCI_POSITIONAFTER, pos, 0);
}

void CScintilla::CopyRange(long start, long end)
{
	SPerform(SCI_COPYRANGE, start, end);
}

void CScintilla::CopyText(int length, const char* text)
{
	SPerform(SCI_COPYTEXT, (long)length, (long)text);
}

void CScintilla::SetSelectionMode(int mode)
{
	SPerform(SCI_SETSELECTIONMODE, (long)mode, 0);
}

int CScintilla::GetSelectionMode()
{
	return (int)SPerform(SCI_GETSELECTIONMODE, 0, 0);
}

long CScintilla::GetLineSelStartPosition(int line)
{
	return SPerform(SCI_GETLINESELSTARTPOSITION, (long)line, 0);
}

long CScintilla::GetLineSelEndPosition(int line)
{
	return SPerform(SCI_GETLINESELENDPOSITION, (long)line, 0);
}

void CScintilla::LineDownRectExtend()
{
	SPerform(SCI_LINEDOWNRECTEXTEND, 0, 0);
}

void CScintilla::LineUpRectExtend()
{
	SPerform(SCI_LINEUPRECTEXTEND, 0, 0);
}

void CScintilla::CharLeftRectExtend()
{
	SPerform(SCI_CHARLEFTRECTEXTEND, 0, 0);
}

void CScintilla::CharRightRectExtend()
{
	SPerform(SCI_CHARRIGHTRECTEXTEND, 0, 0);
}

void CScintilla::HomeRectExtend()
{
	SPerform(SCI_HOMERECTEXTEND, 0, 0);
}

void CScintilla::VCHomeRectExtend()
{
	SPerform(SCI_VCHOMERECTEXTEND, 0, 0);
}

void CScintilla::LineEndRectExtend()
{
	SPerform(SCI_LINEENDRECTEXTEND, 0, 0);
}

void CScintilla::PageUpRectExtend()
{
	SPerform(SCI_PAGEUPRECTEXTEND, 0, 0);
}

void CScintilla::PageDownRectExtend()
{
	SPerform(SCI_PAGEDOWNRECTEXTEND, 0, 0);
}

void CScintilla::StutteredPageUp()
{
	SPerform(SCI_STUTTEREDPAGEUP, 0, 0);
}

void CScintilla::StutteredPageUpExtend()
{
	SPerform(SCI_STUTTEREDPAGEUPEXTEND, 0, 0);
}

void CScintilla::StutteredPageDown()
{
	SPerform(SCI_STUTTEREDPAGEDOWN, 0, 0);
}

void CScintilla::StutteredPageDownExtend()
{
	SPerform(SCI_STUTTEREDPAGEDOWNEXTEND, 0, 0);
}

void CScintilla::WordLeftEnd()
{
	SPerform(SCI_WORDLEFTEND, 0, 0);
}

void CScintilla::WordLeftEndExtend()
{
	SPerform(SCI_WORDLEFTENDEXTEND, 0, 0);
}

void CScintilla::WordRightEnd()
{
	SPerform(SCI_WORDRIGHTEND, 0, 0);
}

void CScintilla::WordRightEndExtend()
{
	SPerform(SCI_WORDRIGHTENDEXTEND, 0, 0);
}

void CScintilla::SetWhitespaceChars(const char* characters)
{
	SPerform(SCI_SETWHITESPACECHARS, 0, (long)characters);
}

void CScintilla::SetCharsDefault()
{
	SPerform(SCI_SETCHARSDEFAULT, 0, 0);
}

int CScintilla::AutoCGetCurrent()
{
	return (int)SPerform(SCI_AUTOCGETCURRENT, 0, 0);
}

void CScintilla::Allocate(int bytes)
{
	SPerform(SCI_ALLOCATE, (long)bytes, 0);
}

void CScintilla::StartRecord()
{
	SPerform(SCI_STARTRECORD, 0, 0);
}

void CScintilla::StopRecord()
{
	SPerform(SCI_STOPRECORD, 0, 0);
}

void CScintilla::SetLexer(int lexer)
{
	SPerform(SCI_SETLEXER, (long)lexer, 0);
}

int CScintilla::GetLexer()
{
	return (int)SPerform(SCI_GETLEXER, 0, 0);
}

void CScintilla::Colourise(long start, long end)
{
	SPerform(SCI_COLOURISE, start, end);
}

void CScintilla::SetProperty(const char* key, const char* value)
{
	SPerform(SCI_SETPROPERTY, (long)key, (long)value);
}

void CScintilla::SetKeyWords(int keywordSet, const char* keyWords)
{
	SPerform(SCI_SETKEYWORDS, (long)keywordSet, (long)keyWords);
}

void CScintilla::SetLexerLanguage(const char* language)
{
	SPerform(SCI_SETLEXERLANGUAGE, 0, (long)language);
}

void CScintilla::LoadLexerLibrary(const char* path)
{
	SPerform(SCI_LOADLEXERLIBRARY, 0, (long)path);
}

void CScintilla::SetPasteConvertEndings(bool convert)
{
	SPerform(SCI_SETPASTECONVERTENDINGS, (long)convert, 0);
}

bool CScintilla::GetPasteConvertEndings()
{
	return SPerform(SCI_GETPASTECONVERTENDINGS, 0, 0) != 0;
}

int CScintilla::TargetAsUTF8(char *s)
{
	return SPerform(SCI_TARGETASUTF8, 0, reinterpret_cast<LPARAM>(s));
}

void CScintilla::ToggleCaretSticky()
{
	SPerform(SCI_TOGGLECARETSTICKY, 0, 0);
}

bool CScintilla::GetCaretSticky()
{
	return SPerform(SCI_GETCARETSTICKY) != 0;
}

void CScintilla::SetCaretSticky(bool sticky)
{
	SPerform(SCI_SETCARETSTICKY, sticky ? 1 : 0, 0);
}

int CScintilla::FindColumn(int line, int column)
{
	return SPerform(SCI_FINDCOLUMN, line, column);
}

int CScintilla::GetPropertyInt(const char* key, int defaultVal)
{
	return SPerform(SCI_GETPROPERTYINT, reinterpret_cast<WPARAM>(key), defaultVal);
}

void CScintilla::SetIndicatorCurrent(int indicator)
{
	SPerform(SCI_SETINDICATORCURRENT, indicator, 0);
}

void CScintilla::SetIndicatorValue(int value)
{
	SPerform(SCI_SETINDICATORVALUE, value, 0);
}

void CScintilla::IndicatorFillRange(int start, int length)
{
	SPerform(SCI_INDICATORFILLRANGE, start, length);
}

void CScintilla::IndicatorClearRange(int start, int length)
{
	SPerform(SCI_INDICATORCLEARRANGE, start, length);
}

//--

int CScintilla::GetSelections()
{
	return SPerform(SCI_GETSELECTIONS, 0, 0);
}

void CScintilla::ClearSelections()
{
	SPerform(SCI_CLEARSELECTIONS, 0, 0);
}


void CScintilla::SetSelection(int caret, int anchor)
{
	SPerform(SCI_SETSELECTION, caret, anchor);
}


void CScintilla::AddSelection(int caret, int anchor)
{
	SPerform(SCI_ADDSELECTION, caret, anchor);
}


void CScintilla::SetMainSelection(int selection)
{
	SPerform(SCI_SETMAINSELECTION, selection, 0);
}


int CScintilla::GetMainSelection()
{
	return SPerform(SCI_GETMAINSELECTION, 0, 0);
}



void CScintilla::SetSelectionNCaret(int selection, int pos)
{
	SPerform(SCI_SETSELECTIONNCARET, selection, pos);
}


int CScintilla::GetSelectionNCaret(int selection)
{
	return SPerform(SCI_GETSELECTIONNCARET, selection, 0);
}


void CScintilla::SetSelectionNCaretVirtualSpace(int selection, int space)
{
	SPerform(SCI_SETSELECTIONNCARETVIRTUALSPACE, selection, space);
}

int CScintilla::GetSelectionNCaretVirtualSpace(int selection)
{
	return SPerform(SCI_GETSELECTIONNCARETVIRTUALSPACE, selection, 0);
}

void CScintilla::SetSelectionNAnchor(int selection, int posAnchor)
{
	SPerform(SCI_SETSELECTIONNANCHOR, selection, posAnchor);
}

int CScintilla::GetSelectionNAnchor(int selection)
{
	return SPerform(SCI_GETSELECTIONNANCHOR, selection, 0);
}

void CScintilla::SetSelectionNAnchorVirtualSpace(int selection, int space)
{
	SPerform(SCI_SETSELECTIONNANCHORVIRTUALSPACE, selection, space);
}

int CScintilla::GetSelectionNAnchorVirtualSpace(int selection)
{
	return SPerform(SCI_GETSELECTIONNANCHORVIRTUALSPACE, selection, 0);
}

void CScintilla::SetSelectionNStart(int selection, int pos)
{
	SPerform(SCI_SETSELECTIONNSTART, selection, pos);
}

int CScintilla::GetSelectionNStart(int selection)
{
	return SPerform(SCI_GETSELECTIONNSTART, selection, 0);
}

void CScintilla::SetSelectionNEnd(int selection, int pos)
{
	SPerform(SCI_SETSELECTIONNEND, selection, pos);
}

int CScintilla::GetSelectionNEnd(int selection)
{
	return SPerform(SCI_GETSELECTIONNEND, selection, 0);
}

void CScintilla::SetRectangularSelectionCaret(int pos)
{
	SPerform(SCI_SETRECTANGULARSELECTIONCARET, pos, 0);
}

int CScintilla::GetRectangularSelectionCaret()
{
	return SPerform(SCI_GETRECTANGULARSELECTIONCARET, 0, 0);
}

void CScintilla::SetRectangularSelectionCaretVirtualSpace(int space)
{
	SPerform(SCI_SETRECTANGULARSELECTIONCARETVIRTUALSPACE, space, 0);
}

int CScintilla::GetRectangularSelectionCaretVirtualSpace()
{
	return SPerform(SCI_GETRECTANGULARSELECTIONCARETVIRTUALSPACE, 0, 0);
}

void CScintilla::SetRectangularSelectionAnchor(int posAnchor)
{
	SPerform(SCI_SETRECTANGULARSELECTIONANCHOR, posAnchor, 0);
}

int CScintilla::GetRectangularSelectionAnchor()
{
	return SPerform(SCI_GETRECTANGULARSELECTIONANCHOR, 0, 0);
}

void CScintilla::SetRectangularSelectionAnchorVirtualSpace(int space)
{
	SPerform(SCI_SETRECTANGULARSELECTIONANCHORVIRTUALSPACE, space, 0);
}

int CScintilla::GetRectangularSelectionAnchorVirtualSpace()
{
	return SPerform(SCI_GETRECTANGULARSELECTIONANCHORVIRTUALSPACE, 0, 0);
}

void CScintilla::SetAdditionalSelAlpha(int alpha)
{
	SPerform(SCI_SETADDITIONALSELALPHA, alpha, 0);
}

int CScintilla::GetAdditionalSelAlpha()
{
	return SPerform(SCI_GETADDITIONALSELALPHA, 0, 0);
}

void CScintilla::SetAdditionalSelFore(int colour)
{
	SPerform(SCI_SETADDITIONALSELFORE, colour, 0);
}

void CScintilla::SetAdditionalSelBack(int colour)
{
	SPerform(SCI_SETADDITIONALSELBACK, colour, 0);
}

void CScintilla::SetAdditionalCaretFore(int colour)
{
	SPerform(SCI_SETADDITIONALCARETFORE, colour, 0);
}

int CScintilla::GetAdditionalCaretFore()
{
	return SPerform(SCI_GETADDITIONALCARETFORE, 0, 0);
}

void CScintilla::SetAdditionalCaretsBlink(bool additionalCaretsBlink)
{
	SPerform(SCI_SETADDITIONALCARETSBLINK, additionalCaretsBlink ? 1 : 0, 0);
}

bool CScintilla::GetAdditionalCaretsBlink()
{
	return SPerform(SCI_GETADDITIONALCARETSBLINK, 0, 0) != 0;
}

void CScintilla::SwapMainAnchorCaret()
{
	SPerform(SCI_SWAPMAINANCHORCARET, 0, 0);
}

void CScintilla::RotateSelection()
{
	SPerform(SCI_ROTATESELECTION, 0, 0);
}