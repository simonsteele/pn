/**
 * @file Schemes.cpp
 * @brief Implement Scheme and SchemeManager.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"

#include "files.h"

#include "Schemes.h"
#include "optionsmanager.h"

#include "resource.h"

///////////////////////////////////////////////////////////
// Scheme
///////////////////////////////////////////////////////////

Scheme::Scheme()
{
	Init();
}

Scheme::Scheme(SchemeManager* pManager)
{
	Init();

	m_pManager = pManager;
}

Scheme::Scheme(SchemeManager* pManager, LPCTSTR filename)
{
	Init();

	m_SchemeFile = new TCHAR[_tcslen(filename)+1];
	_tcscpy(m_SchemeFile, filename);
	m_pManager = pManager;
}

Scheme::Scheme(const Scheme& copy)
{
	Init();
	*this = copy;
}

void Scheme::Init()
{
	m_SchemeFile = NULL;
	m_Name = NULL;
	m_Title = NULL;
	m_pManager = NULL;
	m_bInternal = false;
}

Scheme::~Scheme()
{
	if(m_SchemeFile)
	{
		delete [] m_SchemeFile;
		m_SchemeFile = NULL;
	}

	if(m_Name)
	{
		delete [] m_Name;
		m_Name = NULL;
	}

	if(m_Title)
	{
		delete [] m_Title;
	}
}

bool Scheme::InitialLoad(CFile& file, SchemeHdrRec& hdr)
{
	CompiledHdrRec Header;
	file.Read(&Header, sizeof(CompiledHdrRec));
	if( strcmp(Header.Magic, FileID) != 0 || Header.Version != CompileVersion )
	{
		return false;
	}

	file.Read(&hdr, sizeof(SchemeHdrRec));
	
	#ifdef UNICODE
	// Convert into a unicode string...	
	SetName(&hdr.Name[0]);
	CA2CT title(&hdr.Title[0]);
	SetTitle(title);
	#else
	// Otherwise just copy the string.	
	SetName(&hdr.Name[0]);
	SetTitle(&hdr.Title[0]);
	#endif

	m_bInternal = (hdr.Flags & schInternal) != 0;

	return true;
}

bool Scheme::CheckName()
{
	CFile cfile;

	bool bRet = false;
	
	if( cfile.Open(m_SchemeFile) )
	{
		SchemeHdrRec hdr;
		bRet = InitialLoad(cfile, hdr);
		cfile.Close();
	}

	return bRet;
}

const char* Scheme::GetName() const
{
	return m_Name;
}

LPCTSTR Scheme::GetTitle() const
{
	return m_Title;
}

const wchar_t* Scheme::GetFileName() const
{
	return m_SchemeFile;
}

const char* Scheme::GetLexer() const
{
	return m_Lexer.c_str();
}

const CommentSpecRec& Scheme::GetCommentSpec() const
{
	return m_CommentSpec;
}

void Scheme::SetName(LPCSTR name)
{
	if(name)
	{
		if(m_Name)
			delete [] m_Name;
		m_Name = new char[strlen(name)+1];
		strcpy(m_Name, name);
	}
}

void Scheme::SetTitle(LPCTSTR title)
{
	if(title)
	{
		if(m_Title)
			delete [] m_Title;

		m_Title = new TCHAR[_tcslen(title)+1];
		_tcscpy(m_Title, title);
	}
}

void Scheme::SetFileName(LPCTSTR filename)
{
	if(filename)
	{
		if(m_SchemeFile != NULL)
			delete [] m_SchemeFile;

		m_SchemeFile = new TCHAR[_tcslen(filename)+1];
		_tcscpy(m_SchemeFile, filename);
	}
}

bool Scheme::IsInternal() const
{
	return m_bInternal;
}

void Scheme::SetSchemeManager(SchemeManager* pManager)
{
	m_pManager = pManager;
}

/**
 * This function allocates a list of StyleDetails objects containing
 * the settings used by this scheme. The caller must free the list
 * and the items contained within.
 */
StylesList* Scheme::CreateStylesList()
{
	CFile			cfile;
	SchemeHdrRec	hdr;
	TextRec			Txt;
	MsgRec			Msg;
	char			Next2;

	if( cfile.Open(m_SchemeFile) )
	{
		if (!InitialLoad(cfile, hdr))
			return NULL;

		StylesList* pList = new StylesList;
		StyleDetails* pDefault = new StyleDetails;
		StyleDetails* pS = NULL;
		pDefault->Key = STYLE_DEFAULT;
		pList->AddStyle(pDefault);
		int curStyle = -1;

		std::vector<char> buf;

		// Find the default style...
		while (cfile.GetPosition() < cfile.GetLength())
		{
			cfile.Read(&Next2, sizeof(char));
			switch(Next2)
			{
				case nrMsgRec:
				{
					cfile.Read(&Msg, sizeof(MsgRec));
					if(Msg.MsgNum == SCI_STYLESETBOLD || Msg.MsgNum == SCI_STYLESETITALIC ||
						Msg.MsgNum == SCI_STYLESETUNDERLINE || Msg.MsgNum == SCI_STYLESETSIZE ||
						Msg.MsgNum == SCI_STYLESETFORE || Msg.MsgNum == SCI_STYLESETBACK ||
						Msg.MsgNum == SCI_STYLESETEOLFILLED)
					{
						if(Msg.wParam != curStyle)
						{
							if(Msg.wParam == STYLE_DEFAULT)
							{
								pS = pDefault;
							}
							else
							{
								pS = new StyleDetails(*pDefault);
								pS->Key = Msg.wParam;
								pList->AddStyle(pS);
							}
							curStyle = Msg.wParam;
						}
						
						switch(Msg.MsgNum)
						{
						case SCI_STYLESETBOLD:
							pS->Bold = (Msg.lParam != 0);
							break;
						case SCI_STYLESETITALIC:
							pS->Italic = (Msg.lParam != 0);
							break;
						case SCI_STYLESETUNDERLINE:
							pS->Underline = (Msg.lParam != 0);
							break;
						case SCI_STYLESETSIZE:
							pS->FontSize = Msg.lParam;
							break;
						case SCI_STYLESETFORE:
							pS->ForeColor = Msg.lParam;
							break;
						case SCI_STYLESETBACK:
							pS->BackColor = Msg.lParam;
							break;
						case SCI_STYLESETEOLFILLED:
							pS->EOLFilled = (Msg.lParam != 0);
							break;
						};
					}
				}
				break;

				case nrTextRec:
				{
					cfile.Read(&Txt, sizeof(TextRec));
					if(Txt.TextType == ttFontName)
					{
						if(Txt.wParam != curStyle)
						{
							if(Txt.wParam == STYLE_DEFAULT)
							{
								pS = pDefault;
							}
							else
							{
								pS = new StyleDetails(*pDefault);
								pS->Key = Txt.wParam;
								pList->AddStyle(pS);
							}
							curStyle = Txt.wParam;
						}

						buf.resize(Txt.TextLength + 1);
						cfile.Read(&buf[0], Txt.TextLength * sizeof(char));
						buf[Txt.TextLength] = '\0';
						switch(Txt.TextType)
						{
							case ttFontName : 
								pS->FontName = CA2CT(&buf[0]);
								break;
						}
					}
					else 
						cfile.Seek(Txt.TextLength * sizeof(char), CFile::current);
				}
				break;

				case nrCommentRec:
				{
					// skip here...
					cfile.Seek(sizeof(CommentSpecRec), CFile::current);
				}
				break;
			}
		}

		cfile.Close();

		return pList;
	}

	return NULL;
}

void Scheme::Load(CScintilla& sc, bool allSettings, LPCTSTR filename)
{
	CFile cfile;
	SchemeHdrRec hdr;
	MsgRec Msg;
	TextRec Txt;
	PropRec Prp;
	char Next2;

	memset(&m_CommentSpec, 0, sizeof(CommentSpecRec));

	if( filename )
	{
		SetFileName(filename);
	}

	if( cfile.Open(m_SchemeFile) )
	{
		// Check the file is OK and read the header.
		if(!InitialLoad(cfile, hdr))
		{
			UNEXPECTED(_T("Tried to load invalid binary scheme file at run-time"));
			cfile.Close();
			return;
		}

		// Set the defaults - these may be changed by the load.
		SetupScintilla(sc, allSettings);

		std::vector<char> buf;

		while (cfile.GetPosition() < cfile.GetLength())
		{
			cfile.Read(&Next2, sizeof(char));
			switch(Next2)
			{
				case nrMsgRec:
					cfile.Read(&Msg, sizeof(MsgRec));
					sc.SPerform(Msg.MsgNum, Msg.wParam, Msg.lParam);
					break;
				case nrTextRec:
					{
						cfile.Read(&Txt, sizeof(TextRec));
						buf.resize(Txt.TextLength + 1);
						cfile.Read(&buf[0], Txt.TextLength*sizeof(char));
						buf[Txt.TextLength] = '\0';
						switch(Txt.TextType)
						{
							case ttFontName : 
								sc.SPerform(SCI_STYLESETFONT, Txt.wParam, (long)&buf[0]);
								break;
							case ttKeywords : 
								sc.SetKeyWords(Txt.wParam, &buf[0]);
								break;
							case ttLexerLanguage : 
								{
									sc.SPerform(SCI_SETLEXERLANGUAGE, 0, (long)&buf[0]);
									m_Lexer = &buf[0];
								}
								break;
							case ttWordChars :
								{
									sc.SPerform(SCI_SETWORDCHARS, 0, (long)&buf[0]);
								}
								break;
						}
					}
					break;
				case nrPropRec:
				{
					cfile.Read(&Prp, sizeof(PropRec));
					buf.resize(Prp.NameLength + 1);
					cfile.Read(&buf[0], Prp.NameLength * sizeof(char));
					buf[Prp.NameLength] = '\0';

					std::vector<char> buf2(Prp.ValueLength + 1);
					cfile.Read(&buf2[0], Prp.ValueLength * sizeof(char));
					buf2[Prp.ValueLength] = '\0';
					sc.SPerform(SCI_SETPROPERTY, (long)&buf[0], (long)&buf2[0]);
				}
				break;
				case nrCommentRec:
				{
					// skip here...
					cfile.Read(&m_CommentSpec, sizeof(CommentSpecRec));
				}
				break;
			}
		}

		cfile.Close();

		if((hdr.Flags & fldEnabled) && OPTIONS->GetCached(Options::OFoldingEnabled))
		{
			///@todo obviously these details need to come from settings somewhere...
			sc.SPerform(SCI_SETPROPERTY, (WPARAM)"fold", (LPARAM)"1");
			sc.SPerform(SCI_SETMARGINTYPEN, 2, SC_MARGIN_SYMBOL);
			sc.SPerform(SCI_SETMARGINMASKN, 2, SC_MASK_FOLDERS);
			sc.SPerform(SCI_SETMARGINSENSITIVEN, 2, true);
			sc.SPerform(SCI_SETMARGINWIDTHN, 2, 14);
			sc.SPerform(SCI_SETFOLDFLAGS, 16, 0);
			sc.SetFoldingMargins(efsVSNet);

			sc.SPerform(SCI_SETPROPERTY, (WPARAM)"fold.compact", (LPARAM)((hdr.Flags & fldCompact) ? "1" : "0"));
			
			if(hdr.Flags & fldComments)
				sc.SPerform(SCI_SETPROPERTY, (WPARAM)"fold.comment", (LPARAM)"1");

			if(hdr.Flags & fldPreProc)
				sc.SPerform(SCI_SETPROPERTY, (WPARAM)"fold.preprocessor", (LPARAM)"1");

			if(hdr.Flags & fldElse) 
				sc.SPerform(SCI_SETPROPERTY, (WPARAM)"fold.at.else", (LPARAM)"1");
		}

		if(hdr.Flags & schOverrideTabs)
		{
			if(hdr.Flags & schUseTabs)
				sc.SPerform(SCI_SETUSETABS, 1, 0);
			else
				sc.SPerform(SCI_SETUSETABS, 0, 0);
		}

		if(hdr.Flags & schOverrideTabSize)
		{
			sc.SPerform(SCI_SETTABWIDTH, hdr.TabWidth, 0);
		}

	}
	else
	{
		// Show an error or something...
	}
}

void Scheme::SetupScintilla(CScintilla& sc, bool allSettings)
{
	Options& options = *OPTIONS;

	// Line Indentation...
	sc.SPerform(SCI_SETUSETABS, options.GetCached(Options::OUseTabs) ? 1 : 0);
	sc.SPerform(SCI_SETTABWIDTH, options.GetCached(Options::OTabWidth));
	
	if( options.GetCached(Options::OLineHighlight) )
	{
		sc.SPerform(SCI_SETCARETLINEVISIBLE, true);
		sc.SPerform(SCI_SETCARETLINEBACK, options.GetCached(Options::OLineHighlightColour));
		sc.SPerform(SCI_SETCARETLINEBACKALPHA, options.GetCached(Options::OLineHighlightAlpha));
	}

	sc.SPerform(SCI_SETEDGEMODE, options.GetCached(Options::ORightGuide));
	sc.SPerform(SCI_SETEDGECOLUMN, options.GetCached(Options::ORightColumn));
	sc.SPerform(SCI_SETEDGECOLOUR, options.GetCached(Options::ORightGuideColour));

	if(allSettings)
	{
		sc.SPerform(SCI_SETINDENTATIONGUIDES, (options.GetCached(Options::OShowIndentGuides) ? 1 : 0));
		sc.SPerform(SCI_SETWRAPMODE, options.GetCached(Options::OWordWrap) ? SC_WRAP_WORD : SC_WRAP_NONE);
		sc.SetViewWS((options.GetCached(Options::OVisibleWhiteSpace) ? SCWS_VISIBLEALWAYS : SCWS_INVISIBLE));
		sc.SetViewEOL(options.GetCached(Options::OVisibleLineEndings) != FALSE);
		sc.SPerform(SCI_SETPASTECONVERTENDINGS, options.GetCached(Options::OConvertLinesOnPaste));
	}

	// When using line wrapping, home and end should jump to start and end of wrapped line, not whole wrapped block:
	sc.SPerform(SCI_ASSIGNCMDKEY, SCK_HOME, SCI_VCHOMEWRAP);
	sc.SPerform(SCI_ASSIGNCMDKEY, SCK_END, SCI_LINEENDWRAP);
	sc.SPerform(SCI_ASSIGNCMDKEY, (SCK_HOME + (SCMOD_SHIFT << 16)), SCI_VCHOMEWRAPEXTEND);
	sc.SPerform(SCI_ASSIGNCMDKEY, (SCK_END + (SCMOD_SHIFT << 16)), SCI_LINEENDWRAPEXTEND);

	// Set even treatment of left and right caret positioning, and sloppy behaviour by default. 
	// Use 3 lines as the jump when scrolling up and down, and 20 pixels when scrolling left and right by default.
	sc.SPerform(SCI_SETXCARETPOLICY, options.GetCached(Options::OCaretXFlags), options.GetCached(Options::OCaretXMove));
	sc.SPerform(SCI_SETYCARETPOLICY, options.GetCached(Options::OCaretYFlags), options.GetCached(Options::OCaretYMove));
	sc.SPerform(SCI_SETVISIBLEPOLICY, VISIBLE_SLOP, 1);

	// Default style:
	sc.SPerform(SCI_STYLERESETDEFAULT);
	sc.SPerform(SCI_STYLESETFORE, STYLE_DEFAULT, ::GetSysColor(COLOR_WINDOWTEXT));
	sc.SPerform(SCI_STYLESETBACK, STYLE_DEFAULT, ::GetSysColor(COLOR_WINDOW));
	sc.SPerform(SCI_STYLECLEARALL);

	// Line length measurement:
	sc.SPerform(SCI_SETSCROLLWIDTHTRACKING, 1);
	
	// Line height padding:
	sc.SPerform(SCI_SETEXTRAASCENT, options.GetCached(Options::OLinePaddingTop));
	sc.SPerform(SCI_SETEXTRADESCENT, options.GetCached(Options::OLinePaddingBottom));

	sc.DefineBookmarks();
	sc.DefineNumberedBookmarks();

	sc.SPerform(SCI_SETMARGINWIDTHN, 1, 16/*margin ? marginWidth : 0*/);

	// Indicators:
	sc.SPerform(SCI_INDICSETFORE, INDIC_MARKALL, DEFAULT_MARKALL_COLOUR);
	sc.SPerform(SCI_INDICSETFORE, INDIC_SMARTHIGHLIGHT, DEFAULT_SMARTHIGHLIGHT_COLOUR);
	sc.SPerform(SCI_INDICSETFORE, INDIC_OVERWRITETARGET, DEFAULT_OVERWRITE_COLOUR);
	sc.SPerform(SCI_INDICSETFORE, INDIC_TEXTCLIPFIELD, DEFAULT_TEXTCLIPFIELD_COLOUR);
	sc.SPerform(SCI_INDICSETALPHA, INDIC_SMARTHIGHLIGHT, DEFAULT_INDIC_ALPHA_LEVEL);
	sc.SPerform(SCI_INDICSETALPHA, INDIC_TEXTCLIPFIELD, DEFAULT_INDIC_ALPHA_LEVEL);
	sc.IndicSetStyle(INDIC_TEXTCLIPFIELD, INDIC_ROUNDBOX);
	
	options.BeginGroupOperation(PNSK_EDITOR);

	int vsflags = options.Get(NULL, _T("VirtualSpaceRectSel"), true) ? SCVS_RECTANGULARSELECTION : 0;
	vsflags |= options.Get(NULL, _T("VirtualSpace"), false) ? SCVS_USERACCESSIBLE : 0;
	sc.SPerform(SCI_SETVIRTUALSPACEOPTIONS, vsflags, 0); 

	sc.SPerform(SCI_SETMULTIPLESELECTION, options.Get(NULL, _T("MultipleSelections"), true) ? 1 : 0, 0);
	sc.SPerform(SCI_SETADDITIONALSELECTIONTYPING, options.Get(NULL, _T("TypeIntoMultipleSelections"), true) ? 1: 0, 0);

	// Default windows edit control behaviour... This needs to be optional.
	///@todo allow default scintilla coloured selection...
	if (options.Get(NULL, _T("DefaultSelectionColours"), true))
	{
		sc.SPerform(SCI_SETSELFORE, 1, ::GetSysColor(COLOR_HIGHLIGHTTEXT));
		sc.SPerform(SCI_SETSELBACK, 1, ::GetSysColor(COLOR_HIGHLIGHT));
	}
	else
	{
		COLORREF c;

		if (options.Get(NULL, _T("SetSelectionFore"), false))
		{
			c = (COLORREF)options.Get(NULL, _T("SelectionFore"), (int)::GetSysColor(COLOR_HIGHLIGHTTEXT));
			sc.SPerform(SCI_SETSELFORE, 1, c);
		}
		else
		{
			sc.SPerform(SCI_SETSELFORE, 0, 0);
		}
		
		c = (COLORREF)options.Get(NULL, _T("SelectionBack"), (int)::GetSysColor(COLOR_HIGHLIGHT));
		sc.SPerform(SCI_SETSELBACK, 1, c);
	}

	if (options.Get(NULL, _T("DisplayCaretAsBlock"), false))
	{
		sc.SPerform(SCI_SETCARETSTYLE, CARETSTYLE_BLOCK, 0);
	}
	else
	{
		sc.SPerform(SCI_SETCARETSTYLE, CARETSTYLE_LINE, 0);
		int cwidth = options.Get(NULL, _T("CaretWidth"), 0);
		if (cwidth != 0)
		{
			sc.SPerform(SCI_SETCARETWIDTH, cwidth, 0);
		}
	}

	options.EndGroupOperation();
}

bool Scheme::operator < (const Scheme& compare) const 
{
	return _tcsicmp(GetTitle(), compare.GetTitle()) < 0;
}

bool Scheme::operator > (const Scheme& compare) const
{
	return _tcsicmp(GetTitle(), compare.GetTitle()) > 0;
}

const Scheme& Scheme::operator = (const Scheme& copy)
{
	m_pManager = copy.m_pManager;

	SetName(copy.GetName());
	SetTitle(copy.GetTitle());
	SetFileName(copy.GetFileName());
	m_bInternal = copy.m_bInternal;

	return *this;
}

///////////////////////////////////////////////////////////
// DefaultScheme
///////////////////////////////////////////////////////////

DefaultScheme::DefaultScheme()
{
	tstring name = L10N::StringLoader::Get(IDS_DEFAULTSCHEME);
	m_Title = new TCHAR[name.length() + 1];
	_tcscpy(m_Title, name.c_str());
}

void DefaultScheme::Load(CScintilla& sc, LPCTSTR filename)
{
	Scheme::Load(sc);
}

///////////////////////////////////////////////////////////
// CSchemeSwitcher
///////////////////////////////////////////////////////////

CSchemeSwitcher::CSchemeSwitcher()
{
	
}

CSchemeSwitcher::~CSchemeSwitcher()
{

}

void CSchemeSwitcher::BuildMenu(int iCommand, CommandDispatch* dispatch)
{
	menuid_scheme_pair x;
	SchemeManager& sm = SchemeManager::GetInstanceRef();
	SCHEME_LIST* pSchemes = sm.GetSchemesList();
	
	m_menu.AddItem( sm.GetDefaultScheme()->GetTitle(), dispatch->RegisterCallback(NULL, iCommand, (LPVOID)sm.GetDefaultScheme()) );

	for(SCIT i = pSchemes->begin(); i != pSchemes->end(); ++i)
	{
		if( !(*i).IsInternal() )
		{
			x.pScheme = &(*i);
			x.iCommand = dispatch->RegisterCallback(NULL, iCommand, (LPVOID)x.pScheme);

			m_menu.AddItem( x.pScheme->GetTitle(), x.iCommand );
			m_list.insert(m_list.end(), x);
		}
	}
}

void CSchemeSwitcher::Reset(CommandDispatch* pDispatch, int iCommand)
{
	m_list.clear();
	BuildMenu(iCommand, pDispatch);
}

void CSchemeSwitcher::SetActiveScheme(Scheme* pCurrent)
{
	for(MISCHEMELIST::iterator i = m_list.begin(); i != m_list.end(); ++i)
	{
		if((*i).pScheme == pCurrent)
			::CheckMenuItem((HMENU)m_menu, (*i).iCommand, MF_BYCOMMAND | MF_CHECKED);
		else
			::CheckMenuItem((HMENU)m_menu, (*i).iCommand, MF_BYCOMMAND | MF_UNCHECKED);
	}
}

CSchemeSwitcher::operator HMENU ()
{
	return (HMENU)m_menu;
}