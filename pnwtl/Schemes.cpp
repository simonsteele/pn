/**
 * @file Schemes.cpp
 * @brief Implement Scheme and SchemeManager.
 * @author Simon Steele
 * @note Copyright (c) 2002-2006 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"

#include "files.h"

#include "Schemes.h"
#include "SchemeCompiler.h"
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
		file.Close();
		m_pManager->Compile();

		if( file.Open(m_SchemeFile) )
		{
			file.Read(&Header, sizeof(CompiledHdrRec));
			if(strcmp(Header.Magic, FileID) != 0 || Header.Version != CompileVersion)
			{
				// Not the right version, and compiling didn't help:
				file.Close();
				::OutputDebugString(_T("PN2: Compiled Scheme Header invalid or corrupt after compile."));
				return false;
			}
		}
		else
			return false;
	}

	file.Read(&hdr, sizeof(SchemeHdrRec));
	
	#ifdef UNICODE
	// Convert into a unicode string...	
	USES_CONVERSION;
	SetName(A2T(&hdr.Name[0]));
	SetTitle(A2T(&hdr.Title[0]));
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

LPCTSTR Scheme::GetName() const
{
	return m_Name;
}

LPCTSTR Scheme::GetTitle() const
{
	return m_Title;
}

LPCTSTR Scheme::GetFileName() const
{
	return m_SchemeFile;
}

void Scheme::SetName(LPCTSTR name)
{
	if(name)
	{
		if(m_Name)
			delete [] m_Name;
		m_Name = new TCHAR[_tcslen(name)+1];
		_tcscpy(m_Name, name);
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
	char*			buf;

	if( cfile.Open(m_SchemeFile) )
	{
		InitialLoad(cfile, hdr);

		//long posFirstStyle = cfile.GetPosition();

		StylesList* pList = new StylesList;
		StyleDetails* pDefault = new StyleDetails;
		StyleDetails* pS = NULL;
		pDefault->Key = STYLE_DEFAULT;
		pList->AddStyle(pDefault);
		int curStyle = -1;

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

						buf = new char[Txt.TextLength + 1];
						cfile.Read(buf, Txt.TextLength * sizeof(char));
						buf[Txt.TextLength] = '\0';
						switch(Txt.TextType)
						{
							case ttFontName : 
								pS->FontName = buf;
								break;
						}
						delete [] buf;
						buf = NULL;
					}
					else 
						cfile.Seek(Txt.TextLength * sizeof(char), CFile::current);
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
	char *buf = NULL;
	char Next2;

	if( filename )
	{
		SetFileName(filename);
	}

	if( cfile.Open(m_SchemeFile) )
	{
		// Check the file is OK and read the header.
		InitialLoad(cfile, hdr);

		// Set the defaults - these may be changed by the load.
		SetupScintilla(sc, allSettings);

		if(hdr.Flags & fldEnabled)
		{
			///@todo obviously these details need to come from settings somewhere...
			sc.SPerform(SCI_SETPROPERTY, (WPARAM)_T("fold"), (LPARAM)_T("1"));
			sc.SPerform(SCI_SETMARGINTYPEN, 2, SC_MARGIN_SYMBOL);
			sc.SPerform(SCI_SETMARGINMASKN, 2, SC_MASK_FOLDERS);
			sc.SPerform(SCI_SETMARGINSENSITIVEN, 2, true);
			sc.SPerform(SCI_SETMARGINWIDTHN, 2, 14);
			sc.SPerform(SCI_SETFOLDFLAGS, 16, 0);
			sc.SetFoldingMargins(efsVSNet);

			sc.SPerform(SCI_SETPROPERTY, (WPARAM)_T("fold.compact"), (LPARAM)((hdr.Flags & fldCompact) ? _T("1") : _T("0")));
			
			if(hdr.Flags & fldComments)
				sc.SPerform(SCI_SETPROPERTY, (WPARAM)_T("fold.comment"), (LPARAM)_T("1"));

			if(hdr.Flags & fldPreProc)
				sc.SPerform(SCI_SETPROPERTY, (WPARAM)_T("fold.preprocessor"), (LPARAM)_T("1"));
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
						buf = new char[Txt.TextLength + 1];
						cfile.Read(buf, Txt.TextLength*sizeof(char));
						buf[Txt.TextLength] = '\0';
						switch(Txt.TextType)
						{
							case ttFontName : sc.SPerform(SCI_STYLESETFONT, Txt.wParam, (long)buf);
								break;
							case ttKeywords : sc.SPerform(SCI_SETKEYWORDS, Txt.wParam, (long)buf);
								break;
							case ttLexerLanguage : sc.SPerform(SCI_SETLEXERLANGUAGE, 0, (long)buf);
								break;
						}
						delete [] buf;
						buf = NULL;
					}
					break;
				case nrPropRec:
					{
						cfile.Read(&Prp, sizeof(PropRec));
						buf = new char[Prp.NameLength + 1];
						cfile.Read(buf, Prp.NameLength*sizeof(char));
						buf[Prp.NameLength] = '\0';
						char* buf2 = new char[Prp.ValueLength + 1];
						cfile.Read(buf2, Prp.ValueLength*sizeof(char));
						buf2[Prp.ValueLength] = '\0';
						sc.SPerform(SCI_SETPROPERTY, (long)buf, (long)buf2);
						delete [] buf;
						delete [] buf2;
						buf = NULL;
					}
					break;
			}
		}

		cfile.Close();
	}
	else
	{
		// Show an error or something...
	}
}

void Scheme::SetupScintilla(CScintilla& sc, bool allSettings)
{
	Options& options = *OPTIONS;

	//ss 16/02/2003 Now performed by document...
	//sc.SPerform(SCI_SETEOLMODE, options.LineEndings);

	// Line Indentation...
	sc.SPerform(SCI_SETUSETABS, options.GetCached(Options::OUseTabs) ? 1 : 0);
	sc.SPerform(SCI_SETTABWIDTH, options.GetCached(Options::OTabWidth));
	if( options.GetCached(Options::OLineHighlight) )
	{
		sc.SPerform(SCI_SETCARETLINEVISIBLE, true);
		sc.SPerform(SCI_SETCARETLINEBACK, options.GetCached(Options::OLineHighlightColour));
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

	// Set even treatment of left and right caret positioning, and sloppy behaviour. 
	// Use 3 lines as the jump when scrolling up and down.
	sc.SPerform(SCI_SETXCARETPOLICY, CARET_SLOP | CARET_EVEN, 3);
	sc.SPerform(SCI_SETYCARETPOLICY, CARET_SLOP | CARET_EVEN, 3);

	sc.SPerform(SCI_STYLERESETDEFAULT);
	sc.SPerform(SCI_STYLESETFORE, STYLE_DEFAULT, ::GetSysColor(COLOR_WINDOWTEXT));
	sc.SPerform(SCI_STYLESETBACK, STYLE_DEFAULT, ::GetSysColor(COLOR_WINDOW));
	sc.SPerform(SCI_STYLESETCHARACTERSET, STYLE_DEFAULT, options.GetCached(Options::ODefaultCharSet));
	sc.SPerform(SCI_STYLECLEARALL);

	sc.DefineBookmarks();
	sc.DefineNumberedBookmarks();

	sc.SPerform(SCI_SETMARGINWIDTHN, 1, 16/*margin ? marginWidth : 0*/);
	
	options.BeginGroupOperation(PNSK_EDITOR);

	// Default windows edit control behaviour... This needs to be optional.
	///@todo allow default scintilla coloured selection...
	if(options.Get(NULL, _T("DefaultSelectionColours"), true))
	{
		sc.SPerform(SCI_SETSELFORE, 1, ::GetSysColor(COLOR_HIGHLIGHTTEXT));
		sc.SPerform(SCI_SETSELBACK, 1, ::GetSysColor(COLOR_HIGHLIGHT));
	}
	else
	{
		COLORREF c;

		if(options.Get(NULL, _T("SetSelectionFore"), false))
		{
			c = (COLORREF)options.Get(NULL, _T("SelectionFore"), (int)::GetSysColor(COLOR_HIGHLIGHTTEXT));
			sc.SPerform(SCI_SETSELFORE, 1, c);
		}
		else
			sc.SPerform(SCI_SETSELFORE, 0, 0);
		
		c = (COLORREF)options.Get(NULL, _T("SelectionBack"), (int)::GetSysColor(COLOR_HIGHLIGHT));
		sc.SPerform(SCI_SETSELBACK, 1, c);
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

void DefaultScheme::Load(CScintilla& sc, LPCTSTR filename)
{
	Scheme::Load(sc);
}

///////////////////////////////////////////////////////////
// SchemeManager
///////////////////////////////////////////////////////////


SchemeManager::SchemeManager(LPCTSTR schemepath, LPCTSTR compiledpath)
{
	SetPath(schemepath);
	
	if(compiledpath != NULL)
	{
		SetCompiledPath(compiledpath);
	}
	else
	{
		SetCompiledPath(schemepath);
	}

	Load();
}

SchemeManager::~SchemeManager()
{
	if(m_SchemePath)
		delete [] m_SchemePath;
	
	if(m_CompiledPath)
		delete [] m_CompiledPath;

	m_Schemes.clear();

	m_SchemeNameMap.clear();
	m_SchemeExtMap.clear();
}

void SetSMString(TCHAR*& str, LPCTSTR newpath)
{
	// Set the path, ensuring a trailing slash.
	int i = _tcslen(newpath);
	if( newpath[i-1] != _T('\\') && newpath[i-1] != _T('/'))
		i++;
	
	if(str != NULL)
		delete [] str;

	str = new TCHAR[i+1];
	_tcscpy(str, newpath);
	if(i != (int)_tcslen(newpath))
		_tcscat(str, _T("\\"));
}

void SchemeManager::SetCompiledPath(LPCTSTR compiledpath)
{
	SetSMString(m_CompiledPath, compiledpath);
}

void SchemeManager::SetPath(LPCTSTR schemepath)
{
	SetSMString(m_SchemePath, schemepath);
}

#define BAD_CSCHEME_FILE 0x01

void SchemeManager::Load()
{
	try
	{
		internalLoad(false);
	}
	catch(int& error)
	{
		if(error == BAD_CSCHEME_FILE)
		{
			internalLoad(true);
		}
	}
}

class FindMinder
{
public:
	FindMinder(HANDLE hf) : _hf(hf){}
	~FindMinder() { ::FindClose(_hf); }
private:
	HANDLE _hf;
};

void SchemeManager::internalLoad(bool forceCompile)
{
	PNASSERT(m_SchemePath != NULL);
	PNASSERT(m_CompiledPath != NULL);
	
	tstring sPattern = m_CompiledPath;
	sPattern += _T("*.cscheme");

	tstring usersettings = m_CompiledPath;
	usersettings += _T("UserSettings.xml");
	
	tstring defaultPath = m_CompiledPath;
	defaultPath += _T("default.cscheme");

	m_DefaultScheme.SetFileName(defaultPath.c_str());

	if(forceCompile || !FileExists(defaultPath.c_str()))
	{
		Compile();
	}

	tstring SchemeName;
	tstring to_open;
	
	WIN32_FIND_DATA FindFileData;
	BOOL found;
	HANDLE hFind = FindFirstFile(sPattern.c_str(), &FindFileData);

	if(hFind == INVALID_HANDLE_VALUE)
	{
		// The compiled schemes directory may have been moved, re-compile.
		Compile();
		hFind = FindFirstFile(sPattern.c_str(), &FindFileData);
	}

	if (hFind != INVALID_HANDLE_VALUE)
	{
		FindMinder minder(hFind);

		found = TRUE;

		Scheme *cs = NULL;
		SCIT csi;

		while(found)
		{
			if(_tcscmp(_T("default.cscheme"), FindFileData.cFileName) != 0)
			{
				Scheme sch;
				csi = m_Schemes.insert(m_Schemes.end(), sch);
				cs = &(*csi);

				cs->SetSchemeManager(this);
				
				to_open = m_CompiledPath;
				to_open += FindFileData.cFileName;

				cs->SetFileName(to_open.c_str());
				if(!cs->CheckName())
				{
					tstring dbgout = _T("Skipping bad scheme: ");
					dbgout += FindFileData.cFileName;
					m_Schemes.erase(csi);
					LOG(dbgout.c_str());

					// If this is our first time around, we throw an exception
					// to cause the outer function to force us to try a recompile...
					if(!forceCompile)
					{
						m_Schemes.clear();
						throw BAD_CSCHEME_FILE;
					}
				}
				else
				{
					SchemeName = cs->GetName();
					TCHAR *tcs = new TCHAR[SchemeName.size()+1];
					_tcscpy(tcs, SchemeName.c_str());
					tcs = CharLower(tcs);
					SchemeName = tcs;
					delete [] tcs;

					m_SchemeNameMap.insert(m_SchemeNameMap.end(), SCMITEM(SchemeName, cs));
				}
			}

			found = FindNextFile(hFind, &FindFileData);
		}
	}

	m_Schemes.sort();

	LoadExtMap(m_SchemeExtMap, m_SchemeFileNameMap);
}

/**
 * Load the extension to filetype mappings from a flat
 * properties style key=value file. The file must be formatted:
 * .extension=schemename\r\n
 */
void SchemeManager::LoadExtMap(SCHEME_MAP& extMap, SCHEME_MAP& fnMap, bool noUserMap)
{
	PNASSERT(m_SchemePath != NULL);
	PNASSERT(m_CompiledPath != NULL);
	
	tstring fn;
	
	if(!noUserMap)
	{
		fn = m_CompiledPath;
		fn += _T("extmap.dat");

		internalLoadExtMap(fn.c_str(), extMap, fnMap);
	}
	
	fn = m_SchemePath;
	fn += _T("extmap.dat");
	internalLoadExtMap(fn.c_str(), extMap, fnMap);
}

Scheme* SchemeManager::internalSchemeForFileName(const tstring& filename)
{
	SCHEME_MAPIT i = m_SchemeFileNameMap.find(filename);
	
	if(i != m_SchemeFileNameMap.end())
		return (*i).second;
	return NULL;
}

Scheme* SchemeManager::internalSchemeForExt(const tstring& extension)
{
	SCHEME_MAPIT i = m_SchemeExtMap.find(extension);
	
	if(i != m_SchemeExtMap.end())
		return (*i).second;
	return NULL;
}

/**
 * @return The scheme for extension "ext" e.g. .pas
 */
Scheme* SchemeManager::SchemeForExt(LPCTSTR ext)
{
	TCHAR *e = new TCHAR[_tcslen(ext)+1];
	_tcscpy(e, ext);
	e = CharLower(e);

	Scheme* pRet = internalSchemeForExt( tstring(e) );

	delete [] e;

	if(pRet == NULL)
		return &m_DefaultScheme;

	return pRet;
}

/**
 * @return whatever scheme is appropriate for the filename passed in.
 */
Scheme* SchemeManager::SchemeForFile(LPCTSTR filename)
{
	CFileName fn(filename);
	fn.ToLower();

	Scheme* pScheme = internalSchemeForExt( fn.GetExtension() );
	if(!pScheme)
	{
		// See if we can match on a simple filename basis.
		// This is basically for makefiles etc.
		pScheme = internalSchemeForFileName( fn.GetFileName() );
	}

	if(!pScheme)
		return &m_DefaultScheme;
	
	return pScheme;
}

/**
 * @return Scheme* identified by "name"
 */
Scheme* SchemeManager::SchemeByName(LPCTSTR name)
{
	TCHAR *e = new TCHAR[_tcslen(name)+1];
	_tcscpy(e, name);
	e = CharLower(e);

	SCHEME_MAPIT i = m_SchemeNameMap.find(tstring(e));

	delete [] e;

	if(i != m_SchemeNameMap.end())
	{
		return SCMITEM(*i).second;
	}
	else
		return &m_DefaultScheme;
}

/**
 * Compile all available schemes
 */
void SchemeManager::Compile()
{
	PNASSERT(m_SchemePath != NULL);
	PNASSERT(m_CompiledPath != NULL);

	SchemeCompiler sc;
	sc.Compile(m_SchemePath, m_CompiledPath, _T("master.scheme"));
}

void SchemeManager::BuildMenu(HMENU menu, CommandDispatch* pDispatch, CommandEventHandler* pHandler, int iCommand, bool bNewMenu)
{
	CSMenuHandle m(menu);
	int id;
	
	if(bNewMenu)
	{
		m.AddItem(_T("&Default\tCtrl+N"), ID_FILE_NEW);
		m.AddItem(_T("&Project"), ID_FILE_NEW_PROJECT);
		m.AddItem(_T("&Project Group"), ID_FILE_NEW_WORKSPACE);
		m.AddItem(_T(""));
	}
	
	id = pDispatch->RegisterCallback(pHandler, iCommand, (LPVOID)GetDefaultScheme());
	m.AddItem(_T("Plain Text"), id);

	for(SCIT i = m_Schemes.begin(); i != m_Schemes.end(); ++i)
	{
		if( !(*i).IsInternal() )
		{
			id = pDispatch->RegisterCallback(pHandler, iCommand, (LPVOID)&(*i));
			m.AddItem( (*i).GetTitle(), id);
		}
	}
}

void SchemeManager::SaveExtMap()
{
	// Keep a map around to see if we're actually different from the originals.
	SCHEME_MAP origExts;
	LoadExtMap(origExts, origExts, true);
	
	// Get the file...
	tstring fn;
	fn = m_CompiledPath;
	fn += _T("extmap.dat");
	CTextFile file;
	if(!file.Open(fn.c_str(), CFile::modeWrite | CFile::modeText))
		return;

	tstring line;

	for(SCHEME_MAP::const_iterator i = m_SchemeExtMap.begin(); i != m_SchemeExtMap.end(); ++i)
	{
		SCHEME_MAP::const_iterator match = origExts.find((*i).first);
		if(match != origExts.end())
		{
			if((*match).second == (*i).second)
				continue; // they're the same, skip it.
		}

		// write
		line = (*i).first + "=";
		line += (*i).second->GetName();
		line += "\n";
		file.WriteLine(line.c_str());
	}

	for(SCHEME_MAP::const_iterator j = m_SchemeFileNameMap.begin(); j != m_SchemeFileNameMap.end(); ++j)
	{
		SCHEME_MAP::const_iterator match = origExts.find((*j).first);
		if(match != origExts.end())
		{
			if((*match).second == (*j).second)
				continue; // they're the same, skip it.
		}

		// write
		line = (*j).first + "=";
		line += (*j).second->GetName();
		line += "\n";
		file.WriteLine(line.c_str());
	}

	file.Close();
}

void SchemeManager::internalLoadExtMap(LPCTSTR filename, SCHEME_MAP& extMap, SCHEME_MAP& fnMap)
{
	CTextFile file;
	bool bOK = file.Open(filename, CFile::modeText);
	
	if(bOK)
	{
		CString buf;
		
		tstring ext;
		tstring scheme;
		
		Scheme* sch;
		int pos;

		while(file.ReadLine(buf))
		{
			pos = buf.Find(_T('='));
			ext = buf.Left(pos);
			scheme = buf.Mid(pos+1);

			sch = SchemeByName(scheme.c_str());
			if(ext[0] != _T('.'))
				fnMap.insert(fnMap.end(), SCMITEM(ext, sch));
			else
				extMap.insert(extMap.end(), SCMITEM(ext, sch));
		}
		file.Close();
	}	
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