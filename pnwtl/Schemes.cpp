/**
 * @file Schemes.h
 * @brief Implement CScheme and CSchemeManager.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"

#include "Schemes.h"
#include "files.h"

#include "ssreg.h"
using namespace ssreg;

/////////////////////////////////////////////////////////////////////////////////////////////

CScheme::CScheme()
{
	Init();
}

CScheme::CScheme(CSchemeManager* pManager)
{
	Init();

	m_pManager = pManager;
}

CScheme::CScheme(CSchemeManager* pManager, LPCTSTR filename)
{
	Init();

	m_SchemeFile = new TCHAR[_tcslen(filename)+1];
	_tcscpy(m_SchemeFile, filename);
	m_pManager = pManager;
}

void CScheme::Init()
{
	m_SchemeFile = NULL;
	m_Name = NULL;
	m_Title = NULL;
	m_pManager = NULL;
}

CScheme::~CScheme()
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

void CScheme::CheckName()
{
	CFile cfile;
	CompiledHdrRec Header;
	char *buf = NULL;

	PNASSERT(m_SchemeFile != NULL);

	LPCTSTR fn = m_SchemeFile;

	if(cfile.Open(fn, 0) == true)
	{
		cfile.Read(&Header, sizeof(CompiledHdrRec));
		if(strcmp(Header.Magic, FileID) != 0)
		{
			cfile.Close();
			throw "Not the right kinda file...";
			return;
		}

		if(Header.Version != CompileVersion)
		{
			cfile.Close();
			
			// Attempt to re-compile the schemes...
			m_pManager->Compile();

			cfile.Open(fn, 0);
			cfile.Read(&Header, sizeof(CompiledHdrRec));
			if(strcmp(Header.Magic, FileID) != 0 || Header.Version != CompileVersion)
			{
				// Not the right version, and compiling didn't help:
				cfile.Close();
				throw "Not the right kinda file...";
				return;
			}	
		}

		SchemeHdrRec hdr;
		cfile.Read(&hdr, sizeof(SchemeHdrRec));
		
		#ifdef UNICODE

		USES_CONVERSION;

		// Convert into a unicode string...	
		SetName(A2T(&hdr.Name[0]));
		SetTitle(A2T(&hdr.Title[0]));
		/*TCHAR conv = new TCHAR[strlen(&hdr.Name[0])+1];
		mbstowcs(conv, &hdr.Name[0], strlen(&hdr.Name[0]));
		SetName(conv);
		delete [] conv;*/
		#else
		// Otherwise just copy the string.	
		SetName(&hdr.Name[0]);
		SetTitle(&hdr.Title[0]);
		#endif

		// Close the file...
		cfile.Close();
	}
}

void CScheme::SetName(LPCTSTR name)
{
	if(name)
	{
		if(m_Name)
			delete [] m_Name;
		m_Name = new TCHAR[_tcslen(name)+1];
		_tcscpy(m_Name, name);
	}
}

void CScheme::SetTitle(LPCTSTR title)
{
	if(title)
	{
		if(m_Title)
			delete [] m_Title;

		m_Title = new TCHAR[_tcslen(title)+1];
		_tcscpy(m_Title, title);
	}
}

void CScheme::SetFileName(LPCTSTR filename)
{
	if(filename)
	{
		if(m_SchemeFile != NULL)
			delete [] m_SchemeFile;

		m_SchemeFile = new TCHAR[_tcslen(filename)+1];
		_tcscpy(m_SchemeFile, filename);
	}
}


void CScheme::SetSchemeManager(CSchemeManager* pManager)
{
	m_pManager = pManager;
}

void CScheme::Load(CScintilla& sc, LPCTSTR filename)
{
	CFile cfile;
	CompiledHdrRec Header;
	SchemeHdrRec hdr;
	MsgRec Msg;
	TextRec Txt;
	char *buf = NULL;
	char Next2;

	CFileName fn;

	if(filename)
		fn = filename;
	else
		if(m_SchemeFile)
		{
			fn = m_SchemeFile;
		}
		else
			throw "No filename for scheme to be opened!";

	if (cfile.Open(fn.c_str(), 0) == TRUE)
	{
		cfile.Read(&Header, sizeof(CompiledHdrRec));
		if(strcmp(Header.Magic, FileID) != 0)
		{
			cfile.Close();
			throw "Not the right kinda file...";
			return;
		}

		if(Header.Version != CompileVersion)
		{
			cfile.Close();
			// Attempt to compile me...
			m_pManager->Compile();
			
			cfile.Open(fn.c_str(), 0);
			cfile.Read(&Header, sizeof(CompiledHdrRec));
			if(strcmp(Header.Magic, FileID) != 0 || Header.Version != CompileVersion)
			{
				// Not the right version, and compiling didn't help:
				cfile.Close();
				throw "Not the right kinda file...";
				return;
			}	
		}

		cfile.Read(&hdr, sizeof(SchemeHdrRec));
		
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

		// Set the defaults - these may be changed by the scheme loading
		// process...
		SetupScintilla(sc);

		if(hdr.Folding & fldEnabled == fldEnabled)
		{
			///@todo obviously these details need to come from settings somewhere...
			sc.SPerform(SCI_SETPROPERTY, (WPARAM)_T("fold"), (LPARAM)_T("1"));
			sc.SPerform(SCI_SETMARGINTYPEN, 2, SC_MARGIN_SYMBOL);
			sc.SPerform(SCI_SETMARGINMASKN, 2, SC_MASK_FOLDERS);
			sc.SPerform(SCI_SETMARGINSENSITIVEN, 2, true);
			sc.SPerform(SCI_SETMARGINWIDTHN, 2, 14);
			sc.SPerform(SCI_SETFOLDFLAGS, 16, 0);
			sc.SetFoldingMargins(efsVSNet);

			if(hdr.Folding & fldCompact == fldCompact)
				sc.SPerform(SCI_SETPROPERTY, (WPARAM)_T("fold.compact"), (LPARAM)_T("1"));
			else
				sc.SPerform(SCI_SETPROPERTY, (WPARAM)_T("fold.compact"), (LPARAM)_T("0"));

			if(hdr.Folding & fldComments == fldComments)
				sc.SPerform(SCI_SETPROPERTY, (WPARAM)_T("fold.comment"), (LPARAM)_T("1"));

			//sc.SPerform(SCI_SETPROPERTY, (WPARAM)_T("fold.preprocessor"), (LPARAM)_T("1"));
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
			}
		}

		cfile.Close();
	}
	else
	{
		// Show an error or something...
	}
}

void CScheme::SetupScintilla(CScintilla& sc)
{
	COptionsManager& options = theApp.GetOptionsManager();

	sc.SPerform(SCI_SETEOLMODE, options.LineEndings);

	// Line Indentation...
	if (options.ShowIndentGuides)
	{
		///@todo Specify indentation guides colours?
		sc.SPerform(SCI_SETINDENTATIONGUIDES, 1);
	}

	sc.SPerform(SCI_SETTABWIDTH, options.TabWidth);

	// Set even treatment of left and right caret positioning, and sloppy behaviour. 
	// Use 3 lines as the jump when scrolling up and down.
	sc.SPerform(SCI_SETCARETPOLICY, CARET_SLOP | CARET_XEVEN, 3);

	sc.SPerform(SCI_STYLERESETDEFAULT);
	sc.SPerform(SCI_STYLESETFORE, STYLE_DEFAULT, ::GetSysColor(COLOR_WINDOWTEXT));
	sc.SPerform(SCI_STYLESETBACK, STYLE_DEFAULT, ::GetSysColor(COLOR_WINDOW));
	sc.SPerform(SCI_STYLECLEARALL);
	
	// Default windows edit control behaviour... This needs to be optional.
	///@todo allow default scintilla coloured selection...
	sc.SPerform(SCI_SETSELFORE, 1, ::GetSysColor(COLOR_HIGHLIGHTTEXT));
	sc.SPerform(SCI_SETSELBACK, 1, ::GetSysColor(COLOR_HIGHLIGHT));
}

bool CScheme::operator < (const CScheme& compare) const 
{
	return _tcsicmp(GetTitle(), compare.GetTitle()) < 0;
}

bool CScheme::operator > (const CScheme& compare) const
{
	return _tcsicmp(GetTitle(), compare.GetTitle()) > 0;
}

const CScheme& CScheme::operator = (const CScheme& copy)
{
	m_pManager = copy.m_pManager;

	SetName(copy.GetName());
	SetTitle(copy.GetTitle());
	SetFileName(copy.GetFileName());

	return *this;
}

/////////////////////////////////////////////////////////////////////////////////////////////

void CDefaultScheme::Load(CScintilla& sc, LPCTSTR filename)
{
	SetupScintilla(sc);
}

/////////////////////////////////////////////////////////////////////////////////////////////


CSchemeManager::CSchemeManager(LPCTSTR schemepath, LPCTSTR compiledpath)
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

CSchemeManager::~CSchemeManager()
{
	if(m_SchemePath)
		delete [] m_SchemePath;
	
	if(m_CompiledPath)
		delete [] m_CompiledPath;

	/*for(SCIT i = m_Schemes.begin(); i != m_Schemes.end(); ++i)
	{
		delete (*i);
	}*/
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

void CSchemeManager::SetCompiledPath(LPCTSTR compiledpath)
{
	SetSMString(m_CompiledPath, compiledpath);
}

void CSchemeManager::SetPath(LPCTSTR schemepath)
{
	SetSMString(m_SchemePath, schemepath);
}

void CSchemeManager::Load()
{
	PNASSERT(m_SchemePath != NULL);
	PNASSERT(m_CompiledPath != NULL);

	CSRegistry reg;
	reg.OpenKey(_T("Software\\Echo Software\\PN2\\SchemeDates"), true);

	// Find the scheme def files one by one...
	HANDLE hFind;
	WIN32_FIND_DATA FindFileData;

	ctcString sPattern(m_SchemePath);
	
	ctcString SchemeName(_T(""));

	sPattern += _T("*.scheme");

	bool bCompile = false;

	BOOL found = TRUE;
	ctcString to_open;

	hFind = FindFirstFile(sPattern.c_str(), &FindFileData);
	if (hFind != INVALID_HANDLE_VALUE) 
	{
		while (found)
		{
			///@todo Do we really need to keep a list of Schemes and a map?
			// to_open is a scheme file
			to_open = m_SchemePath;
			to_open += FindFileData.cFileName;

			if( reg.ReadInt(FindFileData.cFileName, 0) != FileAge(to_open.c_str()) )
			{
				bCompile = true;
				break;
			}

			found = FindNextFile(hFind, &FindFileData);
		}

		FindClose(hFind);
	}

	if(bCompile)
	{
		Compile();
	}

	sPattern = m_CompiledPath;
	sPattern += _T("*.cscheme");
	
	hFind = FindFirstFile(sPattern.c_str(), &FindFileData);
	if (hFind != INVALID_HANDLE_VALUE)
	{
		found = TRUE;

		CScheme *cs = NULL;
		SCIT csi;

		while(found)
		{
			to_open = m_CompiledPath;
			to_open += FindFileData.cFileName;

			csi = m_Schemes.insert(m_Schemes.end());
			cs = &(*csi);
			cs->SetSchemeManager(this);
			cs->SetFileName(to_open.c_str());
			cs->CheckName();

			//cs = new CScheme(this, to_open.c_str());
			//m_Schemes.insert(m_Schemes.end(), cs);

			///@todo replace this with something less lower-case un-friendly
			SchemeName = cs->GetName();
			TCHAR *tcs = new TCHAR[SchemeName.size()+1];
			_tcscpy(tcs, SchemeName.c_str());
			tcs = CharLower(tcs);
			SchemeName = tcs;
			delete [] tcs;

			m_SchemeNameMap.insert(m_SchemeNameMap.end(), SCMITEM(SchemeName, cs));

			found = FindNextFile(hFind, &FindFileData);
		}

		FindClose(hFind);
	}

	m_Schemes.sort();

	LoadExtMap(m_CompiledPath);
}

/**
 * Now we load the extension to filetype mappings from a flat
 * properties style key=value file. The file must be formatted:
 * .extension=schemename\r\n
 */
void CSchemeManager::LoadExtMap(LPCTSTR folder)
{
	CString fn(folder);
	fn += _T("extmap.dat");
	
	CTextFile file;
	file.Open(fn, CFile::modeText);

	CString buf;
	
	// The strings currently used for the rest of the scheme management
	// system are std::strings typedef'd to ctcString.
	ctcString ext;
	ctcString scheme;
	
	CScheme* sch;
	int pos;

	while(file.ReadLine(buf))
	{
		pos = buf.Find(_T('='));
		ext = buf.Left(pos);
		scheme = buf.Mid(pos+1);

		sch = SchemeByName(scheme.c_str());
		m_SchemeExtMap.insert(m_SchemeExtMap.end(), SCMITEM(ext, sch));
	}
	file.Close();
}

/**
 * Here is where it may be better to use some custom string class than
 * a basic_string - we can use one with built in allocation limiting,
 * and lowercasing. Therefore, the comparison will save allocating a 
 * string twice for every get. However, this is for later... Alternatively,
 * we could simply make use of the stricmp feature then... Another alternative
 * would be to make a sub-class of basic_string which used _tcsicmp. This would
 * not provide allocation limiting, but might be simpler.
 * @todo above optimisations.
 */
CScheme* CSchemeManager::SchemeForExt(LPCTSTR ext)
{
	TCHAR *e = new TCHAR[_tcslen(ext)+1];
	_tcscpy(e, ext);
	e = CharLower(e);

	SCHEME_MAPIT i = m_SchemeExtMap.find(ctcString(e));

	delete [] e;
	
	if(i != m_SchemeExtMap.end())
	{
		return SCMITEM(*i).second;
	}
	else
		return &m_DefaultScheme;
}

/**
 * Here is where it may be better to use some custom string class than
 * a basic_string - we can use one with built in allocation limiting,
 * and lowercasing. Therefore, the comparison will save allocating a 
 * string twice for every get. However, this is for later... Alternatively,
 * we could simply make use of the stricmp feature then... Another alternative
 * would be to make a sub-class of basic_string which used _tcsicmp. This would
 * not provide allocation limiting, but might be simpler.
 */
CScheme* CSchemeManager::SchemeByName(LPCTSTR name)
{
	TCHAR *e = new TCHAR[_tcslen(name)+1];
	_tcscpy(e, name);
	e = CharLower(e);

	SCHEME_MAPIT i = m_SchemeNameMap.find(ctcString(e));

	delete [] e;

	if(i != m_SchemeNameMap.end())
	{
		return SCMITEM(*i).second;
	}
	else
		return &m_DefaultScheme;
}

void CSchemeManager::Compile()
{
	PNASSERT(m_SchemePath != NULL);
	PNASSERT(m_CompiledPath != NULL);

	SchemeCompiler sc;
	sc.Compile(m_SchemePath, m_CompiledPath, _T("master.scheme"));
}

void CSchemeManager::BuildMenu(HMENU menu, CSMenuEventHandler* pHandler, int iCommand, bool bNewMenu)
{
	CSMenuHandle m(menu);
	int id;
	
	if(bNewMenu)
	{
		m.AddItem(_T("&Default\tCtrl+N"), ID_FILE_NEW);
	}
	else
	{
		id = CSMenuManager::GetInstance()->RegisterCallback(pHandler, iCommand, (LPVOID)theApp.GetSchemes().GetDefaultScheme());
		m.AddItem(_T("Plain Text"), id);
	}

	for(SCIT i = m_Schemes.begin(); i != m_Schemes.end(); ++i)
	{
		id = CSMenuManager::GetInstance()->RegisterCallback(pHandler, iCommand, (LPVOID)&(*i));
		m.AddItem( (*i).GetTitle(), id);
	}
}

/////////////////////////////////////////////////////////////////////////////////////////////

CSchemeSwitcher::CSchemeSwitcher()
{
	
}

CSchemeSwitcher::~CSchemeSwitcher()
{
	m_list.clear();
}

void CSchemeSwitcher::AddMenu(HMENU hMenu, int iCommand)
{
	CSMenuHandle m(hMenu);

	if(m_list.size() == 0)
	{
		BuildMenu(hMenu, iCommand);
	}
	
	for(MISCHEMELIST::iterator i = m_list.begin(); i != m_list.end(); ++i)
	{
		m.AddItem( (*i).pScheme->GetTitle(), (*i).iCommand);
	}
}

void CSchemeSwitcher::BuildMenu(HMENU hMenu, int iCommand)
{
	if(m_list.size() == 0)
	{
		menuid_scheme_pair x;
		
		x.pScheme = theApp.GetSchemes().GetDefaultScheme();
		x.iCommand = CSMenuManager::GetInstance()->RegisterCallback(NULL, iCommand, (LPVOID)x.pScheme);
		m_list.insert(m_list.end(), x);
		
		SCHEME_LIST* pSchemes = theApp.GetSchemes().GetSchemesList();

		for(SCIT i = pSchemes->begin(); i != pSchemes->end(); ++i)
		{
			x.pScheme = &(*i);
			x.iCommand = CSMenuManager::GetInstance()->RegisterCallback(NULL, iCommand, (LPVOID)x.pScheme);
			m_list.insert(m_list.end(), x);
		}
	}
}

void CSchemeSwitcher::Reset()
{
	throw "Not Yet Implemented";
}

void CSchemeSwitcher::SetActiveScheme(HMENU hMenu, CScheme* pCurrent)
{
	for(MISCHEMELIST::iterator i = m_list.begin(); i != m_list.end(); ++i)
	{
		if((*i).pScheme == pCurrent)
			::CheckMenuItem((HMENU)hMenu, (*i).iCommand, MF_BYCOMMAND | MF_CHECKED);
		else
			::CheckMenuItem((HMENU)hMenu, (*i).iCommand, MF_BYCOMMAND | MF_UNCHECKED);
	}
}