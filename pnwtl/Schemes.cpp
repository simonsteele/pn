/**
 * @file Schemes.cpp
 * @brief Implement CScheme and CSchemeManager.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"

#include "Schemes.h"
#include "optionsmanager.h"
#include "files.h"

#include "ssreg.h"
using namespace ssreg;

///////////////////////////////////////////////////////////
// CScheme
///////////////////////////////////////////////////////////

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
				::OutputDebugString(_T("PN2: Compiled Scheme Header invalid or corrupt after compile."));
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

bool CScheme::OpenCompiledFile(CFile& file, LPCTSTR filename)
{
	CFileName fn;

	if(filename)
	{
		fn = filename;
	}
	else
	{
		if(m_SchemeFile)
		{
			fn = m_SchemeFile;
		}
		else
			throw "No filename for scheme to be opened!";
	}

	return ( file.Open(fn.c_str(), 0) == TRUE );
}

/**
 * This function allocates a list of StyleDetails objects containing
 * the settings used by this scheme. The caller must free the list
 * and the items contained within.
 */
StylesList* CScheme::CreateStylesList()
{
	CFile			cfile;
	CompiledHdrRec	Header;
	SchemeHdrRec	hdr;
	TextRec			Txt;
	MsgRec			Msg;
	char			Next2;
	char*			buf;

	if(OpenCompiledFile(cfile))
	{
		cfile.Read(&Header, sizeof(CompiledHdrRec));
		if(strcmp(Header.Magic, FileID) != 0)
		{
			// Can't read the file... never mind.
			cfile.Close();
			return NULL;
		}

		if(Header.Version != CompileVersion)
		{
			cfile.Close();
			return  NULL;
		}

		cfile.Read(&hdr, sizeof(SchemeHdrRec));

		long posFirstStyle = cfile.GetPosition();

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

void CScheme::Load(CScintilla& sc, LPCTSTR filename)
{
	CFile cfile;
	CompiledHdrRec Header;
	SchemeHdrRec hdr;
	MsgRec Msg;
	TextRec Txt;
	char *buf = NULL;
	char Next2;

	if( OpenCompiledFile(cfile, filename) )
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
			
			if( OpenCompiledFile(cfile, filename) )
			{
				cfile.Read(&Header, sizeof(CompiledHdrRec));
				if(strcmp(Header.Magic, FileID) != 0 || Header.Version != CompileVersion)
				{
					// Not the right version, and compiling didn't help:
					cfile.Close();
					throw "Not the right kinda file...";
					return;
				}
			}
			else
				return;
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

		if(hdr.Flags & schUseTabs)
			sc.SPerform(SCI_SETUSETABS, 1, 0);

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
	COptionsManager& options = COptionsManager::GetInstanceRef();

	//ss 16/02/2003 Now performed by document...
	//sc.SPerform(SCI_SETEOLMODE, options.LineEndings);

	// Line Indentation...
	///@todo Specify indentation guides colours?
	sc.SPerform(SCI_SETINDENTATIONGUIDES, (options.ShowIndentGuides ? 1 : 0));
	sc.SPerform(SCI_SETUSETABS, options.UseTabs ? 1 : 0);
	sc.SPerform(SCI_SETTABWIDTH, options.TabWidth);

	// Set even treatment of left and right caret positioning, and sloppy behaviour. 
	// Use 3 lines as the jump when scrolling up and down.
	sc.SPerform(SCI_SETXCARETPOLICY, CARET_SLOP | CARET_EVEN, 3);
	sc.SPerform(SCI_SETYCARETPOLICY, CARET_SLOP | CARET_EVEN, 3);

	sc.SPerform(SCI_STYLERESETDEFAULT);
	sc.SPerform(SCI_STYLESETFORE, STYLE_DEFAULT, ::GetSysColor(COLOR_WINDOWTEXT));
	sc.SPerform(SCI_STYLESETBACK, STYLE_DEFAULT, ::GetSysColor(COLOR_WINDOW));
	sc.SPerform(SCI_STYLECLEARALL);

	sc.DefineBookmarks();
	sc.DefineNumberedBookmarks();
	
	// Default windows edit control behaviour... This needs to be optional.
	///@todo allow default scintilla coloured selection...
	if(options.Get(PNSK_EDITOR, _T("DefaultSelectionColours"), true))
	{
		sc.SPerform(SCI_SETSELFORE, 1, ::GetSysColor(COLOR_HIGHLIGHTTEXT));
		sc.SPerform(SCI_SETSELBACK, 1, ::GetSysColor(COLOR_HIGHLIGHT));
	}
	else
	{
		COLORREF c;

		if(options.Get(PNSK_EDITOR, _T("SetSelectionFore"), false))
		{
			c = (COLORREF)options.Get(PNSK_EDITOR, _T("SelectionFore"), (int)::GetSysColor(COLOR_HIGHLIGHTTEXT));
			sc.SPerform(SCI_SETSELFORE, 1, c);
		}
		else
			sc.SPerform(SCI_SETSELFORE, 0, 0);
		
		c = (COLORREF)options.Get(PNSK_EDITOR, _T("SelectionBack"), (int)::GetSysColor(COLOR_HIGHLIGHT));
		sc.SPerform(SCI_SETSELBACK, 1, c);
	}

	sc.SPerform(SCI_SETPROPERTY, (WPARAM)"asp.default.language", (LPARAM)"2");
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

///////////////////////////////////////////////////////////
// CDefaultScheme
///////////////////////////////////////////////////////////

void CDefaultScheme::Load(CScintilla& sc, LPCTSTR filename)
{
	//SetupScintilla(sc);
	CScheme::Load(sc, m_SchemeFile);
}

///////////////////////////////////////////////////////////
// CSchemeManager
///////////////////////////////////////////////////////////


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

	bool bCompile = false;
	
	tstring usersettings = m_CompiledPath;
	usersettings += _T("UserSettings.xml");
	int us_age = reg.ReadInt(_T("UserSettings"), 0);
	if(FileExists(usersettings.c_str()))
	{
		if(FileAge(usersettings.c_str()) != us_age)
			bCompile = true;
	}
	else
	{
		if(us_age != 0)
			bCompile = true;
	}

	tstring defaultPath = m_CompiledPath;
	defaultPath += _T("default.cscheme");
	m_DefaultScheme.SetFileName(defaultPath.c_str());
	if(!FileExists(defaultPath.c_str()))
		bCompile = true;

	// Find the scheme def files one by one...
	HANDLE hFind;
	WIN32_FIND_DATA FindFileData;

	tstring sPattern(m_SchemePath);
	
	tstring SchemeName(_T(""));

	sPattern += _T("*.scheme");

	BOOL found = TRUE;
	tstring to_open;

	if(!bCompile)
	{

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
	}

	if(bCompile)
	{
		Compile();
	}

	sPattern = m_CompiledPath;
	sPattern += _T("*.cscheme");
	
	hFind = FindFirstFile(sPattern.c_str(), &FindFileData);
	if(hFind == INVALID_HANDLE_VALUE)
	{
		// The compiled schemes directory may have been moved, re-compile.
		Compile();
		hFind = FindFirstFile(sPattern.c_str(), &FindFileData);
	}
	if (hFind != INVALID_HANDLE_VALUE)
	{
		found = TRUE;

		CScheme *cs = NULL;
		SCIT csi;

		while(found)
		{
			if(_tcscmp(_T("default.cscheme"), FindFileData.cFileName) != 0)
			{
				
				CScheme sch;
				csi = m_Schemes.insert(m_Schemes.end(), sch);
				cs = &(*csi);

				cs->SetSchemeManager(this);
				
				to_open = m_CompiledPath;
				to_open += FindFileData.cFileName;

				cs->SetFileName(to_open.c_str());
				cs->CheckName();

				SchemeName = cs->GetName();
				TCHAR *tcs = new TCHAR[SchemeName.size()+1];
				_tcscpy(tcs, SchemeName.c_str());
				tcs = CharLower(tcs);
				SchemeName = tcs;
				delete [] tcs;

				m_SchemeNameMap.insert(m_SchemeNameMap.end(), SCMITEM(SchemeName, cs));
			}

			found = FindNextFile(hFind, &FindFileData);
		}

		FindClose(hFind);
	}

	m_Schemes.sort();

	LoadExtMap();
}

/**
 * Load the extension to filetype mappings from a flat
 * properties style key=value file. The file must be formatted:
 * .extension=schemename\r\n
 */
void CSchemeManager::LoadExtMap()
{
	PNASSERT(m_SchemePath != NULL);
	PNASSERT(m_CompiledPath != NULL);
	
	CTextFile file;
	CString fn;
	bool bOK;
	
	fn = m_CompiledPath;
	fn += _T("extmap.dat");
	bOK = file.Open(fn, CFile::modeText);
	
	if(!bOK)
	{
		fn = m_SchemePath;
		fn += _T("extmap.dat");
		bOK = file.Open(fn, CFile::modeText);
	}
		
	if(bOK)
	{
		CString buf;
		
		tstring ext;
		tstring scheme;
		
		CScheme* sch;
		int pos;

		while(file.ReadLine(buf))
		{
			pos = buf.Find(_T('='));
			ext = buf.Left(pos);
			scheme = buf.Mid(pos+1);

			sch = SchemeByName(scheme.c_str());
			if(ext[0] != _T('.'))
				m_SchemeFileNameMap.insert(m_SchemeFileNameMap.end(), SCMITEM(ext, sch));
			else
				m_SchemeExtMap.insert(m_SchemeExtMap.end(), SCMITEM(ext, sch));
		}
		file.Close();
	}
}

CScheme* CSchemeManager::InternalSchemeForFileName(const tstring& filename)
{
	SCHEME_MAPIT i = m_SchemeFileNameMap.find(filename);
	
	if(i != m_SchemeFileNameMap.end())
		return (*i).second;
	return NULL;
}

CScheme* CSchemeManager::InternalSchemeForExt(const tstring& extension)
{
	SCHEME_MAPIT i = m_SchemeExtMap.find(extension);
	
	if(i != m_SchemeExtMap.end())
		return (*i).second;
	return NULL;
}

/**
 * @return The scheme for extension "ext" e.g. .pas
 */
CScheme* CSchemeManager::SchemeForExt(LPCTSTR ext)
{
	TCHAR *e = new TCHAR[_tcslen(ext)+1];
	_tcscpy(e, ext);
	e = CharLower(e);

	CScheme* pRet = InternalSchemeForExt( tstring(e) );

	delete [] e;

	if(pRet == NULL)
		return &m_DefaultScheme;

	return pRet;
}

/**
 * @return whatever scheme is appropriate for the filename passed in.
 */
CScheme* CSchemeManager::SchemeForFile(LPCTSTR filename)
{
	CFileName fn(filename);
	fn.ToLower();

	CScheme* pScheme = InternalSchemeForExt( fn.GetExtension() );
	if(!pScheme)
	{
		// See if we can match on a simple filename basis.
		// This is basically for makefiles etc.
		pScheme = InternalSchemeForFileName( fn.GetFileName() );
	}

	if(!pScheme)
		return &m_DefaultScheme;
	
	return pScheme;
}

/**
 * @return CScheme* identified by "name"
 */
CScheme* CSchemeManager::SchemeByName(LPCTSTR name)
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
	
	id = CSMenuManager::GetInstance()->RegisterCallback(pHandler, iCommand, (LPVOID)GetDefaultScheme());
	m.AddItem(_T("Plain Text"), id);

	for(SCIT i = m_Schemes.begin(); i != m_Schemes.end(); ++i)
	{
		id = CSMenuManager::GetInstance()->RegisterCallback(pHandler, iCommand, (LPVOID)&(*i));
		m.AddItem( (*i).GetTitle(), id);
	}
}

CSchemeManager * CSchemeManager::GetInstance()
{
	if(!s_pInstance)
		s_pInstance = new CSchemeManager;

	return s_pInstance;
}

CSchemeManager & CSchemeManager::GetInstanceRef()
{
	return *GetInstance();
}

void CSchemeManager::DeleteInstance()
{
	if(s_pInstance)
	{
		delete s_pInstance;
		s_pInstance = NULL;
	}
}

///////////////////////////////////////////////////////////
// CSchemeManager statics
///////////////////////////////////////////////////////////

CSchemeManager* CSchemeManager::s_pInstance = NULL;

///////////////////////////////////////////////////////////
// CSchemeSwitcher
///////////////////////////////////////////////////////////

CSchemeSwitcher::CSchemeSwitcher()
{
	
}

CSchemeSwitcher::~CSchemeSwitcher()
{

}

void CSchemeSwitcher::BuildMenu(int iCommand)
{
	menuid_scheme_pair x;
	CSchemeManager& sm = CSchemeManager::GetInstanceRef();
	SCHEME_LIST* pSchemes = sm.GetSchemesList();
	CSMenuManager& mm = *CSMenuManager::GetInstance();
	
	m_menu.AddItem( sm.GetDefaultScheme()->GetTitle(), mm.RegisterCallback(NULL, iCommand, (LPVOID)sm.GetDefaultScheme()) );

	for(SCIT i = pSchemes->begin(); i != pSchemes->end(); ++i)
	{
		x.pScheme = &(*i);
		x.iCommand = CSMenuManager::GetInstance()->RegisterCallback(NULL, iCommand, (LPVOID)x.pScheme);

		m_menu.AddItem( x.pScheme->GetTitle(), x.iCommand );
		m_list.insert(m_list.end(), x);
	}
}

void CSchemeSwitcher::Reset(int iCommand)
{
	m_list.clear();
	BuildMenu(iCommand);
}

void CSchemeSwitcher::SetActiveScheme(CScheme* pCurrent)
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