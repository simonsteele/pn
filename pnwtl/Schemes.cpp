#include "stdafx.h"
#include "Schemes.h"

/////////////////////////////////////////////////////////////////////////////////////////////

CScheme::CScheme()
{
	// This is a protected constructor, it can't be used
	// unless in a sub-class. Schemes shouldn't be instantiated
	// as stack member objects.

	m_SchemeFile = NULL;
	m_Name = NULL;
	m_pManager = NULL;
}

CScheme::CScheme(CSchemeManager* pManager)
{
	m_SchemeFile = NULL;
	m_Name = NULL;
	m_pManager = pManager;
}

CScheme::CScheme(CSchemeManager* pManager, LPCTSTR filename)
{
	m_Name = NULL;
	m_SchemeFile = new TCHAR[_tcslen(filename)+1];
	_tcscpy(m_SchemeFile, filename);
	m_pManager = pManager;
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
}

void CScheme::CheckName(LPCTSTR filename)
{
	CFile cfile;
	CompiledHdrRec Header;
	char *buf = NULL;

	LPCTSTR fn = NULL;

	if(filename)
		fn = filename;
	else
		if(m_SchemeFile)
			fn = m_SchemeFile;
		else
			throw "No filename for scheme to be opened!";

	CFileName cfn(fn);
	cfn.ChangeExtensionTo(_T(".cscheme"));
	cfn.ChangePathTo(m_pManager->GetCompiledPath());

	if(cfile.Open(cfn.c_str(), 0) == true)
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
			Compile();
			cfile.Open(cfn.c_str(), 0);
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
		// Convert into a unicode string...	
		TCHAR conv = new TCHAR[strlen(&hdr.Name[0])+1];
		mbstowcs(conv, &hdr.Name[0], strlen(&hdr.Name[0]));
		SetName(conv);
		delete [] conv;
		#else
		// Otherwise just copy the string.	
		SetName(&hdr.Name[0]);
		#endif

		// Close the file...
		cfile.Close();
	}
}

void CScheme::SetName(LPCTSTR name)
{
	if(m_Name)
		delete [] m_Name;
	m_Name = new TCHAR[_tcslen(name)+1];
	_tcscpy(m_Name, name);
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
			fn.ChangeExtensionTo(_T(".cscheme"));
			fn.ChangePathTo(m_pManager->GetCompiledPath());
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
			Compile();
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
		TCHAR conv = new TCHAR[strlen(&hdr.Name[0])+1];
		mbstowcs(conv, &hdr.Name[0], strlen(&hdr.Name[0]));
		SetName(conv);
		delete [] conv;
		#else
		// Otherwise just copy the string.	
		SetName(&hdr.Name[0]);
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

			if(hdr.Folding & fldComments == fldComments)
				sc.SPerform(SCI_SETPROPERTY, (WPARAM)_T("fold.comments"), (LPARAM)_T("1"));
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
}

bool CScheme::IsCompiled()
{
	CFileName fn(m_SchemeFile);
	CFileName cfile(fn);

	cfile.ChangeExtensionTo(_T(".cscheme"));
	cfile.ChangePathTo(m_pManager->GetCompiledPath());

	int fac = cfile.GetFileAge();
	if(fac != -1)
	{
		if(fn.GetFileAge() == fac)
			return true;
	}

	return false;
}


void CScheme::EnsureCompiled()
{
	if(!IsCompiled())
	{
		Compile();
	}
}

bool CScheme::Compile(LPCTSTR outfile)
{
	TCHAR* output = (TCHAR*)outfile;
	bool bAllocated = false;
	
	if(!outfile)
	{
		CFileName fn(m_SchemeFile);
		fn.ChangeExtensionTo(_T(".cscheme"));
		fn.ChangePathTo(m_pManager->GetCompiledPath());
		output = new TCHAR[fn.GetLength()+1];
		_tcscpy(output, fn.c_str());
		bAllocated = true;
	}

	CSchemeCompiler compiler;
	bool bRet = compiler.Compile(m_SchemeFile, output);

	if(bAllocated)
		delete [] output;

	return bRet;
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
		m_CompiledPath = NULL;
	}

	Load();
}

CSchemeManager::~CSchemeManager()
{
	if(m_SchemePath)
		delete m_SchemePath;
	
	if(m_CompiledPath)
		delete m_CompiledPath;

	for(scit i = m_Schemes.begin(); i != m_Schemes.end(); ++i)
	{
		delete (*i);
	}
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
		delete str;

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

void CSchemeManager::Load(LPCTSTR fromfolder)
{
	LPCTSTR path = NULL;
	if(NULL == fromfolder)
		path = m_SchemePath;
	else
		path = fromfolder;

	if(!path)
		return;

	// Find the scheme def files one by one...
	HANDLE hFind;
	WIN32_FIND_DATA FindFileData;

	ctcString sPattern = path;
	
	ctcString SchemeName(_T(""));

	sPattern += _T("*.scheme");

	CScheme *cs;

	hFind = FindFirstFile(sPattern.c_str(), &FindFileData);
	if (hFind != INVALID_HANDLE_VALUE) 
	{
		//Found the first file...
		BOOL found = TRUE;
		ctcString to_open;

		while (found)
		{
			///@todo Do we really need to keep a list of Schemes and a map?
			// to_open is a scheme file
			to_open = path;
			to_open += FindFileData.cFileName;

			cs = new CScheme(this, to_open.c_str());
			
			m_Schemes.insert(m_Schemes.end(), cs);

			cs->EnsureCompiled();
			cs->CheckName();
			
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

	LoadExtMap(path);
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