/**
 * @file SchemeCompiler.cpp
 * @brief Implementation of the CSchemeCompiler class.
 * @author Simon Steele
 * @note copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 * 
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
//#include "pn.h"
#include "SchemeCompiler.h"

int chartoval(char inp)
{
	int Result = 0;
	
	if (inp >= '0' && inp <= '9') 
	{
		Result = inp - 48;
	}
	else if (inp >= 'a' && inp <= 'f')
	{
		Result = inp - 87;
	}
	else if (inp >= 'A' && inp <= 'F') 
	{
		Result = inp - 55;
	}
  
	return Result;
}

COLORREF PNStringToColor(const char* input)
{
  	const char*	Part;
	int		res;
	int		Result;

	//b,g,r - output format in hex $bbggrr
	//r,g,b - input format in string, rr,gg,bb
	// Default colour
	Result = ::GetSysColor(COLOR_WINDOWTEXT);
	// only works for xx.xx.xx colours...
	if (strlen(input) != 8)
	{
		return Result;
	}
	
	Part = input;
	res = (0x10 * chartoval(Part[0]));
	if (Part[1] != _T('0'))
	{
		res = res + (chartoval(Part[1]));
	}
	Part += 3;
	res += (0x1000 * chartoval(Part[0]));
	res += (0x100 * chartoval(Part[1]));
	Part += 3;
	res += (0x100000 * chartoval(Part[0]));
	res += (0x10000 * chartoval(Part[1]));
	Result = res;

	return Result;
}

/////////////////////////////////////////////////////////////////////////////////////////////


CStyleDetails::CStyleDetails()
{
	FontName = "Courier New";
	FontSize = 10;
	ForeColor = RGB(0,0,0);
	FCString = "00.00.00";
	BackColor = RGB(255,255,255);
	BCString = "ff.ff.ff";
	Bold = false;
	Italic = false;
	Underline = false;
	EOLFilled = false;
}

CStyleDetails& CStyleDetails::operator = (const CStyleDetails& copy)
{
	FontName = copy.FontName;
	FontSize = copy.FontSize;
	ForeColor = copy.ForeColor;
	FCString = copy.FCString;
	BackColor = copy.BackColor;
	BCString = copy.BCString;
	Bold = copy.Bold;
	Italic = copy.Italic;
	Underline = copy.Underline;
	EOLFilled = copy.EOLFilled;
	return *this;
}

void CStyleDetails::Send(CSchemeCompiler *compiler, int Style)
{	
	compiler->m_next = nrTextRec;
	compiler->m_tType = ttFontName;
	compiler->StyleSetFont(Style, FontName.c_str());
	compiler->StyleSetSize(Style, FontSize);
	compiler->StyleSetFore(Style, ForeColor);
	compiler->StyleSetBack(Style, BackColor);
	compiler->StyleSetBold(Style, Bold);
	compiler->StyleSetItalic(Style, Italic);
	compiler->StyleSetUnderline(Style, Underline);
	compiler->StyleSetEOLFilled(Style, EOLFilled);
}

/////////////////////////////////////////////////////////////////////////////////////////////
bool CSchemeCompiler::Compile(LPCTSTR filename, LPCTSTR outfile)
{
	m_ini = new CIniFile(filename);

	m_out = _tfopen(outfile, _T("wb"));

	// Write File Header...
	CompiledHdrRec hdr;
	hdr.Version = CompileVersion;
	strcpy(&hdr.Magic[0], FileID);
	fwrite(&hdr, sizeof(CompiledHdrRec), 1, m_out);

	BeginParse();

	fclose(m_out);

	delete m_ini;

	NormaliseFileTimes(filename, outfile);
	
	return true;
}

/**
 * This function uses the Windows file time APIs to make the age of
 * both files passed in equal - this is how PN knows whether to compile
 * a scheme or not. Based on code from the FileAge function.
 */
void CSchemeCompiler::NormaliseFileTimes(LPCTSTR setfrom, LPCTSTR set)
{
	HANDLE Handle;
	WIN32_FIND_DATA FindData;

	Handle = FindFirstFile(setfrom, &FindData);
	if (Handle != INVALID_HANDLE_VALUE)
	{
		FindClose(Handle);

		if ((FindData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == 0)
		{
			//FileTimeToLocalFileTime(&FindData.ftLastWriteTime, &OrigFileTime);

			HANDLE hSetFile = CreateFile(set, GENERIC_WRITE, 0, NULL, OPEN_EXISTING, /*FILE_ATTRIBUTE_NORMAL*/0, NULL);

			if(hSetFile != INVALID_HANDLE_VALUE)
			{
				SetFileTime(hSetFile, NULL, NULL, &FindData.ftLastWriteTime);
				CloseHandle(hSetFile);
			}
			else
			{
				OutputDebugString(_T("CSchemeCompiler.NormaliseFileTimes Throwing: Could not open file to set time for."));
				throw "Could not open file to set time for.";
			}

		}
	}
}

/**
 * This is called by CRecordingScintilla which we derive from when
 * any messages are passed to it.
 */

bool CSchemeCompiler::CheckNecessary(long Msg, WPARAM wParam, LPARAM lParam)
{
	//bool res = false;
	bool res = true;
	bool nOther = false;
	switch (Msg)
	{
		case SCI_STYLESETFORE:
			res = lParam != m_DefStyle.ForeColor;
			break;
		case SCI_STYLESETBACK:
			res = lParam != m_DefStyle.BackColor;
			break;
		case SCI_STYLESETBOLD:
			res = (lParam != 0) != m_DefStyle.Bold;
			break;
		case SCI_STYLESETITALIC:
			res = (lParam != 0) != m_DefStyle.Italic;
			break;
		case SCI_STYLESETUNDERLINE:
			res = (lParam != 0) != m_DefStyle.Underline;
			break;
		case SCI_STYLESETEOLFILLED:
			res = (lParam != 0) != m_DefStyle.EOLFilled;
			break;
		case SCI_STYLESETFONT:
			res = m_DefStyle.FontName != (const char*)lParam;
			break;
		case SCI_STYLESETSIZE:
			res = m_DefStyle.FontSize != lParam;
			break;
		default:
			nOther = true;
	}

	if(!nOther)
	{
		if(wParam == STYLE_DEFAULT)
			res = true;
	}
	return res;
}

void CSchemeCompiler::Record(long Msg, WPARAM wParam, LPARAM lParam)
{
	if (CheckNecessary(Msg, wParam, lParam))
	{
		char next = (char)m_next;
		fwrite(&next, sizeof(char), 1, m_out);

		if(m_next == nrMsgRec)
		{
			MsgRec msgr;
			msgr.MsgNum = Msg;
			msgr.lParam = lParam;
			msgr.wParam = wParam;
			fwrite(&msgr, sizeof(MsgRec), 1, m_out);
		}
		else
		{
			TextRec txtr;
			txtr.TextType = m_tType;
			txtr.wParam = wParam;
			txtr.lParam = 0;
			txtr.MsgNum = Msg;
			txtr.TextLength = strlen((const char*)lParam);
			fwrite(&txtr, sizeof(TextRec), 1, m_out);
			fwrite((const char*)lParam, sizeof(char), strlen((const char*)lParam), m_out);
		}

	}
	m_next = nrMsgRec;
}

bool CSchemeCompiler::BeginParse()
{
    COLORREF Col;
	m_next = nrMsgRec;
	
	SchemeHdrRec scHdr;

	if(!m_ini->ValueExists(scLexer, _T("Name")))
	{
		return false;
	}

	std::string scName = m_ini->ReadStringA(scLexer, _T("Name"), _T("none"));
	memset(&scHdr.Name[0], 0, SC_HDR_NAMESIZE);
	strcpy(&scHdr.Name[0], scName.c_str());

	scHdr.Folding = 0;

	bool bFolding = m_ini->ReadBool(scLexer, _T("Folding"));
	if(bFolding)
	{
		scHdr.Folding |= fldEnabled;
		
		bFolding = m_ini->ReadBool(scLexer, _T("CompactFolding"));
		if(bFolding)
			scHdr.Folding |= fldCompact;
		
		bFolding = m_ini->ReadBool(scLexer, _T("CommentFolding"));
		if(bFolding)
			scHdr.Folding |= fldComments;
	}

	fwrite(&scHdr, sizeof(SchemeHdrRec), 1, m_out);

	// Set the Lexer up...
    if (m_ini->ValueExists(scLexer, _T("Index")))
	{
		SetLexer( m_ini->ReadInteger(scLexer, _T("Index"), 0) );
	}
	else
	{
		std::string lexer = m_ini->ReadStringA(scLexer, _T("Lexer"), _T(""));
		if (lexer != "")
		{
			m_next = nrTextRec;
			m_tType = ttLexerLanguage;
			SetLexerLanguage(lexer.c_str());
		}
		else
			SetLexer(0);
    }

	// TODO Implement an alternative to this theApp. stuff
	
	if (m_ini->ValueExists(scLexer, _T("StyleBits")))
		SetStyleBits( m_ini->ReadInteger(scLexer, _T("StyleBits"), DefaultStyleBits) );

	if (m_ini->ValueExists(scLexer, _T("TabWidth")) && _allowTWOverride)
		SetTabWidth( m_ini->ReadInteger(scLexer, _T("TabWidth"), theApp.GetOptionsManager().TabWidth) );

	if (m_ini->ValueExists(scLexer, _T("IndentGuide")) && _allowIDOverride)
		SetIndentationGuides( m_ini->ReadBool(scLexer, _T("IndentGuide"), theApp.GetOptionsManager().ShowIndentGuides) );

	// Go for the default style...
	ParseDefaultStyle();

	// Brace Information
	if (m_ini->SectionExists(scBraces))
	{
		std::string tempstr;
		m_BracesSloppy = m_ini->ReadBool(scBraces, _T("BracesSloppy"), true);
		m_BracesStyle = m_ini->ReadInteger(scBraces, _T("BracesStyle"), 10);
		
		// ForeColor
		tempstr = m_ini->ReadStringA(scBraces, _T("ForeColor.Complete"), _T("00,00,FF"));
		Col = PNStringToColor(tempstr.c_str());
		StyleSetFore(34, Col);
		
		// BackColor
		tempstr = m_ini->ReadString(scBraces, _T("ForeColor.Incomplete"), _T("FF,00,00"));
		Col = PNStringToColor(tempstr.c_str());
		StyleSetFore(35, Col);

		StyleSetBold(34, m_ini->ReadBool(scBraces, _T("FontBold.Complete"), true));
		StyleSetBold(35, m_ini->ReadBool(scBraces, _T("FontBold.Incomplete"), true));

		StyleSetItalic(34, m_ini->ReadBool(scBraces, _T("FontItalic.Complete"), false));
		StyleSetItalic(35, m_ini->ReadBool(scBraces, _T("FontItalic.Incomplete"), false));

		StyleSetUnderline(34, m_ini->ReadBool(scBraces, _T("FontUnderline.Complete"), false));
		StyleSetUnderline(35, m_ini->ReadBool(scBraces, _T("FontUnderline.Incomplete"), false));
	}

	m_SchemeOffset = 0;
	int n = 0;
	int SchemeMax = m_ini->ReadInteger(scLexer, _T("Styles"), 0);

	TCHAR sKeyName[100];

	while (n <= (SchemeMax - 1))
	{
		_stprintf(&sKeyName[0], _T("%s%d"), sStyles, n);
		ParseStyle(&sKeyName[0], n);
		if (m_SchemeOffset > 0)
		{
			n = n + m_SchemeOffset;
			m_SchemeOffset = 0;
		}
		else
			n++;
    }

	for (int i = 0; i <= 5; i++)
	{
		_stprintf(&sKeyName[0], _T("%s%d"), sKeywords, i);
		ParseKeyWords(&sKeyName[0], i);
	}
	return true;
}

void CSchemeCompiler::ParseDefaultStyle()
{
	///@todo Defaults should be set here from interface settings...
	
	m_DefStyle.FontName = m_ini->ReadStringA(sDefStyle, _T("FontName"), _T("Courier New"));
	m_DefStyle.FontSize = m_ini->ReadInteger(sDefStyle, _T("FontSize"), 10);
	
	m_DefStyle.FCString = m_ini->ReadString(sDefStyle, _T("ForeColor"), _T("00.00.00"));
	m_DefStyle.ForeColor = PNStringToColor(m_DefStyle.FCString.c_str());
	
	m_DefStyle.BCString = m_ini->ReadString(sDefStyle, _T("BackColor"), _T("ff.ff.ff"));
	m_DefStyle.BackColor = PNStringToColor(m_DefStyle.BCString.c_str());
	
	m_DefStyle.Bold = m_ini->ReadBool(sDefStyle, _T("FontBold"), false);
	m_DefStyle.Italic = m_ini->ReadBool(sDefStyle, _T("FontItalic"), false);;
	m_DefStyle.Underline = m_ini->ReadBool(sDefStyle, _T("FontUnderline"), false);
	m_DefStyle.EOLFilled = m_ini->ReadBool(sDefStyle, _T("EOLFilled"), false);

	// Set up the default style.
	m_next = nrTextRec;
	m_tType = ttFontName;
	StyleSetFont(STYLE_DEFAULT, m_DefStyle.FontName.c_str());
	StyleSetSize(STYLE_DEFAULT, m_DefStyle.FontSize);
	StyleSetFore(STYLE_DEFAULT, m_DefStyle.ForeColor);
	StyleSetBack(STYLE_DEFAULT, m_DefStyle.BackColor);
	StyleSetBold(STYLE_DEFAULT, m_DefStyle.Bold);
	StyleSetItalic(STYLE_DEFAULT, m_DefStyle.Italic);
	StyleSetUnderline(STYLE_DEFAULT, m_DefStyle.Underline);
	// Make sure we apply the default bits to all of the styles to be applied.
	StyleClearAll();
}

void CSchemeCompiler::ParseStyle(LPCTSTR scS, int Style, bool CanSend)
{
	int ofs;  
	m_NextStyle = m_DefStyle;

	// Check if we're jumping to a new start point for scheme reading.
	ofs = m_ini->ReadInteger(scS, sNewBase, -1);
	if (ofs != -1)
	{
		m_SchemeOffset = ofs - Style;
		return;
	}

	// Group switching...
	iniString ssGroup(m_ini->ReadString(scS, sGroup, _T("")));
	if (ssGroup != _T(""))
	{
		TCHAR tGroup[50];
		_stprintf(&tGroup[0], _T("%s%s"), sStyles, ssGroup.c_str());
		ParseStyle(&tGroup[0], Style);
		return;
	}

	// This allows a style to be based upon another style.
	iniString ssBase(m_ini->ReadString(scS, sBase, _T("")));
	if (ssBase != _T(""))
	{
		TCHAR tBase[50];
		_stprintf(&tBase[0], _T("%s%s"), sStyles, ssBase.c_str());
		ParseStyle(&tBase[0], Style, false);
	}

	if (m_ini->ValueExists(scS, _T("FontName")))
		m_NextStyle.FontName = m_ini->ReadString(scS, _T("FontName"), m_DefStyle.FontName.c_str());
	
	if (m_ini->ValueExists(scS, _T("FontSize")))
		m_NextStyle.FontSize = m_ini->ReadInteger(scS, _T("FontSize"), m_DefStyle.FontSize);

	if (m_ini->ValueExists(scS, _T("ForeColor")))
	{
		iniString fclr = m_ini->ReadString(scS, _T("ForeColor"), m_DefStyle.FCString.c_str());
		m_NextStyle.ForeColor = PNStringToColor(fclr.c_str());
	}

	if (m_ini->ValueExists(scS, _T("BackColor")))
	{
		iniString bclr = m_ini->ReadString(scS, _T("BackColor"), m_DefStyle.BCString.c_str());
		m_NextStyle.BackColor = PNStringToColor(bclr.c_str());
	}

	if (m_ini->ValueExists(scS, _T("FontBold")))
		m_NextStyle.Bold = m_ini->ReadBool(scS, _T("FontBold"), m_DefStyle.Bold);

	if (m_ini->ValueExists(scS, _T("FontItalic")))
		m_NextStyle.Italic = m_ini->ReadBool(scS, _T("FontItalic"), m_DefStyle.Italic);

	if (m_ini->ValueExists(scS, _T("FontUnderline")))
		m_NextStyle.Underline = m_ini->ReadBool(scS, _T("FontUnderline"), m_DefStyle.Underline);

	if (m_ini->ValueExists(scS, _T("EolFilled")))
		m_NextStyle.EOLFilled = m_ini->ReadBool(scS, _T("EolFilled"), m_DefStyle.EOLFilled);

	if(CanSend)
	{
		m_NextStyle.Send(this, Style);
	}
}

std::string CSchemeCompiler::ParseKeyWords(LPCTSTR Section, int KId)
{
	// s and s2 are used to build up section names to read.
	iniString s;
	iniString s2;
	int			n;
	int			j;
	// words are the actual words...
	std::string	words;
	std::string Result("");
  
	if (m_ini->SectionExists(Section))
	{
		words = "";
		s = Section;

		// Linked Sections
		n = m_ini->ReadInteger(s.c_str(), _T("Links"), 0);
		if (n > 0)
		{
			for (j = 1; j <= n; j++)
			{
				if (words.size() > 0)
				{
					words = words + " ";
				}

				s2 = sKeywords;
				TCHAR tLink[50];
				_stprintf(&tLink[0], _T("%s%d"), _T("Link"), j);
				s2 += m_ini->ReadString(s.c_str(), &tLink[0], _T(""));

				words = words + ParseKeyWords(s2.c_str(), -1);
			}
		}

		n = m_ini->ReadInteger(s.c_str(), _T("Sets"), 0);

		if (n > 0)
		{
			for (j = 1; j <= n; j++)
			{
				if (words.size() > 0)
				{
					words += " ";
				}
				TCHAR tWordsSec[50];
				_stprintf(&tWordsSec[0], _T("%s%d"), _T("Words"), j);
				words += m_ini->ReadStringA(s.c_str(), &tWordsSec[0], _T("error"));
			}
		}
		else
		{
			if( words.size() == 0)
			{
				words = m_ini->ReadStringA(s.c_str(), _T("Words"), _T(""));
			}
		}

		if (KId != -1)
		{
			m_next = nrTextRec;
			m_tType = ttKeywords;
			SetKeyWords(KId, words.c_str());
			Result = "";
		}
		else
			Result = words;
	}
	else
		Result = "";
    
	return Result;
}