/**
 * @file SchemeCompiler.cpp
 * @brief Implement scheme reader and compiler classes.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * Unicode Status: Unicode Ready (untested).
 */

#include "stdafx.h"
#include "SchemeCompiler.h"
#include "ssreg.h"

using namespace ssreg;

////////////////////////////////////////////////////////////
// Useful Functions
////////////////////////////////////////////////////////////

static int chartoval(TCHAR inp)
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

static COLORREF PNStringToColor(LPCTSTR input)
{
  	LPCTSTR	Part;
	int		res;
	int		Result;

	//b,g,r - output format in hex $bbggrr
	//r,g,b - input format in string, rr,gg,bb
	// Default colour
	Result = ::GetSysColor(COLOR_WINDOWTEXT);
	// only works for xxxxxx colours...
	if (_tcslen(input) != 6)
	{
		return Result;
	}
	
	Part = input;
	res = (0x10 * chartoval(Part[0]));
	if (Part[1] != _T('0'))
	{
		res = res + (chartoval(Part[1]));
	}
	Part += 2;
	res += (0x1000 * chartoval(Part[0]));
	res += (0x100 * chartoval(Part[1]));
	Part += 2;
	res += (0x100000 * chartoval(Part[0]));
	res += (0x10000 * chartoval(Part[1]));
	Result = res;

	return Result;
}

static bool PNStringToBool(LPCTSTR input)
{
	return (input[0] == 'T' || input[0] == 't');
}

////////////////////////////////////////////////////////////
// SchemeRecorder Implementation
////////////////////////////////////////////////////////////

SchemeRecorder::SchemeRecorder() : CScintilla()
{
	m_out = NULL;
	m_next = nrMsgRec;
}

bool SchemeRecorder::StartRecording(LPCTSTR scheme, LPCTSTR title, LPCTSTR outfile, int FoldFlags)
{
	PNASSERT(m_out == NULL);

	m_out = _tfopen(outfile, _T("wb"));

	// Write File Header...
	CompiledHdrRec hdr;
	hdr.Version = CompileVersion;
	strcpy(&hdr.Magic[0], FileID);
	fwrite(&hdr, sizeof(CompiledHdrRec), 1, m_out);

	WriteHeader(scheme, title, FoldFlags);
	
	return true;
}

void SchemeRecorder::WriteHeader(LPCTSTR schemename, LPCTSTR schemetitle, int FoldFlags)
{
	USES_CONVERSION;

	SchemeHdrRec scHdr;

	memset(&scHdr.Name[0], 0, SC_HDR_NAMESIZE);
	strcpy(&scHdr.Name[0], T2CA(schemename));

	if(schemetitle != NULL)
	{
		memset(&scHdr.Title[0], 0, SC_HDR_TITLESIZE);
		strcpy(&scHdr.Title[0], T2CA(schemetitle));
	}

	scHdr.Folding = FoldFlags;

	fwrite(&scHdr, sizeof(SchemeHdrRec), 1, m_out);
}

bool SchemeRecorder::EndRecording()
{
	if(m_out)
	{
		fclose(m_out);
		m_out = NULL;
		return true;
	}
	else return false;
}

long SchemeRecorder::SPerform(long Msg, WPARAM wParam, LPARAM lParam)
{
	if(Msg == SCI_STYLESETFONT)
	{
		m_next = nrTextRec;
		m_tType = ttFontName;
	}
	else if(Msg == SCI_SETLEXERLANGUAGE)
	{
		m_next = nrTextRec;
		m_tType = ttLexerLanguage;
	}
	else if(Msg == SCI_SETKEYWORDS)
	{
		m_next = nrTextRec;
		m_tType = ttKeywords;
	}
	Record(Msg, wParam, lParam);
	return 0;
}

bool SchemeRecorder::CheckNecessary(long Msg, WPARAM wParam, LPARAM lParam)
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
			res = (m_DefStyle.FontName != (const char*)lParam);
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

void SchemeRecorder::Record(long Msg, WPARAM wParam, LPARAM lParam)
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

void SchemeRecorder::SetDefStyle(StyleDetails* defaults)
{
	m_DefStyle = *defaults;
}

////////////////////////////////////////////////////////////
// SchemeCompiler Implementation
////////////////////////////////////////////////////////////

void SchemeCompiler::Compile(LPCTSTR path, LPCTSTR outpath, LPCTSTR mainfile)
{
	XMLParserCallback<SchemeCompiler> callback(*this, startElement, endElement, characterData);
	
	XMLParser parser;
	parser.SetParseState(&callback);
	
	callback.SetUserData((void*)&m_LoadState);

	m_LoadState.m_pParser = &parser;

	m_LoadState.m_csBasePath = path;
	m_LoadState.m_csOutPath = outpath;

	CSRegistry reg;
	reg.OpenKey(_T("Software\\Echo Software\\PN2\\SchemeDates"), true);
	ctcString filepart;

	CString csFile = path;
	csFile += mainfile;
	m_LoadState.m_IncludeFiles.insert(m_LoadState.m_IncludeFiles.end(), csFile);
	
	for(CSTRING_LIST::iterator i = m_LoadState.m_IncludeFiles.begin(); i != m_LoadState.m_IncludeFiles.end(); ++i)
	{
		m_LoadState.m_State = 0;
		CString& file = (*i);

		try
		{
			parser.LoadFile(file);
			
			CFileName fn(file);
			fn.GetFileName(filepart);
			reg.WriteInt(filepart.c_str(), FileAge(file));
		}
		catch (CSchemeCompilerException& E)
		{
			CString err;
			err.Format(_T("Error Parsing Scheme XML: %s\n (file: %s, line: %d, column %d)"), 
				E.GetMessage(), E.GetFileName(), E.GetLine(), E.GetColumn());
			
			OutputDebugString(err);
		}
		catch (XMLParserException& E)
		{
			CString err;
			err.Format(_T("Error Parsing Scheme XML: %s\n (file: %s, line: %d, column %d)"), 
				XML_ErrorString(E.GetErrorCode()), E.GetFileName(), E.GetLine(), E.GetColumn());
			
			OutputDebugString(err);
		}

		
		parser.Reset();
	}
}

void SchemeCompiler::processGlobal(CSchemeLoaderState* pState, XMLAttributes& atts)
{
	for(int i = 0; i < atts.getCount(); i++)
	{
		if(_tcscmp(atts.getName(i), _T("name")) == 0)
		{
			pState->m_csGName = atts.getValue(i);
			pState->m_State = DOING_GLOBAL;
			pState->m_csCData = _T("");
			break;
		}
	}
}

void SchemeCompiler::processKeywordClass(CSchemeLoaderState* pState, XMLAttributes& atts)
{
	for(int i = 0; i < atts.getCount(); i++)
	{
		if(_tcscmp(atts.getName(i), _T("name")) == 0)
		{
			pState->m_csGName = atts.getValue(i);
			pState->m_State = DOING_KEYWORDS;
			pState->m_csCData = _T("");
			break;
		}
	}
}

void SchemeCompiler::parseStyle(CSchemeLoaderState* pState, XMLAttributes& atts, StyleDetails* pStyle)
{
	LPCTSTR nm;
	CString t;
	CString valname;
	int x = atts.getCount();

	// need to add global substitution.
	for(int i = 0; i < x; i++)
	{
		nm = atts.getName(i);
		t = atts.getValue(i);
		
		valname = nm;

		if(valname == _T("name"))
			continue;

		CSTRING_MAP::iterator it = pState->m_Globals.find(t);
		if(it != pState->m_Globals.end())
		{
			t = (*it).second;
		}
		else
		{
			CString todebug;
			todebug.Format(_T("No match for: %s=%s\n"), nm, t);
			OutputDebugString(todebug);
		}

		if(valname == _T("fore"))
		{
			pStyle->ForeColor = PNStringToColor(t);
		}
		else if(valname == _T("back"))
		{
			pStyle->BackColor = PNStringToColor(t);
		}
		else if(valname == _T("font"))
		{
			USES_CONVERSION;
			pStyle->FontName = T2CA((LPCTSTR)t);
		}
		else if(valname == _T("size"))
		{
			pStyle->FontSize = _ttoi(t);
		}
		else if(valname == _T("italics"))
		{
			pStyle->Italic = PNStringToBool(t);
		}
		else if(valname == _T("bold"))
		{
			pStyle->Bold = PNStringToBool(t);
		}
		else if(valname == _T("underline"))
		{
			pStyle->Underline = PNStringToBool(t);
		}
		else if(valname == _T("eolfilled"))
		{
			pStyle->EOLFilled = PNStringToBool(t);
		}
	}
}

void SchemeCompiler::sendStyle(StyleDetails* s, SchemeRecorder* compiler)
{
	compiler->StyleSetFont(s->Key, s->FontName.c_str());
	compiler->StyleSetSize(s->Key, s->FontSize);
	if(s->ForeColor != -1)
		compiler->StyleSetFore(s->Key, s->ForeColor);
	if(s->BackColor != -1)
		compiler->StyleSetBack(s->Key, s->BackColor);
	compiler->StyleSetBold(s->Key, s->Bold);
	compiler->StyleSetItalic(s->Key, s->Italic);
	compiler->StyleSetUnderline(s->Key, s->Underline);
	compiler->StyleSetEOLFilled(s->Key, s->EOLFilled);

	if(s->Key == STYLE_DEFAULT)
		compiler->StyleClearAll();
}

void SchemeCompiler::processStyleClass(CSchemeLoaderState* pState, XMLAttributes& atts)
{
	//<style-class name="comment" inherit-style="bold" fore="comment-color"/>
	// fore, back, font, size, italics, bold, underline, eolfilled
	
	pState->m_State = DOING_STYLEC;

	LPCTSTR t = atts.getValue(_T("name"));
	StyleDetails* pStyle = NULL;
	if(t != NULL)
	{
		CString name(t);
		if(name == _T("default"))
		{
			//special case default class...
			pStyle = &pState->m_Default;
		}
		else
		{
			pStyle = new StyleDetails(pState->m_Default);
			pState->m_StyleClasses.insert(pState->m_StyleClasses.end(), STYLEDETAILS_NAMEMAP::value_type(name, pStyle));
		}

		t = atts.getValue(_T("inherit-style"));
		if(t != NULL)
		{
			CString inh(t);
			SDNM_IT i = pState->m_StyleClasses.find(inh);
			if(i != pState->m_StyleClasses.end())
				*pStyle = *((*i).second);
		}

		parseStyle(pState, atts, pStyle);

		
	}
}

void SchemeCompiler::processLanguageStyle(CSchemeLoaderState* pState, XMLAttributes& atts)
{
	CString classname = atts.getValue(_T("class"));
	StyleDetails* pBase = &pState->m_Default;

	if(classname != _T("default"))
	{
		SDNM_IT i = pState->m_StyleClasses.find(classname);
		if(i != pState->m_StyleClasses.end())
			pBase = (*i).second;
	}

	StyleDetails Style(*pBase);

	parseStyle(pState, atts, &Style);

	LPCTSTR key = atts.getValue(_T("key"));
	Style.Key = _ttoi(key);

	if(Style.Key == STYLE_DEFAULT)
	{
		m_Recorder.SetDefStyle(&pState->m_Default);
	}

	sendStyle(&Style, &m_Recorder);	
}

void SchemeCompiler::processLanguageKeywords(CSchemeLoaderState* pState, XMLAttributes& atts)
{
	 //<keyword key="0" class="hypertext"/>
	int x = atts.getCount();
	CString name = _T("");
	CString val = _T("");
	CString kw = _T("");
	int key = -1;

	for(int i = 0; i < x; i++)
	{
		name = atts.getName(i);
		val = atts.getValue(i);

		if(name == _T("key"))
		{
			key = _ttoi(val);
		}
		else if(name == _T("class"))
		{
			CSTRING_MAP::iterator z = pState->m_Keywords.find(val);
			if(z != pState->m_Keywords.end())
			{
				// we have some keywords...
				kw = (*z).second;
			}
		}
	}

	if(kw != _T("") && key != -1)
	{
		USES_CONVERSION;
		m_Recorder.SetKeyWords(key, T2CA((LPCTSTR)kw));
	}
}

void SchemeCompiler::processLanguageElement(CSchemeLoaderState* pState, LPCTSTR name, XMLAttributes& atts)
{
	LPCTSTR t = NULL;

	if(pState->m_State == DOING_LANGUAGE && _tcscmp(name, _T("language")) == 0)
	{
		LPCTSTR scheme = atts.getValue(_T("name"));
		LPCTSTR title = atts.getValue(_T("title"));
		if(scheme != NULL)
		{
			CString filename(pState->m_csOutPath);
			filename += scheme;
			filename += ".cscheme";
			
			int foldflags = 0;
			
			t = atts.getValue(_T("folding"));
			if(t != NULL && PNStringToBool(t))
			{
				//fldEnabled = 1, fldCompact = 2, fldComments = 4
				foldflags = fldEnabled;
				
				t = atts.getValue(_T("foldcompact"));
				if(t != NULL && PNStringToBool(t))
					foldflags |= fldCompact;

				t = atts.getValue(_T("foldcomments"));
				if(t != NULL && PNStringToBool(t))
					foldflags |= fldComments;
			}

			m_Recorder.StartRecording(scheme, title, filename, foldflags);
			
			m_Recorder.SetDefStyle(&pState->m_Default);
			pState->m_State = DOING_LANGUAGE_DETAILS;
		}
	}
	else
	{
		ATLASSERT(pState->m_State == DOING_LANGUAGE_DETAILS);

		if(_tcscmp(name, _T("lexer")) == 0)
		{
			t = atts.getValue(_T("name"));
			if(t != NULL)
			{
				USES_CONVERSION;
				m_Recorder.SetLexerLanguage(T2A((LPTSTR)t));
			}
			t = atts.getValue(_T("stylebits"));
			if(t != NULL)
			{
				int a = _ttoi(t);
				if(a != 0)
				{
					m_Recorder.SetStyleBits(a);
				}
				else
					throw CSchemeCompilerException(pState->m_pParser, _T("Style Bits value not valid (0 or non-numeric)"));
				
			}
		}
		else if(_tcscmp(name, _T("use-keywords")) == 0)
		{
			pState->m_State = DOING_LANGUAGE_KW;
		}
		else if(_tcscmp(name, _T("use-styles")) == 0)
		{
			pState->m_State = DOING_LANGUAGE_STYLES;
		}
	}
}

void SchemeCompiler::specifyImportFile(CSchemeLoaderState* pState, XMLAttributes& atts)
{
	LPCTSTR name = atts.getValue(_T("name"));
	if(name != NULL)
	{
		CString filename = pState->m_csBasePath;
		filename += name;
		pState->m_IncludeFiles.insert(pState->m_IncludeFiles.end(), filename);
	}
	else
	{
		throw CSchemeCompilerException(pState->m_pParser, _T("Import element with no name attribute."));
	}
}

void SchemeCompiler::specifyImportSet(CSchemeLoaderState* pState, XMLAttributes& atts)
{
	LPCTSTR pattern = atts.getValue(_T("pattern"));

	if(pattern != NULL)
	{
		HANDLE hFind;
		WIN32_FIND_DATA FindFileData;

		CString sPattern = pState->m_csBasePath;
		sPattern += pattern;
		
		CString toAdd;

		hFind = FindFirstFile(sPattern, &FindFileData);
		if (hFind != INVALID_HANDLE_VALUE) 
		{
			BOOL found = TRUE;
			CString filename;

			while (found)
			{
				if(_tcsicmp(FindFileData.cFileName, _T("master.scheme")) != 0)
				{
					toAdd = pState->m_csBasePath;
					toAdd += FindFileData.cFileName;

					pState->m_IncludeFiles.insert(pState->m_IncludeFiles.end(), toAdd);
				}

				found = FindNextFile(hFind, &FindFileData);
			}

			FindClose(hFind);
		}
	}
	else
	{
		throw CSchemeCompilerException(pState->m_pParser, _T("Set element with no pattern attribute."));
	}
}

void SchemeCompiler::processKeywordCombine(CSchemeLoaderState* pState, XMLAttributes& atts)
{
	pState->m_State = DOING_KEYWORDCOMBINE;

	LPCTSTR name = atts.getValue(_T("name"));
	if(name != NULL)
	{
		CSTRING_MAP::iterator z = pState->m_Keywords.find(CString(name));
		if(z != pState->m_Keywords.end())
		{
			pState->m_csCData += (*z).second;
		}
		else
		{
			throw CSchemeCompilerException(pState->m_pParser, _T("Unmatched keyword class inclusion."));
		}
	}
	else
	{
		throw CSchemeCompilerException(pState->m_pParser, _T("include-class element with no name attribute"));
	}
}

void SchemeCompiler::startElement(void *userData, LPCTSTR name, XMLAttributes& atts)
{
	CSchemeLoaderState* pState = static_cast<CSchemeLoaderState*>(userData);
	int state = pState->m_State;

	CString stattext;

	if(state == DOING_GLOBALS && _tcscmp(name, _T("value")) == 0)
	{
		processGlobal(pState, atts);
	}
	else if(state == DOING_KEYWORDC && _tcscmp(name, _T("keyword-class")) == 0)
	{
		processKeywordClass(pState, atts);
	}
	else if(state == DOING_KEYWORDS && _tcscmp(name, _T("include-class")) == 0)
	{
		processKeywordCombine(pState, atts);
	}
	else if(state == DOING_STYLECS && _tcscmp(name, _T("style-class")) == 0)
	{
		processStyleClass(pState, atts);
	}
	else if(state == DOING_LANGUAGE || state == DOING_LANGUAGE_DETAILS)
	{
		processLanguageElement(pState, name, atts);
	}
	else if(state == DOING_LANGUAGE_STYLES)
	{
		processLanguageStyle(pState, atts);
	}
	else if(state == DOING_LANGUAGE_KW)
	{
		processLanguageKeywords(pState, atts);
	}
	else if(state == DOING_IMPORTS)
	{
		if(_tcscmp(name, _T("set")) == 0)
		{
			specifyImportSet(pState, atts);
		}
		else
		{
			specifyImportFile(pState, atts);
		}
	}
	else if(_tcscmp(name, _T("globals")) == 0)
	{		
		stattext.Format(_T("Processing Globals\r\n"));
		pState->m_State = DOING_GLOBALS;
	}
	else if(_tcscmp(name, _T("keyword-classes")) == 0)
	{
		stattext.Format(_T("Processing Keyword Classes\r\n"));
		pState->m_State = DOING_KEYWORDC;
	}
	else if(_tcscmp(name, _T("style-classes")) == 0)
	{
		stattext.Format(_T("Processing Style Classes\r\n"));
		pState->m_State = DOING_STYLECS;
	}
	else if(_tcscmp(name, _T("language")) == 0)
	{
		stattext.Format(_T("Processing Language\r\n"));
		pState->m_State = DOING_LANGUAGE;
		processLanguageElement(pState, name, atts);
	}
	else if(_tcscmp(name, _T("imports")) == 0)
	{
		stattext.Format(_T("Processing Import Specs\r\n"));
		pState->m_State = DOING_IMPORTS;
	}
	else
	{
		stattext.Format(_T("Start Element: %s (%d attributes)\r\n"), name, atts.getCount());
	}

	if(stattext.GetLength() > 0)
		OutputDebugString(stattext);
}

void SchemeCompiler::endElement(void *userData, LPCTSTR name)
{
	CSchemeLoaderState* pS = static_cast<CSchemeLoaderState*>(userData);
	
	int state = pS->m_State;

	CString stattext;

	if(state == DOING_GLOBAL)
	{
		// Add a global...
		if(pS->m_csCData.GetLength() > 0)
		{
			CString global = pS->m_csCData;

			// Now to match global back in with the globals...
			CSTRING_MAP::iterator it = pS->m_Globals.find(global);
			if(it != pS->m_Globals.end())
			{
				global = (*it).second;
			}
			else
			{
				CString todebug;
				todebug.Format(_T("No match for: %s\n"), global);
				OutputDebugString(todebug);
			}

			stattext.Format(_T("Added Global: %s = %s\r\n"), pS->m_csGName, global);
			OutputDebugString(stattext);

			pS->m_Globals.insert(pS->m_Globals.end(), CSTRING_MAP::value_type(pS->m_csGName, global));
		}

		pS->m_State = DOING_GLOBALS;
	}
	else if(state == DOING_STYLEC)
	{
		pS->m_State = DOING_STYLECS;
	}
	else if(state == DOING_KEYWORDS)
	{
		if(pS->m_csCData.GetLength() > 0)
		{
			// 1: Remove #13.
			// 2: Replace #10 with " ".
			// 3: Replace #9 with " ".
			// 4: Compress Spaces.
			// 5: Trim left and right.
			pS->m_csCData.Replace(_T("\r"), _T(""));
			pS->m_csCData.Replace(_T("\n"), _T(" "));
			pS->m_csCData.Replace(_T("\t"), _T(" "));
			while (pS->m_csCData.Replace(_T("  "), _T(" ")));
			pS->m_csCData.TrimLeft(_T(' '));
			pS->m_csCData.TrimRight(_T(' '));

			pS->m_Keywords.insert(pS->m_Keywords.end(), CSTRING_MAP::value_type(pS->m_csGName, pS->m_csCData));

			stattext.Format(_T("Added Keyword Class: %s\r\n"), pS->m_csGName);
			OutputDebugString(stattext);
		}

		pS->m_State = DOING_KEYWORDC;
	}
	else if(state == DOING_LANGUAGE || state == DOING_LANGUAGE_DETAILS)
	{
		// Only come out of language mode if we're really out...
		if(_tcscmp(name, _T("language")) == 0)
		{
			if (m_Recorder.IsRecording())
				m_Recorder.EndRecording();
			pS->m_State = 0;
		}
	}
	else if(state == DOING_LANGUAGE_KW || state == DOING_LANGUAGE_STYLES)
	{
		if((_tcscmp(name, _T("use-keywords")) == 0) || (_tcscmp(name, _T("use-styles")) == 0))
			pS->m_State = DOING_LANGUAGE_DETAILS;
	}
	else if(state == DOING_KEYWORDCOMBINE)
	{
		pS->m_State = DOING_KEYWORDS;
	}
	else
	{
		pS->m_State = 0;
	}
	
	// Clear character data after every tag end...
	if(state != DOING_KEYWORDCOMBINE)
		pS->m_csCData = _T("");
}

void SchemeCompiler::characterData(void* userData, LPCTSTR data, int len)
{
	CSchemeLoaderState* pState = static_cast<CSchemeLoaderState*>(userData);

	CString cdata;
	TCHAR* buf = cdata.GetBuffer(len+1);
	_tcsncpy(buf, data, len);
	buf[len] = 0;
	cdata.ReleaseBuffer();

	pState->m_csCData += cdata;
}