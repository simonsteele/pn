/**
 * @file SchemeParser.cpp
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
#include "files.h"
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

	//b,g,r - output format in hex $bbggrr
	//r,g,b - input format in string, rr,gg,bb
	// Default colour
	res = ::GetSysColor(COLOR_WINDOWTEXT);
	// only works for xxxxxx colours...
	if (_tcslen(input) != 6)
	{
		return res;
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

	return res;
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
// CSchemeLoaderState Implementation
////////////////////////////////////////////////////////////

CSchemeLoaderState::~CSchemeLoaderState()
{
	for(CNM_IT j = m_CustomSchemes.begin(); j != m_CustomSchemes.end(); ++j)
	{
		delete (*j).second;
	}
}

////////////////////////////////////////////////////////////
// UserSettingsParser Implementation
////////////////////////////////////////////////////////////

UserSettingsParser::UserSettingsParser()
{
	pScheme = NULL;
}

void UserSettingsParser::Parse(LPCTSTR path, CSchemeLoaderState* pState)
{
	CSRegistry reg;
	reg.OpenKey(_T("Software\\Echo Software\\PN2\\SchemeDates"), true);
	
	if(FileExists(path))
	{
		reg.WriteInt(_T("UserSettings"),  FileAge(path));
	}
	else
	{
		reg.DeleteValue(_T("UserSettings"));
		return;
	}

	XMLParserCallback<UserSettingsParser> callback(*this, startElement, endElement, characterData);

	XMLParser parser;
	parser.SetParseState(&callback);
	
	callback.SetUserData((void*)pState);

	pState->m_pParser = &parser;

	pState->m_State = 0;

	try
	{
		parser.LoadFile(path);
	}
	catch (CSchemeParserException& E)
	{
		CString err;
		err.Format(_T("Error Parsing Scheme UserSettings XML: %s\n (file: %s, line: %d, column %d)"), 
			E.GetMessage(), E.GetFileName(), E.GetLine(), E.GetColumn());
		
		OutputDebugString(err);
	}
	catch (XMLParserException& E)
	{
		CString err;
		err.Format(_T("Error Parsing Scheme UserSettings XML: %s\n (file: %s, line: %d, column %d)"), 
			XML_ErrorString(E.GetErrorCode()), E.GetFileName(), E.GetLine(), E.GetColumn());
		
		OutputDebugString(err);
	}
}

void UserSettingsParser::characterData(void* userData, LPCTSTR data, int len)
{
	CSchemeLoaderState* pState = static_cast<CSchemeLoaderState*>(userData);

	if(pState->m_State == US_KEYWORDS)
	{
		CString cdata;
		TCHAR* buf = cdata.GetBuffer(len+1);
		_tcsncpy(buf, data, len);
		buf[len] = 0;
		cdata.ReleaseBuffer();

		pState->m_csCData += cdata;
	}
}

void UserSettingsParser::startElement(void *userData, LPCTSTR name, XMLAttributes& atts)
{
	CSchemeLoaderState* pState = static_cast<CSchemeLoaderState*>(userData);
	int state = pState->m_State;

	if(state == US_SCHEMES && (_tcscmp(name, _T("scheme")) == 0))
	{
		processScheme(pState, atts);
	}
	else if(state == US_SCHEME || state == US_KEYWORD_OVERRIDES || state == US_STYLE_OVERRIDES)
	{
		processSchemeElement(pState, name, atts);
	}
	else if(state == US_CLASSES)
	{
		processClassElement(pState, name, atts);
	}
	else if(_tcscmp(name, _T("schemes")) == 0)
	{
		pState->m_State = US_SCHEMES;
	}
	else if(_tcscmp(name, _T("override-classes")) == 0)
	{
		pState->m_State = US_CLASSES;
	}
}

void UserSettingsParser::endElement(void *userData, LPCTSTR name)
{
	CSchemeLoaderState* pState = static_cast<CSchemeLoaderState*>(userData);
	int state = pState->m_State;

	if(state == US_KEYWORDS && (_tcscmp(name, _T("keywords")) == 0))
	{
		pState->m_csCData.Replace(_T("\r"), _T(""));
		pState->m_csCData.Replace(_T("\n"), _T(" "));
		pState->m_csCData.Replace(_T("\t"), _T(" "));
		while (pState->m_csCData.Replace(_T("  "), _T(" ")));
		pState->m_csCData.TrimLeft(_T(' '));
		pState->m_csCData.TrimRight(_T(' '));

		if(pState->m_csCData.GetLength() > 0)
		{
			CustomKeywordSet* pSet = new CustomKeywordSet;
			pSet->key = m_idval;
			pSet->pName = NULL;
			pSet->pWords = new TCHAR[pState->m_csCData.GetLength()+1];
			_tcscpy(pSet->pWords, (LPCTSTR)pState->m_csCData);
			pScheme->AddKeywordSet(pSet);
		}

		pState->m_State = US_KEYWORD_OVERRIDES;
	}
	else if(state == US_SCHEME && (_tcscmp(name, _T("scheme")) == 0))
	{
		pState->m_CustomSchemes.insert(pState->m_CustomSchemes.begin(), CUSTOMISED_NAMEMAP::value_type(m_SchemeName, pScheme));
		pScheme = NULL;

		pState->m_State = US_SCHEMES;
	}
	else if(state == US_SCHEMES && (_tcscmp(name, _T("schemes")) == 0))
	{
		pState->m_State = 0;
	}
	else if(state == US_KEYWORD_OVERRIDES && (_tcscmp(name, _T("override-keywords")) == 0))
	{
		pState->m_State = US_SCHEME;
	}
	else if(state == US_STYLE_OVERRIDES && (_tcscmp(name, _T("override-styles")) == 0))
	{
		pState->m_State = US_SCHEME;
	}
	else if(state == US_CLASSES && (_tcscmp(name, _T("override-classes")) == 0))
	{
		pState->m_State = 0;
	}

	pState->m_csCData = _T("");
}

void UserSettingsParser::DefineStyle(StyleDetails* pStyle, XMLAttributes atts)
{
	int c = atts.getCount();
			
	for(int i = 0; i < c; i++)
	{
		LPCTSTR aname = atts.getName(i);
	
		if(_tcscmp(aname, _T("key")) == 0)
		{
			pStyle->Key = _ttoi(atts.getValue(i));
		}
		if(_tcscmp(aname, _T("font")) == 0)
		{
			pStyle->FontName = atts.getValue(i);
			pStyle->values |= edvFontName;
		}
		else if(_tcscmp(aname, _T("size")) == 0)
		{
			pStyle->FontSize = _ttoi(atts.getValue(i));
			pStyle->values |= edvFontSize;
		}
		else if(_tcscmp(aname, _T("fore")) == 0)
		{
			pStyle->ForeColor = PNStringToColor(atts.getValue(i));
			pStyle->values |= edvForeColor;
		}
		else if(_tcscmp(aname, _T("back")) == 0)
		{
			pStyle->BackColor = PNStringToColor(atts.getValue(i));
			pStyle->values |= edvBackColor;
		}
		else if(_tcscmp(aname, _T("bold")) == 0)
		{
			pStyle->Bold = PNStringToBool(atts.getValue(i));
			pStyle->values |= edvBold;
		}
		else if(_tcscmp(aname, _T("italic")) == 0)
		{
			pStyle->Italic = PNStringToBool(atts.getValue(i));
			pStyle->values |= edvItalic;
		}
		else if(_tcscmp(aname, _T("underline")) == 0)
		{
			pStyle->Underline = PNStringToBool(atts.getValue(i));
			pStyle->values |= edvUnderline;
		}
		else if(_tcscmp(aname, _T("eolfilled")) == 0)
		{
			pStyle->EOLFilled = PNStringToBool(atts.getValue(i));
			pStyle->values |= edvEOLFilled;
		}
		else if(_tcscmp(aname, _T("class")) == 0)
		{
			pStyle->classname = atts.getValue(i);
			pStyle->values |= edvClass;
		}
		else if(_tcscmp(aname, _T("name")) == 0)
		{
			pStyle->name = atts.getValue(i);
			//pStyle->values |= edvName;
		}
			
	}
}

void UserSettingsParser::processClassElement(CSchemeLoaderState* pState, LPCTSTR name, XMLAttributes& atts)
{
	if(_tcscmp(name, _T("style-class")) == 0)
	{
		StyleDetails* pStyle = new StyleDetails;
		DefineStyle(pStyle, atts);
		
		pState->m_CustomClasses.AddStyle(pStyle->name.c_str(), pStyle);
	}
}

void UserSettingsParser::processSchemeElement(CSchemeLoaderState* pState, LPCTSTR name, XMLAttributes& atts)
{
	if(pState->m_State == US_STYLE_OVERRIDES)
	{
		if(_tcscmp(name, _T("style")) == 0)
		{
			StyleDetails* pStyle = new StyleDetails;
			DefineStyle(pStyle, atts);

			pScheme->m_Styles.push_back(pStyle);
		}
	}
	else if (pState->m_State == US_KEYWORD_OVERRIDES)
	{
		if(_tcscmp(name, _T("keywords")) == 0)
		{
			LPCTSTR key = atts.getValue(_T("key"));
			m_idval = _ttoi(key);

			pState->m_csCData = _T("");

			pState->m_State = US_KEYWORDS;
		}
	}
	else if(pState->m_State == US_SCHEME)
	{
		if(_tcscmp(name, _T("override-keywords")) == 0)
		{
			pState->m_State = US_KEYWORD_OVERRIDES;
		}
		else if(_tcscmp(name, _T("override-styles")) == 0)
		{
			pState->m_State = US_STYLE_OVERRIDES;
		}
	}
}

void UserSettingsParser::processScheme(CSchemeLoaderState* pState, XMLAttributes& atts)
{
	LPCTSTR pName =  atts.getValue(_T("name"));

	if(pName && ((int)_tcslen(pName) > 0))
	{
		pScheme = new CustomisedScheme;
		m_SchemeName = pName;
		pState->m_State = US_SCHEME;
	}
#ifdef _DEBUG
	else
	{
		::OutputDebugStr(_T("UserSettingsParser::processScheme(): Scheme section without name attribute.\n"));
	}
#endif
}

////////////////////////////////////////////////////////////
// SchemeCompiler Implementation
////////////////////////////////////////////////////////////

void SchemeCompiler::Compile(LPCTSTR path, LPCTSTR outpath, LPCTSTR mainfile)
{
	CString UserSettingsFile = outpath;
	UserSettingsFile += _T("UserSettings.xml");

	m_LoadState.m_csOutPath = outpath;

	SchemeParser::Parse(path, mainfile, (LPCTSTR)UserSettingsFile);

	CString filename(m_LoadState.m_csOutPath);
	filename += _T("default.cscheme");

	// Now we record a default scheme for use when no Scheme is selected. 
	// It has only one style (0) and default.
	m_Recorder.StartRecording(_T("default"), _T("default"), filename, 0);
	m_Recorder.SetLexer(0);
	StyleDetails* pDefault = m_LoadState.m_CustomClasses.GetStyle(_T("default"));
	if(!pDefault)
		pDefault = &m_LoadState.m_Default;
	StyleDetails temp(*pDefault);
	temp.Key = STYLE_DEFAULT;
	m_Recorder.SetDefStyle(&temp);
	sendStyle(&temp, &m_Recorder);
	temp.Key = 0;
	sendStyle(&temp, &m_Recorder);
	m_Recorder.EndRecording();
}

void SchemeCompiler::onLanguage(LPCTSTR name, LPCTSTR title, int foldflags)
{
	CString filename(m_LoadState.m_csOutPath);
	filename += name;
	filename += _T(".cscheme");

	m_Recorder.StartRecording(name, title, filename, foldflags);
	
	m_Recorder.SetDefStyle(&m_LoadState.m_Default);
}

void SchemeCompiler::onLanguageEnd()
{
	if (m_Recorder.IsRecording())
		m_Recorder.EndRecording();
}

void SchemeCompiler::onStyle(StyleDetails* pStyle, StyleDetails* pCustom)
{
	if(pCustom)
		customiseStyle(pStyle, pCustom);

	if(pStyle->Key == STYLE_DEFAULT)
	{
		m_Recorder.SetDefStyle(&m_LoadState.m_Default);
	}

	sendStyle(pStyle, &m_Recorder);	
}

void SchemeCompiler::onStyleClass(StyleDetails* pClass, StyleDetails* pCustom)
{
	if(pCustom)
		customiseStyle(pClass, pCustom);
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

void SchemeCompiler::onFile(LPCTSTR filename)
{
	CSRegistry reg;
	reg.OpenKey(_T("Software\\Echo Software\\PN2\\SchemeDates"), true);

	ctcString filepart;
	CFileName fn(filename);
	fn.GetFileName(filepart);

	reg.WriteInt(filepart.c_str(), FileAge(filename));
}

void SchemeCompiler::onKeywords(int key, LPCSTR keywords, LPCTSTR name, LPCTSTR custom)
{
	USES_CONVERSION;
	if(custom)
		m_Recorder.SetKeyWords(key, T2CA((LPCTSTR)custom));
	else
		m_Recorder.SetKeyWords(key, T2CA((LPCTSTR)keywords));
}

void SchemeCompiler::onLexer(LPCTSTR name, int styleBits)
{
	if(name)
	{
		USES_CONVERSION;
		m_Recorder.SetLexerLanguage(T2A((LPTSTR)name));
	}
	m_Recorder.SetStyleBits(styleBits);
}

////////////////////////////////////////////////////////////
// SchemeParser Implementation
////////////////////////////////////////////////////////////

void SchemeParser::Parse(LPCTSTR path, LPCTSTR mainfile, LPCTSTR userfile)
{
	XMLParserCallback<SchemeParser> callback(*this, startElement, endElement, characterData);
	
	UserSettingsParser p;
	p.Parse(userfile, &m_LoadState);

	XMLParser parser;
	parser.SetParseState(&callback);
	
	callback.SetUserData((void*)&m_LoadState);

	m_LoadState.m_pParser = &parser;

	m_LoadState.m_csBasePath = path;

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
			
			onFile(file);
		}
		catch (CSchemeParserException& E)
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

void SchemeParser::processGlobal(CSchemeLoaderState* pState, XMLAttributes& atts)
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

void SchemeParser::processKeywordClass(CSchemeLoaderState* pState, XMLAttributes& atts)
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

void SchemeParser::parseStyle(CSchemeLoaderState* pState, XMLAttributes& atts, StyleDetails* pStyle)
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
		{
			pStyle->name = t;
			continue;
		}

		CSTRING_MAP::iterator it = pState->m_Globals.find(t);
		if(it != pState->m_Globals.end())
		{
			t = (*it).second;
		}
#ifdef _DEBUG
		else
		{
			CString todebug;
			todebug.Format(_T("No match for: %s=%s\n"), nm, t);
			OutputDebugString(todebug);
		}
#endif

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

void SchemeParser::customiseStyle(StyleDetails* style, StyleDetails* custom)
{
	if(custom->values & edvFontName)
		style->FontName = custom->FontName;
	
	if(custom->values & edvFontSize)
		style->FontSize = custom->FontSize;

	if(custom->values & edvForeColor)
		style->ForeColor = custom->ForeColor;

	if(custom->values & edvBackColor)
		style->BackColor = custom->BackColor;

	if(custom->values & edvBold)
		style->Bold = custom->Bold;

	if(custom->values & edvItalic)
		style->Italic = custom->Italic;

	if(custom->values & edvUnderline)
		style->Underline = custom->Underline;

	if(custom->values & edvEOLFilled)
		style->EOLFilled = custom->EOLFilled;
}

void SchemeParser::processStyleClass(CSchemeLoaderState* pState, XMLAttributes& atts)
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
			pState->m_StyleClasses.AddStyle(name, pStyle);
		}

		t = atts.getValue(_T("inherit-style"));
		if(t != NULL)
		{
			CString inh(t);
			StyleDetails* pE = pState->m_StyleClasses.GetStyle(inh);
			if(pE)
				*pStyle = *pE;
		}

		parseStyle(pState, atts, pStyle);

        // NULL if not found.
		StyleDetails* pCustom = pState->m_CustomClasses.GetStyle(name);;

		onStyleClass(pStyle, pCustom);
	}
}

void SchemeParser::processLanguageStyleGroup(CSchemeLoaderState* pState, XMLAttributes& atts)
{
	onStyleGroup(atts);
}

void SchemeParser::processLanguageStyle(CSchemeLoaderState* pState, XMLAttributes& atts)
{
	CString classname;
	StyleDetails* pCustom = NULL;
	StyleDetails* pBase = &pState->m_Default;

	LPCTSTR skey = atts.getValue(_T("key"));
	int key = _ttoi(skey);

	if(pState->m_pCustom)
	{
		pCustom = pState->m_pCustom->FindStyle(key);
		if(pCustom)
		{
			if(pCustom->values |= edvClass)
				classname = pCustom->classname.c_str();
		}
	}

	if(classname.GetLength() == 0)
	{
		classname = atts.getValue(_T("class"));
	}

	if((classname.GetLength() > 0) && (classname != _T("default")))
	{
		StyleDetails* pFound = pState->m_StyleClasses.GetStyle(classname);
		if(pFound)
			pBase = pFound;
	}

	StyleDetails Style(*pBase);

	parseStyle(pState, atts, &Style);

	Style.Key = key;

	onStyle(&Style, pCustom);
}

void SchemeParser::processLanguageKeywords(CSchemeLoaderState* pState, XMLAttributes& atts)
{
	 //<keyword key="0" class="hypertext"/>
	int x = atts.getCount();
	CString name = _T("");
	CString val = _T("");
	CString kw = _T("");
	CString namestr = _T("");
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
		else if(name == _T("name"))
		{
			namestr = val;
		}
	}

	LPCTSTR custom = NULL;
	if(pState->m_pCustom)
	{
		CustomKeywordSet* pCustom = pState->m_pCustom->FindKeywordSet(key);
		if(pCustom)
			custom = pCustom->pWords;
	}

	if(kw != _T("") && key != -1)
	{
		onKeywords(key, kw, namestr, custom);
	}
}

void SchemeParser::processLanguageElement(CSchemeLoaderState* pState, LPCTSTR name, XMLAttributes& atts)
{
	LPCTSTR t = NULL;

	if(pState->m_State == DOING_LANGUAGE && _tcscmp(name, _T("language")) == 0)
	{
		LPCTSTR scheme = atts.getValue(_T("name"));
		LPCTSTR title = atts.getValue(_T("title"));
		if(scheme != NULL)
		{
			pState->m_csLangName = scheme;
			CNM_IT custom = pState->m_CustomSchemes.find(pState->m_csLangName);
			if(custom != pState->m_CustomSchemes.end())
			{
				pState->m_pCustom = (*custom).second;
			}
			else
			{
				pState->m_pCustom = NULL;
			}
			
			int foldflags = 0;
			
			t = atts.getValue(_T("folding"));
			if(t != NULL && PNStringToBool(t))
			{
				//fldEnabled = 1, fldCompact = 2, fldComments = 4, fldPreProc = 8
				foldflags = fldEnabled;
				
				t = atts.getValue(_T("foldcompact"));
				if(t != NULL && PNStringToBool(t))
					foldflags |= fldCompact;

				t = atts.getValue(_T("foldcomments"));
				if(t != NULL && PNStringToBool(t))
					foldflags |= fldComments;

				t = atts.getValue(_T("foldpreproc"));
				if(t != NULL && PNStringToBool(t))
					foldflags |= fldPreProc;
			}

			onLanguage(scheme, title, foldflags);
			
			pState->m_State = DOING_LANGUAGE_DETAILS;
		}
	}
	else
	{
		ATLASSERT(pState->m_State == DOING_LANGUAGE_DETAILS);

		if(_tcscmp(name, _T("lexer")) == 0)
		{
			LPCTSTR lexer;
			int		sbits = 5;
			lexer = atts.getValue(_T("name"));
			
			t = atts.getValue(_T("stylebits"));
			if(t != NULL)
			{
				sbits = _ttoi(t);
				if(sbits == 0)
				{
					//throw CSchemeParserException(pState->m_pParser, _T("Style Bits value not valid (0 or non-numeric)"));
					::OutputDebugString(_T("Style Bits value not valid (0 or non-numeric)"));
					sbits = 5;
				}
			}

			onLexer(lexer, sbits);
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

void SchemeParser::specifyImportFile(CSchemeLoaderState* pState, XMLAttributes& atts)
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
		throw CSchemeParserException(pState->m_pParser, _T("Import element with no name attribute."));
	}
}

void SchemeParser::specifyImportSet(CSchemeLoaderState* pState, XMLAttributes& atts)
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
		throw CSchemeParserException(pState->m_pParser, _T("Set element with no pattern attribute."));
	}
}

void SchemeParser::processKeywordCombine(CSchemeLoaderState* pState, XMLAttributes& atts)
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
			throw CSchemeParserException(pState->m_pParser, _T("Unmatched keyword class inclusion."));
		}
	}
	else
	{
		throw CSchemeParserException(pState->m_pParser, _T("include-class element with no name attribute"));
	}
}

void SchemeParser::startElement(void *userData, LPCTSTR name, XMLAttributes& atts)
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
	else if(state == DOING_LANGUAGE_STYLES && _tcscmp(name, _T("group")) == 0)
	{
		processLanguageStyleGroup(pState, atts);
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
		stattext = _T("Processing Globals\r\n");
		pState->m_State = DOING_GLOBALS;
	}
	else if(_tcscmp(name, _T("keyword-classes")) == 0)
	{
		stattext = _T("Processing Keyword Classes\r\n");
		pState->m_State = DOING_KEYWORDC;
	}
	else if(_tcscmp(name, _T("style-classes")) == 0)
	{
		stattext = _T("Processing Style Classes\r\n");
		pState->m_State = DOING_STYLECS;
	}
	else if(_tcscmp(name, _T("language")) == 0)
	{
		stattext = _T("Processing Language\r\n");
		pState->m_State = DOING_LANGUAGE;
		processLanguageElement(pState, name, atts);
	}
	else if(_tcscmp(name, _T("imports")) == 0)
	{
		stattext = _T("Processing Import Specs\r\n");
		pState->m_State = DOING_IMPORTS;
	}
	else
	{
		stattext.Format(_T("Start Element: %s (%d attributes)\r\n"), name, atts.getCount());
	}

	if(stattext.GetLength() > 0)
		OutputDebugString(stattext);
}

void SchemeParser::endElement(void *userData, LPCTSTR name)
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
#ifdef _DEBUG
			else
			{
				CString todebug;
				todebug.Format(_T("No match for: %s\n"), global);
				OutputDebugString(todebug);
			}

			stattext.Format(_T("Added Global: %s = %s\r\n"), pS->m_csGName, global);
			OutputDebugString(stattext);
#endif
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

#ifdef _DEBUG
			stattext.Format(_T("Added Keyword Class: %s\r\n"), pS->m_csGName);
			OutputDebugString(stattext);
#endif
		}

		pS->m_State = DOING_KEYWORDC;
	}
	else if(state == DOING_LANGUAGE || state == DOING_LANGUAGE_DETAILS)
	{
		// Only come out of language mode if we're really out...
		if(_tcscmp(name, _T("language")) == 0)
		{
			onLanguageEnd();
			
			pS->m_State = 0;
		}
	}
	else if(state == DOING_LANGUAGE_KW || state == DOING_LANGUAGE_STYLES)
	{
		if((_tcscmp(name, _T("use-keywords")) == 0) || (_tcscmp(name, _T("use-styles")) == 0))
			pS->m_State = DOING_LANGUAGE_DETAILS;
		else if(_tcscmp(name, _T("group")) == 0)
			onStyleGroupEnd();
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

///@todo perhaps change this to use strncat and go straight into the CData buffer...
void SchemeParser::characterData(void* userData, LPCTSTR data, int len)
{
	CSchemeLoaderState* pState = static_cast<CSchemeLoaderState*>(userData);

	CString cdata;
	TCHAR* buf = cdata.GetBuffer(len+1);
	_tcsncpy(buf, data, len);
	buf[len] = 0;
	cdata.ReleaseBuffer();

	pState->m_csCData += cdata;
}