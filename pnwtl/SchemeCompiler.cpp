/**
 * @file SchemeCompiler.cpp
 * @brief Implement scheme reader and compiler classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2005 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * Unicode Status: Unicode Ready (untested).
 */

#include "stdafx.h"
#include "SchemeCompiler.h"
#include "ssreg.h"

// Parser State Defines
#define DOING_GLOBALS			1
#define DOING_GLOBAL			2
#define DOING_KEYWORDC			3
#define DOING_KEYWORDS			4
#define DOING_STYLECS			5	//style-classes
#define DOING_STYLEC			6	//style-class
#define DOING_LANGUAGE			7	//language and children...
#define DOING_LANGUAGE_DETAILS	8
#define DOING_LANGUAGE_KW		9
#define DOING_LANGUAGE_STYLES	10
#define	DOING_IMPORTS			11
#define DOING_KEYWORDCOMBINE	12
#define DOING_BASE_OPTIONS		13

#define US_SCHEMES				1
#define US_SCHEME				2
#define US_KEYWORD_OVERRIDES	3
#define US_STYLE_OVERRIDES		4
#define US_KEYWORDS				5
#define US_CLASSES				6
#define US_CLASS				7

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

	// Fail if we can't open the file...
	if(m_out == NULL)
		return false;

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

	scHdr.Flags = FoldFlags;

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
	else if(Msg == SCI_SETPROPERTY)
	{
		m_next = nrPropRec;
	}

	Record(Msg, wParam, lParam);
	return 0;
}

bool SchemeRecorder::CheckNecessary(long Msg, WPARAM wParam, LPARAM lParam)
{
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
		else if(m_next == nrTextRec)
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
		else
		{
			PropRec propr;
			propr.NameLength = strlen((const char*)wParam);
			propr.ValueLength = strlen((const char*)lParam);
			fwrite(&propr, sizeof(PropRec), 1, m_out);
			fwrite((const char*)wParam, sizeof(char), strlen((const char*)wParam), m_out);
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

	for(CNM_IT i = m_BaseSchemes.begin(); i != m_BaseSchemes.end(); ++i)
	{
		BaseScheme* pS = static_cast<BaseScheme*>( (*i).second );
		delete pS;
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
	ssreg::CSRegistry reg;
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

	XMLParserCallback<UserSettingsParser> callback(*this, &UserSettingsParser::startElement, &UserSettingsParser::endElement, &UserSettingsParser::characterData);

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
		
		LOG(err);
	}
	catch (XMLParserException& E)
	{
		CString err;
		err.Format(_T("Error Parsing Scheme UserSettings XML: %s\n (file: %s, line: %d, column %d)"), 
			XML_ErrorString(E.GetErrorCode()), E.GetFileName(), E.GetLine(), E.GetColumn());
		
		LOG(err);
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

void UserSettingsParser::processClassElement(CSchemeLoaderState* pState, LPCTSTR name, XMLAttributes& atts)
{
	if(_tcscmp(name, _T("style-class")) == 0)
	{
		StyleDetails* pStyle = new StyleDetails;
		SchemeParser::parseStyle(pState, atts, pStyle, false);
		
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
			SchemeParser::parseStyle(pState, atts, pStyle, false);

			pScheme->AddStyle(pStyle);
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
		else if(_tcscmp(name, _T("colours")) == 0)
		{
			pScheme->m_editorColours.SetFromXml(atts);
		}
	}
}

#define SZTRUE(s) \
	(s[0] == 't')

void UserSettingsParser::processScheme(CSchemeLoaderState* pState, XMLAttributes& atts)
{
	LPCTSTR pName =  atts.getValue(_T("name"));

	if(pName && ((int)_tcslen(pName) > 0))
	{
		pScheme = new CustomisedScheme;
		m_SchemeName = pName;
		pState->m_State = US_SCHEME;

		LPCTSTR temp = atts.getValue("ovtabs");
		if(temp != NULL && _tcslen(temp) > 0)
		{
			pScheme->hasflags |= schOverrideTabs;

			if(SZTRUE(temp))
				// Signal that we definitely want to override the tab use.
				pScheme->flags |= schOverrideTabs;
		
			temp = atts.getValue("usetabs");
			if(temp != NULL && _tcslen(temp) > 0)
			{
				pScheme->hasflags |= schUseTabs;
				if(SZTRUE(temp))
					pScheme->flags |= schUseTabs;
			}

		}

		temp = atts.getValue("tabwidth");
		if(temp != NULL && _tcslen(temp) > 0)
		{
			pScheme->hasflags |= schOverrideTabSize;
			pScheme->flags |= schOverrideTabSize;

			pScheme->m_tabwidth = _ttoi(temp);
		}
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

void SchemeCompiler::onLanguage(LPCTSTR name, LPCTSTR title, int foldflags, int /*ncfoldflags*/)
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

void SchemeCompiler::onProperty(LPCTSTR name, LPCTSTR value)
{
	USES_CONVERSION;

	m_Recorder.SetProperty(CT2CA(name), CT2CA(value));
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
	compiler->StyleSetHotSpot(s->Key, s->Hotspot);

	if(s->Key == STYLE_DEFAULT)
		compiler->StyleClearAll();
}

void SchemeCompiler::onFile(LPCTSTR filename)
{
	ssreg::CSRegistry reg;
	reg.OpenKey(_T("Software\\Echo Software\\PN2\\SchemeDates"), true);

	tstring filepart;
	CFileName fn(filename);
	fn.GetFileName(filepart);

	reg.WriteInt(filepart.c_str(), FileAge(filename));
}

void SchemeCompiler::onKeywords(int key, LPCTSTR keywords, LPCTSTR name, LPCTSTR custom)
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

void SchemeCompiler::onColours(const EditorColours* colours)
{
	colours->SendColours(&m_Recorder);
}

void SchemeCompiler::onError(XMLParserException& ex)
{
	if (m_Recorder.IsRecording())
		m_Recorder.EndRecording();
}

////////////////////////////////////////////////////////////
// SchemeParser Implementation
////////////////////////////////////////////////////////////

void SchemeParser::Parse(LPCTSTR path, LPCTSTR mainfile, LPCTSTR userfile)
{
	m_LoadState.m_StartLoad = ::GetTickCount();

	XMLParserCallback<SchemeParser> callback(*this, &SchemeParser::startElement, &SchemeParser::endElement, &SchemeParser::characterData);
	
	UserSettingsParser p;
	p.Parse(userfile, &m_LoadState);

	XMLParser parser;
	parser.SetParseState(&callback);
	
	callback.SetUserData((void*)&m_LoadState);

	m_LoadState.m_pGroupClass = NULL;
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
			
			LOG(err);

			onError(E);
		}
		catch (XMLParserException& E)
		{
			CString err;
			err.Format(_T("Error Parsing Scheme XML: %s\n (file: %s, line: %d, column %d)"), 
				XML_ErrorString(E.GetErrorCode()), E.GetFileName(), E.GetLine(), E.GetColumn());
			
			LOG(err);

			onError(E);
		}

		parser.Reset();
	}

	DWORD dwTimeDiff = GetTickCount() - m_LoadState.m_StartLoad;
	TCHAR buf[200];
	_sntprintf(buf, 200, _T("Schemes Load: %dms\n"), dwTimeDiff);
	LOG(buf);
}

/**
 * A base style is a style that every scheme reflects.
 */
void SchemeParser::processBaseStyle(CSchemeLoaderState* pState, XMLAttributes& atts)
{
	// Create a style with no default settings, the defaults will be applied
	// later as the base style is used for each scheme.
	StyleDetails* pS = new StyleDetails( );
	
	parseStyle(pState, atts, pS);

	pState->m_BaseStyles.AddStyle(pS);
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

void SchemeParser::processProperty(CSchemeLoaderState* pState, XMLAttributes& atts)
{
	LPCTSTR name = atts.getValue(_T("name"));
	LPCTSTR value = atts.getValue(_T("value"));

	if(name != NULL && value != NULL)
	{
		onProperty(name, value);
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

void SchemeParser::parseStyle(CSchemeLoaderState* pState, XMLAttributes& atts, StyleDetails* pStyle, bool bExpandGlobals)
{
	LPCTSTR nm;
	LPCTSTR t;
	int c = atts.getCount();

	// need to add global substitution.
	for(int i = 0; i < c; i++)
	{
		nm = atts.getName(i);
		t = atts.getValue(i);

		// Definitely don't want any variable expansion for these:
		if(_tcscmp(nm, _T("name")) == 0)
		{
			pStyle->name = t;
			continue;
		}
		else if(_tcscmp(nm, _T("class")) == 0)
		{
			pStyle->classname = t;
			pStyle->values |= edvClass;
			continue;
		}
		else if(_tcscmp(nm, _T("key")) == 0)
		{
			pStyle->Key = _ttoi(t);
			continue;
		}
		else if(_tcscmp(nm, _T("message")) == 0)
		{
			pStyle->Key = _ttoi(t);
			pStyle->KeyIsMessage = true;
			continue;
		}

		if(bExpandGlobals)
		{
			CSTRING_MAP::iterator it = pState->m_Globals.find(CString(t));
			if(it != pState->m_Globals.end())
			{
				t = (*it).second;
			}
#ifdef _DEBUG
			else
			{
				CString todebug;
				todebug.Format(_T("No match for: %s=%s\n"), nm, t);
				LOG(todebug);
			}
#endif
		}

		if(_tcscmp(nm, _T("fore")) == 0)
		{
			pStyle->ForeColor = PNStringToColor(t);
			pStyle->values |= edvForeColor;
		}
		else if(_tcscmp(nm, _T("back")) == 0)
		{
			pStyle->BackColor = PNStringToColor(t);
			pStyle->values |= edvBackColor;
		}
		else if(_tcscmp(nm, _T("font")) == 0)
		{
			USES_CONVERSION;
			pStyle->FontName = T2CA(t);
			pStyle->values |= edvFontName;
		}
		else if(_tcscmp(nm, _T("size")) == 0)
		{
			pStyle->FontSize = _ttoi(t);
			pStyle->values |= edvFontSize;
		}
		else if(_tcscmp(nm, _T("italic")) == 0)
		{
			pStyle->Italic = PNStringToBool(t);
			pStyle->values |= edvItalic;
		}
		else if(_tcscmp(nm, _T("bold")) == 0)
		{
			pStyle->Bold = PNStringToBool(t);
			pStyle->values |= edvBold;
		}
		else if(_tcscmp(nm, _T("underline")) == 0)
		{
			pStyle->Underline = PNStringToBool(t);
			pStyle->values |= edvUnderline;
		}
		else if(_tcscmp(nm, _T("eolfilled")) == 0)
		{
			pStyle->EOLFilled = PNStringToBool(t);
			pStyle->values |= edvEOLFilled;
		}
		else if(_tcscmp(nm, _T("hotspot")) == 0)
		{
			pStyle->Hotspot = PNStringToBool(t);
			// no values flag for this, it's set in the bases only.
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
	StyleDetails* pClass = NULL;
	LPCTSTR pszClass = atts.getValue(_T("class"));

	if(!pState->m_bBaseParse)
	{
		if(pszClass)
		{
			pClass = pState->m_StyleClasses.GetStyle(pszClass);
			if(pClass)
				pState->m_pGroupClass = pClass;
		}

		onStyleGroup(atts, pClass);
	}
	else
	{
		StyleDetails* pGroupMarker = new StyleDetails;
		pGroupMarker->values = edvGroupStart;
		if(pszClass)
			pGroupMarker->classname = pszClass;
		pState->m_pBase->AddStyle(pGroupMarker);
		LPCTSTR name = atts.getValue(_T("name"));
		LPCTSTR description = atts.getValue(_T("description"));
		pState->m_pBase->AddGroupDetails(name, description);
	}
}

/**
 * Style Classes are found in this order of priority:
 * 1) Customised style class attribute.
 * 2) Class assigned to property group.
 * 3) Class in the current style class attribute.
 * 4) Default style.
 */
void SchemeParser::processLanguageStyle(CSchemeLoaderState* pState, XMLAttributes& atts)
{
	CString classname;
	StyleDetails* pCustom = NULL;
	StyleDetails* pBase = NULL;

	LPCTSTR skey = atts.getValue(_T("key"));
	int key = _ttoi(skey);

	// If we're parsing a base-style, we will re-do this process later
	// so we don't bother now.
	if(!pState->m_bBaseParse)
	{

		// Custom styles first (storing any custom version for later)...
		if(pState->m_pCustom)
		{
			pCustom = pState->m_pCustom->GetStyle(key);
			if(pCustom)
			{
				if((pCustom->values & edvClass) != 0)
					classname = pCustom->classname.c_str();
			}
		}

		// No customised style class, is there a group class?
		if(classname.GetLength() == 0)
		{
			if(pState->m_pGroupClass != NULL)
			{
				// There is a class associated with a group of styles.
				// We also don't need to find the style, it will already
				// be the m_pGroupClass member of pState.
				pBase = pState->m_pGroupClass;
			}
			else
			{
				classname = atts.getValue(_T("class"));
			}
		}

		// We've not found a class yet, but if we do have a class name, we try to find that.
		if(!pBase && (classname.GetLength() > 0) && (classname != _T("default")))
		{
			pBase = pState->m_StyleClasses.GetStyle(classname);
		}
	}

	// If we didn't find a class, we base the style on the default style...
	if(!pBase)
		pBase = &pState->m_Default;

	// Create a new style based on either the class or the default style.
	StyleDetails Style(*pBase);
	
	// we reset this, so that parseStyle will tell us what has been changed.
	Style.values = 0;

	// Read the details into it from it's attributes.
	parseStyle(pState, atts, &Style);

	Style.Key = key;

	if(!pState->m_bBaseParse)
	{
		// Pass it to whatever wants to know about it.
		onStyle(&Style, pCustom);
	}
	else
	{
		StyleDetails* pStored = new StyleDetails(Style);
		pState->m_pBase->AddStyle(pStored);
	}
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
		if(!pState->m_bBaseParse)
		{
            onKeywords(key, kw, namestr, custom);
		}
		else
		{
			CustomKeywordSet* pSet = new CustomKeywordSet;
			pSet->key = key;
			int sLen = _tcslen(namestr);
			if(sLen != 0)
			{
				pSet->pName = new TCHAR[sLen + 1];
				_tcscpy(pSet->pName, namestr);
			}
			sLen = _tcslen(kw);
			if(sLen != 0)
			{
                pSet->pWords = new TCHAR[sLen + 1];
				_tcscpy(pSet->pWords, kw);
			}
			pState->m_pBase->AddKeywordSet(pSet);
		}
	}
}

/**
 * @brief Process some XML element related to the "language" block of a schemes file.
 */
void SchemeParser::processLanguageElement(CSchemeLoaderState* pState, LPCTSTR name, XMLAttributes& atts)
{
	LPCTSTR t = NULL;
	int flags = 0;
	int ncflags = 0;
	BaseScheme* pBase = NULL;

	if(pState->m_State == DOING_LANGUAGE && 
		(_tcscmp(name, _T("language")) == 0 || _tcscmp(name, _T("schemedef")) == 0 || 
		_tcscmp(name, _T("base-language")) == 0) )
	{
		LPCTSTR scheme = atts.getValue(_T("name"));
		LPCTSTR title = atts.getValue(_T("title"));
		if(scheme != NULL)
		{
			pState->m_StartLang = ::GetTickCount(); // diags.

			pState->m_csLangName = scheme;

			LPCTSTR base = atts.getValue(_T("base"));
			if(base != NULL)
			{
				// The language has a base-language reference.
				CNM_IT iBase = pState->m_BaseSchemes.find(CString(base));
				if( iBase != pState->m_BaseSchemes.end() )
				{
					pBase = static_cast<BaseScheme*>( (*iBase).second );
					flags = pBase->flags;
				}
			}
			
			t = atts.getValue(_T("folding"));
			if(t != NULL && PNStringToBool(t))
			{
				//fldEnabled = 1, fldCompact = 2, fldComments = 4, fldPreProc = 8
				flags |= fldEnabled;
				
				t = atts.getValue(_T("foldcompact"));
				if(t != NULL && PNStringToBool(t))
					flags |= fldCompact;

				t = atts.getValue(_T("foldcomments"));
				if(t != NULL && PNStringToBool(t))
					flags |= fldComments;

				t = atts.getValue(_T("foldpreproc"));
				if(t != NULL && PNStringToBool(t))
					flags |= fldPreProc;
			}

			t = atts.getValue(_T("usetabs"));
			if(t != NULL)
			{
				if(PNStringToBool(t))
					flags |= schOverrideTabs & schUseTabs;
				else
				{
					flags |= schOverrideTabs;
					flags &= ~schUseTabs;
				}
			}

			t = atts.getValue(_T("internal"));
			if(t != NULL && PNStringToBool(t))
				flags |= schInternal;

			// Store the non-customised flags to pass on
			ncflags = flags;

			// Only look for customisations if this is an actual scheme.
			if(!pState->m_bBaseParse)
			{
				CNM_IT custom = pState->m_CustomSchemes.find(pState->m_csLangName);
				if(custom != pState->m_CustomSchemes.end())
				{
					pState->m_pCustom = (*custom).second;

					// Final flags stage, see if the user has removed some.
					if(pState->m_pCustom->hasflags & schOverrideTabs)
					{
						flags &= ~(schOverrideTabs|schUseTabs);
						flags |= (pState->m_pCustom->flags & (schOverrideTabs|schUseTabs));
					}
					
					if(pState->m_pCustom->hasflags & schOverrideTabSize)
					{
						flags &= ~(schOverrideTabSize);
						flags |= (pState->m_pCustom->flags & schOverrideTabSize);
					}
				}
				else
				{
					pState->m_pCustom = NULL;
				}
			}
			else
				pState->m_pCustom = NULL;

			if(!pState->m_bBaseParse)
			{
				// Signal the implementing class that there's a language (scheme) coming.
				onLanguage(scheme, title, flags, ncflags);

				if( pBase )
				{
					sendBaseScheme(pState, pBase);
				}
			}
			else
			{
				pState->m_pBase = new BaseScheme;
				pState->m_BaseSchemes.insert(pState->m_BaseSchemes.end(), CUSTOMISED_NAMEMAP::value_type(pState->m_csLangName, pState->m_pBase));
				pState->m_pBase->flags = flags;
			}

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
					LOG(_T("Style Bits value not valid (0 or non-numeric)"));
					sbits = 5;
				}
			}

			if(!pState->m_bBaseParse)
				onLexer(lexer, sbits);
			else
			{
				pState->m_pBase->styleBits = sbits;
				pState->m_pBase->lexer = lexer;
				pState->m_pBase->valuesSet |= ebvLexer;
			}
		}
		else if(_tcscmp(name, _T("property")) == 0)
		{
			if(!pState->m_bBaseParse)
			{
				processProperty(pState, atts);
			}
			else
			{
				LOG(_T("property element not supported in base languages."));
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

/**
 * @brief Add a specific file to the list of files to be processed.
 */
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

void SchemeParser::sendBaseStyle(CSchemeLoaderState* pState, StyleDetails* pS)
{
	StyleDetails* pCustom = NULL;
	StyleDetails* pBase = NULL;
	CString classname;
	int key = pS->Key;

	if(pState->m_pCustom)
	{
		pCustom = pState->m_pCustom->GetStyle(key);
		if(pCustom)
		{
			if((pCustom->values & edvClass) != 0)
				classname = pCustom->classname.c_str();
		}
	}

	// No customised style class, is there a group class?
	if(classname.GetLength() == 0)
	{
		if(pState->m_pGroupClass != NULL)
		{
			// There is a class associated with a group of styles.
			// We also don't need to find the style, it will already
			// be the m_pGroupClass member of pState.
			pBase = pState->m_pGroupClass;
		}
		else
		{
			classname = pS->classname.c_str();
		}
	}

	// We've not found a class yet, but if we do have a class name, we try to find that.
	if(!pBase && (classname.GetLength() > 0) && (classname != _T("default")))
	{
		pBase = pState->m_StyleClasses.GetStyle(classname);
	}
	else
	{
		// If we didn't find a class, we base the style on the default style...
		pBase = &pState->m_Default;
	}

	StyleDetails Style(*pS);
	
	if( pBase )
		customiseStyle(&Style, pBase);

	onStyle(&Style, pCustom);
}

/**
 * Send all of the base styles for the current scheme.
 */
void SchemeParser::sendBaseStyles(CSchemeLoaderState* pState)
{
	LPCTSTR attstr[5];
	attstr[0] = _T("name");
	attstr[1] = _T("Common");
	attstr[2] = _T("description");
	attstr[3] = _T("Common styles and colours.");
	attstr[4] = NULL;
	XMLAttributes atts(&attstr[0]);

	onStyleGroup(atts, NULL);
	
	for(SL_CIT i = pState->m_BaseStyles.StylesBegin();
		i != pState->m_BaseStyles.StylesEnd();
		++i)
	{
		sendBaseStyle(pState, (*i));
	}

	onStyleGroupEnd();
}

void SchemeParser::sendBaseScheme(CSchemeLoaderState* pState, BaseScheme* pBase)
{
	GroupDetails_t* pGroupDetails = pBase->pGroupDetails;

	if( (pBase->valuesSet & ebvLexer) != 0 )
	{
		onLexer(pBase->lexer.c_str(), pBase->styleBits);
	}

	for(SL_CIT i = pBase->StylesBegin(); i != pBase->StylesEnd(); ++i)
	{
		StyleDetails* pS = (*i);
		if( (pS->values & edvGroupStart) != 0 )
		{
			StyleDetails* pClass = NULL;
			
			if( pS->classname.length() != 0 )
			{
				pClass = pState->m_StyleClasses.GetStyle(pS->classname.c_str());
				if( pClass )
					pState->m_pGroupClass = pClass;
			}

			if( pGroupDetails != NULL )
			{
				LPCTSTR attstr[5];
				attstr[0] = _T("name");
				attstr[1] = pGroupDetails->name;
				attstr[2] = _T("description");
				attstr[3] = pGroupDetails->description;
				attstr[4] = NULL;
				XMLAttributes atts(&attstr[0]);

				onStyleGroup(atts, pClass);

				pGroupDetails = pGroupDetails->pNext;
			}
		}
		else if( (pS->values & edvGroupEnd) != 0 )
		{
			onStyleGroupEnd();
			pState->m_pGroupClass = NULL;
		}
		else
		{
			sendBaseStyle(pState, pS);
		}
	}
}

/**
 * @brief Take an import fileset specification and add the relevant files to be processed.
 */
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
	/*else if(state == DOING_BASE_OPTIONS && _tcscmp(name, _T("colour")) == 0)
	{
		processBaseColour(pState, atts);
	}*/
	else if(state == DOING_BASE_OPTIONS && _tcscmp(name, _T("style")) == 0)
	{
		processBaseStyle(pState, atts);
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
	else if(_tcscmp(name, _T("base-options")) == 0)
	{
		stattext = _T("Processing Base Options\r\n");
		pState->m_State = DOING_BASE_OPTIONS;
	}
	else if(_tcscmp(name, _T("language")) == 0 || _tcscmp(name, _T("schemedef")) == 0)
	{
		stattext = _T("Processing Language\r\n");
		pState->m_State = DOING_LANGUAGE;
		pState->m_bBaseParse = false;
		processLanguageElement(pState, name, atts);
	}
	else if(_tcscmp(name, _T("imports")) == 0)
	{
		stattext = _T("Processing Import Specs\r\n");
		pState->m_State = DOING_IMPORTS;
	}
	else if(_tcscmp(name, _T("base-language")) == 0)
	{
		stattext = _T("Processing Base Class\r\n");
		pState->m_State = DOING_LANGUAGE;
		pState->m_bBaseParse = true;
		processLanguageElement(pState, name, atts);
	}
	else
	{
		stattext.Format(_T("Start Element: %s (%d attributes)\r\n"), name, atts.getCount());
	}

	if(stattext.GetLength() > 0)
		LOG(stattext);
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
				LOG(todebug);
			}

			stattext.Format(_T("Added Global: %s = %s\r\n"), pS->m_csGName, global);
			LOG(stattext);
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
			LOG(stattext);
#endif
		}

		pS->m_State = DOING_KEYWORDC;
	}
	else if(state == DOING_LANGUAGE || state == DOING_LANGUAGE_DETAILS)
	{
		// Only come out of language mode if we're really out...
		if(_tcscmp(name, _T("language")) == 0 || _tcscmp(name, _T("schemedef")) == 0)
		{
			// Now we send any styles that haven't been sent yet, and the last of
			// the editor setup. This has to be done here because otherwise it's wiped
			// out by the clearall which is sent after the default style.
			sendBaseStyles(pS);

			if(pS->m_pCustom)
				if(pS->m_pCustom->m_editorColours.HasColours())
					onColours(&pS->m_pCustom->m_editorColours);

			onLanguageEnd();

			DWORD dwTimeDiff = ::GetTickCount() - pS->m_StartLang;
			TCHAR buf[300];
			_sntprintf(buf, 300, _T("Language Load (%s): %dms\n"), pS->m_csLangName, dwTimeDiff);
			LOG(buf);
			
			pS->m_State = 0;
		}
		else if(_tcscmp(name, _T("base-language")) == 0)
		{
			pS->m_pBase = NULL;

			pS->m_State = 0;
		}
	}
	else if(state == DOING_LANGUAGE_KW || state == DOING_LANGUAGE_STYLES)
	{
		if((_tcscmp(name, _T("use-keywords")) == 0) || (_tcscmp(name, _T("use-styles")) == 0))
		{
			pS->m_State = DOING_LANGUAGE_DETAILS;
		}
		else if(_tcscmp(name, _T("group")) == 0)
		{
			// Remove any custom group class we had assigned.
			pS->m_pGroupClass = NULL;
			
			if(!pS->m_bBaseParse)
			{
				onStyleGroupEnd();
			}
			else
			{
				// Add a dummy style to mark the end of the group.
				StyleDetails* pGroupEndMarker = new StyleDetails;
				pGroupEndMarker->values = edvGroupEnd;
				pS->m_pBase->AddStyle(pGroupEndMarker);
			}
		}
	}
	else if(state == DOING_KEYWORDCOMBINE)
	{
		pS->m_State = DOING_KEYWORDS;
	}
	else if(state == DOING_IMPORTS)
	{
		if( _tcscmp(name, _T("imports")) == 0 )
			pS->m_State = 0;
	}
	else if(state == DOING_BASE_OPTIONS)
	{
		if( _tcscmp(name, _T("base-options")) == 0 )
		{
			pS->m_State = 0;
		}
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