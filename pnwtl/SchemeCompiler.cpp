/**
 * @file SchemeCompiler.cpp
 * @brief Implement scheme reader and compiler classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "SchemeCompiler.h"
#include "ssreg.h"
#include "include/filefinder.h"

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

StylePtr SchemeLoaderState::GetClass(LPCTSTR name)
{
	StylePtrMap::const_iterator iC = m_Classes.find(tstring(name));
	if(iC != m_Classes.end())
		return (*iC).second;
	return StylePtr();
}

////////////////////////////////////////////////////////////
// SchemeRecorder Implementation
////////////////////////////////////////////////////////////

SchemeRecorder::SchemeRecorder() : CScintilla()
{
	m_out = NULL;
	m_next = nrMsgRec;
}

bool SchemeRecorder::StartRecording(LPCSTR scheme, LPCTSTR title, LPCTSTR outfile, int FoldFlags)
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

void SchemeRecorder::WriteCommentBlock(const char* linecomment, const char* streamcommentstart, const char* streamcommentend, 
			const char* commentblockstart, const char* commentblockend, const char* commentblockline)
{
	if (!m_out)
	{
		return;
	}

	CommentSpecRec csr;
	memset(&csr, 0, sizeof(CommentSpecRec));
	if(linecomment)
		strncpy(csr.CommentLineText, linecomment, SC_HDR_COMMENTTEXTSIZE);
	if(streamcommentstart)
		strncpy(csr.CommentStreamStart, streamcommentstart, SC_HDR_COMMENTTEXTSIZE);
	if(streamcommentend)
		strncpy(csr.CommentStreamEnd, streamcommentend, SC_HDR_COMMENTTEXTSIZE);
	if(commentblockstart)
		strncpy(csr.CommentBlockStart, commentblockstart, SC_HDR_COMMENTBLOCKTEXTSIZE);
	if(commentblockend)
		strncpy(csr.CommentBlockEnd, commentblockend, SC_HDR_COMMENTBLOCKTEXTSIZE);
	if(commentblockline)
		strncpy(csr.CommentBlockLine, commentblockline, SC_HDR_COMMENTTEXTSIZE);
	
	char type = nrCommentRec;
	fwrite(&type, sizeof(char), 1, m_out);
	fwrite(&csr, sizeof(CommentSpecRec), 1, m_out);

}

void SchemeRecorder::WriteHeader(LPCSTR schemename, LPCTSTR schemetitle, int FoldFlags)
{
	if (!m_out)
	{
		return;
	}

	CT2CA asciiTitle(schemetitle);

	SchemeHdrRec scHdr;

	memset(&scHdr.Name[0], 0, SC_HDR_NAMESIZE);
	strcpy(&scHdr.Name[0], schemename);

	if(schemetitle != NULL)
	{
		memset(&scHdr.Title[0], 0, SC_HDR_TITLESIZE);
		strcpy(&scHdr.Title[0], asciiTitle);
	}

	scHdr.Flags = FoldFlags;

	fwrite(&scHdr, sizeof(SchemeHdrRec), 1, m_out);
}

bool SchemeRecorder::EndRecording()
{
	if (m_out)
	{
		fclose(m_out);
		m_out = NULL;
		return true;
	}
	else 
		return false;
}

long SchemeRecorder::SPerform(long Msg, WPARAM wParam, LPARAM lParam)
{
	if (!m_out)
	{
		return 0;
	}

	switch (Msg)
	{
		case SCI_STYLESETFONT:
		{
			m_next = nrTextRec;
			m_tType = ttFontName;
		}
		break;
		case SCI_SETLEXERLANGUAGE:
		{
			m_next = nrTextRec;
			m_tType = ttLexerLanguage;
		}
		break;
		case SCI_SETKEYWORDS:
		{
			m_next = nrTextRec;
			m_tType = ttKeywords;
		}
		break;
		case SCI_SETPROPERTY:
		{
			m_next = nrPropRec;
		}
		break;
		case SCI_SETWORDCHARS:
		{
			m_next = nrTextRec;
			m_tType = ttWordChars;
		}
		break;
	}

	Record(Msg, wParam, lParam);
	return 0;
}

bool SchemeRecorder::CheckNecessary(long Msg, WPARAM wParam, LPARAM lParam)
{
	bool res = true;
	bool nOther = false;
    CString fontName;
	switch (Msg)
	{
		case SCI_STYLESETFORE:
			res = (COLORREF)lParam != m_DefStyle.ForeColor;
			break;
		case SCI_STYLESETBACK:
			res = (COLORREF)lParam != m_DefStyle.BackColor;
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
            fontName = (const char*)lParam;
			res = m_DefStyle.FontName.compare(fontName) != 0;
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
	if (!m_out)
	{
		return;
	}

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
			fwrite((const char*)lParam, sizeof(char), txtr.TextLength, m_out);
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
// SchemeLoaderState Implementation
////////////////////////////////////////////////////////////

SchemeLoaderState::~SchemeLoaderState()
{
	for(SchemeDetailsMap::const_iterator i = m_SchemeDetails.begin(); i != m_SchemeDetails.end(); ++i)
	{
		delete (*i).second;
	}

	for(SchemeDetailsMap::const_iterator i = m_BaseSchemeDetails.begin(); i != m_BaseSchemeDetails.end(); ++i)
	{
		delete (*i).second;
	}
}

////////////////////////////////////////////////////////////
// SchemeCompiler Implementation
////////////////////////////////////////////////////////////

void SchemeCompiler::Compile(LPCTSTR path, LPCTSTR outpath, LPCTSTR mainfile)
{
	m_newestFile = 0;

	tstring UserSettingsFile = outpath;
	UserSettingsFile += _T("UserSettings.xml");

	m_LoadState.m_outputPath = outpath;

	SchemeParser::Parse(path, mainfile, UserSettingsFile.c_str());

	// Now we record a default scheme for use when no Scheme is selected. 
	// It has only the basic styles.
	tstring filename(m_LoadState.m_outputPath);
	filename += _T("default.cscheme");

	// Create a scheme details object, this is needed when sendBaseStyles is called.
	SchemeDetails sdDefault("default");
	m_LoadState.m_pCurScheme = &sdDefault;

	m_Recorder.StartRecording("default", _T("default"), filename.c_str(), 0);
	m_Recorder.SetLexer(0);
	
	// Set default and whitespace styles:
	StyleDetails temp(m_LoadState.m_Default);
	temp.Key = STYLE_DEFAULT;
	m_Recorder.SetDefStyle(&temp);
	sendStyle(&temp, &m_Recorder);
	
	temp.Key = 0;
	sendStyle(&temp, &m_Recorder);

	// Send colours and styles for line numbers etc.
	m_LoadState.m_DefaultColours.SendColours(&m_Recorder);
	sendBaseStyles(&m_LoadState);

	m_Recorder.EndRecording();
}

uint64_t SchemeCompiler::GetNewestFileTime() const
{
	return m_newestFile;
}

void SchemeCompiler::onLanguage(LPCSTR name, LPCTSTR title, int foldflags, int /*ncfoldflags*/)
{
	tstring filename(m_LoadState.m_outputPath);
	CA2CT nameconv(name);
	filename += nameconv;
	filename += _T(".cscheme");

	if (!m_Recorder.StartRecording(name, title, filename.c_str(), foldflags))
	{
		tstring msg(_T("Failed to create file "));
		msg += filename;
		msg += _T(", schemes may not work correctly");
		UNEXPECTED(msg.c_str());
	}
	
	m_Recorder.SetDefStyle(&m_LoadState.m_Default);
}

void SchemeCompiler::onCommentSpec(const char* linecomment, const char* streamcommentstart, const char* streamcommentend, 
			const char* commentblockstart, const char* commentblockend, const char* commentblockline)
{
	m_Recorder.WriteCommentBlock(linecomment, streamcommentstart, streamcommentend, commentblockstart, commentblockend, commentblockline);
}

void SchemeCompiler::onLanguageEnd()
{
	if (m_Recorder.IsRecording())
		m_Recorder.EndRecording();
}

void SchemeCompiler::onStyle(const StylePtr& details, bool)
{
	StyleDetails full;
	details->Combine(&m_LoadState.m_Default, full);
	full.Key = details->GetKey();
	//if(pCustom)
	//	customiseStyle(pStyle, pCustom);

	if(full.Key == STYLE_DEFAULT)
	{
		m_Recorder.SetDefStyle(&m_LoadState.m_Default);
	}

	sendStyle(&full, &m_Recorder);
}

void SchemeCompiler::onStyleClass(const StylePtr& details)
{
}

void SchemeCompiler::onProperty(LPCTSTR name, LPCTSTR value)
{
	USES_CONVERSION;

	m_Recorder.SetProperty(CT2CA(name), CT2CA(value));
}

void SchemeCompiler::sendStyle(StyleDetails* s, SchemeRecorder* compiler)
{
    CT2CA fontconv(s->FontName.c_str());
	compiler->StyleSetFont(s->Key, fontconv);
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
	m_newestFile = max(m_newestFile, FileAge(filename));
}

void SchemeCompiler::onKeywords(int key, LPCSTR keywords, LPCTSTR name, LPCSTR custom)
{
	if(custom)
		m_Recorder.SetKeyWords(key, custom);
	else
		m_Recorder.SetKeyWords(key, keywords);
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

void SchemeCompiler::onColours(const EditorColours* defCols, const EditorColours* colours)
{
	// Combine the defaults and the custom colours and send on...
	EditorColours combine = *defCols;
	if(colours)
		combine.Combine(colours);
	combine.SendColours(&m_Recorder);
}

void SchemeCompiler::onError(XMLParserException& ex)
{
	if (m_Recorder.IsRecording())
		m_Recorder.EndRecording();
}

void SchemeCompiler::onWordChars(const char* charset)
{
	m_Recorder.SetWordChars(charset);
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

	//m_LoadState.m_pGroupClass = NULL;
	m_LoadState.m_pParser = &parser;
	m_LoadState.m_basePath = path;

	tstring incfile = path;
	incfile += mainfile;
	m_LoadState.m_IncludeFiles.push_back(incfile);
	
	for(std::list<tstring>::const_iterator i = m_LoadState.m_IncludeFiles.begin(); i != m_LoadState.m_IncludeFiles.end(); ++i)
	{
		m_LoadState.m_State = 0;
		const tstring& file = (*i);

		try
		{
			parser.LoadFile(file.c_str());
			
			onFile(file.c_str());
		}
		catch (SchemeParserException& E)
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
void SchemeParser::processBaseStyle(SchemeLoaderState* pState, const XMLAttributes& atts)
{
	// Create a style with no default settings, the defaults will be applied
	// later as the base style is used for each scheme.
	StyleDetails* pS = new StyleDetails;
	parseStyle(pState, atts, pS);
	StylePtr style(new FullStyleDetails(pS->Key));
	style->Style = pS;

	if(pS->classname.size())
	{
		style->Class = pState->GetClass(pS->classname.c_str());
	}

	pState->m_BaseStyles.push_back(style);
}

void SchemeParser::processProperty(SchemeLoaderState* pState, const XMLAttributes& atts)
{
	LPCTSTR name = atts.getValue(_T("name"));
	LPCTSTR value = atts.getValue(_T("value"));

	if(name != NULL && value != NULL)
	{
		onProperty(name, value);
	}
}

void SchemeParser::processKeywordClass(SchemeLoaderState* pState, const XMLAttributes& atts)
{
	for(int i = 0; i < atts.getCount(); i++)
	{
		if(_tcscmp(atts.getName(i), _T("name")) == 0)
		{
			pState->m_storedName = atts.getValue(i);
			pState->m_State = DOING_KEYWORDS;
			pState->m_CDATA = "";
			break;
		}
	}
}

void SchemeParser::parseStyle(SchemeLoaderState* pState, const XMLAttributes& atts, StyleDetails* pStyle)
{
	LPCTSTR nm;
	LPCTSTR t;
	int c = atts.getCount();

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
			pStyle->FontName = t;
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
	style->layer(*custom);
}

void SchemeParser::processStyleClass(SchemeLoaderState* pState, const XMLAttributes& atts)
{
	//<style-class name="comment" inherit-style="bold" fore="comment-color"/>
	// fore, back, font, size, italics, bold, underline, eolfilled
	
	pState->m_State = DOING_STYLEC;

	LPCTSTR t = atts.getValue(_T("name"));
	
	StylePtr pS;
	StyleDetails* pStyle = NULL;

	if(t != NULL)
	{
		tstring name = t;
		
		pS = pState->GetClass(name.c_str());

		if(!pS.get())
		{
			pS.reset(new NamedStyleDetails(-1));
			pState->m_Classes.insert( StylePtrMap::value_type(name, pS));
		}

		if(!pS->Style)
		{
			pS->Style = new StyleDetails(pState->m_Default);
		}
		
		pStyle = pS->Style;

		// Global styles can have a description. If they do, they can be
		// customised in the global styles options pane....
		t = atts.getValue(_T("description"));
		if(t != NULL)
		{
			static_cast<NamedStyleDetails*>(pS.get())->FriendlyName = t;
		}

		// We can inherit a style from somewhere else
		t = atts.getValue(_T("inherit-style"));
		if(t != NULL)
		{
			StylePtr pE = pState->GetClass(t);
			if(pE.get() && pE->Style != NULL)
				*pStyle = *pE->Style;
		}

		parseStyle(pState, atts, pStyle);

		if (name == _T("default") && pStyle->FontName.size() == 0)
		{
			// No font specified, and this is our default style.
			if (WTL::RunTimeHelper::IsVista())
			{
				pStyle->FontName = _T("Consolas");
			}
			else
			{
				pStyle->FontName = _T("Lucida Console");
			}

			pStyle->values |= edvFontName;
		}

		onStyleClass(pS);

		if(name == _T("default"))
		{
			//special case default class...
			pS->Combine(NULL, pState->m_Default);
		}
	}
}

void SchemeParser::processLanguageStyleGroup(SchemeLoaderState* pState, const XMLAttributes& atts)
{
	StylePtr pClass;
	LPCTSTR pszClass = atts.getValue(_T("class"));

	if(!pState->m_bBaseParse)
	{
		if(pszClass)
		{
			pClass = pState->GetClass(pszClass);
			if(pClass.get())
				pState->m_pGroupClass = pClass;
		}

		onStyleGroup(atts, pClass);
	}
	else
	{
		pState->m_pBase->BeginStyleGroup( atts.getValue(_T("name")), atts.getValue(_T("description")), NULL );
	}
}

/**
 * Style Classes are found in this order of priority:
 * 1) Customised style class attribute.
 * 2) Class assigned to property group.
 * 3) Class in the current style class attribute.
 * 4) Default style.
 */
void SchemeParser::processLanguageStyle(SchemeLoaderState* pState, const XMLAttributes& atts)
{
	LPCTSTR classname;

	LPCTSTR skey = atts.getValue(_T("key"));
	int key = _ttoi(skey);

	StylePtr style = pState->m_pCurScheme->GetStyle(key);

	if(pState->m_pGroupClass != NULL)
	{
		// There is a class associated with a group of styles.
		// We also don't need to find the style, it will already
		// be the m_pGroupClass member of pState.
		style->GroupClass = pState->m_pGroupClass;
	}
	
	classname = atts.getValue(_T("class"));

	// We've not found a class yet, but if we do have a class name, we try to find that.
	if(classname && (_tcslen(classname) > 0) && (_tcscmp(classname, _T("default")) != 0))
	{
		style->Class = pState->GetClass(classname);
	}

	style->Style = new StyleDetails;
	
	// Read the details into it from it's attributes.
	parseStyle(pState, atts, style->Style);
	
	if(!pState->m_bBaseParse)
	{
		// Pass it to whatever wants to know about it.
		onStyle(style, false);//&Style, pCustom);
	}
	/*else
	{
		// I think that if base schemes use the SchemeDetails then we can
		// just store the styles there like usual and pull them later.
		StyleDetails* pStored = new StyleDetails(*style->Style);
		pState->m_pBase->AddStyle(pStored);
	}*/
}

void SchemeParser::processLanguageKeywords(SchemeLoaderState* pState, const XMLAttributes& atts)
{
	 //<keyword key="0" class="hypertext"/>
	int x = atts.getCount();
	tstring name;
	LPCTSTR val = NULL;
	std::string kw = "";
	tstring namestr;
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
			tstring_string_map::const_iterator z = pState->m_Keywords.find(tstring(val));
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

	LPCSTR custom = NULL;
	CustomKeywordSet* pCustom = pState->m_pCurScheme->CustomKeywords.FindKeywordSet(key);
	if(pCustom)
		custom = pCustom->pWords;

	if(/*kw != _T("") && */key != -1) // We want empty keyword sets for customisation.
	{
		if(!pState->m_bBaseParse)
		{
			onKeywords(key, kw.c_str(), namestr.c_str(), custom);
		}
		else
		{
			CustomKeywordSet* pSet = new CustomKeywordSet;
			pSet->key = key;
			
			int sLen = namestr.length();
			if(sLen != 0)
			{
				pSet->pName = new TCHAR[sLen + 1];
				_tcscpy(pSet->pName, namestr.c_str());
			}
			
			sLen = kw.length();
			if(sLen != 0)
			{
                pSet->pWords = new char[sLen + 1];
				strcpy(pSet->pWords, kw.c_str());
			}

			pState->m_pBase->Keywords.AddKeywordSet(pSet);
		}
	}
}

SchemeDetails* ensureSchemeDetails(SchemeDetailsMap& map, const std::string& name)
{
	SchemeDetailsMap::const_iterator i = map.find( name );
	if( i != map.end() )
	{
		return (*i).second;
	}
	else
	{
		SchemeDetails* n = new SchemeDetails( name.c_str() );
		map.insert( SchemeDetailsMap::value_type( name, n ) );
		return n;
	}
}

SchemeDetails* ensureBaseSchemeDetails(SchemeDetailsMap& map, const std::string& name)
{
	SchemeDetailsMap::const_iterator i = map.find( name );
	if( i != map.end() )
	{
		return (*i).second;
	}
	else
	{
		SchemeDetails* n = new BaseScheme( name.c_str() );
		map.insert( SchemeDetailsMap::value_type( name, n ) );
		return n;
	}
}

/**
 * @brief Process some XML element related to the "language" block of a schemes file.
 */
void SchemeParser::processLanguageElement(SchemeLoaderState* pState, LPCTSTR name, const XMLAttributes& atts)
{
	LPCTSTR t = NULL;
	int flags = 0;
	int ncflags = 0;
	std::string wordchars;
	BaseScheme* pBase = NULL;

	if(pState->m_State == DOING_LANGUAGE && 
		(_tcscmp(name, _T("language")) == 0 || _tcscmp(name, _T("schemedef")) == 0 || 
		_tcscmp(name, _T("base-language")) == 0) )
	{
		LPCTSTR schval = atts.getValue(_T("name"));
		if(schval != NULL)
		{
			// Make sure scheme name is only 10 characters long
			CT2CA schemenameconv(schval);
			std::string scheme(schemenameconv);
			if (scheme.length() > SC_HDR_NAMESIZE)
			{
				scheme.resize(SC_HDR_NAMESIZE);
			}

			LPCTSTR titval = atts.getValue(_T("title"));
			tstring title;
			if (titval != NULL)
			{
				title = titval;
				if (title.length() > SC_HDR_TITLESIZE)
				{
					title.resize(SC_HDR_TITLESIZE);
				}
			}

			pState->m_StartLang = ::GetTickCount(); // diags.

			pState->m_langName = scheme;

			if(!pState->m_bBaseParse)
			{
				pState->m_pCurScheme = ensureSchemeDetails(
					pState->m_SchemeDetails, 
					pState->m_langName);
			}
			else
			{
				pState->m_pCurScheme = ensureBaseSchemeDetails(
					pState->m_BaseSchemeDetails,
					pState->m_langName);
			}

			LPCTSTR base = atts.getValue(_T("base"));
			if(base != NULL)
			{
				// The language has a base-language reference.
				CT2CA basename(base);
				SchemeDetailsMap::iterator iBase = pState->m_BaseSchemeDetails.find(std::string(basename));
				if( iBase != pState->m_BaseSchemeDetails.end() )
				{
					pBase = static_cast<BaseScheme*>( (*iBase).second );
					flags = pBase->flags;
					wordchars = pBase->wordchars;
				}
			}
			
			t = atts.getValue(_T("folding"));
			if(t != NULL && PNStringToBool(t))
			{
				//fldEnabled = 1, fldCompact = 2, fldComments = 4, fldPreProc = 8, fldElse = 16
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

				t = atts.getValue(_T("foldelse"));
				if(t != NULL && PNStringToBool(t))
					flags |= fldElse;
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

			t = atts.getValue(_T("wordchars"));
			if (t != NULL && t[0] != NULL)
			{
				CT2CA wordcharsconv(t);
				wordchars = wordcharsconv;
			}

			// Store the non-customised flags to pass on
			ncflags = flags;

			// Only look for customisations if this is an actual scheme.
			if(!pState->m_bBaseParse)
			{
				// Final flags stage, see if the user has removed some.
				if(pState->m_pCurScheme->CustomFlagFlags & schOverrideTabs)
				{
					flags &= ~(schOverrideTabs|schUseTabs);
					flags |= (pState->m_pCurScheme->CustomFlags & (schOverrideTabs|schUseTabs));
				}
				
				if(pState->m_pCurScheme->CustomFlagFlags & schOverrideTabSize)
				{
					flags &= ~(schOverrideTabSize);
					flags |= (pState->m_pCurScheme->CustomFlags & schOverrideTabSize);
				}
			}

			if(!pState->m_bBaseParse)
			{
				// Signal the implementing class that there's a language (scheme) coming.
				onLanguage(scheme.c_str(), title.c_str(), flags, ncflags);
				
				if (wordchars.length())
				{
					onWordChars(wordchars.c_str());
				}

				if( pBase )
				{
					CT2CA basename(base);
					sendBaseScheme(pState, pBase, basename);
				}
			}
			else
			{
				pState->m_pBase = static_cast<BaseScheme*>( pState->m_pCurScheme );
				pState->m_pBase->flags = flags;

				pState->m_pBase->wordchars = wordchars.c_str();
			}

			pState->m_State = DOING_LANGUAGE_DETAILS;
		}
	}
	else
	{
		PNASSERT(pState->m_State == DOING_LANGUAGE_DETAILS);

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
					//throw SchemeParserException(pState->m_pParser, _T("Style Bits value not valid (0 or non-numeric)"));
					LOG(_T("Style Bits value not valid (0 or non-numeric)"));
					sbits = 5;
				}
			}

			if(!pState->m_bBaseParse)
			{
				onLexer(lexer, sbits);
			}
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
		else if(_tcscmp(name, _T("comments")) == 0)
		{
			processComments(pState, atts);
		}
	}
}

/**
 * @brief Add a specific file to the list of files to be processed.
 */
void SchemeParser::specifyImportFile(SchemeLoaderState* pState, const XMLAttributes& atts)
{
	LPCTSTR name = atts.getValue(_T("name"));
	if(name != NULL)
	{
		tstring filename = pState->m_basePath;
		filename += name;
		pState->m_IncludeFiles.insert(pState->m_IncludeFiles.end(), filename.c_str());
	}
	else
	{
		throw SchemeParserException(pState->m_pParser, _T("Import element with no name attribute."));
	}
}

void SchemeParser::sendBaseScheme(SchemeLoaderState* pState, BaseScheme* pBase, LPCSTR baseName)
{
	GroupDetailsList::const_iterator iGroup = pBase->GroupDetails.begin();

	if( (pBase->valuesSet & ebvLexer) != 0 )
	{
		onLexer(pBase->lexer.c_str(), pBase->styleBits);
	}

	SchemeDetailsMap::const_iterator i = pState->m_BaseSchemeDetails.find( std::string(baseName) );
	if(i != pState->m_BaseSchemeDetails.end())
	{
		SchemeDetails* sd = (*i).second;

		for(StylePtrList::const_iterator i = sd->Styles.begin(); i != sd->Styles.end(); ++i)
		{
			StyleDetails* pS = (*i)->Style;
			
			// See if this is a dummy style to mark a group start...
			if( (pS->values & edvGroupStart) != 0 )
			{
				StylePtr pClass;
				
				if( pS->classname.length() != 0 )
				{
					pClass = pState->GetClass(pS->classname.c_str());
					if( pClass.get() )
						pState->m_pGroupClass = pClass;
				}

				if( iGroup != pBase->GroupDetails.end() )
				{
					LPCTSTR attstr[5];
					attstr[0] = _T("name");
					attstr[1] = (*iGroup).name.c_str();
					attstr[2] = _T("description");
					attstr[3] = (*iGroup).description.c_str();
					attstr[4] = NULL;
					XMLAttributes atts(&attstr[0]);

					onStyleGroup(atts, pClass);

					++iGroup;
				}
			}
			// or a dummy style to mark a group end
			else if( (pS->values & edvGroupEnd) != 0 )
			{
				onStyleGroupEnd();
				pState->m_pGroupClass.reset();
			}
			// otherwise it's a bonafide style
			else
			{
				// Copy the base style to customise for this instance:
				
				StylePtr p = pState->m_pCurScheme->GetCustomStyle(pS->Key);
				if(!p)
				{
					p.reset(new FullStyleDetails( *(*i) ));
				}
				else
				{
					p->Style = new StyleDetails(*pS);
				}

				onStyle(p, true);
			}
		}
	}
}

/**
 * Send all of the base styles for the current scheme.
 */
void SchemeParser::sendBaseStyles(SchemeLoaderState* pState)
{
	LPCTSTR attstr[5];
	attstr[0] = _T("name");
	attstr[1] = _T("Common");
	attstr[2] = _T("description");
	attstr[3] = _T("Common styles and colours.");
	attstr[4] = NULL;
	XMLAttributes atts(&attstr[0]);

	onStyleGroup(atts, StylePtr());
	
	for(StylePtrList::const_iterator i = pState->m_BaseStyles.begin();
		i != pState->m_BaseStyles.end();
		++i)
	{
		StylePtr p = pState->m_pCurScheme->GetStyle( (*i)->GetKey() );
		p->Style = new StyleDetails( *(*i)->Style );
		p->Class = (*i)->Class;
		
		onStyle(p, false);
	}

	onStyleGroupEnd();
}

void SchemeParser::processComments(SchemeLoaderState* pState, const XMLAttributes& atts)
{
	LPCTSTR lineComment(atts.getValue(_T("line")));
	LPCTSTR startComment(atts.getValue(_T("streamStart")));
	LPCTSTR endComment(atts.getValue(_T("streamEnd")));
	LPCTSTR startBlock(atts.getValue(_T("blockStart")));
	LPCTSTR endBlock(atts.getValue(_T("blockEnd")));
	LPCTSTR blockLine(atts.getValue(_T("blockLine")));

	CT2CA lineCommentConv(lineComment);
	CT2CA startCommentConv(startComment);
	CT2CA endCommentConv(endComment);
	CT2CA startBlockConv(startBlock);
	CT2CA endBlockConv(endBlock);
	CT2CA blockLineConv(blockLine);

	onCommentSpec(lineCommentConv, startCommentConv, endCommentConv, startBlockConv, endBlockConv, blockLineConv);
}

/**
 * @brief Take an import fileset specification and add the relevant files to be processed.
 */
void SchemeParser::specifyImportSet(SchemeLoaderState* pState, const XMLAttributes& atts)
{
	LPCTSTR pattern = atts.getValue(_T("pattern"));

	if (pattern != NULL)
	{
		FileFinderList finder;
		std::list<tstring> files = finder.GetFiles(pState->m_basePath.c_str(), pattern, false);

		for (std::list<tstring>::const_iterator i = files.begin(); i != files.end(); ++i)
		{
			if (!boost::iends_with((*i), _T("master.scheme")))
			{
				CFileName fn(*i);
				fn.Root(pState->m_basePath.c_str());
				pState->m_IncludeFiles.push_back(fn.c_str());
			}
		}
	}
	else
	{
		throw SchemeParserException(pState->m_pParser, _T("Set element with no pattern attribute."));
	}
}

void SchemeParser::processKeywordCombine(SchemeLoaderState* pState, const XMLAttributes& atts)
{
	pState->m_State = DOING_KEYWORDCOMBINE;

	LPCTSTR name = atts.getValue(_T("name"));
	if(name != NULL)
	{
		tstring_string_map::const_iterator z = pState->m_Keywords.find(tstring(name));
		if(z != pState->m_Keywords.end())
		{
			pState->m_CDATA += (*z).second.c_str();
		}
		else
		{
			throw SchemeParserException(pState->m_pParser, _T("Unmatched keyword class inclusion."));
		}
	}
	else
	{
		throw SchemeParserException(pState->m_pParser, _T("include-class element with no name attribute"));
	}
}

void SchemeParser::startElement(void *userData, LPCTSTR name, const XMLAttributes& atts)
{
	SchemeLoaderState* pState = static_cast<SchemeLoaderState*>(userData);
	int state = pState->m_State;

	tstring stattext;

	if(state == DOING_KEYWORDC && _tcscmp(name, _T("keyword-class")) == 0)
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
	else if(state == DOING_BASE_OPTIONS && _tcscmp(name, _T("style")) == 0)
	{
		processBaseStyle(pState, atts);
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
		stattext = _T("Unknown Start Element: ");
		stattext += name;
	}

	if(!stattext.empty())
		LOG(stattext.c_str());
}

void SchemeParser::endElement(void *userData, LPCTSTR name)
{
	SchemeLoaderState* pS = static_cast<SchemeLoaderState*>(userData);
	
	int state = pS->m_State;

	tstring stattext;

	if(state == DOING_STYLEC)
	{
		pS->m_State = DOING_STYLECS;
	}
	else if(state == DOING_KEYWORDS)
	{
		if(pS->m_CDATA.length() > 0)
		{
			std::string kw = NormaliseKeywords( std::string(pS->m_CDATA) );

			pS->m_Keywords.insert(pS->m_Keywords.end(), tstring_string_map::value_type(pS->m_storedName, kw));

#ifdef _DEBUG
			stattext = _T("Added Keyword Class: ");
			stattext += pS->m_storedName.c_str();
			LOG(stattext.c_str());
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

			// Get the default editor colours
			EditorColours* ec = &pS->m_DefaultColours;
			EditorColours* ecCust = NULL;

			// See if there's any customised editor colours
			if( pS->m_pCurScheme->CustomColours.HasColours() )
				ecCust = &pS->m_pCurScheme->CustomColours;

			// Set those colours!
			onColours(ec, ecCust);

			onLanguageEnd();

			DWORD dwTimeDiff = ::GetTickCount() - pS->m_StartLang;
			TCHAR buf[300];
			_sntprintf(buf, 300, _T("Language Load (%s): %dms\n"), pS->m_langName.c_str(), dwTimeDiff);
			LOG(buf);
			
			pS->m_State = 0;

			pS->m_pCurScheme = NULL;
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
			pS->m_pGroupClass.reset();
			
			if(!pS->m_bBaseParse)
			{
				onStyleGroupEnd();
			}
			else
			{
				// Add a dummy style to mark the end of the group.
				StylePtr pStyle(new FullStyleDetails(-1));
				pStyle->Style = new StyleDetails;
				pStyle->Style->values = edvGroupEnd;
				pS->m_pBase->Styles.push_back(pStyle);
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
		pS->m_CDATA = "";
}

///@todo perhaps change this to use strncat and go straight into the CData buffer...
void SchemeParser::characterData(void* userData, LPCTSTR data, int len)
{
	SchemeLoaderState* pState = static_cast<SchemeLoaderState*>(userData);

	tstring cdata(data, len);
	CT2CA convert(cdata.c_str());
	pState->m_CDATA += convert;
}