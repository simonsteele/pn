/**
 * @file textclips.cpp
 * @brief text clips functionality.
 * @author Simon Steele
 * @note Copyright (c) 2002-2003 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "textclips.h"
#include "include/filefinder.h"
#include "include/encoding.h"

namespace TextClips {

//////////////////////////////////////////////////////////////////////////////
// Clip
//////////////////////////////////////////////////////////////////////////////

void Clip::Insert(CScintilla *scintilla)
{
	tstring clipstr;
	tstring theText(Text);
	tstring theIndent;

	// Indentation:
	int indentation = scintilla->GetLineIndentation(scintilla->LineFromPosition(scintilla->GetCurrentPos()));
	
	// Work out tabs and spaces combination to get the indentation right
	// according to user's settings.
	theIndent = MakeIndentText( indentation, scintilla->GetUseTabs(), scintilla->GetTabWidth() );

	for(size_t i = 0; i < theText.size(); i++)
	{
		if(theText[i] == '\n')
		{
			theText.insert(i+1, theIndent);
			i += theIndent.size();
		}
	}
	
	// Text is in Unix EOL mode (LF only), convert it to target EOL mode
	switch(scintilla->GetEOLMode())
	{
	case PNSF_Unix:
		// no conversion needed, just copy
		clipstr = theText;
		break;

	case PNSF_Windows:
		{
			// heuristically reserve size for the target string
			// and copy the string by inserting '\r' where appropriate
			clipstr.reserve(theText.size() + (theText.size() / 16));
			tstring::const_iterator it = theText.begin();
			tstring::const_iterator end = theText.end();
			for(; it != end; ++it)
			{
				if(*it == '\n')
					clipstr += '\r';
				clipstr += *it;
			}
		}
		break;

	case PNSF_Mac:
		// reserve size for the target string and use standard algorithm
		clipstr.reserve(theText.size());
		std::replace_copy(theText.begin(), theText.end(),
			std::back_inserter(clipstr), '\n', '\r');
		break;
	}
	
	size_t offset = clipstr.find(_T('|'));
	if(offset != clipstr.npos)
		clipstr.erase(offset, 1);
	else
		offset = 0;
	
	int curPos = scintilla->GetCurrentPos();

	int length = scintilla->GetSelLength();
	if(length != 0)
	{
		// Get the selected text from Scintilla.
		char* selData = new char[length+1];
		int rxlen = scintilla->GetSelText(selData);
		PNASSERT(rxlen == length+1);
		
		// Insert the text into the buffer.
		clipstr.insert(offset, selData);
		delete [] selData;
		
		// Adjust the offset to place the cursor after the selected text.
		offset += length;
	}

	// Wrap everything in an undo block.
	scintilla->BeginUndoAction();
	if(length)
	{
		scintilla->DeleteBack(); // kill the selection text, we're inserting it again.
		curPos = scintilla->GetCurrentPos();
	}
	scintilla->InsertText(curPos, clipstr.c_str());
	scintilla->SetCurrentPos(curPos + offset);
	scintilla->SetSel(curPos + offset, curPos + offset);
	scintilla->EndUndoAction();
}

//////////////////////////////////////////////////////////////////////////////
// TextClipSet
//////////////////////////////////////////////////////////////////////////////

TextClipSet::TextClipSet(LPCTSTR filename)
{
	parse(filename);
}

TextClipSet::~TextClipSet()
{
	clear();
}

const LIST_CLIPS& TextClipSet::GetClips()
{
	return clips;
}

LPCTSTR TextClipSet::GetName()
{
	return name.c_str();
}

void TextClipSet::clear()
{
	encoding = eNone;
	decodeNames = false;

	for(LIST_CLIPS::iterator i = clips.begin(); i != clips.end(); ++i)
	{
		delete (*i);
	}

	clips.clear();
}

void TextClipSet::parse(LPCTSTR filename)
{
	XMLParser parser;
	parser.SetParseState(this);
	
	clear();
	parseState = 0;
	
	try
	{
		parser.LoadFile(filename);
	}
	catch( XMLParserException& ex )
	{
		::OutputDebugString(ex.GetMessage());
	}
}

#define TCPS_START	0
#define TCPS_CLIPS	1
#define TCPS_CLIP	2

#define MATCH_ELEMENT(ename) \
	if(_tcscmp(name, ename) == 0)

#define IN_STATE(state) \
	(parseState == state)

#define SET_STATE(state) \
	parseState = state

void TextClipSet::startElement(LPCTSTR name, XMLAttributes& atts)
{
	if( IN_STATE(TCPS_START) )
	{
		MATCH_ELEMENT(_T("clips"))
		{
			// get name attribute...
			SET_STATE(TCPS_CLIPS);

			LPCTSTR szName = atts.getValue(_T("name"));
			if(szName != NULL)
				this->name = szName;
			else
				this->name = _T("(unknown)");

			LPCTSTR szEncoding = atts.getValue(_T("encoding"));
			if(szEncoding != NULL)
			{
				if(_tcscmp(szEncoding, _T("windows-1252")) == 0)
					encoding = eWindows1252;
				else if(_tcscmp(szEncoding, _T("ansi")) == 0)
					encoding = eANSI;
			}

			szEncoding = atts.getValue(_T("decodeNames"));
			if(szEncoding != NULL)
				if(szEncoding[0] == _T('t') || szEncoding[0] == _T('T'))
					decodeNames = true;
		}
	}
	else if( IN_STATE(TCPS_CLIPS) )
	{
		MATCH_ELEMENT(_T("clip"))
		{
			LPCTSTR pName = atts.getValue(_T("name"));
			curName = (pName != NULL ? pName : _T("error"));
			SET_STATE(TCPS_CLIP);
		}
	}
}

void TextClipSet::endElement(LPCTSTR name)
{
	if( IN_STATE(TCPS_CLIP) )
	{
		// Create new clip - name = curName, content = cData;
		Clip* clip = new Clip;
		if(decodeNames)
#ifndef _UNICODE
			clip->Name = Utf8_Windows1252( curName.c_str() );
#else
			//todo 
			clip->Name = curName;
#endif
		else
			clip->Name = curName;
		
		if(encoding != eNone)
			decodeData();

		clip->Text = cData;

		///@todo implement this, and remove the delete.
		clips.insert(clips.end(), clip);

		SET_STATE(TCPS_CLIPS);
	}
	else if( IN_STATE(TCPS_CLIPS) )
	{
		SET_STATE(TCPS_START);
	}

	cData = _T("");
}

void TextClipSet::characterData(LPCTSTR data, int len)
{
	if( IN_STATE(TCPS_CLIP) )
	{
		CString cdata;
		TCHAR* buf = cdata.GetBuffer(len+1);
		_tcsncpy(buf, data, len);
		buf[len] = 0;
		cdata.ReleaseBuffer();

		cData += cdata;
	}
}

void TextClipSet::decodeData()
{
	// Until we have UTF-16 to 'x' conversion routines these won't work.
	#ifndef _UNICODE
	switch(encoding)
	{
		case eWindows1252:
		{
			Utf8_Windows1252 conv( cData.c_str() );
			if(conv.IsValid())
				cData = (const char*)conv;
		}
		break;
		case eANSI:
		{
			Utf8_ANSI conv( cData.c_str() );
			if(conv.IsValid())
				cData = (const char*)conv;
		}
		break;
	}
	#endif
}

//////////////////////////////////////////////////////////////////////////////
// TextClipsManager
//////////////////////////////////////////////////////////////////////////////

TextClipsManager::TextClipsManager()
{
	FindClips();
}

TextClipsManager::~TextClipsManager()
{
	clear();
}

const LIST_CLIPSETS& TextClipsManager::GetClipSets()
{
	return clipSets;
}

/**
 * Uses FileFinder to find the clips files.
 */
void TextClipsManager::FindClips()
{
	FileFinder<TextClipsManager> finder(this, &TextClipsManager::OnFound);

	tstring path;
	
	// Search distribution clips directory.
	OPTIONS->GetPNPath(path, PNPATH_CLIPS);
	if( DirExists(path.c_str()) )
		finder.Find(path.c_str(), _T("*.clips"));
	
	// Search user clips directory.
	OPTIONS->GetPNPath(path, PNPATH_USERCLIPS);
	if( DirExists(path.c_str()) )
		finder.Find(path.c_str(), _T("*.clips"));
}

/**
 * Called by the file finder class, creates a new TextClipSet from
 * the given file path.
 */
void TextClipsManager::OnFound(LPCTSTR path, LPCTSTR filename)
{
	tstring fullpath(path);
	fullpath += filename;
	TextClipSet* tcs = new TextClipSet(fullpath.c_str());
	clipSets.insert(clipSets.end(), tcs);
}

/**
 * Free all the TextClipSet instances and clear the list.
 */
void TextClipsManager::clear()
{
	for(LIST_CLIPSETS::iterator i = clipSets.begin(); i != clipSets.end(); ++i)
	{
		delete (*i);
	}

	clipSets.clear();
}

} // namespace TextClips