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