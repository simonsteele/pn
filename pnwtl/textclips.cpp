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

namespace TextClips {

//////////////////////////////////////////////////////////////////////////////
// TextClipSet
//////////////////////////////////////////////////////////////////////////////

TextClipSet::TextClipSet(LPCTSTR filename)
{
	parse(filename);
}

void TextClipSet::parse(LPCTSTR filename)
{
	parseState = 0;
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
	if IN_STATE(TCPS_START)
	{
		MATCH_ELEMENT(_T("clips"))
		{
			// get name attribute...
			SET_STATE(TCPS_CLIPS);
		}
	}
	else if IN_STATE(TCPS_CLIPS)
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
	if IN_STATE(TCPS_CLIP)
	{
		// Create new clip - name = curName, content = cData;
		Clip* clip = new TextClip;
		clip->Name = curName;
		clip->Text = cData;

		///@todo implement this, and remove the delete.
		delete clip;

		SET_STATE(TCPS_CLIPS);
	}
	else if IN_STATE(TCPS_CLIPS)
	{
		SET_STATE(TCPS_START);
	}

	cData = _T("");
}

void TextClipSet::characterData(LPCTSTR data, int len)
{
	if IN_STATE(TCPS_CLIP)
	{
		CString cdata;
		TCHAR* buf = cdata.GetBuffer(len+1);
		_tcsncpy(buf, data, len);
		buf[len] = 0;
		cdata.ReleaseBuffer();

		cData += cdata;
	}
}

} // namespace TextClips