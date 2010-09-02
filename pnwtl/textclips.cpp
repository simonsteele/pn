/**
 * @file textclips.cpp
 * @brief text clips functionality.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "textclips.h"
#include "textclips/clipmanager.h"
#include "textclips/clipwriter.h"
#include "filename.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

namespace TextClips {

//////////////////////////////////////////////////////////////////////////////
// TextClipSet
//////////////////////////////////////////////////////////////////////////////

TextClipSet::TextClipSet(LPCTSTR filename, LPCTSTR name, LPCSTR scheme, bool encodeClipNames) :
	m_encodeClipNames(encodeClipNames)
{
	if (filename != NULL)
	{
		m_filename = filename;
	}

	if (name != NULL)
	{
		m_name = name;
	}
	
	if (scheme != NULL)
	{
		m_scheme = scheme;
	}
}

TextClipSet::TextClipSet(const TextClipSet& copy)
{
	m_filename = copy.m_filename;
	m_name = copy.m_name;
	m_scheme = copy.m_scheme;
	m_encodeClipNames = copy.m_encodeClipNames;

	for(LIST_CLIPS::const_iterator i = copy.m_clips.begin();
		i != copy.m_clips.end();
		++i)
	{
		m_clips.push_back(new Clip(**i));
	}
}

TextClipSet::~TextClipSet()
{
	clear();
}

const Clip* TextClipSet::FindByShortcut(const std::string& shortcut) const
{

	for (LIST_CLIPS::const_iterator i = m_clips.begin();
		i != m_clips.end();
		++i)
	{
		if ( (*i)->Shortcut == shortcut )
		{
			return (*i);
		}
	}

	return NULL;
}

const LIST_CLIPS& TextClipSet::GetClips() const
{
	return m_clips;
}

LPCTSTR TextClipSet::GetName() const
{
	return m_name.c_str();
}

LPCTSTR TextClipSet::GetFilename() const
{
	if (m_filename.size())
	{
		return m_filename.c_str();
	}
	else
	{
		return _T("");
	}
}

void TextClipSet::SetFilename(LPCTSTR filename)
{
	m_filename = filename;
}

LPCSTR TextClipSet::GetScheme() const
{
	if (m_scheme.empty())
		return NULL;
	return m_scheme.c_str();
}

/**
 * Should we encode text clip names?
 */
bool TextClipSet::GetEncodeClipNames() const
{
	return m_encodeClipNames;
}

void TextClipSet::Remove(TextClips::Clip* clip)
{
	m_clips.remove(clip);
}

void TextClipSet::Save()
{
	CFileName fn(m_filename.c_str());
	if (!DirExists(fn.GetPath().c_str()))
	{
		CreateDirectoryRecursive(fn.GetPath().c_str(), NULL);
	}

	TextClipsWriter writer;
	if (!writer.Start(m_filename.c_str()))
	{
		UNEXPECTED(_T("Failed to open text clip set file for writing."));
		return;
	}

	writer.WriteClipSet(this);
	writer.Close();
}

void TextClipSet::clear()
{
	for(LIST_CLIPS::iterator i = m_clips.begin(); i != m_clips.end(); ++i)
	{
		delete (*i);
	}

	m_clips.clear();
}

void TextClipSet::Add(Clip* clip)
{
	m_clips.push_back(clip);
	m_bDirty = true;
}


} // namespace TextClips