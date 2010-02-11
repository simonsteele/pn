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
#include "include/filefinder.h"
#include "include/encoding.h"
#include "third_party/genx/genx.h"
#include "include/pngenx.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

namespace TextClips {

//////////////////////////////////////////////////////////////////////////////
// TextsClipWriter
//////////////////////////////////////////////////////////////////////////////

class TextClipsWriter : public GenxXMLWriter
{
public:
	void BeginClipSets()
	{
		genxStartElement(m_eClipSets);
	}
	
	void EndClipSets()
	{
		genxEndElement(m_writer);
	}

	void WriteClipSet(const TextClipSet* clips)
	{
		beginClips(clips->GetName(), clips->GetScheme(), clips->GetEncodeClipNames());

		for(LIST_CLIPS::const_iterator i = clips->GetClips().begin();
			i != clips->GetClips().end();
			++i)
		{
			writeClip((*i));
		}

		endClips();
	}

protected:
	virtual void initXmlBits()
	{
		genxStatus s;

		m_eClipSets = genxDeclareElement(m_writer, NULL, u("clipSets"), &s);
		m_eClips = genxDeclareElement(m_writer, NULL, u("clips"), &s);
		m_eClip = genxDeclareElement(m_writer, NULL, u("clip"), &s);

		PREDECLARE_ATTRIBUTES()
			ATT("name", m_aName);
			ATT("scheme", m_aScheme);
			ATT("shortcut", m_aShortcut);
			ATT("decodeNames", m_aDecodeNames);
		END_ATTRIBUTES();
	}

private:
	void beginClips(const TCHAR* name, const char* scheme, bool decodeNames)
	{
		genxStartElement(m_eClips);
		addAttributeConvertUTF8(m_aName, name);
		
		if (scheme != NULL)
		{
			addAttributeConvertUTF8(m_aScheme, scheme);
		}

		if (decodeNames)
		{
			genxAddAttribute(m_aDecodeNames, u("true"));
		}
	}

	void endClips()
	{
		genxEndElement(m_writer);
	}

	void writeClip(Clip* clip)
	{
		genxStartElement(m_eClip);
		addAttributeConvertUTF8(m_aName, clip->Name.c_str());
		addAttributeConvertUTF8(m_aShortcut, clip->Shortcut.c_str());
		
		Windows1252_Utf8 conv(clip->Text.c_str());
		genxAddText(m_writer, conv);

		genxEndElement(m_writer);
	}

	genxElement m_eClipSets;
	genxElement m_eClips;
	genxElement m_eClip;
	genxAttribute m_aName;
	genxAttribute m_aScheme;
	genxAttribute m_aShortcut;
	genxAttribute m_aDecodeNames;
};

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

bool SortClipsByShortcut(const Clip* p1, const Clip* p2)
{
	return (p1->Shortcut < p2->Shortcut);
}

std::string TextClipSet::BuildSortedClipList() const
{
	std::string result;
	
	LIST_CLIPS sortedClips = m_clips;
	sortedClips.sort(SortClipsByShortcut);
	
	for (LIST_CLIPS::const_iterator i = sortedClips.begin();
		i != sortedClips.end();
		++i)
	{
		if (result.size())
			result += ",";

		result += (*i)->Shortcut;
		CT2CA name((*i)->Name.c_str());
		result += ": ";
		result += name;
	}

	return result;
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
		return NULL;
	}
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
	TextClipsWriter writer;
	writer.Start(m_filename.c_str());
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
}

//////////////////////////////////////////////////////////////////////////////
// TextClipsManager
//////////////////////////////////////////////////////////////////////////////

TextClipsManager::TextClipsManager()
{
	findClips();
}

/**
 * Copy constructor - deep copy
 */
TextClipsManager::TextClipsManager(const TextClipsManager& other)
{
	copy(other);
}

TextClipsManager::~TextClipsManager()
{
	clear();
}

const LIST_CLIPSETS& TextClipsManager::GetClipSets()
{
	return m_clipSets;
}

TextClipSet* TextClipsManager::GetClips(LPCSTR schemeName)
{
	MAP_CLIPSETS::const_iterator i = m_schemeClipSets.find(std::string(schemeName));
	
	if (i != m_schemeClipSets.end())
	{
		return (*i).second;
	}
	
	return NULL;
}

void TextClipsManager::Add(TextClipSet* clips)
{
	if(clips->GetScheme() != NULL)
	{
		m_schemeClipSets.insert(MAP_CLIPSETS::value_type(std::string(clips->GetScheme()), clips));
	}
	else
	{
		m_clipSets.push_back(clips);
	}
}

/**
 * First save all cached clip sets, then save those in their
 * own files.
 */
void TextClipsManager::Save(bool ignoreFilenames)
{
	LIST_CLIPSETS toSave;

	tstring usPath;
	OPTIONS->GetPNPath(usPath, PNPATH_USERCLIPS);

	tstring clipcache = usPath + _T("installClipCache.xml");

	TextClipsWriter writer;
	writer.Start(clipcache.c_str());
	writer.BeginClipSets();

	// iterate through and store the clips that need saving into a file other
	// than the cache file in toSave, then save them.

	for (LIST_CLIPSETS::iterator i = m_clipSets.begin(); i != m_clipSets.end(); ++i)
	{
		LPCTSTR filename = (*i)->GetFilename();
		if (!ignoreFilenames && filename != NULL)
		{
			toSave.push_back(*i);
		}
		else
		{
			writer.WriteClipSet(*i);
		}
	}

	for (MAP_CLIPSETS::iterator j = m_schemeClipSets.begin(); j != m_schemeClipSets.end(); ++j)
	{
		LPCTSTR filename = (*j).second->GetFilename();
		if (!ignoreFilenames && filename != NULL)
		{
			toSave.push_back((*j).second);
		}
		else
		{
			writer.WriteClipSet((*j).second);
		}
	}

	writer.EndClipSets();
	writer.Close();

	// Save the clip sets that have their own file
	for (LIST_CLIPSETS::iterator k = toSave.begin(); k != toSave.end(); ++k)
	{
		(*k)->Save();
	}
}

void TextClipsManager::Reset(const TextClipsManager& other)
{
	clear();
	copy(other);
}

/**
 * Called by the file finder class, creates a new TextClipSet from
 * the given file path.
 */
void TextClipsManager::OnFound(LPCTSTR path, FileFinderData& file, bool& /*shouldContinue*/)
{
	tstring fullpath(path);
	fullpath += file.GetFilename();
	m_curFileName = fullpath;
	parse(fullpath.c_str());
}

/**
 * Free all the TextClipSet instances and clear the list.
 */
void TextClipsManager::clear()
{
	for (LIST_CLIPSETS::iterator i = m_clipSets.begin(); i != m_clipSets.end(); ++i)
	{
		delete (*i);
	}

	m_clipSets.clear();

	for (MAP_CLIPSETS::iterator j = m_schemeClipSets.begin(); j != m_schemeClipSets.end(); ++j)
	{
		delete (*j).second;
	}

	m_schemeClipSets.clear();
}

void TextClipsManager::copy(const TextClipsManager& copy)
{
	for (LIST_CLIPSETS::const_iterator i = copy.m_clipSets.begin();
		i != copy.m_clipSets.end();
		++i)
	{
		m_clipSets.push_back( new TextClipSet(**i) );
	}

	for (MAP_CLIPSETS::const_iterator j = copy.m_schemeClipSets.begin();
		j != copy.m_schemeClipSets.end();
		++j)
	{
		m_schemeClipSets.insert(MAP_CLIPSETS::value_type( (*j).first, new TextClipSet(*(*j).second)));
	}
}

/**
 * Uses FileFinder to find the clips files.
 */
void TextClipsManager::findClips()
{
	FileFinder<TextClipsManager> finder(this, &TextClipsManager::OnFound);

	tstring path;
	tstring usPath;
	
	// Search distribution clips directory.
	OPTIONS->GetPNPath(path, PNPATH_CLIPS);
	OPTIONS->GetPNPath(usPath, PNPATH_USERCLIPS);

	tstring clipcache = usPath + _T("installClipCache.xml");

	if (!FileExists(clipcache.c_str()))
	{
		if (DirExists(path.c_str()))
			finder.Find(path.c_str(), _T("*.clips"));

		// Save the clips we found in the cache
		Save(true);
	}
	else
	{
		// Read the cache
		m_curFileName = _T("");
		parse(clipcache.c_str());
	}
	
	// Search user clips directory and load those:
	if( DirExists(usPath.c_str()) )
		finder.Find(usPath.c_str(), _T("*.clips"));
}

void TextClipsManager::parse(LPCTSTR filename)
{
	XMLParser parser;
	parser.SetParseState(this);
	
	m_parseState = 0;
	
	try
	{
		parser.LoadFile(filename);
	}
	catch( XMLParserException& ex )
	{
		LOG(ex.GetMessage());
	}
}

#define TCPS_START	0
#define TCPS_CLIPS	1
#define TCPS_CLIP	2
#define TCPS_CHAR	3

#define MATCH_ELEMENT(ename) \
	if(_tcscmp(name, ename) == 0)

#define IN_STATE(state) \
	(m_parseState == state)

#define SET_STATE(state) \
	m_parseState = state

void TextClipsManager::startElement(LPCTSTR name, XMLAttributes& atts)
{
	if ( IN_STATE(TCPS_START) )
	{
		MATCH_ELEMENT(_T("clips"))
		{
			decodeNames = false;
			m_curShortcut = "";
			m_cData = "";
			m_curEncoding = eNone;

			// get name attribute...
			SET_STATE(TCPS_CLIPS);

			LPCTSTR szName = atts.getValue(_T("name"));
			if(szName == NULL)
				szName = _T("(unknown)");

			LPCTSTR szScheme = atts.getValue(_T("scheme"));
			
			LPCTSTR szEncoding = atts.getValue(_T("encoding"));
			if(szEncoding != NULL)
			{
				if(_tcscmp(szEncoding, _T("windows-1252")) == 0)
					m_curEncoding = eWindows1252;
				else if(_tcscmp(szEncoding, _T("ansi")) == 0)
					m_curEncoding = eANSI;
			}

			szEncoding = atts.getValue(_T("decodeNames"));
			if(szEncoding != NULL && (szEncoding[0] == _T('t') || szEncoding[0] == _T('T')))
					decodeNames = true;

			CT2CA schemeconv(szScheme);
			
			m_pCurSet = new TextClipSet(m_curFileName.c_str(), szName, schemeconv, decodeNames);
		}
	}
	else if (IN_STATE(TCPS_CLIPS))
	{
		MATCH_ELEMENT(_T("clip"))
		{
			LPCTSTR pName = atts.getValue(_T("name"));
			m_curName = (pName != NULL ? pName : _T("error"));
			pName = atts.getValue(_T("shortcut"));
			CT2CA shortcut(pName);
			m_curShortcut = (shortcut != NULL ? shortcut : "");
			SET_STATE(TCPS_CLIP);
		}
	}
	else if (IN_STATE(TCPS_CLIP))
	{
		MATCH_ELEMENT(_T("char"))
		{
			LPCTSTR charValue = atts.getValue(_T("value"));
			if (charValue != NULL && charValue[0] != NULL)
			{
				char val = static_cast<char>(_ttoi(charValue));
				m_cData += val;
			}

			SET_STATE(TCPS_CHAR);
		}
	}
}

void TextClipsManager::endElement(LPCTSTR name)
{
	if (IN_STATE(TCPS_CLIP))
	{
		// Create new clip - name = m_curName, content = m_cData;
		if (m_curEncoding != eNone)
		{
			decodeData();
		}

#ifndef _UNICODE
		if(decodeNames)
		{
			m_curName = Utf8_Windows1252(m_curName.c_str());
		}
#endif

		Clip* clip = new Clip(m_curName, m_curShortcut, m_cData);

		m_pCurSet->Add( clip );

		m_cData = "";

		SET_STATE(TCPS_CLIPS);
	}
	else if (IN_STATE(TCPS_CLIPS))
	{
		// General text clips or schemes based?
		if (m_pCurSet->GetScheme() != NULL)
		{
			m_schemeClipSets.insert(MAP_CLIPSETS::value_type(std::string(m_pCurSet->GetScheme()), m_pCurSet));
		}
		else
		{
			m_clipSets.push_back(m_pCurSet);
			m_pCurSet = NULL;
		}

		SET_STATE(TCPS_START);
	}
	else if (IN_STATE(TCPS_CHAR))
	{
		SET_STATE(TCPS_CLIP);
	}
}

void TextClipsManager::characterData(LPCTSTR data, int len)
{
	if ( IN_STATE(TCPS_CLIP) )
	{
		tstring buf(data, len);
		CT2CA conv(buf.c_str());
		m_cData += conv;
	}
}

void TextClipsManager::decodeData()
{
	// Until we have UTF-16 to 'x' conversion routines these won't work.
	#ifndef _UNICODE
	switch (m_curEncoding)
	{
		case eWindows1252:
		{
			Utf8_Windows1252 conv( m_cData.c_str() );
			if(conv.IsValid())
				m_cData = (const char*)conv;
		}
		break;

		case eANSI:
		{
			Utf8_ANSI conv( m_cData.c_str() );
			if(conv.IsValid())
				m_cData = (const char*)conv;
		}
		break;
	}
	#endif
}

} // namespace TextClips