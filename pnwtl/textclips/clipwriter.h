/**
 * @file clipwriter.h
 * @brief Text Clips Writer.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef CLIPWRITER_H_INCLUDED
#define CLIPWRITER_H_INCLUDED

#pragma once

#include "../third_party/genx/genx.h"
#include "../include/pngenx.h"

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

} // namespace TextClips

#endif //#ifndef CLIPWRITER_H_INCLUDED