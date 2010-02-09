/**
 * @file clip.cpp
 * @brief Single Text Clip functionality.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "../textclips.h"
#include "../include/encoding.h"

#include <set>

using namespace TextClips;

//////////////////////////////////////////////////////////////////////////////
// Clip
//////////////////////////////////////////////////////////////////////////////

void Clip::Insert(CScintilla *scintilla) const
{
	std::string clipstr = FixText(scintilla);
	
	size_t offset = clipstr.find(_T('|'));
	if (offset != clipstr.npos)
		clipstr.erase(offset, 1);
	else
		offset = 0;
	
	int curPos = scintilla->GetCurrentPos();

	std::string sel(scintilla->GetSelText());
	if (sel.length() != 0)
	{
		// Insert the text into the buffer.
		clipstr.insert(offset, sel);
		
		// Adjust the offset to place the cursor after the selected text.
		offset += sel.length();
	}

	// Wrap everything in an undo block.
	scintilla->BeginUndoAction();
	if (sel.length())
	{
		scintilla->DeleteBack(); // kill the selection text, we're inserting it again.
		curPos = scintilla->GetCurrentPos();
	}

	scintilla->InsertText(curPos, clipstr.c_str());
	scintilla->SetCurrentPos(curPos + offset);
	scintilla->SetSel(curPos + offset, curPos + offset);
	scintilla->EndUndoAction();
}

class TextClipParser : public CustomFormatStringBuilder<TextClipParser>
{
	public:
		void OnFormatChar(TCHAR thechar) 
		{
			pushChunk();

			if (thechar >= '0' && thechar <= '9')
			{
				int stop = (thechar - '0');
				EChunkType chunkType = m_seenStops.insert(stop).second ? ctMasterField : ctField;
				m_chunks.push_back(Chunk(chunkType, stop));
			}

			// TODO: Other format chars... 
		}

		void OnFormatKey(LPCTSTR key)
		{
			pushChunk();

			boost::xpressive::tsregex tabstopMatch = boost::xpressive::tsregex::compile(_T("([0-9]+):(.+)"));
			boost::xpressive::tsregex tabstopMatchSimple = boost::xpressive::tsregex::compile(_T("([0-9]+)"));

			boost::xpressive::tsmatch match;
			tstring prop(key);

			if (boost::xpressive::regex_match(prop, match, tabstopMatch))
			{
				tstring stopid(match[1]);
				tstring text(match[2]);

				int stop = _ttoi(stopid.c_str());

				// If this is the first insertion this is a master chunk:
				int chunkType = m_seenStops.insert(stop).second ? ctMasterField : ctField;
				if (stop == 0)
				{
					chunkType = chunkType | ctFinalCaretPos;
				}
				
				Tcs_Utf8 texta(text.c_str());

				m_chunks.push_back(Chunk(chunkType, stop, std::string((const char*)(const unsigned char*)texta)));
			}
			else if (boost::xpressive::regex_match(prop, match, tabstopMatchSimple))
			{
				tstring stopid(match[1]);

				int stop = _ttoi(stopid.c_str());

				// If this is the first insertion this is a master chunk:
				int chunkType = m_seenStops.insert(stop).second ? ctMasterField : ctField;
				if (stop == 0)
				{
					chunkType = chunkType | ctFinalCaretPos;
				}

				m_chunks.push_back(Chunk(chunkType, stop));
			}
		}
		
		void OnFormatPercentKey(LPCTSTR key) {}
		void OnFormatScriptRef(LPCTSTR key) {}

		void SwapChunks(std::vector<Chunk>& chunks)
		{
			pushChunk();
			m_chunks.swap(chunks);
		}

	private:
		void pushChunk()
		{
			if (m_string.size())
			{
				Utf16_Utf8 conv(m_string.c_str());
				m_chunks.push_back(Chunk(ctNone, std::string((const char*)(const unsigned char*)conv)));
				m_string = _T("");
			}
		}

		std::vector<Chunk> m_chunks;
		std::set<int> m_seenStops;
};

void Clip::GetChunks(std::vector<Chunk>& chunks) const
{
	return GetChunks(chunks, NULL);
}

void Clip::GetChunks(std::vector<Chunk>& chunks, CScintilla* scintilla) const
{
	TextClipParser parser;
	
	std::string insertText = FixText(scintilla);

	Utf8_Tcs conv(insertText.c_str());
	parser.Build(conv);
	
	std::vector<Chunk> results;
	parser.SwapChunks(chunks);
}

std::string Clip::FixText(CScintilla* scintilla) const
{
	if (scintilla == NULL)
	{
		return Text;
	}

	// Indentation:
	int indentation = scintilla->GetLineIndentation(scintilla->LineFromPosition(scintilla->GetCurrentPos()));
	
	// Work out tabs and spaces combination to get the indentation right
	// according to user's settings.
	std::string theIndent = MakeIndentText( indentation, scintilla->GetUseTabs(), scintilla->GetTabWidth() );

	std::string theText(Text);
	for (size_t i = 0; i < theText.size(); i++)
	{
		if (theText[i] == '\n')
		{
			theText.insert(i+1, theIndent);
			i += theIndent.size();
		}
	}
	
	std::string clipstr;

	// Text is in Unix EOL mode (LF only), convert it to target EOL mode
	switch (scintilla->GetEOLMode())
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
			std::string::const_iterator it = theText.begin();
			std::string::const_iterator end = theText.end();
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

	return clipstr;
}