#include "stdafx.h"
#include "../textclips.h"
#include "../include/encoding.h"

using namespace TextClips;

//////////////////////////////////////////////////////////////////////////////
// Clip
//////////////////////////////////////////////////////////////////////////////

void Clip::Insert(CScintilla *scintilla) const
{
	std::string clipstr;
	std::string theText(Text);
	std::string theIndent;

	// Indentation:
	int indentation = scintilla->GetLineIndentation(scintilla->LineFromPosition(scintilla->GetCurrentPos()));
	
	// Work out tabs and spaces combination to get the indentation right
	// according to user's settings.
	theIndent = MakeIndentText( indentation, scintilla->GetUseTabs(), scintilla->GetTabWidth() );

	for (size_t i = 0; i < theText.size(); i++)
	{
		if (theText[i] == '\n')
		{
			theText.insert(i+1, theIndent);
			i += theIndent.size();
		}
	}
	
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
				m_chunks.push_back(Chunk(true, (thechar - '0') + 1));
			}

			// TODO: Other format chars... 
		}

		void OnFormatKey(LPCTSTR key) {}
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
				m_chunks.push_back(Chunk(false, std::string((const char*)(const unsigned char*)conv)));
				m_string = _T("");
			}
		}

		std::vector<Chunk> m_chunks;
};

void Clip::GetChunks(std::vector<Chunk>& chunks) const
{
	TextClipParser parser;
	Utf8_Tcs conv(Text.c_str());
	parser.Build(conv);
	
	std::vector<Chunk> results;
	parser.SwapChunks(chunks);
}