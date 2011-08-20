/**
 * @file clip.cpp
 * @brief Single Text Clip functionality.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "../textclips.h"
#include "chunkparser.h"
#include "../include/encoding.h"

#include <set>

using namespace TextClips;

//////////////////////////////////////////////////////////////////////////////
// Clip
//////////////////////////////////////////////////////////////////////////////

void Clip::Insert(CScintilla *scintilla) const
{
	std::vector<Chunk> chunks;
	chunks.push_back(Chunk(0, Text.c_str()));
	FixText(scintilla, chunks);
	std::string clipstr = chunks[0].GetText();
	
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

void Clip::GetChunks(std::vector<Chunk>& chunks) const
{
	return GetChunks(chunks, NULL, NULL, NULL);
}

void Clip::GetChunks(std::vector<Chunk>& chunks, CScintilla* scintilla, IVariableProvider* variables, extensions::IScriptRegistry* scriptRegistry) const
{
	ChunkParser parser;

	if (variables)
	{
		parser.SetVariableProvider(variables);
	}

	if (scriptRegistry)
	{
		extensions::IScriptRunner* runner = scriptRegistry->GetRunner("python");
		if (runner != NULL)
		{
			parser.SetScriptRunner(runner);
		}
	}

	parser.Parse(std::string(Text.c_str()), chunks);
	FixText(scintilla, chunks);
}

void Clip::FixText(CScintilla* scintilla, std::vector<Chunk>& chunks) const
{
	if (scintilla == NULL)
	{
		return;
	}

	// Indentation:
	int indentation = scintilla->GetLineIndentation(scintilla->LineFromPosition(scintilla->GetCurrentPos()));
	
	// Work out tabs and spaces combination to get the indentation right
	// according to user's settings.
	std::string theIndent = MakeIndentText(indentation, scintilla->GetUseTabs(), scintilla->GetTabWidth());

	for(auto cit = chunks.begin(); cit != chunks.end(); ++cit)
	{
		std::string result = cit->GetText();
		result = FixChunkText(result, theIndent, scintilla->GetEOLMode());
		cit->SetText(result.c_str());
	}
}

std::string Clip::FixChunkText(std::string& theText, const std::string& theIndent, int eolMode) const
{
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
	switch (eolMode)
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