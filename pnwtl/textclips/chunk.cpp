/**
 * @file chunk.cpp
 * @brief Text Clips Chunk.
 * @author Simon Steele
 * @note Copyright (c) 2009+ Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "..\textclips.h"

using namespace TextClips;

Chunk::Chunk() : 
	m_field(false), 
	m_masterField(false), 
	m_finalCaretPos(false), 
	m_start(0), 
	m_end(0), 
	Id(0) 
{}

Chunk::Chunk(/*EChunkType*/int field, const std::string& text) : 
	m_field((field & ctField) != 0),
	m_masterField((field & ctMasterField) == ctMasterField),
	m_finalCaretPos((field & ctFinalCaretPos) != 0),
	m_text(text),
	m_start(0),
	m_end(0),
	Id(0) {}

Chunk::Chunk(/*EChunkType*/int field, int id) : 
	m_field((field & ctField) != 0),
	m_masterField((field & ctMasterField) == ctMasterField),
	m_finalCaretPos((field & ctFinalCaretPos) != 0),
	Id(id),
	m_start(0),
	m_end(0) {}

Chunk::Chunk(/*EChunkType*/int field, int id, const std::string& text) : 
	m_field((field & ctField) != 0),
	m_masterField((field & ctMasterField) == ctMasterField),
	m_finalCaretPos((field & ctFinalCaretPos) != 0),
	Id(id),
	m_text(text),
	m_start(0),
	m_end(0) 
{}

/**
 * Is this chunk a field?
 */
bool Chunk::IsField() const
{
	return m_field;
}

/**
 * Is this chunk a field?
 */
bool Chunk::IsMasterField() const
{
	return m_masterField;
}

/**
 * Is this chunk a field?
 */
bool Chunk::IsFinalCaretPos() const
{
	return m_finalCaretPos;
}

/**
 * Get the text for this chunk.
 */
std::string Chunk::GetText() const
{
	// TODO: Apply any effect desired.
	if (m_text.size() == 0 && Id > 0)
	{
		return std::string("");
	}

	return m_text;
}

/**
 * Replace the text for this chunk.
 */
void Chunk::SetText(const char* text)
{
	m_text = text;

	// TODO: Disable any transforms and/or links etc.
}

void Chunk::SetPos(int start, int end)
{
	m_start = start;
	m_end = end;
}

void Chunk::GetPos(int& start, int& end) const
{
	start = m_start;
	end = m_end;
}

void Chunk::OffsetPos(int offset)
{
	m_start += offset;
	m_end += offset;
}