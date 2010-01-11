/**
 * @file chunk.cpp
 * @brief Text Clips Chunk.
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "..\textclips.h"

using namespace TextClips;

/**
 * Is this chunk a field?
 */
bool Chunk::IsField() const
{
	return m_field;
}

/**
 * Get the text for this chunk.
 */
std::string Chunk::GetText() const
{
	// TODO: Apply any effect desired.
	if (m_text.size() == 0 && Id > 0)
	{
		return std::string("Field");
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