/**
 * @file cliphandler.cpp
 * @brief Handle textclip insertion in the text view Scintilla control.
 * @author Simon Steele
 * @note Copyright (c) 2010+ Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "../textview.h"

/**
 * Iterator for moving through chunk vector.
 */
typedef std::vector<TextClips::Chunk>::iterator ChunkIt_t;

/**
 * Declared inside textview.h as anonymous class.
 */
class ClipInsertionState
{
public:
	ClipInsertionState() : 
	    CurrentFieldStart(0),
		CurrentFieldEnd(0),
		LastModifyValid(false),
		Working(false),
		Valid(true)
	{
	}

	void HandleModification(CTextView* tv, int pos, int len, bool insert);
	void OffsetOtherChunks(int offset);
	ChunkIt_t FindChunk(int pos);

	std::vector<TextClips::Chunk> Chunks;
	int CurrentFieldStart;
	int CurrentFieldEnd;

	int LastModifyPos;
	int LastModifyLen;
	bool LastModifyWasInsert;
	bool LastModifyValid;
	bool Valid;

	bool Working;

	ChunkIt_t CurrentChunk;
};

class WorkingLock
{
public:
	WorkingLock(boost::shared_ptr<ClipInsertionState>& state) : m_state(state)
	{
		m_state->Working = true;
	}

	~WorkingLock()
	{
		m_state->Working = false;
	}

private:
	boost::shared_ptr<ClipInsertionState> m_state;
};



void ClipInsertionState::HandleModification(CTextView* tv, int pos, int len, bool insert)
{
	// We don't want to make any changes while handling the user's ones!
	if (Working)
	{
		return;
	}

	if (pos >= CurrentFieldStart &&
		pos <= CurrentFieldEnd)
	{
		int origEndPos = CurrentFieldEnd;

		// Modified this text:
		if (insert)
		{
			CurrentFieldEnd += len;
		}
		else
		{
			CurrentFieldEnd -= len;
			if (CurrentFieldEnd < CurrentFieldStart)
			{
				// Clip insertion is no longer valid:
				Valid = false;
				return;
			}
		}

		// Update the chunk:
		std::string current(tv->GetTextRange(CurrentFieldStart, CurrentFieldEnd));
		(*CurrentChunk).SetText(current.c_str());
		
		// Update the chunk positions:
		(*CurrentChunk).SetPos(CurrentFieldStart, CurrentFieldEnd);
		OffsetOtherChunks(CurrentFieldEnd - origEndPos);
	}
}

void ClipInsertionState::OffsetOtherChunks(int offset)
{
	// Offset everything after the current chunk:
	ChunkIt_t i = CurrentChunk;
	++i;
	for (; i != Chunks.end(); ++i)
	{
		if (i != CurrentChunk && (*i).IsField())
		{
			(*i).OffsetPos(offset);
		}
	}
}

ChunkIt_t ClipInsertionState::FindChunk(int pos)
{
	for (ChunkIt_t i = Chunks.begin(); i != Chunks.end(); ++i)
	{
		int start, end;
		(*i).GetPos(start, end);

		if (pos >= start && pos <= end)
		{
			return i;
		}
	}

	return Chunks.end();
}

void CTextView::beginInsertClip(std::vector<TextClips::Chunk>& chunks)
{
	if (m_bInsertClip)
	{
		// We're already inserting a clip, abandon that one and switch to this one:
		endInsertClip();
	}

	// We open an Undo action as soon as we start clip insertion:
	BeginUndoAction();

	m_insertClipState.reset(new ClipInsertionState());

	WorkingLock preventReentrance(m_insertClipState);

	m_insertClipState->Chunks.swap(chunks);

	if (m_insertClipState->Chunks.size() == 0)
	{
		// No text!
		m_insertClipState.reset();
		return;
	}

	int pos = GetCurrentPos();
	int firstFieldPos = 0;

	SPerform(SCI_SETINDICATORCURRENT, INDIC_TEXTCLIPFIELD, 0);
	SetIndicatorValue(INDIC_BOX);
	IndicSetStyle(INDIC_TEXTCLIPFIELD, INDIC_ROUNDBOX);
	IndicSetFore(INDIC_TEXTCLIPFIELD, RGB(0xff, 0xff, 0xff));
	SPerform(SCI_INDICSETALPHA, INDIC_TEXTCLIPFIELD, 80);

	for (ChunkIt_t i = m_insertClipState->Chunks.begin(); i != m_insertClipState->Chunks.end(); ++i)
	{
		std::string chunkText = (*i).GetText();
		InsertText(pos, chunkText.c_str());

		if ((*i).IsMasterField())
		{
			(*i).SetPos(pos, pos + chunkText.size());

			IndicatorFillRange(pos, chunkText.size());

			if (firstFieldPos == 0)
			{
				firstFieldPos = pos;
				m_insertClipState->CurrentChunk = i;
			}
		}
		else if ((*i).IsField())
		{
			(*i).SetPos(pos, pos + chunkText.size());
		}

		pos += chunkText.size();
	}

	if (firstFieldPos == 0)
	{
		firstFieldPos = pos;
	}

	// Set the selection to the insertion point or first field point.
	if (m_insertClipState->CurrentChunk != m_insertClipState->Chunks.end())
	{
		(*m_insertClipState->CurrentChunk).GetPos(m_insertClipState->CurrentFieldStart, m_insertClipState->CurrentFieldEnd);
		SetSel(m_insertClipState->CurrentFieldStart, m_insertClipState->CurrentFieldEnd);
	}

	m_bInsertClip = true;
}

void CTextView::updateInsertClip()
{
	if (!m_bInsertClip || m_insertClipState->Working)
	{
		return;
	}

	WorkingLock preventReentrance(m_insertClipState);

	if (!m_insertClipState->Valid)
	{
		// Some modification outside the template, bail!
		endInsertClip();
		return;
	}

	SPerform(SCI_SETINDICATORCURRENT, INDIC_TEXTCLIPFIELD, 0);
	
	// Process modification:
	if (m_insertClipState->LastModifyValid)
	{
		m_insertClipState->LastModifyValid = false;

		IndicatorFillRange(m_insertClipState->CurrentFieldStart, m_insertClipState->CurrentFieldEnd - m_insertClipState->CurrentFieldStart);

		// Update chunk text and positions:
		int offset(0);
		std::string chunkText((*m_insertClipState->CurrentChunk).GetText());

		ChunkIt_t i = m_insertClipState->CurrentChunk;
		++i;
		for (; i != m_insertClipState->Chunks.end(); ++i)
		{
			if ((*i).IsField())
			{
				// Offset by our running offset:
				(*i).OffsetPos(offset);

				if ((*i).Id == (*m_insertClipState->CurrentChunk).Id)
				{
					// See if the text has changed:
					std::string oldText((*i).GetText());
					if (oldText != chunkText)
					{
						int start, end;
						(*i).GetPos(start, end);

						SetTarget(start, end);
						ReplaceTarget(chunkText.size(), chunkText.c_str());

						end += chunkText.size() - oldText.size();
						offset += chunkText.size() - oldText.size();

						(*i).SetText(chunkText.c_str());
						(*i).SetPos(start, end);

						// IndicatorFillRange(start, end - start);
					}
				}
			}
		}
	}

	int pos = GetCurrentPos();
	m_insertClipState->CurrentChunk = m_insertClipState->FindChunk(pos);
	if (m_insertClipState->CurrentChunk == m_insertClipState->Chunks.end())
	{
		endInsertClip();
	}
	else
	{
		(*m_insertClipState->CurrentChunk).GetPos(m_insertClipState->CurrentFieldStart, m_insertClipState->CurrentFieldEnd);
	}
}

void CTextView::endInsertClip()
{
	m_bInsertClip = false;
	SPerform(SCI_INDICATORCLEARRANGE, 0, GetLength());
	m_insertClipState.reset();
	EndUndoAction();
}

void CTextView::prevClipField()
{
	if (m_insertClipState->CurrentChunk != m_insertClipState->Chunks.end())
	{
		if (m_insertClipState->CurrentChunk != m_insertClipState->Chunks.begin())
		{
			--m_insertClipState->CurrentChunk;
			while (!(*m_insertClipState->CurrentChunk).IsMasterField())
			{
				if (m_insertClipState->CurrentChunk == m_insertClipState->Chunks.begin())
				{
					// Can't go any further back:
					endInsertClip();
					return;
				}

				--m_insertClipState->CurrentChunk;
			}

			int start, end;
			(*m_insertClipState->CurrentChunk).GetPos(start, end);
			SetSel(start, end);
		}
		else
		{
			endInsertClip();
		}
	}
}

void CTextView::nextClipField()
{
	if (m_insertClipState->CurrentChunk != m_insertClipState->Chunks.end())
	{
		++m_insertClipState->CurrentChunk;
		while (m_insertClipState->CurrentChunk != m_insertClipState->Chunks.end() && !(*m_insertClipState->CurrentChunk).IsMasterField())
		{
			++m_insertClipState->CurrentChunk;
		}

		if (m_insertClipState->CurrentChunk != m_insertClipState->Chunks.end())
		{
			int start, end;
			(*m_insertClipState->CurrentChunk).GetPos(start, end);
			SetSel(start, end);
		}
		else
		{
			endInsertClip();
		}
	}
	else
	{
		endInsertClip();
	}
}

void CTextView::handleInsertClipNotify(Scintilla::SCNotification* scn)
{
	int msg = scn->nmhdr.code;

	if(msg == SCN_UPDATEUI)
	{
		updateInsertClip();
	}
	else if(msg == SCN_MODIFIED)
	{
		if ((scn->modificationType & (SC_MOD_INSERTTEXT | SC_MOD_DELETETEXT)) != 0)
		{
			m_insertClipState->HandleModification(this, scn->position, scn->length, (scn->modificationType & SC_MOD_INSERTTEXT) != 0);
			m_insertClipState->LastModifyValid = true;
		}
	}
}