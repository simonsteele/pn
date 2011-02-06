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
	explicit ClipInsertionState(std::vector<TextClips::Chunk>& chunkDonor) : 
	    CurrentFieldStart(0),
		CurrentFieldEnd(0),
		LastModifyValid(false),
		Working(false),
		Valid(true)
	{
		Chunks.swap(chunkDonor);
		CurrentChunk = Chunks.end();
	}

	void HandleModification(CTextView* tv, int pos, int len, bool insert);
	void OffsetOtherChunks(int offset);
	ChunkIt_t FindChunk(int pos);
	ChunkIt_t FindMasterChunk(int id);
	bool MoveToNextField();
	bool MoveToFirstField();
	bool MoveToPrevField();

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

ChunkIt_t ClipInsertionState::FindMasterChunk(int id)
{
	for (ChunkIt_t i = Chunks.begin(); i != Chunks.end(); ++i)
	{
		if ((*i).Id == id && (*i).IsMasterField())
		{
			return i;
		}
	}

	return Chunks.end();
}

bool ClipInsertionState::MoveToFirstField()
{
	CurrentChunk = Chunks.end();

	// Find first field:
	for (ChunkIt_t i = Chunks.begin(); i != Chunks.end(); ++i)
	{
		if ((*i).IsMasterField() && !(*i).IsFinalCaretPos())
		{
			if (CurrentChunk == Chunks.end())
			{
				CurrentChunk = i;
			}
			else if ((*CurrentChunk).Id > (*i).Id)
			{
				CurrentChunk = i;
			}
		}
	}

	if (CurrentChunk == Chunks.end())
	{
		// No normal master fields found, see if there's a final caret pos:
		CurrentChunk = FindMasterChunk(0);
	}

	return CurrentChunk != Chunks.end();
}

bool ClipInsertionState::MoveToNextField()
{
	// We normally want to move to the subsequent chunk:
	int currentChunkKey = (*CurrentChunk).Id;
	if (currentChunkKey == 0)
	{
		// We're on the last chunk.
		return false;
	}

	int desiredChunkKey = currentChunkKey + 1;
	ChunkIt_t desired = FindMasterChunk(desiredChunkKey);
	if (desired != Chunks.end())
	{
		CurrentChunk = desired;
		return true;
	}
	
	// We couldn't find the next chunk, see if there's an end placeholder:
	desired = FindMasterChunk(0);
	if (desired != Chunks.end())
	{
		CurrentChunk = desired;
		return true;
	}

	return false;
}

bool ClipInsertionState::MoveToPrevField()
{
	if ((*CurrentChunk).IsFinalCaretPos())
	{
		// We're at the final stop, we need to find the highest chunk and jump to that.
		ChunkIt_t final = CurrentChunk;
		for (ChunkIt_t i = Chunks.begin(); i != Chunks.end(); ++i)
		{
			if ((*i).IsMasterField() && (*i).Id > (*final).Id)
			{
				final = i;
			}
		}

		if (final == CurrentChunk)
		{
			return false;
		}
		else
		{
			CurrentChunk = final;
			return true;
		}
	}

	int desiredChunkKey = (*CurrentChunk).Id;
	desiredChunkKey--;

	if (desiredChunkKey == 0)
	{
		return false;
	}

	ChunkIt_t desired = FindMasterChunk(desiredChunkKey);
	if (desired != Chunks.end())
	{
		CurrentChunk = desired;
		return true;
	}

	return false;
}

//////////////////////////////////////////////////////////////////////////////////////////////
// CTextView

void CTextView::beginInsertClip(std::vector<TextClips::Chunk>& chunks)
{
	if (m_bInsertClip)
	{
		// We're already inserting a clip, abandon that one and switch to this one:
		endInsertClip();
	}

	// And another for this initial insertion:

	m_insertClipState.reset(new ClipInsertionState(chunks));

	WorkingLock preventReentrance(m_insertClipState);

	if (m_insertClipState->Chunks.size() == 0)
	{
		// No text!
		m_insertClipState.reset();
		return;
	}

	// We open an Undo action as soon as we start clip insertion:
	BeginUndoAction();

	int pos = GetCurrentPos();
	
	SPerform(SCI_SETINDICATORCURRENT, INDIC_TEXTCLIPFIELD, 0);
	SetIndicatorValue(INDIC_BOX);

	for (ChunkIt_t i = m_insertClipState->Chunks.begin(); i != m_insertClipState->Chunks.end(); ++i)
	{
		std::string chunkText = (*i).GetText();

		if ((*i).IsMasterField())
		{
			InsertText(pos, chunkText.c_str());

			(*i).SetPos(pos, pos + chunkText.size());

			IndicatorFillRange(pos, chunkText.size());
		}
		else if ((*i).IsField())
		{
			// Need to get the master field for the text:
			ChunkIt_t master = m_insertClipState->FindMasterChunk((*i).Id);
			if (master != m_insertClipState->Chunks.end())
			{
				chunkText = (*master).GetText();
				(*i).SetText(chunkText.c_str());
			}

			InsertText(pos, chunkText.c_str());

			(*i).SetPos(pos, pos + chunkText.size());
		}
		else
		{
			InsertText(pos, chunkText.c_str());
		}

		pos += chunkText.size();
	}

	// Set the selection to the insertion point or first field point.
	if (m_insertClipState->MoveToFirstField())
	{
		(*m_insertClipState->CurrentChunk).GetPos(m_insertClipState->CurrentFieldStart, m_insertClipState->CurrentFieldEnd);
		SetSel(m_insertClipState->CurrentFieldStart, m_insertClipState->CurrentFieldEnd);

		// End the initial insertion, start an action to contain the rest of the clip insertion:
		EndUndoAction();
		BeginUndoAction();

		m_bInsertClip = true;
	}
	else
	{
		endInsertClip();
	}
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
		if (m_insertClipState->MoveToPrevField())
		{
			int start, end;
			(*m_insertClipState->CurrentChunk).GetPos(start, end);
			SetSel(start, end);
			return;
		}
	}

	endInsertClip();
}

void CTextView::nextClipField()
{
	if (m_insertClipState->CurrentChunk != m_insertClipState->Chunks.end())
	{
		ChunkIt_t oldChunk(m_insertClipState->CurrentChunk);

		if (m_insertClipState->MoveToNextField())
		{
			int start, end;
			(*m_insertClipState->CurrentChunk).GetPos(start, end);
			SetSel(start, end);

			if ((*m_insertClipState->CurrentChunk).IsFinalCaretPos() && (*m_insertClipState->CurrentChunk).GetText() == "")
			{
				// If we just reached the final position, and there's no text to edit,
				// end clip insertion straight away.
				endInsertClip();
			}
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
		else if (scn->modificationType & SC_PERFORMED_UNDO)
		{
			// For the moment no undo handling, we bail out of the clip handling:
			endInsertClip();
		}
	}
}