#include "stdafx.h"
#include "scaccessor.h"

ScintillaAccessor::~ScintillaAccessor()
{
}

bool ScintillaAccessor::InternalIsLeadByte(char ch)
{
	if (SC_CP_UTF8 == codePage)
		// For lexing, all characters >= 0x80 are treated the
		// same so none is considered a lead byte.
		return false;	
	else
		//return Platform::IsDBCSLeadByte(codePage, ch);
		return ::IsDBCSLeadByteEx(codePage, ch) != 0;
}

void ScintillaAccessor::Fill(int position)
{
	if (lenDoc == -1)
		lenDoc = m_pS->SPerform(SCI_GETTEXTLENGTH, 0, 0);
	startPos = position - slopSize;
	if (startPos + bufferSize > lenDoc)
		startPos = lenDoc - bufferSize;
	if (startPos < 0)
		startPos = 0;
	endPos = startPos + bufferSize;
	if (endPos > lenDoc)
		endPos = lenDoc;

	Scintilla::TextRange tr = {{startPos, endPos}, buf};
	m_pS->SPerform(SCI_GETTEXTRANGE, 0, (LPARAM)(void*)&tr);
}

bool ScintillaAccessor::Match(int pos, const char *s)
{
	for (int i=0; *s; i++)
	{
		if (*s != SafeGetCharAt(pos+i))
			return false;
		s++;
	}
	return true;
}

char ScintillaAccessor::StyleAt(int position)
{
	return static_cast<char>(m_pS->SPerform(SCI_GETSTYLEAT, position, 0));
}

int ScintillaAccessor::GetLine(int position)
{
	return m_pS->SPerform(SCI_LINEFROMPOSITION, position, 0);
}

int ScintillaAccessor::LineStart(int line)
{
	return m_pS->SPerform(SCI_POSITIONFROMLINE, line, 0);
}

int ScintillaAccessor::LevelAt(int line)
{
	return m_pS->SPerform(SCI_GETFOLDLEVEL, line, 0);
}

int ScintillaAccessor::Length()
{
	if (lenDoc == -1) 
		lenDoc = m_pS->SPerform(SCI_GETTEXTLENGTH, 0, 0);
	
	return lenDoc;
}

int ScintillaAccessor::GetLineState(int line)
{
	return m_pS->SPerform(SCI_GETLINESTATE, line);
}

int ScintillaAccessor::SetLineState(int line, int state)
{
	return m_pS->SPerform(SCI_SETLINESTATE, line, state);
}

void ScintillaAccessor::StartAt(unsigned int start, char chMask)
{
	m_pS->SPerform(SCI_STARTSTYLING, start, chMask);
}

void ScintillaAccessor::StartSegment(unsigned int pos)
{
	startSeg = pos;
}

void ScintillaAccessor::ColourTo(unsigned int pos, int chAttr)
{
	// Only perform styling if non empty range
	if (pos != startSeg - 1)
	{
		if (pos < startSeg)
		{
			//Platform::DebugPrintf("Bad colour positions %d - %d\n", startSeg, pos);
		}

		if (validLen + (pos - startSeg + 1) >= bufferSize)
			Flush();

		if (validLen + (pos - startSeg + 1) >= bufferSize)
		{
			// Too big for buffer so send directly
			m_pS->SPerform(SCI_SETSTYLING, pos - startSeg + 1, chAttr);
		} 
		else 
		{
			if (chAttr != chWhile)
				chFlags = 0;
		
			chAttr |= chFlags;
			
			for (unsigned int i = startSeg; i <= pos; i++)
			{
				styleBuf[validLen++] = static_cast<char>(chAttr);
			}
		}
	}
	startSeg = pos+1;
}

void ScintillaAccessor::SetLevel(int line, int level)
{
	m_pS->SPerform(SCI_SETFOLDLEVEL, line, level);
}

void ScintillaAccessor::Flush()
{
	startPos = extremePosition;
	lenDoc = -1;
	if (validLen > 0)
	{
		m_pS->SPerform(SCI_SETSTYLINGEX, validLen, (LPARAM)(void*)styleBuf);
		validLen = 0;
	}
}

/*int ScintillaAccessor::IndentAmount(int line, int *flags, PFNIsCommentLeader pfnIsCommentLeader)
{
	int end = Length();
	int spaceFlags = 0;

	// Determines the indentation level of the current line and also checks for consistent 
	// indentation compared to the previous line.
	// Indentation is judged consistent when the indentation whitespace of each line lines 
	// the same or the indentation of one line is a prefix of the other.

	int pos = LineStart(line);
	char ch = (*this)[pos];
	int indent = 0;
	bool inPrevPrefix = line > 0;
	int posPrev = inPrevPrefix ? LineStart(line-1) : 0;
	while ((ch == ' ' || ch == '\t') && (pos < end)) {
		if (inPrevPrefix) {
			char chPrev = (*this)[posPrev++];
			if (chPrev == ' ' || chPrev == '\t') {
				if (chPrev != ch)
					spaceFlags |= wsInconsistent;
			} else {
				inPrevPrefix = false;
			}
		}
		if (ch == ' ') {
			spaceFlags |= wsSpace;
			indent++;
		} else {	// Tab
			spaceFlags |= wsTab;
			if (spaceFlags & wsSpace)
				spaceFlags |= wsSpaceTab;
			indent = (indent / 8 + 1) * 8;
		}
		ch = (*this)[++pos];
	}

	*flags = spaceFlags;
	indent += SC_FOLDLEVELBASE;
	// if completely empty line or the start of a comment...
	if (isspace(ch) || (pfnIsCommentLeader && (*pfnIsCommentLeader)(*this, pos, end-pos)) )
		return indent | SC_FOLDLEVELWHITEFLAG;
	else
		return indent;
}*/