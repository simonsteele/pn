/**
 * @file scaccessor.h
 * @brief Define a scintilla accessor for external use.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 * @note Most code copyright 1998-2001 by Neil Hodgson <neilh@scintilla.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * This class is basically an amalgamation of the code from Accessor and
 * WindowAccessor classes from Scintilla. I have done this to avoid having
 * all the platform types from scintilla along with everything else included.
 */
#ifndef accessor_h__included
#define accessor_h__included

class ScintillaAccessor
{
	// Private so WindowAccessor objects can not be copied
	ScintillaAccessor(const ScintillaAccessor& source) {}
	ScintillaAccessor& operator = (const ScintillaAccessor&) { return *this; }

public:
	ScintillaAccessor(CScintilla* sc) : 
		lenDoc(-1), validLen(0), chFlags(0), 
		chWhile(0), m_pS(sc), startPos(extremePosition), 
		endPos(0), codePage(0){}

	~ScintillaAccessor();

	bool Match(int pos, const char *s);
	char StyleAt(int position);
	int GetLine(int position);
	int LineStart(int line);
	int LevelAt(int line);
	int Length();
	void Flush();
	int GetLineState(int line);
	int SetLineState(int line, int state);

	void StartAt(unsigned int start, char chMask = 31);
	void StartSegment(unsigned int pos);
	void ColourTo(unsigned int pos, int chAttr);
	void SetLevel(int line, int level);
	
	unsigned int GetStartSegment()
	{ 
		return startSeg; 
	}

	void SetFlags(char chFlags_, char chWhile_)
	{
		chFlags = chFlags_; 
		chWhile = chWhile_; 
	};

// Accessor Methods:
public:
	char operator[](int position)
	{
		if (position < startPos || position >= endPos)
		{
			Fill(position);
		}
		return buf[position - startPos];
	}

	/** Safe version of operator[], returning a defined value for invalid position. */
	char SafeGetCharAt(int position, char chDefault=' ')
	{
		if (position < startPos || position >= endPos)
		{
			Fill(position);
			if (position < startPos || position >= endPos)
			{
				// Position is outside range of document 
				return chDefault;
			}
		}
		return buf[position - startPos];
	}

	bool IsLeadByte(char ch)
	{
		return codePage && InternalIsLeadByte(ch);
	}
	
	void SetCodePage(int codePage_)
	{
		codePage = codePage_;
	}

// Added methods:
public:
	bool AtEOL(unsigned int i)
	{
		return (SafeGetCharAt(i) == '\n') ||
		((SafeGetCharAt(i) == '\r') && (SafeGetCharAt(i + 1) != '\n'));
	}

// Accessor members:
protected:
	enum {extremePosition=0x7FFFFFFF};
	
	/** @a bufferSize is a trade off between time taken to copy the characters
	 * and retrieval overhead.
	 * @a slopSize positions the buffer before the desired position
	 * in case there is some backtracking. */
	enum {bufferSize=4000, slopSize=bufferSize/8};
	char buf[bufferSize+1];
	int startPos;
	int endPos;
	int codePage;

// WindowAccessor members:
protected:
	int lenDoc;

	char styleBuf[bufferSize];
	int validLen;
	char chFlags;
	char chWhile;
	unsigned int startSeg;

	CScintilla* m_pS;

	bool InternalIsLeadByte(char ch);
	void Fill(int position);
};

/* Non-Implemented WindowAccessor methods...
int GetPropertyInt(const char *key, int defaultValue=0) { 
	return props.GetInt(key, defaultValue); 
}
char *GetProperties() {
	return props.ToString();
}

int IndentAmount(int line, int *flags, PFNIsCommentLeader pfnIsCommentLeader = 0);
*/

#endif