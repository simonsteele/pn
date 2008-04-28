#include <stdlib.h>

#include "Platform.h"
#include "SplitVector.h"
#include "Partitioning.h"
#include "RunStyles.h"
#include "CellBuffer.h"
#include "CharClassify.h"
#include "Decoration.h"
#include <Document.h>

#include <boost/xpressive/xpressive.hpp>

#ifdef SCI_NAMESPACE
using namespace Scintilla;
#endif

/**
 * std::iterator compatible iterator for Scintilla contents
 */
class DocumentIterator : public std::iterator<std::bidirectional_iterator_tag, char>
{
public:
	DocumentIterator() : 
		m_doc(0), 
		m_pos(0),
		m_end(0)
	{
	}

	DocumentIterator(Document* doc, int pos, int end) : 
		m_doc(doc),
		m_pos(pos),
		m_end(end)
	{
	}

	DocumentIterator(const DocumentIterator& copy) :
		m_doc(copy.m_doc),
		m_pos(copy.m_pos),
		m_end(copy.m_end)
	{
	}

	bool operator == (const DocumentIterator& other) const
	{
		return (ended() == other.ended()) && (m_doc == other.m_doc) && (m_pos == other.m_pos);
	}

	bool operator != (const DocumentIterator& other) const
	{
		return !(*this == other);
	}

	char operator * () const
	{
		return charAt(m_pos);
	}

	DocumentIterator& operator ++ ()
	{
		m_pos++;
		return *this;
	}

	DocumentIterator& operator -- ()
	{
		m_pos--;
		return *this;
	}

	int pos() const
	{
		return m_pos;
	}

private:
	char charAt(int position) const
	{
		return m_doc->CharAt(position);
	}

	bool ended() const
	{
		return m_pos == m_end;
	}

	int m_pos;
	int m_end;
	Document* m_doc;
};

typedef boost::xpressive::basic_regex<DocumentIterator> docregex;
typedef boost::xpressive::match_results<DocumentIterator> docmatch;
typedef boost::xpressive::sub_match<DocumentIterator> docsub_match;

class Document::CachedRegex
{
public:
	docregex re;
	docmatch match;
};

void Document::CleanupRegex()
{
	delete pre;
}

/**
 * Find text in document, supporting both forward and backward
 * searches (just pass minPos > maxPos to do a backward search)
 * Has not been tested with backwards DBCS searches yet.
 */
long Document::RegexFindText(int minPos, int maxPos, const char *s,
                        bool caseSensitive, bool word, bool wordStart, bool posix, int *length) 
{
	
	//@todo The character class support should be initialised from this->charClass
	if (!pre)
	{
		pre = new CachedRegex;
	}

	int increment = (minPos <= maxPos) ? 1 : -1;

	int startPos = minPos;
	int endPos = maxPos;

	// Range endpoints should not be inside DBCS characters, but just in case, move them.
	startPos = MovePositionOutsideChar(startPos, 1, false);
	endPos = MovePositionOutsideChar(endPos, 1, false);

	std::string restring(s, *length);
	
	try
	{
		pre->re = docregex::compile(restring.c_str());
	}
	catch(boost::xpressive::regex_error& ex)
	{
		// -1 is normally used for not found, -2 is used here for invalid regex
		return -2;
	}

	int lineRangeStart = LineFromPosition(startPos);
	int lineRangeEnd = LineFromPosition(endPos);
	if ((increment == 1) &&
		(startPos >= LineEnd(lineRangeStart)) &&
		(lineRangeStart < lineRangeEnd)) 
	{
		// the start position is at end of line or between line end characters.
		lineRangeStart++;
		startPos = LineStart(lineRangeStart);
	}
	
	int pos = -1;
	int lenRet = 0;
	char searchEnd = s[*length - 1];
	int lineRangeBreak = lineRangeEnd + increment;
	for (int line = lineRangeStart; line != lineRangeBreak; line += increment) 
	{
		int startOfLine = LineStart(line);
		int endOfLine = LineEnd(line);
		if (increment == 1) 
		{
			if (line == lineRangeStart) 
			{
				if ((startPos != startOfLine) && (s[0] == '^'))
					continue;	// Can't match start of line if start position after start of line
				startOfLine = startPos;
			}
			if (line == lineRangeEnd) 
			{
				if ((endPos != endOfLine) && (searchEnd == '$'))
					continue;	// Can't match end of line if end position before end of line
				endOfLine = endPos;
			}
		} 
		else 
		{
			if (line == lineRangeEnd) 
			{
				if ((endPos != startOfLine) && (s[0] == '^'))
					continue;	// Can't match start of line if end position after start of line
				startOfLine = endPos;
			}
			if (line == lineRangeStart) 
			{
				if ((startPos != endOfLine) && (searchEnd == '$'))
					continue;	// Can't match end of line if start position before end of line
				endOfLine = startPos;
			}
		}

		bool success = boost::xpressive::regex_search(DocumentIterator(this, startOfLine, endOfLine), DocumentIterator(this, endOfLine, endOfLine), pre->match, pre->re);
		if (success) 
		{
			pos = startOfLine + pre->match.position(0);
			lenRet = pre->match.length();
			if (increment == -1) 
			{
				// Check for the last match on this line.
				int repetitions = 1000;	// Break out of infinite loop
				while (success && ((pos+lenRet) <= endOfLine) && (repetitions--)) 
				{
					success = boost::xpressive::regex_search(DocumentIterator(this, pos+1, endOfLine), DocumentIterator(this, endOfLine, endOfLine), pre->match, pre->re);
					if (success) 
					{
						if ((pos+lenRet) <= minPos) 
						{
							pos = (pos+1) + pre->match.position();
							lenRet = pre->match.length();
						} 
						else 
						{
							success = 0;
						}
					}
				}
			}
			
			break;
		}
	}
	
	*length = lenRet;
	
	return pos;
}

const char *Document::SubstituteByPosition(const char *text, int *length) {
	if (!pre)
		return 0;
	delete []substituted;
	substituted = 0;
	/*DocumentIndexer di(this, Length());
	if (!pre->GrabMatches(di))
		return 0;*/
	unsigned int lenResult = 0;
	for (int i = 0; i < *length; i++) {
		if (text[i] == '\\') {
			if (text[i + 1] >= '1' && text[i + 1] <= '9') {
				unsigned int patNum = text[i + 1] - '0';
				if (pre->match.size() > patNum) {
					
					lenResult += pre->match.length(patNum);
				} else {
					// We'll insert the \x
					lenResult += 2;
				}
				i++;
			} else {
				switch (text[i + 1]) {
				case 'a':
				case 'b':
				case 'f':
				case 'n':
				case 'r':
				case 't':
				case 'v':
					i++;
				}
				lenResult++;
			}
		} else {
			lenResult++;
		}
	}
	substituted = new char[lenResult + 1];
	if (!substituted)
		return 0;
	char *o = substituted;
	for (int j = 0; j < *length; j++) {
		if (text[j] == '\\') {
			if (text[j + 1] >= '1' && text[j + 1] <= '9') {
				unsigned int patNum = text[j + 1] - '0';
				if (pre->match.size() > patNum) {
					memcpy(o, pre->match.str(patNum).c_str(), pre->match.length(patNum));
					o += pre->match.length(patNum);
				} else {
					*o++ = '\\';
					*o++ = text[j];
				}

				j++;
			} else {
				j++;
				switch (text[j]) {
				case 'a':
					*o++ = '\a';
					break;
				case 'b':
					*o++ = '\b';
					break;
				case 'f':
					*o++ = '\f';
					break;
				case 'n':
					*o++ = '\n';
					break;
				case 'r':
					*o++ = '\r';
					break;
				case 't':
					*o++ = '\t';
					break;
				case 'v':
					*o++ = '\v';
					break;
				default:
					*o++ = '\\';
					j--;
				}
			}
		} else {
			*o++ = text[j];
		}
	}
	*o = '\0';
	*length = lenResult;
	return substituted;
}