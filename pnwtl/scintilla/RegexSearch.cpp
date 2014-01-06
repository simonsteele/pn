/**
 * @file RegexSearch.cpp
 * @brief XPressive searching for Scintilla, developed for Programmer's Notepad.
 * @author Simon Steele
 * @note Copyright (c) since 2009-2012 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */ 
#include <stdlib.h>

#include "Platform.h"
#include "SplitVector.h"
#include "Partitioning.h"
#include "RunStyles.h"
#include "CellBuffer.h"
#include "CharClassify.h"
#include "Decoration.h"
#include <ILexer.h>
#include <Document.h>

#include <boost/xpressive/xpressive.hpp>

#ifdef SCI_NAMESPACE
using namespace Scintilla;
#endif

using namespace boost::xpressive;

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
		// Check for debug builds
		PLATFORM_ASSERT(m_pos <= m_end);

		// Ensure for release.
		if (m_pos > m_end)
		{
			m_pos = m_end;
		}
	}

	DocumentIterator(const DocumentIterator& copy) :
		m_doc(copy.m_doc),
		m_pos(copy.m_pos),
		m_end(copy.m_end)
	{
		// Check for debug builds
		PLATFORM_ASSERT(m_pos <= m_end);

		// Ensure for release.
		if (m_pos > m_end)
		{
			m_pos = m_end;
		}
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
		PLATFORM_ASSERT(m_pos < m_end);

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

typedef basic_regex<DocumentIterator> docregex;
typedef match_results<DocumentIterator> docmatch;
typedef sub_match<DocumentIterator> docsub_match;

class XpressiveRegexSearch : public RegexSearchBase
{
public:
	XpressiveRegexSearch() : substituted(NULL){}
	
	virtual ~XpressiveRegexSearch()
	{
		if (substituted)
		{
			delete [] substituted;
			substituted = NULL;
		}
	}

	virtual long FindText(Document* doc, int minPos, int maxPos, const char *s,
                        bool caseSensitive, bool word, bool wordStart, int flags, int *length);

	virtual long OldFindText(Document* doc, int minPos, int maxPos, const char *s,
                        bool caseSensitive, bool word, bool wordStart, bool posix, int *length);

	virtual const char *SubstituteByPosition(Document* doc, const char *text, int *length);

private:
	docregex re;
	docmatch match;
	char *substituted;
	std::string restring;
};

namespace Scintilla
{

RegexSearchBase *CreateRegexSearch(CharClassify *charClassTable)
{
	return new XpressiveRegexSearch();
}

}

/**
 * Utility function to enclode unicode multi-byte encodings with non-capture groups: (?:mbcs)
 */
std::string& convertUTF8Regex(std::string& regexStr);

/**
 * Find text in document, supporting both forward and backward
 * searches (just pass minPos > maxPos to do a backward search)
 * Has not been tested with backwards DBCS searches yet.
 */
long XpressiveRegexSearch::FindText(Document* doc, int minPos, int maxPos, const char *s,
                        bool caseSensitive, bool word, bool wordStart, int searchFlags, int *length) 
{
	int startPos, endPos, increment;

	if (minPos > maxPos)
	{
		startPos = maxPos;
		endPos = minPos;
		increment = -1;
	}
	else
	{
		startPos = minPos;
		endPos = maxPos;
		increment = 1;
	}

	// Range endpoints should not be inside DBCS characters, but just in case, move them.
	startPos = doc->MovePositionOutsideChar(startPos, 1, false);
	endPos = doc->MovePositionOutsideChar(endPos, 1, false);

	restring = std::string(s, *length);
	restring = convertUTF8Regex(restring);

	int compileFlags(regex_constants::ECMAScript | regex_constants::not_dot_newline);
	if (!caseSensitive)
	{
		compileFlags |= regex_constants::icase;
	}
	
	try
	{
		re = docregex::compile(restring.c_str(), static_cast<regex_constants::syntax_option_type>(compileFlags));
	}
	catch(regex_error& /*ex*/)
	{
		// -1 is normally used for not found, -2 is used here for invalid regex
		return -2;
	}

	// Work out the range of lines we're searching across, moving beyond an empty end-of-line
	int lineRangeStart = doc->LineFromPosition(startPos);
	int lineRangeEnd = doc->LineFromPosition(endPos);
	if ((increment == 1) &&
		(startPos >= doc->LineEnd(lineRangeStart)) &&
		(lineRangeStart < lineRangeEnd)) 
	{
		// the start position is at end of line or between line end characters.
		lineRangeStart++;
		startPos = doc->LineStart(lineRangeStart);
	}

	int flags(regex_constants::match_default);

	// Work out the flags:
	if (startPos != doc->LineStart(lineRangeStart))
	{
		flags |= regex_constants::match_not_bol;
	}

	if (endPos != doc->LineEnd(lineRangeEnd))
	{
		flags |= regex_constants::match_not_eol;
	}

	int pos(-1);
	int lenRet(0);
	DocumentIterator end(doc, endPos, endPos);
	bool success = regex_search(DocumentIterator(doc, startPos, endPos), end, match, re, static_cast<regex_constants::match_flag_type>(flags));
	if (success)
	{
		pos = startPos + match.position(0);
		lenRet = match.length();
		
		if (increment == -1)
		{
			// Check for the last match on this line.
			int repetitions = 1000;	// Break out of infinite loop
			while (success && ((pos + lenRet) <= endPos) && (repetitions--)) 
			{
				success = regex_search(DocumentIterator(doc, pos + 1, endPos), end, match, re, static_cast<regex_constants::match_flag_type>(flags));
				if (success) 
				{
					if ((pos + lenRet) <= minPos) 
					{
						pos = (pos + 1) + match.position(0);
						lenRet = match.length();
					} 
					else 
					{
						success = 0;
					}
				}
			}
		}
		
		*length = lenRet;
	}

	return pos;
}

/**
 * Find text in document, supporting both forward and backward
 * searches (just pass minPos > maxPos to do a backward search)
 * Has not been tested with backwards DBCS searches yet.
 */
long XpressiveRegexSearch::OldFindText(Document* doc, int minPos, int maxPos, const char *s,
                        bool caseSensitive, bool word, bool wordStart, bool posix, int *length) 
{
	
	//@todo The character class support should be initialised from this->charClass

	int increment = (minPos <= maxPos) ? 1 : -1;

	int startPos = minPos;
	int endPos = maxPos;

	// Range endpoints should not be inside DBCS characters, but just in case, move them.
	startPos = doc->MovePositionOutsideChar(startPos, 1, false);
	endPos = doc->MovePositionOutsideChar(endPos, 1, false);

	std::string restring(s, *length);
	
	try
	{
		re = docregex::compile(restring.c_str());
	}
	catch(boost::xpressive::regex_error& /*ex*/)
	{
		// -1 is normally used for not found, -2 is used here for invalid regex
		return -2;
	}

	int lineRangeStart = doc->LineFromPosition(startPos);
	int lineRangeEnd = doc->LineFromPosition(endPos);
	if ((increment == 1) &&
		(startPos >= doc->LineEnd(lineRangeStart)) &&
		(lineRangeStart < lineRangeEnd)) 
	{
		// the start position is at end of line or between line end characters.
		lineRangeStart++;
		startPos = doc->LineStart(lineRangeStart);
	}
	
	int pos = -1;
	int lenRet = 0;
	char searchEnd = s[*length - 1];
	int lineRangeBreak = lineRangeEnd + increment;
	for (int line = lineRangeStart; line != lineRangeBreak; line += increment) 
	{
		int startOfLine = doc->LineStart(line);
		int endOfLine = doc->LineEnd(line);
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

		bool success = boost::xpressive::regex_search(DocumentIterator(doc, startOfLine, endOfLine), DocumentIterator(doc, endOfLine, endOfLine), match, re);
		if (success) 
		{
			pos = startOfLine + match.position(0);
			lenRet = match.length();
			if (increment == -1) 
			{
				// Check for the last match on this line.
				int repetitions = 1000;	// Break out of infinite loop
				while (success && ((pos+lenRet) <= endOfLine) && (repetitions--)) 
				{
					success = boost::xpressive::regex_search(DocumentIterator(doc, pos+1, endOfLine), DocumentIterator(doc, endOfLine, endOfLine), match, re);
					if (success) 
					{
						if ((pos+lenRet) <= minPos) 
						{
							pos = (pos+1) + match.position(0);
							lenRet = match.length();
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


const char *XpressiveRegexSearch::SubstituteByPosition(Document* doc, const char *text, int *length) {
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
                                if (match.size() > patNum) {
                                        
                                        lenResult += match.length(patNum);
                                } else {
                                        // We'll insert the \x
                                        lenResult += 2;
                                }
                                i++;
                        } else {
                                switch (text[i + 1]) {
                                case '\\':
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
                                if (match.size() > patNum) {
                                        memcpy(o, match.str(patNum).c_str(), match.length(patNum));
                                        o += match.length(patNum);
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
                                case '\\':
                                        *o++ = '\\';
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

std::string& convertUTF8Regex(std::string& regexStr) {
	enum Conv_States { ASCII_CHAR, ESCAPE_CHAR, ESCAPED_SEQUENCE, UTF_SEQUENCE};
		 //TODO: deal with escaped sequences! 
	Conv_States state = ASCII_CHAR;
	std::string	temp;
	for(size_t i = 0; i < regexStr.length(); ++i) {
		char ch = regexStr[i];
		switch(state) {
		case ASCII_CHAR:
			if(ch == '\\') {
				state = ESCAPE_CHAR;
			} else if ((ch & 0x80) == 0x80) { //begin an encoded sequence
				temp.append("(?:");
				state = UTF_SEQUENCE;
			} 
			temp.push_back(ch);
            break;
		case ESCAPE_CHAR:
			if(ch == 'Q') {
				state = ESCAPED_SEQUENCE;
			} else if (ch == 'E') {
				state = ASCII_CHAR;
			}
			temp.push_back(ch);
            break;
		case ESCAPED_SEQUENCE:
			if(ch == '\\') {
				state = ESCAPE_CHAR;
			}
			temp.push_back(ch);
            break;
		case UTF_SEQUENCE:
			if ((ch & 0x80) != 0x80) { //found a non-encoded char -- end current run.
				temp.push_back(')');
				if (ch == '\\') {
					state = ESCAPE_CHAR;
					break;
				} else {
					state = ASCII_CHAR;
				}
			} 
			temp.push_back(ch);
            break;
		}
	}
	if (state == UTF_SEQUENCE) {
		temp.push_back(')');
	}
	std::swap(regexStr, temp);
	return regexStr;
}