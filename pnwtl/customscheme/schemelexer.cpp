/**
 * @file schemelexer.cpp
 * @brief Custom lexer for user-defined languages - based on simple language settings.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "schemelexer.h"

#include "scintilla/accessor.h"
#include "scintilla/windowaccessor.h"
#include "scintilla/stylecontext.h"

CustomLexer::CustomLexer()
{
	static const CharSet chStartSet("[a-zA-Z0-9_]");

	for(int i = 0; i < MAX_STRINGTYPES; i++)
	{
		memset(&stringTypes[i], 0, sizeof(StringType_t));
		stringTypes[i].bValid = false;
	}

	memset(kwEnable, 0, sizeof(kwEnable));

	bCaseSensitive = true;
	bPreProc = false;

	singleLineComment.scLength = eDouble;
	singleLineComment.pSCode = NULL;
	singleLineComment.pECode = NULL;

	memset(&singleLineComment, 0, sizeof(CommentType_t));
	memset(&blockComment, 0, sizeof(CommentType_t));
	singleLineComment.relatedStyle = STYLE_LINECOMMENT;
	blockComment.relatedStyle = STYLE_BLOCKCOMMENT;

	wordContentSet = chStartSet;
	wordStartSet = chStartSet;
}

///@todo check identifier range characters...
bool CustomLexer::IsAWordStart(int ch) const
{
	//return (ch < 0x80) && (isalnum(ch) || ch == '_');
	return wordStartSet.Match(ch);
}

///@todo check valid identifier characters...
bool CustomLexer::IsAWordChar(int ch) const
{
	//return (ch < 0x80) && (isalnum(ch) /*|| ch == '.'*/ || ch == '_');
	return wordContentSet.Match(ch);
}

bool CustomLexer::IsANumStart(int ch) const
{
	return numberStartSet.Match(ch);
}

bool CustomLexer::IsANumChar(int ch) const
{
	return numberContentSet.Match(ch);
}

/**
 * TODO:
 *
 * Should we get rid of the arbitrary 100-character keyword-length limit?
 *
 * Styling within pre-processor?
 */

#define LINE_CONTINUE(x) \
	if (cc.ch == x) { \
			if (cc.chNext == '\n' || cc.chNext == '\r') { \
				cc.Forward(); \
				if (cc.ch == '\r' && cc.chNext == '\n') { \
					cc.Forward(); \
				} \
				continue; \
			} \
		}

void CustomLexer::DoLex(unsigned int startPos, int length, int initStyle, WordList *keywordlists[],
					Accessor &styler) const
{
	// String EOL styles do not leak onto the next line - could these styles be the same one?
	bool s1 = stringTypes[0].bValid;
	bool s2 = stringTypes[1].bValid;
		
	StyleContext cc(startPos, length, initStyle, styler);

	//Here we loop over the characters we're working with...
	for(; cc.More(); cc.Forward())
	{
		// Check for end-of-line with a non multiline string...
		if( cc.state == STYLE_LINECOMMENT )
		{
			if( singleLineComment.bContinuation )
			{
				LINE_CONTINUE(singleLineComment.continuation);
			}

			if( cc.atLineEnd )
				cc.SetState(ST_DEFAULT);
		}
		else if( cc.state == STYLE_BLOCKCOMMENT )
		{
			if(blockComment.ecLength == eSingle)
			{
				if( cc.Match(blockComment.ecode[0]) )
					cc.SetState(ST_DEFAULT);
			}
			else if(blockComment.ecLength == eDouble)
			{
				if( cc.Match(blockComment.ecode[0], blockComment.ecode[1]) )
					cc.SetState(ST_DEFAULT);
			}
			else
			{
				if( cc.MatchIgnoreCase(blockComment.pECode) )
				{
					cc.Forward(_tcslen(blockComment.pECode));
					cc.SetState(ST_DEFAULT);
				}
			}
		}
		else if( cc.state == STYLE_IDENTIFIER )
		{
			if(! IsAWordChar(cc.ch) )
			{
				char s[100];
				if(bCaseSensitive)
					cc.GetCurrent(s, sizeof(s));
				else
					cc.GetCurrentLowered(s, sizeof(s));
				
				for(int z = 0; z < MAX_KEYWORDS; z++)
				{
					if( (*keywordlists[z]).InList(s) )
					{
						cc.ChangeState(STYLE_KEYWORDS + z);
						break;
					}
				}
				
				cc.SetState(ST_DEFAULT);
			}
		}
		else if( cc.state == STYLE_NUMBER )
		{
			///@todo - should this undo the setting of the number set if it 
			//finds a non-matching non-space char?
			if( cc.atLineEnd || !numberContentSet.Match(cc.ch))
				cc.SetState(ST_DEFAULT);
		}
		else if( cc.state == STYLE_KNOWNIDENT )
		{
			///@todo - if we find a non-valid non-space char should we cancel
			//the word state?
			if( cc.atLineEnd || !identContentSet.Match(cc.ch))
				cc.SetState(ST_DEFAULT);
		}
		else if( cc.state == STYLE_STRING )
		{
			if( stringTypes[0].bContinuation )
			{
				LINE_CONTINUE( stringTypes[0].continuation );
			}

			if( cc.atLineEnd && !stringTypes[0].multiLine)
			{
				cc.SetState(ST_DEFAULT);
			}
			else if( stringTypes[0].bEscape && cc.Match( stringTypes[0].escape ) )
			{
				// Skip over a double-escape, or a double end-string.
				if( cc.chNext == stringTypes[0].escape || cc.chNext == stringTypes[0].end )
					cc.Forward();
			} 
			else if( cc.Match( stringTypes[0].end ) )
				cc.ForwardSetState(ST_DEFAULT);
		}
		else if( cc.state == STYLE_STRING2 )
		{
			if( stringTypes[1].bContinuation )
			{
				LINE_CONTINUE( stringTypes[1].continuation );
			}

			if( cc.atLineEnd && !stringTypes[1].multiLine )
			{
				cc.SetState(ST_DEFAULT);
			}
			else if( stringTypes[1].bEscape && cc.Match(stringTypes[1].escape) )
			{
				// Skip over a double-escape, or a double end-string.
				if( cc.chNext == stringTypes[1].escape || cc.chNext == stringTypes[1].end )
					cc.Forward();
			} 
			else if( cc.ch == stringTypes[1].end )
				cc.ForwardSetState(ST_DEFAULT);
		}
		else if( cc.state == STYLE_PREPROC )
		{
			if(bPreProcContinuation)
			{
				LINE_CONTINUE(preProcContinue);
			}
			
			if(cc.atLineEnd)
				cc.SetState(ST_DEFAULT);
		}
		// Finally we do default state handling...
		else if( cc.state == ST_DEFAULT )
		{
			if( s1 && (cc.ch == stringTypes[0].start) )
			{
				cc.SetState(STYLE_STRING);
			} 
			else if( s2 && (cc.ch == stringTypes[1].start) )
			{
				cc.SetState(STYLE_STRING2);
			}
			else if( bPreProc && (cc.ch == preProcStart) )
			{
				cc.SetState(STYLE_PREPROC);
			}
			else if( IsANumStart(cc.ch) )
			{
				cc.SetState(STYLE_NUMBER);
			}
			else if( identStartSet.Match(cc.ch) )
			{
				cc.SetState(STYLE_KNOWNIDENT);
			}
			else if( IsAWordStart(cc.ch) )
			{
				cc.SetState(STYLE_IDENTIFIER);
			}
			else
			{
				const CommentType_t* types[2] = {&singleLineComment, &blockComment};
				
				for(int cs = 0; cs < 2; cs++)
				{
					const CommentType_t& comment = *types[cs];
					if(!comment.bValid)
						continue;
					if( comment.scLength == eSingle )
					{
						if( cc.Match(comment.scode[0]) )
						{
							cc.SetState(comment.relatedStyle); 
							break;
						}
					}
					else if( comment.scLength == eDouble )
					{
						if( cc.Match(comment.scode[0], comment.scode[1]) )
						{
							cc.SetState(comment.relatedStyle);
							break;
						}
					}
					else
					{
						// multiple (>2) character start code.
						if( cc.MatchIgnoreCase(comment.pSCode) )
						{
							cc.SetState(comment.relatedStyle);
							cc.Forward(_tcslen(comment.pSCode)-1);
							break;
						}
					} // comment....
				} // for each comment type
			} // else check comment types....
		} // else if default state
	} // for each char

	cc.Complete();
}

void CustomLexer::DoFold(unsigned int startPos, int length, int initStyle, WordList *keywordlists[],
                    Accessor &styler) const
{

}