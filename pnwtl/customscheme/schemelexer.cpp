/**
 * @file schemelexer.cpp
 * @brief Custom lexer for user-defined languages - based on simple language settings.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "schemelexer.h"

CustomLexer::CustomLexer(const LexerConfig& config) : m_config(config)
{
}

///@todo check identifier range characters...
bool CustomLexer::IsAWordStart(int ch) const
{
	//return (ch < 0x80) && (isalnum(ch) || ch == '_');
	return m_config.wordStartSet.Match(ch);
}

///@todo check valid identifier characters...
bool CustomLexer::IsAWordChar(int ch) const
{
	//return (ch < 0x80) && (isalnum(ch) /*|| ch == '.'*/ || ch == '_');
	return m_config.wordContentSet.Match(ch);
}

bool CustomLexer::IsANumStart(int ch) const
{
	return m_config.numberStartSet.Match(ch);
}

bool CustomLexer::IsANumChar(int ch) const
{
	return m_config.numberContentSet.Match(ch);
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

void CustomLexer::Lexer(unsigned int startPos, int length, int initStyle, IDocument*, Accessor &styler)
{
	// String EOL styles do not leak onto the next line - could these styles be the same one?
	bool s1 = m_config.stringTypes[0].bValid;
	bool s2 = m_config.stringTypes[1].bValid;
		
	StyleContext cc(startPos, length, initStyle, styler);

	//Here we loop over the characters we're working with...
	for(; cc.More(); cc.Forward())
	{
		// Check for end-of-line with a non multiline string...
		if( cc.state == STYLE_LINECOMMENT )
		{
			if( m_config.singleLineComment.bContinuation )
			{
				LINE_CONTINUE(m_config.singleLineComment.continuation);
			}

			if( cc.atLineEnd )
				cc.SetState(ST_DEFAULT);
		}
		else if( cc.state == STYLE_BLOCKCOMMENT )
		{
			handleBlockComment(cc, m_config.blockComment[0]);
		}
		else if( cc.state == STYLE_BLOCKCOMMENT2 )
		{
			handleBlockComment(cc, m_config.blockComment[1]);
		}
		else if( cc.state == STYLE_BLOCKCOMMENT3 )
		{
			handleBlockComment(cc, m_config.blockComment[2]);
		}
		else if( cc.state == STYLE_UNKNOWNIDENT )
		{
			if(! IsAWordChar(cc.ch) )
			{
				//Get the current typed keyword
				char s[100];
				(m_config.bCaseSensitive) ? cc.GetCurrent(s, sizeof(s)) : cc.GetCurrentLowered(s, sizeof(s));

				//Loop through all keywords untill we find what we need
				for(int z = 0; z < MAX_KEYWORDS; z++)
				{
					if( keyWordLists[z]->InList(s) )
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
			if( cc.atLineEnd || !m_config.numberContentSet.Match(cc.ch))
				cc.SetState(ST_DEFAULT);
		}
		else if (cc.state == STYLE_IDENTIFIER)
		{
			if( cc.atLineEnd || !m_config.identContentSet.Match(cc.ch))
				cc.SetState(ST_DEFAULT);
		}
		else if( cc.state == STYLE_KNOWNIDENT )
		{
			if( cc.atLineEnd || !m_config.identContentSet2.Match(cc.ch))
				cc.SetState(ST_DEFAULT);
		}
		else if( cc.state == STYLE_STRING )
		{
			if( m_config.stringTypes[0].bContinuation )
			{
				LINE_CONTINUE( m_config.stringTypes[0].continuation );
			}

			if( cc.atLineEnd && !m_config.stringTypes[0].multiLine)
			{
				cc.SetState(ST_DEFAULT);
			}
			else if( m_config.stringTypes[0].bEscape && cc.Match( m_config.stringTypes[0].escape ) )
			{
				// Skip over a double-escape, or a double end-string.
				if( cc.chNext == m_config.stringTypes[0].escape || cc.chNext == m_config.stringTypes[0].end )
					cc.Forward();
			} 
			else if( cc.Match( m_config.stringTypes[0].end ) )
				cc.ForwardSetState(ST_DEFAULT);
		}
		else if( cc.state == STYLE_STRING2 )
		{
			if( m_config.stringTypes[1].bContinuation )
			{
				LINE_CONTINUE( m_config.stringTypes[1].continuation );
			}

			if( cc.atLineEnd && !m_config.stringTypes[1].multiLine )
			{
				cc.SetState(ST_DEFAULT);
			}
			else if( m_config.stringTypes[1].bEscape && cc.Match(m_config.stringTypes[1].escape) )
			{
				// Skip over a double-escape, or a double end-string.
				if( cc.chNext == m_config.stringTypes[1].escape || cc.chNext == m_config.stringTypes[1].end )
					cc.Forward();
			} 
			else if( cc.ch == m_config.stringTypes[1].end )
				cc.ForwardSetState(ST_DEFAULT);
		}
		else if( cc.state == STYLE_PREPROC )
		{
			if(m_config.bPreProcContinuation)
			{
				LINE_CONTINUE(m_config.preProcContinue);
			}
			
			if(cc.atLineEnd)
				cc.SetState(ST_DEFAULT);
		}
		
		// Finally we do default state handling...
		if( cc.state == ST_DEFAULT )
		{
			if( s1 && (cc.ch == m_config.stringTypes[0].start) )
			{
				cc.SetState(STYLE_STRING);
			} 
			else if( s2 && (cc.ch == m_config.stringTypes[1].start) )
			{
				cc.SetState(STYLE_STRING2);
			}
			else if( m_config.bPreProc && (cc.ch == m_config.preProcStart) )
			{
				cc.SetState(STYLE_PREPROC);
			}
			else if( IsANumStart(cc.ch) )
			{
				cc.SetState(STYLE_NUMBER);
			}
			else if( m_config.identStartSet2.Match(cc.ch) )
			{
				cc.SetState(STYLE_KNOWNIDENT);
			}
			else if( IsAWordStart(cc.ch) )
			{
				cc.SetState(STYLE_UNKNOWNIDENT);
			}
			else if (m_config.identStartSet.Match(cc.ch))
			{
				cc.SetState(STYLE_IDENTIFIER);
			}
			else
			{
				const CommentType_t* types[4] = { &m_config.singleLineComment, &m_config.blockComment[0], &m_config.blockComment[1], &m_config.blockComment[2] };
				
				for(int cs = 0; cs < _countof(types); cs++)
				{
					const CommentType_t& comment = *types[cs];
					if(!comment.bValid)
					{
						continue;
					}

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
							cc.Forward(strlen(comment.pSCode)-1);
							break;
						}
					} // comment....
				} // for each comment type
			} // else check comment types....
		} // else if default state
	} // for each char

	cc.Complete();
}

void CustomLexer::handleBlockComment(StyleContext& cc, const CommentType_t& comment)
{
	if (comment.ecLength == eSingle)
	{
		if( cc.Match(comment.ecode[0]) )
			cc.ForwardSetState(ST_DEFAULT);
	}
	else if(comment.ecLength == eDouble)
	{
		if( cc.Match(comment.ecode[0], comment.ecode[1]) )
		{
			cc.Forward();
			cc.ForwardSetState(ST_DEFAULT);
		}
	}
	else
	{
		if( cc.MatchIgnoreCase(comment.pECode) )
		{
			cc.Forward(strlen(comment.pECode));
			cc.SetState(ST_DEFAULT);
		}
	}
}

void CustomLexer::Folder(unsigned int startPos, int length, int initStyle, IDocument*, Accessor &styler)
{

}