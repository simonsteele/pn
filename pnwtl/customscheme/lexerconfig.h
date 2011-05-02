/**
 * @file lexerconfig.h
 * @brief Custom lexer for user-defined languages - based on simple language settings.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef LEXERCONFIG_H_INCLUDED
#define LEXERCONFIG_H_INCLUDED

#include "charset.h"

#define ST_DEFAULT			0
#define STYLE_LINECOMMENT	1
#define STYLE_BLOCKCOMMENT	2
#define STYLE_IDENTIFIER	3
#define STYLE_NUMBER		4
#define STYLE_KEYWORDS		5
#define STYLE_KEYWORDS2		6
#define STYLE_KEYWORDS3		7
#define STYLE_KEYWORDS4		8
#define STYLE_KEYWORDS5		9
#define STYLE_STRING		10
#define STYLE_STRING2		11
#define STYLE_PREPROC		12
#define STYLE_KNOWNIDENT	13
#define STYLE_BLOCKCOMMENT2 14
#define STYLE_BLOCKCOMMENT3 15
#define STYLE_UNKNOWNIDENT  16

#define MAX_STRINGTYPES		2	// Number of string types supported.
#define MAX_KEYWORDS		5	// Number of comment types supported.
#define MAX_BLOCKCOMMENTS   3   // Number of block comment types supported.

/**
 * All the settings for each string type supported.
 */
typedef struct tagStringType
{
	bool bValid;

	char start;
	char end;
	bool multiLine;
	
	bool bContinuation;
	char continuation;

	bool bEscape;
	char escape;
} StringType_t;

typedef enum {eSingle, eDouble, eMore} ECodeLength;

/**
 * All the settings for each comment type supported.
 */
typedef struct tagCommentSettings
{
	bool bValid;

	// Start and end of comment code lengths
	ECodeLength scLength;
	ECodeLength ecLength;
	
	// First and second chars for eSingle or eDouble mode, 
	// pCode valid in eMore mode.
	char		scode[2];
	char		ecode[2];
	char*		pSCode;
	char*		pECode;
	
	bool		bContinuation;
	char		continuation;

	// Set to the Style to set if this type is detected...
	int			relatedStyle;
} CommentType_t;

/**
 * Lexer Configuration.
 */
class LexerConfig
{
	public:
		explicit LexerConfig()
		{
			static const CharSet chStartSet("[-a-zA-Z0-9_]");

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
			memset(&blockComment, 0, sizeof(blockComment));
			
			singleLineComment.relatedStyle = STYLE_LINECOMMENT;
			blockComment[0].relatedStyle = STYLE_BLOCKCOMMENT;
			blockComment[1].relatedStyle = STYLE_BLOCKCOMMENT2;
			blockComment[2].relatedStyle = STYLE_BLOCKCOMMENT3;

			wordContentSet = chStartSet;
			wordStartSet = chStartSet;
		}

		const char* GetName() const
		{
			return tsName.c_str();
		}

		std::string		tsName;
		bool			bCaseSensitive;
		StringType_t	stringTypes[MAX_STRINGTYPES];
		bool			kwEnable[MAX_KEYWORDS];

		bool			bPreProc;
		bool			bPreProcContinuation;
		char			preProcStart;
		char			preProcContinue;

		CommentType_t	singleLineComment;
		CommentType_t	blockComment[MAX_BLOCKCOMMENTS];

		CharSet			wordStartSet;
		CharSet			wordContentSet;

		CharSet			numberStartSet;
		CharSet			numberContentSet;

		CharSet			identStartSet;
		CharSet			identContentSet;

		CharSet			identStartSet2;
		CharSet			identContentSet2;
};

#endif // LEXERCONFIG_H_INCLUDED