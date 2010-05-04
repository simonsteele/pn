/**
 * @file schemelexer.h
 * @brief Custom lexer for user-defined languages - based on simple language settings.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * This lexer is based upon the fact that most languages have a very simple
 * set of constructs and that most such constructs have starts, finishes and 
 * defined content types. Hopefully this lexer works with an abstraction of the
 * concepts present in these languages.
 */
#ifndef schemelexer_h__included
#define schemelexer_h__included

#include "lexerutils.h"
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

#define MAX_STRINGTYPES		2	// Number of string types supported.
#define MAX_KEYWORDS		5	// Number of comment types supported.

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
 * @brief Represents one custom lexer.
 */
class CustomLexer : public Lexer
{
	public:
		CustomLexer();

		virtual const char* GetName() const
		{
			return tsName.c_str();
		}

		virtual void DoLex(unsigned int startPos, int length, int initStyle, char *words[],
                            Accessor &styler) const;

		virtual void DoFold(unsigned int startPos, int length, int initStyle, WordList *keywordlists[],
                            Accessor &styler) const;

	// Custom Lexer Attributes
	public:
		std::string		tsName;
		bool			bCaseSensitive;
		StringType_t	stringTypes[MAX_STRINGTYPES];
		bool			kwEnable[MAX_KEYWORDS];

		bool			bPreProc;
		bool			bPreProcContinuation;
		char			preProcStart;
		char			preProcContinue;

		CommentType_t	singleLineComment;
		CommentType_t	blockComment;

		CharSet			wordStartSet;
		CharSet			wordContentSet;

		CharSet			numberStartSet;
		CharSet			numberContentSet;

		CharSet			identStartSet;
		CharSet			identContentSet;

	private:
		inline bool IsAWordStart(int ch) const;
		inline bool IsAWordChar(int ch) const;
		inline bool IsANumStart(int ch) const;
		inline bool IsANumChar(int ch) const;
};

#endif