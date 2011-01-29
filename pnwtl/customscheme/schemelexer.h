/**
 * @file schemelexer.h
 * @brief Custom lexer for user-defined languages - based on simple language settings.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
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

#include "charset.h"
#include "lexerconfig.h"

/**
 * @brief Represents one custom lexer.
 */
class CustomLexer : public LexerNoExceptions
{
	public:
		CustomLexer(const LexerConfig& config);
		virtual ~CustomLexer(){}

		const char* GetName() const
		{
			return m_config.tsName.c_str();
		}

		// Signature for Scintilla lexers:
		void Lexer(unsigned int startPos, int length, int initStyle, IDocument *, Accessor &styler);
		void Folder(unsigned int startPos, int length, int initStyle, IDocument *, Accessor &styler);

	// Custom Lexer Attributes
	public:
		const LexerConfig& m_config;

	private:
		inline bool IsAWordStart(int ch) const;
		inline bool IsAWordChar(int ch) const;
		inline bool IsANumStart(int ch) const;
		inline bool IsANumChar(int ch) const;

		void handleBlockComment(StyleContext& cc, const CommentType_t& comment);
};

#endif