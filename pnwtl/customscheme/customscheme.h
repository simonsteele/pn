/**
 * @file CustomScheme.h
 * @brief Defines the CustomLexerFactory class.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "lexerutils.h"
#include "schemelexer.h"
#include "../xmlparser.h"

#define STATE_DEFAULT		0
#define STATE_INSCHEME		1
#define STATE_INSTRINGS		2
#define STATE_INLANG		3
#define STATE_INUSEKW		4
#define STATE_INCOMMENTS	5

#define CT_LINE		1
#define CT_BLOCK	2

#define SBOOL(x) ((x[0] == _T('t')) || (x[0] == _T('T')))

class CustomLexerFactory : public XMLParseState
{
	public:
		CustomLexerFactory(const TCHAR* path);

	protected:
		bool Parse(LPCTSTR file);

		virtual void startElement(XML_CSTR name, const XMLAttributes& atts);
		virtual void endElement(XML_CSTR name);
		virtual void characterData(XML_CSTR data, int len);

		void doStringType(const XMLAttributes& atts);
		void doScheme(const XMLAttributes& atts);
		void doKeyword(const XMLAttributes& atts);
		void doPreProcessor(const XMLAttributes& atts);
		void doNumbers(const XMLAttributes& atts);
		void doIdentifiers(const XMLAttributes& atts);
		void doIdentifiers2(const XMLAttributes& atts);
		
		void SetCommentTypeCode(LPCSTR pVal, ECodeLength& length, 
			char* code, char*& pCode, CommentType_t* type);
		void doCommentType(int type, const XMLAttributes& atts);

	protected:
		XMLParser		m_parser;
		CustomLexer*	m_pCurrent;
		bool			m_bFileOK;
		int				m_state;
};