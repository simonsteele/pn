/**
 * @file CustomScheme.h
 * @brief Defines the CustomLexerFactory class.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "schemelexer.h"
#include "../xmlparser.h"

class CustomLexerFactory : public XMLParseState
{
	typedef std::vector<LexerConfig*> Lexers;

	public:
		explicit CustomLexerFactory(const TCHAR* path, Lexers& lexers);

	protected:
		bool Parse(LPCTSTR file);

		virtual void startElement(XML_CSTR name, const XMLAttributes& atts);
		virtual void endElement(XML_CSTR name);
		virtual void characterData(XML_CSTR data, int len);

		void doStringType(const XMLAttributes& atts);
		void doScheme(const XMLAttributes& atts);
		void doKeywords(const XMLAttributes& atts);
		void doKeyword(const XMLAttributes& atts);
		void doPreProcessor(const XMLAttributes& atts);
		void doNumbers(const XMLAttributes& atts);
		void doIdentifiers(const XMLAttributes& atts);
		void doIdentifiers2(const XMLAttributes& atts);
		
		void SetCommentTypeCode(LPCSTR pVal, ECodeLength& length, 
			char* code, char*& pCode, CommentType_t* type);
		void doCommentType(int type, const XMLAttributes& atts);

	protected:
		Lexers&			m_lexers;
		XMLParser		m_parser;
		LexerConfig*	m_pCurrent;
		bool			m_bFileOK;
		int				m_state;
};