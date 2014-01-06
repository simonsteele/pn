#ifndef CLIPPARSER_H_INCLUDED
#define CLIPPARSER_H_INCLUDED

#pragma once

#include "../textclips.h"

namespace TextClips {

/**
 * Text Clip File XML Parser Handler.
 */
class Parser : public XMLParseState
{
	public:
		Parser(LIST_CLIPSETS& clipSets, LPCTSTR filename) : m_parseState(0), decodeNames(false), m_pCurSet(NULL), m_clipSets(clipSets), m_curFileName(filename) {}
	
	//XMLParseState
	public:
		virtual void startElement(LPCTSTR name, const XMLAttributes& atts);
		virtual void endElement(LPCTSTR name);
		virtual void characterData(LPCTSTR data, int len);

		void decodeData();

	private:
		typedef enum tagEncodings
		{
			eNone,
			eWindows1252,
			eANSI,
		} EEncoding;

		// Parse state:
		bool decodeNames;
		int	m_parseState;
		std::string m_cData;
		tstring m_curName;
		std::string m_curShortcut;
		tstring m_curFileName;
		TextClipSet* m_pCurSet;
		EEncoding m_curEncoding;

		// Output:
		LIST_CLIPSETS&	m_clipSets;
		//MAP_CLIPSETS	m_schemeClipSets;
};

}

#endif // #ifndef CLIPPARSER_H_INCLUDED