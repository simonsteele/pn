/**
 * @file rtfexporter.cpp
 * @brief RTF Exporter
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/, Portions copyright SciTE authors.
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "../exporters.h"

////////////////////////////////////////////////////////////////////////////////
// RTFExporter class

#define RTF_HEADEROPEN    "{\\rtf1\\ansi\\deff0\\deftab720"
#define RTF_FONTDEFOPEN   "{\\fonttbl"
#ifdef _UNICODE
#define RTF_FONTDEF       "{\\f%d\\fnil\\fcharset%u %S;}"
#else
#define RTF_FONTDEF       "{\\f%d\\fnil\\fcharset%u %s;}"
#endif
#define RTF_FONTDEFCLOSE  "}"
#define RTF_COLORDEFOPEN  "{\\colortbl;"
#define RTF_COLORDEF      "\\red%d\\green%d\\blue%d;"
#define RTF_COLORDEFCLOSE "}"
#define RTF_HEADERCLOSE   "\n"
#define RTF_BODYOPEN      ""
#define RTF_BODYCLOSE     "}"

#define RTF_SETFONTFACE   "\\f"
#define RTF_SETFONTSIZE   "\\fs"
#define RTF_SETCOLOR      "\\cf"

// Note chcbpat found by exporting from Word, it appears that the documented \cb doesn't work:
#define RTF_SETBACKCOLOR  "\\chcbpat"

#define RTF_BOLD_ON       "\\b"
#define RTF_BOLD_OFF      "\\b0"
#define RTF_ITALIC_ON     "\\i"
#define RTF_ITALIC_OFF    "\\i0"
#define RTF_UNDERLINE_ON  "\\ul"
#define RTF_UNDERLINE_OFF "\\ulnone"
#define RTF_STRIKE_ON     "\\i"
#define RTF_STRIKE_OFF    "\\strike0"

#define RTF_EOLN          "\\par\n"
#define RTF_TAB           "\\tab "

#define MAX_STYLEDEF      128
#define MAX_FONTDEF       64
#define MAX_COLORDEF      8
#define RTF_FONTFACE      "Courier New"
#define RTF_COLOR         "#000000"

RTFExporter::RTFExporter(IOutput* pOutput, LPCSTR lpszSchemeName, StylesList* pStyles, CScintilla* pScintilla)
: BaseExporter(pOutput, lpszSchemeName, pStyles, pScintilla)
{

}

int RTFExporter::GetHexChar(char ch) 
{ // 'H'
	return ch > '9' ? (ch | 0x20) - 'a' + 10 : ch - '0';
}

int RTFExporter::GetHexByte(const char *hexbyte) 
{ // "HH"
	return (GetHexChar(*hexbyte) << 4) | GetHexChar(hexbyte[1]);
}

/**
 * There are a fixed number of highlights available to us. We try and find
 * the one that most closely matches any background colour set.
 */
int RTFExporter::GetRTFHighlight(const char *rgb)
{ // "#RRGGBB"
	static int highlights[][3] = 
	{
		{ 0x00, 0x00, 0x00 },         // highlight1  0;0;0       black
		{ 0x00, 0x00, 0xFF },         // highlight2  0;0;255     blue
		{ 0x00, 0xFF, 0xFF },         // highlight3  0;255;255   cyan
		{ 0x00, 0xFF, 0x00 },         // highlight4  0;255;0     green
		{ 0xFF, 0x00, 0xFF },         // highlight5  255;0;255   violet
		{ 0xFF, 0x00, 0x00 },         // highlight6  255;0;0     red
		{ 0xFF, 0xFF, 0x00 },         // highlight7  255;255;0   yellow
		{ 0xFF, 0xFF, 0xFF },         // highlight8  255;255;255 white
		{ 0x00, 0x00, 0x80 },         // highlight9  0;0;128     dark blue
		{ 0x00, 0x80, 0x80 },         // highlight10 0;128;128   dark cyan
		{ 0x00, 0x80, 0x00 },         // highlight11 0;128;0     dark green
		{ 0x80, 0x00, 0x80 },         // highlight12 128;0;128   dark violet
		{ 0x80, 0x00, 0x00 },         // highlight13 128;0;0     brown
		{ 0x80, 0x80, 0x00 },         // highlight14 128;128;0   khaki
		{ 0x80, 0x80, 0x80 },         // highlight15 128;128;128 dark grey
		{ 0xC0, 0xC0, 0xC0 },         // highlight16 192;192;192 grey
	};

	int maxdelta = 3 * 255 + 1, delta, index = -1;
	int r = GetHexByte(rgb + 1), g = GetHexByte(rgb + 3), b = GetHexByte(rgb + 5);

	for (unsigned int i = 0; i < sizeof(highlights) / sizeof(*highlights); i++)
	{
		delta = abs(r - *highlights[i]) + abs(g - highlights[i][1]) + 
			abs(b - highlights[i][2]);
		if (delta < maxdelta)
		{
			maxdelta = delta;
			index = i;
		}
	}
	return index + 1;
}

// \f0\fs20\cf0\highlight0\b0\i0


/**
 * This function steps through the incoming and outgoing style definitions (RTF style)
 * and decides which bits of the style have changed - so most style changes will end
 * up as just a simple colour change.
 */
std::string RTFExporter::GetRTFStyleChange(StyleDetails* currentStyle, StyleDetails* newStyle) 
{ // \f0\fs20\cf0\highlight0\b0\i0
	char sprintfbuf[40];
	std::string styleChange;
	
	if (currentStyle->BackColor != newStyle->BackColor)
	{
		sprintf(sprintfbuf, RTF_SETBACKCOLOR "%d", m_colorMap[newStyle->BackColor]);
		styleChange += sprintfbuf;
	}

	if (currentStyle->ForeColor != newStyle->ForeColor)
	{
		sprintf(sprintfbuf, RTF_SETCOLOR "%d", m_colorMap[newStyle->ForeColor]);
		styleChange += sprintfbuf;
	}

	if (currentStyle->Bold != newStyle->Bold)
	{
		styleChange += newStyle->Bold ? RTF_BOLD_ON : RTF_BOLD_OFF;
	}

	if (currentStyle->Italic != newStyle->Italic)
	{
		styleChange += newStyle->Italic ? RTF_ITALIC_ON : RTF_ITALIC_OFF;
	}

	if (currentStyle->FontSize != newStyle->FontSize)
	{
		sprintf(sprintfbuf, RTF_SETFONTSIZE "%d", newStyle->FontSize << 1);
		styleChange += sprintfbuf;
	}

	if (currentStyle->FontName != newStyle->FontName)
	{
		sprintf(sprintfbuf, RTF_SETFONTFACE "%d", m_fontMap[newStyle->FontName]);
	}

	// TODO: Underline
	
	// Terminate the RTF sequence:
	if(styleChange.size())
	{
		styleChange += " ";
	}

	return styleChange;
}

/**
 * Originally ported from SciTE code, this version removes file access and instead 
 * uses the IOutput class to store the result of its work. I also substantially rewrote
 * the handling of style changes to avoid lots of string parsing and instead use Style 
 * definitions. Finally I sorted out background handling.
 */
void RTFExporter::InternalExport(int start, int end)
{
	int wysiwyg           = /*pOptions->GetRtfWYSIWYG()*/	1;
	unsigned characterset = /*pOptions->GetRtfCharset()*/	0;
	int tabs              = /*pOptions->GetRtfTabs()*/		1;

	int tabSize = SendEditor(SCI_GETTABWIDTH, 0, 0);
	if (tabSize == 0)
		tabSize = 4;
	
	// Write the RTF header...
	m_out->puts(RTF_HEADEROPEN RTF_FONTDEFOPEN);

	StyleDetails* pDefStyle = GetStyle(STYLE_DEFAULT);

	m_out->printf(RTF_FONTDEF, 0, characterset, pDefStyle->FontName.c_str());
	
	int fontIndex(1);

	for (int i = 0; i < GetMaxStyleKey(); i++)
	{
		StyleDetails* pStyle = GetStyle(i);
		if (pStyle)
		{
			if (m_fontMap.find(pStyle->FontName) == m_fontMap.end())
			{
				m_out->printf(RTF_FONTDEF, fontIndex, characterset, pStyle->FontName.c_str());
				m_fontMap[pStyle->FontName] = fontIndex++;
			}
		}
	}

	// Written the font table, now start to write out the colour table.
	m_out->puts(RTF_FONTDEFCLOSE RTF_COLORDEFOPEN);

	int colorIndex(1);

	// First comes first, we parse the styles in this scheme and define
	// the rtf style lookup tables for them.
	for (int istyle = 0; istyle <= STYLE_DEFAULT; istyle++)
	{
		StyleDetails* pStyle = GetStyle(istyle);
		
		// If there is no style at number 0, then get the default style.
		if(istyle == 0 && !pStyle)
		{
			pStyle = pDefStyle;
		}

		if (pStyle)
		{
			// Background colour:
			if (m_colorMap.find(pStyle->BackColor) == m_colorMap.end())
			{
				m_colorMap[pStyle->BackColor] = colorIndex++;
				m_out->printf(RTF_COLORDEF, GetRValue(pStyle->BackColor), GetGValue(pStyle->BackColor), GetBValue(pStyle->BackColor));
			}
			if (m_colorMap.find(pStyle->ForeColor) == m_colorMap.end())
			{
				m_colorMap[pStyle->ForeColor] = colorIndex++;
				m_out->printf(RTF_COLORDEF, GetRValue(pStyle->ForeColor), GetGValue(pStyle->ForeColor), GetBValue(pStyle->ForeColor));
			}
		}
	}

	// Written the colour table, now we begin the document.
	m_out->printf(RTF_COLORDEFCLOSE RTF_HEADERCLOSE RTF_BODYOPEN RTF_SETFONTFACE "0"
			RTF_SETFONTSIZE "%d" RTF_SETCOLOR "0 ", pDefStyle->FontSize << 1);

	bool prevCR = false;
	int styleCurrent = -1;
	char ch;
	int style;
	std::string deltaStyle;
	for (int i = start; i < end; i++)
	{
		ch = CharAt(i);
		style = StyleAt(i);
		if (style > STYLE_DEFAULT)
			style = 0;
		if (style != styleCurrent)
		{
			StyleDetails* current = styleCurrent == -1 ? pDefStyle : GetStyle(styleCurrent);
			if (current == NULL)
			{
				current = pDefStyle;
			}

			StyleDetails* newStyle = GetStyle(style);
			if (newStyle == NULL)
			{
				newStyle = pDefStyle;
			}

			std::string deltaStyle = GetRTFStyleChange(current, newStyle);
			if (deltaStyle.size())
			{
				m_out->puts(deltaStyle.c_str());
			}

			styleCurrent = style;
		}
		
		// Deal with special characters:
		if (ch == '{')
			m_out->puts("\\{");
		else if (ch == '}')
			m_out->puts("\\}");
		else if (ch == '\\')
			m_out->puts("\\\\");
		else if (ch == '\t')
		{
			if (tabs)
				m_out->puts(RTF_TAB);
			else
				for (int itab = 0; itab < tabSize; itab++)
					m_out->putc(' ');
		}
		else if (ch == '\n')
		{
			if (!prevCR)
				m_out->puts(RTF_EOLN);
		}
		else if (ch == '\r')
			m_out->puts(RTF_EOLN);
		else
			m_out->putc(ch);

		prevCR = ch == '\r';
	}
	m_out->puts(RTF_BODYCLOSE);
}

LPCTSTR RTFExporter::GetDefaultExtension()
{
	return _T("rtf");
}

LPCTSTR RTFExporter::GetFileMask()
{
	return _T("Rich Text Files (*.rtf, *.doc)|*.rtf;*.doc|");
}