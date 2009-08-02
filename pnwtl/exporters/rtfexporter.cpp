#include "stdafx.h"
#include "../exporters.h"

////////////////////////////////////////////////////////////////////////////////
// RTFExporter class

#define RTF_HEADEROPEN    "{\\rtf1\\ansi\\deff0\\deftab720"
#define RTF_FONTDEFOPEN   "{\\fonttbl"
#define RTF_FONTDEF       "{\\f%d\\fnil\\fcharset%u %s;}"
#define RTF_FONTDEFCLOSE  "}"
#define RTF_COLORDEFOPEN  "{\\colortbl"
#define RTF_COLORDEF      "\\red%d\\green%d\\blue%d;"
#define RTF_COLORDEFCLOSE "}"
#define RTF_HEADERCLOSE   "\n"
#define RTF_BODYOPEN      ""
#define RTF_BODYCLOSE     "}"

#define RTF_SETFONTFACE   "\\f"
#define RTF_SETFONTSIZE   "\\fs"
#define RTF_SETCOLOR      "\\cf"
#define RTF_SETBACKGROUND "\\highlight"
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

/**
 * This function steps through the incoming and outgoing style definitions (RTF style)
 * and decides which bits of the style have changed - so most style changes will end
 * up as just a simple colour change.
 */
void RTFExporter::GetRTFStyleChange(char *delta, char *last, const char *current) 
{ // \f0\fs20\cf0\highlight0\b0\i0
	int lastLen = strlen(last), offset = 2, lastOffset, currentOffset, len;
	*delta = '\0';
	// font face
	lastOffset = offset + 1;
	while (last[lastOffset] != '\\')
		lastOffset++;
	currentOffset = offset + 1;
	while (current[currentOffset] != '\\')
		currentOffset++;
	if (lastOffset != currentOffset ||         // change
			strncmp(last + offset, current + offset, lastOffset - offset))
	{
		if (lastOffset != currentOffset)
		{
			memmove(last + currentOffset, last + lastOffset, lastLen - lastOffset + 1);
			lastLen += currentOffset - lastOffset;
		}
		len = currentOffset - offset;
		memcpy(last + offset, current + offset, len);
		strcat(delta, RTF_SETFONTFACE);
		lastOffset = strlen(delta);
		memcpy(delta + lastOffset, last + offset, len);
		delta[lastOffset + len] = '\0';
	}
	offset = currentOffset + 3;
	// size
	lastOffset = offset + 1;
	while (last[lastOffset] != '\\')
		lastOffset++;
	currentOffset = offset + 1;
	while (current[currentOffset] != '\\')
		currentOffset++;
	if (lastOffset != currentOffset ||         // change
			strncmp(last + offset, current + offset, lastOffset - offset))
	{
		if (lastOffset != currentOffset)
		{
			memmove(last + currentOffset, last + lastOffset, lastLen - lastOffset + 1);
			lastLen += currentOffset - lastOffset;
		}
		len = currentOffset - offset;
		memcpy(last + offset, current + offset, len);
		strcat(delta, RTF_SETFONTSIZE);
		lastOffset = strlen(delta);
		memcpy(delta + lastOffset, last + offset, len);
		delta[lastOffset + len] = '\0';
	}
	offset = currentOffset + 3;
	// color
	lastOffset = offset + 1;
	while (last[lastOffset] != '\\')
		lastOffset++;
	currentOffset = offset + 1;
	while (current[currentOffset] != '\\')
		currentOffset++;
	if (lastOffset != currentOffset ||         // change
			strncmp(last + offset, current + offset, lastOffset - offset))
	{
		if (lastOffset != currentOffset)
		{
			memmove(last + currentOffset, last + lastOffset, lastLen - lastOffset + 1);
			lastLen += currentOffset - lastOffset;
		}
		len = currentOffset - offset;
		memcpy(last + offset, current + offset, len);
		strcat(delta, RTF_SETCOLOR);
		lastOffset = strlen(delta);
		memcpy(delta + lastOffset, last + offset, len);
		delta[lastOffset + len] = '\0';
	}
	offset = currentOffset + 10;
	// background
	lastOffset = offset + 1;
	while (last[lastOffset] != '\\')
		lastOffset++;
	currentOffset = offset + 1;
	while (current[currentOffset] != '\\')
		currentOffset++;
	if (lastOffset != currentOffset ||         // change
			strncmp(last + offset, current + offset, lastOffset - offset))
	{
		if (lastOffset != currentOffset)
		{
			memmove(last + currentOffset, last + lastOffset, lastLen - lastOffset + 1);
			lastLen += currentOffset - lastOffset;
		}
		len = currentOffset - offset;
		memcpy(last + offset, current + offset, len);
		strcat(delta, RTF_SETBACKGROUND);
		lastOffset = strlen(delta);
		memcpy(delta + lastOffset, last + offset, len);
		delta[lastOffset + len] = '\0';
	}
	offset = currentOffset + 2;
	// bold
	if (last[offset] != current[offset])
	{
		if (current[offset] == '\\')
		{
			// turn on
			memmove(last + offset, last + offset + 1, lastLen-- - offset);
			strcat(delta, RTF_BOLD_ON);
			offset += 2;
		}
		else 
		{ // turn off
			memmove(last + offset + 1, last + offset, ++lastLen - offset);
			last[offset] = '0';
			strcat(delta, RTF_BOLD_OFF);
			offset += 3;
		}
	}
	else
		offset += current[offset] == '\\' ? 2 : 3;
	// italic
	if (last[offset] != current[offset])
	{
		if (current[offset] == '\\')
		{
			// turn on
			memmove(last + offset, last + offset + 1, lastLen-- - offset);
			strcat(delta, RTF_ITALIC_ON);
		}
		else 
		{ // turn off
			memmove(last + offset + 1, last + offset, ++lastLen - offset);
			last[offset] = '0';
			strcat(delta, RTF_ITALIC_OFF);
		}
	}
	if (*delta)
	{
		lastOffset = strlen(delta);
		delta[lastOffset] = ' ';
		delta[lastOffset + 1] = '\0';
	}
}

/**
 * Ported (ish) from SciTE, this version removes file access and instead 
 * uses the IOutput class to store the result of its work. I've also commented
 * the code a bit as I work out how it works.
 */
void RTFExporter::InternalExport(int start, int end)
{
	int wysiwyg           = /*pOptions->GetRtfWYSIWYG()*/	1;
	CStringA strFontFace   = /*pOptions->GetRtfFontName()*/	"Courier New";
	unsigned characterset = /*pOptions->GetRtfCharset()*/	0;
	int fontSize          = /*pOptions->GetRtfFontSize()*/	10 << 1;
	int tabs              = /*pOptions->GetRtfTabs()*/		1;

	int tabSize = SendEditor(SCI_GETTABWIDTH, 0, 0);
	if (tabSize == 0)
		tabSize = 4;
	if (!strFontFace.GetLength())
		strFontFace = RTF_FONTFACE;
	
	// Define up the style lookup tables.
	char styles[STYLE_DEFAULT + 1][MAX_STYLEDEF];
	char fonts[STYLE_DEFAULT + 1][MAX_FONTDEF];
	char colors[STYLE_DEFAULT + 1][MAX_COLORDEF];
	char lastStyle[MAX_STYLEDEF], deltaStyle[MAX_STYLEDEF];
	
	int fontCount = 1, colorCount = 1, i;
	
	// Write the RTF header...
	m_out->puts(RTF_HEADEROPEN RTF_FONTDEFOPEN);
	strncpy(*fonts, strFontFace, MAX_FONTDEF);
	m_out->printf(RTF_FONTDEF, 0, characterset, strFontFace);
	strncpy(*colors, "#000000", MAX_COLORDEF);
	
	CStringA strFontFamily;
	CStringA strForeground;
	CStringA strBackground;
	
	StyleDetails* pDefStyle = GetStyle(STYLE_DEFAULT);

	// First comes first, we parse the styles in this scheme and define
	// the rtf style lookup tables for them.
	for (int istyle = 0; istyle <= STYLE_DEFAULT; istyle++)
	{
		char szColorTmp[32] = {0};

		StyleDetails* pStyle = GetStyle(istyle);
		
		// If there is no style at number 0, then get the default style.
		if(istyle == 0 && !pStyle)
		{
			pStyle = pDefStyle;
		}

		if (pStyle)
		{
			// Background colour:
			if(pDefStyle && pStyle->BackColor != pDefStyle->BackColor)
			{
				sprintf(szColorTmp, "#%02X%02X%02X", 
					GetRValue(pStyle->BackColor), 
					GetGValue(pStyle->BackColor), 
					GetBValue(pStyle->BackColor));
				
				strBackground = szColorTmp;
			}
			else
				strBackground = "";

			sprintf(szColorTmp, "#%02X%02X%02X", 
				GetRValue(pStyle->ForeColor), 
				GetGValue(pStyle->ForeColor), 
				GetBValue(pStyle->ForeColor));
			
			strForeground = szColorTmp;

			strFontFamily = pStyle->FontName.c_str();

			// Try and match the font of the style to one that has been defined
			// in the RTF font table. Only do this in wysiwyg mode.
			if (wysiwyg && strFontFamily.GetLength())
			{
				for (i = 0; i < fontCount; i++)
				{
					if (strFontFamily.CompareNoCase(fonts[i]) == 0)
						break;
				}

				if (i >= fontCount)
				{
					strncpy(fonts[fontCount++], strFontFamily, MAX_FONTDEF);
					m_out->printf(RTF_FONTDEF, i, characterset, strFontFamily);
				}

				sprintf(lastStyle, RTF_SETFONTFACE "%d", i);
			}
			else
				strcpy(lastStyle, RTF_SETFONTFACE "0");

			// Font size - if wysiwyg then the size from the style else the default.
			sprintf(lastStyle + strlen(lastStyle), RTF_SETFONTSIZE "%d",
				wysiwyg && pStyle->FontSize ? pStyle->FontSize << 1 : fontSize);

			// Now for the foreground colour. Again these are stored in a table with
			// rtf so we see if we've already added it.
			if (strForeground.GetLength())
			{
				for (i = 0; i < colorCount; i++)
				{
					if (strForeground.CompareNoCase(colors[i]) == 0)
						break;
				}

				if (i >= colorCount)
					strncpy(colors[colorCount++], strForeground, MAX_COLORDEF);

				sprintf(lastStyle + strlen(lastStyle), RTF_SETCOLOR "%d", i);
			}
			else
				strcat(lastStyle, RTF_SETCOLOR "0");

			// Find the closest RTF highlight to the background colour.
			sprintf(lastStyle + strlen(lastStyle), RTF_SETBACKGROUND "%d",
					strBackground.GetLength() ? GetRTFHighlight(strBackground) : 0);
			
			// Other styles - why is underline not included?
			strcat(lastStyle, pStyle->Bold ? RTF_BOLD_ON : RTF_BOLD_OFF);
			strcat(lastStyle, pStyle->Italic ? RTF_ITALIC_ON : RTF_ITALIC_OFF);
			
			strncpy(styles[istyle], lastStyle, MAX_STYLEDEF);
		}
		else
		{
			sprintf(styles[istyle], RTF_SETFONTFACE "0" RTF_SETFONTSIZE "%d"
					RTF_SETCOLOR "0" RTF_SETBACKGROUND "0"
					RTF_BOLD_OFF RTF_ITALIC_OFF, fontSize);
		}
	}

	// Written the font table, now write out the colour table that we just parsed.
	m_out->puts(RTF_FONTDEFCLOSE RTF_COLORDEFOPEN);
	for (i = 0; i < colorCount; i++)
	{
		m_out->printf(RTF_COLORDEF, GetHexByte(colors[i] + 1),
				GetHexByte(colors[i] + 3), GetHexByte(colors[i] + 5));
	}

	// Written the colour table, now we begin the document.
	m_out->printf(RTF_COLORDEFCLOSE RTF_HEADERCLOSE RTF_BODYOPEN RTF_SETFONTFACE "0"
			RTF_SETFONTSIZE "%d" RTF_SETCOLOR "0 ", fontSize);
	sprintf(lastStyle, RTF_SETFONTFACE "0" RTF_SETFONTSIZE "%d"
			RTF_SETCOLOR "0" RTF_SETBACKGROUND "0"
			RTF_BOLD_OFF RTF_ITALIC_OFF, fontSize);
	
	bool prevCR = false;
	int styleCurrent = -1;
	char ch;
	int style;
	for (i = start; i < end; i++)
	{
		ch = CharAt(i);
		style = StyleAt(i);
		if (style > STYLE_DEFAULT)
			style = 0;
		if (style != styleCurrent)
		{
			GetRTFStyleChange(deltaStyle, lastStyle, styles[style]);
			if (*deltaStyle)
				m_out->puts(deltaStyle);
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