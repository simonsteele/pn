#include "stdafx.h"
#include "exporters.h"
#include "styles.h"

////////////////////////////////////////////////////////////////////////////////
// PrintfConduit class

void PrintfConduit::printf(const char* format, ...)
{
	va_list args;
	va_start(args, format);

	str.FormatV(format, args);
	puts(str);

	va_end(args);
}

////////////////////////////////////////////////////////////////////////////////
// BaseExporter class

BaseExporter::BaseExporter(IOutput* pOutput, StylesList* pStyles, CScintilla* pScintilla)
{
	m_out = pOutput;
	m_pStyles = pStyles;
	m_pScintilla = pScintilla;
}

/**
 * @return character at position.
 */
char BaseExporter::CharAt(int position)
{
	return (char)( m_pScintilla->SPerform(SCI_GETCHARAT, position) & 0xFF );
}

/**
 * @return style at position.
 */
int BaseExporter::StyleAt(int position)
{
	return m_pScintilla->SPerform(SCI_GETSTYLEAT, position);
}

/**
 * Find a StyleDetails given its style number (key)
 */
StyleDetails* BaseExporter::GetStyle(int key)
{
	return m_pStyles->GetStyle(key);
}

int BaseExporter::SendEditor(long Msg, WPARAM wParam, LPARAM lParam)
{
	return m_pScintilla->SPerform(Msg, wParam, lParam);
}

void BaseExporter::Export(int start, int finish)
{
	SendEditor(SCI_COLOURISE, 0, -1);
	
	if (finish < 0)
		finish = SendEditor(SCI_GETLENGTH, 0, 0);;

	InternalExport(start, finish);
}

LPCTSTR BaseExporter::GetDefaultExtension()
{
	return _T("txt");
}

LPCTSTR BaseExporter::GetFileMask()
{
	return _T("Text Files (*.txt)|*.txt|");
}

////////////////////////////////////////////////////////////////////////////////
// ExporterFactory class
			
BaseExporter* ExporterFactory::GetExporter(EExporterType type, IOutput* pOutput, StylesList* pStyles, CScintilla* pScintilla)
{
	switch(type)
	{
		case RTF:
			return new RTFExporter(pOutput, pStyles, pScintilla);
		case HTML:
			return new HTMLExporter(pOutput, pStyles, pScintilla);
	}
	
	return NULL;
}

////////////////////////////////////////////////////////////////////////////////
// StringOutput class

/**
 * @param baseSize = the basic size of the buffer to use for text output.
 */
StringOutput::StringOutput(unsigned int baseSize)
{
	// This allocates an initial buffer.
	m_buffer.grow(baseSize);
	// This re-sets the size pointer to 0, doesn't de-allocate.
	m_buffer.grow(0);
}

void StringOutput::puts(const char* str)
{
	int len = (int)strlen(str);
	int startPos = m_buffer.size();
	m_buffer.grow(startPos + len);
	memcpy(&m_buffer[startPos], str, strlen(str));
}

void StringOutput::putc(const char ch)
{
	int startPos = m_buffer.size();
	m_buffer.grow(startPos + 1);
	m_buffer[startPos] = ch;
}

const char* StringOutput::c_str()
{
	int size = m_buffer.size();
	m_buffer.grow(size+1);
	m_buffer[size] = '\0';
	return &m_buffer[0];
}

////////////////////////////////////////////////////////////////////////////////
// FileOutput class

/**
 * @param fileName to write output to.
 */
FileOutput::FileOutput(LPCTSTR fileName)
{
	if(fileName)
	{
		m_bValid = m_file.Open(fileName, CFile::modeWrite | CFile::modeBinary);
	}
	else m_bValid = false;
}

FileOutput::~FileOutput()
{
	if(m_bValid)
	{
		m_file.Close();
	}
}

void FileOutput::SetFileName(LPCTSTR fileName)
{
	if(!m_bValid)
	{
		m_bValid = m_file.Open(fileName, CFile::modeWrite | CFile::modeBinary);
	}
}

bool FileOutput::IsValid()
{
	return m_bValid;
}

void FileOutput::puts(const char* str)
{
	if(m_bValid)
	{
		m_file.Write((void*)str, (UINT)strlen(str));
	}
}

void FileOutput::putc(const char ch)
{
	if(m_bValid)
	{
		m_file.Write((void*)&ch, 1);
	}
}

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

RTFExporter::RTFExporter(IOutput* pOutput, StylesList* pStyles, CScintilla* pScintilla)
: BaseExporter(pOutput, pStyles, pScintilla)
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
	CString strFontFace   = /*pOptions->GetRtfFontName()*/	"Courier New";
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
	
	CString strFontFamily;
	CString strForeground;
	CString strBackground;
	
	StyleDetails* pDefStyle = GetStyle(STYLE_DEFAULT);

	// First comes first, we parse the styles in this scheme and define
	// the rtf style lookup tables for them.
	for (int istyle = 0; istyle <= STYLE_DEFAULT; istyle++)
	{
		TCHAR szColorTmp[32] = {0};

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


////////////////////////////////////////////////////////////////////////////////
// HTMLExporter

#define HTML_HTML401STRICT _T("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">")
#define HTML_XHTML10STRICT _T("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
#define HTML_XHTML11STRICT _T("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")

#define HTML_START       "<html>\n"
#define HTML_STARTXHTML  "<html xmlns='http://www.w3.org/1999/xhtml'>\n"
#define HTML_HEADOPEN    "<head>\n\t<meta http-equiv='Content-Type' content='text/html; charset=iso-8859-1' />\n"
#define HTML_TITLEOPEN   "\t<title>"
#define HTML_TITLECLOSE  "</title>\n"
#define HTML_CSSOPEN     "\t<style type='text/css' media='all'>\n"
#define HTML_CSSCLASSOPEN   "{\n"
#define HTML_CSSSTYLEOPEN   " style='"
#define HTML_CSSFORECOLOR   "\tcolor: "
#define HTML_CSSBKGNDCOLOR  "\tbackground: "
// name, style, weight, size
#define HTML_CSSFONT        "\tfont: "
#define HTML_CSSFONTITALIC  "italic"
#define HTML_CSSFONTBOLD    "bold"
#define HTML_CSSUNDERLINE   "\ttext-decoration: underline;\n"
#define HTML_CSSPROPERTYEND ";\n"
#define HTML_CSSSTYLECLOSE  "'"
#define HTML_CSSCLASSCLOSE  "}\n"
#define HTML_CSSCLOSE    "\t</style>\n"
#define HTML_HEADCLOSE   "</head>\n"
#define HTML_BODYOPEN    "<body>\n"
#define HTML_PREOPEN     "<pre class='pn2code'>"
#define HTML_PRECLOSE    "</pre>\n"
#define HTML_BODYCLOSE   "</body>\n"
#define HTML_END         "</html>\n"

#define HTML_EOLN          "\n"
#define HTML_TAB           ""

#define HTML_FONTFACE      "Courier New"
#define HTML_COLOR         "#000"

HTMLExporter::HTMLExporter(IOutput* pOutput, StylesList* pStyles, CScintilla* pScintilla)
	: BaseExporter(pOutput, pStyles, pScintilla)
{
}

LPCTSTR HTMLExporter::GetDefaultExtension()
{
	return _T("html");
}
LPCTSTR HTMLExporter::GetFileMask()
{
	return _T("HTML and CSS Files (*.html, *.css)|*.html;*.css|");
}

void HTMLExporter::InternalExport(int start, int end)
{
	bool writeXHTML = true;
	CString strFontFace(_T("Courier New"));
	int  fontSize = 10;
	// IE use quirks mode with <?xml?>
	bool addXmlProlog = false;
	bool useTabs = false;

	int tabSize = SendEditor(SCI_GETTABWIDTH, 0, 0);
	if (tabSize == 0)
		tabSize = 4;
	if (!strFontFace.GetLength())
		strFontFace = HTML_FONTFACE;

	// Define a class name lookup table.
	CString cssClassNames[STYLE_DEFAULT + 1];
	
	// Write the HTML header...
	//@todo: add a way to select the DOCTYPE
	m_out->puts(HTML_XHTML11STRICT);
	m_out->puts(HTML_STARTXHTML);
	m_out->puts(HTML_HEADOPEN);
	m_out->puts(HTML_TITLEOPEN);
	//@todo: add title
	m_out->puts(HTML_TITLECLOSE);
	m_out->puts(HTML_CSSOPEN);

	CString strTemp;

	// Dump the styles into CSS classes
	//@todo: add support for CSS files

	StyleDetails* pDefStyle = GetStyle(STYLE_DEFAULT);
	if(pDefStyle)
	{
		// Apply Default style to the pre block (class name: pn2code)
		cssClassNames[STYLE_DEFAULT] = _T("pre.pn2code\n");
		m_out->puts(cssClassNames[STYLE_DEFAULT]);

		m_out->puts(HTML_CSSCLASSOPEN HTML_CSSBKGNDCOLOR);
		strTemp.Format(_T("#%02X%02X%02X"),
			GetRValue(pDefStyle->BackColor),
			GetGValue(pDefStyle->BackColor),
			GetBValue(pDefStyle->BackColor));
		m_out->puts(strTemp);
		m_out->puts(HTML_CSSPROPERTYEND);

		m_out->puts(HTML_CSSFORECOLOR);
		strTemp.Format(_T("#%02X%02X%02X;"),
			GetRValue(pDefStyle->ForeColor),
			GetGValue(pDefStyle->ForeColor),
			GetBValue(pDefStyle->ForeColor));
		m_out->puts(strTemp);
		m_out->puts(HTML_CSSPROPERTYEND);

		m_out->puts(HTML_CSSFONT);
		if(pDefStyle->FontName.length())
		{
			m_out->puts(pDefStyle->FontName.c_str());
			m_out->puts(_T(","));
		}

		strTemp.Format(_T("%dpt"), pDefStyle->FontSize ? pDefStyle->FontSize : fontSize);
		m_out->puts(strTemp);

		if(pDefStyle->Bold)
			m_out->puts(_T(",") HTML_CSSFONTBOLD);
		if(pDefStyle->Italic)
			m_out->puts(_T(",") HTML_CSSFONTITALIC);
		m_out->puts(HTML_CSSPROPERTYEND);

		if(pDefStyle->Underline)
			m_out->puts(HTML_CSSUNDERLINE);

		m_out->puts(HTML_CSSCLASSCLOSE);
	}

	for(int istyle = 0; istyle < STYLE_DEFAULT; istyle++)
	{
		StyleDetails* pStyle = GetStyle(istyle);
		if(pStyle)
		{
			// Use the styles classname as CSS class name (if any), or create a
			// generic one
			if(pStyle->classname.length())
				strTemp = pStyle->classname.c_str();
			else
				strTemp.Format(_T("pn2%02d"), istyle);
			cssClassNames[istyle] = strTemp;
			m_out->putc('.');
			m_out->puts(cssClassNames[istyle]);
			m_out->puts(HTML_CSSCLASSOPEN);

			// Add properties if they differ from the default style only

			if(!pDefStyle || pStyle->BackColor != pDefStyle->BackColor)
			{
				m_out->puts(HTML_CSSBKGNDCOLOR);
				strTemp.Format(_T("#%02X%02X%02X"),
					GetRValue(pStyle->BackColor),
					GetGValue(pStyle->BackColor),
					GetBValue(pStyle->BackColor));
				m_out->puts(strTemp);
				m_out->puts(HTML_CSSPROPERTYEND);
			}

			if(!pDefStyle || pStyle->ForeColor != pDefStyle->ForeColor)
			{
				m_out->puts(HTML_CSSFORECOLOR);
				strTemp.Format(_T("#%02X%02X%02X"),
					GetRValue(pStyle->ForeColor),
					GetGValue(pStyle->ForeColor),
					GetBValue(pStyle->ForeColor));
				m_out->puts(strTemp);
				m_out->puts(HTML_CSSPROPERTYEND);
			}

			CString strFont;
			if(pStyle->FontName.length() &&
				(!pDefStyle || pStyle->FontName != pDefStyle->FontName))
			{
				strFont = pStyle->FontName.c_str();
			}

			if(pStyle->FontSize > 0 &&
				(!pDefStyle || pDefStyle->FontSize != pStyle->FontSize))
			{
				if(strFont.GetLength())
					strFont += _T(", ");
				strTemp.Format(_T("%dpt"), pStyle->FontSize);
				strFont += strTemp;
			}

			if(pStyle->Bold &&
				(!pDefStyle || pDefStyle->Bold != pStyle->Bold))
			{
				if(strFont.GetLength())
					strFont += _T(", ");
				strFont += HTML_CSSFONTBOLD;
			}
			if(pStyle->Italic &&
				(!pDefStyle || pDefStyle->Italic != pStyle->Italic))
			{
				if(strFont.GetLength())
					strFont += _T(", ");
				strFont += HTML_CSSFONTITALIC;
			}

			if(strFont.GetLength())
			{
				m_out->puts(HTML_CSSFONT);
				m_out->puts(strFont);
				m_out->puts(HTML_CSSPROPERTYEND);
			}

			if(pStyle->Underline &&
				(!pDefStyle || pDefStyle->Underline != pStyle->Underline))
				m_out->puts(HTML_CSSUNDERLINE);

			m_out->puts(HTML_CSSCLASSCLOSE);
		}
	}

	m_out->puts(HTML_CSSCLOSE);
	m_out->puts(HTML_HEADCLOSE);
	m_out->puts(HTML_BODYOPEN HTML_PREOPEN);

	int styleCurrent = -1;
	bool closePrevious = false;
	StyleDetails* pCurrent = NULL;
	for(int i = start; i < end; i++)
	{
		char ch = CharAt(i);
		int style = StyleAt(i);
		if(style > STYLE_DEFAULT)
			style = 0;
		if(style != styleCurrent && style != STYLE_DEFAULT)
		{
			//@todo: check for empty/white space tags
			if(closePrevious == true)
				m_out->puts("</span>");

			if(cssClassNames[style].GetLength() > 0)
			{
				strTemp.Format(_T("<span class='%s'>"), cssClassNames[style]);
				m_out->puts(strTemp);
				closePrevious = true;
/*				// inline styles; not recommend
				m_out->puts("<span");
				StyleDetails* pNext = GetStyle(style);
				if(pNext == NULL)
					pNext = pDefStyle;

				GetStyleChange(pCurrent, pNext, NULL);
				pCurrent = pNext;
				m_out->puts(">");*/
			}
			else
				closePrevious = false;

			styleCurrent = style;
		}

		// Deal with special characters:
		if(ch == '&')
			m_out->puts("&amp;");
		if(ch == '"')
			m_out->puts("&quot;");
		else if(ch == '<')
			m_out->puts("&lt;");
		else if(ch == '>')
			m_out->puts("&gt;");
		else if(ch == '\t')
		{
			if(useTabs)
				m_out->putc(ch);
			else
				for(int itab = 0; itab < tabSize; itab++)
					m_out->putc(' ');
		}
		else if(ch == '\r')
			;
		else if(ch == '\n')
			m_out->putc(ch);
		else
			m_out->putc(ch);
	}

	m_out->puts(HTML_PRECLOSE HTML_BODYCLOSE HTML_END);
}

#if 0
void HTMLExporter::GetStyleChange(StyleDetails* pCurrent, StyleDetails* pNext, StyleDetails* pDelta)
{
	CString strTemp;
	CString strStyle;

	//@todo: create a lookup table
//	ATLTRACE(_T("bgknd: %06X != %06X\n"), pCurrent ? pCurrent->BackColor : 0, pNext->BackColor);
	if(!pCurrent || pNext->BackColor != pCurrent->BackColor)
	{
		strStyle = HTML_CSSBKGNDCOLOR;
		strTemp.Format(_T("#%02X%02X%02X"),
			GetRValue(pNext->BackColor),
			GetGValue(pNext->BackColor),
			GetBValue(pNext->BackColor));
		strStyle += strTemp;
		strStyle += HTML_CSSPROPERTYEND;
	}
	else if(pCurrent)
	{
		strStyle = HTML_CSSBKGNDCOLOR;
		strTemp.Format(_T("#%02X%02X%02X"),
			GetRValue(pCurrent->BackColor),
			GetGValue(pCurrent->BackColor),
			GetBValue(pCurrent->BackColor));
		strStyle += strTemp;
		strStyle += HTML_CSSPROPERTYEND;
	}

//	ATLTRACE(_T("fore: %06X != %06X\n"), pCurrent ? pCurrent->ForeColor : 0, pNext->ForeColor);
	//if(!pCurrent || pNext->ForeColor != pCurrent->ForeColor)
	{
		strStyle += HTML_CSSFORECOLOR;
		strTemp.Format(_T("#%02X%02X%02X"),
			GetRValue(pNext->ForeColor),
			GetGValue(pNext->ForeColor),
			GetBValue(pNext->ForeColor));
		strStyle += strTemp;
		strStyle += HTML_CSSPROPERTYEND;
	}

	CString strFont;
	if(pNext->FontName.length() &&
		(!pCurrent || pNext->FontName != pCurrent->FontName))
	{
		strFont = pNext->FontName.c_str();
	}

	if(pNext->FontSize > 0 &&
		(!pCurrent || pNext->FontSize != pCurrent->FontSize))
	{
		if(strFont.GetLength())
			strFont += _T(", ");
		strTemp.Format(_T("%dpt"), pNext->FontSize);
		strFont += strTemp;
	}

	if(pNext->Bold)
	{
		if(strFont.GetLength())
			strFont += _T(", ");
		strFont += HTML_CSSFONTBOLD;
	}
	if(pNext->Italic)
	{
		if(strFont.GetLength())
			strFont += _T(", ");
		strFont += HTML_CSSFONTITALIC;
	}

	if(strFont.GetLength())
	{
		strStyle += HTML_CSSFONT;
		strStyle += strFont;
		strStyle += HTML_CSSPROPERTYEND;
	}

	if(pNext->Underline)
		strStyle += HTML_CSSUNDERLINE;

	if(strStyle.GetLength())
	{
		m_out->puts(HTML_CSSSTYLEOPEN);
		m_out->puts(strStyle);
		m_out->puts(HTML_CSSSTYLECLOSE);
	}
}
#endif

////////////////////////////////////////////////////////////////////////////////
// TestCode

#ifdef TESTCODE

void testExporterClasses()
{
	StringOutput out(10);
	out.puts("This is a ");
	out.puts("test of the ");
	out.puts("StringOutput class. If it works correctly, you should see a big");
	out.puts(" long string.");
	::MessageBox(NULL, out.c_str(), "testExporterClasses()", MB_OK);
}

#endif