#include "stdafx.h"
#include "../exporters.h"

////////////////////////////////////////////////////////////////////////////////
// HTMLExporter

#define HTML_HTML401STRICT "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"
#define HTML_XHTML10STRICT "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"
#define HTML_XHTML11STRICT "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"

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
#define HTML_CSSFONTSHORT	"\tfont: "
#define HTML_CSSFONT		"\tfont-family: \""
#define HTML_CSSENDFONT		"\";\n"
#define HTML_CSSFONTSIZE	"\tfont-size: "
#define HTML_CSSFONTBOLD	"\tfont-weight: bold;\n"
#define HTML_CSSFONTITALIC	"\tfont-style: italic;\n"
#define HTML_CSSUNDERLINE   "\ttext-decoration: underline;\n"

#define HTML_CSSPROPERTYEND ";\n"
#define HTML_CSSSTYLECLOSE  "'"
#define HTML_CSSCLASSCLOSE  "}\n"
#define HTML_CSSCLOSE    "\t</style>\n"
#define HTML_HEADCLOSE   "</head>\n"
#define HTML_BODYOPEN    "<body>\n"
#define HTML_PREOPEN     "<pre class='%scode'>"
#define HTML_PRECLOSE    "</pre>\n"
#define HTML_BODYCLOSE   "</body>\n"
#define HTML_END         "</html>\n"

#define HTML_EOLN          "\n"
#define HTML_TAB           ""

#define HTML_FONTFACE      "Courier New"
#define HTML_COLOR         "#000"

HTMLExporter::HTMLExporter(IOutput* pOutput, LPCSTR lpszSchemeName, StylesList* pStyles, CScintilla* pScintilla)
	: BaseExporter(pOutput, lpszSchemeName, pStyles, pScintilla)
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
	CString strFontFace(_T("Courier New"));
	int  fontSize = 10;
	bool useTabs = false;

	int tabSize = SendEditor(SCI_GETTABWIDTH, 0, 0);
	if (tabSize == 0)
		tabSize = 4;
	if (!strFontFace.GetLength())
		strFontFace = HTML_FONTFACE;

	// Define a class name lookup table.
	CStringA cssClassNames[STYLE_MAX + 1];
	
	// Write the HTML header...
	//@todo: add a way to select the DOCTYPE
	m_out->puts(HTML_XHTML11STRICT);
	m_out->puts(HTML_STARTXHTML);
	m_out->puts(HTML_HEADOPEN);
	m_out->puts(HTML_TITLEOPEN);
	//@todo: add title
	m_out->puts(HTML_TITLECLOSE);
	m_out->puts(HTML_CSSOPEN);

	CStringA strTemp;

	// Dump the styles into CSS classes
	//@todo: add support for CSS files

	StyleDetails* pDefStyle = GetStyle(STYLE_DEFAULT);
	if(pDefStyle)
	{
		// Apply Default style to the pre block (class name: pn2code)
		strTemp.Format("pre.%scode\n", m_pSchemeName);
		cssClassNames[STYLE_DEFAULT] = strTemp;
		m_out->puts(cssClassNames[STYLE_DEFAULT]);

		m_out->puts(HTML_CSSCLASSOPEN HTML_CSSBKGNDCOLOR);
		strTemp.Format("#%02X%02X%02X",
			GetRValue(pDefStyle->BackColor),
			GetGValue(pDefStyle->BackColor),
			GetBValue(pDefStyle->BackColor));
		m_out->puts(strTemp);
		m_out->puts(HTML_CSSPROPERTYEND);

		m_out->puts(HTML_CSSFORECOLOR);
		strTemp.Format("#%02X%02X%02X;",
			GetRValue(pDefStyle->ForeColor),
			GetGValue(pDefStyle->ForeColor),
			GetBValue(pDefStyle->ForeColor));
		m_out->puts(strTemp);
		m_out->puts(HTML_CSSPROPERTYEND);

		// SF Bug #107412 points to http://www.w3.org/TR/REC-CSS2/fonts.html#font-shorthand
		// defining the (simplified) use of font shorthand as: 
		//    style weight size font-family
		
		m_out->puts(HTML_CSSFONTSHORT);
		
		if(pDefStyle->Italic)
			m_out->puts("italic ");

		if(pDefStyle->Bold)
			m_out->puts("bold ");

		strTemp.Format("%dpt ", pDefStyle->FontSize ? pDefStyle->FontSize : fontSize);
		m_out->puts(strTemp);

		m_out->putc('"');
		if(pDefStyle->FontName.length())
        {
            strTemp = pDefStyle->FontName.c_str();
			m_out->puts(strTemp);
        }
		else
			m_out->puts(HTML_FONTFACE);
		m_out->putc('"');

		m_out->puts(HTML_CSSPROPERTYEND);

		if(pDefStyle->Underline)
			m_out->puts(HTML_CSSUNDERLINE);

		m_out->puts(HTML_CSSCLASSCLOSE);
	}

	for(int istyle = 0; istyle < STYLE_MAX; istyle++)
	{
		StyleDetails* pStyle = GetStyle(istyle);
		if(pStyle)
		{
			// Use the styles classname as CSS class name (if any), or create a
			// generic one
			if(pStyle->classname.length())
				strTemp = pStyle->classname.c_str();
			else
				strTemp.Format("%s%02d", m_pSchemeName, istyle);
			cssClassNames[istyle] = strTemp;
			m_out->putc('.');
			m_out->puts(cssClassNames[istyle]);
			m_out->puts(HTML_CSSCLASSOPEN);

			// Add properties if they differ from the default style only

			if(!pDefStyle || pStyle->BackColor != pDefStyle->BackColor)
			{
				m_out->puts(HTML_CSSBKGNDCOLOR);
				strTemp.Format("#%02X%02X%02X",
					GetRValue(pStyle->BackColor),
					GetGValue(pStyle->BackColor),
					GetBValue(pStyle->BackColor));
				m_out->puts(strTemp);
				m_out->puts(HTML_CSSPROPERTYEND);
			}

			if(!pDefStyle || pStyle->ForeColor != pDefStyle->ForeColor)
			{
				m_out->puts(HTML_CSSFORECOLOR);
				strTemp.Format("#%02X%02X%02X",
					GetRValue(pStyle->ForeColor),
					GetGValue(pStyle->ForeColor),
					GetBValue(pStyle->ForeColor));
				m_out->puts(strTemp);
				m_out->puts(HTML_CSSPROPERTYEND);
			}

			//CString strFont;
			if(pStyle->FontName.length() &&
				(!pDefStyle || pStyle->FontName != pDefStyle->FontName))
			{
				m_out->puts(HTML_CSSFONT);
                strTemp = pStyle->FontName.c_str();
				m_out->puts(strTemp);
				m_out->puts(HTML_CSSENDFONT);
			}

			if(pStyle->FontSize > 0 &&
				(!pDefStyle || pDefStyle->FontSize != pStyle->FontSize))
			{
				m_out->puts(HTML_CSSFONTSIZE);
				strTemp.Format("%dpt", pStyle->FontSize);
				m_out->puts(strTemp);
				m_out->puts(HTML_CSSPROPERTYEND);
			}

			if(pStyle->Bold &&
				(!pDefStyle || pDefStyle->Bold != pStyle->Bold))
			{
				m_out->puts(HTML_CSSFONTBOLD);
			}

			if(pStyle->Italic &&
				(!pDefStyle || pDefStyle->Italic != pStyle->Italic))
			{
				m_out->puts(HTML_CSSFONTITALIC);
			}

			if(pStyle->Underline &&
				(!pDefStyle || pDefStyle->Underline != pStyle->Underline))
				m_out->puts(HTML_CSSUNDERLINE);

			m_out->puts(HTML_CSSCLASSCLOSE);
		}
	}

	m_out->puts(HTML_CSSCLOSE);
	m_out->puts(HTML_HEADCLOSE);
	m_out->puts(HTML_BODYOPEN);

	strTemp.Format(HTML_PREOPEN, m_pSchemeName);
	m_out->puts((LPCSTR)strTemp);

	int styleCurrent = -1;
	bool closePrevious = false;
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
				strTemp.Format("<span class='%s'>", cssClassNames[style]);
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
		else if(ch == '"')
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

	if(closePrevious == true)
		m_out->puts("</span>");

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