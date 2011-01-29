/**
 * @file exporters.cpp
 * @brief Define style and style-containing classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

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

BaseExporter::BaseExporter(IOutput* pOutput, LPCSTR lpszSchemeName, StylesList* pStyles, CScintilla* pScintilla)
{
	m_out = pOutput;
	m_pStyles = pStyles;
	m_pScintilla = pScintilla;
	m_pSchemeName = lpszSchemeName;
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

int BaseExporter::GetMaxStyleKey()
{
	int maxKey(0);
	for (auto i = m_pStyles->StylesBegin(); i != m_pStyles->StylesEnd(); ++i)
	{
		maxKey = max(maxKey, (*i)->Key);
	}

	return maxKey;
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
			
BaseExporter* ExporterFactory::GetExporter(EExporterType type, IOutput* pOutput, LPCSTR lpszSchemeName, StylesList* pStyles, CScintilla* pScintilla)
{
	switch(type)
	{
		case RTF:
			return new RTFExporter(pOutput, lpszSchemeName, pStyles, pScintilla);
		case HTML:
			return new HTMLExporter(pOutput, lpszSchemeName, pStyles, pScintilla);
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