/**
 * @file exporters.h
 * @brief Define style and style-containing classes.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef exporters_h__included
#define exporters_h__included

#include "include/sscontainers.h"
#include "files.h"
#include "scintillaif.h"

/**
 * Interface class defining an output conduit for exported data.
 */
class IOutput
{
	public:
		virtual void puts(const char* str) = 0;
		virtual void putc(const char ch) = 0;
		virtual void printf(const char* format, ...) = 0;
};

/**
 * Class implementing printf for IOutput conduit classes
 */
class PrintfConduit : public IOutput
{
	public:
		virtual void printf(const char* format, ...);

	protected:
		CString str;
};

/**
 * This is the base class for all data exporters, it defines common
 * functionality.
 */
class BaseExporter
{
	public:
		BaseExporter(IOutput* pOutput, LPCTSTR lpszSchemeName, StylesList* pStyles, CScintilla* pScintilla);
		virtual ~BaseExporter(){}

		void Export(int start, int finish);

		virtual LPCTSTR GetDefaultExtension();
		virtual LPCTSTR GetFileMask();

	protected:
		virtual void InternalExport(int start, int finish) = 0;

		char CharAt(int position);
		int StyleAt(int position);

		StyleDetails* GetStyle(int key);

		int SendEditor(long Msg, WPARAM wParam=0, LPARAM lParam=0);

		CScintilla*	m_pScintilla;
		StylesList*	m_pStyles;
		IOutput*	m_out;

		LPCTSTR		m_pSchemeName;
};

class ExporterFactory
{
	public:
		typedef enum { RTF, HTML } EExporterType;

		static BaseExporter* GetExporter(EExporterType type, 
			IOutput* pOutput, LPCTSTR lpszSchemeName, StylesList* pStyles, CScintilla* pScintilla);

	private:
		ExporterFactory(){}
};

/**
 * This is a data exporter output class, it stores all output
 * in a growing string buffer.
 */
class StringOutput : public PrintfConduit
{
	public:
		StringOutput(unsigned int baseSize = 4096);

		virtual void puts(const char* str);
		virtual void putc(const char ch);

		const char* c_str();

	protected:
		GArray<char>	m_buffer;
};

/**
 * This is a data exporter output class, it writes output into
 * a file opened on construction.
 */
class FileOutput : public PrintfConduit
{
	public:
		FileOutput(LPCTSTR fileName);
		~FileOutput();

		void SetFileName(LPCTSTR fileName);
		bool IsValid();

		virtual void puts(const char* str);
		virtual void putc(const char ch);

	protected:
		CFile	m_file;
		bool	m_bValid;
};

/**
 * This class implements RTF exporting. The code is modified from that found
 * in SciTE.
 */
class RTFExporter : public BaseExporter
{
	public:
		RTFExporter(IOutput* pOutput, LPCTSTR lpszSchemeName, StylesList* pStyles, CScintilla* pScintilla);

		virtual LPCTSTR GetDefaultExtension();
		virtual LPCTSTR GetFileMask();

	protected:
		virtual void InternalExport(int start, int end);

		// Utility Functions
		int GetRTFHighlight(const char *rgb);
		static int GetHexChar(char ch);
		static int GetHexByte(const char *hexbyte);
		void GetRTFStyleChange(char *delta, char *last, const char *current);
};

/**
 * This class implements (X)HTML/CSS exporting.
 */
class HTMLExporter : public BaseExporter
{
public:
	HTMLExporter(IOutput* pOutput, LPCTSTR lpszSchemeName, StylesList* pStyles, CScintilla* pScintilla);

	virtual LPCTSTR GetDefaultExtension();
	virtual LPCTSTR GetFileMask();

protected:
	virtual void InternalExport(int start, int end);
};

#ifdef TESTCODE
void testExporterClasses();
#endif

#endif