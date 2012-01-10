/**
 * @file exporters.h
 * @brief Define style and style-containing classes.
 * @author Simon Steele
 * @note Copyright (c) 2002-2012 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef exporters_h__included
#define exporters_h__included

#include "include/sscontainers.h"
#include "files.h"
#include "scintillaif.h"

#include <unordered_map>

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
 * This is the base class for all data exporters, it defines common
 * functionality.
 */
class BaseExporter
{
	public:
		BaseExporter(IOutput* pOutput, LPCSTR lpszSchemeName, StylesList* pStyles, CScintilla* pScintilla);
		virtual ~BaseExporter(){}

		void Export(int start, int finish);

		virtual LPCTSTR GetDefaultExtension();
		virtual LPCTSTR GetFileMask();

	protected:
		virtual void InternalExport(int start, int finish) = 0;

		char CharAt(int position);
		int StyleAt(int position);

		StyleDetails* GetStyle(int key);
		int GetMaxStyleKey();

		int SendEditor(long Msg, WPARAM wParam=0, LPARAM lParam=0);

		CScintilla*	m_pScintilla;
		StylesList*	m_pStyles;
		IOutput*	m_out;

		LPCSTR		m_pSchemeName;
};

class ExporterFactory
{
	public:
		typedef enum { RTF, HTML } EExporterType;

		static BaseExporter* GetExporter(EExporterType type, 
			IOutput* pOutput, LPCSTR lpszSchemeName, StylesList* pStyles, CScintilla* pScintilla);

	private:
		ExporterFactory(){}
};

/**
 * This is a data exporter output class, it stores all output
 * in a growing string buffer.
 */
class StringOutput : public IOutput
{
	public:
		StringOutput(unsigned int baseSize = 4096);

		virtual void puts(const char* str);
		virtual void putc(const char ch);

		const char* c_str();

	private:
		GArray<char>	m_buffer;
        std::string     str;

};

/**
 * This is a data exporter output class, it writes output into
 * a file opened on construction.
 */
class FileOutput : public IOutput
{
	public:
		FileOutput(LPCTSTR fileName);
		~FileOutput();

		void SetFileName(LPCTSTR fileName);
		bool IsValid();

		virtual void puts(const char* str);
		virtual void putc(const char ch);

	private:
		CFile       m_file;
		bool        m_bValid;
        std::string str;
};

/**
 * This class implements RTF exporting. The code is modified from that found
 * in SciTE.
 */
class RTFExporter : public BaseExporter
{
	public:
		RTFExporter(IOutput* pOutput, LPCSTR lpszSchemeName, StylesList* pStyles, CScintilla* pScintilla);

		virtual LPCTSTR GetDefaultExtension();
		virtual LPCTSTR GetFileMask();

	protected:
		virtual void InternalExport(int start, int end);

	private:
		// Utility Functions
		int GetRTFHighlight(const char *rgb);
		static int GetHexChar(char ch);
		static int GetHexByte(const char *hexbyte);
		std::string GetRTFStyleChange(StyleDetails* currentStyle, StyleDetails* newStyle);
		
		std::unordered_map<int, int> m_colorMap;
		std::unordered_map<tstring, int> m_fontMap;
};

/**
 * This class implements (X)HTML/CSS exporting.
 */
class HTMLExporter : public BaseExporter
{
public:
	HTMLExporter(IOutput* pOutput, LPCSTR lpszSchemeName, StylesList* pStyles, CScintilla* pScintilla);

	virtual LPCTSTR GetDefaultExtension();
	virtual LPCTSTR GetFileMask();

protected:
	virtual void InternalExport(int start, int end);
};

#ifdef TESTCODE
void testExporterClasses();
#endif

#endif