/**
 * @file unicodefilewriter.h
 * @brief UTF-8 to UTF-8/16 file writing.
 * @author Simon Steele
 * @note Copyright (c) 2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef UNICODEFILEWRITER_H_INCLUDED
#define UNICODEFILEWRITER_H_INCLUDED

#include "include/Utf8_16.h"

/**
 * Adaptation of Utf8_16_Write to work with IFile
 */
class UnicodeFileWriter : public Utf8_16 {
public:
	UnicodeFileWriter(IFilePtr file);
	~UnicodeFileWriter();

	void SetEncoding(encodingType eType);
	void SetWriteBOM(bool writeBOM);

	size_t Write(const void* p, size_t _size);

protected:
	IFilePtr m_file;
	encodingType m_eEncoding;
	utf16* m_pBuf;
	size_t m_nBufSize;
	bool m_bFirstWrite;
	bool m_bWriteBOM;
};

#endif // #ifndef UNICODEFILEWRITER_H_INCLUDED