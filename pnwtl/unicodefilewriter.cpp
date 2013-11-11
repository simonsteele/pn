/**
 * @file unicodefilewriter.cpp
 * @brief UTF-8 to UTF-8/16 file writing.
 * @author Simon Steele
 * @note Copyright (c) 2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "unicodefilewriter.h"

UnicodeFileWriter::UnicodeFileWriter(IFilePtr file) : 
	m_file(file),
	m_eEncoding(eUnknown),
	m_pBuf(NULL),
	m_bFirstWrite(true),
	m_nBufSize(0),
	m_bWriteBOM(true)
{
}

UnicodeFileWriter::~UnicodeFileWriter()
{
	delete [] m_pBuf;
	m_pBuf = NULL;
}

size_t UnicodeFileWriter::Write(const void* p, size_t _size)
{
	if (m_eEncoding == eUnknown)
	{
		// Normal write
		return m_file->Write(p, _size);
	}

	if (m_eEncoding == eUtf8)
	{
		if (m_bFirstWrite)
		{
			if(m_bWriteBOM)
			{
				m_file->Write(k_Boms[m_eEncoding], 3);
			}

			m_bFirstWrite = false;
		}

		return m_file->Write(p, _size);
	}

	if (_size > m_nBufSize)
	{
		m_nBufSize = _size;
		delete [] m_pBuf;
		m_pBuf = NULL;
		m_pBuf = new utf16[_size + 1];
	}

	if (m_bFirstWrite)
	{
		if (m_bWriteBOM && (m_eEncoding == eUtf16BigEndian || m_eEncoding == eUtf16LittleEndian))
		{
			// Write the BOM
			m_file->Write(k_Boms[m_eEncoding], 2);
		}

		m_bFirstWrite = false;
	}

	Utf8_Iter iter8;
	iter8.set(static_cast<const ubyte*>(p), _size, m_eEncoding);

	utf16* pCur = m_pBuf;

	for (; iter8; ++iter8)
	{
		if (iter8.canGet())
		{
			*pCur++ = iter8.get();
		}
	}

	size_t ret = m_file->Write(m_pBuf, (const char*)pCur - (const char*)m_pBuf);

	return ret;
}

void UnicodeFileWriter::SetEncoding(Utf8_16::encodingType eType)
{
	m_eEncoding = eType;
}

void UnicodeFileWriter::SetWriteBOM(bool writeBOM)
{
	m_bWriteBOM = writeBOM;
}
