/**
 * @file Document.cpp
 * @brief PN Document
 * @author Simon Steele
 * @note Copyright (c) 2005 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "ChildFrm.h"
#include "Document.h"

Document::Document(LPCTSTR filename)
{
	m_pFrame = NULL;
	
	if(filename)
        m_sFilename = filename;
	else
		m_sFilename = "<new>";
}

void Document::AddChildFrame(CChildFrame* pFrame)
{
	PNASSERT(m_pFrame == NULL); // Currently only support one frame per doc.
	m_pFrame = pFrame;
}

bool Document::FileExists() const
{
	return HasFile() && ::FileExists(m_sFilename.c_str());
}

long Document::GetFileAge() const
{
	return FileAge(m_sFilename.c_str());
}

tstring Document::GetFileName(EGFNType type) const
{
	CFileName fn(m_sFilename);

	switch(type)
	{
		case FN_FULL:
			return fn;

		case FN_FILE:
			return fn.GetFileName();

		case FN_FILEPART:
			return fn.GetFileName_NoExt();

		case FN_PATH:
			return fn.GetPath();

		default:
			return fn;
	};
}

CChildFrame* Document::GetFrame() const
{
	return m_pFrame;
}

bool Document::HasFile() const
{
	return ((m_sFilename != _T("")) && (m_sFilename.find(_T("<")) == -1));
}

void Document::SetFileName(LPCTSTR filename)
{
	m_sFilename = filename;
}

void Document::SetValid(bool bValid)
{
	m_bIsValid = bValid;
	if(!bValid)
		m_pFrame = NULL;
}

bool Document::IsValid() const
{
	return m_bIsValid;
}