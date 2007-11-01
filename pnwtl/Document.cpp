/**
 * @file Document.cpp
 * @brief PN Document
 * @author Simon Steele
 * @note Copyright (c) 2005 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "ChildFrm.h"
#include "Document.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

Document::Document(LPCTSTR filename)
{
	m_pFrame = NULL;
	
	if(filename)
        m_sFilename = filename;
	else
		m_sFilename = "<new>";
}

Document::~Document()
{

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

uint64_t Document::GetFileAge() const
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

const char* Document::GetTitle() const
{
	return m_sFilename.c_str();
}

const char* Document::GetFileName() const
{
	return m_sFilename.c_str();
}

const char* Document::GetCurrentScheme() const
{
	return m_pFrame->GetTextView()->GetCurrentScheme()->GetName();
}

bool Document::GetModified() const
{
	return m_pFrame->GetModified();
}

bool Document::GetCanSave() const
{
	return m_pFrame->CanSave();
}

HWND Document::GetScintillaHWND() const
{
	return m_pFrame->GetTextView()->m_hWnd;
}

bool Document::Save(const char* filename, bool setFilename)
{
	return m_pFrame->SaveFile(filename, setFilename, setFilename);
}

LRESULT Document::SendEditorMessage(UINT msg, WPARAM wParam, LPARAM lParam)
{
	return m_pFrame->GetTextView()->SendMessage(msg, wParam, lParam);
}

LRESULT Document::SendEditorMessage(UINT msg, WPARAM wParam, const char* strParam)
{
	return m_pFrame->GetTextView()->SendMessage(msg, wParam, (LPARAM)strParam);
}

void Document::OnCharAdded(char c)
{
	for(EventSinks::const_iterator i = m_sinks.begin(); i != m_sinks.end(); ++i)
	{
		(*i)->OnCharAdded(c);
	}
}

void Document::OnSchemeChange(const char* scheme)
{
	for(EventSinks::const_iterator i = m_sinks.begin(); i != m_sinks.end(); ++i)
	{
		(*i)->OnSchemeChange(scheme);
	}
}

void Document::OnAfterLoad()
{
	for(EventSinks::const_iterator i = m_sinks.begin(); i != m_sinks.end(); ++i)
	{
		(*i)->OnAfterLoad();
	}
}

void Document::OnBeforeSave(const char* filename)
{
	for(EventSinks::const_iterator i = m_sinks.begin(); i != m_sinks.end(); ++i)
	{
		(*i)->OnBeforeSave(filename);
	}
}

void Document::OnDocClosing()
{
	for(EventSinks::const_iterator i = m_sinks.begin(); i != m_sinks.end(); ++i)
	{
		(*i)->OnDocClosing();
	}
}

void Document::AddEventSink(extensions::IDocumentEventSinkPtr sink)
{
	m_sinks.push_back(sink);
}

void Document::RemoveEventSink(extensions::IDocumentEventSinkPtr sink)
{
	m_sinks.remove(sink);
}

FindNextResult Document::FindNext(extensions::ISearchOptions* options)
{
	return m_pFrame->FindNext(static_cast<SearchOptions*>(options));
}

bool Document::Replace(extensions::ISearchOptions* options)
{
	return m_pFrame->Replace(static_cast<SearchOptions*>(options));
}

int Document::ReplaceAll(extensions::ISearchOptions* options)
{
	return m_pFrame->ReplaceAll(static_cast<SearchOptions*>(options));
}

void Document::Close(bool dontAskUserIfUnsaved)
{
	if(dontAskUserIfUnsaved)
	{
		m_pFrame->PostMessage(WM_CLOSE, 0, PNID_DONTASKUSER);
	}
	else
	{
		m_pFrame->PostMessage(WM_CLOSE, 0, 0);
	}
}