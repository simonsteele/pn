/**
 * @file Document.cpp
 * @brief PN Document
 * @author Simon Steele
 * @note Copyright (c) 2005-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "ChildFrm.h"
#include "Document.h"
#include "FileUtil.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

Document::Document(LPCTSTR filename)
{
	m_pFrame = NULL;
	
	if(filename)
	{
        SetFileName(filename);
	}
	else
	{
		m_sTitle = LS(IDS_NEW_FILE_TITLE);
		if (m_sTitle.length() == 0) // in case of resource problems
		{
			m_sTitle = _T("Untitled");
		}

		m_sFilename = _T("");
	}
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
	return m_sFilename.size() > 0;
}

void Document::SetFileName(LPCTSTR filename)
{
	m_sFilename = filename;
	CFileName fn(m_sFilename);
	m_sTitle = fn.GetFileName();
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

const wchar_t* Document::GetTitle() const
{
	return m_sTitle.c_str();
}

const wchar_t* Document::GetFileName() const
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

bool Document::GetWriteProtect() const
{
	return m_pFrame->GetWriteProtect();
}

bool Document::GetCanSave() const
{
	return m_pFrame->CanSave();
}

HWND Document::GetScintillaHWND() const
{
	return m_pFrame->GetTextView()->m_hWnd;
}

bool Document::Save(const wchar_t* filename, bool setFilename)
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
	BOOST_FOREACH(extensions::ITextEditorEventSinkPtr& i, m_editSinks)
	{
		i->OnCharAdded(c);
	}
}

void Document::OnSchemeChange(const char* scheme)
{
	BOOST_FOREACH(extensions::IDocumentEventSinkPtr& i, m_sinks)
	{
		i->OnSchemeChange(scheme);
	}
}

void Document::OnAfterLoad()
{
	BOOST_FOREACH(extensions::IDocumentEventSinkPtr& i, m_sinks)
	{
		i->OnAfterLoad();
	}
}

void Document::OnBeforeSave(const wchar_t* filename)
{
	// Create a backup if the option is configured
	if (HasFile() && OPTIONS->Get(PNSK_GENERAL, _T("BackupOnSave"), false))
	{
		FileUtil::CreateBackupFile(m_sFilename.c_str(), NULL, _T(".bak"));
	}

	// Notify the event sinks that we're about to save...
	BOOST_FOREACH(extensions::IDocumentEventSinkPtr& i, m_sinks)
	{
		i->OnBeforeSave(filename);
	}
}

void Document::OnAfterSave()
{
	BOOST_FOREACH(extensions::IDocumentEventSinkPtr& i, m_sinks)
	{	
		i->OnAfterSave();
	}
}

void Document::OnModifiedChanged(bool modified)
{
	BOOST_FOREACH(extensions::IDocumentEventSinkPtr& i, m_sinks)
	{
		i->OnModifiedChanged(modified);
	}
}

void Document::OnDocClosing()
{
	BOOST_FOREACH(extensions::IDocumentEventSinkPtr& i, m_sinks)
	{
		i->OnDocClosing();
	}
}

void Document::OnWriteProtectChanged(bool writeProtect)
{
	BOOST_FOREACH(extensions::IDocumentEventSinkPtr& i, m_sinks)
	{
		i->OnWriteProtectChanged(writeProtect);
	}
}

void Document::AddEventSink(extensions::IDocumentEventSinkPtr& sink)
{
	m_sinks.push_back(sink);
}

void Document::RemoveEventSink(extensions::IDocumentEventSinkPtr& sink)
{
	std::remove(m_sinks.begin(), m_sinks.end(), sink);
	//m_sinks.remove(sink);
}

void Document::AddEventSink(extensions::ITextEditorEventSinkPtr& sink)
{
	m_editSinks.push_back(sink);
}

void Document::RemoveEventSink(extensions::ITextEditorEventSinkPtr& sink)
{
	std::remove(m_editSinks.begin(), m_editSinks.end(), sink);
	//m_editSinks.remove(sink);
}

FindNextResult Document::FindNext(extensions::ISearchOptions* options)
{
	return m_pFrame->FindNext(options);
}

bool Document::Replace(extensions::ISearchOptions* options)
{
	return m_pFrame->Replace(options);
}

int Document::ReplaceAll(extensions::ISearchOptions* options)
{
	return m_pFrame->ReplaceAll(options);
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

void Document::Activate()
{
	SetFocus(m_pFrame->m_hWnd);
}