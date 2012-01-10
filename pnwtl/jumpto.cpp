/**
 * @file jumpto.cpp
 * @brief Tag finding stuff, interfaces for plugins
 * @author Simon Steele
 * @note Copyright (c) 2002-2012 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "resource.h"
#include "../libpeanut/libpeanut/ieditor.h"
#include "../libpeanut/libpeanut/filefactory.h"
#include "plugins.h"
#include "jumpto.h"

#include "include/filefinder.h"
#include "include/tempfile.h"

//////////////////////////////////////////////////////////////////////////////
// JumpToHandler
//////////////////////////////////////////////////////////////////////////////

/**
 * Constructor
 */
JumpToHandler::JumpToHandler()
{

}

/**
 * Destructor
 */
JumpToHandler::~JumpToHandler()
{
	handlers.clear();
}

/**
 * Add a source
 */
void JumpToHandler::AddSource(extensions::ITagSource* source)
{
	char* schemesstr = _strdup(source->GetSchemesSupported());
	char* p = strtok(schemesstr, ";");
	
	while(p)
	{
		handlers.insert( HANDLERS_MAP::value_type(std::string(p), source) );
		p = strtok(NULL, ";");
	}

	free(schemesstr);
}

/**
 * Find tags
 */
void JumpToHandler::FindTags(IEditorFrame* pChildFrame, ITagSink* pNotifySink)
{
	MASKSTRUCT		tagMaskAll={~0,~0};

	// First let's find out what scheme it is...
	Scheme* pScheme = pChildFrame->GetTextView()->GetCurrentScheme();
	std::string schemeName = pScheme->GetName();
	
	extensions::ITagSource* pSource = NULL;

	HANDLERS_MAP::iterator iHandler = handlers.find(schemeName);
	if(iHandler != handlers.end())
	{
		pSource = (*iHandler).second;
	}

	if(!pSource)
	{
		return;
	}

	TempFileName* tfn = NULL;

	tstring fnstr;

	if(pChildFrame->GetModified() || !pChildFrame->CanSave())
	{
		if(pChildFrame->CanSave())
		{
			tfn = new TempFileName(pChildFrame->GetDocument()->GetFileName(), NULL, true, true);
			fnstr = tfn->t_str();
		}
		else
		{
			tfn = new TempFileName(NULL, _T(".tmp"), true);
			fnstr = tfn->t_str();
		}

		try
		{
			IFilePtr file(PN::IO::FileFactory::OpenWrite(tfn->t_str()));
			pChildFrame->SaveFile(file, false);
		}
		catch (FileSourceException&)
		{
		}
	}
	else
	{
		fnstr = pChildFrame->GetDocument()->GetFileName();
	}
	
	if(!pSource->FindTags(pNotifySink, fnstr.c_str(), static_cast<void*>(pChildFrame), tagMaskAll, schemeName.c_str()))
	{
		g_Context.m_frame->SetStatusText(_T("Failed to run tagger."));
	}

	if(tfn != NULL)
	{
		tfn->erase();
		delete tfn;
	}
}