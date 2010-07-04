/**
 * @file recorder.h
 * @brief Script recorder
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.txt) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "recorder.h"
#include "App.h"

Recorder::Recorder(App* app) : m_app(app), m_pn(app->GetPN()), m_glue(app->PyPnGlue())
{
}

void Recorder::RecordScintillaAction(int message, WPARAM wParam, LPARAM lParam)
{
	if (!m_recorder.get())
	{
		return;
	}

	try
	{
		boost::python::call_method<void>(m_recorder.get(), "recordScintillaAction", message, wParam, lParam);
	}
	catch(boost::python::error_already_set&)
	{
		std::string s = getPythonErrorString();
		m_app->AddOutput(s.c_str());
	}
}

/**
 * Called when a search action occurs, such as Find Next, Replace, Replace All
 */
void Recorder::RecordSearchAction(SearchType type, const extensions::ISearchOptions* options, FindNextResult result)
{
	if (!m_recorder.get())
	{
		return;
	}

	try
	{
		boost::python::call_method<void>(m_recorder.get(), "recordSearchAction", type, SearchOptions(*options), result);
	}
	catch(boost::python::error_already_set&)
	{
		std::string s = getPythonErrorString();
		m_app->AddOutput(s.c_str());
	}
}

/**
 * Called to start the record process.
 */
void Recorder::StartRecording()
{
	// Make sure we have no recordings in progress.
	StopRecording();
	
	try
	{
		m_recorder = boost::python::call_method<boost::python::handle<>>(m_glue.ptr(), "startRecording");
	}
	catch(boost::python::error_already_set&)
	{
		std::string s = getPythonErrorString();
		m_app->AddOutput(s.c_str());
	}
}

/**
 * Called to stop the record process.
 */
void Recorder::StopRecording()
{
	if (!m_recorder.get())
	{
		return;
	}

	// Store the document that's currently being edited.
	extensions::IDocumentPtr currentDoc(m_pn->GetCurrentDocument());

	try
	{
		boost::python::call_method<void>(m_recorder.get(), "stopRecording");
	}
	catch(boost::python::error_already_set&)
	{
		std::string s = getPythonErrorString();
		m_app->AddOutput(s.c_str());
	}

	m_recorder.reset();

	// Reactivate the document we had when we were called, making sure
	// that we haven't stolen the user's focus.
	currentDoc->Activate();
}