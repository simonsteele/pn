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
	if (!m_doc.get())
	{
		return;
	}

	try
	{
		boost::python::call_method<void>(m_recorder.ptr(), "recordScintillaAction", message, wParam, lParam);
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
	if (!m_doc.get())
	{
		return;
	}
}

/**
 * Called to start the record process.
 */
void Recorder::StartRecording()
{
	// Make sure we have no recordings in progress.
	StopRecording();

	extensions::IDocumentPtr currentDoc(m_pn->GetCurrentDocument());

	m_doc = m_pn->NewDocument("python");

	currentDoc->Activate();
	
	if (!m_doc.get())
	{
		return;
	}

	try
	{
		m_recorder = boost::python::call_method<boost::python::object>(m_glue.ptr(), "startRecording", m_doc);
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
	if (!m_doc.get())
	{
		return;
	}

	try
	{
		boost::python::call_method<void>(m_recorder.ptr(), "stopRecording");
	}
	catch(boost::python::error_already_set&)
	{
		std::string s = getPythonErrorString();
		m_app->AddOutput(s.c_str());
	}

	m_doc.reset();
	m_recorder = boost::python::object();
}