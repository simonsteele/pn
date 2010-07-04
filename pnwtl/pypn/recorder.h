/**
 * @file recorder.h
 * @brief Definition of the script recorder class
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.txt) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef recorder_h__included
#define recorder_h__included

#include "../extiface.h"

class App;

/**
 * Implement the script recorder extensions interface, this is where we deal with
 * making scripts out of user actions.
 */
class Recorder : public extensions::IRecorder
{
public:
	/**
	 * Constructor takes a pointer to PN to create new windows etc.
	 */
	Recorder(App* app);
	
	virtual ~Recorder(){};

	/**
	 * Called when Scintilla reports an action, these will be withheld during any operation
	 * that this interface has a stronger contract for (e.g. find/replace).
	 */
	virtual void RecordScintillaAction(int message, WPARAM wParam, LPARAM lParam);

	/**
	 * Called when a search action occurs, such as Find Next, Replace, Replace All
	 */
	virtual void RecordSearchAction(SearchType type, const extensions::ISearchOptions* options, FindNextResult result);

	/**
	 * Called to start the record process.
	 */
	virtual void StartRecording();

	/**
	 * Called to stop the record process.
	 */
	virtual void StopRecording();

private:
	App* m_app;
	boost::python::object& m_glue;
	boost::python::handle<> m_recorder;
	extensions::IPN* m_pn;
};

#endif // #ifndef recorder_h__included