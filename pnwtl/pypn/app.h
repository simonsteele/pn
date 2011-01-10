/**
 * @file app.h
 * @brief Plugin Main Implementation
 * @author Simon Steele
 * @note Copyright (c) 2006-2011 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef APP_H_INCLUDED
#define APP_H_INCLUDED

/**
 * This class manages the environment for the plugin, storing
 * the links to IPN and acting as the main event sink.
 */
class App : 
	public extensions::IAppEventSink,
	public extensions::IScriptRunner
{
public:
	explicit App(boost::python::handle<>& obj, extensions::IPN* app);

	virtual ~App();

	void Initialise();

// IAppEventSink
	virtual void OnNewDocument(extensions::IDocumentPtr& doc);
	virtual void OnAppClose();
	virtual void OnDocSelected(extensions::IDocumentPtr& doc);
	virtual void OnFirstEditorCreated(HWND hWndScintilla);
	
// IScriptRunner
	virtual void RunScript(const char* name);
	virtual void RunDocScript(extensions::IDocumentPtr& doc);
	virtual void Eval(const char* script, PN::BaseString& output);
	virtual void Exec(const char* function, const char* param, int flags, PN::BaseString& output);

// Other
	void RegisterScript(const char* scriptname, const char* group, const char* name);

	extensions::IPN* GetPN() const;
	
	boost::python::object& PyModule();
	boost::python::object& PyNamespace();
	boost::python::object& PyPnGlue();

	void AddOutput(const char* text, int length = -1);
	void ClearOutput();
	void SetOutputRegEx(const char* regex);
	void SetOutputDefaultParser();
	void SetOutputBasePath(const wchar_t* path);

private:
	bool ensureOutput();
	void loadInitScript();
	void runFile(const char* filename);
	
private:
	extensions::ITextOutput* m_output;
	extensions::IScriptRegistry* m_registry;
	extensions::IPN* m_app;
	extensions::IRecorderPtr m_recorder;
	boost::python::object main_module;
	boost::python::object main_namespace;
	boost::python::object m_glue;
	bool m_outputShown;
};

extern App* g_app;

#endif // #ifndef APP_H_INCLUDED