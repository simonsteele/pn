#include "stdafx.h"
#include "sinks.h"
#include "app.h"

using namespace extensions;
using namespace boost::python;

void RegisterScript(const char* scriptname, const char* group, const char* name)
{
	g_app->RegisterScript(scriptname, group, name);
}

boost::shared_ptr<IDocument> CurrentDoc()
{
	return g_app->GetPN()->GetCurrentDocument();
}

std::string GetPNPath()
{
	std::string str;
	const TCHAR* szstr = g_app->GetPN()->GetOptionsManager()->GetPNPath();
	str = szstr;
	g_app->GetPN()->ReleaseString(szstr);
	return str;
}

void AddOutput(const char* text)
{
	g_app->AddOutput(text);
}

void ClearOutput()
{
	g_app->ClearOutput();
}

void SetOutputRegEx(const char* regex)
{
	g_app->SetOutputRegEx(regex);
}

void SetOutputDefaultParser()
{
	g_app->SetOutputDefaultParser();
}

void SetOutputBasePath(const char* path)
{
	g_app->SetOutputBasePath(path);
}

int PNMessageBox(const char* text, const char* title, int type)
{
	// TODO: Get PN Main Window Handle
	HWND hw = NULL;
	IDocumentPtr ptr = g_app->GetPN()->GetCurrentDocument();
	if(ptr.get())
		hw = ptr->GetScintillaHWND();

	return ::MessageBox(hw, text, title, type);
}

std::string PNInputBox(const char* title, const char* caption)
{
	char* result = g_app->GetPN()->InputBox(title, caption);
	if(result == NULL)
		return "";
	std::string r(result);
	g_app->GetPN()->ReleaseString(result);
	return r;
}

ISearchOptions* PNGetUserSearchOptions()
{
	return g_app->GetPN()->GetUserSearchOptions();
}

IDocumentPtr PNOpenDocument(const char* filepath, const char* scheme)
{
	return g_app->GetPN()->OpenDocument(filepath, scheme);
}

IDocumentPtr PNNewDocument(const char* scheme)
{
	return g_app->GetPN()->NewDocument(scheme);
}

#define CONSTANT(x) scope().attr(#x) = x

BOOST_PYTHON_MODULE(pn)
{
	scope().attr("__doc__") = "Utilities for working with Programmer's Notepad, entry point for all APIs";

	boost::python::docstring_options docstring_options(true);

	///////////////////////////////////////////////////////////////////////////////////////////////
	// Expose Useful Bits

	def("CurrentDoc", &CurrentDoc, "Get the current focused document");
	def("RegisterScript", &RegisterScript, "Register a script");
	def("AppPath", &GetPNPath, "Get the path Programmer's Notepad is running from");

	def("AddOutput", &AddOutput, "Add text to the output window");
	def("ClearOutput", &ClearOutput, "Clear the output window");
	def("SetOutputRegEx", &SetOutputRegEx, "Set the regular expression used to parse output text for errors");
	def("SetOutputDefaultParser", &SetOutputDefaultParser, "Switch to the default output parser");
	def("SetOutputBasePath", &SetOutputBasePath, "Set the path to use to make relative paths parsed in output text absolute");

	def("MessageBox", &PNMessageBox, "Display a message box");

	def("InputBox", &PNInputBox, "Ask the user for input");

	def("OpenDocument", &PNOpenDocument, "Open a file");

	def("NewDocument", &PNNewDocument, "Create a new document");

	CONSTANT(IDOK);
	CONSTANT(IDCANCEL);
	CONSTANT(IDYES);
	CONSTANT(IDNO);
	CONSTANT(MB_OK);
	CONSTANT(MB_YESNO);
	CONSTANT(MB_OKCANCEL);
	CONSTANT(MB_YESNOCANCEL);
	CONSTANT(MB_ICONINFORMATION);
	CONSTANT(MB_ICONQUESTION);
	CONSTANT(MB_ICONERROR);

	enum_<FindNextResult>("FindNextResult")
		.value("fnNotFound", fnNotFound)
		.value("fnFound", fnFound)
		.value("fnReachedStart", fnReachedStart)
		.value("fnInvalidRegex", fnInvalidRegex)
		.value("fnInvalidSearch", fnInvalidSearch);

	///////////////////////////////////////////////////////////////////////////////////////////////
	// Expose IDocument

	// This bit of magic gets overloaded functions working...
	LRESULT (IDocument::*pSendMessage)(UINT msg, WPARAM wParam, LPARAM lParam) = &IDocument::SendEditorMessage;
	LRESULT (IDocument::*pSendMessage2)(UINT msg, WPARAM wParam, const char* strParam) = &IDocument::SendEditorMessage;

	class_<IDocument, /*boost::shared_ptr<IDocument>,*/ boost::noncopyable >("IDocument", no_init)
		.add_property("Title", &IDocument::GetTitle, "Display name of the document")
		.add_property("FileName", &IDocument::GetFileName, "Full filename of the document")
		.add_property("CurrentScheme", &IDocument::GetCurrentScheme, "Name of the current scheme")
		.add_property("Modified", &IDocument::GetModified, "Indicates whether the document has been modified")
		.add_property("CanSave", &IDocument::GetCanSave, "Indicates whether the document can be saved (i.e. it has a filename)")

		.def("SendMessage", pSendMessage, "Send a message to the scintilla window")
		.def("SendMessage", pSendMessage2)
	
		.def("IsValid", &IDocument::IsValid, "Check if this document object is still valid")

		.def("FindNext", &IDocument::FindNext, "Find the next occurrence of a user search item, see GetUserSearchOptions")
		.def("Replace", &IDocument::Replace, "Replace the current find match based on user search settings, see GetUserSearchOptions")
		.def("ReplaceAll", &IDocument::ReplaceAll, "Replace all based on user search settings, see GetUserSearchOptions")

		.def("Save", &IDocument::Save, "Save the document")
		.def("Close", &IDocument::Close, "Close the document")
    ;

	class_<ISearchOptions, boost::noncopyable>("ISearchOptions", no_init)
		.add_property("FindText", &ISearchOptions::GetFindText, &ISearchOptions::SetFindText)
		.add_property("MatchWholeWord", &ISearchOptions::GetMatchWholeWord, &ISearchOptions::SetMatchWholeWord)
		.add_property("MatchCase", &ISearchOptions::GetMatchCase, &ISearchOptions::SetMatchCase)
		.add_property("UseRegExp", &ISearchOptions::GetUseRegExp, &ISearchOptions::SetUseRegExp)
		.add_property("SearchBackwards", &ISearchOptions::GetSearchBackwards, &ISearchOptions::SetSearchBackwards)
		.add_property("LoopOK", &ISearchOptions::GetLoopOK, &ISearchOptions::SetLoopOK)
		.add_property("UseSlashes", &ISearchOptions::GetUseSlashes, &ISearchOptions::SetUseSlashes)
		.add_property("ReplaceText", &ISearchOptions::GetReplaceText, &ISearchOptions::SetReplaceText)
		.add_property("ReplaceInSelection", &ISearchOptions::GetReplaceInSelection, &ISearchOptions::SetReplaceInSelection)
		.add_property("FileExts", &ISearchOptions::GetFileExts, &ISearchOptions::SetFileExts)
		.add_property("SearchPath", &ISearchOptions::GetSearchPath, &ISearchOptions::SetSearchPath)
		.add_property("Recurse", &ISearchOptions::GetRecurse, &ISearchOptions::SetRecurse)
		.add_property("IncludeHidden", &ISearchOptions::GetIncludeHidden, &ISearchOptions::SetIncludeHidden)
		.add_property("Found", &ISearchOptions::GetFound)
	;

	def("GetUserSearchOptions", &PNGetUserSearchOptions, return_value_policy<reference_existing_object>(), "Get the object storing the user's current search options");
	
	try
	{
		register_ptr_to_python< boost::shared_ptr<IDocument> >();
	}
	catch(error_already_set&)
	{
		std::string s = getPythonErrorString();
		OutputDebugString(s.c_str());
	}
}