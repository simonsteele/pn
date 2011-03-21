/**
 * @file mod_pn.cpp
 * @brief PN module for PyPN, most extiface items exposed here
 * @author Simon Steele
 * @note Copyright (c) 2006-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "sinks.h"
#include "app.h"

using namespace extensions;
using namespace boost::python;

#define PN_SETSCHEME		(WM_APP+24)

namespace 
{

void RegisterScript(const char* scriptname, const char* group, const char* name)
{
	g_app->RegisterScript(scriptname, group, name);
}

boost::shared_ptr<IDocument> CurrentDoc()
{
	return g_app->GetPN()->GetCurrentDocument();
}

std::wstring GetPNPath()
{
	const wchar_t* szstr = g_app->GetPN()->GetOptionsManager()->GetPNPath();
	std::wstring str(szstr);
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

void SetOutputBasePath(std::wstring path)
{
	g_app->SetOutputBasePath(path.c_str());
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

std::wstring PNInputBox(std::wstring title, std::wstring caption)
{
	wchar_t* result = g_app->GetPN()->InputBox(title.c_str(), caption.c_str());
	if(result == NULL)
		return L"";
	std::wstring r(result);
	g_app->GetPN()->ReleaseString(result);
	return r;
}

std::string PNStringFromPointer(DWORD_PTR stringptr)
{
	const char* str = reinterpret_cast<const char*>(stringptr);
	return std::string(str);
}

extensions::ISearchOptions* PNGetUserSearchOptions()
{
	return g_app->GetPN()->GetUserSearchOptions();
}

IDocumentPtr PNOpenDocument(std::wstring filepath, const char* scheme)
{
	return g_app->GetPN()->OpenDocument(filepath.c_str(), scheme);
}

IDocumentPtr PNNewDocument(const char* scheme)
{
	return g_app->GetPN()->NewDocument(scheme);
}

void PNEvalDocument(IDocumentPtr doc)
{
	return g_app->RunDocScript(doc);
}

/**
 * wrap GetFileName to return wstring which BP has a converter for.
 */
std::wstring GetDocumentFileName(IDocumentPtr& doc)
{
	return std::wstring(doc->GetFileName());
}

/**
 * wrap GetTitle to return wstring which BP has a converter for.
 */
std::wstring GetDocumentTitle(IDocumentPtr& doc)
{
	return std::wstring(doc->GetTitle());
}

/**
 * Set the current scheme by name.
 */
void SetScheme(IDocumentPtr& doc, std::string str)
{
	doc->SendEditorMessage(PN_SETSCHEME, 0, reinterpret_cast<LPARAM>(str.c_str()));
}

/**
 * Save the current document.
 */
void SaveDocument(IDocumentPtr& doc, std::wstring str, bool setFilename)
{
	doc->Save(str.c_str(), setFilename);
}

/**
 * wrap GetFindText to return wstring which BP has a converter for.
 */
std::wstring GetSearchOptionsFindText(ISearchOptions* so)
{
	return so->GetFindText();
}

/** 
 * Wrap SetFindText to use wstring.
 */
void SetSearchOptionsFindText(ISearchOptions* so, std::wstring str)
{
	so->SetFindText(str.c_str());
}

/**
 * wrap GetReplaceText to return wstring which BP has a converter for.
 */
std::wstring GetSearchOptionsReplaceText(ISearchOptions* so)
{
	return so->GetReplaceText();
}

/**
 * wrap SetReplaceText to take wstring which BP can convert.
 */
void SetSearchOptionsReplaceText(ISearchOptions* so, std::wstring str)
{
	so->SetReplaceText(str.c_str());
}

/**
 * wrap GetFileExts to return wstring which BP has a converter for.
 */
std::wstring GetSearchOptionsFileExts(ISearchOptions* so)
{
	return so->GetFileExts();
}

/**
 * Wrap SetFileExts to use wstring which BP can convert.
 */
void SetSearchOptionsFileExts(ISearchOptions* so, std::wstring str)
{
	so->SetFileExts(str.c_str());
}

/**
 * wrap GetSearchPath to return wstring which BP has a converter for.
 */
std::wstring GetSearchOptionsSearchPath(ISearchOptions* so)
{
	return so->GetSearchPath();
}

/**
 * Wrap SetSearchPath to use wstring which BP can convert.
 */
void SetSearchOptionsSearchPath(ISearchOptions* so, std::wstring str)
{
	so->SetSearchPath(str.c_str());
}

void PNSetClipboardText(const char* text)
{
	if (text == NULL)
	{
		return;
	}

	size_t length = strlen(text);

	HGLOBAL hData = ::GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT, length + 1);
	if (hData != NULL)
	{
		HWND owner = g_app->GetPN()->GetMainWindow();

		if (::OpenClipboard(owner))
		{
			::EmptyClipboard();
			
			char* pBuf = static_cast<char*>(::GlobalLock(hData));
			memcpy(pBuf, &text[0], length + 1);
			::GlobalUnlock(hData);
			
			::SetClipboardData(CF_TEXT, hData);
			::CloseClipboard();
		}
	}
}

} // namespace

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

	def("EvalDocument", &PNEvalDocument, "Run a document as a PN python script");

	def("StringFromPointer", &PNStringFromPointer, "Get a string value from a c-style string pointer");

	def("SetClipboardText", &PNSetClipboardText, "Set clipboard text");

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

	enum_<SearchType>("SearchType")
		.value("stFindNext", stFindNext)
		.value("stReplace", stReplace)
		.value("stReplaceAll", stReplaceAll);

	///////////////////////////////////////////////////////////////////////////////////////////////
	// Expose IDocument

	// This bit of magic gets overloaded functions working...
	LRESULT (IDocument::*pSendMessage)(UINT msg, WPARAM wParam, LPARAM lParam) = &IDocument::SendEditorMessage;
	LRESULT (IDocument::*pSendMessage2)(UINT msg, WPARAM wParam, const char* strParam) = &IDocument::SendEditorMessage;

	class_<IDocument, /*boost::shared_ptr<IDocument>,*/ boost::noncopyable >("IDocument", no_init)
		.add_property("Title", &GetDocumentTitle, "Display name of the document")
		.add_property("FileName", &GetDocumentFileName, "Full filename of the document")
		.add_property("CurrentScheme", &IDocument::GetCurrentScheme, &SetScheme, "Name of the current scheme")
		.add_property("Modified", &IDocument::GetModified, "Indicates whether the document has been modified")
		.add_property("CanSave", &IDocument::GetCanSave, "Indicates whether the document can be saved (i.e. it has a filename)")

		.def("SendMessage", pSendMessage, "Send a message to the scintilla window")
		.def("SendMessage", pSendMessage2)
	
		.def("IsValid", &IDocument::IsValid, "Check if this document object is still valid")

		.def("FindNext", &IDocument::FindNext, "Find the next occurrence of a user search item, see GetUserSearchOptions")
		.def("Replace", &IDocument::Replace, "Replace the current find match based on user search settings, see GetUserSearchOptions")
		.def("ReplaceAll", &IDocument::ReplaceAll, "Replace all based on user search settings, see GetUserSearchOptions")

		.def("Save", &SaveDocument, "Save the document")
		.def("Close", &IDocument::Close, "Close the document")

		.def("Activate", &IDocument::Activate, "Activate the document")
    ;

	class_<ISearchOptions, boost::noncopyable>("ISearchOptions", no_init)
		.add_property("FindText", &GetSearchOptionsFindText, &SetSearchOptionsFindText)
		.add_property("MatchWholeWord", &ISearchOptions::GetMatchWholeWord, &ISearchOptions::SetMatchWholeWord)
		.add_property("MatchCase", &ISearchOptions::GetMatchCase, &ISearchOptions::SetMatchCase)
		.add_property("UseRegExp", &ISearchOptions::GetUseRegExp, &ISearchOptions::SetUseRegExp)
		.add_property("SearchBackwards", &ISearchOptions::GetSearchBackwards, &ISearchOptions::SetSearchBackwards)
		.add_property("LoopOK", &ISearchOptions::GetLoopOK, &ISearchOptions::SetLoopOK)
		.add_property("UseSlashes", &ISearchOptions::GetUseSlashes, &ISearchOptions::SetUseSlashes)
		.add_property("ReplaceText", &GetSearchOptionsReplaceText, &SetSearchOptionsReplaceText)
		.add_property("ReplaceInSelection", &ISearchOptions::GetReplaceInSelection, &ISearchOptions::SetReplaceInSelection)
		.add_property("FileExts", &GetSearchOptionsFileExts, &SetSearchOptionsFileExts)
		.add_property("SearchPath", &GetSearchOptionsSearchPath, &SetSearchOptionsSearchPath)
		.add_property("Recurse", &ISearchOptions::GetRecurse, &ISearchOptions::SetRecurse)
		.add_property("IncludeHidden", &ISearchOptions::GetIncludeHidden, &ISearchOptions::SetIncludeHidden)
		.add_property("Found", &ISearchOptions::GetFound)
	;

	class_<SearchOptions, bases<ISearchOptions>>("SearchOptions")
		.def(init<const ISearchOptions&>());

	def("GetUserSearchOptions", &PNGetUserSearchOptions, return_value_policy<reference_existing_object>(), "Get the object storing the user's current search options");
	//def("GetUserSearchOptions", &PNGetUserSearchOptions, "Get the object storing the user's current search options");
	
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