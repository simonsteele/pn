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

#define CONSTANT(x) scope().attr(#x) = x

BOOST_PYTHON_MODULE(pn)
{
	///////////////////////////////////////////////////////////////////////////////////////////////
	// Expose Useful Bits

	def("CurrentDoc", &CurrentDoc);
	def("RegisterScript", &RegisterScript);
	def("AppPath", &GetPNPath);

	def("AddOutput", &AddOutput);
	def("ClearOutput", &ClearOutput);
	def("SetOutputRegEx", &SetOutputRegEx);
	def("SetOutputDefaultParser", &SetOutputDefaultParser);
	def("SetOutputBasePath", &SetOutputBasePath);

	def("MessageBox", &PNMessageBox);

	def("InputBox", &PNInputBox);

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
	//CONSTANT(fnNotFound);
	//CONSTANT(fnFound);
	//CONSTANT(fnReachedStart);

	enum_<FindNextResult>("FindNextResult")
		.value("fnNotFound", fnNotFound)
		.value("fnFound", fnFound)
		.value("fnReachedStart", fnReachedStart);

	///////////////////////////////////////////////////////////////////////////////////////////////
	// Expose IDocument

	// This bit of magic gets overloaded functions working...
	LRESULT (IDocument::*pSendMessage)(UINT msg, WPARAM wParam, LPARAM lParam) = &IDocument::SendEditorMessage;
	LRESULT (IDocument::*pSendMessage2)(UINT msg, WPARAM wParam, const char* strParam) = &IDocument::SendEditorMessage;

	class_<IDocument, /*boost::shared_ptr<IDocument>,*/ boost::noncopyable >("IDocument", no_init)
		.def("GetTitle", &IDocument::GetTitle)
		.def("GetFileName", &IDocument::GetFileName)
		.def("GetCurrentScheme", &IDocument::GetCurrentScheme)

		.def("SendMessage", pSendMessage)
		.def("SendMessage", pSendMessage2)
	
		.def("IsValid", &IDocument::IsValid)

		.def("FindNext", &IDocument::FindNext)
		.def("Replace", &IDocument::Replace)
		.def("ReplaceAll", &IDocument::ReplaceAll)
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

	def("GetUserSearchOptions", &PNGetUserSearchOptions, return_value_policy<reference_existing_object>());
	
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