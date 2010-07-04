/**
 * @file tools.cpp
 * @brief External tools code
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "tools.h"
#include "childfrm.h"
#include "project.h"
#include "pndialogs.h"
#include "projectprops.h"
#include "scriptregistry.h"

//////////////////////////////////////////////////////////////////////////////
// ToolCommandString
//////////////////////////////////////////////////////////////////////////////

void ToolCommandString::OnFormatChar(TCHAR thechar)
{
	switch (thechar)
	{
		case _T('f'):

			if (pChild)
				m_string += pChild->GetFileName(FN_FILE);

			break;

		case _T('d'):
		{
			if (pChild)
			{
				CPathName pn(pChild->GetFileName(FN_PATH));

				if (reversePathSeps)
					pn.SetForwardSlashes();

				m_string += pn;
			}
		}
		break;

		case _T('n'):

			if (pChild)
				m_string += pChild->GetFileName(FN_FILEPART);

			break;

		case _T('l'):

			if (pChild)
			{
				_itot(pChild->GetPosition(EP_LINE), itosbuf, 10);
				m_string += itosbuf;
			}

			break;

		case _T('c'):

			if (pChild)
			{
				_itot(pChild->GetPosition(EP_COL), itosbuf, 10);
				m_string += itosbuf;
			}

			break;

		case _T('w'):

			if (pChild)
			{
				CA2CT word(pChild->GetTextView()->GetCurrentWord().c_str());
				m_string += word;
			}

			break;

			// current project file.
		case _T('p'):
		{
			Projects::Workspace* pWorkspace = g_Context.m_frame->GetActiveWorkspace();

			if (pWorkspace != NULL)
			{
				Projects::Project* pProject = pWorkspace->GetActiveProject();

				if (pProject != NULL && pProject->Exists())
				{
					CFileName fn(pProject->GetFileName());

					if (reversePathSeps)
						fn.SetForwardSlashes();

					m_string += fn;
				}
			}
		}
		break;

		// current project group (workspace) file.
		case _T('g'):
		{
			Projects::Workspace* pWorkspace = GetWorkspace();

			if (pWorkspace != NULL && pWorkspace->CanSave())
			{
				CFileName fn(pWorkspace->GetFileName());

				if (reversePathSeps)
					fn.SetForwardSlashes();

				m_string += fn;
			}
		}
		break;

		case _T('?'):
		{
			CInputDialog dlg(_T("Tool Parameters"), _T("Parameters:"));

			if ( dlg.DoModal() == IDOK )
			{
				m_string += dlg.GetInput();
			}
			else
			{
				throw FormatStringBuilderException();
			}
		}
		break;
	}
}

#define MATCH(s) \
	(_tcscmp(key, s) == 0)

#define MATCH_START(s) \
	((_tcslen(key) > _tcslen(s)) && (_tcsnicmp(s, key, _tcslen(s)) == 0))

void ToolCommandString::OnFormatKey(LPCTSTR key)
{
	if (MATCH(_T("ProjectPath")))
	{
		Projects::Project* pP = GetActiveProject();

		if (pP)
		{
			CFileName fn(pP->GetFileName());

			if (reversePathSeps)
				fn.SetForwardSlashes();

			m_string += fn.GetPath();
		}
	}
	else if (MATCH(_T("ProjectGroupPath")))
	{
		Projects::Workspace* pWorkspace = GetWorkspace();

		if (pWorkspace != NULL && pWorkspace->CanSave())
		{
			CFileName fn(pWorkspace->GetFileName());

			if (reversePathSeps)
				fn.SetForwardSlashes();

			m_string += fn.GetPath();
		}
	}
	else if (MATCH_START(_T("ProjectProp:")))
	{
		Projects::Project* pP = GetActiveProject();

		if (!pP)
			return;

		Projects::ProjectTemplate* pTemplate = pP->GetTemplate();

		if (!pTemplate)
			return;

		boost::xpressive::tsregex re = boost::xpressive::tsregex::compile(L"ProjectProp:(?P<group>[-_a-zA-Z0-9]+)\\.(?P<cat>[-_a-zA-Z0-9]+)\\.(?P<val>[-_a-zA-Z0-9]+)");
		boost::xpressive::tsmatch match;
		
		tstring prop(key);

		if (boost::xpressive::regex_match(prop, match, re))
		{
			// Extract the named matches from the RE, noting if there was a line or column.
			tstring group(match[_T("group")]);
			tstring cat(match[_T("cat")]);
			tstring val(match[_T("val")]);

			if (group.empty() || cat.empty() || val.empty())
			{
				return;
			}

			LPCTSTR retval = pP->GetUserData().Lookup(pTemplate->GetNamespace(), group.c_str(), cat.c_str(), val.c_str(), _T(""));

			if (retval != NULL)
			{
				m_string += retval;
			}
		}
	}
	else if (MATCH_START(_T("FileProp:")))
	{
		Projects::Project* pP = GetActiveProject();

		if (!pP)
			return;

		Projects::ProjectTemplate* pTemplate = pP->GetTemplate();

		if (!pTemplate)
			return;

		if (!pChild)
			return;

		Projects::File* pFileObj = pP->FindFile(pChild->GetFileName().c_str());

		if (!pFileObj)
			return;

		boost::xpressive::tsregex re = boost::xpressive::tsregex::compile(_T("FileProp:(?P<group>[-_a-zA-Z0-9]+)\\.(?P<cat>[-_a-zA-Z0-9]+)\\.(?P<val>[-_a-zA-Z0-9]+)"));
		boost::xpressive::tsmatch match;
		tstring prop(key);

		if (boost::xpressive::regex_match(prop, match, re))
		{
			// Extract the named matches from the RE, noting if there was a line or column.
			tstring group(match[_T("group")]);
			tstring cat(match[_T("cat")]);
			tstring val(match[_T("val")]);

			if (group.empty() || cat.empty() || val.empty())
			{
				return;
			}

			LPCTSTR retval = pFileObj->GetUserData().Lookup(pTemplate->GetNamespace(), group.c_str(), cat.c_str(), val.c_str(), _T(""));

			if (retval != NULL)
			{
				m_string += retval;
			}
		}
	}
	else if (MATCH(_T("ProjectName")))
	{
		Projects::Project *pP = GetActiveProject();

		if (pP)
		{
			m_string += pP->GetName();
		}
	}
	else if (MATCH(_T("ProjectGroupName")))
	{
		Projects::Workspace *pW = GetWorkspace();

		if (pW != NULL)
		{
			m_string += pW->GetName();
		}
	}
	else if (MATCH(_T("PNPath")))
	{
		tstring pn;
		OPTIONS->GetPNPath(pn);
		m_string += pn;
	}
	else
	{
		tstring s = _T("Unknown constant: $(");
		s += key;
		s += _T(").");
		g_Context.m_frame->SetStatusText(s.c_str());
	}
}

/**
 * Support environment variables
 */
void ToolCommandString::OnFormatPercentKey(LPCTSTR key)
{
	TCHAR value[32767]; // Max size for an environment variable

	if (GetEnvironmentVariable(key, &value[0], 32767) == 0)
	{
		value[0] = 0; // Make sure it's an empty string even on error
	}

	m_string += value;
}

/**
 * Let a script engine make our parameter value
 */
void ToolCommandString::OnFormatScriptRef(LPCTSTR key)
{
	// We're going to evaluate tool parameters within our script call:
	ToolCommandString cmdstr;
	tstring script = cmdstr.Build(key);

	CT2CA scriptconv(script.c_str());
	
	std::string thescript(scriptconv);
	int index = thescript.find(_T(':'));
	
	std::string engine = thescript.substr(0, index);
	thescript = thescript.substr(index + 1);

	extensions::IScriptRunner* runner = ScriptRegistry::GetInstanceRef().GetRunner(engine.c_str());
	if (runner == NULL)
	{
		return;
	}

	PN::AString output;
	runner->Eval(thescript.c_str(), output);
	
	if (output.GetLength())
	{
		CA2CT outputconv(output.Get());
		m_string += outputconv;
	}
}

#undef MATCH
#undef MATCH_START

Projects::Workspace* ToolCommandString::GetWorkspace()
{
	Projects::Workspace* pWorkspace = g_Context.m_frame->GetActiveWorkspace();
	return pWorkspace;
}

Projects::Project* ToolCommandString::GetActiveProject()
{
	Projects::Workspace* pWorkspace = GetWorkspace();

	if (pWorkspace != NULL)
	{
		Projects::Project* pProject = pWorkspace->GetActiveProject();

		if (pProject != NULL && pProject->Exists())
			return pProject;
	}

	return NULL;
}