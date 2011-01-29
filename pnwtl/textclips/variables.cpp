/**
 * @file variables.cpp
 * @brief PN Project Variables
 * @author Simon Steele
 * @note Copyright (c) 2010+ Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "variables.h"
#include "../childfrm.h"
#include "../project.h"
#include "../projectprops.h"
#include "../include/encoding.h"

using namespace TextClips;

DefaultVariableProvider::DefaultVariableProvider(CChildFrame* childFrame, Projects::Workspace* currentWorkspace) :
    m_pChild(childFrame),
	m_pWorkspace(currentWorkspace),
	m_pActiveProject(NULL),
	m_selectionUsed(false)
{
	if (currentWorkspace != NULL)
	{
		m_pActiveProject = currentWorkspace->GetActiveProject();
	}
}

/**
 * Get the value of a variable.
 * @param value Output for the variable value
 * @returns true if value stored, false otherwise
 */
bool DefaultVariableProvider::GetVariable(const char* name, std::string& value)
{
	std::string n(name);

	if (boost::starts_with(n, "TM_"))
	{
		n = getTextmateVariable(n);
	}

	value = "";
	char itosbuf[40];

	if (iequals(n, "PN_FileName"))
	{
		if (m_pChild)
		{
			set(value, m_pChild->GetFileName(FN_FILE));
		}
	}
	else if (iequals(n, "PN_FileDirectory"))
	{
		if (m_pChild)
		{
			CPathName pn(m_pChild->GetFileName(FN_PATH).c_str());
			set(value, pn.c_str());
		}
	}
	else if (iequals(n, "PN_FilenameNoExt"))
	{
		if (m_pChild)
		{
			set(value, m_pChild->GetFileName(FN_FILEPART));
		}
	}
	else if (iequals(n, "PN_FilePath"))
	{
		if (m_pChild)
		{
			set(value, m_pChild->GetFileName(FN_FULL));
		}
	}
	else if (iequals(n, "PN_CurrentLine"))
	{
		if (m_pChild)
		{
			_itoa(m_pChild->GetPosition(EP_LINE), itosbuf, 10);
			set(value, itosbuf);
			boost::trim_left_if(value, boost::is_any_of("\r\n"));
		}
	}
	else if (iequals(n, "PN_CurrentColumn"))
	{
		if (m_pChild)
		{
			_itoa(m_pChild->GetPosition(EP_COL), itosbuf, 10);
			set(value, itosbuf);
		}
	}
	else if (iequals(n, "PN_CurrentWord"))
	{
		if (m_pChild)
		{
			set(value, m_pChild->GetTextView()->GetCurrentWord());
		}
	}
	else if (iequals(n, "PN_CurrentLineText"))
	{
		if (m_pChild)
		{
			set(value, m_pChild->GetTextView()->GetLineText());
		}
	}
	else if (iequals(n, "PN_SelectedText"))
	{
		if (m_pChild)
		{
			set(value, m_pChild->GetTextView()->GetSelText());
			m_selectionUsed = true;
		}
	}
	else if (iequals(n, "PN_CurrentProjectFile"))
	{
		if (m_pWorkspace != NULL)
		{
			Projects::Project* pProject = m_pWorkspace->GetActiveProject();

			if (pProject != NULL && pProject->Exists())
			{
				CFileName fn(pProject->GetFileName());
				set(value, fn.c_str());
			}
		}
	}
	else if (iequals(n, "PN_CurrentProjectGroupFile"))
	{
		if (m_pWorkspace != NULL && m_pWorkspace->CanSave())
		{
			CFileName fn(m_pWorkspace->GetFileName());
			set(value, fn.c_str());
		}
	}
	else if (iequals(n, "PN_ProjectPath"))
	{
		if (m_pActiveProject)
		{
			CFileName fn(m_pActiveProject->GetFileName());
			set(value, fn.c_str());
		}
	}
	else if (iequals(n, "PN_ProjectGroupPath"))
	{
		if (m_pWorkspace != NULL && m_pWorkspace->CanSave())
		{
			CFileName fn(m_pWorkspace->GetFileName());
			set(value, fn.c_str());
		}
	}
	else if (boost::istarts_with(n, "PN_ProjectProp:"))
	{
		if (m_pActiveProject)
		{
			set(value, getProjectProp(n));
		}
	}
	else if (boost::istarts_with(n, "PN_FileProp:"))
	{
		if (m_pActiveProject)
		{
			set(value, getFileProp(n));
		}
	}
	else if (iequals(n, "PN_ProjectName"))
	{
		if (m_pActiveProject)
		{
			set(value, m_pActiveProject->GetName());
		}
	}
	else if (iequals(n, "PN_ProjectGroupName"))
	{
		if (m_pWorkspace != NULL)
		{
			set(value, m_pWorkspace->GetName());
		}
	}
	else if (iequals(n, "PN_PNPath"))
	{
		tstring pn;
		OPTIONS->GetPNPath(pn);
		set(value, pn);
	}
	else if (iequals(n, "PN_UseTabsYesNo"))
	{
		// TODO
		value = "NO";
	}
	else if (iequals(n, "PN_TabSize"))
	{
		// TODO
		if (m_pChild)
		{
			int tabSize = m_pChild->GetTextView()->GetTabWidth();
			char buffer[34];
			_snprintf(buffer, 33, "%d", tabSize);
			set(value, buffer);
		}
	}
	else if (iequals(n, "PN_Time"))
	{
		time_t now;
		time(&now);
		char buffer[40];
		struct tm* local(localtime(&now));
		if (strftime(buffer, 40, "%X", local) != 0)
		{
			value = buffer;
		}
	}
	else if (iequals(n, "PN_Date"))
	{
		time_t now;
		time(&now);
		char buffer[41];
		struct tm* local(localtime(&now));
		if (strftime(buffer, 40, "%x", local) != 0)
		{
			value = buffer;
		}
	}

	// unmatched:
	else
	{
		return false;
	}

	return true;		
}

/**
 * Find out if the user's selection was used.
 */
bool DefaultVariableProvider::GetSelectionUsed() const
{
	return m_selectionUsed;
}

tstring DefaultVariableProvider::getProjectProp(const std::string n)
{
	Projects::ProjectTemplate* pTemplate = m_pActiveProject->GetTemplate();

	if (!pTemplate)
	{
		return tstring(_T(""));
	}

	boost::xpressive::tsregex re = boost::xpressive::tsregex::compile(L"ProjectProp:(?P<group>[-_a-zA-Z0-9]+)\\.(?P<cat>[-_a-zA-Z0-9]+)\\.(?P<val>[-_a-zA-Z0-9]+)");
	boost::xpressive::tsmatch match;
	
	Utf8_Tcs conv(n.c_str());
	tstring prop(conv);

	if (boost::xpressive::regex_match(prop, match, re))
	{
		// Extract the named matches from the RE, noting if there was a line or column.
		tstring group(match[_T("group")]);
		tstring cat(match[_T("cat")]);
		tstring val(match[_T("val")]);

		if (group.empty() || cat.empty() || val.empty())
		{
			return tstring(_T(""));
		}

		LPCTSTR retval = m_pActiveProject->GetUserData().Lookup(pTemplate->GetNamespace(), group.c_str(), cat.c_str(), val.c_str(), _T(""));

		if (retval != NULL)
		{
			return retval;
		}
	}

	return tstring(_T(""));
}

tstring DefaultVariableProvider::getFileProp(const std::string n)
{
	if (!m_pActiveProject)
	{
		return tstring(_T(""));
	}

	Projects::ProjectTemplate* pTemplate = m_pActiveProject->GetTemplate();

	if (!pTemplate || !m_pChild)
	{
		return tstring(_T(""));
	}

	Projects::File* pFileObj = m_pActiveProject->FindFile(m_pChild->GetFileName().c_str());

	if (!pFileObj)
	{
		return tstring(_T(""));
	}

	boost::xpressive::tsregex re = boost::xpressive::tsregex::compile(_T("FileProp:(?P<group>[-_a-zA-Z0-9]+)\\.(?P<cat>[-_a-zA-Z0-9]+)\\.(?P<val>[-_a-zA-Z0-9]+)"));
	boost::xpressive::tsmatch match;
	
	Utf8_Tcs conv(n.c_str());
	
	tstring prop(conv);

	if (boost::xpressive::regex_match(prop, match, re))
	{
		// Extract the named matches from the RE, noting if there was a line or column.
		tstring group(match[_T("group")]);
		tstring cat(match[_T("cat")]);
		tstring val(match[_T("val")]);

		if (group.empty() || cat.empty() || val.empty())
		{
			return tstring(_T(""));
		}

		LPCTSTR retval = pFileObj->GetUserData().Lookup(pTemplate->GetNamespace(), group.c_str(), cat.c_str(), val.c_str(), _T(""));

		if (retval != NULL)
		{
			return retval;
		}
	}

	return tstring(_T(""));
}

std::string DefaultVariableProvider::getTextmateVariable(const std::string& name)
{
	if (name == "TM_CURRENT_LINE")
	{
		// Current line text
		return "PN_CurrentLineText";
	}
	else if (name == "TM_CURRENT_WORD")
	{
		// Current cursor word
		return "PN_CurrentWord";
	}
	else if (name == "TM_DIRECTORY")
	{
		// Current directory
		return "PN_FileDirectory";
	}
	else if (name == "TM_FILEPATH")
	{
		// Full path for current file
		return "PN_FilePath";
	}
	else if (name == "TM_LINE_INDEX")
	{
		// Index in the current line, or current column in real money
		return "PN_CurrentColumn";
	}
	else if (name == "TM_LINE_NUMBER")
	{
		// Current line number.
		return "PN_CurrentLine";
	}
	else if (name == "TM_PROJECT_DIRECTORY")
	{
		// Top-level directory in the project drawer, probably the project path
		return "PN_ProjectPath";
	}
	else if (name == "TM_SELECTED_FILES")
	{
		// Shell-escaped list of selected files in the projects window
		return "Unsupported Variable: TM_SELECTED_FILES";
	}
	else if (name == "TM_SELECTED_FILE")
	{
		// Shell-escaped first selected file in the projects window
		return "Unsupported Variable: TM_SELECTED_FILE";
	}
	else if (name == "TM_SELECTED_TEXT")
	{
		// Current selected text
		return "PN_SelectedText";
	}
	else if (name == "TM_SOFT_TABS")
	{
		// YES if soft tabs are enabled, NO otherwise
		return "PN_UseTabsYesNo";
	}
	else if (name == "TM_TAB_SIZE")
	{
		// Tab size
		return "PN_TabSize";
	}

	return name;
}

void DefaultVariableProvider::set(std::string& value, const char* variable)
{
	PNASSERT(variable != NULL);
	value = variable;
}

void DefaultVariableProvider::set(std::string& value, const unsigned char* variable)
{
	PNASSERT(variable != NULL);
	value = reinterpret_cast<const char*>(variable);
}

void DefaultVariableProvider::set(std::string& value, const TCHAR* variable)
{
	PNASSERT(variable != NULL);
	Tcs_Utf8 conv(variable);
	value = static_cast<const char*>(conv.c_str());
}

void DefaultVariableProvider::set(std::string& value, const std::string& variable)
{
	value = variable;
}

void DefaultVariableProvider::set(std::string& value, const std::wstring& variable)
{
	set(value, variable.c_str());
}