/**
 * @file variables.h
 * @brief default text clips variable provider
 * @author Simon Steele
 * @note Copyright (c) 2010+ Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef TEXTCLIPS_VARIABLES_H__INCLUDED
#define TEXTCLIPS_VARIABLES_H__INCLUDED

#pragma once

#include "../textclips.h"

class CChildFrame;
namespace Projects { class Project; class Workspace; }

namespace TextClips
{

using boost::iequals;

class DefaultVariableProvider : public IVariableProvider
{
public:
	DefaultVariableProvider(CChildFrame* childFrame, Projects::Workspace* currentWorkspace);

	virtual ~DefaultVariableProvider()
	{}

	/**
	 * Get the value of a variable.
	 * @param value Output for the variable value
	 * @returns true if value stored, false otherwise
	 */
	virtual bool GetVariable(const char* name, std::string& value);

	/**
	 * Find out if the user's selection was used.
	 */
	bool GetSelectionUsed() const;

private:
	tstring getProjectProp(const std::string n);
	tstring getFileProp(const std::string n);
	std::string getTextmateVariable(const std::string& name);

	void set(std::string& value, const char* variable);
	void set(std::string& value, const unsigned char* variable);
	void set(std::string& value, const TCHAR* variable);
	void set(std::string& value, const std::string& variable);
	void set(std::string& value, const std::wstring& variable);

	bool m_selectionUsed;
	CChildFrame* m_pChild;
	Projects::Workspace* m_pWorkspace;
	Projects::Project* m_pActiveProject;
};

}

#endif // #ifndef TEXTCLIPS_VARIABLES_H__INCLUDED