/**
 * @file WorkspaceState.cpp
 * @brief Load and save the set of open files.
 * @author Simon Steele
 * @note Copyright (c) 2002-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "third_party/genx/genx.h"
#include "include/pngenx.h"
#include "WorkspaceState.h"
#include "project.h"
#include "resource.h"

//
// <Workspace>
//     <File path="c:\asdfa\sdfsdfsdf.sdfs"/>
//     <Project path="\\monkey\banana\tepl.pnproj"/>
// </Workspace>

//////////////////////////////////////////////////////////////////////////////
// WSWriter - write the XML for the WorkspaceState file.
//////////////////////////////////////////////////////////////////////////////

class WSWriter : public GenxXMLWriter
{
public:
	void WriteProjectGroup(LPCTSTR grouppath)
	{
		genxStartElement(m_eProjectGroup);
		Tcs_Utf8 conv(grouppath);
		genxAddAttribute(m_aPath, conv);
		genxEndElement(m_writer);
	}

	void WriteProject(LPCTSTR projpath)
	{
		genxStartElement(m_eProject);
		Tcs_Utf8 conv(projpath);
		genxAddAttribute(m_aPath, conv);
		genxEndElement(m_writer);
	}

	void WriteFile(LPCTSTR filepath)
	{
		genxStartElement(m_eFile);
		Tcs_Utf8 conv(filepath);
		genxAddAttribute(m_aPath, conv);
		genxEndElement(m_writer);
	}

	void BeginWorkspace()
	{
		genxStartElement(m_eWorkspace);
	}

	void EndWorkspace()
	{
		genxEndElement(m_writer);
	}

protected:
	/**
	 * Use this to initialize all your elements that you'll use over and
	 * over.
	 */
	virtual void initXmlBits()
	{
		genxStatus s;
		m_eWorkspace = genxDeclareElement(m_writer, NULL, u("Workspace"), &s);
		m_eProjectGroup = genxDeclareElement(m_writer, NULL, u("ProjectGroup"), &s);
		m_eProject = genxDeclareElement(m_writer, NULL, u("Project"), &s);
		m_eFile = genxDeclareElement(m_writer, NULL, u("File"), &s);
		m_aPath = genxDeclareAttribute(m_writer, NULL, u("path"), &s);
	}

protected:
	genxElement m_eWorkspace;
	genxElement m_eProjectGroup;
	genxElement m_eProject;
	genxElement m_eFile;
	genxAttribute m_aPath;
};

//////////////////////////////////////////////////////////////////////////////
// WorkspaceState
//////////////////////////////////////////////////////////////////////////////

void WorkspaceState::Load(LPCTSTR szPath)
{
	if(szPath == NULL)
	{
		tstring path;
		getDefaultPath(path);
		load(path.c_str());
	}
	else
	{
		load(szPath);
	}
}

void WorkspaceState::Save(LPCTSTR szPath)
{
	if(szPath == NULL)
	{
		tstring path;
		getDefaultPath(path);
		save(path.c_str());
	}
	else
	{
		save(szPath);
	}
}

void WorkspaceState::getDefaultPath(tstring& str) const
{
	OPTIONS->GetPNPath(str, PNPATH_USERSETTINGS);
	CFileName fn(_T("workspace.pnws"));
	fn.Root(str.c_str());
	str = fn.c_str();
}

void WorkspaceState::load(LPCTSTR filename)
{
	XMLParser parser;
	parser.SetParseState(this);
	m_parseState = 0;
	try
	{
		parser.LoadFile(filename);
	}
	catch(XMLParserException& ex)
	{
		CString err;
		err.Format(_T("Error Parsing Workspace XML: %s\n (file: %s, line: %d, column %d)"), 
			XML_ErrorString(ex.GetErrorCode()), ex.GetFileName(), ex.GetLine(), ex.GetColumn());

		g_Context.m_frame->SetStatusText(err);
	}
}

void WorkspaceState::save(LPCTSTR filename)
{
	WSWriter writer;
	if (!writer.Start(filename))
	{
		CString str;
		str.Format(IDS_WORKSPACESAVEFAIL, filename);

		PNTaskDialog(g_Context.m_frame->GetWindow()->m_hWnd, IDR_MAINFRAME, _T(""), (LPCTSTR)str, TDCBF_OK_BUTTON, TDT_WARNING_ICON);
		return;
	}

	writer.BeginWorkspace();

	DocumentList list;
	g_Context.m_frame->GetOpenDocuments(list);
	
	// TODO - perhaps save project open files in project state, and
	// then remove them from the list of open files to save.

	for(DocumentList::iterator i = list.begin(); i != list.end(); ++i)
	{
		if(!(*i)->HasFile())
			continue;
		writer.WriteFile( (*i)->GetFileName(FN_FULL).c_str() );
	}

	Projects::Workspace* ws = g_Context.m_frame->GetActiveWorkspace();
	if(ws)
	{
		if(ws->CanSave())
		{
			// We can write ProjectGroup information
			writer.WriteProjectGroup(ws->GetFileName());
		}
		else
		{
			const Projects::PROJECT_LIST& projects = ws->GetProjects();
			for(Projects::PROJECT_LIST::const_iterator j = projects.begin(); j != projects.end(); ++j)
			{
				writer.WriteProject( (*j)->GetFileName().c_str() );
			}
		}
	}

	writer.EndWorkspace();

	writer.Close();
}

#define MATCH(x) \
	(_tcscmp(x, name) == 0)
#define IN_STATE(x) \
	(m_parseState == x)
#define STATE(x) \
	m_parseState = x

#define WS_BEGIN		0
#define WS_WORKSPACE	1

void WorkspaceState::startElement(LPCTSTR name, const XMLAttributes& atts)
{
	if(IN_STATE(WS_BEGIN))
	{
		if(MATCH(_T("Workspace")))
		{
			STATE(WS_WORKSPACE);
		}
	}
	else if(IN_STATE(WS_WORKSPACE))
	{
		if(MATCH(_T("ProjectGroup")))
		{
			handleProjectGroup(atts);
		}
		else if(MATCH(_T("Project")))
		{
			handleProject(atts);
		}
		else if(MATCH(_T("File")))
		{
			handleFile(atts);
		}
		else
		{
			PNASSERT(false);
		}
	}
}

void WorkspaceState::endElement(LPCTSTR name)
{
	if(IN_STATE(WS_WORKSPACE) && MATCH(_T("Workspace")))
	{
		STATE(WS_BEGIN);
	}
}

void WorkspaceState::handleProjectGroup(const XMLAttributes& atts)
{
	LPCTSTR path = atts.getValue(_T("path"));
	if(path != NULL && _tcslen(path) > 0)
	{
		g_Context.m_frame->OpenProjectGroup(path);
	}
}

void WorkspaceState::handleProject(const XMLAttributes& atts)
{
	LPCTSTR path = atts.getValue(_T("path"));
	if(path != NULL && _tcslen(path) > 0)
	{
		g_Context.m_frame->OpenProject(path, true);
	}
}

void WorkspaceState::handleFile(const XMLAttributes& atts)
{
	LPCTSTR path = atts.getValue(_T("path"));
	if(path != NULL && _tcslen(path) > 0)
	{
		if (!g_Context.m_frame->CheckAlreadyOpen(path, eSwitch))
		{
			g_Context.m_frame->Open(path);
		}
	}
}