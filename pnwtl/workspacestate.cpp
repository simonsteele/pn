#include "stdafx.h"
#include "include/genx/genx.h"
#include "include/pngenx.h"
#include "WorkspaceState.h"

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
		Windows1252_Utf8 conv(grouppath);
		genxAddAttribute(m_aPath, conv);
		genxEndElement(m_writer);
	}

	void WriteProject(LPCTSTR projpath)
	{
		genxStartElement(m_eProject);
		Windows1252_Utf8 conv(projpath);
		genxAddAttribute(m_aPath, conv);
		genxEndElement(m_writer);
	}

	void WriteFile(LPCTSTR filepath)
	{
		genxStartElement(m_eFile);
		Windows1252_Utf8 conv(filepath);
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

void WorkspaceState::Load()
{
	
}

void WorkspaceState::Save()
{
	WSWriter writer;
	tstring path;
	OPTIONS->GetPNPath(path, PNPATH_USERSETTINGS);
	CFileName fn(_T("workspace.pnws"));
	fn.Root(path.c_str());
	writer.Start(fn.c_str());

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

void WorkspaceState::startElement(LPCTSTR name, XMLAttributes& atts)
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

void WorkspaceState::handleProjectGroup(XMLAttributes& atts)
{
	
}

void WorkspaceState::handleProject(XMLAttributes& atts)
{
	
}

void WorkspaceState::handleFile(XMLAttributes& atts)
{
	LPCTSTR path = atts.getValue(_T("path"));
	if(path != NULL && _tcslen(path) > 0)
		g_Context.m_frame->Open(path);
}