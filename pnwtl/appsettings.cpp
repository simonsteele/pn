/**
 * @file appsettings.cpp
 * @brief Loading of core application settings
 * @author Simon Steele
 * @note Copyright (c) 2005-2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "appsettings.h"
#include "extension.h"
#include "fileutil.h"
#include "third_party/genx/genx.h"
#include "include/pngenx.h"
#include "include/filefinder.h"

//////////////////////////////////////////////////////////////////////////////////
// AppSettingsWriter

class AppSettingsWriter : public GenxXMLWriter
{
public:
	void WriteStoreType(bool ini)
	{
		genxStartElementLiteral(m_writer, NULL, u("storeType"));
		if(ini)
		{
			genxAddAttribute(m_attType, u("ini"));
		}
		else
		{
			genxAddAttribute(m_attType, u("default"));
		}
		genxEndElement(m_writer);
	}

	void WriteUserSettingsPath(LPCTSTR path)
	{
		genxStartElementLiteral(m_writer, NULL, u("userSettings"));
		addAttributeConvertUTF8(m_attPath, path);
		genxEndElement(m_writer);
	}

	void WriteExtension(LPCTSTR path, bool disabled)
	{
		genxStartElementLiteral(m_writer, NULL, u("extension"));
		addAttributeConvertUTF8(m_attPath, path);
		if(disabled)
		{
			genxAddAttribute(m_attDisabled, u("true"));
		}
		genxEndElement(m_writer);
	}

	void StartConfig()
	{
		genxStartElementLiteral(m_writer, NULL, u("config"));
	}

	void EndConfig()
	{
		genxEndElement(m_writer);
	}

protected:
	virtual void initXmlBits()
	{
		genxStatus s;
		PREDECLARE_ATTRIBUTES()
			ATT("path", m_attPath);
			ATT("type", m_attType);
			ATT("value", m_attValue);
			ATT("disabled", m_attDisabled);
		END_ATTRIBUTES()
	}

private:
	genxAttribute m_attPath;
	genxAttribute m_attType;
	genxAttribute m_attValue;
	genxAttribute m_attDisabled;
};

//////////////////////////////////////////////////////////////////////////////////
// ExtDetails

ExtDetails::ExtDetails(LPCTSTR path, LPCTSTR basePath) :
	Path(path),
	Disabled(false)
{
	CFileName fn(path);
	if(fn.IsRelativePath())
	{
		fn.Root(basePath);
	}

	FullPath = fn;
}

bool ExtDetails::Exists() const
{
	return ::FileExists( FullPath.c_str() );
}

//////////////////////////////////////////////////////////////////////////////////
// AppSettings

AppSettings::AppSettings() :
	m_bUseIni(false),
	m_userPath(_T("")),
	m_bAppSettingsPathSpecified(false)
{
	Options::StaticGetPNPath(m_pnpath);

	load();
}

OptionsFactory::EOptionsType AppSettings::GetOptionsType() const
{
	return m_bUseIni ? OptionsFactory::OTIni : OptionsFactory::OTRegistry;
}

const TCHAR* AppSettings::GetUserPath() const
{
	return m_userPath.c_str();
}

bool AppSettings::HaveUserPath() const
{
	return m_userPath.size() > 0;
}

const extlist& AppSettings::GetExtensions() const
{
	return m_extensions;
}

Options* AppSettings::MakeOptions() const
{
	Options* options = OptionsFactory::GetOptions(GetOptionsType());
	
	// See if there's a custom user settings dir.
	if(HaveUserPath())
		options->SetUserSettingsPath(GetUserPath());

	return options;
}

void AppSettings::FindExtensions()
{
	 FileFinder<AppSettings> finder(this, &AppSettings::findExtensionHandler); 
	 tstring pnpath;

	 OPTIONS->GetPNPath(pnpath);
	 finder.Find(pnpath.c_str(), _T("*.dll"), false);
}

void AppSettings::Save()
{
	save();
}

void AppSettings::findExtensionHandler(LPCTSTR path, FileFinderData& file, bool& /*shouldContinue*/)
{
	tstring fn;
	fn = file.GetFilename();
	
	ExtDetails details(fn.c_str(), m_pnpath.c_str());

	extensions::Extension test(details.FullPath.c_str(), NULL);
	if(!test.Valid())
		return;

	// See if we already know about this extension:
	for(extlist::const_iterator i = m_extensions.begin();
		i != m_extensions.end();
		++i)
	{
		CFileName fn1(details.Path.c_str());
		CFileName fn2((*i).Path.c_str());

		if(_tcsicmp(fn1.Sanitise().c_str(), fn2.Sanitise().c_str()) == 0)
		{
			// Already known
			return;
		}
	}
	
	m_extensions.push_back(details);
}

#define MATCH(ename) \
	(_tcscmp(name, ename) == 0)

#define IN_STATE(state) \
	(m_parseState == state)

#define STATE(state) \
	m_parseState = state

#define ATTVAL(attname) \
	atts.getValue(attname)

#define BEGIN_HANDLERS() \
	if(name == NULL) \
	{	return;		}

#define HANDLE(ename, fn) \
	if(MATCH(ename)) \
	{ \
		fn(atts); \
	} else

#define HANDLEEND(ename, fn) \
	if(MATCH(ename)) \
	{ \
		fn(); \
	} else

#define HANDLE_NEWSTATE(ename, fn, s) \
	if(MATCH(ename)) \
	{ \
		fn(atts); \
		STATE(s); \
	} else

#define HANDLEEND_NEWSTATE(ename, fn, s) \
	if(MATCH(ename)) \
	{ \
		fn(); \
		STATE(s); \
	} else

#define MATCH_NEWSTATE(ename, s) \
	if(MATCH(ename)) \
	{ \
		STATE(s); \
	} else

#define BEGIN_STATE(state) \
	if(m_parseState == state) \
	{ \

#define END_STATE() \
		{} \
	} else

#define END_STATE_HANDLE_UNKNOWN(fn) \
		{ \
			fn(name, atts); \
		} \
	}

#define HANDLE_UNKNOWN_NEWSTATE(fn, s) \
	else \
	{ \
		fn(name, atts); \
		STATE(s); \
	}

#define END_HANDLERS() \
	{}

#define AS_DEFAULT	0
#define AS_CONFIG	1

void AppSettings::load()
{
	CFileName fn(_T("config.xml"));
	fn.Root(m_pnpath.c_str());
	
	if(::FileExists(fn.c_str()))
	{
		load(fn.c_str());
	}

	// See if there's a user-specified user settings path, and if not use the default:
	if (!HaveUserPath())
	{
		TCHAR buf[MAX_PATH +1];
		buf[0] = '\0';

		if (FileUtil::PNGetSpecialFolderPath(buf, CSIDL_APPDATA))
		{
			m_userPath = buf;
			if (m_userPath[m_userPath.length()-1] != _T('\\'))
			{
				m_userPath += _T('\\');
			}

			m_userPath += _T("Echo Software\\PN2\\");
		}
	}

	// Make sure the User Settings path exists
	if (!CreateDirectoryRecursive(m_userPath.c_str()))
	{
		UNEXPECTED(_T("Could not create user settings folder"));
	}

	fn = _T("config.xml");
	fn.Root(m_userPath.c_str());

	if (::FileExists(fn.c_str()))
	{
		// We only use extensions found in the user settings config.xml
		m_extensions.clear();
		load(fn.c_str());
	}
}

void AppSettings::load(const TCHAR* filename)
{
	XMLParser parser;
	parser.SetParseState(this);
	m_parseState = AS_DEFAULT;

	try
	{
        parser.LoadFile(filename);
	}
	catch(XMLParserException& E)
	{
		CString err;
		err.Format(_T("Error parsing application configuration xml: %s\n (file: %s, line: %d, column %d)"), 
			XML_ErrorString(E.GetErrorCode()), E.GetFileName(), E.GetLine(), E.GetColumn());
		
		UNEXPECTED((LPCTSTR)err);
	}
}

void AppSettings::save()
{
	//TODO: Save out config here
	//1. Extensions needs expanding to include disabled or not
	//2. Need a new scan for extensions method
	//3. Need UI for extensions, and UI for other options
	//4. Need command-line parameters for setting options and scanning extensions
	//5. Should we store extensions list in user profile instead of config.xml?

	CFileName fn(_T("config.xml"));
	fn.Root(m_userPath.c_str());
	
	AppSettingsWriter writer;
	if (!writer.Start(fn.c_str()))
	{
		return;
	}

	writer.StartConfig();

	if(m_bUseIni)
	{
		writer.WriteStoreType(true);
	}

	if (m_bAppSettingsPathSpecified && m_userPath.size())
	{
		writer.WriteUserSettingsPath(m_userPath.c_str());
	}

	for(extlist::const_iterator i = m_extensions.begin();
		i != m_extensions.end();
		++i)
	{
		writer.WriteExtension( (*i).Path.c_str(), (*i).Disabled );
	}

	writer.EndConfig();
	writer.Close();
}

void AppSettings::startElement(LPCTSTR name, const XMLAttributes& atts)
{
	BEGIN_HANDLERS()
		BEGIN_STATE(AS_DEFAULT)
			MATCH_NEWSTATE(_T("config"), AS_CONFIG)
		END_STATE()
		BEGIN_STATE(AS_CONFIG)
			HANDLE(_T("userSettings"), onUserSettingsPath)
			HANDLE(_T("storeType"), onStoreType)
			HANDLE(_T("extension"), onExtension)
		END_STATE()
	END_HANDLERS()
}

void AppSettings::endElement(LPCTSTR name)
{
	BEGIN_HANDLERS()
		BEGIN_STATE(AS_CONFIG)
			MATCH_NEWSTATE(_T("config"), AS_DEFAULT)
		END_STATE()
	END_HANDLERS()
}

void AppSettings::characterData(LPCTSTR /*data*/, int /*len*/)
{

}

void AppSettings::onUserSettingsPath(const XMLAttributes& atts)
{
	LPCTSTR szPath = atts.getValue(_T("path"));
	if (szPath != NULL && szPath[0] != NULL)
	{
		// Check for relative paths
		CPathName path(szPath);
		if (path.IsRelativePath())
		{
			path.Root( m_pnpath.c_str() );
		}

		m_userPath = path.c_str();
		m_bAppSettingsPathSpecified = true;
	}
}

void AppSettings::onStoreType(const XMLAttributes& atts)
{
	LPCTSTR value = atts.getValue(_T("value"));
	if(value != NULL && value[0] != NULL)
	{
		// bit kludgy, but quick.
		if(value[0] == _T('I'))
		{
			m_bUseIni = true;
		}
	}
}

/**
 * Handle an extension element.
 */
void AppSettings::onExtension(const XMLAttributes& atts)
{
	LPCTSTR path = atts.getValue(_T("path"));
	LPCTSTR disabled = atts.getValue(_T("disabled"));
	if(path == NULL || path[0] == NULL)
	{
		LOG(_T("Found extension element with no \"path\" attribute, ignoring."));
		return;
	}

	ExtDetails details(path, m_pnpath.c_str());

	// If the extension is disabled we prepend a hash.
	if(disabled != NULL && disabled[0] != NULL)
	{
		if(disabled[0] == 't') //rue
		{
			details.Disabled = true;
		}
	}

	// Store the extension path.
	m_extensions.push_back(details);
}