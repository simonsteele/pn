/**
 * @file appsettings.cpp
 * @brief Loading of core application settings
 * @author Simon Steele
 * @note Copyright (c) 2005 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "appsettings.h"

AppSettings::AppSettings()
{
	m_bUseIni = false;
	m_userPath = _T("");
}

/*AppSettings::~AppSettings()
{

}*/

#define PTL_DEFAULT	0
#define PTL_CONFIG	1
#define PTL_SET		2
#define PTL_GROUP	3
#define PTL_CAT		4
#define PTL_OPTLIST	5

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

void AppSettings::startElement(LPCTSTR name, XMLAttributes& atts)
{
	BEGIN_HANDLERS()
		BEGIN_STATE(AS_DEFAULT)
			MATCH_NEWSTATE(_T("config"), AS_CONFIG)
		END_STATE()
		BEGIN_STATE(AS_CONFIG)
			HANDLE(_T("userSettings"), onUserSettingsPath)
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

void AppSettings::characterData(LPCTSTR data, int len)
{

}

void AppSettings::onUserSettingsPath(XMLAttributes& atts)
{
	LPCTSTR szPath = atts.getValue(_T("path"));
	if(szPath != NULL)
	{
		if(DirExists(szPath))
			m_userPath = szPath;
	}
}

void AppSettings::onStoreType(XMLAttributes& atts)
{
	LPCTSTR value = atts.getValue(_T("value"));
	if(value != NULL)
	{
		// bit kludgy, but quick.
		if(value[0] == _T('I'))
		{
			m_bUseIni = true;
		}
	}
}