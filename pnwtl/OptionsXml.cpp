/**
 * @file OptionsXml.cpp
 * @brief Xml configuration functionality.
 * @author Simon Steele
 * @note Copyright (c) 2008-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "OptionsManager.h"
#include "third_party/genx/genx.h"
#include "include/pngenx.h"
#include "OptionsXml.h"

#pragma warning( push )
#pragma warning(disable: 4996) // see MSDN on hash_map

class OptionsXmlWriter : public GenxXMLWriter
{
public:
	void WriteOption(LPCTSTR key, LPCTSTR value)
	{
		genxStartElementLiteral(m_writer, NULL, u("o"));
		addAttributeConvertUTF8(m_attKey, key);
		Tcs_Utf8 val8(value);
		genxAddText(m_writer, val8);
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
			ATT("k", m_attKey);
		END_ATTRIBUTES()
	}

private:
	genxAttribute m_attKey;
};


XmlOptions::XmlOptions() : m_loaded(false), m_groupLocked(false)
{
	GetPNPath(m_userSettingsPath, PNPATH_USERSETTINGS);
	m_userSettingsPath += _T("UserSettings.xml");
}

XmlOptions::~XmlOptions()
{
	if (m_loaded)
	{
		save();
	}
}

void XmlOptions::Set(LPCTSTR subkey, LPCTSTR value, bool bVal)
{
	if (!m_loaded)
	{
		load();
	}

	subkey = m_groupLocked ? (m_group.c_str()) : subkey;

	tstring sVal(bVal ? _T("true") : _T("false"));
	map_type::iterator i = m_options.find((tstring(subkey) + _T(".")) + value);
	if (i != m_options.end())
	{
		(*i).second.swap(sVal);
	}
	else
	{
		m_options.insert(map_type::value_type((tstring(subkey) + _T(".")) + value, sVal));
	}
}

void XmlOptions::Set(LPCTSTR subkey, LPCTSTR value, int iVal)
{
	if (!m_loaded)
	{
		load();
	}

	subkey = m_groupLocked ? (m_group.c_str()) : subkey;

	TCHAR cbuf[40];
	_itot(iVal, cbuf, 10);

	map_type::iterator i = m_options.find((tstring(subkey) + _T(".")) + value);
	if (i != m_options.end())
	{
		(*i).second = cbuf;
	}
	else
	{
		m_options.insert(map_type::value_type((tstring(subkey) + _T(".")) + value, cbuf));
	}
}

void XmlOptions::Set(LPCTSTR subkey, LPCTSTR value, uint64_t iVal)
{
	if (!m_loaded)
	{
		load();
	}

	subkey = m_groupLocked ? (m_group.c_str()) : subkey;

	TCHAR cbuf[70];
	_ui64tot(iVal, cbuf, 10);

	map_type::iterator i = m_options.find((tstring(subkey) + _T(".")) + value);
	if (i != m_options.end())
	{
		(*i).second = cbuf;
	}
	else
	{
		m_options.insert(map_type::value_type((tstring(subkey) + _T(".")) + value, cbuf));
	}
}

void XmlOptions::Set(LPCTSTR subkey, LPCTSTR value, LPCTSTR szVal)
{
	if (!m_loaded)
	{
		load();
	}

	subkey = m_groupLocked ? (m_group.c_str()) : subkey;

	map_type::iterator i = m_options.find((tstring(subkey) + _T(".")) + value);
	if (i != m_options.end())
	{
		(*i).second = szVal;
	}
	else
	{
		m_options.insert(map_type::value_type((tstring(subkey) + _T(".")) + value, szVal));
	}
}

bool XmlOptions::Get(LPCTSTR subkey, LPCTSTR value, bool bDefault)
{
	if (!m_loaded)
	{
		load();
	}

	subkey = m_groupLocked ? (m_group.c_str()) : subkey;

	map_type::const_iterator it = m_options.find((tstring(subkey) + _T(".")) + value);
	if (it != m_options.end() && (*it).second.size())
	{
		return (*it).second[0] == 't' || (*it).second[0] == 'T';
	}

	return bDefault;
}

int XmlOptions::Get(LPCTSTR subkey, LPCTSTR value, int iDefault)
{
	if (!m_loaded)
	{
		load();
	}

	subkey = m_groupLocked ? (m_group.c_str()) : subkey;

	map_type::const_iterator it = m_options.find((tstring(subkey) + _T(".")) + value);
	if (it != m_options.end())
	{
		return _ttoi((*it).second.c_str());
	}	

	return iDefault;
}

uint64_t XmlOptions::Get(LPCTSTR subkey, LPCTSTR value, uint64_t iDefault)
{
	if (!m_loaded)
	{
		load();
	}

	subkey = m_groupLocked ? (m_group.c_str()) : subkey;

	map_type::const_iterator it = m_options.find((tstring(subkey) + _T(".")) + value);
	if (it != m_options.end())
	{
		TCHAR* end(NULL);
		return _tcstoui64((*it).second.c_str(), &end, 10);
	}

	return iDefault;
}

tstring XmlOptions::Get(LPCTSTR subkey, LPCTSTR value, LPCTSTR szDefault)
{
	if (!m_loaded)
	{
		load();
	}

	subkey = m_groupLocked ? (m_group.c_str()) : subkey;

	map_type::const_iterator it = m_options.find((tstring(subkey) + _T(".")) + value);
	if (it != m_options.end())
	{
		return (*it).second;
	}

	return szDefault;
}

void XmlOptions::Clear(LPCTSTR subkey)
{
}

void XmlOptions::SetUserSettingsPath(LPCTSTR path)
{
	Options::SetUserSettingsPath(path);
	GetPNPath(m_userSettingsPath, PNPATH_USERSETTINGS);
	m_userSettingsPath += _T("UserSettings.xml");
	m_loaded = false;
	m_options.clear();
}

void XmlOptions::group(LPCTSTR location)
{
	m_group = location;
	m_groupLocked = true;
}

void XmlOptions::ungroup()
{
	m_groupLocked = false;
}

void XmlOptions::load()
{
	XMLParser parser;
	parser.SetParseState(this);
	parser.LoadFile(m_userSettingsPath.c_str());
	m_loaded = true;
}

void XmlOptions::save()
{
	OptionsXmlWriter writer;
	if (!writer.Start(m_userSettingsPath.c_str()))
	{
		UNEXPECTED(_T("Failed to open user settings xml file for writing."));
		return;
	}

	writer.StartConfig();

	for(map_type::const_iterator i = m_options.begin(); i != m_options.end(); ++i)
	{
		writer.WriteOption((*i).first.c_str(), (*i).second.c_str());
	}

	writer.EndConfig();
	writer.Close();
}

void XmlOptions::startElement(LPCTSTR name, const XMLAttributes& atts)
{
	if (*name != NULL && *name == _T('o') && *(name+1) == NULL)
	{
		m_element = atts.getValue(0);
	}
}

void XmlOptions::endElement(LPCTSTR name)
{
	m_options.insert(map_type::value_type(m_element, m_value));
	m_value.clear();
}

void XmlOptions::characterData(LPCTSTR data, int len)
{
	m_value.append(data, len);
}