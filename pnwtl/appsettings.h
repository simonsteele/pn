/**
 * @file appsettings.h
 * @brief Loading of core application settings
 * @author Simon Steele
 * @note Copyright (c) 2005 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef appsettings_h__included_DF1DF359_1F19_495a_B4B6_BFECC531AB30
#define appsettings_h__included_DF1DF359_1F19_495a_B4B6_BFECC531AB30

class AppSettings : public XMLParseState
{
public:
	AppSettings();

// XMLParseState
public:
	virtual void startElement(LPCTSTR name, XMLAttributes& atts);
	virtual void endElement(LPCTSTR name);
	virtual void characterData(LPCTSTR data, int len);

protected:
	void onUserSettingsPath(XMLAttributes& atts);
	void onStoreType(XMLAttributes& atts);

protected:
	bool	m_bUseIni;
	int		m_parseState;
	tstring m_userPath;
};

#endif // #ifndef appsettings_h__included_DF1DF359_1F19_495a_B4B6_BFECC531AB30