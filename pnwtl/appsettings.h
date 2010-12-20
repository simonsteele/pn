/**
 * @file appsettings.h
 * @brief Loading of core application settings
 * @author Simon Steele
 * @note Copyright (c) 2005 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef appsettings_h__included_DF1DF359_1F19_495a_B4B6_BFECC531AB30
#define appsettings_h__included_DF1DF359_1F19_495a_B4B6_BFECC531AB30

class ExtDetails
{
public:
	ExtDetails(LPCTSTR path, LPCTSTR basePath);

	bool Disabled;
	tstring Path;
	tstring FullPath;

	bool Exists() const;
};

typedef std::list<ExtDetails> extlist;

class FileFinderData;

class AppSettings : public XMLParseState
{
public:
	AppSettings();

	OptionsFactory::EOptionsType GetOptionsType() const;
	LPCTSTR GetUserPath() const;
	bool HaveUserPath() const;
	const extlist& GetExtensions() const;

	Options* MakeOptions() const;

	void FindExtensions();

	void Save();

// XMLParseState
public:
	virtual void startElement(XML_CSTR name, const XMLAttributes& atts);
	virtual void endElement(XML_CSTR name);
	virtual void characterData(XML_CSTR data, int len);

protected:
	void findExtensionHandler(LPCTSTR path, FileFinderData& data, bool& /*shouldContinue*/);
	void load();
	void load(const TCHAR* filename);
	void save();
	void onUserSettingsPath(const XMLAttributes& atts);
	void onStoreType(const XMLAttributes& atts);
	void onExtension(const XMLAttributes& atts);

protected:
	bool			m_bUseIni;
	bool			m_bAppSettingsPathSpecified;
	int				m_parseState;
	tstring			m_pnpath;
	tstring			m_userPath;
	extlist			m_extensions;
};

#endif // #ifndef appsettings_h__included_DF1DF359_1F19_495a_B4B6_BFECC531AB30