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
	ExtDetails(const char* path, const char* basePath);

	bool Disabled;
	std::string Path;
	std::string FullPath;

	bool Exists() const;
};

typedef std::list<ExtDetails> extlist;

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
	virtual void startElement(LPCTSTR name, XMLAttributes& atts);
	virtual void endElement(LPCTSTR name);
	virtual void characterData(LPCTSTR data, int len);

protected:
	void findExtensionHandler(LPCTSTR path, LPCTSTR filename);
	void load();
	void save();
	void onUserSettingsPath(XMLAttributes& atts);
	void onStoreType(XMLAttributes& atts);
	void onExtension(XMLAttributes& atts);

protected:
	bool			m_bHuntingTaggers;
	bool			m_bUseIni;
	int				m_parseState;
	tstring			m_pnpath;
	tstring			m_userPath;
	extlist			m_extensions;
};

#endif // #ifndef appsettings_h__included_DF1DF359_1F19_495a_B4B6_BFECC531AB30