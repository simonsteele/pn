/**
 * @file OptionsXml.h
 * @brief Xml configuration functionality.
 * @author Simon Steele
 * @note Copyright (c) 2008 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef xmloptions_h__included
#define xmloptions_h__included

class XmlOptions : public Options, private XMLParseState
{
	friend class OptionsFactory;

public:
	virtual ~XmlOptions();

	virtual void Set(LPCTSTR subkey, LPCTSTR value, bool bVal);
	virtual void Set(LPCTSTR subkey, LPCTSTR value, int iVal);
	virtual void Set(LPCTSTR subkey, LPCTSTR value, uint64_t iVal);
	virtual void Set(LPCTSTR subkey, LPCTSTR value, LPCTSTR szVal);

	virtual bool Get(LPCTSTR subkey, LPCTSTR value, bool bDefault);
	virtual int Get(LPCTSTR subkey, LPCTSTR value, int iDefault);
	virtual uint64_t Get(LPCTSTR subkey, LPCTSTR value, uint64_t iDefault);
	virtual tstring Get(LPCTSTR subkey, LPCTSTR value, LPCTSTR szDefault);

	virtual void Clear(LPCTSTR subkey);

	virtual void SetUserSettingsPath(LPCTSTR path);

// XmlParseState
private:
	virtual void startElement(XML_CSTR name, const XMLAttributes& atts);
	virtual void endElement(XML_CSTR name);
	virtual void characterData(XML_CSTR data, int len);

private:
	XmlOptions();

	virtual void group(LPCTSTR location);
	virtual void ungroup();

	void load();
	void save();

	bool m_groupLocked;
	typedef std::map<tstring, tstring> map_type;
	map_type m_options;
	tstring m_group;
	tstring m_userSettingsPath;
	tstring m_element;
	tstring m_value;
	bool m_loaded;
};

#endif