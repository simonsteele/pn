/**
 * @file OptionsIni.h
 * @brief Ini configuration functionality.
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef inioptions_h__included
#define inioptions_h__included

class IniKeyMap;

class IniOptions : public Options
{
	friend class OptionsFactory;

public:
	virtual ~IniOptions();

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

protected:
	IniOptions();

	virtual void group(LPCTSTR location);
	virtual void ungroup();

	bool groupLocked;
	IniKeyMap* keyMap;
	TCHAR*	_filename;
	LPCTSTR _group;
};

#endif