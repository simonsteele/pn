/**
 * @file RegistryOptions.h
 * @brief Registry configuration functionality.
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

// Predecs
class RegAccess;
namespace ssreg
{
	class CSRegistry;
}

class RegistryOptions : public Options
{
	friend class RegAccess;
	friend class OptionsFactory;

public:
	virtual ~RegistryOptions();

	virtual void Set(LPCTSTR subkey, LPCTSTR value, bool bVal);
	virtual void Set(LPCTSTR subkey, LPCTSTR value, int iVal);
	virtual void Set(LPCTSTR subkey, LPCTSTR value, uint64_t iVal);
	virtual void Set(LPCTSTR subkey, LPCTSTR value, LPCTSTR szVal);

	virtual bool Get(LPCTSTR subkey, LPCTSTR value, bool bDefault);
	virtual int Get(LPCTSTR subkey, LPCTSTR value, int iDefault);
	virtual uint64_t Get(LPCTSTR subkey, LPCTSTR value, uint64_t iDefault);
	virtual tstring Get(LPCTSTR subkey, LPCTSTR value, LPCTSTR szDefault);

	virtual void Clear(LPCTSTR subkey);

protected:
	RegistryOptions();

	virtual void group(LPCTSTR location);
	virtual void ungroup();
	
	void open(LPCTSTR location);
	void close();
	
	bool groupLocked;
	
	ssreg::CSRegistry* _preg;
};