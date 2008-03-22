/**
 * @file OptionsRegistry.h
 * @brief Registry configuration functionality.
 * @author Simon Steele
 * @note Copyright (c) 2004-2008 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "OptionsManager.h"
#include "OptionsRegistry.h"

#include "ssreg.h"
using namespace ssreg;

//////////////////////////////////////////////////////////////////////////////
// RegAccess
//////////////////////////////////////////////////////////////////////////////

/**
 * This helper class opens the registry in a RegistryOptions class unless
 * that class is already working in group mode. It takes care of closing
 * the key when going out of scope.
 */
class RegAccess
{
	public:
		RegAccess(LPCTSTR subkey, RegistryOptions* options)
		{
			if(!options->groupLocked)
			{
				PNASSERT(subkey != NULL);
				options->open(subkey);
			}
			
			_options = options;
		}

		~RegAccess()
		{
			if(!_options->groupLocked)
			{
				_options->_preg->CloseKey();
			}
		}

	protected:
		RegistryOptions* _options;
};

//////////////////////////////////////////////////////////////////////////////
// RegistryOptions
//////////////////////////////////////////////////////////////////////////////

RegistryOptions::RegistryOptions()
{
	groupLocked = false;
	_preg = new CSRegistry();
}

RegistryOptions::~RegistryOptions()
{
	if(_preg)
	{
		delete _preg;
		_preg = NULL;
	}
}

void RegistryOptions::group(LPCTSTR location)
{
	groupLocked = true;
	open(location);
	/*LOG("\n**** GROUP: ");
	LOG(location);
	LOG(" ****");*/
}

void RegistryOptions::open(LPCTSTR location)
{
	tstring root(pnregroot);
	root += location;
	_preg->OpenKey(root.c_str(), true);
}

void RegistryOptions::close()
{
	_preg->CloseKey();
}

void RegistryOptions::ungroup()
{
	close();
	groupLocked = false;
	/*LOG("\n**** UNGROUP****");*/
}

bool RegistryOptions::Get(LPCTSTR subkey, LPCTSTR value, bool bDefault)
{
	RegAccess a(subkey, this);
	
	bool bVal = _preg->ReadBool(value, bDefault);

	/*TCHAR buffer[1024];
	_stprintf(buffer, "\n<%s.%s: %s", subkey, value, bVal ? "true" : "false");
	LOG(buffer);*/

	return bVal;
}

int RegistryOptions::Get(LPCTSTR subkey, LPCTSTR value, int iDefault)
{
	RegAccess a(subkey, this);

	int iVal = _preg->ReadInt(value, iDefault);

	/*TCHAR buffer[1024];
	_stprintf(buffer, "\n<%s.%s: %d", subkey, value, iVal);
	LOG(buffer);*/

	return iVal;
}

uint64_t RegistryOptions::Get(LPCTSTR subkey, LPCTSTR value, uint64_t iDefault)
{
	RegAccess a(subkey, this);

	uint64_t iVal = _preg->ReadUInt64(value, iDefault);

	/*TCHAR buffer[1024];
	_stprintf(buffer, "\n<%s.%s: %I64d", subkey, value, iVal);
	LOG(buffer);*/

	return iVal;
}

tstring RegistryOptions::Get(LPCTSTR subkey, LPCTSTR value, LPCTSTR szDefault)
{
	RegAccess a(subkey, this);

	tstring str;
	if(!_preg->ReadString(value, str))
		str = szDefault;
	
	/*TCHAR buffer[1024];
	_stprintf(buffer, "\n<%s.%s: %s", subkey, value, str.c_str());
	LOG(buffer);*/

	return str;
}

void RegistryOptions::Clear(LPCTSTR subkey)
{
	tstring root(pnregroot);
	root += subkey;
	_preg->DeleteKey(root.c_str());
}

void RegistryOptions::Set(LPCTSTR subkey, LPCTSTR value, bool bVal)
{
	RegAccess a(subkey, this);

	_preg->WriteBool(value, bVal);

	/*TCHAR buffer[1024];
	_stprintf(buffer, "\n>%s.%s: %s", subkey, value, bVal ? "true" : "false");
	LOG(buffer);*/
}

void RegistryOptions::Set(LPCTSTR subkey, LPCTSTR value, int iVal)
{
	RegAccess a(subkey, this);

	_preg->WriteInt(value, iVal);

	/*TCHAR buffer[1024];
	_stprintf(buffer, "\n>%s.%s: %d", subkey, value, iVal);
	LOG(buffer);*/
}

void RegistryOptions::Set(LPCTSTR subkey, LPCTSTR value, uint64_t iVal)
{
	RegAccess a(subkey, this);

	_preg->WriteUInt64(value, iVal);

	/*TCHAR buffer[1024];
	_stprintf(buffer, "\n>%s.%s: %I64d", subkey, value, iVal);
	LOG(buffer);*/
}

void RegistryOptions::Set(LPCTSTR subkey, LPCTSTR value, LPCTSTR szVal)
{
	RegAccess a(subkey, this);

	_preg->WriteString(value, szVal);

	//TCHAR buffer[1024];
	//_stprintf(buffer, "\n>%s.%s: %s", subkey, value, szVal);
	//LOG(buffer);
}