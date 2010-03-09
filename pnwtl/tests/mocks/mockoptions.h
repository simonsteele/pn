#ifndef MOCKOPTIONS_H__INCLUDED
#define MOCKOPTIONS_H__INCLUDED

#include "../../IOptions.h"
#include "../../extiface.h"

#pragma once

class IOptionsWithString : public extensions::IOptions
{
public:
	virtual void GetPNPath(tstring& path, int pathtype = PNPATH_PN) = 0;
};

/**
 * Mock implementation of IOptions
 */
class MockOptions : public IOptionsWithString
{
public:
	/// Set a bool value
	virtual void Set(const wchar_t* subkey, const wchar_t* value, bool bVal) {}
	
	/// Set an int value
	virtual void Set(const wchar_t* subkey, const wchar_t* value, int iVal) {}
	
	/// Set a string value
	virtual void Set(const wchar_t* subkey, const wchar_t* value, const wchar_t* szVal) { }

	/// Get a bool value
	virtual bool Get(const wchar_t* subkey, const wchar_t* value, bool bDefault) { return bDefault; }
	
	/// Get an int value
	virtual int Get(const wchar_t* subkey, const wchar_t* value, int iDefault) { return iDefault; }
	
	/// Get a string (note, you should free this using IPN::ReleaseString from an extension)
	virtual wchar_t* GetS(const wchar_t* subkey, const wchar_t* value, const wchar_t* szDefault) { return _tcsdup(szDefault); }

	virtual void GetPNPath(tstring& path, int pathtype = PNPATH_PN)
	{
		path = GetPNPath(pathtype);
	}

	/// Get a PN path (note, you should free this using IPN::ReleaseString from an extension)
	virtual wchar_t* GetPNPath(int pathtype = PNPATH_PN)
	{
		switch (pathtype)
		{
		case PNPATH_CLIPS:
			return L"Tests\\Clips\\";
		case PNPATH_COMPILEDSCHEMES:
			return L"Tests\\CompiledSchemes\\";
		case PNPATH_PRESETS:
			return L"Tests\\Presets\\";
		case PNPATH_PROJECTTEMPLATES:
			return L"Tests\\Project Templates\\";
		case PNPATH_SCHEMES:
			return L"Tests\\Schemes\\";
		case PNPATH_TAGGERS:
			return L"Tests\\Taggers\\";
		case PNPATH_TOOLS:
			return L"Tests\\Tools\\";
		case PNPATH_USERCLIPS:
			return L"Tests\\User\\";
		/*case PNPATH_USERSETTINGS:
			return L"User\\Settings\\";
		case PNPATH_USERTOOLS:
			return L"User\\Tools\\";*/

		default:
		case PNPATH_PN:
			return L"Tests\\PN\\";
		}
	}

	/// Get a cached option
	virtual int GetCached(ECachedOption option) { return 0; }
	
	/// Set a cached option
	virtual void SetCached(ECachedOption option, int value) { }

	/// Start a group options operation
	virtual void BeginGroupOperation(const wchar_t* subkey) { }
	
	/// End a group options operation
	virtual void EndGroupOperation() { }

	/// Get the global search options cache
	virtual extensions::ISearchOptions* GetSearchOptions() { return NULL; }
};

#endif