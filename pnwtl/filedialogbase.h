/**
 * @file filedialogbase.h
 * @brief Interfaces to be implemented by file dialogs
 * @author Simon Steele
 * @note Copyright (c) 2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef filedialogbase_h__included
#define filedialogbase_h__included

class IFileDialogBase
{
public:
	virtual ~IFileDialogBase(){}
	virtual INT_PTR DoModal(HWND hWndParent = ::GetActiveWindow()) = 0;

	virtual LPCTSTR GetSingleFileName() = 0;

	virtual void SetTitle(LPCTSTR title) = 0;
	virtual void SetInitialPath(LPCTSTR initial) = 0;
};

class IFileOpenDialogBase : public IFileDialogBase
{
public:
	virtual ~IFileOpenDialogBase(){}
	virtual void SetAllowMultiSelect(bool allow) = 0;

	typedef std::vector<tstring>::const_iterator const_iterator;

	virtual const_iterator begin() = 0;
	virtual const_iterator end() = 0;
};

class IFileSaveDialogBase : public IFileDialogBase
{
public:
	virtual ~IFileSaveDialogBase(){}
	
	virtual void SetDefaultExtension(LPCTSTR ext) = 0;

	virtual void SetInitialFilename(LPCTSTR initial) = 0;
};

class IHasEncoding
{
public:
	virtual ~IHasEncoding(){}
	virtual EPNEncoding GetEncoding() = 0;
};

class IHasSaveFormat
{
public:
	virtual ~IHasSaveFormat(){}
	virtual EPNSaveFormat GetSaveFormat() = 0;
};

#endif // #ifndef filedialogbase_h__included