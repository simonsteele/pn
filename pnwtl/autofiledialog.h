/**
 * @file autofiledialog.h
 * @brief File dialogs that automatically work best on pre or post Vista
 * @author Simon Steele
 * @note Copyright (c) 2008-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef autofiledialog_h__included
#define autofiledialog_h__included

template <class TClassicDialog, class TVistaDialog, class TBase>
class CAdvancedDialogImpl
{
public:
	CAdvancedDialogImpl(LPCTSTR filter)
	{
		if (WTL::RunTimeHelper::IsVista())
		{
			CT2CW filterw(filter);
			m_dialog = new TVistaDialog(filterw);
		}
		else
		{
			m_dialog = new TClassicDialog(filter, m_path.size() ? m_path.c_str() : NULL);
		}
	}

	~CAdvancedDialogImpl()
	{
		if (m_dialog != NULL)
		{
			delete m_dialog;
		}
	}

	INT_PTR DoModal(HWND hWndParent = ::GetActiveWindow())
	{
		PNASSERT(m_dialog != NULL);
		return m_dialog->DoModal(hWndParent);
	}

	LPCTSTR GetSingleFileName()
	{
		PNASSERT(m_dialog != NULL);
		return m_dialog->GetSingleFileName();
	}

	void SetTitle(LPCTSTR title)
	{
		PNASSERT(m_dialog != NULL);
		return m_dialog->SetTitle(title);
	}

	void SetInitialPath(LPCTSTR path)
	{
		PNASSERT(m_dialog != NULL);
		return m_dialog->SetInitialPath(path);
	}

protected:
	tstring m_path;
	TBase* m_dialog;
};

template <class TClassicDialog, class TVistaDialog>
class CAutoOpenDialogImpl : public CAdvancedDialogImpl<TClassicDialog, TVistaDialog, IFileOpenDialogBase>
{
	typedef CAdvancedDialogImpl<TClassicDialog, TVistaDialog, IFileOpenDialogBase> TBase;
public:
	CAutoOpenDialogImpl(LPCTSTR filter) : TBase(filter)
	{
	}
	
	void SetAllowMultiSelect(bool allow)
	{
		PNASSERT(m_dialog != NULL);
		return m_dialog->SetAllowMultiSelect(allow);
	}

	IFileOpenDialogBase::const_iterator begin()
	{
		PNASSERT(m_dialog != NULL);
		return m_dialog->begin();
	}

	IFileOpenDialogBase::const_iterator end()
	{
		PNASSERT(m_dialog != NULL);
		return m_dialog->end();
	}
};

template <class TClassicDialog, class TVistaDialog>
class CAutoSaveDialogImpl : public CAdvancedDialogImpl<TClassicDialog, TVistaDialog, IFileSaveDialogBase>
{
	typedef CAdvancedDialogImpl<TClassicDialog, TVistaDialog, IFileSaveDialogBase> TBase;
public:
	CAutoSaveDialogImpl(LPCTSTR filter) : TBase(filter)
	{
	}

	virtual void SetDefaultExtension(LPCTSTR ext)
	{
		PNASSERT(m_dialog != NULL);
		m_dialog->SetDefaultExtension(ext);
	}

	virtual void SetInitialFilename(LPCTSTR initial)
	{
		PNASSERT(m_dialog != NULL);
		m_dialog->SetInitialFilename(initial);
	}
};

typedef CAutoOpenDialogImpl<CPNOpenDialog, CVistaOpenDialog> CAutoOpenDialog;
typedef CAutoSaveDialogImpl<CPNSaveDialog, CVistaSaveDialog> CAutoSaveDialog;

class  CAutoOpenDialogEx : public CAutoOpenDialogImpl<CPNOpenDialogEx, CVistaOpenDialogEx>
{
public:
	CAutoOpenDialogEx(LPCTSTR filter) : CAutoOpenDialogImpl<CPNOpenDialogEx, CVistaOpenDialogEx>(filter)
	{
	}

	EPNEncoding GetEncoding()
	{
		PNASSERT(m_dialog != NULL);
		if (WTL::RunTimeHelper::IsVista())
		{
			CVistaOpenDialogEx* exdlg = static_cast<CVistaOpenDialogEx*>(m_dialog);
			IHasEncoding* dlg = static_cast<IHasEncoding*>(exdlg);
			return dlg->GetEncoding();
		}
		else
		{
			CPNOpenDialogEx* exdlg = static_cast<CPNOpenDialogEx*>(m_dialog);
			IHasEncoding* dlg = static_cast<IHasEncoding*>(exdlg);
			return dlg->GetEncoding();
		}
	}
};

class CAutoSaveDialogEx : public CAutoSaveDialogImpl<CPNSaveDialogEx, CVistaSaveDialogEx>
{
public:
	CAutoSaveDialogEx(LPCTSTR filter) : CAutoSaveDialogImpl<CPNSaveDialogEx, CVistaSaveDialogEx>(filter)
	{
	}

	EPNSaveFormat GetSaveFormat()
	{
		PNASSERT(m_dialog != NULL);
		if (WTL::RunTimeHelper::IsVista())
		{
			CVistaSaveDialogEx* exdlg = static_cast<CVistaSaveDialogEx*>(m_dialog);
			IHasSaveFormat* dlg = static_cast<IHasSaveFormat*>(exdlg);
			return dlg->GetSaveFormat();
		}
		else
		{
			CPNSaveDialogEx* exdlg = static_cast<CPNSaveDialogEx*>(m_dialog);
			IHasSaveFormat* dlg = static_cast<IHasSaveFormat*>(exdlg);
			return dlg->GetSaveFormat();
		}
	}
};

#endif // #ifndef autofiledialog_h__included