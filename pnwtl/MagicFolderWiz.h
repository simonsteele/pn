/**
 * @file magicfolderwiz.h
 * @brief Wizard to create magic folders
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef magicfolderwiz_h__included
#define magicfolderwiz_h__included

//class CBrowseTree;
class CShellTreeCtrl;

class MagicFolderWizard1 : public CPropertyPageImpl<MagicFolderWizard1>
{
	typedef CPropertyPageImpl<MagicFolderWizard1> baseClass;
	friend class baseClass;
public:
	MagicFolderWizard1();
	~MagicFolderWizard1();

	enum { IDD = IDD_MAGICFOLDERWIZ1 };

	BEGIN_MSG_MAP(MagicFolderWizard1)
        MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
		NOTIFY_HANDLER(IDC_SHELLTREE, TVN_SELCHANGED, OnSelChanged)
        CHAIN_MSG_MAP(baseClass)
		REFLECT_NOTIFICATIONS()
    END_MSG_MAP()

	LPCTSTR GetSelFolder() const;

protected:
	LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
	//BOOL OnApply();
	int OnSetActive();

protected:
	CString selFolder;
	CShellTreeCtrl* shelltree;
};

class MagicFolderWizard2 : public CPropertyPageImpl<MagicFolderWizard2>
{
	typedef CPropertyPageImpl<MagicFolderWizard2> baseClass;
	friend class baseClass;
public:
	MagicFolderWizard2();

	enum { IDD = IDD_MAGICFOLDERWIZ2 };

	BEGIN_MSG_MAP(MagicFolderWizard2)
		MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
		CHAIN_MSG_MAP(baseClass);
	END_MSG_MAP()

	LPCTSTR GetFileFilter() const;
	LPCTSTR GetExcludedFileFiler() const;
	LPCTSTR GetFolderFilter() const;	

protected:
	LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	int OnSetActive();

	int OnWizardFinish();

protected:
	CString strFileFilter;
	CString strExcludedFileFilter;
	CString strFolderFilter;
};

#endif