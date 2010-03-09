/**
 * @file newprojectdialog.cpp
 * @brief New Project Dialog
 * @author Simon Steele
 * @note Copyright (c) 2005-2010 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "project.h"
#include "projectprops.h"
#include "projectregistry.h"
#include "CustomAutoComplete.h"
#include "include/accombo.h"
#include "pndialogs.h"
#include "newprojectdialog.h"

static int GetComCtlVersion()
{
	static int ComCtlVersion = 0;

	TCHAR* FileName = _T("comctl32.dll");
	DWORD InfoSize, Dummy = 0;
	BYTE* buf;
	UINT VerSize;
	VS_FIXEDFILEINFO * ffi;

	if(ComCtlVersion == 0)
	{
		// Dummy gets set to 0.
		InfoSize = ::GetFileVersionInfoSize(FileName, &Dummy);
		if(InfoSize > 0)
		{
			buf = new BYTE[InfoSize];
			if (::GetFileVersionInfo(FileName, Dummy, InfoSize, buf))
			{
				if(::VerQueryValue(buf, _T("\\"), (void**)&ffi, &VerSize))
				{
					ComCtlVersion = ffi->dwFileVersionMS;
				}
			}

			delete [] buf;
		}
	}

	return ComCtlVersion;
}

CNewProjectDialog::CNewProjectDialog() : m_editName(this, 1)
{
	const UINT flags[2] = {ILC_COLOR8 | ILC_MASK, ILC_COLOR32 | ILC_MASK};

	m_images = ::ImageList_Create(32, 32, flags[GetComCtlVersion() >= 0x60000], 2, 4);
	
	HICON hPFIcon = ::LoadIcon( _Module.m_hInst, MAKEINTRESOURCE(IDI_PROJECTFOLDER) );
	m_images.AddIcon(hPFIcon);
}

LPCTSTR CNewProjectDialog::GetName() const
{
	return m_name.c_str();
}

LPCTSTR CNewProjectDialog::GetFolder() const
{
	return m_folder.c_str();
}

LPCTSTR CNewProjectDialog::GetTemplateGUID() const
{
	return m_tguid.c_str();
}

LRESULT CNewProjectDialog::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_list.Attach(GetDlgItem(IDC_LIST));
	m_folderCombo.SubclassWindow(GetDlgItem(IDC_FOLDER_COMBO));
	m_editName.SubclassWindow(GetDlgItem(IDC_PROJNAMEEDIT));

	m_list.SetImageList(m_images, LVSIL_NORMAL);

	CString str;
	str.LoadString(IDS_PROJECTS_DEFAULTTEMPLATE);
	m_list.InsertItem(0, str, 0);
	m_list.SetItemData(0, NULL);
	int index = 1;

	// Now we're going to add all the available project templates
	const Projects::TEMPLATE_MAP& templates = Projects::Registry::GetInstance()->GetTemplates();

	tstring path;
	OPTIONS->GetPNPath(path, PNPATH_PROJECTTEMPLATES);

	for(Projects::TEMPLATE_MAP::const_iterator i = templates.begin();
		i != templates.end();
		++i)
	{
		Projects::ProjectTemplate* pTemplate = (*i).second;

		LPCTSTR szName = pTemplate->GetName();
		LPCTSTR szIcon = pTemplate->GetImagePath();
		int icon = 0;

		// If the template has an associated icon we try to
		// load it and set it in the imagelist. Loads standard
		// 32x32 windows icons.
		if(szIcon != NULL && _tcslen(szIcon) > 0)
		{
			CFileName fn(szIcon);
			fn.Root(path.c_str());
			if(FileExists(fn.c_str()))
			{
				HICON hIcon = (HICON)::LoadImage(NULL, fn.c_str(), IMAGE_ICON, 32, 32, LR_LOADFROMFILE);
				if(hIcon)
				{
					icon = m_images.AddIcon(hIcon);
					::DestroyIcon(hIcon);
				}
			}
		}

		// belts and braces
		int addedIndex = m_list.InsertItem(index, szName, icon);
		m_list.SetItemData(addedIndex, reinterpret_cast<DWORD_PTR>( pTemplate ));

		index++;
	}

	m_list.SelectItem(0);

	CenterWindow(GetParent());

	return 0;
}

LRESULT CNewProjectDialog::OnBnClickedBrowse(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CString str;
	m_folderCombo.GetWindowText(str);
	if(!DirExists(str))
		str = _T("");

	CString strTitle;
	strTitle.LoadString(IDS_BROWSEFINDROOT);

	CPNFolderDialog fd(m_hWnd, (LPCTSTR)str, strTitle);
	if(fd.DoModal() == IDOK)
	{
		m_folderCombo.SetWindowText( fd.GetFolderPath() );
	}

	return 0;
}

LRESULT CNewProjectDialog::OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CString strName;
	CString strFolder;
	Projects::ProjectTemplate* pTemplate = NULL;

	CWindow(GetDlgItem(IDC_PROJNAMEEDIT)).GetWindowText(strName);
	m_folderCombo.GetWindowText(strFolder);
	
	DWORD_PTR selData = m_list.GetItemData(m_list.GetSelectedIndex());
	if(selData != NULL)
		pTemplate = reinterpret_cast<Projects::ProjectTemplate*>(selData);

	if(strName.GetLength() > 0 && strFolder.GetLength() > 0)
	{
		if(!::DirExists(strFolder))
		{
			CString str;
			str.Format(IDS_CREATEPROJECTFOLDER, strFolder);
			switch(::MessageBox(m_hWnd, str, LS(IDR_MAINFRAME), MB_YESNOCANCEL | MB_ICONQUESTION))
			{
			case IDYES:
				if(!CreateDirectoryRecursive(strFolder))
				{
					str.Format(IDS_CREATEPROJECTFOLDERFAILED, strFolder);
					::MessageBox(m_hWnd, str, LS(IDR_MAINFRAME), MB_OK | MB_ICONWARNING);
					return 0;
				}
				break;
			case IDNO:
			case IDCANCEL:
				return 0;
			}
		}
		m_name = strName;
		m_folder = strFolder;
		if(pTemplate != NULL)
			m_tguid = pTemplate->GetID();
		
		EndDialog(wID);
	}
	
	return 0;
}

LRESULT CNewProjectDialog::OnCloseCmd(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	EndDialog(wID);

	return 0;
}

LRESULT CNewProjectDialog::OnEditKeyDown(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	switch(wParam)
	{
		case L'\\':
		case L'/':
		case L':':
		case L'*':
		case L'?':
		case L'"':
		case L'<':
		case L'>':
		case L'|':
			break;

		default:
			bHandled = FALSE;
			break;
	}

	return 0;
}