//////////////////////////////////////////////////////////////////////
//
// ShellBrowser.cpp: implementation of the CShellBrowser class.
//

#include "stdafx.h"
#include "BrowseForFolder.h"

//////////////////////////////////////////////////////////////////////
//
// Construction/Destruction
//

CCustomBrowseForFolder::CCustomBrowseForFolder(const HWND hParent /*= NULL*/, const LPITEMIDLIST pidl /*= NULL*/, const int nTitleID /*= 0*/)
{
	m_hwnd = NULL;
	SetOwner(hParent);
	SetRoot(pidl);
	SetTitle(nTitleID);
	m_bi.lpfn = BrowseCallbackProc;
	m_bi.lParam = reinterpret_cast<long>(this);
	m_bi.pszDisplayName = m_szSelected;
}

CCustomBrowseForFolder::CCustomBrowseForFolder(const HWND hParent, const LPITEMIDLIST pidl, const CString& strTitle)
{
	m_hwnd = NULL;
	SetOwner(hParent);
	SetRoot(pidl);
	SetTitle(strTitle);
	m_bi.lpfn = BrowseCallbackProc;
	m_bi.lParam = reinterpret_cast<long>(this);
	m_bi.pszDisplayName = m_szSelected;
}

CCustomBrowseForFolder::~CCustomBrowseForFolder()
{

}

//////////////////////////////////////////////////////////////////////
//
// Implementation
//

void CCustomBrowseForFolder::SetOwner(const HWND hwndOwner)
{
	if (m_hwnd != NULL)
		return;

	m_bi.hwndOwner = hwndOwner;
}

void CCustomBrowseForFolder::SetRoot(const LPITEMIDLIST pidl)
{
	if (m_hwnd != NULL)
		return;

	m_bi.pidlRoot = pidl;
}

CString CCustomBrowseForFolder::GetTitle() const
{
	return m_bi.lpszTitle;
}

void CCustomBrowseForFolder::SetTitle(const CString& strTitle)
{
	if (m_hwnd != NULL)
		return;

	m_pchTitle = std::auto_ptr<char>(new char [static_cast<size_t>(strTitle.GetLength()) + 1]);
	strcpy(m_pchTitle.get(), strTitle);
	m_bi.lpszTitle = m_pchTitle.get();
}

bool CCustomBrowseForFolder::SetTitle(const int nTitle)
{
	if (nTitle <= 0)
		return false;

	CString strTitle;
	if(!strTitle.LoadString(static_cast<size_t>(nTitle)))
	{
		return false;
	}
	SetTitle(strTitle);
	return true;
}

void CCustomBrowseForFolder::SetFlags(const UINT ulFlags)
{
	if (m_hwnd != NULL)
		return;

	m_bi.ulFlags = ulFlags;
}

CString CCustomBrowseForFolder::GetSelectedFolder() const
{
	return m_szSelected;
}

bool CCustomBrowseForFolder::SelectFolder()
{
	bool bRet = false;

	LPITEMIDLIST pidl;
	if ((pidl = ::SHBrowseForFolder(&m_bi)) != NULL)
	{
		m_strPath.Empty();
		if (SUCCEEDED(::SHGetPathFromIDList(pidl, m_szSelected)))
		{
			bRet = true;
			m_strPath = m_szSelected;
		}

		LPMALLOC pMalloc;
		//Retrieve a pointer to the shell's IMalloc interface
		if (SUCCEEDED(SHGetMalloc(&pMalloc)))
		{
			// free the PIDL that SHBrowseForFolder returned to us.
			pMalloc->Free(pidl);
			// release the shell's IMalloc interface
			(void)pMalloc->Release();
		}
	}
	m_hwnd = NULL;

	return bRet;
}

void CCustomBrowseForFolder::OnInit() const
{

}

void CCustomBrowseForFolder::OnSelChanged(const LPITEMIDLIST pidl) const
{
	(void)pidl;
}

void CCustomBrowseForFolder::EnableOK(const bool bEnable) const
{
	if (m_hwnd == NULL)
		return;

	(void)SendMessage(m_hwnd, BFFM_ENABLEOK, static_cast<WPARAM>(bEnable), NULL);
}

void CCustomBrowseForFolder::SetSelection(const LPITEMIDLIST pidl) const
{
	if (m_hwnd == NULL)
		return;

	(void)SendMessage(m_hwnd, BFFM_SETSELECTION, FALSE, reinterpret_cast<long>(pidl));
}

void CCustomBrowseForFolder::SetSelection(const CString& strPath) const
{
	if (m_hwnd == NULL)
		return;

	(void)SendMessage(m_hwnd, BFFM_SETSELECTION, TRUE, reinterpret_cast<long>(LPCTSTR(strPath)));
}

void CCustomBrowseForFolder::SetStatusText(const CString& strText) const
{
	if (m_hwnd == NULL)
		return;

	(void)SendMessage(m_hwnd, BFFM_SETSTATUSTEXT, NULL, reinterpret_cast<long>(LPCTSTR(strText)));
}

int __stdcall CCustomBrowseForFolder::BrowseCallbackProc(HWND hwnd, UINT uMsg, LPARAM lParam, LPARAM lpData)
{
	CCustomBrowseForFolder* pbff = reinterpret_cast<CCustomBrowseForFolder*>(lpData);
	pbff->m_hwnd = hwnd;
	if (uMsg == BFFM_INITIALIZED)
		pbff->OnInit();
	else if (uMsg == BFFM_SELCHANGED)
		pbff->OnSelChanged(reinterpret_cast<LPITEMIDLIST>(lParam));
	
	return 0;
}

CBrowseForFolder::CBrowseForFolder(const HWND hParent, LPCTSTR title) 
	: CCustomBrowseForFolder(hParent, NULL, title)
{

}

CBrowseForFolder::~CBrowseForFolder()
{

}

void CBrowseForFolder::SetInitialSelection(LPCTSTR Path)
{
	m_csInitialSelection = Path;
}

void CBrowseForFolder::OnInit() const
{
	SetSelection(m_csInitialSelection);
	SetStatusText(m_csInitialSelection);
}

void CBrowseForFolder::OnSelChanged(const LPITEMIDLIST pidl) const
{
	CString strBuffer;
	if (SHGetPathFromIDList(pidl, strBuffer.GetBuffer(MAX_PATH)))
		strBuffer.ReleaseBuffer();
	else
		strBuffer.ReleaseBuffer(0);
	SetStatusText(strBuffer);
}