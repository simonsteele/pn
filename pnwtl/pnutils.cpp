#include "stdafx.h"
#include "pnutils.h"

///////////////////////////////////////////////////////////////
// CMRUList
///////////////////////////////////////////////////////////////

CMRUList::CMRUList(int size)
{
	SetSize(size);
}

void CMRUList::SetSize(int size)
{
	m_iMaxSize = size;
	Resize();
}

void CMRUList::AddEntry(LPCTSTR data)
{
	// Set up an _entry
	_entry e;
	e.pszData = new TCHAR[_tcslen(data)+1];
	_tcscpy(e.pszData, data);

	int f = m_entries.Find(e);
	if(f != -1)
	{
		m_entries.RemoveAt(f);
	}

	if(m_entries.GetSize() == m_iMaxSize)
		m_entries.RemoveAt(0);

	BOOL bRet = m_entries.Add(e);
}

void CMRUList::Resize()
{
	if(m_iMaxSize > m_entries.GetSize())
	{
		int nTooMany = m_entries.GetSize() - m_iMaxSize;
		for(int i = 0; i < nTooMany; i++)
		{
			m_entries.RemoveAt(0);
		}
	}
}

///////////////////////////////////////////////////////////////
// CMRUMenu
///////////////////////////////////////////////////////////////

CMRUMenu::CMRUMenu(UINT baseCmd, int size) : CMRUList(size)
{
	m_iBase = baseCmd;
	m_szEmpty = new TCHAR[_tcslen(_T("(empty)"))+1];
	_tcscpy(m_szEmpty, _T("(empty)"));
}

CMRUMenu::~CMRUMenu()
{
	if(m_szEmpty)
		delete [] m_szEmpty;
}

void CMRUMenu::UpdateMenu()
{
	CSMenuHandle m = m_Menu.GetHandle();
	TCHAR szBuf[50];
	TCHAR szItemText[50+6]; // add space for &, 2 digits, and a space

	UINT id;
	int insertPoint = 0;
	int offset = 0;

	if(m.GetCount() != 0)
	{
		for(int i = m.GetCount() - 1; i >= 0; i--)
		{
			id = ::GetMenuItemID(m, i);
			if( (id >= m_iBase+1) && (id <= (m_iBase + m_iMaxSize)) )
			{
				::RemoveMenu(m, i, MF_BYPOSITION);
			}
			if(id == m_iBase)
				insertPoint = i;
		}
	}
	else
	{
		m.AddItem(m_szEmpty, m_iBase);
	}

	int nSize = m_entries.GetSize();

	if(nSize > 0)
	{
		for(offset = 0; offset < m_entries.GetSize(); offset++)
		{
			_entry& e = m_entries[nSize - 1 - offset];
			AtlCompactPath(szBuf, e.pszData, 40);
			wsprintf(szItemText, _T("&%i %s"), offset + 1, szBuf);
			::InsertMenu(m, insertPoint + offset, MF_BYPOSITION | MF_STRING, m_iBase + offset, szItemText);
		}
	}
	else
	{
		::InsertMenu(m, insertPoint, MF_BYPOSITION | MF_STRING, m_iBase, m_szEmpty);
		::EnableMenuItem(m, m_iBase, MF_GRAYED);
		offset += 1;
	}
	::DeleteMenu(m, insertPoint + offset, MF_BYPOSITION);
}

CMRUMenu::operator HMENU()
{
	return (HMENU)m_Menu;
}