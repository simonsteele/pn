/**
 * @file pnutils.cpp
 * @brief Utility classes implementation.
 * @author Simon Steele
 * @note Copyright (c) 2002 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "pnutils.h"

#include "ssreg.h"
using namespace ssreg;

///////////////////////////////////////////////////////////////
// CMRUList
///////////////////////////////////////////////////////////////

CMRUList::CMRUList(int size)
{
	SetSize(size);
}

CMRUList::~CMRUList()
{
	if(m_regkey.GetLength() > 0)
	{
		SaveToRegistry();
	}
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

	/*BOOL bRet = */m_entries.Add(e);
}

LPCTSTR CMRUList::GetEntry(int index)
{
	ATLASSERT(index >= 0 && index < m_iMaxSize);
	return m_entries[index].pszData;
}

void CMRUList::Resize()
{
	if(m_iMaxSize < m_entries.GetSize())
	{
		int nTooMany = m_entries.GetSize() - m_iMaxSize;
		for(int i = 0; i < nTooMany; i++)
		{
			m_entries.RemoveAt(0);
		}
	}
}

void CMRUList::SetRegistryKey(LPCTSTR key, bool load)
{
	m_regkey = key;
	if(load)
		LoadFromRegistry();
}

void CMRUList::SaveToRegistry()
{
	ATLASSERT(m_regkey.GetLength() != 0);

	if(m_regkey.GetLength() > 0)
	{
		CSRegistry	reg;
		TCHAR		buf[3];
		int size = m_entries.GetSize();
		
		reg.OpenKey(m_regkey);
		reg.WriteInt(_T("Number"), size);

		for(int i = 0; i < size; i++)
		{
			_itot(i, buf, 10);
			reg.WriteString(buf, m_entries[i].pszData);
		}
	}
}

void CMRUList::LoadFromRegistry()
{
	ATLASSERT(m_regkey.GetLength() != 0);

	if(m_regkey.GetLength() > 0)
	{
		CSRegistry	reg;
		TCHAR		buf[3];
		tstring		valbuf;

		reg.OpenKey(m_regkey);

		int size = reg.ReadInt(_T("Number"));
		for(int i = 0; i < size; i++)
		{
			_itot(i, buf, 10);
			reg.ReadString(buf, valbuf);
			AddEntry(valbuf.c_str());
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

#define MRUMENU_MAXCHARS 96

void CMRUMenu::UpdateMenu()
{
	CSMenuHandle m = m_Menu.GetHandle();
	TCHAR* pszBuf = NULL;
	TCHAR* pszItemText = NULL;

	UINT id;
	int insertPoint = 0;
	int offset = 0;

	UINT maxChars = 96;
	TCHAR szBuf[MRUMENU_MAXCHARS];
	TCHAR szItemText[MRUMENU_MAXCHARS+6]; // add space for &, 2 digits, and a space
	
	int num = m.GetCount();
	if(num != 0)
	{
		for(int i = num - 1; i >= 0; i--)
		{
			id = ::GetMenuItemID(m, i);
			if( (id >= m_iBase+1) && (id <= (m_iBase + num)) )
			{
				::RemoveMenu(m, i, MF_BYPOSITION);
			}
			if(id == m_iBase + (num - 1))
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
			int co = nSize - 1 - offset;
			_entry& e = m_entries[co];

			// Fixed length strings...
			AtlCompactPath(szBuf, e.pszData, maxChars);
			wsprintf(szItemText, _T("&%i %s"), offset + 1, szBuf);
			::InsertMenu(m, insertPoint + offset, MF_BYPOSITION | MF_STRING, m_iBase + co, szItemText);
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

UINT CMRUMenu::base() const
{
	return m_iBase;
}

UINT CMRUMenu::last() const
{
	return m_iBase + m_iMaxSize;
}

void XMLSafeString(LPCTSTR from, tstring& to)
{
	int len = _tcslen(from);

	for(int i = 0; i < len; i++)
	{
		switch(from[i])
		{
			case _T('"'):
				to += "&quot;";
				break;
			case _T('<'):
				to += "&lt;";
				break;
			case _T('>'):
				to += "&gt;";
				break;
			case _T('&'):
				to += "&amp;";
				break;
			case _T('\''):
				to += "&apos;";
				break;
			default:
				to += from[i];
		}
	}
}

void XMLSafeString(tstring& str)
{
	// make an attempt at reducing re-allocs...
	int len = str.size();
	TCHAR * buffer = new TCHAR[len+1];
	_tcscpy(buffer, str.c_str());
	str.reserve(len + 20);
	str = _T("");
	
	XMLSafeString(buffer, str);

	delete [] buffer;
}

/**
 * Register an instance of a DelObject derived class for deletion.
 */
void DeletionManager::Register(DelObject* pObject)
{
	if(!s_pFirst)
	{
		s_pFirst = s_pLast = pObject;
	}
	else
	{
		s_pLast->m_pNextToDelete = pObject;
		s_pLast = pObject;
	}
}

/**
 * Unregister an instance of a DelObject derived class for deletion.
 */
void DeletionManager::UnRegister(DelObject* pObject)
{
	if(!s_pFirst)
		return;
	
	if(pObject == s_pFirst && pObject == s_pLast)
	{
		s_pFirst = s_pLast = NULL;
	}
	else if(pObject == s_pFirst)
	{
		s_pFirst = pObject->m_pNextToDelete;
	}
	else
	{
		DelObject* pObj = s_pFirst;
		while(pObj->m_pNextToDelete != pObject && pObj != NULL)
		{
			pObj = pObj->m_pNextToDelete;
		}

		if(pObj != NULL)
		{
			pObj->m_pNextToDelete = pObject->m_pNextToDelete;
			if(pObject == s_pLast)
				s_pLast = pObj;
		}
	}
}

/**
 * Delete all registered instances.
 */
void DeletionManager::DeleteAll()
{
	DelObject* pObj = s_pFirst;
	DelObject* pNext = NULL;

	while(pObj)
	{
		pNext = pObj->m_pNextToDelete;
		delete pObj;
		pObj = pNext;
	}

	s_pFirst = s_pLast = NULL;
}

DelObject* DeletionManager::s_pFirst = NULL;
DelObject* DeletionManager::s_pLast = NULL;