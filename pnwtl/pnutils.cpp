#include "stdafx.h"
#include "pnutils.h"

///////////////////////////////////////////////////////////////
// MRUManager
///////////////////////////////////////////////////////////////

MRUManager::MRUManager(UINT baseCmd, int size)
{
	SetSize(size);
	m_iBase = baseCmd;
}

void MRUManager::SetSize(int size)
{
	m_iMaxSize = size;
	Resize();
}

void MRUManager::UpdateMenu(HMENU hMenu)
{
	CSMenuHandle m(hMenu);
	TCHAR buf[50];

	for(int i = 0; i < m_entries.GetSize(); i++)
	{
		_entry& e = m_entries[i];
		AtlCompactPath(buf, e.pszFilename, 40);
		m.AddItem(buf, m_iBase + i);
	}
}

void MRUManager::AddFile(LPCTSTR filename)
{
	// Set up an _entry
	_entry e;
	e.pszFilename = new TCHAR[_tcslen(filename)+1];
	_tcscpy(e.pszFilename, filename);

	int f = m_entries.Find(e);
	if(f != -1)
	{
		m_entries.RemoveAt(f);
	}

	if(m_entries.GetSize() == m_iMaxSize)
		m_entries.RemoveAt(0);

	BOOL bRet = m_entries.Add(e);
}

void MRUManager::Resize()
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