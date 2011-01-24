/**
 * @file pnutils.cpp
 * @brief Utility classes implementation.
 * @author Simon Steele
 * @note Copyright (c) 2002-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "pnutils.h"

#include "ssreg.h"
using namespace ssreg;

#include "include/sscontainers.h"

BOOL PNCenterWindow(HWND hWnd, HWND hWndCenter) throw()
{
	ATLASSERT(::IsWindow(hWnd));

	// determine owner window to center against
	DWORD dwStyle = (DWORD)::GetWindowLong(hWnd, GWL_STYLE);//GetStyle();
	if(hWndCenter == NULL)
	{
		if(dwStyle & WS_CHILD)
			hWndCenter = ::GetParent(hWnd);
		else
			hWndCenter = ::GetWindow(hWnd, GW_OWNER);
	}

	// get coordinates of the window relative to its parent
	RECT rcDlg;
	::GetWindowRect(hWnd, &rcDlg);
	RECT rcArea;
	RECT rcCenter;
	HWND hWndParent;
	if(!(dwStyle & WS_CHILD))
	{
		// don't center against invisible or minimized windows
		if(hWndCenter != NULL)
		{
			DWORD dwStyleCenter = ::GetWindowLong(hWndCenter, GWL_STYLE);
			if(!(dwStyleCenter & WS_VISIBLE) || (dwStyleCenter & WS_MINIMIZE))
				hWndCenter = NULL;
		}
		
		// We get an area to ensure the window sits within it...
		if(g_Context.OSVersion.dwMajorVersion >= 5) // support multiple monitors on 2k+
		{
			rcArea.top = ::GetSystemMetrics(SM_YVIRTUALSCREEN);
			rcArea.left = ::GetSystemMetrics(SM_XVIRTUALSCREEN);
			rcArea.right = rcArea.left + ::GetSystemMetrics(SM_CXVIRTUALSCREEN);
			rcArea.bottom = rcArea.top + ::GetSystemMetrics(SM_CYVIRTUALSCREEN);
		}
		else
		{
			// On older systems we rely on GetWorkArea which doesn't support 
			// multiple monitors.
			::SystemParametersInfo(SPI_GETWORKAREA, NULL, &rcArea, NULL);
		}
		
		if(hWndCenter == NULL)
			// center within screen coordinates
			rcCenter = rcArea;
		else
			::GetWindowRect(hWndCenter, &rcCenter);
	}
	else
	{
		// center within parent client coordinates
		hWndParent = ::GetParent(hWnd);
		ATLASSERT(::IsWindow(hWndParent));

		::GetClientRect(hWndParent, &rcArea);
		ATLASSERT(::IsWindow(hWndCenter));
		::GetClientRect(hWndCenter, &rcCenter);
		::MapWindowPoints(hWndCenter, hWndParent, (POINT*)&rcCenter, 2);
	}

	int DlgWidth = rcDlg.right - rcDlg.left;
	int DlgHeight = rcDlg.bottom - rcDlg.top;

	// find dialog's upper left based on rcCenter
	int xLeft = (rcCenter.left + rcCenter.right) / 2 - DlgWidth / 2;
	int yTop = (rcCenter.top + rcCenter.bottom) / 2 - DlgHeight / 2;

	// if the dialog is outside the screen, move it inside
	if(xLeft < rcArea.left)
		xLeft = rcArea.left;
	else if(xLeft + DlgWidth > rcArea.right)
		xLeft = rcArea.right - DlgWidth;

	if(yTop < rcArea.top)
		yTop = rcArea.top;
	else if(yTop + DlgHeight > rcArea.bottom)
		yTop = rcArea.bottom - DlgHeight;

	// map screen coordinates to child coordinates
	return ::SetWindowPos(hWnd, NULL, xLeft, yTop, -1, -1,
		SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
}

///////////////////////////////////////////////////////////////
// CMRUList
///////////////////////////////////////////////////////////////

CMRUList::CMRUList(int size)
{
	SetSize(size);
}

CMRUList::~CMRUList()
{
	
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

bool CMRUList::MoveToTop(int index)
{
	if(index >= 0 && index < m_entries.GetSize())
	{
		_entry e(m_entries[index]);
		m_entries.RemoveAt(index);
		return (m_entries.Add(e) != FALSE);
	}

	return false;
}

bool CMRUList::RemoveEntry(int index)
{
	return (m_entries.RemoveAt(index) != FALSE);
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

void CMRUList::Save(extensions::IOptions* options, LPCTSTR key)
{
	options->BeginGroupOperation(key);
	
	TCHAR		buf[3];
	int size = m_entries.GetSize();
	
	options->Set(NULL, _T("Number"), size);

	for(int i = 0; i < size; i++)
	{
		_itot(i, buf, 10);
		options->Set(NULL, buf, m_entries[i].pszData);
	}

	options->EndGroupOperation();
}

void CMRUList::Load(extensions::IOptions* options, LPCTSTR key)
{
	options->BeginGroupOperation(key);

	TCHAR		buf[3];
	tstring		valbuf;

	int size = options->Get(NULL, _T("Number"), 0);
	for(int i = 0; i < size; i++)
	{
		_itot(i, buf, 10);
		const TCHAR* opt = options->GetS(NULL, buf, _T(""));
		AddEntry( opt );
		delete [] opt;
	}

	options->EndGroupOperation();
}

int CMRUList::GetCount()
{
	return m_entries.GetSize();
}


///////////////////////////////////////////////////////////////
// CMRUMenu
///////////////////////////////////////////////////////////////

CMRUMenu::CMRUMenu(UINT baseCmd, int size) : CMRUList(size), m_strEmpty(LS(IDS_EMPTY))
{
	m_iBase = baseCmd;
}

CMRUMenu::~CMRUMenu()
{
}

#define MRUMENU_MAXCHARS 96

void CMRUMenu::UpdateMenu()
{
	CSMenuHandle m(m_Menu.GetHandle());

	UINT id;
	int insertPoint = 0;
	int offset = 0;

	UINT maxChars = 96;
	TCHAR szBuf[MRUMENU_MAXCHARS];
	TCHAR szItemText[MRUMENU_MAXCHARS+6]; // add space for &, 2 digits, and a space
	
	int num = m.GetCount();
	if (num != 0)
	{
		for(int i = num - 1; i >= 0; i--)
		{
			id = m.GetItemID(i);
			if( (id >= m_iBase+1) && (id <= (m_iBase + num)) )
			{
				m.RemoveItemByPosition(i);
			}

			if(id == m_iBase + (num - 1))
			{
				insertPoint = i;
			}
		}
	}
	else
	{
		m.AddItem(m_strEmpty.c_str(), m_iBase);
	}

	int nSize = m_entries.GetSize();

	if (nSize > 0)
	{
		for(offset = 0; offset < m_entries.GetSize(); offset++)
		{
			int co = nSize - 1 - offset;
			_entry& e = m_entries[co];

			// Fixed length strings...
			AtlCompactPath(szBuf, e.pszData, maxChars);
			wsprintf(szItemText, _T("&%i %s"), offset + 1, szBuf);
			m.InsertItemAtPosition(szItemText, m_iBase + co, insertPoint + offset);
			// ::InsertMenu(m, insertPoint + offset, MF_BYPOSITION | MF_STRING, m_iBase + co, szItemText);
		}
	}
	else
	{
		m.InsertItemAtPosition(m_strEmpty.c_str(), m_iBase, insertPoint, false, false, false);
		offset++;
	}

	m.RemoveItemByPosition(insertPoint + offset);
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

void CMRUMenu::RemoveEntry(int index)
{
	if(CMRUList::RemoveEntry(index))
		UpdateMenu();
}

void CMRUMenu::MoveToTop(int index)
{
	if(CMRUList::MoveToTop(index))
		UpdateMenu();
}

void XMLSafeString(LPCTSTR from, tstring& to)
{
	int len = _tcslen(from);

	for(int i = 0; i < len; i++)
	{
		switch(from[i])
		{
			case _T('"'):
				to += _T("&quot;");
				break;
			case _T('<'):
				to += _T("&lt;");
				break;
			case _T('>'):
				to += _T("&gt;");
				break;
			case _T('&'):
				to += _T("&amp;");
				break;
			case _T('\''):
				to += _T("&apos;");
				break;
			default:
				to += from[i];
		}
	}
}

void XMLSafeString(LPCSTR from, std::string& to)
{
	int len = strlen(from);

	for(int i = 0; i < len; i++)
	{
		switch(from[i])
		{
			case '"':
				to += "&quot;";
				break;
			case '<':
				to += "&lt;";
				break;
			case '>':
				to += "&gt;";
				break;
			case '&':
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

///////////////////////////////////////////////////////////////
// DeletionManager
///////////////////////////////////////////////////////////////

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

/**
 * Process the raw command line arguments for the process, making sure
 * all filename arguments are fully-qualified paths.
 */
std::list<tstring> GetCommandLineArgs()
{
	std::list<tstring> params;

	TCHAR curDir[MAX_PATH+1];
	::GetCurrentDirectory(MAX_PATH+1, curDir);

	// Process cmdline params... __argv and __argc in VC++
	for (int i = 1; i < __argc; ++i)
	{
		tstring arg = __wargv[i]; 

		if (arg[0] != _T('/') && arg[0] != _T('-'))
		{
			CFileName fn(arg);
			
			// If it's a relative path, root it and
			// make arg point to it.
			if(fn.IsRelativePath())
			{
				fn.Root(curDir);
				arg = fn.c_str();
			}

			params.insert(params.end(), arg);
		}
		else
		{
			// It's a parameter, we don't want to turn it into
			// a rooted filename

			// Process -1, this means all following args are a single file:
			if (arg == _T("-z"))
			{
				// Most past -z and the next parameter, this will be notepad.exe path:
				i += 2;

				tstring oneparam;
				
				// Add any remaining parameters to oneparam:
				for (; i < __argc; ++i)
				{
					arg = __wargv[i];

					if (oneparam.size())
					{
						oneparam += _T(" ");
					}

					oneparam += arg;
				}

				if (oneparam.size())
				{
					CFileName fn(oneparam);
				
					// If it's a relative path, root it and
					// make arg point to it.
					if(fn.IsRelativePath())
					{
						fn.Root(curDir);
						oneparam = fn.c_str();
					}

					params.insert(params.end(), oneparam);
				}
			}
			else
			{
				params.insert(params.end(), arg);

				if (i < (__argc - 1))
				{
					bool takeNext = ( arg[1] == _T('l') || arg[1] == _T('c') || arg[1] == _T('p') || arg[1] == _T('s') );

					if ( arg[1] == _T('-') )
					{
						takeNext = ( arg[2] == _T('l') || arg[2] == _T('c') || arg[2] == _T('p') || arg[2] == _T('s') );
					}

					if (takeNext)
					{
						arg = __wargv[++i];
						params.insert(params.end(), arg);
					}
				}
			}
		}
	}

	return params;
}

bool IsXPOrLater()
{
	// If v5 then greater than v5.0 else v6 or better
	return (g_Context.OSVersion.dwPlatformId == VER_PLATFORM_WIN32_NT) &&
		(( (g_Context.OSVersion.dwMajorVersion == 5) && (g_Context.OSVersion.dwMinorVersion > 0) ) ||
		 (g_Context.OSVersion.dwMajorVersion >= 6) );
}