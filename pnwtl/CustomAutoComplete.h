//--------------------------------------------------------------------------------------------
//  Name:           CCustomAutoComplete (CCUSTOMAUTOCOMPLETE.H)
//  Type:           Wrapper class
//  Description:    Matches IAutoComplete, IEnumString and the registry (optional) to provide
//					custom auto-complete functionality for EDIT controls - including those in
//					combo boxes - in WTL projects.
//
//  Author:         Klaus H. Probst [kprobst@vbbox.com]
//  URL:            http://www.vbbox.com/
//  Copyright:      This work is copyright © 2002, Klaus H. Probst
//  Usage:          You may use this code as you see fit, provided that you assume all
//                  responsibilities for doing so.
//  Distribution:   Distribute freely as long as you maintain this notice as part of the
//					file header.
//
//
//  Updates:        Simon Steele - http://www.pnotepad.org/
//                     - rewrote for custom storage engines
//					   
//  Notes:			
//
//
//  Dependencies:
//
//					The usual ATL/WTL headers for a normal EXE, plus <atlmisc.h>
//
//--------------------------------------------------------------------------------------------

#if !defined(CCustomAutoComplete_INCLUDED)
#define CCustomAutoComplete_INCLUDED

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

// Bring in the GUID hack and the shell stuff
#include <initguid.h>
#include <shldisp.h>
#include <shlguid.h>

template <class TStorage>
class CCustomAutoComplete : public IEnumString
{
protected:
	typedef std::vector<tstring> TList;
	TList m_asList;
	CComPtr<IAutoComplete> m_pac;

	ULONG m_nCurrentElement;
	ULONG m_nRefCount;
	BOOL m_fBound;
	INT m_nMaxElements;

public:
	// Constructors/destructors

	CCustomAutoComplete()
	{
		InternalInit();
	}

	CCustomAutoComplete(const CSimpleArray<CString>& p_sItemList)
	{
		InternalInit();

		SetList(p_sItemList);
	}
	
	~CCustomAutoComplete()
	{
		if (m_pac)
		{
			m_pac.Release();
		}
	}

public:
	// Implementation

	BOOL SetList(const TList& p_sItemList)
	{	
		ATLASSERT(p_sItemList.size() != 0);
		
		Clear();

		m_asList = p_sItemList;

		return TRUE;
	}

	const TList& GetList()
	{
		return m_asList;
	}

	BOOL Bind(HWND p_hWndEdit, DWORD p_dwOptions = 0, LPCTSTR p_lpszFormatString = NULL)
	{
		ATLASSERT(::IsWindow(p_hWndEdit));

		if ((m_fBound) || (m_pac))
			return FALSE;

		HRESULT hr = S_OK;

		hr = m_pac.CoCreateInstance(CLSID_AutoComplete);

		if (SUCCEEDED(hr))
		{
			if (p_dwOptions)
			{
				CComQIPtr<IAutoComplete2> pAC2(m_pac);
                
				//ATLASSERT(pAC2);
				// IAutoComplete2 only available with shell.dll v5+
				if(pAC2 != NULL)
				{
					hr = pAC2->SetOptions(p_dwOptions);			// This never fails?
					pAC2.Release();
				}
			}

			CT2COLE fms(p_lpszFormatString);
			
			hr = m_pac->Init(p_hWndEdit, this, NULL, fms);

			if (SUCCEEDED(hr))
			{
				m_fBound = TRUE;
				return TRUE;
			}
		}
			
		return FALSE;
	}

	VOID Unbind()
	{
		if (!m_fBound)
			return;

		ATLASSERT(m_pac);

		if (m_pac)
		{
			m_pac.Release();
			m_fBound = FALSE;
		}
	}

	BOOL AddItem(const tstring& p_sItem)
	{
		if (p_sItem.size() != 0)
		{
			if (std::find(m_asList.begin(), m_asList.end(), p_sItem) == m_asList.end())
			{
				if (m_asList.size() == (size_t)m_nMaxElements)
				{
					static_cast<TStorage*>(this)->RemoveFromStorage(m_asList.back());
					m_asList.resize(m_asList.size()-1);
				}

				m_asList.insert(m_asList.begin(), p_sItem);
				
				return static_cast<TStorage*>(this)->AddToStorage(p_sItem);
			}			
		}

		return FALSE;
	}

	BOOL AddItem(LPCTSTR p_lpszItem)
	{
		return AddItem(tstring(p_lpszItem));
	}

	INT GetItemCount() const
	{
		return m_asList.size();
	}

	BOOL RemoveItem(CString& p_sItem)
	{
		if (p_sItem.GetLength() != 0)
		{
			if (m_asList.find(p_sItem) != m_asList.end())
			{
				m_asList.remove(p_sItem);
				
				return static_cast<TStorage*>(this)->RemoveFromStorage(p_sItem);
			}
		}

		return FALSE;
	}
	
	BOOL RemoveItem(LPCTSTR p_lpszItem)
	{
		return RemoveItem(tstring(p_lpszItem));
	}

	BOOL Clear()
	{
		if (m_asList.GetSize() != 0)
		{
			if (!static_cast<TStorage*>(this)->ClearStorage())
			{
				return FALSE;
			}
			
			m_asList.clear();
				
			return TRUE;
		}

		return FALSE;
	}

	BOOL Disable()
	{
		if ((!m_pac) || (!m_fBound))
			return FALSE;

		return SUCCEEDED(EnDisable(FALSE));
	}

	BOOL Enable(VOID)
	{
		if ((!m_pac) || (m_fBound))
			return FALSE;

		return SUCCEEDED(EnDisable(TRUE));
	}

public:

	//
	//	IUnknown implementation
	//
	STDMETHODIMP_(ULONG) AddRef()
	{
		return ::InterlockedIncrement(reinterpret_cast<LONG*>(&m_nRefCount));
	}

	STDMETHODIMP_(ULONG) Release()
	{
		ULONG nCount = 0;
		nCount = (ULONG) ::InterlockedDecrement(reinterpret_cast<LONG*>(&m_nRefCount));

		if (nCount == 0)
		{
			delete static_cast<TStorage*>(this);
		}

		return nCount;
	}

	STDMETHODIMP QueryInterface(REFIID riid, void** ppvObject)
	{
		HRESULT hr = E_NOINTERFACE;
		
		if (ppvObject != NULL)
		{
			*ppvObject = NULL;

			if (IID_IUnknown == riid)
				*ppvObject = static_cast<IUnknown*>(this);

			if (IID_IEnumString == riid)
				*ppvObject = static_cast<IEnumString*>(this);

			if (*ppvObject != NULL)
			{
				hr = S_OK;
				((LPUNKNOWN)*ppvObject)->AddRef();
			}
		}
		else
		{
			hr = E_POINTER;
		}
		
		return hr;
	}

public:

	//
	//	IEnumString implementation
	//
	STDMETHODIMP Next(ULONG celt, LPOLESTR* rgelt, ULONG* pceltFetched)
	{
		HRESULT hr = S_FALSE;

		if (!celt)
			celt = 1;

		ULONG i;
		for (i = 0; i < celt; i++)
		{
			if (m_nCurrentElement == (ULONG)m_asList.size())
				break;

			CT2COLE el(m_asList[m_nCurrentElement].c_str());

			rgelt[i] = (LPWSTR)::CoTaskMemAlloc((ULONG) sizeof(WCHAR) * (m_asList[m_nCurrentElement].length() + 1));
			lstrcpyW(rgelt[i], el);

			if (pceltFetched)
				*pceltFetched++;
			
			m_nCurrentElement++;
		}

		if (i == celt)
			hr = S_OK;

		return hr;
	}
 
	STDMETHODIMP Skip(ULONG celt)
	{
		m_nCurrentElement += celt;
		
		if (m_nCurrentElement > (ULONG)m_asList.size())
			m_nCurrentElement = 0;

		return S_OK;
	}
 
	STDMETHODIMP Reset(void)
	{	
		m_nCurrentElement = 0;
		return S_OK;
	}
 
	STDMETHODIMP Clone(IEnumString** ppenum)
	{
		if (!ppenum)
			return E_POINTER;
		
		CCustomAutoComplete<TStorage>* pnew = new CCustomAutoComplete<TStorage>();

		pnew->AddRef();
		*ppenum = pnew;

		return S_OK;
	}

private:

	// Internal implementation

	void InternalInit()
	{
		m_nCurrentElement = 0;
		m_nRefCount = 0;
		m_fBound = FALSE;
	}

	HRESULT EnDisable(BOOL p_fEnable)
	{
		HRESULT hr = S_OK;

		ATLASSERT(m_pac);

		hr = m_pac->Enable(p_fEnable);

		if (SUCCEEDED(hr))
		{
			m_fBound = p_fEnable;
		}

		return hr;
	}
};

/**
 * Implementation of CCustomAutoComplete for registry storage
 */
class CCustomAutoCompleteRegistry : public CCustomAutoComplete<CCustomAutoCompleteRegistry>
{
public:
	CCustomAutoCompleteRegistry(const HKEY p_hRootKey, const tstring& p_sSubKey) : m_hKey(NULL)
	{
		SetStorageSubkey(p_hRootKey, p_sSubKey);
	}

	~CCustomAutoCompleteRegistry()
	{
		if (m_hKey)
		{
			::RegCloseKey(m_hKey);
		}
	}

	BOOL LoadFromStorage()
	{
		ATLASSERT(m_hKey);

		DWORD dwCounter = 0;
		LONG lResult = ERROR_SUCCESS;
		DWORD dwValueNameSize = 0;
		TCHAR szValueName[MAX_PATH];				// This should be enough...?

		if (m_asList.size() != 0)
			m_asList.clear();

		while (ERROR_SUCCESS == lResult)
		{
			dwValueNameSize = sizeof(szValueName);
			lResult = ::RegEnumValue(m_hKey, dwCounter, szValueName, &dwValueNameSize, NULL, NULL, NULL, NULL);
			if (ERROR_SUCCESS == lResult)
				m_asList.push_back(tstring(szValueName));

			dwCounter++;
		}

		if ((ERROR_SUCCESS == lResult) || (ERROR_NO_MORE_ITEMS == lResult))
			return TRUE;

		return FALSE;
	}

	BOOL ClearStorage()
	{
		// Since we do not hold the parent key to our
		// own HKEY, we need to iterate through the
		// array and delete each one.

		for (TList::const_iterator i = m_asList.begin();
			i != m_asList.end();
			++i)
		{
			if (! RemoveFromStorage((*i).c_str()))
				return FALSE;
		}

		return TRUE;
	}

	BOOL AddToStorage(const tstring& p_sName)
	{
		ATLASSERT(m_hKey);

		LONG lResult = 0;

		lResult = ::RegSetValueEx(m_hKey, (LPCTSTR) p_sName.c_str(), 0, REG_SZ, (BYTE*)p_sName.c_str(), p_sName.length() * sizeof(TCHAR));

		if (ERROR_SUCCESS == lResult)
			return TRUE;

		return FALSE;
	}

	BOOL SetStorageSubkey(LPCTSTR p_lpszSubKey, HKEY p_hRootKey = HKEY_CURRENT_USER)
	{
		return SetStorageSubkey(p_hRootKey, tstring(p_lpszSubKey));
	}
	
	BOOL SetStorageSubkey(HKEY p_hRootKey, const tstring& p_sSubKey)
	{
		ATLASSERT(p_hRootKey);
		ATLASSERT(p_sSubKey.length());

		if (InitStorage(p_hRootKey, p_sSubKey))
			return LoadFromStorage();

		return FALSE;
	}

	BOOL InitStorage(HKEY p_hRootKey, const tstring& p_sSubKey)
	{
		LONG lResult = ERROR_SUCCESS;
		SECURITY_ATTRIBUTES sa = {0};

		if (m_hKey)
		{
			::RegCloseKey(m_hKey);
			m_hKey = NULL;
		}
		
		sa.nLength = sizeof(SECURITY_ATTRIBUTES);
		lResult = ::RegCreateKeyEx(p_hRootKey, p_sSubKey.c_str(), 0, NULL, REG_OPTION_NON_VOLATILE, KEY_READ | KEY_WRITE, &sa, &m_hKey, NULL);

		if ((ERROR_SUCCESS == lResult) && (NULL != m_hKey))
			return TRUE;

		return FALSE;
	}

	BOOL RemoveFromStorage(const tstring& p_sItem)
	{
		ATLASSERT(m_hKey);

		LONG lResult = 0;

		lResult = ::RegDeleteValue(m_hKey, (LPCTSTR) p_sItem.c_str());

		if (ERROR_SUCCESS == lResult)
			return TRUE;

		return FALSE;
	}

	BOOL WriteRecentItem(const tstring& p_sItem)
	{
		ATLASSERT(m_hKey);

		LONG lResult = 0;

		lResult = ::RegSetValueEx(m_hKey, (LPCTSTR) _T("_Recent"), 0, REG_SZ, (BYTE*)(LPCTSTR) p_sItem.c_str(), p_sItem.length() * sizeof(TCHAR));

		if (ERROR_SUCCESS == lResult)
			return TRUE;

		return FALSE;
	}

	BOOL ReadRecentItem(tstring& p_sItem)
	{
		ATLASSERT(m_hKey);

		LONG lResult = ERROR_SUCCESS;
		DWORD dwValueSize = 0;

		dwValueSize = MAX_PATH;
		CString sItem;
		lResult = ::RegQueryValueEx(m_hKey, _T("_Recent"), NULL, NULL, (LPBYTE)sItem.GetBufferSetLength(dwValueSize), &dwValueSize);
		sItem.ReleaseBuffer();
		p_sItem = sItem;

		if (ERROR_SUCCESS == lResult)
			return TRUE;

		return FALSE;
	}

private:
	HKEY m_hKey;
};

class CCustomAutoCompletePN : public CCustomAutoComplete<CCustomAutoCompletePN>
{
	friend class CCustomAutoComplete<CCustomAutoCompletePN>;

public:
	CCustomAutoCompletePN(LPCTSTR settingsKey, int maxEntries) : m_key(settingsKey)
	{
		m_nMaxElements = maxEntries;
		loadData();
	}

	~CCustomAutoCompletePN()
	{
		saveData();
	}

private:
	
	/// We don't do anything here, we only update storage at start and exit
	BOOL AddToStorage(const tstring& value)
	{
		return true;
	}

	/// We don't do anything here, we only update storage at start and exit
	BOOL RemoveFromStorage(const tstring& value)
	{
		return true;
	}

	void loadData()
	{
		//DWORD dwTicks = ::GetTickCount();
		tstring path;
		OPTIONS->GetPNPath(path, PNPATH_USERSETTINGS);
		path += m_key;
		path += _T(".acd");

		wchar_t buf[2048];
		FILE* f = _tfopen(path.c_str(), _T("rb"));
		if (f != NULL)
		{
			int read;
			BYTE bom[2];

			read = fread(&bom, 1, 2, f);
			if (read == 2 && bom[0] == 0xff && bom[1] == 0xfe)
			{
				while((read = fread(&buf, 1, sizeof(buf), f)) > 0)
				{
					load(buf, read / 2);
				}
			}
			
			fclose(f);
		}
		
		/*TCHAR dbg[300];
		_stprintf(dbg, "%s: %dms", m_key.c_str(), ::GetTickCount()-dwTicks);
		LOG(dbg);*/
	}

	void load(wchar_t* buf, int count)
	{
		for (int i = 0; i < count; i++)
		{
			if (buf[i] != L'\n')
			{
				m_cur += buf[i];
			}
			else
			{
#if !defined(_UNICODE)
				USES_CONVERSION;
				CW2CT cur(m_cur.c_str());
				m_asList.push_back((LPCTSTR)cur);
#else
				m_asList.push_back(m_cur);
#endif
				m_cur.clear();
			}
		}
	}

	void saveData()
	{
		tstring path;
		OPTIONS->GetPNPath(path, PNPATH_USERSETTINGS);
		path += m_key;
		path += _T(".acd");

		wchar_t linebrk = L'\n';
		BYTE bom[] = {0xFF, 0xFE};
		FILE* f = _tfopen(path.c_str(), _T("wb"));
		fwrite(&bom, 2, 1, f);
		if (f != NULL)
		{
			if (m_asList.size() > static_cast<size_t>(m_nMaxElements))
			{
				m_asList.resize(m_nMaxElements);
			}

			for(TList::const_iterator i = m_asList.begin();
				i != m_asList.end();
				++i)
			{
#if !defined (_UNICODE)
				USES_CONVERSION;
				CT2CW cur((*i).c_str());
				fwrite((LPWSTR)cur, (*i).length() * sizeof(wchar_t), 1, f);
#else
				fwrite((*i).c_str(), (*i).length() * sizeof(wchar_t), 1, f);
#endif
				fwrite(&linebrk, sizeof(wchar_t), 1, f);
			}
			
			fclose(f);
		}
	}

	std::wstring m_cur;
	tstring m_key;
};

#endif // !defined(CCustomAutoComplete_INCLUDED)