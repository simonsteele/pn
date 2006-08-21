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
//  Updates:        
//
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

class CCustomAutoComplete : public IEnumString
{

private:

	CSimpleArray<CString> m_asList;
	HKEY m_hKey;
	CComPtr<IAutoComplete> m_pac;

	ULONG m_nCurrentElement;
	ULONG m_nRefCount;
	BOOL m_fBound;


public:


	// Constructors/destructors

	CCustomAutoComplete()
	{
		InternalInit();

	}

	CCustomAutoComplete(const HKEY p_hRootKey, const CString& p_sSubKey)
	{
		InternalInit();

		SetStorageSubkey(p_hRootKey, p_sSubKey);

	}

	CCustomAutoComplete(const CSimpleArray<CString>& p_sItemList)
	{
		InternalInit();

		SetList(p_sItemList);

	}
	
	
	~CCustomAutoComplete()
	{

		m_asList.RemoveAll();

		if (m_hKey)
			::RegCloseKey(m_hKey);

		if (m_pac)
			m_pac.Release();


	}

public:

	// Implementation

	BOOL SetList(const CSimpleArray<CString>& p_sItemList)
	{
		
		ATLASSERT(p_sItemList.GetSize() != 0);
		
		Clear();

		m_asList = p_sItemList;

		return TRUE;

	}

	const CSimpleArray<CString>& GetList()
	{
		return m_asList;
	}

	BOOL SetStorageSubkey(LPCTSTR p_lpszSubKey, HKEY p_hRootKey = HKEY_CURRENT_USER)
	{

		return SetStorageSubkey(p_hRootKey, CString(p_lpszSubKey));

	}
	
	BOOL SetStorageSubkey(HKEY p_hRootKey, const CString& p_sSubKey)
	{
		
		ATLASSERT(p_hRootKey);
		ATLASSERT(p_sSubKey.GetLength());

		if (InitStorage(p_hRootKey, p_sSubKey))
			return LoadFromStorage();

		return FALSE;


	}
	
	BOOL Bind(HWND p_hWndEdit, DWORD p_dwOptions = 0, LPCTSTR p_lpszFormatString = NULL)
	{

		ATLASSERT(::IsWindow(p_hWndEdit));

		LPOLESTR pFormatString = NULL;

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

			USES_CONVERSION;
			
			hr = m_pac->Init(p_hWndEdit, this, NULL, T2OLE(p_lpszFormatString));

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

	BOOL AddItem(CString& p_sItem)
	{

		if (p_sItem.GetLength() != 0)
		{
			if (m_asList.Find(p_sItem) == -1)
			{
				m_asList.Add(p_sItem);

				if (m_hKey)
					return AddToStorage(p_sItem);
				
					return TRUE;
			}
				
		}

		return FALSE;
	}

	BOOL AddItem(LPCTSTR p_lpszItem)
	{

		return AddItem(CString(p_lpszItem));

	}

	INT GetItemCount()
	{
		return m_asList.GetSize();
	}

	BOOL SetRecentItem(const CString& p_sItem)
	{
		
		// Only supported if registry persistance is enabled.
		// Call this function after setting the registry root
		if ((!m_hKey) || (!m_fBound))
			return FALSE;


		return WriteRecentItem(p_sItem);

	}

	BOOL GetRecentItem(CString& p_sItem)
	{
		
		// Only supported if registry persistance is enabled.
		// Call this function after setting the registry root
		if ((!m_hKey) || (!m_fBound))
			return FALSE;

				
		return ReadRecentItem(p_sItem);


	}

	BOOL RemoveItem(CString& p_sItem)
	{
		if (p_sItem.GetLength() != 0)
		{
			if (m_asList.Find(p_sItem) != -1)
			{
				m_asList.Remove(p_sItem);
					
				if (m_hKey)
					return RemoveFromStorage(p_sItem);

					return TRUE;
			}

		}

		return FALSE;

	}
	
	BOOL RemoveItem(LPCTSTR p_lpszItem)
	{

		return RemoveItem(CString(p_lpszItem));
	}


	BOOL Clear()
	{
		if (m_asList.GetSize() != 0)
		{
			if (m_hKey)
			{
				if (!ClearStorage())
					return FALSE;
			}
			
			m_asList.RemoveAll();
				
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
			delete this;

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

			USES_CONVERSION;

			if (m_nCurrentElement == (ULONG)m_asList.GetSize())
				break;

			rgelt[i] = (LPWSTR)::CoTaskMemAlloc((ULONG) sizeof(WCHAR) * (m_asList[m_nCurrentElement].GetLength() + 1));
			lstrcpyW(rgelt[i], T2OLE(m_asList[m_nCurrentElement]));

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
		
		if (m_nCurrentElement > (ULONG)m_asList.GetSize())
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
		
		CCustomAutoComplete* pnew = new CCustomAutoComplete();

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
		m_hKey = NULL;


	}

	HRESULT EnDisable(BOOL p_fEnable)
	{

		HRESULT hr = S_OK;

		ATLASSERT(m_pac);

		hr = m_pac->Enable(p_fEnable);

		if (SUCCEEDED(hr))
			m_fBound = p_fEnable;

		return hr;


	}

	BOOL InitStorage(HKEY p_hRootKey, const CString& p_sSubKey)
	{

		LONG lResult = ERROR_SUCCESS;
		SECURITY_ATTRIBUTES sa = {0};

		if (m_hKey)
		{
			::RegCloseKey(m_hKey);
			m_hKey = NULL;
		}

		
		sa.nLength = sizeof(SECURITY_ATTRIBUTES);
		lResult = ::RegCreateKeyEx(p_hRootKey, p_sSubKey, 0, NULL, REG_OPTION_NON_VOLATILE, KEY_READ | KEY_WRITE, &sa, &m_hKey, NULL);

		if ((ERROR_SUCCESS == lResult) && (NULL != m_hKey))
			return TRUE;


		return FALSE;


	}


	BOOL RemoveFromStorage(const CString& p_sItem)
	{

		ATLASSERT(m_hKey);

		LONG lResult = 0;

		lResult = ::RegDeleteValue(m_hKey, (LPCTSTR) p_sItem);

		if (ERROR_SUCCESS == lResult)
			return TRUE;


		return FALSE;


	}

	BOOL LoadFromStorage()
	{

		ATLASSERT(m_hKey);

		DWORD dwCounter = 0;
		LONG lResult = ERROR_SUCCESS;
		DWORD dwValueNameSize = 0;
		TCHAR szValueName[MAX_PATH];				// This should be enough...?

		if (m_asList.GetSize() != 0)
			m_asList.RemoveAll();

		while (ERROR_SUCCESS == lResult)
		{

			dwValueNameSize = sizeof(szValueName);
			lResult = ::RegEnumValue(m_hKey, dwCounter, szValueName, &dwValueNameSize, NULL, NULL, NULL, NULL);
			if (ERROR_SUCCESS == lResult)
				m_asList.Add(CString(szValueName));

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

		for (int i = 0; i < m_asList.GetSize(); i++)
		{
			if (! RemoveFromStorage(m_asList[i]))
				return FALSE;

		}

		return TRUE;

	}


	BOOL AddToStorage(const CString& p_sName)
	{

		ATLASSERT(m_hKey);

		LONG lResult = 0;

		lResult = ::RegSetValueEx(m_hKey, (LPCTSTR) p_sName, 0, REG_SZ, (BYTE*)(LPCTSTR) p_sName, p_sName.GetLength() * sizeof(TCHAR));

		if (ERROR_SUCCESS == lResult)
			return TRUE;


		return FALSE;

	}


	BOOL WriteRecentItem(const CString& p_sItem)
	{

		ATLASSERT(m_hKey);

		LONG lResult = 0;

		lResult = ::RegSetValueEx(m_hKey, (LPCTSTR) _T("_Recent"), 0, REG_SZ, (BYTE*)(LPCTSTR) p_sItem, p_sItem.GetLength() * sizeof(TCHAR));

		if (ERROR_SUCCESS == lResult)
			return TRUE;

		return FALSE;



	}

	BOOL ReadRecentItem(CString& p_sItem)
	{

		ATLASSERT(m_hKey);

		LONG lResult = ERROR_SUCCESS;
		DWORD dwValueSize = 0;

		dwValueSize = MAX_PATH;
		lResult = ::RegQueryValueEx(m_hKey, _T("_Recent"), NULL, NULL, (LPBYTE)p_sItem.GetBufferSetLength(dwValueSize), &dwValueSize);
		p_sItem.ReleaseBuffer();

		if (ERROR_SUCCESS == lResult)
			return TRUE;


		return FALSE;


	}


};

#endif // !defined(CCustomAutoComplete_INCLUDED)