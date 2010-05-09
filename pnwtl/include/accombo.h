/**
 * @file autocompletecombo.h
 * @brief Merge the autocompletion code with a combo box for ease of use.
 * @author Bjoern Graf
 * @note Copyright (c) 2003 Bjoern Graf
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef accombo_h__included
#define accombo_h__included

#define BXTN_ENTER	11
#define BXT_WM_ENTER WM_USER + 1

namespace WTL
{
namespace BXT
{

/**
 * @class CComboBoxACImpl
 * @brief template class implementing auto completion for combo boxes.
 */
template <class T, class TBase = CComboBox, class TWinTraits = CControlWinTraits>
class ATL_NO_VTABLE CComboBoxACImpl : public CWindowImpl< T, TBase, TWinTraits>
{
public:
	DECLARE_WND_SUPERCLASS(NULL, TBase::GetWndClassName())

	typedef CWindowImpl< T, TBase, TWinTraits >	baseClass;

	/**
	 * @class CComboBoxEditImpl
	 * @brief template class implementing simple edit control selection persistence.
	 */
	template <class T, class TBase = CEdit, class TWinTraits = CControlWinTraits>
	class ATL_NO_VTABLE CComboBoxEditImpl : public CWindowImpl< T, TBase, TWinTraits>
	{
	public:
		DECLARE_WND_SUPERCLASS(NULL, TBase::GetWndClassName())

	// Constructors
		CComboBoxEditImpl() : m_selStart(0), m_selEnd(0) { }

		CComboBoxEditImpl< TBase >& operator=(HWND hWnd)
		{
			m_hWnd = hWnd;
			return *this;
		}

		/**
		 * Get the selection stored when the combo box edit control lost focus
		 */
		void GetStoredSel(int& start, int& end)
		{
			start = m_selStart;
			end = m_selEnd;
		}

	// Message map and handlers
		typedef CComboBoxEditImpl< T, TBase, TWinTraits >	thisClass;
		BEGIN_MSG_MAP(thisClass)
			MESSAGE_HANDLER(WM_KILLFOCUS, OnKillFocus)
			MESSAGE_HANDLER(WM_KEYUP, OnKeyUp)
		END_MSG_MAP()

		/**
		 * We need to use Kill Focus to store away the current text box selection as the combo
		 * control wipes it when focus is lost. This is needed so we can insert text at specific
		 * points for regex match chars etc.
		 */
		LRESULT OnKillFocus(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
		{
			TBase::GetSel(m_selStart, m_selEnd);
			bHandled = FALSE;
			return 0;
		}

		LRESULT OnKeyUp(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& /*bHandled*/)
		{
			if( /*wParam == VK_ENTER ||*/ wParam == VK_RETURN )
			{
				::SendMessage(GetParent(), BXT_WM_ENTER, 0, 0);
			}

			return 0;
		}

	protected:
		bool m_bInternalGetSel;
		int m_selStart;
		int m_selEnd;
	};

	class CComboBoxEdit : public CComboBoxEditImpl<CComboBoxEdit>
	{
	public:
		DECLARE_WND_SUPERCLASS(_T("WTL_ComboBoxEdit"), GetWndClassName())

		CComboBoxEdit() : 
			CComboBoxEditImpl<CComboBoxEdit>()
		{ }
	};

// Constructors
	CComboBoxACImpl() : m_pAC(NULL), m_hWndOwner(NULL), m_bAutoAdd(true) { }

	CComboBoxACImpl(bool autoAdd) : m_pAC(NULL), m_hWndOwner(NULL), m_bAutoAdd(autoAdd) { }

	CComboBoxACImpl< TBase >& operator=(HWND hWnd)
	{
		m_hWnd = hWnd;

		return *this;
	}

	// overridden to provide proper initialization
	BOOL SubclassWindow(HWND hWnd, LPCTSTR szSubKey)
	{
		BOOL bRet = baseClass::SubclassWindow(hWnd);
		if(bRet)
			InitUserAC(szSubKey);
		return bRet;
	}

	BOOL SubclassWindow(HWND hWnd)
	{
		BOOL bRet = baseClass::SubclassWindow(hWnd);
		if(bRet)
			InitFileAC();
		return bRet;
	}

	// These 2 provide for registry-based AutoComplete.
	HWND Create(HWND hWndParent, _U_RECT rect, LPCTSTR szWindowName,
			DWORD dwStyle, DWORD dwExStyle, _U_MENUorID MenuOrID,
			LPCTSTR szSubKey, UINT nDummyId)
	{
		m_hWndOwner = hWndParent;

		HWND hWnd = baseClass::Create(hWndParent, *rect.m_lpRect, szWindowName, dwStyle, dwExStyle, (unsigned int)MenuOrID.m_hMenu);
		if(hWnd)
		{
			InitUserAC(szSubKey);
			SetWindowPos(GetDlgItem(nDummyId), 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_SHOWWINDOW);
		}
		return hWnd;
	}

	HWND Create(HWND hWndParent, _U_RECT rect, LPCTSTR szWindowName,
			DWORD dwStyle, DWORD dwExStyle, _U_MENUorID MenuOrID,
			LPCTSTR szSubKey)
	{
		m_hWndOwner = hWndParent;

		HWND hWnd = baseClass::Create(hWndParent, *rect.m_lpRect, szWindowName, dwStyle, dwExStyle, (unsigned int)MenuOrID.m_hMenu);
		if(hWnd)
		{
			InitUserAC(szSubKey);
		}
		return hWnd;
	}

	HWND Create(HWND hWndParent, _U_RECT rect, LPCTSTR szWindowName,
			DWORD dwStyle, DWORD dwExStyle, _U_MENUorID MenuOrID)
	{
		m_hWndOwner = hWndParent;

		HWND hWnd = baseClass::Create(hWndParent, *rect.m_lpRect, szWindowName, dwStyle, dwExStyle, (unsigned int)MenuOrID.m_hMenu);
		if(hWnd)
		{
			InitFileAC();
		}

		return hWnd;
	}

	HWND Create(HWND hWndParent, _U_RECT rect, LPCTSTR szWindowName,
			DWORD dwStyle, DWORD dwExStyle, _U_MENUorID MenuOrID, UINT nDummyId)
	{
		m_hWndOwner = hWndParent;

		HWND hWnd = baseClass::Create(hWndParent, *rect.m_lpRect, szWindowName, dwStyle, dwExStyle, (unsigned int)MenuOrID.m_hMenu);
		if(hWnd)
		{
			InitFileAC();
			SetWindowPos(GetDlgItem(nDummyId), 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_SHOWWINDOW);
		}

		return hWnd;
	}

	void SetOwnerHWND(HWND hWndOwner)
	{
		m_hWndOwner = hWndOwner;
	}

// Message map and handlers
	typedef CComboBoxACImpl< T, TBase, TWinTraits >	thisClass;
	BEGIN_MSG_MAP(thisClass)
		MESSAGE_HANDLER(WM_SETTEXT, OnSetText)
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy)
		MESSAGE_HANDLER(BXT_WM_ENTER, OnEnterPressed)
	END_MSG_MAP()

	LRESULT OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		if(m_pAC)
		{
			m_pAC->Unbind();
		}
		if(m_filesAC)
		{
			m_filesAC.Release();
		}
		return 0;
	}

	LRESULT OnSetText(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
	{
		if(m_bAutoAdd)
			AddString((LPCTSTR)lParam);

		bHandled = FALSE;
		return 0;
	}

	LRESULT OnEnterPressed(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
	{
		::SendMessage(m_hWndOwner, WM_COMMAND, MAKELONG(GetWindowLong(GWL_ID), BXTN_ENTER), (LPARAM)m_hWnd);
		return 0;
	}

// Implementation

	void AddString(LPCTSTR string)
	{
		AddString(string,0);
	}
	
	void AddString(LPCTSTR string, int addPos )
	{
		CString text((LPCTSTR)string);
		if(text.GetLength() > 0)
		{
			int i = FindStringExactMatchCase(text);
			
			// Bring the string on top
			if (i >= 0)
			{
				DeleteString(i);
				InsertString(addPos, text);
			}
			else if (i == CB_ERR)
			{
				InsertString(addPos, text);
			}

			SetCurSel(addPos);

			if(m_pAC)
				m_pAC->AddItem(text);
		}
	}

	int FindStringExactMatchCase(LPCTSTR string)
	{
		size_t searchLength(_tcslen(string));
		std::vector<TCHAR> buffer(searchLength+1);
		for (int i = 0; i < GetCount(); i++)
		{
			size_t entryLength = GetLBTextLen(i);
			if (entryLength != searchLength)
			{
				continue;
			}

			GetLBText(i, &buffer[0]);

			if (_tcscmp(string, &buffer[0]) == 0)
			{
				return i;
			}
		}

		return CB_ERR;
	}

	/**
	 * Initialise the combobox for user-list autocomplete
	 */
	bool InitUserAC(LPCTSTR szSubKey)
	{
		SetFont((HFONT)::SendMessage(GetParent(), WM_GETFONT, 0, 0));
		HWND hWndEdit = GetWindow(GW_CHILD);
		
		if(hWndEdit)
		{
			m_edit.SubclassWindow(hWndEdit);

			m_pAC = new CCustomAutoCompletePN(szSubKey, 20);
			m_pAC->Bind(hWndEdit, /*ACO_UPDOWNKEYDROPSLIST |*/ ACO_AUTOSUGGEST /*| ACO_AUTOAPPEND*/);

			// Fill combobox with the 20 recent entries, assuming AC stores the
			// strings inorder.
			const std::vector<tstring>& items = m_pAC->GetList();
			int size = min(items.size(), 20);
			for(int i = 0; i < size; i++)
			{
				AddString(items[i].c_str(),-1);
			}

			return true;
		}

		return false;
	}

	/**
	 * Initialise the combobox for filesystem autocomplete
	 */
	bool InitFileAC()
	{
		SetFont((HFONT)::SendMessage(GetParent(), WM_GETFONT, 0, 0));
		HWND hWndEdit = GetWindow(GW_CHILD);
		
		if(hWndEdit)
		{
			m_edit.SubclassWindow(hWndEdit);

			/*IUnknown *punkSource;

CoCreateInstance(clsidSource, 
                 NULL, 
                 CLSCTX_INPROC_SERVER,
                 IID_IACList, 
                 (LPVOID*)&punkSource);*/

			CComPtr<IUnknown> punkSource;
			punkSource.CoCreateInstance(CLSID_ACListISF, NULL, CLSCTX_INPROC_SERVER);

			IACList2 *pal2;

			if (SUCCEEDED(punkSource->QueryInterface(IID_IACList2, (LPVOID*)&pal2)))
			{
				pal2->SetOptions(ACLO_FILESYSDIRS | ACLO_FILESYSONLY);
				pal2->Release();
			}

			HRESULT hr = S_OK;
			hr = m_filesAC.CoCreateInstance(CLSID_AutoComplete);
			m_filesAC->Init(hWndEdit, punkSource, NULL, NULL);

			IAutoComplete2 *pac2;

			if (SUCCEEDED(m_filesAC->QueryInterface(IID_IAutoComplete2, (LPVOID*)&pac2)))
			{
				pac2->SetOptions(ACO_AUTOSUGGEST);
				pac2->Release();
			}

			return true;
		}

		return false;
	}

	CComboBoxEdit& GetEditCtrl()
	{
		return m_edit;
	}

private:
	HWND m_hWndOwner; // window that wants enter key-press notifies.
	CComboBoxEdit m_edit;
	CCustomAutoCompletePN*	m_pAC;
	CComPtr<IAutoComplete>	m_filesAC;
	bool m_bAutoAdd;
};

class CComboBoxAC : public CComboBoxACImpl<CComboBoxAC>
{
public:
	DECLARE_WND_SUPERCLASS(_T("WTL_ComboBoxAC"), GetWndClassName())

	CComboBoxAC() : 
		CComboBoxACImpl<CComboBoxAC>()
	{ }

	CComboBoxAC(bool autoAdd) : 
		CComboBoxACImpl<CComboBoxAC>(autoAdd)
	{ }
};

} } // namespace WTL::BXT

#endif // accombo_h__included