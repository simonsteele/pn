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
		CComboBoxEditImpl() : m_bInternalGetSel(false), m_selStart(0), m_selEnd(0) { }

		CComboBoxEditImpl< TBase >& operator=(HWND hWnd)
		{
			m_hWnd = hWnd;
			return *this;
		}

	// Message map and handlers
		typedef CComboBoxEditImpl< T, TBase, TWinTraits >	thisClass;
		BEGIN_MSG_MAP(thisClass)
			MESSAGE_HANDLER(EM_GETSEL, OnGetSel)
			MESSAGE_HANDLER(EM_REPLACESEL, OnReplaceSel)
			MESSAGE_HANDLER(WM_KILLFOCUS, OnKillFocus)
			MESSAGE_HANDLER(WM_KEYUP, OnKeyUp)
		END_MSG_MAP()

		LRESULT OnGetSel(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
		{
			LRESULT lRes;
			if(m_bInternalGetSel)
			{
				lRes = DefWindowProc(uMsg, wParam, lParam);
			}
			else
			{
				if(wParam)
					*(int*)wParam = m_selStart;
				if(lParam)
					*(int*)lParam = m_selEnd;
				lRes = MAKELRESULT(m_selStart, m_selEnd);
			}
			return lRes;
		}

		LRESULT OnReplaceSel(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
		{
			TBase::SetSel(m_selStart, m_selEnd);
			return DefWindowProc(uMsg, wParam, lParam);
		}

		LRESULT OnKillFocus(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
		{
			m_bInternalGetSel = true;
			TBase::GetSel(m_selStart, m_selEnd);
			m_bInternalGetSel = false;
			//ATLTRACE(_T("CurSel: [%d, %d]\n"), m_selStart, m_selEnd);
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

	HWND m_hWndOwner; // window that wants enter key-press notifies.
	CComboBoxEdit m_edit;
	CCustomAutoComplete* m_pAC;

// Constructors
	CComboBoxACImpl() : m_pAC(NULL), m_hWndOwner(NULL) { }

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
			Init(szSubKey);
		return bRet;
	}

	HWND Create(HWND hWndParent, _U_RECT rect, LPCTSTR szWindowName,
			DWORD dwStyle, DWORD dwExStyle, _U_MENUorID MenuOrID,
			LPCTSTR szSubKey, UINT nDummyId)
	{
		m_hWndOwner = hWndParent;

		HWND hWnd = baseClass::Create(hWndParent, *rect.m_lpRect, szWindowName, dwStyle, dwExStyle, (unsigned int)MenuOrID.m_hMenu);
		if(hWnd)
		{
			Init(szSubKey);
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
			Init(szSubKey);
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
		m_pAC->Unbind();
		return 0;
	}

	LRESULT OnSetText(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
	{
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
		CString text((LPCTSTR)string);
		if(text.GetLength() > 0)
		{
			int i = FindStringExact(-1, text);
			// Bring the string on top
			if(i > 0)
			{
				DeleteString(i);
				InsertString(0, text);
			}
			else if(i < 0)
				InsertString(0, text);
			SetCurSel(0);
			m_pAC->AddItem(text);
		}
	}

	bool Init(LPCTSTR szSubKey)
	{
		SetFont((HFONT)::SendMessage(GetParent(), WM_GETFONT, 0, 0));
		HWND hWndEdit = GetWindow(GW_CHILD);
		
		if(hWndEdit)
		{
			m_edit.SubclassWindow(hWndEdit);

			m_pAC = new CCustomAutoComplete(HKEY_CURRENT_USER, szSubKey);
			m_pAC->Bind(hWndEdit, /*ACO_UPDOWNKEYDROPSLIST |*/ ACO_AUTOSUGGEST | ACO_AUTOAPPEND);

			// Fill combobox with the 20 recent entries, assuming AC stores the
			// strings inorder.
			const CSimpleArray<CString> items = m_pAC->GetList();
			int size = min(items.GetSize(), 20);
			for(int i = 0; i < size; i++)
			{
				AddString(items[i]);
			}

			return true;
		}

		return false;
	}
};
class CComboBoxAC : public CComboBoxACImpl<CComboBoxAC>
{
public:
	DECLARE_WND_SUPERCLASS(_T("WTL_ComboBoxAC"), GetWndClassName())

	CComboBoxAC() : 
		CComboBoxACImpl<CComboBoxAC>()
	{ }
};

} } // namespace WTL::BXT

#endif // accombo_h__included