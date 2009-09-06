/**
 * @file optionsdialog.h
 * @brief A Tree-Item-Per-Page Options dialog.
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 *
 * To create your options dialog, first create a dialog template and give
 * it the resource ID "IDD_OPTIONS". On that dialog, ensure that there is
 * a tree control with ID "IDC_TREE" and somewhere for your child dialogs to
 * go, with the ID "IDC_PLACEHOLDER".
 *
 * For each page in the dialog, derive a class from COptionsPageImpl (like 
 * CDialogImpl) and implement the GetTreePosition method. From this method,
 * you should return a path-style options tree position. Examples would be:
 *    Root\Child
 *    AnotheerRoot\Folder\Child
 *    ARootNode
 *
 * The options dialog will deal with organising the items for each page in the
 * tree. The items are not currently sorted, so the order can be controlled
 * by order of addition.
 */

#ifndef optionsdialog_h__included
#define optionsdialog_h__included

class COptionsDialog;

class COptionsPage
{
	friend class COptionsDialog;

public:
	COptionsPage() : m_bCreated(false)
	{
	}

	// Methods called by COptionsDialog when dealing with COptionsPage.
	virtual void OnOK(){};
	virtual void OnCancel(){};
	virtual void OnInitialise(){};

	// Return a tree position like "Schemes\AddOn Options"
	virtual tstring GetTreePosition() = 0;

	virtual HWND CreatePage(HWND hOwner, LPRECT rcPos, HWND hInsertAfter = NULL) = 0;
	virtual void ClosePage() = 0;
	virtual void ShowPage(int showCmd) = 0;
	virtual LRESULT ForwardMessage(UINT message, WPARAM wParam, LPARAM lParam) = 0;

protected:
	bool m_bCreated;
};

template <class T>
class COptionsPageImpl : public CDialogImpl<T>, public COptionsPage, CThemeImpl< COptionsPageImpl<T> >
{
	public:
		virtual ~COptionsPageImpl(){}

		virtual HWND CreatePage(HWND hParent, LPRECT rcPos, HWND hInsertAfter = NULL)
		{
			Create(hParent);
			SetWindowPos(hInsertAfter, rcPos, SWP_NOACTIVATE);
			MoveWindow(rcPos);

			return m_hWnd;
		}

		virtual void ClosePage()
		{
			DestroyWindow();
		}

		virtual void ShowPage(int showCmd)
		{
			ShowWindow(showCmd);
		}

		virtual LRESULT ForwardMessage(UINT message, WPARAM wParam, LPARAM lParam)
		{
			return SendMessage(message, wParam, lParam);
		}
};

class COptionsDialog : public CDialogImpl<COptionsDialog>, CThemeImpl<COptionsDialog>
{
	typedef std::list<COptionsPage*> PAGEPTRLIST;
	typedef CDialogImpl<COptionsDialog> baseClass;

	public:

		COptionsDialog() : m_pCurrentPage(NULL), m_pInitialPage(NULL), m_hInitialItem(NULL)
		{
		}

		enum { IDD = IDD_OPTIONS };

		BEGIN_MSG_MAP(COptionsDialog)
			MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
			MESSAGE_HANDLER(PN_NOTIFY, OnNotifyPages)
			COMMAND_ID_HANDLER(IDOK, OnOK)
			COMMAND_ID_HANDLER(IDCANCEL, OnCancel)
			NOTIFY_ID_HANDLER(IDC_TREE, OnTreeNotify)
		END_MSG_MAP()

		BOOL EndDialog(int nRetCode)
		{
			ClosePages();
			return baseClass::EndDialog(nRetCode);
		}

		void AddPage(COptionsPage* pPage)
		{
			m_Pages.push_back(pPage);
		}
	
		void SetInitialPage(COptionsPage* pPage)
		{
			m_pInitialPage = pPage;
		}

	protected:

		LRESULT OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
		{
			PAGEPTRLIST::const_iterator i;

			for(i = m_Pages.begin(); i != m_Pages.end(); ++i)
			{
				(*i)->OnOK();
			}

			EndDialog(wID);

			return TRUE;
		}

		LRESULT OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
		{
			PAGEPTRLIST::const_iterator i;

			for(i = m_Pages.begin(); i != m_Pages.end(); ++i)
			{
				(*i)->OnCancel();
			}

			EndDialog(wID);
			
			return TRUE;
		}

		/**
		 * Dialog is being initialised
		 */
		LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
		{
			m_tree.Attach(GetDlgItem(IDC_TREE));

			if (WTL::RunTimeHelper::IsVista() && IsThemingSupported()) 
			{
				::SetWindowTheme(m_tree.m_hWnd, L"explorer", NULL);
			}

			InitialisePages();
				
			CenterWindow(GetParent());

			if(m_pInitialPage)
			{
				m_tree.SelectItem(m_hInitialItem);
			}
			
			return TRUE;
		}

		LRESULT OnNotifyPages(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
		{
			for(PAGEPTRLIST::iterator i = m_Pages.begin();
				i != m_Pages.end();
				++i)
			{
				if( (*i)->m_bCreated )
				{
					(*i)->ForwardMessage(PN_NOTIFY, wParam, lParam);
				}
			}

			return TRUE;
		}

		void InitialisePages()
		{
			TCHAR buf[200];
			PAGEPTRLIST::iterator i;

			for(i = m_Pages.begin(); i != m_Pages.end(); ++i)
			{
				tstring treeloc = (*i)->GetTreePosition();
				PNASSERT(treeloc.length() < 200);
				_tcscpy(buf, treeloc.c_str());

				HTREEITEM ti = NULL;
				TCHAR* pSlash = NULL;
				TCHAR* pNext = buf;

				while((pSlash = _tcschr(pNext, _T('\\'))) != NULL)
				{
					*pSlash = NULL;
					ti = AddTreeEntry(pNext, ti);
					*pSlash = '\\';
					pNext = pSlash + 1;
				}

				// Add Tree Item
				HTREEITEM item = AddTreeEntry(pNext, ti);
				m_tree.SetItemData(item, reinterpret_cast<DWORD_PTR>(*i));

				if((*i) == m_pInitialPage)
					m_hInitialItem = item;
			}
		}

		void ClosePages()
		{
			PAGEPTRLIST::iterator i;
			for(i = m_Pages.begin(); i != m_Pages.end(); ++i)
			{
				if((*i)->m_bCreated)
				{
					(*i)->ClosePage();
				}
			}
		}

		void SelectPage(COptionsPage* pPage)
		{
			CRect rcPage;
			::GetWindowRect(GetDlgItem(IDC_PLACEHOLDER), rcPage);
			ScreenToClient(rcPage);

			if(m_pCurrentPage)
			{
				m_pCurrentPage->ShowPage(SW_HIDE);
			}

			if(!pPage->m_bCreated)
			{
				pPage->CreatePage(m_hWnd, rcPage, m_tree);
				pPage->OnInitialise();
				pPage->m_bCreated = true;
			}
			
			pPage->ShowPage(SW_SHOW);
			m_pCurrentPage = pPage;
		}

		HTREEITEM FindAtThisLevel(LPCTSTR title, HTREEITEM context)
		{
			HTREEITEM i = (context ? m_tree.GetChildItem(context) : m_tree.GetRootItem());

			TCHAR buf[32];
			TVITEM tvi;
			tvi.mask = TVIF_TEXT;
			tvi.pszText = buf;
			tvi.cchTextMax = 30;
			
			while(i)
			{
				tvi.hItem = i;
				m_tree.GetItem(&tvi);

				if(_tcscmp(buf, title) == 0)
					break;

				i = m_tree.GetNextSiblingItem(i);
			}

			return i;
		}

		HTREEITEM AddTreeEntry(LPCTSTR title, HTREEITEM context)
		{
			HTREEITEM i = FindAtThisLevel(title, context);

			if(!i)
			{
				i = m_tree.InsertItem(title, context, NULL);
				if(context)
					m_tree.Expand(context, TVE_EXPAND);
				m_tree.SetItemData(i, NULL);
			}

			return i;
		}


		LRESULT OnTreeNotify(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
		{
			LPNMTREEVIEW pN = reinterpret_cast<LPNMTREEVIEW>(pnmh);
			if(pnmh->code == TVN_SELCHANGED)
			{
				COptionsPage* pPage = reinterpret_cast<COptionsPage*>(m_tree.GetItemData(pN->itemNew.hItem));
				if(pPage)
				{
					SelectPage(pPage);
				}
				else
				{
					// If we're a parent with a child and we don't have a page show the child page
					HTREEITEM child = m_tree.GetChildItem(pN->itemNew.hItem);
					if(child != NULL)
					{
						pPage = reinterpret_cast<COptionsPage*>(m_tree.GetItemData(child));
						if(pPage)
						{
							SelectPage(pPage);
						}
					}
				}

			}

			return 0;
		}

	protected:
		PAGEPTRLIST		m_Pages;
		COptionsPage*	m_pCurrentPage;
		COptionsPage*	m_pInitialPage;
		HTREEITEM		m_hInitialItem;
		CTreeViewCtrl	m_tree;
};

#endif