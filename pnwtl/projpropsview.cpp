/**
 * @file projpropsview.cpp
 * @brief Project Properties
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "project.h"
#include "projectprops.h"
#include "projpropsview.h"

LRESULT CProjPropsView::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_props.SubclassWindow(GetDlgItem(IDC_LISTPROPS));
	m_tree.Attach(GetDlgItem(IDC_TREE));
	
	// Categorized properties TODO: make this optional.
	m_props.SetExtendedListStyle(PLS_EX_CATEGORIZED | PLS_EX_XPLOOK);

	displayGroups(m_pPropSet->GetGroups());
	
	m_tree.SelectItem( m_tree.GetRootItem() );

	CString s;
	s.Format(IDS_PROPSTITLE, _T("Project"));
	SetWindowText(s);

	CenterWindow(GetParent());

	return 0;
}

LRESULT CProjPropsView::OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	EndDialog(wID);
	return 0;
}

LRESULT CProjPropsView::OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	EndDialog(wID);
	return 0;
}

LRESULT CProjPropsView::OnTreeSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMTREEVIEW pN = reinterpret_cast<LPNMTREEVIEW>(pnmh);
	if(pnmh->code == TVN_SELCHANGED)
	{
		Projects::PropGroup* pGroup = reinterpret_cast<Projects::PropGroup*>( m_tree.GetItemData( pN->itemNew.hItem ) );
		if(pGroup != NULL)
		{
			selectGroup(pGroup);
		}
	}

	return 0;
}

void CProjPropsView::DisplayFor(Projects::ProjectType* pItem, Projects::ProjectTemplate* pTemplate)
{
	PNASSERT(pItem != NULL);
	PNASSERT(pTemplate != NULL);

	m_pCurItem = pItem;
	m_pPropSet = pTemplate->GetProperties(pItem->GetType());

	DoModal();	
}

void CProjPropsView::displayGroups(Projects::PropGroupList& groups, HTREEITEM hParent)
{
	HTREEITEM hItem = NULL;

	for(Projects::PropGroupList::const_iterator i = groups.begin();
		i != groups.end();
		++i)
	{
		hItem = m_tree.InsertItem((*i)->GetName(), hParent, hItem);
		m_tree.SetItemData(hItem, reinterpret_cast<DWORD_PTR>( (*i) ));

		displayGroups((*i)->GetSubGroups(), hItem);
	}
}

void CProjPropsView::displayCategories(Projects::PropCatList& categories)
{
	for(Projects::PropCatList::const_iterator j = categories.begin();
			j != categories.end();
			++j)
	{
        m_props.AddItem( PropCreateCategory((*j)->GetDescription()) );
		displayProperties( (*j)->GetProperties() );
	}
}

void CProjPropsView::displayProperties(Projects::PropList& properties)
{
	for(Projects::PropList::const_iterator i = properties.begin();
		i != properties.end();
		++i)
	{
		switch((*i)->GetType())
		{

		case Projects::propBool:
			//m_props.AddItem( PropCreateSimple((*i)->GetDescription(), false) );
			m_props.AddItem( PropCreateCheckButton((*i)->GetDescription(), false));
			break;

		case Projects::propInt:
			m_props.AddItem( PropCreateSimple((*i)->GetDescription(), 0) );
			break;

		case Projects::propChoice:
			{
				CPropertyListItem* pListItem = new CPropertyListItem((*i)->GetDescription(), 0);

				const Projects::ValueList& values = static_cast<Projects::ListProp*>((*i))->GetValues();
				
				for(Projects::ValueList::const_iterator j = values.begin();
					j != values.end();
					++j)
				{
					pListItem->AddListItem( (*j)->Description.c_str() );
				}

				m_props.AddItem(pListItem);
			}
			break;

		case Projects::propString:
			m_props.AddItem( PropCreateSimple((*i)->GetDescription(), _T("")) );
			break;

		case Projects::propLongString:
			m_props.AddItem( PropCreateSimple((*i)->GetDescription(), _T("")) );
			break;

		case Projects::propFile:
			m_props.AddItem( PropCreateFileName((*i)->GetDescription(), _T("")) );
			break;

		case Projects::propFolder:
			m_props.AddItem( PropCreateFileName((*i)->GetDescription(), _T("")) );
			break;
		}
	}
}

void CProjPropsView::selectGroup(Projects::PropGroup* group)
{
	displayCategories(group->GetCategories());
	m_pCurGroup = group;
}