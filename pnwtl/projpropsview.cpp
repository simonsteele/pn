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
	
	// Categorized properties TODO: make this optional.
	m_props.SetExtendedListStyle(PLS_EX_CATEGORIZED | PLS_EX_XPLOOK);

	//m_props.AddItem( PropCreateSimple(_T("Property"), false) );
	//m_props.AddItem( PropCreateCategory(_T("Category")) );
	//m_props.AddItem( PropCreateSimple(_T("Property"), false) );

	displayGroups(m_pPropSet->GetGroups());

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

void CProjPropsView::DisplayFor(Projects::ProjectType* pItem, Projects::ProjectTemplate* pTemplate)
{
	m_pPropSet = pTemplate->GetProperties(pItem->GetType());
	//pTemplate->
	//Projects::PropSet* pSet = 

	DoModal();

	//Projects::PropGroupList& groups = pSet->GetGroups();
	
}

void CProjPropsView::displayGroups(Projects::PropGroupList& groups, HTREEITEM hParent)
{
	for(Projects::PropGroupList::const_iterator i = groups.begin();
		i != groups.end();
		++i)
	{
		HTREEITEM hItem = NULL;

		// Later this will happen on a tree item selection.
		displayCategories((*i)->GetCategories());

		displayGroups((*i)->GetSubGroups(), hItem);
	}
}

void CProjPropsView::displayCategories(Projects::PropCatList& categories)
{
	for(Projects::PropCatList::const_iterator j = categories.begin();
			j != categories.end();
			++j)
	{
        m_props.AddItem( PropCreateCategory((*j)->GetName()) );
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
		case Projects::ptBool:
			m_props.AddItem( PropCreateSimple((*i)->GetName(), false) );
			break;
		case Projects::ptInt:
			m_props.AddItem( PropCreateSimple((*i)->GetName(), 0) );
			break;
		case Projects::ptChoice:
			//m_props.AddItem( PropC
			break;
		case Projects::ptString:
			m_props.AddItem( PropCreateSimple((*i)->GetName(), _T("")) );
			break;
		case Projects::ptLongString:
			m_props.AddItem( PropCreateSimple((*i)->GetName(), _T("longstring")) );
			break;
		}
	}
}