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

	/*if(m_state == OS_DEFAULT)
	{
		if(_tcscmp(name, _T("optionset")) == 0)
		{
			LPCTSTR title = atts.getValue(_T("title"));
			if(!title)
				return;

			m_list.AddItem( PropCreateCategory(title) );
			m_state = OS_OPTIONS;
		}
	}
	else if(m_state == OS_OPTIONS)
	{
		LPCTSTR title = atts.getValue(_T("title"));
		LPCTSTR elname = atts.getValue(_T("name"));

		if(_tcscmp(name, _T("folderpath")) == 0)
		{
			m_list.AddItem( PropCreateFileName(title, _T("")) );
		}
		else if(_tcscmp(name, _T("option")) == 0)
		{
			m_list.AddItem( PropCreateSimple(title, false));
		}
		
		else if(_tcscmp(name, _T("optionlist")) == 0)
		{
			m_state = OS_OPTLIST;
			m_pListItem = static_cast<CPropertyListItem*>( PropCreateList(title, NULL) );
		}
		else if(_tcscmp(name, _T("text")) == 0)
		{
			m_list.AddItem( PropCreateSimple(title, _T("")));
		}
	}
	else if(m_state == OS_OPTLIST)
	{
		if(_tcscmp(name, _T("value")) == 0)
			onValue(atts);
	}*/


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
			m_props.AddItem( PropCreateSimple((*i)->GetDescription(), _T("longstring")) );
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