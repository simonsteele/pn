/**
 * @file projpropsview.cpp
 * @brief Project Properties
 * @author Simon Steele
 * @note Copyright (c) 2004-2005 Simon Steele <s.steele@pnotepad.org>
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
	// Make sure we get all of the modified options (if any);
	transferOptions();
	m_pCurItem->GetUserData() = m_nodeData;

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

bool CProjPropsView::DisplayFor(Projects::ProjectType* pItem, Projects::ProjectTemplate* pTemplate)
{
	PNASSERT(pItem != NULL);
	PNASSERT(pTemplate != NULL);

	m_pCurItem = pItem;
	m_pPropSet = pTemplate->GetProperties(pItem->GetType());
	m_namespace = pTemplate->GetNamespace();
	m_nodeData = pItem->GetUserData();

	return DoModal() == IDOK;
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

void CProjPropsView::displayCategories(LPCTSTR groupName, Projects::PropCatList& categories)
{
	for(Projects::PropCatList::const_iterator j = categories.begin();
			j != categories.end();
			++j)
	{
        HPROPERTY hProp = m_props.AddItem( PropCreateCategory((*j)->GetDescription()) );
		hProp->SetItemData( reinterpret_cast<LPARAM>( (*j) ) );
		displayProperties( groupName, (*j)->GetName(), (*j)->GetProperties() );
	}
}

HPROPERTY CProjPropsView::createPropertyItem(
		Projects::ProjectProp* prop, 
		LPCTSTR groupName,
		LPCTSTR catName
		)
{
	HPROPERTY ret = NULL;
	switch(prop->GetType())
	{

	case Projects::propBool:
		{
			//ret = m_props.AddItem( PropCreateSimple(prop->GetDescription(), false) );
			bool bVal = m_nodeData.Lookup(m_namespace.c_str(), groupName, catName, prop->GetName(), false);
			ret = PropCreateCheckButton(prop->GetDescription(), bVal);
		}
		break;

	case Projects::propInt:
		{
			int iVal = m_nodeData.Lookup(m_namespace.c_str(), groupName, catName, prop->GetName(), 0);
			ret = PropCreateSimple(prop->GetDescription(), iVal);
		}
		break;

	case Projects::propChoice:
		{
			CPropertyListItem* pListItem = new CPropertyListItem(prop->GetDescription(), 0);

			LPCTSTR szVal = m_nodeData.Lookup(m_namespace.c_str(), groupName, catName, prop->GetName(), _T(""));
			LPCTSTR szSetVal = NULL; //TODO: Need to do the default here.

			const Projects::ValueList& values = static_cast<Projects::ListProp*>(prop)->GetValues();
			
			for(Projects::ValueList::const_iterator j = values.begin();
				j != values.end();
				++j)
			{
				if((*j)->Value == szVal)
					szSetVal = (*j)->Description.c_str();

				pListItem->AddListItem( (*j)->Description.c_str() );
			}

			if(szSetVal != NULL)
			{
				CComVariant v = szSetVal;
				pListItem->SetValue(v);
			}

			ret = pListItem;
		}
		break;

	case Projects::propString:
		{
			LPCTSTR szVal = m_nodeData.Lookup(m_namespace.c_str(), groupName, catName, prop->GetName(), _T(""));
			ret = PropCreateSimple(prop->GetDescription(), szVal != NULL ? szVal : _T(""));
		}
		break;

	case Projects::propLongString:
		{
			LPCTSTR szVal = m_nodeData.Lookup(m_namespace.c_str(), groupName, catName, prop->GetName(), _T(""));
			ret = PropCreateSimple(prop->GetDescription(), szVal != NULL ? szVal : _T(""));
		}
		break;

	case Projects::propFile:
		{
			LPCTSTR szVal = m_nodeData.Lookup(m_namespace.c_str(), groupName, catName, prop->GetName(), _T(""));
			ret = PropCreateFileName(prop->GetDescription(), szVal != NULL ? szVal : _T(""));
		}
		break;

	case Projects::propFolder:
		{
			LPCTSTR szVal = m_nodeData.Lookup(m_namespace.c_str(), groupName, catName, prop->GetName(), _T(""));
			ret = PropCreateFileName(prop->GetDescription(), szVal != NULL ? szVal : _T(""));
		}
		break;
	}

	return m_props.AddItem(ret);
}

void CProjPropsView::displayProperties(LPCTSTR groupName, LPCTSTR catName, Projects::PropList& properties)
{
	for(Projects::PropList::const_iterator i = properties.begin();
		i != properties.end();
		++i)
	{
		// Create the correct type of property::
		HPROPERTY hProp = createPropertyItem((*i), groupName, catName);
		hProp->SetItemData( reinterpret_cast<LPARAM>( (*i) ) );
	}
}

void CProjPropsView::clear()
{
	m_props.ResetContent();
}

void CProjPropsView::selectGroup(Projects::PropGroup* group)
{
	if(m_props.GetCount() > 0)
	{
		transferOptions();
		clear();
	}
	displayCategories(group->GetName(), group->GetCategories());
	m_pCurGroup = group;
}

void CProjPropsView::transferOptions()
{
	int propCount = m_props.GetCount();
	tstring currentCat;
	for(int i = 0; i < propCount; i++)
	{
		HPROPERTY hDispProp = m_props.GetProperty(i);
		PNASSERT(hDispProp != NULL);

		// If it's a category, we extract the name of the category for lookups.
		if(hDispProp->GetKind() == PROPKIND_CATEGORY)
		{
			Projects::PropCategory* pCat = (Projects::PropCategory*)hDispProp->GetItemData();
			currentCat = pCat->GetName();
			continue;
		}
		
		// Otherwise it's a property so we transfer the selected value.
		Projects::ProjectProp* prop = reinterpret_cast<Projects::ProjectProp*>( hDispProp->GetItemData() );
		PNASSERT(prop != NULL);

		CComVariant var;
		hDispProp->GetValue(&var);
		
		LPCTSTR catName = currentCat.c_str();
		LPCTSTR groupName = m_pCurGroup->GetName();

		switch(prop->GetType())
		{
			case Projects::propBool:
			{
				m_nodeData.Set(m_namespace.c_str(), groupName, catName, prop->GetName(), var.boolVal != FALSE);
			}
			break;

			case Projects::propInt:
			{
				m_nodeData.Set(m_namespace.c_str(), groupName, catName, prop->GetName(), var.intVal);
			}
			break;

			case Projects::propChoice:
			{
				Projects::ListProp* pListProp = reinterpret_cast<Projects::ListProp*>( prop );
				const Projects::ValueList& values = pListProp->GetValues();

				// Find the selected value by its index.
				int selIndex = var.intVal;
				Projects::ValueList::const_iterator j = values.begin();
				for(int i = 0; i < selIndex; i++)
					j++;

				m_nodeData.Set(m_namespace.c_str(), groupName, catName, prop->GetName(), (*j)->Value.c_str());
			}
			break;

			case Projects::propString:
			case Projects::propLongString:
			case Projects::propFile:
			case Projects::propFolder:
			{
				CString str;
				str = var;
				m_nodeData.Set(m_namespace.c_str(), groupName, catName, prop->GetName(), str);
			}
			break;
		}
	}
}