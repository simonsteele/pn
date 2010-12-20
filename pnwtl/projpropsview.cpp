/**
 * @file projpropsview.cpp
 * @brief Project Properties
 * @author Simon Steele
 * @note Copyright (c) 2004-2010 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "project.h"
#include "projectprops.h"
#include "projpropsview.h"
#include "pndialogs.h"

#include <htmlhelp.h>

using namespace ProjPropsInternal;

PropViewSet::PropViewSet(Projects::UserData* userData)
{
	m_userData = *userData;
	m_realUserData = userData;
	m_pTemplate = NULL;
}

PropViewSet::PropViewSet(Projects::ProjectTemplate* theTemplate, Projects::ProjectType* lastItem)
{
	PNASSERT(lastItem != NULL);

	m_type = lastItem->GetType();

	m_pTemplate = theTemplate;

	m_userData = lastItem->GetUserData();
	m_realUserData = &lastItem->GetUserData();
	
	if(theTemplate)
	{
		PropNamespace = theTemplate->GetNamespace();
		Projects::PropSet* ps = theTemplate->GetProperties(lastItem->GetType());
		if(ps)
			PropertyGroups = &ps->GetGroups();
		else
			PropertyGroups = NULL;
	}
	else
	{
		PropNamespace = _T("");
		PropertyGroups = NULL;
	}
}

Projects::UserData* PropViewSet::GetUserData()
{
	return &m_userData;
}

Projects::ProjectTemplate* PropViewSet::GetTemplate() const
{
	return m_pTemplate;
}

Projects::PROJECT_TYPE PropViewSet::GetItemType() const
{
	return m_type;
}

bool PropViewSet::IsValid() const
{
	return (PropertyGroups != NULL && m_realUserData != NULL && PropertyGroups->size() > 0);
}

void PropViewSet::TransferSettings()
{
	*m_realUserData = m_userData;
}

CProjPropsView::CProjPropsView()
{
	m_pExtraSet = NULL;
}

CProjPropsView::~CProjPropsView()
{
	for(DG_LIST::const_iterator i = m_dglist.begin(); i != m_dglist.end(); ++i)
	{
		delete (*i);
	}
	m_dglist.clear();
}

LRESULT CProjPropsView::OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	m_props.SubclassWindow(GetDlgItem(IDC_LISTPROPS));
	m_tree.Attach(GetDlgItem(IDC_TREE));
	
	// Categorized properties TODO: make this optional.
	m_props.SetExtendedListStyle(PLS_EX_CATEGORIZED | PLS_EX_XPLOOK);

	if(m_pExtraSet->IsValid())
		displayGroups(m_pExtraSet);

	if(m_pMainSet->IsValid())
		displayGroups(m_pMainSet);
	
	m_tree.SelectItem( m_tree.GetRootItem() );

	CString s;

	switch(m_pMainSet->GetItemType())
	{
	case Projects::ptProject:
		s.Format(IDS_PROPSTITLE, _T("Project"));
		break;
	case Projects::ptWorkspace:
		s.Format(IDS_PROPSTITLE, _T("Project Group"));
		break;
	case Projects::ptMagicFolder:
	case Projects::ptFolder:
		s.Format(IDS_PROPSTITLE, _T("Folder"));
		break;
	case Projects::ptFile:
		s.Format(IDS_PROPSTITLE, _T("File"));
		break;
	}
	
	SetWindowText(s);

	CenterWindow(GetParent());

	CButton(GetDlgItem(IDHELP)).EnableWindow(FALSE);

	DlgResize_Init();

	return 0;
}

LRESULT CProjPropsView::OnHelp(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	LPHELPINFO lphi = (LPHELPINFO)lParam;
	if(lphi->iContextType == HELPINFO_WINDOW)
	{
		// Help on a window control...
		int idCtrl = 0;
		HWND hWndParent = (HWND)lphi->hItemHandle;
		
		// Seek either the properties window or the dialog (in which case who knows
		// what control it was).
		while(hWndParent != m_props.m_hWnd && hWndParent != m_hWnd)
			hWndParent = ::GetParent(hWndParent);

		if(hWndParent == m_props.m_hWnd)
			idCtrl = IDC_LISTPROPS;
		else
			idCtrl = lphi->iCtrlId;

		switch(idCtrl)
		{
		case IDC_TREE:
			{
				::MessageBox(m_hWnd, _T("The Tree"), _T("Help for..."), MB_OK);
			}
			break;
		case IDC_LISTPROPS:
			{
				// F1 pressed on the property list...
				int iSelIndex = m_props.GetCurSel();
				HPROPERTY hProp = m_props.GetProperty(iSelIndex);
				Projects::ProjectProp* pProp = reinterpret_cast<Projects::ProjectProp*>( hProp->GetItemData() );
				showHelpFor(pProp, &lphi->MousePos );
			}
			break;
		}
	}

	return 0;
}

LRESULT CProjPropsView::OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	// Make sure we get all of the modified options (if any);
	transferOptions();
	
	if(m_pMainSet->IsValid())
		m_pMainSet->TransferSettings();

	if(m_pExtraSet->IsValid())
		m_pExtraSet->TransferSettings();

	EndDialog(wID);
	return 0;
}

LRESULT CProjPropsView::OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	EndDialog(wID);
	return 0;
}

LRESULT CProjPropsView::OnHelp(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	return 0;
}

LRESULT CProjPropsView::OnTreeSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMTREEVIEW pN = reinterpret_cast<LPNMTREEVIEW>(pnmh);
	if(pnmh->code == TVN_SELCHANGED)
	{
		DisplayGroup* pGroup = reinterpret_cast<DisplayGroup*>( m_tree.GetItemData( pN->itemNew.hItem ) );
		if(pGroup != NULL)
		{
			selectGroup(pGroup);
		}
	}

	return 0;
}

LRESULT CProjPropsView::OnBrowse(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	NMPROPERTYITEM* pH = (NMPROPERTYITEM*)pnmh;
	
	CPropertyFileNameItem* pItem = static_cast<CPropertyFileNameItem*>(	pH->prop );
	Projects::ProjectProp* pProp = reinterpret_cast<Projects::ProjectProp*>( pItem->GetItemData() );
	
	if(pItem->IsDirectory())
	{
		CFolderDialog fd(m_hWnd, pProp->GetDescription());
		if(fd.DoModal() == IDOK)
		{
			CComVariant v = fd.GetFolderPath();
			pItem->SetValue( v );
		}
	}
	else
	{
		CAutoOpenDialog dlgOpen(LS(IDS_ALLFILES));
		dlgOpen.SetTitle(pProp->GetDescription());

		if(dlgOpen.DoModal() == IDOK)
		{
			CComVariant v = dlgOpen.GetSingleFileName();
			pItem->SetValue( v );
		}
	}

	return 0;
}

void CProjPropsView::SetExtraSet(PropViewSet* viewSet)
{
	m_pExtraSet = viewSet;
}

bool CProjPropsView::DisplayFor(PropViewSet* viewSet)
{
	PNASSERT(viewSet != NULL);

	m_pMainSet = viewSet;

	return DoModal() == IDOK;
}

void CProjPropsView::displayGroups(PropViewSet* viewSet, HTREEITEM hParent)
{
	Projects::PropGroupList& groups = *viewSet->PropertyGroups;
	displayGroups(groups, viewSet, hParent);
}

void CProjPropsView::displayGroups(Projects::PropGroupList& groups, PropViewSet* viewSet, HTREEITEM hParent)
{
	HTREEITEM hItem = NULL;
	for(Projects::PropGroupList::const_iterator i = groups.begin();
		i != groups.end();
		++i)
	{
		hItem = m_tree.InsertItem((*i)->GetDescription(), hParent, hItem);
		
		DisplayGroup* dg = new DisplayGroup(viewSet, (*i));
		m_dglist.insert(m_dglist.end(), dg);
        m_tree.SetItemData(hItem, reinterpret_cast<DWORD_PTR>( dg ));

		displayGroups((*i)->GetSubGroups(), viewSet, hItem);
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
			bool bVal = m_pNodeData->Lookup(m_namespace.c_str(), groupName, catName, prop->GetName(), prop->GetDefaultAsBool());
			ret = PropCreateCheckButton(prop->GetDescription(), bVal);
		}
		break;

	case Projects::propInt:
		{
			int iVal = m_pNodeData->Lookup(m_namespace.c_str(), groupName, catName, prop->GetName(), prop->GetDefaultAsInt());
			ret = PropCreateSimple(prop->GetDescription(), iVal);
		}
		break;

	case Projects::propChoice:
		{
			CPropertyListItem* pListItem = new CPropertyListItem(prop->GetDescription(), 0);

			LPCTSTR szVal = m_pNodeData->Lookup(m_namespace.c_str(), groupName, catName, prop->GetName(), prop->GetDefault());
			LPCTSTR szSetVal = NULL;

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
			LPCTSTR szVal = m_pNodeData->Lookup(m_namespace.c_str(), groupName, catName, prop->GetName(), prop->GetDefault());
			ret = PropCreateSimple(prop->GetDescription(), szVal != NULL ? szVal : _T(""));
		}
		break;

	case Projects::propLongString:
		{
			LPCTSTR szVal = m_pNodeData->Lookup(m_namespace.c_str(), groupName, catName, prop->GetName(), prop->GetDefault());
			ret = PropCreateSimple(prop->GetDescription(), szVal != NULL ? szVal : _T(""));
		}
		break;

	case Projects::propFile:
		{
			LPCTSTR szVal = m_pNodeData->Lookup(m_namespace.c_str(), groupName, catName, prop->GetName(), prop->GetDefault());
			ret = PropCreateFileName(prop->GetDescription(), szVal != NULL ? szVal : _T(""));
		}
		break;

	case Projects::propFolder:
		{
			LPCTSTR szVal = m_pNodeData->Lookup(m_namespace.c_str(), groupName, catName, prop->GetName(), prop->GetDefault());
			ret = PropCreatePathName(prop->GetDescription(), szVal != NULL ? szVal : _T(""));
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

void CProjPropsView::selectGroup(ProjPropsInternal::DisplayGroup* group)
{
	if(m_props.GetCount() > 0)
	{
		transferOptions();
		clear();
	}

	m_pNodeData = group->ViewSet->GetUserData();
	m_namespace = group->ViewSet->PropNamespace;

	displayCategories(group->Group->GetName(), group->Group->GetCategories());
	m_pCurGroup = group;
}

void CProjPropsView::showHelpFor(Projects::ProjectProp* prop, LPPOINT pt)
{
	PNASSERT(prop != NULL);
	int iID = prop->GetHelpId();
	if(iID != 0)
	{
		Projects::ProjectTemplate* pTemplate = m_pCurGroup->ViewSet->GetTemplate();
		if(pTemplate != NULL)
		{
			LPCTSTR hf = pTemplate->GetHelpFile();
			if(hf != NULL)
			{
				// Combine help filename with projecttemplates path.
				tstring path;
				OPTIONS->GetPNPath(path, PNPATH_PROJECTTEMPLATES);
				CFileName fn(hf);
				fn.Root(path.c_str());
				path = fn.c_str();

				// Show that help!
				HH_POPUP popup = {sizeof(HH_POPUP), 0, iID, 0, {pt->x, pt->y}, static_cast<COLORREF>(-1), static_cast<COLORREF>(-1), {-1,-1,-1,-1}, NULL};
				::HtmlHelp(m_hWnd, path.c_str(), HH_DISPLAY_TEXT_POPUP, (DWORD_PTR)&popup);
			}
		}
	}
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
		LPCTSTR groupName = m_pCurGroup->Group->GetName();

		switch(prop->GetType())
		{
			case Projects::propBool:
			{
				m_pNodeData->Set(m_namespace.c_str(), groupName, catName, prop->GetName(), var.boolVal != FALSE);
			}
			break;

			case Projects::propInt:
			{
				m_pNodeData->Set(m_namespace.c_str(), groupName, catName, prop->GetName(), var.intVal);
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

				m_pNodeData->Set(m_namespace.c_str(), groupName, catName, prop->GetName(), (*j)->Value.c_str());
			}
			break;

			case Projects::propString:
			case Projects::propLongString:
			case Projects::propFile:
			case Projects::propFolder:
			{
				CString str;
				str = var;
				m_pNodeData->Set(m_namespace.c_str(), groupName, catName, prop->GetName(), str);
			}
			break;
		}
	}
}