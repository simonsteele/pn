/**
 * @file projpropsview.h
 * @brief Project Properties
 * @author Simon Steele
 * @note Copyright (c) 2004-2005 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef projpropsview_h__included
#define projpropsview_h__included

#include "include/proplist/propertylist.h"

/**
 * This class represents a meta data item and a set of properties that
 * that meta data item represents.
 */
class PropViewSet
{
public:
	PropViewSet(Projects::UserData* UserData);
	PropViewSet(Projects::ProjectTemplate* theTemplate, Projects::ProjectType* item);
	Projects::UserData* GetUserData();
	
	bool IsValid();
	void TransferSettings();

	Projects::PropGroupList* PropertyGroups;
	tstring PropNamespace;

protected:
	Projects::UserData m_userData;
	Projects::UserData* m_realUserData;
};

namespace ProjPropsInternal
{
	class DisplayGroup
	{
	public:
		DisplayGroup(PropViewSet* viewSet, Projects::PropGroup* propGroup) :
		ViewSet(viewSet), Group(propGroup){}
		
		PropViewSet* ViewSet;
		Projects::PropGroup* Group;
	};

	typedef std::list<DisplayGroup*> DG_LIST;
}

class CProjPropsView : public CDialogImpl<CProjPropsView>
{
public:
	CProjPropsView();
	~CProjPropsView();

	enum { IDD = IDD_PROJPROPS };

	BEGIN_MSG_MAP(CProjPropsView)
		MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
		COMMAND_ID_HANDLER(IDOK, OnOK)
		COMMAND_ID_HANDLER(IDCANCEL, OnCancel)
		//NOTIFY_HANDLER(IDC_LIST1, PIN_BROWSE, OnBrowse)
		NOTIFY_CODE_HANDLER(TVN_SELCHANGED, OnTreeSelChanged)
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	void SetExtraSet(PropViewSet* viewSet);
	bool DisplayFor(PropViewSet* viewSet);

protected:
	LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnTreeSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	void displayGroups(PropViewSet* viewSet, HTREEITEM hParent = NULL);
	void displayGroups(Projects::PropGroupList& groups, PropViewSet* viewSet, HTREEITEM hParent = NULL);
	void displayCategories(LPCTSTR groupName, Projects::PropCatList& categories);
	void displayProperties(LPCTSTR groupName, LPCTSTR catName, Projects::PropList& properties);

	HPROPERTY createPropertyItem(Projects::ProjectProp* prop, LPCTSTR groupName, LPCTSTR catName);

	void clear();
	void selectGroup(ProjPropsInternal::DisplayGroup* group);
	void transferOptions();

protected:
	CPropertyListCtrl		m_props;
	CTreeViewCtrl			m_tree;
	
	PropViewSet*			m_pExtraSet;
	PropViewSet*			m_pMainSet;
	
	Projects::PropGroup*	m_pCurGroup;
	Projects::UserData*		m_pNodeData;
	tstring					m_namespace;

	ProjPropsInternal::DG_LIST	m_dglist;
};

#endif // #ifndef projpropsview_h__included