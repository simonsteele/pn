/**
 * @file projpropsview.h
 * @brief Project Properties
 * @author Simon Steele
 * @note Copyright (c) 2004-2005 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef projpropsview_h__included
#define projpropsview_h__included

#include "third_party/viksoe_proplist/propertylist.h"

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
	Projects::ProjectTemplate* GetTemplate() const;
	Projects::PROJECT_TYPE GetItemType() const;
	
	bool IsValid() const;
	void TransferSettings();

	Projects::PropGroupList* PropertyGroups;
	tstring PropNamespace;

protected:
	Projects::PROJECT_TYPE m_type;
	Projects::UserData m_userData;
	Projects::UserData* m_realUserData;
	Projects::ProjectTemplate* m_pTemplate;
};

namespace ProjPropsInternal
{
	/**
	 * @brief Map a displayed property group to its source PropViewSet instance.
	 */
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

/**
 * @brief Project Properties Dialog
 */
class CProjPropsView : public CDialogImpl<CProjPropsView>, 
						public CDialogResize<CProjPropsView>
{
public:
	CProjPropsView();
	~CProjPropsView();

	enum { IDD = IDD_PROJPROPS };

	BEGIN_MSG_MAP(CProjPropsView)
		MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
		MESSAGE_HANDLER(WM_HELP, OnHelp)
		COMMAND_ID_HANDLER(IDOK, OnOK)
		COMMAND_ID_HANDLER(IDCANCEL, OnCancel)
		COMMAND_ID_HANDLER(IDHELP, OnHelp)
		NOTIFY_HANDLER(IDC_LISTPROPS, PIN_BROWSE, OnBrowse)
		NOTIFY_CODE_HANDLER(TVN_SELCHANGED, OnTreeSelChanged)
		REFLECT_NOTIFICATIONS()
		CHAIN_MSG_MAP( CDialogResize<CProjPropsView> )
	END_MSG_MAP()

	BEGIN_DLGRESIZE_MAP(CProjPropsView)
		BEGIN_DLGRESIZE_GROUP()
			DLGRESIZE_CONTROL(IDC_TREE, DLSZ_SIZE_X)	
			DLGRESIZE_CONTROL(IDC_LISTPROPS, DLSZ_SIZE_X)	
		END_DLGRESIZE_GROUP()
		DLGRESIZE_CONTROL(IDC_LISTPROPS, DLSZ_SIZE_Y)
		DLGRESIZE_CONTROL(IDC_TREE, DLSZ_SIZE_Y)
		DLGRESIZE_CONTROL(IDOK, DLSZ_MOVE_X | DLSZ_MOVE_Y)
		DLGRESIZE_CONTROL(IDCANCEL, DLSZ_MOVE_X | DLSZ_MOVE_Y)
		DLGRESIZE_CONTROL(IDHELP, DLSZ_MOVE_Y)
    END_DLGRESIZE_MAP()

	void SetExtraSet(PropViewSet* viewSet);
	bool DisplayFor(PropViewSet* viewSet);

protected:
	LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnHelp(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	
	LRESULT OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnHelp(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnTreeSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
	LRESULT OnBrowse(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	void displayGroups(PropViewSet* viewSet, HTREEITEM hParent = NULL);
	void displayGroups(Projects::PropGroupList& groups, PropViewSet* viewSet, HTREEITEM hParent = NULL);
	void displayCategories(LPCTSTR groupName, Projects::PropCatList& categories);
	void displayProperties(LPCTSTR groupName, LPCTSTR catName, Projects::PropList& properties);

	HPROPERTY createPropertyItem(Projects::ProjectProp* prop, LPCTSTR groupName, LPCTSTR catName);

	void clear();
	void selectGroup(ProjPropsInternal::DisplayGroup* group);
	void showHelpFor(Projects::ProjectProp* prop, LPPOINT pt);
	void transferOptions();

protected:
	CPropertyListCtrl		m_props;
	CTreeViewCtrl			m_tree;
	
	PropViewSet*			m_pExtraSet;
	PropViewSet*			m_pMainSet;
	
	Projects::UserData*		m_pNodeData;
	tstring					m_namespace;

	ProjPropsInternal::DG_LIST			m_dglist;
	ProjPropsInternal::DisplayGroup*	m_pCurGroup;
};

#endif // #ifndef projpropsview_h__included