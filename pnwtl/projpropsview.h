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

class CProjPropsView : public CDialogImpl<CProjPropsView>
{
public:
	enum { IDD = IDD_PROJPROPS };

	BEGIN_MSG_MAP(CProjPropsView)
		MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
		COMMAND_ID_HANDLER(IDOK, OnOK)
		COMMAND_ID_HANDLER(IDCANCEL, OnCancel)
		//NOTIFY_HANDLER(IDC_LIST1, PIN_BROWSE, OnBrowse)
		NOTIFY_CODE_HANDLER(TVN_SELCHANGED, OnTreeSelChanged)
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	bool DisplayFor(Projects::ProjectType* pItem, Projects::ProjectTemplate* pTemplate);

protected:
	LRESULT OnInitDialog(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnOK(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnCancel(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT OnTreeSelChanged(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	void displayGroups(Projects::PropGroupList& groups, HTREEITEM hParent = NULL);
	void displayCategories(LPCTSTR groupName, Projects::PropCatList& categories);
	void displayProperties(LPCTSTR groupName, LPCTSTR catName, Projects::PropList& properties);

	HPROPERTY createPropertyItem(Projects::ProjectProp* prop, LPCTSTR groupName, LPCTSTR catName);

	void clear();
	void selectGroup(Projects::PropGroup* group);
	void transferOptions();

protected:
	CPropertyListCtrl		m_props;
	CTreeViewCtrl			m_tree;
	Projects::PropSet*		m_pPropSet;
	Projects::PropGroup*	m_pCurGroup;
	Projects::ProjectType*	m_pCurItem;
	Projects::UserData		m_nodeData;
	tstring					m_namespace;
};

#endif // #ifndef projpropsview_h__included