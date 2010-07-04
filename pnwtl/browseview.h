/**
 * @file browseview.h
 * @brief File Browser View
 * @author Simon Steele
 * @note Copyright (c) 2008 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef browseview_h__included
#define browseview_h__included

class CShellTreeCtrl;
class ShellContextMenu;

/**
 * Explorer docking window
 */
class CBrowseDocker : public CWindowImpl<CBrowseDocker>
{
	typedef CWindowImpl<CBrowseDocker> baseClass;

public:
	DECLARE_WND_CLASS(_T("CBrowseDocker"))

	CBrowseDocker();
	~CBrowseDocker();
	
	enum {
		IDC_BROWSETREE = 106,
	};

	BEGIN_MSG_MAP(CBrowseDocker)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_SIZE, OnSize)
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy)
		MESSAGE_HANDLER(WM_GETMINMAXINFO, OnGetMinMaxInfo)
		MESSAGE_HANDLER(WM_CTLCOLOREDIT, OnCtlColor)
		MESSAGE_HANDLER(WM_SHOWWINDOW, OnShow)
		
		NOTIFY_HANDLER(IDC_BROWSETREE, NM_DBLCLK, OnTreeDblClick)
		NOTIFY_HANDLER(IDC_BROWSETREE, NM_RCLICK, OnRightClick)

		COMMAND_ID_HANDLER(ID_BROWSER_REFRESH, OnRefresh)

		{
			if(handleSystemContextMenuMessage(hWnd, uMsg, wParam, lParam, lResult))
				return TRUE;
		}

		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

private:
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);
	LRESULT OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT	OnGetMinMaxInfo(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT	OnShow(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	LRESULT OnCtlColor(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);

	LRESULT OnTreeDblClick(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	LRESULT OnRightClick(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	LRESULT OnRefresh(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);

	LRESULT handleSystemContextMenuMessage(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam, LRESULT& lResult);

	HTREEITEM			m_rightClickNode;
	ShellContextMenu*	m_menuHandler;
	CShellTreeCtrl*		m_view;
};

#endif // #ifndef browseview_h__included