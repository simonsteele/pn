#ifndef browseview_h__included
#define browseview_h__included

class CShellTreeCtrl;

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

	CShellTreeCtrl*		m_view;
};

#endif // #ifndef browseview_h__included