#ifndef scriptview_h__included
#define scriptview_h__included

class CScriptDocker : public CWindowImpl<CScriptDocker>
{
	typedef CScriptDocker thisClass;
	typedef CWindowImpl<CScriptDocker> baseClass;

public:
	DECLARE_WND_CLASS(_T("CScriptDocker"))

	enum {
		IDC_SCRIPTSLIST = 100,
	};

	BEGIN_MSG_MAP(thisClass)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_SIZE, OnSize)
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

protected:
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnSize(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled);

protected:
	CTreeViewCtrl	m_view;
};

#endif // #ifndef scriptview_h__included