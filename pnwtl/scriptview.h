/**
 * @file scriptview.h
 * @brief Scripts Docker
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef scriptview_h__included
#define scriptview_h__included

class CThemedTree : public CTreeViewCtrl, public CThemeImpl<CThemedTree>
{
};

class CScriptDocker : public CWindowImpl<CScriptDocker>,
	public IScriptRegistryEventSink
{
	typedef CScriptDocker thisClass;
	typedef CWindowImpl<CScriptDocker> baseClass;

public:
	DECLARE_WND_CLASS(_T("CScriptDocker"))

	CScriptDocker();

	enum {
		IDC_SCRIPTSLIST = 100,
	};

	BEGIN_MSG_MAP(thisClass)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_SIZE, OnSize)
		MESSAGE_HANDLER(WM_CTLCOLOREDIT, OnCtlColor)
		NOTIFY_HANDLER(IDC_SCRIPTSLIST, NM_DBLCLK, OnTreeDblClick)
		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()

	virtual void OnScriptAdded(ScriptGroup* group, Script* script);
	virtual void OnScriptRemoved(ScriptGroup* group, Script* script);

protected:
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnSize(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled);
	LRESULT OnCtlColor(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);
	LRESULT OnTreeDblClick(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	HTREEITEM findScript(LPCTSTR group, LPCTSTR name);
	HTREEITEM findGroup(LPCTSTR name);
	HTREEITEM addGroup(LPCTSTR name);
	HTREEITEM addScript(HTREEITEM group, Script* script);

	void buildInitial();

protected:
	CThemedTree	m_view;
};

#endif // #ifndef scriptview_h__included