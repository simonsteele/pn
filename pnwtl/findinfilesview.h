#ifndef findinfiles_h__included_33B3B680_2120_4038_97EA_A109AAE08AB0
#define findinfiles_h__included_33B3B680_2120_4038_97EA_A109AAE08AB0

class CFindInFilesView : public CWindowImpl<CFindInFilesView>
{
public:
	BEGIN_MSG_MAP(CFindInFilesView)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_SIZE, OnSize)
	END_MSG_MAP()

	void AddResult(LPCTSTR file, int line, LPCTSTR str);

protected:
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

protected:
	CListViewCtrl	m_list;
};

#endif //#ifndef findinfiles_h__included_33B3B680_2120_4038_97EA_A109AAE08AB0