#ifndef findinfiles_h__included_33B3B680_2120_4038_97EA_A109AAE08AB0
#define findinfiles_h__included_33B3B680_2120_4038_97EA_A109AAE08AB0

class CFindInFilesView : public CWindowImpl<CFindInFilesView>, public FIFSink
{
public:
	CFindInFilesView();

	enum {
		IDC_FIF_LIST = 100,
	};

	BEGIN_MSG_MAP(CFindInFilesView)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_SIZE, OnSize)
		NOTIFY_HANDLER(IDC_FIF_LIST, NM_DBLCLK, OnListDblClk)
	END_MSG_MAP()

	void AddResult(LPCTSTR file, int line, LPCTSTR str);
	void Clear();

// Implement FIFSink
public:
	virtual void OnBeginSearch(LPCTSTR stringLookingFor, bool bIsRegex);
	virtual void OnEndSearch(int nFound, int nFiles);
	virtual void OnFoundString(LPCTSTR stringFound, LPCTSTR szFilename, int line, LPCTSTR buf);

protected:
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	LRESULT OnListDblClk(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

protected:
	CListViewCtrl	m_list;
	int				m_nItems;
	DWORD			m_dwStartTicks;
	TCHAR			m_NCBuf[40];
};

#endif //#ifndef findinfiles_h__included_33B3B680_2120_4038_97EA_A109AAE08AB0