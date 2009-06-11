/**
 * @file findinfilesview.cpp
 * @brief Find In Files View
 * @author Simon Steele
 * @note Copyright (c) 2005-2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef findinfilesview_h__included_33B3B680_2120_4038_97EA_A109AAE08AB0
#define findinfilesview_h__included_33B3B680_2120_4038_97EA_A109AAE08AB0

#define PN_FIFFINISH	(PN_FIFMATCH+1)
#define PN_FIFSTART		(PN_FIFMATCH+2)
#include <queue>

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
		MESSAGE_HANDLER(PN_FIFMATCH, OnFIFMatch)
		MESSAGE_HANDLER(PN_FIFFINISH, OnFIFFinish)
		MESSAGE_HANDLER(PN_FIFSTART, OnFIFStart)
		NOTIFY_HANDLER(IDC_FIF_LIST, NM_DBLCLK, OnListDblClk)
		NOTIFY_HANDLER(IDC_FIF_LIST, NM_RETURN, OnReturn)
	END_MSG_MAP()

	void AddResult(LPCTSTR file, int line, LPCTSTR str);
	void Clear();
	int GetResultCount() const;

// Implement FIFSink
public:
	virtual void OnBeginSearch(LPCTSTR stringLookingFor, bool bIsRegex);
	virtual void OnEndSearch(int nFound, int nFiles);
	virtual void OnFoundString(LPCTSTR stringFound, LPCTSTR szFilename, int line, LPCTSTR buf);

private:
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnFIFMatch(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnFIFFinish(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnFIFStart(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	LRESULT OnListDblClk(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
	LRESULT OnReturn(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	void handleUserSelection(int index);

private:
	class FIFMatch
	{
	public:
		FIFMatch(LPCTSTR szFileName, int line, LPCTSTR szBuf);
		FIFMatch(const FIFMatch& other);
		~FIFMatch();

		TCHAR*	FileName;
		int		Line;
		TCHAR*	Buf;
	};

	CListViewCtrl	m_list;
	int				m_nItems;
	DWORD			m_dwStartTicks;
	tstring			m_lookingFor;
	TCHAR			m_NCBuf[40];
	std::queue<FIFMatch> m_matchQueue;
};

#endif //#ifndef findinfilesview_h__included_33B3B680_2120_4038_97EA_A109AAE08AB0