/**
 * @file openfilesview.h
 * @brief Docking window showing open files
 * @author Simon Steele
 * @note Copyright (c) 2008 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef openfilesview_h__included
#define openfilesview_h__included

class ShellContextMenu;

/**
 * Explorer docking window
 */
class COpenFilesDocker : public CWindowImpl<COpenFilesDocker>	
{
	typedef CWindowImpl<COpenFilesDocker> baseClass;

public:
	DECLARE_WND_CLASS(_T("COpenFilesDocker"))

	COpenFilesDocker();
	~COpenFilesDocker();
	
	enum {
		IDC_FILESLIST = 107,
	};

	BEGIN_MSG_MAP(COpenFilesDocker)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_SIZE, OnSize)
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy)
		MESSAGE_HANDLER(WM_GETMINMAXINFO, OnGetMinMaxInfo)
		MESSAGE_HANDLER(WM_CTLCOLOREDIT, OnCtlColor)
		MESSAGE_HANDLER(WM_CONTEXTMENU, OnContextMenu)

		NOTIFY_HANDLER(IDC_FILESLIST, NM_DBLCLK, OnListDblClk)
		NOTIFY_HANDLER(IDC_FILESLIST, NM_CLICK, OnListDblClk)
		NOTIFY_HANDLER(IDC_FILESLIST, LVN_GETINFOTIP, OnGetInfoTip);

		REFLECT_NOTIFICATIONS()
	END_MSG_MAP()
	
class AppEventSink;
class DocEventSink;

private:
	// Private, but called by inner sink classes
	void AddDocument(extensions::IDocumentPtr& doc);
	void RemoveDocument(extensions::IDocumentPtr& doc);
	void UpdateDocument(extensions::IDocumentPtr& doc);
	void SelectDocument(extensions::IDocumentPtr& doc);

	// Really Private:
	int findDocument(extensions::IDocumentPtr& doc);
	void handleUserSelection(int index);
	extensions::IDocument* docFromListItem(int item);

	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);
	LRESULT OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT	OnGetMinMaxInfo(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnContextMenu(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);

	LRESULT OnCtlColor(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);

	LRESULT OnListDblClk(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
	LRESULT OnGetInfoTip(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);

	CListViewCtrl m_view;
	CImageList m_images;
	extensions::IAppEventSinkPtr m_appSink;
	ShellContextMenu* m_explorerMenu;
};

#endif //#ifndef openfilesview_h__included