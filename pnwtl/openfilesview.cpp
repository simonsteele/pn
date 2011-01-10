/**
 * @file openfilesview.cpp
 * @brief Docking window showing open files
 * @author Simon Steele
 * @note Copyright (c) 2008-2011 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "openfilesview.h"
#include "extapp.h"
#include "childfrm.h"
#include "ExplorerMenu.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

/// Application Event Sink implementation for the open files window
class COpenFilesDocker::AppEventSink : public extensions::IAppEventSink
{
public:
	explicit AppEventSink(COpenFilesDocker* owner) : m_owner(owner)
	{
	}

	virtual ~AppEventSink(){}

	/// Called when a new document is opened/created
	virtual void OnNewDocument(extensions::IDocumentPtr& doc);
	
	/// Called when PN is closing (you are about to be unloaded!)
	virtual void OnAppClose();

	/// Called when the user switches to a different document
	virtual void OnDocSelected(extensions::IDocumentPtr& doc);

	/// Called when the very first Scintilla window is created, used for loading external lexers
	virtual void OnFirstEditorCreated(HWND hWndScintilla);

private:
	COpenFilesDocker* m_owner;
};

/// Called when a new document is opened/created
void COpenFilesDocker::AppEventSink::OnNewDocument(extensions::IDocumentPtr& doc)
{
	m_owner->AddDocument(doc);
}

/// Called when PN is closing (you are about to be unloaded!)
void COpenFilesDocker::AppEventSink::OnAppClose()
{
}

void COpenFilesDocker::AppEventSink::OnDocSelected(extensions::IDocumentPtr& doc)
{
	m_owner->SelectDocument(doc);
}

void COpenFilesDocker::AppEventSink::OnFirstEditorCreated(HWND hWndScintilla)
{
}

/// Document Event Sink implementation for the open files window
class COpenFilesDocker::DocEventSink : public extensions::IDocumentEventSink
{
public:
	DocEventSink(COpenFilesDocker* owner, extensions::IDocumentPtr& doc) : 
		m_owner(owner),
		m_doc(doc)
	{
	}

	virtual ~DocEventSink(){}

	/// Called when a character is added to the document
	virtual void OnCharAdded(char c){};

	/// Called when the scheme changes
	virtual void OnSchemeChange(const char* scheme){};
	
	/// Called when the document closes
	virtual void OnDocClosing()
	{
		m_owner->RemoveDocument(m_doc);
		m_doc.reset();
	}

	/// Called after a document is loaded
	virtual void OnAfterLoad()
	{
		m_owner->UpdateDocument(m_doc);
	}

	/// Called before the document is saved
	virtual void OnBeforeSave(const wchar_t* filename){}

	/// Called after the document is saved
	virtual void OnAfterSave()
	{
		m_owner->UpdateDocument(m_doc);
	}

	virtual void OnModifiedChanged(bool modified)
	{
		m_owner->UpdateDocument(m_doc);
	}

	virtual void OnWriteProtectChanged(bool writeProtect)
	{
		m_owner->UpdateDocument(m_doc);
	}

private:
	extensions::IDocumentPtr m_doc;
	COpenFilesDocker* m_owner;
};

COpenFilesDocker::COpenFilesDocker() :
	m_explorerMenu(new ShellContextMenu())
{	
	m_appSink.reset(new AppEventSink(this));
	g_Context.ExtApp->AddEventSink(m_appSink);
}

COpenFilesDocker::~COpenFilesDocker()
{
	delete m_explorerMenu;
}

LRESULT COpenFilesDocker::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	HICON hIconSmall = (HICON)::LoadImage(_Module.GetResourceInstance(), MAKEINTRESOURCE(IDI_OPENFILES), 
			IMAGE_ICON, ::GetSystemMetrics(SM_CXSMICON), ::GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);
	SetIcon(hIconSmall, FALSE);

	RECT rc;
	GetClientRect(&rc);
	
	m_view.Create(m_hWnd, rc, _T("OpenFilesList"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | LVS_REPORT | LVS_NOCOLUMNHEADER | LVS_SHOWSELALWAYS, 0, IDC_FILESLIST);
	m_view.AddColumn(_T(""), 0);
	m_view.SetColumnWidth(0, (rc.right-rc.left) -::GetSystemMetrics(SM_CXVSCROLL));
	
	if (WTL::RunTimeHelper::IsVista())
	{
		m_view.SetExtendedListViewStyle(LVS_EX_FULLROWSELECT | LVS_EX_INFOTIP | LVS_EX_DOUBLEBUFFER);
	}
	else
	{
		m_view.SetExtendedListViewStyle(LVS_EX_FULLROWSELECT | LVS_EX_INFOTIP);
	}

	m_images.Create(16, 16, ILC_COLOR32 | ILC_MASK, 3, 1);
	HBITMAP bmp = (HBITMAP)::LoadImage(ATL::_AtlBaseModule.GetResourceInstance(), MAKEINTRESOURCE(IDB_OPENFILES), IMAGE_BITMAP, 48, 16, LR_CREATEDIBSECTION | LR_DEFAULTSIZE);
	m_images.Add(bmp, RGB(255, 0 ,255));
	m_view.SetImageList(m_images, LVSIL_SMALL);
	
	bHandled = FALSE;

	return 0;
}

LRESULT COpenFilesDocker::OnSize(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
	if(wParam != SIZE_MINIMIZED)
	{
		RECT rc;
		GetClientRect(&rc);
		m_view.SetWindowPos(NULL, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top ,SWP_NOZORDER | SWP_NOACTIVATE);
		m_view.SetColumnWidth(0, (rc.right-rc.left)-::GetSystemMetrics(SM_CXVSCROLL));
	}

	bHandled = FALSE;

	return 0;
}

LRESULT COpenFilesDocker::OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	//Hide();

	return 0;
}

LRESULT COpenFilesDocker::OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = FALSE;

	return 0;
}

LRESULT COpenFilesDocker::OnGetMinMaxInfo(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	LPMINMAXINFO mmi = reinterpret_cast<LPMINMAXINFO>(lParam);
	mmi->ptMinTrackSize.x = 80;
	mmi->ptMinTrackSize.y = 100;
	return 0;
}

/**
 * For some reason the WM_CTLCOLOR* messages do not get to the child
 * controls with the docking windows (todo with reflection). This returns
 * the proper result.
 */
LRESULT COpenFilesDocker::OnCtlColor(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	return (LRESULT)::GetSysColorBrush( COLOR_WINDOW );
}

/// List was double clicked
LRESULT COpenFilesDocker::OnListDblClk(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMITEMACTIVATE lpnm = (LPNMITEMACTIVATE)pnmh;

	if (lpnm->iItem == -1)
	{
		return 0;
	}

	handleUserSelection( lpnm->iItem );

	return 0;
}

LRESULT COpenFilesDocker::OnGetInfoTip(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMLVGETINFOTIP pGetInfoTip = (LPNMLVGETINFOTIP)pnmh;

	extensions::IDocument* rawdoc = docFromListItem(pGetInfoTip->iItem);
	Document* pndoc = static_cast<Document*>(rawdoc);
	
	if (rawdoc && rawdoc->GetCanSave())
	{
		tstring str;
		if (pGetInfoTip->dwFlags == 0)
		{
			str = pGetInfoTip->pszText;
			str += _T(":\n");
			str += pndoc->GetFileName(FN_FULL);
		}
		else
		{
			str = pndoc->GetFileName(FN_FULL);
		}

		if (str.size() >= (size_t)(pGetInfoTip->cchTextMax - 3))
		{
			str.resize(pGetInfoTip->cchTextMax-4);
			str += _T("...");
		}
		
		_tcscpy(pGetInfoTip->pszText, str.c_str());
	}

	return true;
}

LRESULT COpenFilesDocker::OnContextMenu(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	if (GET_X_LPARAM(lParam) == -1 && GET_Y_LPARAM(lParam) == -1)
	{
		/*CRect rc;
		GetItemRect(hLastItem, &rc, TRUE);
		CPoint pt(rc.right, rc.top);
		ClientToScreen(&pt);
		doContextMenu(&pt);*/
	}
	else
	{
		CPoint thept(GetMessagePos());
		CPoint* pt(&thept);
		
		CPoint pt2(*pt);

		// Test for keyboard right-click...
		if (pt->x != -1)
		{
			m_view.ScreenToClient(&pt2);

			LVHITTESTINFO lvhti;
			memset(&lvhti, 0, sizeof(LVHITTESTINFO));
			
			lvhti.pt = pt2;
			m_view.HitTest(&lvhti);

			if (lvhti.iItem != -1 && (lvhti.flags & LVHT_ONITEM))
			{
				extensions::IDocument* doc = docFromListItem(lvhti.iItem);
				Document* pndoc = static_cast<Document*>(doc);
				
				CSPopupMenu popup(IDR_POPUP_TABS);

				if (pndoc->HasFile() && pndoc->FileExists())
				{
					tstring fn = pndoc->GetFileName();

					CPidl pidl;
					AtlGetFilePidl2(fn.c_str(), &pidl);

					CSPopupMenu explorer;
					InsertMenu(popup.GetHandle(), popup.GetCount(), MF_BYPOSITION | MF_POPUP, reinterpret_cast<UINT_PTR>(explorer.GetHandle()), _T("Explorer"));
						
					int nCmd;
					BOOL result = m_explorerMenu->TrackPopupMenu(pidl, pt->x, pt->y, m_hWnd, popup.GetHandle(), explorer.GetHandle(), 10000, 11000, nCmd);
					if (result && (nCmd < 10000 || nCmd > 11000))
					{
						pndoc->GetFrame()->PostMessage(WM_COMMAND, nCmd, 0L);
					}
				}
				else
				{
					g_Context.m_frame->TrackPopupMenu(popup.GetHandle(), 0, pt->x, pt->y);
				}
			}
		}
	}

	return 0;
}

/// Add a document to the list
void COpenFilesDocker::AddDocument(extensions::IDocumentPtr& doc)
{
	int index = m_view.InsertItem(0, doc->GetTitle());
	m_view.SetItemData(index, reinterpret_cast<DWORD_PTR>(doc.get()));
	extensions::IDocumentEventSinkPtr docHandler(new DocEventSink(this, doc));
	doc->AddEventSink(docHandler);
	UpdateDocument(doc);
}

/// Remove a document from the list
void COpenFilesDocker::RemoveDocument(extensions::IDocumentPtr& doc)
{
	int itemIndex = findDocument(doc);
	if (itemIndex != -1)
	{
		m_view.DeleteItem(itemIndex);
	}
}

/// Update a document in the list
void COpenFilesDocker::UpdateDocument(extensions::IDocumentPtr& doc)
{
	int itemIndex = findDocument(doc);
	if (itemIndex != -1)
	{
		LVITEM lvi = {0};
		if (doc->GetWriteProtect())
			lvi.iImage = 2;
		else if (doc->GetModified())
			lvi.iImage = 1;
		else
			lvi.iImage = 0;
		lvi.pszText = const_cast<LPTSTR>(doc->GetTitle());
		lvi.iItem = itemIndex;
		lvi.iSubItem = 0;
		lvi.mask = LVIF_IMAGE | LVIF_TEXT;
		m_view.SetItem(&lvi);
	}
}

/// Select a document in the list
void COpenFilesDocker::SelectDocument(extensions::IDocumentPtr& doc)
{
	int itemIndex = findDocument(doc);

	for (int i = 0; i < m_view.GetItemCount(); i++)
	{
		if (i != itemIndex)
		{
			m_view.SetItemState(i, 0, LVIS_SELECTED);
		}
		else
		{
			m_view.SetItemState(itemIndex, LVIS_SELECTED | LVIS_FOCUSED, LVIS_SELECTED | LVIS_FOCUSED);
			m_view.EnsureVisible(itemIndex, FALSE);
		}
	}
}

/// Find a list item given a document
inline int COpenFilesDocker::findDocument(extensions::IDocumentPtr& doc)
{
	for(int i = 0; i < m_view.GetItemCount(); ++i)
	{
		extensions::IDocument* rawdoc = docFromListItem(i);
		if (doc.get() == rawdoc)
		{
			return i;
		}
	}
	
	return -1;
}

/// User has selected an item (double-clicked), activate the view
void COpenFilesDocker::handleUserSelection(int index)
{
	extensions::IDocument* rawdoc = docFromListItem(index);
	Document* pndoc = static_cast<Document*>(rawdoc);
	CChildFrame* frame = pndoc->GetFrame();
	
	if (frame)
	{
		::LockWindowUpdate(frame->GetParent());
		frame->SetFocus();
		::LockWindowUpdate(NULL);
	}
}

/// Single place for the evil cast
inline extensions::IDocument* COpenFilesDocker::docFromListItem(int item)
{
	return reinterpret_cast<extensions::IDocument*>(m_view.GetItemData(item));
}