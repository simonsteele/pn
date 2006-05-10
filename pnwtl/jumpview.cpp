/**
 * @file jumpview.cpp
 * @brief View to display ctags trees.
 * @author Simon Steele
 * @note Copyright (c) 2002-2005 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "pndialogs.h"
#include "jumpto.h"
#include "childfrm.h"
#include "plugins.h"
#include "jumpview.h"


using namespace Projects;

#define TCEX_DRAGTIMER	2

//neu
//#define COLWIDTH_PARENT 90
//#define COLWIDTH_LINE	60
//#define COLWIDTH_TAG	153

typedef struct{
		int imagesNumber;
		LPCTSTR imageName;
}TAGIMAGES;

static TAGIMAGES jumpToTagImages [TAG_MAX+1] = 
{
	{1,_T("unknown")},		/* TAG_UNKNOWN      0*/
	{2,_T("function")},		/* TAG_FUNCTION 	1*/
	{3,_T("procedure")},	/* TAG_PROCEDURE 	2*/
	{4,_T("class")},		/* TAG_CLASS 		3*/
	{5,_T("macro")},		/* TAG_MACRO 		4*/
	{6,_T("enum")},			/* TAG_ENUM 		5*/
	{1,_T("filename")},		/* TAG_FILENAME 	6*/
	{2,_T("enumname")},		/* TAG_ENUMNAME 	7*/
	{7,_T("member")},		/* TAG_MEMBER 		8*/
	{8,_T("prototype")},	/* TAG_PROTOTYPE 	9*/
	{9,_T("structure")},	/* TAG_STRUCTURE 	10*/	
	{10,_T("typedef")},		/* TAG_TYPEDEF 		11*/
	{1,_T("union")},		/* TAG_UNION 		12*/
	{11,_T("variable")},	/* TAG_VARIABLE 	13*/
	{1,_T("namespace")},	/* TAG_NAMESPACE 	14*/
	{1,_T("method")},		/* TAG_METHOD 		15*/
	{1,_T("event")},		/* TAG_EVENT 		16*/
	{1,_T("interface")},	/* TAG_INTERFACE 	17*/
	{1,_T("property")},		/* TAG_PROPERTY 	18*/
	{1,_T("program")},		/* TAG_PROGRAM 		19*/
	{15,_T("constant")},	/* TAG_CONSTANT 	20*/
	{1,_T("label")},		/* TAG_LABEL 		21*/
	{1,_T("singleton")},	/* TAG_SINGLETON 	22*/
	{1,_T("mixin")},		/* TAG_MIXIN 		23*/
	{1,_T("module")},		/* TAG_MODULE 		24*/
	{12,_T("net")},			/* TAG_NET 			25*/
	{13,_T("port")},		/* TAG_PORT 		26*/	
	{14,_T("register")},	/* TAG_REGISTER 	27*/
	{1,_T("task")},			/* TAG_TASK 		28*/
	{1,_T("cursor")},		/* TAG_CURSOR 		29*/
	{1,_T("record")},		/* TAG_RECORD 		30*/
	{1,_T("subtype")},		/* TAG_SUBTYPE 		31*/
	{1,_T("trigger")},		/* TAG_TRIGGER 		32*/	
	{1,_T("set")},			/* TAG_SET 			33*/
	{1,_T("field")},		/* TAG_FIELD 		34*/
	{1,_T("table")},		/* TAG_TABLE 		35*/
	{10,_T("attribute")},	/* TAG_ATTRIBUTE 	36*/
	{4,_T("component")},	/* TAG_COMPONENT 	37*/
	{6,_T("package")},		/* TAG_PACKAGE 		38*/
	{9,_T("entity")},		/* TAG_ENTITY 		39*/
	{9,_T("architecture")},	/* TAG_ARCHITECTURE 40*/
	{2,_T("process")},		/* TAG_PROCESS		41*/
};

//////////////////////////////////////////////////////////////////////////////
// CJumpTreeCtrl
//////////////////////////////////////////////////////////////////////////////
CJumpTreeCtrl::CJumpTreeCtrl()
{
//	EnableToolTips(TRUE);
//	processNotifications = true;
}

CJumpTreeCtrl::~CJumpTreeCtrl()
{
}

HWND CJumpTreeCtrl::Create(HWND hWndParent, _U_RECT rect, LPCTSTR szWindowName ,
		DWORD dwStyle, DWORD dwExStyle,
		_U_MENUorID MenuOrID, LPVOID lpCreateParam)
{
	HWND hWndRet = baseClass::Create(hWndParent, rect.m_lpRect, szWindowName, dwStyle, dwExStyle, MenuOrID.m_hMenu, lpCreateParam);


		// Set up the tab control images.
	m_imageList.Create(16, 16, ILC_COLOR32 | ILC_MASK, 4, 4);
	HBITMAP hBitmap = (HBITMAP)::LoadImage(
										_Module.m_hInst,
										MAKEINTRESOURCE(IDB_TAGTYPES),
										IMAGE_BITMAP, 0, 0, LR_SHARED);

	m_imageList.Add(hBitmap, RGB(255,0,255));
	SetImageList(m_imageList.m_hImageList, TVSIL_NORMAL);

return hWndRet;
}


LRESULT CJumpTreeCtrl::OnViewNotify(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	CChildFrame* pChildFrame= (CChildFrame*)lParam;
	tstring fn;
	std::wstring fnstr;

	if(pChildFrame->GetModified() || !pChildFrame->CanSave())
	{
		CJumpTreeCtrl::activateFileTree(pChildFrame);
	}
	else
	{
		SetRedraw(FALSE); 
		if ((int)wParam==JUMPVIEW_FILE_CLOSE){
			CJumpTreeCtrl::deleteFileTreeItem(pChildFrame);
		} else if ((int)wParam==JUMPVIEW_FILE_ADD){
			CJumpTreeCtrl::addFileTree(pChildFrame);
		} else {
			CJumpTreeCtrl::activateFileTree(pChildFrame);
		}
		SetRedraw(TRUE); 
	}
	return TRUE;
}


void CJumpTreeCtrl::addFileTree(CChildFrame* pChildFrame)
{
	tstring fn, filepart;
CToolTipCtrl pmyToolTip;
	fn=pChildFrame->GetFileName();
	LPCTSTR sFullPath = fn.c_str();
	if( _tcschr(sFullPath, _T('\\')) != NULL )
	{
		CFileName fn(sFullPath);
		filepart = fn.GetFileName();
	}
	else
		filepart = sFullPath;
	HTREEITEM hFolder = InsertItem( filepart.c_str(), 0, 0, TVI_ROOT, TVI_LAST );
	SetItemData(hFolder,reinterpret_cast<DWORD_PTR>( pChildFrame ));
	SetItemState(hFolder, TVIS_BOLD, TVIS_BOLD);
	SetItemState(hFolder, TVIF_DI_SETITEM, TVIF_DI_SETITEM);
	JumpToHandler::GetInstance()->DoJumpTo(pChildFrame, this);
	if( GetChildItem(hFolder)){
		SortChildren(hFolder);
		activateFileTree(pChildFrame);
	} else {
		deleteFileItem(hFolder);
	}
}

void CJumpTreeCtrl::activateFileTree(CChildFrame* pChildFrame)
{
 	HTREEITEM hRoot;
	CChildFrame* childFrameItem;
	hRoot = GetRootItem();
	while (hRoot) {
		childFrameItem = reinterpret_cast<CChildFrame*>(GetItemData(hRoot));
		if (childFrameItem==pChildFrame){
			Expand(hRoot,TVE_EXPAND);
			SelectItem(hRoot);
		} else {
			Expand(hRoot,TVE_COLLAPSE);		
		}
		hRoot=GetNextItem(hRoot, TVGN_NEXT );
	}
}

void CJumpTreeCtrl::deleteFileTree()
{
 	HTREEITEM hRoot;
	hRoot = GetRootItem();
	while (hRoot) {
		deleteFileItem(hRoot);
		hRoot=GetRootItem();
	}
}

void CJumpTreeCtrl::deleteFileTreeItem(CChildFrame* pChildFrame)
{
    CString sText;
 	HTREEITEM hRoot;
	CChildFrame* childFrameItem;
	hRoot = GetRootItem();
	while (hRoot) {
		childFrameItem = reinterpret_cast<CChildFrame*>(GetItemData(hRoot));
		if (childFrameItem==pChildFrame){
			deleteFileItem(hRoot);
			break;
		}
		hRoot=GetNextItem(hRoot, TVGN_NEXT );
	}
}

void CJumpTreeCtrl::deleteFileItem(HTREEITEM hRoot)
{
 	HTREEITEM hChildItem,hItem;
	LPMETHODINFO methodInfoItem;
	hChildItem = GetChildItem(hRoot);
	while( hChildItem != NULL)
	{
		hItem = GetChildItem(hChildItem);
		while( hItem != NULL)
		{ 
			methodInfoItem = reinterpret_cast<LPMETHODINFO>(GetItemData(hItem));
			if (methodInfoItem->methodName)
				delete methodInfoItem->methodName;
			if (methodInfoItem->parentName)
				delete methodInfoItem->parentName;
			if (methodInfoItem->fullText)
				delete methodInfoItem->fullText;
			delete methodInfoItem;
			DeleteItem(hItem);
			hItem = GetChildItem(hChildItem);
		}
		methodInfoItem = reinterpret_cast<LPMETHODINFO>(GetItemData(hChildItem));
		delete methodInfoItem;
		DeleteItem(hChildItem);
		hChildItem = GetChildItem(hRoot);
	}
	DeleteItem(hRoot);
}
 
void CJumpTreeCtrl::OnFound(int count, LPMETHODINFO methodInfo)
{
	int imagesNumber;
	CChildFrame* pChildFrame;
	HTREEITEM hRoot,hChildItem;
	LPMETHODINFO methodInfoItem;
 	hRoot = GetRootItem();
	while (hRoot) {
		pChildFrame = reinterpret_cast<CChildFrame*>(GetItemData(hRoot));
		if (pChildFrame==static_cast<CChildFrame*>(methodInfo->userData)){
			break;
		}
		hRoot=GetNextItem(hRoot, TVGN_NEXT );
	}
	if (!hRoot) 
		return;
	hChildItem = GetChildItem(hRoot);
	while (hChildItem) {
		methodInfoItem = reinterpret_cast<LPMETHODINFO>(GetItemData(hChildItem));
		if (methodInfoItem->type==methodInfo->type){
			break;
		}
		hChildItem=GetNextItem(hChildItem, TVGN_NEXT );
	}
	imagesNumber=jumpToTagImages[methodInfo->type].imagesNumber;
	if (!hChildItem){
		if (methodInfo->type <= TAG_MAX){
			hChildItem = InsertItem( jumpToTagImages[methodInfo->type].imageName, 0, 0, hRoot, TVI_LAST );
			methodInfoItem = new METHODINFO;
			memcpy(methodInfoItem, methodInfo, sizeof(METHODINFO));
			methodInfoItem->methodName=0;
			methodInfoItem->parentName=0;
			methodInfoItem->fullText=0;
			SetItemData(hChildItem,reinterpret_cast<DWORD_PTR>( methodInfoItem ));
			SetItemImage(hChildItem, imagesNumber, imagesNumber);

		} else {
			return;
		}

	}
	hChildItem = InsertItem( methodInfo->methodName, imagesNumber, imagesNumber, hChildItem, TVI_LAST );
	methodInfoItem = new METHODINFO;
	memcpy(methodInfoItem, methodInfo, sizeof(METHODINFO));
	if (methodInfo->methodName){
		methodInfoItem->methodName=new char[strlen(methodInfo->methodName)+1];
		strcpy(methodInfoItem->methodName,methodInfo->methodName);
	}
	if (methodInfo->parentName){
		methodInfoItem->parentName=new char[strlen(methodInfo->parentName)+1];
		strcpy(methodInfoItem->parentName,methodInfo->parentName);
	}
	if (methodInfo->fullText){
		methodInfoItem->fullText=new char[strlen(methodInfo->fullText)+1];
		strcpy(methodInfoItem->fullText,methodInfo->fullText);
	}
	SetItemData(hChildItem,reinterpret_cast<DWORD_PTR>( methodInfoItem ));
}


LRESULT CJumpTreeCtrl::OnLButtonDblClick(UINT /*uMsg*/, WPARAM wParam/**/, LPARAM lParam/**/, BOOL& bHandled)
{
	HTREEITEM hChildItem;
	LPMETHODINFO methodInfoItem;

	bHandled = false;
	hChildItem=GetSelectedItem();
	if (GetParentItem(hChildItem)==0) //If RootItem -> return
		return 0;
	methodInfoItem = reinterpret_cast<LPMETHODINFO>(GetItemData(hChildItem));
	if ( methodInfoItem->methodName == 0) // if ChildItem -> return
		return 0;
	bHandled = true;	// its the Item with line Info -> goto line
	::PostMessage(static_cast<CChildFrame*>(methodInfoItem->userData)->m_hWnd,PN_GOTOLINE,0,(LPARAM)methodInfoItem->lineNumber);
	return 0;
}

LRESULT CJumpTreeCtrl::OnRButtonDown(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = false;
	if (GetRootItem()) {
		CPoint pt(GetMessagePos());
		CSPopupMenu popup(IDR_POPUP_CTAGS);
		g_Context.m_frame->TrackPopupMenu(popup, 0, pt.x, pt.y, NULL, m_hWnd);
	}
	return 0;
}

LRESULT CJumpTreeCtrl::OnCollapsAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	HTREEITEM hRoot;
 	hRoot = GetRootItem();
	while (hRoot) {
		Expand(hRoot,TVE_COLLAPSE);		
		hRoot=GetNextItem(hRoot, TVGN_NEXT );
	}
	return 0;
}

LRESULT CJumpTreeCtrl::OnExpandAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	HTREEITEM hRoot;
 	hRoot = GetRootItem();
	while (hRoot) {
		Expand(hRoot,TVE_EXPAND);
		hRoot=GetNextItem(hRoot, TVGN_NEXT );
	}
	return 0;
}

#if 0
//Notification handler
BOOL CJumpTreeCtrl::OnToolTipNotify(UINT id, NMHDR *pNMHDR,
   LRESULT *pResult)
{
   // need to handle both ANSI and UNICODE versions of the message
   TOOLTIPTEXTA* pTTTA = (TOOLTIPTEXTA*)pNMHDR;
   TOOLTIPTEXTW* pTTTW = (TOOLTIPTEXTW*)pNMHDR;
   CString strTipText;
   UINT nID = pNMHDR->idFrom;
   if (pNMHDR->code == TTN_NEEDTEXTA && (pTTTA->uFlags & TTF_IDISHWND) ||
      pNMHDR->code == TTN_NEEDTEXTW && (pTTTW->uFlags & TTF_IDISHWND))
   {
      // idFrom is actually the HWND of the tool
      nID = ::GetDlgCtrlID((HWND)nID);
   }

   if (nID != 0) // will be zero on a separator
      strTipText.Format("Control ID = %d", nID);

   if (pNMHDR->code == TTN_NEEDTEXTA)
      lstrcpyn(pTTTA->szText, strTipText, sizeof(pTTTA->szText));
   else
      ::MultiByteToWideChar( CP_ACP , 0, strTipText, -1, pTTTW->szText, sizeof(pTTTW->szText) );
   *pResult = 0;
   return TRUE;    // message was handled
}
#endif



LRESULT CJumpTreeCtrl::OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = FALSE;

//	if(m_pDropTarget != NULL)
//	{
		deleteFileTree();
//		HRESULT hr = RevokeDragDrop(m_hWnd);
//		ATLASSERT(SUCCEEDED(hr));
//	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////////
// CJumpDocker
//////////////////////////////////////////////////////////////////////////////

CJumpDocker::CJumpDocker()
{
::OutputDebugString(_T("CJumpDocker::CJumpDocker\n"));
}

CJumpDocker::~CJumpDocker()
{
::OutputDebugString(_T("CJumpDocker::~CJumpDocker\n"));
}

LRESULT CJumpDocker::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	HICON hIconSmall = (HICON)::LoadImage(_Module.GetResourceInstance(), MAKEINTRESOURCE(IDI_CTAGS), 
			IMAGE_ICON, ::GetSystemMetrics(SM_CXSMICON), ::GetSystemMetrics(SM_CYSMICON), LR_DEFAULTCOLOR);
	SetIcon(hIconSmall, FALSE);

	RECT rc;
	GetClientRect(&rc);
	m_hWndClient = m_view.Create(m_hWnd, rc, _T("CtagsTree"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | TVS_HASBUTTONS | TVS_HASLINES | TVS_EDITLABELS | TVS_SHOWSELALWAYS, 0, IDC_JUMPVIEW);

	bHandled = FALSE;

	return 0;
}

LRESULT CJumpDocker::OnSize(UINT /*uMsg*/, WPARAM wParam, LPARAM /*lParam*/, BOOL& bHandled)
{
::OutputDebugString(_T("CJumpDocker::OnSize\n"));
	if(wParam != SIZE_MINIMIZED )
	{
		RECT rc;
		GetClientRect(&rc);
		m_view.SetWindowPos(NULL, rc.left, rc.top, rc.right - rc.left, rc.bottom - rc.top ,SWP_NOZORDER | SWP_NOACTIVATE);
	}

	bHandled = FALSE;

	return 0;
}

LRESULT CJumpDocker::OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	//Hide();

	return 0;
}

LRESULT CJumpDocker::OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
::OutputDebugString(_T("CJumpDocker::OnDestroy\n"));

	bHandled = FALSE;

	return 0;
}

LRESULT CJumpDocker::OnGetMinMaxInfo(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	LPMINMAXINFO mmi = reinterpret_cast<LPMINMAXINFO>(lParam);
	mmi->ptMinTrackSize.x = 80;
	mmi->ptMinTrackSize.y = 100;
	return 0;
}


LRESULT CJumpDocker::OnTreeNotify(int /*idCtrl*/, LPNMHDR pnmh, BOOL& bHandled)
{
	LPNMTREEVIEW pN = reinterpret_cast<LPNMTREEVIEW>(pnmh);
::OutputDebugString(_T("CJumpDocker::OnTreeNotify\n"));

	if(pnmh->code == NM_DBLCLK)
	{
#if 0
		File* file = m_view.GetSelectedFile();
::OutputDebugString(_T("CJumpDocker::OnTreeNotify1\n"));
		if(file != NULL)
		{
			if( !g_Context.m_frame->CheckAlreadyOpen(file->GetFileName(), eSwitch) )
			{
				g_Context.m_frame->Open(file->GetFileName(), true);
				HWND hWndEditor = GetCurrentEditor();
				::PostMessage(hWndEditor, WM_SETFOCUS, NULL, NULL);
			}
		}
#endif
	}
	else
		bHandled = false;

	return 0;
}
LRESULT CJumpDocker::OnClipGetInfoTip(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/)
{
	LPNMTVDISPINFO pGetInfoTip = (LPNMTVDISPINFO)pnmh;
	return 0;
}
/**
 * For some reason the WM_CTLCOLOR* messages do not get to the child
 * controls with the docking windows (todo with reflection). This returns
 * the proper result.
 */
LRESULT CJumpDocker::OnCtlColor(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	return (LRESULT)::GetSysColorBrush( COLOR_WINDOW );
}