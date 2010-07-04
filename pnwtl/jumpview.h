/**
 * @file jumpview.h
 * @brief View to display ctags trees.
 * @author Simon Steele, Manuel Sandoval
 * @note Copyright (c) 2002-2008 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef jumpview_h__included
#define jumpview_h__included

#include "include/wtltreems.h"

#include "jumptointerface.h"
#include "jumpto.h"

typedef struct
{
		int imagesNumber;
		LPCTSTR imageName;
} TAGIMAGES;

extern TAGIMAGES jumpToTagImages [TAG_MAX+1];

#define JUMPVIEW_FILE_MIN			0x1
#define JUMPVIEW_FILE_ADD			0x1
#define JUMPVIEW_FILE_CLOSE			0x2
#define JUMPVIEW_FILE_ACTIVATE		0x3
#define JUMPVIEW_FILE_MAX			0x3

#define JUMPVIEW_FIND_DEFINITIONS	0x10

class Definitions
{
public:
	virtual ~Definitions(){}

	std::string SearchTerm;
	std::vector<std::string> Prototypes;
	std::vector<int> Lines;
	std::vector<CChildFrame*> Windows;
};

class ShellImageList;

class CJumpTreeCtrl : public CMSTreeViewCtrl ,ITagSink
{
	typedef CMSTreeViewCtrl baseClass;
	

public:
	DECLARE_WND_CLASS(_T("CtagsTree"))

	CJumpTreeCtrl();
	virtual ~CJumpTreeCtrl();
	HWND GetHandle(){return m_hWnd;};

	BEGIN_MSG_MAP(CJumpTreeCtrl)
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy)
		// Drag and Drop...
		MESSAGE_HANDLER(WM_LBUTTONDBLCLK, OnLButtonDblClick)
		MESSAGE_HANDLER(WM_RBUTTONDOWN, OnRButtonDown)
//		MESSAGE_HANDLER(WM_MOUSEWHEEL, OnMouseWheel)
		MESSAGE_HANDLER(PN_NOTIFY, OnViewNotify)
		COMMAND_ID_HANDLER(ID_CTAGS_COLLAPSALL, OnCollapsAll)
		COMMAND_ID_HANDLER(ID_CTAGS_EXPANDALL, OnExpandAll)
//		ON_NOTIFY_EX_RANGE(TTN_NEEDTEXTA, 0, 0xFFFF, OnToolTipNotify)
//		ON_NOTIFY_EX_RANGE(TTN_NEEDTEXTW, 0, 0xFFFF, OnToolTipNotify)

		REFLECTED_NOTIFY_CODE_HANDLER(NM_RETURN, OnReturn)
		REFLECTED_NOTIFY_CODE_HANDLER(TVN_SELCHANGED, OnSelChanged)
	
	END_MSG_MAP()

	HWND Create(HWND hWndParent, _U_RECT rect = NULL, LPCTSTR szWindowName = NULL,
			DWORD dwStyle = 0, DWORD dwExStyle = 0,
			_U_MENUorID MenuOrID = 0U, LPVOID lpCreateParam = NULL);

	virtual void OnFound(int count, LPMETHODINFO methodInfo);
	HTREEITEM RecursiveInsert(HTREEITEM hRoot, LPMETHODINFO methodInfo);//Manuel Sandoval: Function defined for recursive insertion into tree
	LRESULT OnViewNotify(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/);

private:
	void		deleteFileTree();
	void		deleteFileTreeItem(CChildFrame* pChildFrame);
	void		deleteFileItem(HTREEITEM hRoot);
	void		addFileTree(CChildFrame* pChildFrame);
	void 		activateFileTree(CChildFrame* pChildFrame);
	void		deleteMethodInfo(LPMETHODINFO mi);
	void		recursiveDelete(HTREEITEM hParent);

	void		findDefinitions(Definitions& definitions);
    void		recursiveDefinitionSearch(HTREEITEM hRoot, Definitions& definitions);
	
	// Drag and Drop Handlers
	LRESULT		OnLButtonDblClick(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT		OnRButtonDown(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	// Misc. Message Handlers
	LRESULT		OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	// Command Handlers
	LRESULT		OnCollapsAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT		OnExpandAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	// Notify
	LRESULT		OnReturn(int /*idCtrl*/, LPNMHDR /*pnmh*/, BOOL& /*bHandled*/);

	HTREEITEM				hLastItem;
	ShellImageList*			shellImages;
	CImageList				m_imageList;
};

class CJumpDocker : public CWindowImpl<CJumpDocker>// CPNDockingWindow<CJumpDocker>
{
	typedef CJumpDocker thisClass;
	//typedef CPNDockingWindow<CJumpDocker> baseClass;
	typedef CWindowImpl<CJumpDocker> baseClass;

public:
	DECLARE_WND_CLASS(_T("CJumpDocker"))

	CJumpDocker();
	~CJumpDocker();
	
	enum {
		IDC_JUMPVIEW = 105,
	};

	BEGIN_MSG_MAP(thisClass)
		MESSAGE_HANDLER(WM_CREATE, OnCreate)
		MESSAGE_HANDLER(WM_SIZE, OnSize)
		MESSAGE_HANDLER(WM_DESTROY, OnDestroy)
		MESSAGE_HANDLER(WM_GETMINMAXINFO, OnGetMinMaxInfo)
		MESSAGE_HANDLER(WM_CTLCOLOREDIT, OnCtlColor)
		COMMAND_ID_HANDLER(ID_OUTPUT_HIDE, OnHide)
//		NOTIFY_ID_HANDLER(100, OnTreeNotify)
		NOTIFY_HANDLER(IDC_JUMPVIEW, TVN_GETDISPINFO , OnClipGetInfoTip)
		REFLECT_NOTIFICATIONS()
		//CHAIN_MSG_MAP(baseClass)
	END_MSG_MAP()

	HWND getHandle(){return m_hWndClient;};
protected:
	LRESULT OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT OnSize(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);
	LRESULT OnHide(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/);
	LRESULT OnDestroy(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);
	LRESULT	OnGetMinMaxInfo(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/);

	LRESULT OnCtlColor(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/);

	LRESULT OnTreeNotify(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);
	LRESULT OnClipGetInfoTip(int /*idCtrl*/, LPNMHDR pnmh, BOOL& /*bHandled*/);


	CJumpTreeCtrl		m_view;
	HWND				m_hWndClient;
};

#endif