/**
 * @file ChildFrm.cpp
 * @brief Implementation of CChildFrame, the MDI Child window.
 * @author Simon Steele
 * @note Copyright (c) 2002-2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#include "stdafx.h"
#include "resource.h"
#include "childfrm.h"
#include "tools.h"
#include "toolsmanager.h"
#include "outputview.h"
#include "exporters.h"
#include "pndialogs.h"
#include "docprops.h"
#include "include/pagesetupdialog.h"
#include "jumpto.h"
#include "JumpToDialog.h"
#include "jumpview.h"
#include "afiles.h"
#include "scriptregistry.h"
#include "textclips.h"
#include "autocomplete.h"
#include "autocompletehandler.h"
#include "project.h"
#include "projectprops.h"
#include "fileutil.h"
#include "editorcommands.h"
#include "tabbingframework/TabbedMDISave.h"
#include "extapp.h"
#include "include/filefinder.h"
#include "views/splitview.h"

#if defined (_DEBUG)
	#define new DEBUG_NEW
	#undef THIS_FILE
	static char THIS_FILE[] = __FILE__;
#endif

bool CChildFrame::s_bFirstChild = true;

#define USERLIST_TEXTCLIPS 1
#define TOOLS_MENU_INDEX 5

namespace { // Implementation details:

class BaseView : public Views::View
{
public:
	BaseView(CChildFrame* owner) : Views::View(Views::vtUnknown, Views::ViewPtr()), m_owner(owner)
	{

	}

	virtual ~BaseView()
	{
	}

	HWND GetHwnd()
	{
		return m_owner->m_hWnd;
	}

protected:
	void NotifyGotFocus(Views::ViewPtr& focused)
	{
		m_owner->SetLastView(focused);
	}

private:
	CChildFrame* m_owner;
};

}

CChildFrame::CChildFrame(DocumentPtr doc, CommandDispatch* commands, TextClips::TextClipsManager* textclips, AutoCompleteManager* autoComplete) : 
	m_spDocument(doc), 
	m_primeView(new CTextView(doc, Views::ViewPtr(), commands, autoComplete)),
	m_lastTextView(m_primeView),
	m_focusView(m_primeView),
	m_autoComplete(autoComplete),
	m_pCmdDispatch(commands),
	m_pTextClips(textclips),
	m_hImgList(NULL),
	m_bClosing(false),
	m_pScript(NULL),
	m_FileAge(-1),
	m_iFirstToolCmd(ID_TOOLS_DUMMY),
	m_bModifiedOverride(false),
	m_bReadOnly(false),
	m_bIgnoreUpdates(false),
	m_bHandlingCommand(false),
	m_hWndOutput(NULL)
{
	m_po.hDevMode = 0;
	m_po.hDevNames = 0;
	memset(&m_po.rcMargins, 0, sizeof(RECT));
	OPTIONS->LoadPrintSettings(&m_po);

	InitUpdateUI();

	m_baseView.reset(new BaseView(this));
	m_primeView->SetParentView(m_baseView);
}

CChildFrame::~CChildFrame()
{
	if( ToolOwner::HasInstance() )
	{
		// Can't afford to wait, no "completed" events will get through...
		// That isn't actually true any more, but we'll leave it as no wait for now.
		ToolOwner::GetInstance()->KillTools(false, this);
	}

	if(m_hImgList)
		::ImageList_Destroy(m_hImgList);

	if(m_pUIData)
		delete [] m_pUIData;
}

void CChildFrame::OnFinalMessage(HWND /*hWnd*/)
{
	delete this;
}

/**
 * We override UpdateLayout in order to call UpdateBarsPosition in this class,
 * instead of in CFrameWindowImplBase. This way we can automagically have a toolbar
 * at the bottom of the window. 
 */
void CChildFrame::UpdateLayout(BOOL bResizeBars)
{
	RECT rect;
	GetClientRect(&rect);

	// position bars and offset their dimensions
	UpdateBarsPosition(rect, bResizeBars);

	if (/*m_primeView.get() || */m_hWndClient != NULL)
	{
		// Our m_hWndClient will always be the prime view, regardless of type...
		::SetWindowPos(m_hWndClient, NULL, rect.left, rect.top,
			rect.right - rect.left, rect.bottom - rect.top,
			SWP_NOZORDER | SWP_NOACTIVATE);
	}
}

void CChildFrame::UpdateBarsPosition(RECT& rect, BOOL bResizeBars)
{
	// resize toolbar
	if(m_hWndToolBar != NULL && ((DWORD)::GetWindowLong(m_hWndToolBar, GWL_STYLE) & WS_VISIBLE))
	{
		if(bResizeBars)
		{
			// Size of mini bar controlled here...
			::SetWindowPos(m_hWndToolBar, HWND_TOP, rect.left, rect.bottom - MINI_BAR_HEIGHT, rect.right-rect.left, MINI_BAR_HEIGHT, SWP_NOACTIVATE | SWP_NOZORDER);
		}

		RECT rectTB;
		::GetWindowRect(m_hWndToolBar, &rectTB);
		rect.bottom -= rectTB.bottom - rectTB.top;
	}

	if (m_cmdTextBox.m_hWnd != NULL)
	{
		if (bResizeBars)
		{
			m_cmdTextBox.SetWindowPos(HWND_TOP, rect.left, rect.bottom - 22, rect.right-rect.left, 22, SWP_NOACTIVATE | SWP_NOZORDER);
		}

		RECT rectTB;
		m_cmdTextBox.GetWindowRect(&rectTB);
		rect.bottom -= rectTB.bottom - rectTB.top;
	}

	// resize status bar
	if(m_hWndStatusBar != NULL && ((DWORD)::GetWindowLong(m_hWndStatusBar, GWL_STYLE) & WS_VISIBLE))
	{
		if(bResizeBars)
			::SendMessage(m_hWndStatusBar, WM_SIZE, 0, 0);
		RECT rectSB;
		::GetWindowRect(m_hWndStatusBar, &rectSB);
		rect.bottom -= rectSB.bottom - rectSB.top;
	}
}

TBBUTTON MINI_BAR_BUTTONS[6] = 
{
	{ 1, ID_EDITOR_COLOURISE, TBSTATE_ENABLED | TBSTATE_CHECKED, TBSTYLE_CHECK, 0, 0, 0 },
	{ 0, ID_EDITOR_WORDWRAP, TBSTATE_ENABLED, TBSTYLE_CHECK, 0, 0, 0 },
	{ 2, ID_EDITOR_LINENOS, TBSTATE_ENABLED, TBSTYLE_CHECK, 0, 0, 0 },
	{ 3, ID_EDITOR_WHITESPACE, TBSTATE_ENABLED, TBSTYLE_CHECK, 0, 0, 0 },
	{ 4, ID_EDITOR_EOLCHARS, TBSTATE_ENABLED, TBSTYLE_CHECK, 0, 0, 0 },
	{ 5, ID_EDITOR_WRITEPROTECT, TBSTATE_ENABLED, TBSTYLE_CHECK, 0, 0, 0 }
};

void CChildFrame::SetupToolbar()
{
	CToolBarCtrl toolbar;

	bool lowColour = !IsXPOrLater() || OPTIONS->Get(PNSK_INTERFACE, _T("LowColourToolbars"), false);

	CImageList imglist;
	HBITMAP bmp;
	
	if (lowColour)
	{
		imglist.Create(9, 9, ILC_COLOR24 | ILC_MASK, 6, 1);
		bmp = static_cast<HBITMAP>(::LoadImage(ATL::_AtlBaseModule.GetResourceInstance(), MAKEINTRESOURCE(IDB_TBEDITOR24), IMAGE_BITMAP, 54, 9, LR_SHARED));
	}
	else
	{
		imglist.Create(9, 9, ILC_COLOR32 | ILC_MASK, 6, 1);
		bmp = static_cast<HBITMAP>(::LoadImage(ATL::_AtlBaseModule.GetResourceInstance(), MAKEINTRESOURCE(IDB_EDITOR), IMAGE_BITMAP, 54, 9, LR_SHARED | LR_CREATEDIBSECTION));
	}

	imglist.Add(bmp, RGB(255, 0, 255));

	m_hImgList = imglist.Detach();

	CRect rc;
	GetClientRect(rc);
	rc.top = rc.bottom - MINI_BAR_HEIGHT;

	// We fix the size of the mini toolbar to make it suitably small (see MINI_BAR_HEIGHT).
	DWORD dwStyle = WS_CHILD | WS_VISIBLE | /*WS_CLIPCHILDREN | WS_CLIPSIBLINGS |*/ CCS_BOTTOM | 
					CCS_NODIVIDER | TBSTYLE_TOOLTIPS | CCS_NORESIZE | TBSTYLE_FLAT;

	toolbar.Create(m_hWnd, rc, NULL, dwStyle, cwMinibar);
	
	toolbar.SetBitmapSize(CSize(9,9));
	toolbar.SetImageList(m_hImgList);
	
	toolbar.AddButtons(6, &MINI_BAR_BUTTONS[0]);
	toolbar.SetButtonSize(CSize(16, 15));

	m_hWndToolBar = toolbar.Detach();
}

void CChildFrame::EnsureOutputWindow()
{
	if (!m_outputView.get())
	{
		// TODO: This code should share with the split windows code:
		Views::ViewPtr parent(m_primeView->GetParentView());

		CRect rc;
		::GetClientRect(m_primeView->GetHwnd(), rc);
		
		m_outputView.reset(new COutputView);
		COutputView* view = static_cast<COutputView*>(m_outputView.get());

		CRect rc2(rc);
		rc2.top += (rc.Height() / 4) * 3;

		m_hWndOutput = view->Create(m_hWnd, rc2, _T("Output"), WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_CLIPCHILDREN, WS_EX_CLIENTEDGE, cwOutputView);

		Views::ESplitType split = OPTIONS->Get(PNSK_EDITOR, _T("OutputSplitHorizontal"), true) ? Views::splitHorz : Views::splitVert;

		Views::ViewPtr newView(Views::SplitView::MakeSplitView(split, parent, m_primeView, m_outputView));
		Views::SplitView* sv = static_cast<Views::SplitView*>(newView.get());
				
		HWND hWndSplit = sv->Create(parent->GetHwnd(), rc, cwSplitter);

		m_primeView = newView;
		m_hWndClient = hWndSplit;

		UpdateLayout();
	}
}

void CChildFrame::ToggleOutputWindow(bool bSetValue, bool bSetShowing)
{
	bool bVisible(true);
	if (!m_outputView.get())
	{
		EnsureOutputWindow();
		bVisible = false;
	}

	Views::SplitView* sv = static_cast<Views::SplitView*>(m_outputView->GetParentView().get());
	
	bVisible = bVisible && sv->GetSinglePaneMode() == SPLITTER_NORMAL;
	bool bShow = bSetValue ? bSetShowing : !bVisible;

	if(bShow && !bVisible)
	{
		sv->SetSinglePaneMode(SPLITTER_NORMAL);
	}
	else if(!bShow && bVisible)
	{
		sv->SetSinglePaneMode(SPLITTER_TOP);
	}

	UISetChecked(ID_VIEW_INDIVIDUALOUTPUT, bShow);
}

////////////////////////////////////////////////////
// Autocomplete methods

bool CChildFrame::InsertClipCompleted(Scintilla::SCNotification* notification)
{
	std::string text = notification->text;
	int colon = text.find(':');
	text.resize(colon);

	const TextClips::TextClipSet* set = m_pTextClips->GetClips( GetTextView()->GetCurrentScheme()->GetName() );
	if(set != NULL)
	{
		const TextClips::Clip* clip = set->FindByShortcut(text);
		if(clip != NULL)
		{
			GetTextView()->BeginUndoAction();
			GetTextView()->DelWordLeft();
			clip->Insert(GetTextView());
			GetTextView()->EndUndoAction();
		}
	}

	return true;
}

////////////////////////////////////////////////////
// Recording methods

/**
 * Start recording the user actions for a new script
 */
void CChildFrame::StartRecord(extensions::IRecorderPtr recorder)
{
	GetTextView()->StartRecording(recorder);
}

/**
 * Called when the user wants to stop recording or when the window is being closed
 * and a recording is in progress.
 */
void CChildFrame::StopRecord()
{
	GetTextView()->StopRecording();
	g_Context.m_frame->RecordingStopped();
}

/**
 * Find out whether a script recording is in progress.
 */
bool CChildFrame::IsRecording()
{
	return GetTextView()->IsRecording();
}

////////////////////////////////////////////////////
// Document Entries

DocumentPtr CChildFrame::GetDocument() const
{
	return m_spDocument;
}

void CChildFrame::SetTitle(bool bModified)
{
	tstring fn(m_spDocument->GetFileName());
	tstring title(m_spDocument->GetTitle());
	tstring tabTitle(title);

	if (fn.length() && OPTIONS->GetCached(Options::OShowFullPath))
	{
		title = fn.c_str();
	}

	if (bModified)
	{
		title += _T(" *");
		tabTitle += _T(" *");
	}

	::SendMessage(GetParent(), UWM_MDICHILDICONCHANGE, reinterpret_cast<WPARAM>(m_hWnd), m_bReadOnly ? 0 : -1);

	SetWindowText(title.c_str());
	SetTabText(tabTitle.c_str());
	SetTabToolTip(fn.c_str());

	MDIRefreshMenu();
}

tstring CChildFrame::GetFileName(EGFNType type)
{
	return m_spDocument->GetFileName(type);
}

tstring CChildFrame::GetTitle()
{
	return m_spDocument->GetTitle();
}

bool CChildFrame::GetModified()
{
	return GetTextView()->GetModified() || m_bModifiedOverride;
}

bool CChildFrame::CanClose()
{
	bool bRet = true;

	if(GetModified())
	{
		CString title;
		title.Format(IDS_SAVE_CHANGES, GetTitle().c_str());
		int res = MessageBox(title, LS(IDR_MAINFRAME), MB_YESNOCANCEL | MB_ICONQUESTION);
		switch (res)
		{
			case IDYES:
			{
				if( CanSave() )
				{
					return Save(false);
				}
				else
				{
					return SaveAs(false);
				}
			}
			break;

			case IDCANCEL:
			{
				return false;
			}
		} // switch (res)
	}
	return bRet;
}

/**
 * Functor to load lexers we find.
 */
struct LexerLoader
{
	LexerLoader(CTextView* view) : m_view(view){}

	void operator ()(LPCTSTR path, FileFinderData& match, bool& shouldContinue)
	{
		CFileName to_open(match.GetFilename());
		to_open.Root(path);
		CT2CA lexerPath(to_open.c_str());
		m_view->SPerform(SCI_LOADLEXERLIBRARY, 0, StrToLp(lexerPath));
	}

	CTextView* m_view;
};

void CChildFrame::LoadExternalLexers()
{
	tstring sPath;
	OPTIONS->GetPNPath(sPath, PNPATH_SCHEMES);

	FileFinderFunctor<LexerLoader>(LexerLoader(GetTextView())).Find(sPath.c_str(), _T("*.lexer"), false);
}

////////////////////////////////////////////////////
// Message Handlers

LRESULT CChildFrame::OnCreate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	m_hWndClient = GetTextView()->Create(m_hWnd, rcDefault, NULL, WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_CLIPCHILDREN, WS_EX_CLIENTEDGE, cwScintilla);

	if(s_bFirstChild)
	{
		LoadExternalLexers();
		s_bFirstChild = false;
	}

	SetTitle();
	if (OPTIONS->Get(PNSK_INTERFACE, _T("MiniToolbar"), true))
	{
		SetupToolbar();
	}

	CSMenuHandle menu(m_hMenu);

	// This is where we enable the prototype command bar
	if (OPTIONS->Get(PNSK_INTERFACE, _T("Commandbar"), false))
	{
		CRect rcTextBox;
		GetClientRect(rcTextBox);
		rcTextBox.top  = rcTextBox.bottom - 22;
		
		if (m_hWndToolBar != NULL)
			rcTextBox.MoveToY(rcTextBox.top - MINI_BAR_HEIGHT);
		
		m_cmdTextBox.Create(m_hWnd, rcTextBox, _T(""), WS_CHILD | WS_VISIBLE, 0, cwCommandWnd);
		HFONT fn = (HFONT)::GetStockObject(DEFAULT_GUI_FONT);
		m_cmdTextBox.SetFont(fn);

		menu.EnableMenuItem(ID_EDIT_FOCUSCOMMAND, true);
	}

	m_pCmdDispatch->UpdateMenuShortcuts(m_hMenu);

	UISetChecked(ID_EDITOR_COLOURISE, true);
	UISetChecked(ID_EDITOR_WORDWRAP, OPTIONS->GetCached(Options::OWordWrap) != 0);
	UISetChecked(ID_EDITOR_LINENOS, OPTIONS->GetCached(Options::OLineNumbers) != 0);
	UISetChecked(ID_TOOLS_LECONVERT, true);

	GetTextView()->ShowLineNumbers(OPTIONS->GetCached(Options::OLineNumbers) != 0);
	updateViewKeyBindings();

	ExtensionItemList& items = g_Context.ExtApp->GetExtensionMenuItems();
	if (items.size())
	{
		CSPopupMenu plugins;

		BOOST_FOREACH(ExtensionMenuItem* i, items)
		{
			i->BuildMenu(plugins, m_pCmdDispatch);
		}

		menu.InsertSubMenuAtPosition(LS(IDS_EXTENSIONS_MENU), 4, plugins);
		plugins.Detach();
	}

	UpdateMenu();

	bHandled = FALSE;
	return 1;
}

LRESULT CChildFrame::OnMDIActivate(UINT uMsg, WPARAM wParam, LPARAM lParam, BOOL& bHandled)
{
	if (m_hWnd == (HWND)lParam)
	{
		// Activate
		::PostMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, (WPARAM)JUMPVIEW_FILE_ACTIVATE, (LPARAM)this);
	
		::PostMessage(m_hWnd, PN_CHECKAGE, 0, 0);
	}
	//else // Deactivate
	
	UpdateMenu();
	
	// The base-class WM_MDIACTIVATE implementation breaks the window menu for localised builds
	// and there's no way to avoid it but to handle this message completely here.
	bHandled = TRUE;

	BOOL fake(false);
	baseClass::OnMDIActivate(uMsg, wParam, lParam, fake);
	
	if((HWND)lParam == m_hWnd && m_hMenu != NULL)
	{
		setMDIFrameMenu();
	}
	else if((HWND)lParam == NULL)
	{
		::SendMessage(GetMDIFrame(), WM_MDISETMENU, 0, 0);
	}

	return DefWindowProc(uMsg, wParam, lParam);
}

HMENU CChildFrame::getWindowMenu()
{
	CSMenuHandle menu(m_hMenu);
	return menu.GetSubMenu(LS(IDS_MENU_WINDOW));
}

void CChildFrame::setMDIFrameMenu()
{
	HMENU hWindowMenu = getWindowMenu();
	MDISetMenu(m_hMenu, hWindowMenu);
	MDIRefreshMenu();
	::DrawMenuBar(GetMDIFrame());
}

LRESULT CChildFrame::OnClose(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
{
	bHandled = FALSE;

	// PNID_DONTASKUSER is a special value that means "don't ask the user first". Normally for
	// use if we've already asked the user.
	if((lParam != PNID_DONTASKUSER) && !CanClose())
	{
		bHandled = TRUE;
	}
	else
	{
		handleClose();
	}

	return 0;
}

LRESULT CChildFrame::OnCloseNoPrompt(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	bHandled = FALSE;

	handleClose();

	return 0;
}

LRESULT CChildFrame::OnPaint(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	// Paint a background for the toolbar.
	PAINTSTRUCT ps;
	CDC cdc(BeginPaint(&ps));
	
	CRect rectTB;
	::GetWindowRect(m_hWndToolBar, &rectTB);
	CBrush brush;
	brush.CreateSysColorBrush(COLOR_3DFACE);

	cdc.FillRect(rectTB, brush);

	EndPaint(&ps);

	return 0;		
}

LRESULT CChildFrame::OnEraseBackground(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	// Need to do this so the toolbar is drawn...
	return 0;
}

LRESULT CChildFrame::OnForwardMsg(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	LPMSG pMsg = reinterpret_cast<LPMSG>(lParam);

	if(CTabbedMDIChildWindowImpl<CChildFrame>::PreTranslateMessage(pMsg))
		return TRUE;

	if(pMsg->message == WM_KEYDOWN && pMsg->wParam == VK_ESCAPE && GetFocus() == GetTextView()->m_hWnd)
		if(OnEscapePressed())
			return TRUE;

	return GetTextView()->PreTranslateMessage(pMsg);
}

LRESULT CChildFrame::OnCheckAge(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& bHandled)
{
	if(!m_bIgnoreUpdates && !m_bClosing)
	{
		MSG msg;
		if (PeekMessage(&msg, m_hWnd, WM_CLOSE, WM_CLOSE, PM_NOREMOVE) || (PeekMessage(&msg, m_hWnd, WM_SYSCOMMAND, WM_SYSCOMMAND, PM_NOREMOVE) && msg.wParam == SC_CLOSE))
		{
			// There is a quit message in the queue, don't do this.
			return 0;
		}

		CheckAge();
	}

	return 0;
}

LRESULT CChildFrame::OnViewNotify(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	if(lParam == SCN_SAVEPOINTREACHED || lParam == SCN_SAVEPOINTLEFT)
	{
		SetTitle(GetModified());
		m_spDocument->OnModifiedChanged(GetModified());
	}
	else
	{
		SendMessage(GetMDIFrame(), PN_NOTIFY, wParam, lParam);
	}

	return TRUE;
}


void CChildFrame::updateViewKeyBindings()
{
	GetTextView()->SPerform(SCI_CLEARALLCMDKEYS, 0, 0);
	Commands::KeyMap* map = m_pCmdDispatch->GetCurrentScintillaMap();
	const KeyToCommand* key = map->GetMappings();

	for(size_t j = map->GetCount() - 1; j >= 0; j--)
	{	
		GetTextView()->SPerform(SCI_ASSIGNCMDKEY, CodeToScintilla(&key[j]), key[j].msg);
		if (j == 0)
			break;
	}
}

struct OptionsUpdateVisitor : public Views::Visitor
{
	OptionsUpdateVisitor(Scheme* scheme) : _scheme(scheme) {}
	virtual void operator ()(Views::View* view)
	{
		if (view->GetType() == Views::vtText)
		{
			if (_scheme) static_cast<CTextView*>(view)->SetScheme(_scheme);
			static_cast<CTextView*>(view)->ShowLineNumbers(OPTIONS->GetCached(Options::OLineNumbers) != 0);
		}
	}
	Scheme* _scheme;
};

struct ViewUpdateVisitor : public Views::Visitor
{
	ViewUpdateVisitor(CTextView* other) : m_other(other) {}
	virtual void operator ()(Views::View* view)
	{
		if (view->GetType() == Views::vtText)
		{
			CTextView* current = static_cast<CTextView*>(view);
			if (current == m_other)
			{
				return;
			}

			current->SetScheme(m_other->GetCurrentScheme(), scfNoViewSettings | scfNoRestyle);
			current->SetWrapMode(m_other->GetWrapMode());
			current->SetViewEOL(m_other->GetViewEOL());
			current->SetViewWS(m_other->GetViewWS());
			current->SetMarginWidthN(0, m_other->GetMarginWidthN(0));
			current->SetIndentationGuides(m_other->GetIndentationGuides());
			current->SetPasteConvertEndings(m_other->GetPasteConvertEndings());
		}
	}
	CTextView* m_other;
};

/**
 * Split the currently selected view into two, with a new Scintilla control
 * as the new split.
 */
void CChildFrame::splitSelectedView(bool horizontal)
{
	Views::ViewPtr parent = m_focusView->GetParentView();

	// Create the view we're going to split into:
	Views::ViewPtr newTextViewPtr(new CTextView(m_spDocument, Views::ViewPtr(), m_pCmdDispatch, m_autoComplete));	
	CTextView* newTextView = static_cast<CTextView*>(newTextViewPtr.get());
	newTextView->Create(parent->GetHwnd(), rcDefault, NULL, WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_CLIPCHILDREN, WS_EX_CLIENTEDGE, cwScintilla+1);
	newTextView->SetDocPointer(GetTextView()->GetDocPointer());

	// Now we want the parent of the last-focused view to get a new child, the splitter.
	// The splitter will have the last-focused view, and also the new text view.
	Views::ViewPtr newView(Views::SplitView::MakeSplitView(horizontal ? Views::splitHorz : Views::splitVert, parent, m_focusView, newTextViewPtr));
	Views::SplitView* sv = static_cast<Views::SplitView*>(newView.get());

	CRect rc;
	::GetClientRect(m_focusView->GetHwnd(), rc);
	HWND hWndSplit = sv->Create(parent->GetHwnd(), rc, cwViewSplitter);

	if (parent->GetType() == Views::vtSplit)
	{
		// Re-parent the child...
		Views::SplitView* parentSplitter = static_cast<Views::SplitView*>(parent.get());
		parentSplitter->SwapChildren(m_focusView, newView);
	}

	if (m_focusView == m_primeView)
	{
		m_primeView = newView;
		m_hWndClient = hWndSplit;
	}

	// Now the view hierarchy is all sorted, we can restyle the document:
	newTextView->Visit(ViewUpdateVisitor(GetTextView()));

	UpdateLayout();
}

LRESULT CChildFrame::OnOptionsUpdate(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	Scheme* pS = GetTextView()->GetCurrentScheme();
	UpdateTools(pS);

	m_primeView->Visit(OptionsUpdateVisitor(pS));

	// update scintilla shortcuts (does this need to be per view?)
	updateViewKeyBindings();

	UpdateMenu();

	SetTitle(GetModified());

	m_pCmdDispatch->UpdateMenuShortcuts(m_hMenu);

	return 0;
}

LRESULT CChildFrame::OnToggleOutput(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	ToggleOutputWindow(wParam != 0, lParam != 0);
	return 0;
}

LRESULT CChildFrame::OnToolFinished(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	UpdateMenu();

	return 0;
}

LRESULT CChildFrame::OnSchemeChanged(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	SchemeChanged(reinterpret_cast<Scheme*>(lParam));
	::SendMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, (WPARAM)JUMPVIEW_FILE_CLOSE, (LPARAM)this);
	::PostMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, (WPARAM)JUMPVIEW_FILE_ADD, (LPARAM)this);
	return 0;
}

LRESULT CChildFrame::OnProjectNotify(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	UpdateTools(GetTextView()->GetCurrentScheme());
	return 0;
}

LRESULT CChildFrame::OnChildIsModified(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& /*bHandled*/)
{
	if( GetModified() )
	{
		ITabbedMDIChildModifiedItem* pMI = (ITabbedMDIChildModifiedItem*)lParam;
		
		USES_CONVERSION;
		
		std::wstring wstr = T2CW(GetFileName(FN_FILE).c_str());
		pMI->put_DisplayName( wstr.c_str() );
		
		if( CanSave() )
		{
			wstr = T2CW( GetFileName().c_str() );
			pMI->put_Description( wstr.c_str() );
		}
		else
		{
			wstr = L"New File: ";
			wstr += T2CW( (LPCTSTR)GetTextView()->GetCurrentScheme()->GetTitle() );
			pMI->put_Description( wstr.c_str() );
		}

		pMI->put_Window( m_hWnd );

		HICON icon = ::LoadIcon(_Module.m_hInst, MAKEINTRESOURCE(IDR_MDICHILD));
		pMI->put_Icon( icon );

		return TRUE;
	}
	else
		return FALSE;
}

LRESULT CChildFrame::OnChildSaveModified(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM /*lParam*/, BOOL& /*bHandled*/)
{
	if( CanSave() )
	{
		Save(true); 
		return 0;
	}
	else
	{
		// If SaveAs succeeds, we return 0 - we can close. Else return -1, user cancelled.
		return SaveAs(true) ? 0 : -1;
	}
}

LRESULT CChildFrame::OnShowTabContextMenu(UINT /*uMsg*/, WPARAM /*wParam*/, LPARAM lParam, BOOL& bHandled)
{
	bHandled = TRUE;

	POINT point = {GET_X_LPARAM(lParam), GET_Y_LPARAM(lParam)};

	CSPopupMenu popup(IDR_POPUP_TABS);
	g_Context.m_frame->TrackPopupMenu(popup, 0, point.x, point.y, NULL, g_Context.m_frame->GetWindow()->m_hWnd);

	return 0;
}

////////////////////////////////////////////////////
// Command Handlers

LRESULT CChildFrame::OnPrint(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	m_po.Filename = m_spDocument->GetFileName(FN_FULL).c_str();
	GetTextView()->PrintDocument(&m_po, true);

	return TRUE;
}

LRESULT CChildFrame::OnPrintSetup(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	PrintSetup();

	return TRUE;
}

LRESULT CChildFrame::OnOpenContainingFolder(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(CanSave())
	{
		CFileName fn(GetFileName().c_str());
		::ShellExecute(m_hWnd, NULL, fn.GetPath().c_str(), NULL, NULL, SW_SHOWNORMAL);
	}

	return 0;
}

LRESULT CChildFrame::OnShellOpen(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(CanSave())
	{
		::ShellExecute(m_hWnd, _T("open"), GetFileName().c_str(), NULL, NULL, SW_SHOWNORMAL);
	}

	return 0;
}

LRESULT CChildFrame::OnExportRTF(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Export(ExporterFactory::RTF);

	return 0;
}

LRESULT CChildFrame::OnExportHTML(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Export(ExporterFactory::HTML);

	return 0;
}

LRESULT CChildFrame::OnRedo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	GetTextView()->Redo();
	return TRUE;
}

LRESULT CChildFrame::OnDelete(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	GetTextView()->DeleteBack();
	return TRUE;
}

LRESULT CChildFrame::OnFindNext(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	SearchOptions* pOptions = reinterpret_cast<SearchOptions*>( OPTIONS->GetSearchOptions() );
	if( _tcslen(pOptions->GetFindText()) > 0 )
	{
		if( !FindNext(pOptions) )
		{
			CString cs;
			cs.Format(IDS_FINDNOTFOUND, pOptions->GetFindText());
			MessageBox(cs, LS(IDR_MAINFRAME), MB_OK);
		}
	}
	return TRUE;
}

LRESULT CChildFrame::OnFindPrevious(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	SearchOptions* pOptions = reinterpret_cast<SearchOptions*>( OPTIONS->GetSearchOptions() );
	if( _tcslen(pOptions->GetFindText()) > 0 )
	{
		pOptions->SetSearchBackwards(!pOptions->GetSearchBackwards());

		if( !FindNext(pOptions) )
		{
			CString cs;
			cs.Format(IDS_FINDNOTFOUND, pOptions->GetFindText());
			MessageBox(cs, LS(IDR_MAINFRAME), MB_OK);
		}

		pOptions->SetSearchBackwards(!pOptions->GetSearchBackwards());
	}
	
	return TRUE;
}

LRESULT CChildFrame::OnFindNextWordUnderCursor(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	findNextWordUnderCursor(false);

	return 0;
}

LRESULT CChildFrame::OnFindPrevWordUnderCursor(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	findNextWordUnderCursor(true);

	return 0;
}

LRESULT CChildFrame::OnCopyRTF(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	int selectionLength = GetTextView()->GetSelLength();
	int rtfStringLength;
	//If nothing is selected, copy entire file	
	if(selectionLength == 0)
		rtfStringLength = GetTextView()->GetTextLength() * 2;
	else
		rtfStringLength = GetTextView()->GetSelLength() * 2;

	StringOutput so(rtfStringLength);
	std::auto_ptr<StylesList> pStyles(GetTextView()->GetCurrentScheme()->CreateStylesList());
	RTFExporter rtf(&so, GetTextView()->GetCurrentScheme()->GetName(), pStyles.get(), GetTextView());
	
	//If nothing is selected, copy entire file
	if(selectionLength == 0) 
		rtf.Export(0,GetTextView()->GetTextLength());
	else
		rtf.Export(GetTextView()->GetSelectionStart(), GetTextView()->GetSelectionEnd());

	std::string rtfstr(so.c_str());

	HGLOBAL hData = ::GlobalAlloc(GMEM_MOVEABLE | GMEM_ZEROINIT, rtfstr.size() + 1);
	if( hData )
	{
		if( OpenClipboard() )
		{
			::EmptyClipboard();
			char* pBuf = static_cast<char*>(::GlobalLock(hData));
			memcpy(pBuf, rtfstr.c_str(), rtfstr.size() + 1);
			::GlobalUnlock(hData);
			::SetClipboardData(::RegisterClipboardFormat(CF_RTF), hData);
			::CloseClipboard();
		}
	}

	return 0;
}

LRESULT CChildFrame::OnAutoComplete(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	GetTextView()->AttemptAutoComplete();
	return 0;
}

LRESULT CChildFrame::OnCopyFilePath(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(::OpenClipboard(m_hWnd))
	{
		::EmptyClipboard();

		tstring fn = GetFileName();

		HGLOBAL mem = ::GlobalAlloc(GMEM_MOVEABLE, (fn.length() + 1) * sizeof(TCHAR));
		if(mem == NULL)
		{
			::CloseClipboard();
			return 0;
		}

		TCHAR* strbuf = static_cast<TCHAR*>( ::GlobalLock(mem) );
		_tcscpy(strbuf, fn.c_str());
		::GlobalUnlock(mem);

#ifdef _UNICODE
		::SetClipboardData(CF_UNICODETEXT, mem);
#else
		::SetClipboardData(CF_TEXT, mem);
#endif

		::CloseClipboard();
	}

	return 0;
}

LRESULT CChildFrame::OnInsertClip(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	std::string word = GetTextView()->GetCurrentWord();	

	const TextClips::TextClipSet* clips = m_pTextClips->GetClips(GetTextView()->GetCurrentScheme()->GetName());
	
	if(clips == NULL)
	{
		return 0;
	}

	const TextClips::Clip* desired = clips->FindByShortcut(word);
	if(desired != NULL)
	{
		GetTextView()->BeginUndoAction();
		GetTextView()->DelWordLeft();
		desired->Insert(GetTextView());
		GetTextView()->EndUndoAction();
	}
	else
	{
		// Now we want to autocomplete a list of clips:
		AutoCompleteHandlerPtr p(new AutoCompleteAdaptor<CChildFrame>(this, &CChildFrame::InsertClipCompleted));
		GetTextView()->SetAutoCompleteHandler(p);

		std::string cliptext = clips->BuildSortedClipList();
		int sep = GetTextView()->AutoCGetSeparator();
		GetTextView()->AutoCSetSeparator(',');
		GetTextView()->AutoCShow(word.size(), cliptext.c_str());
		GetTextView()->AutoCSetSeparator(sep);
	}

	return 0;
}

LRESULT CChildFrame::OnJumpTo(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CJumpToDialog dlg(this);
	if(dlg.DoModal() == IDOK)
	{
		PostMessage(PN_GOTOLINE, 0, dlg.GetLine());
	}

	return 0;
}

LRESULT CChildFrame::OnRevert(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Revert();

	return 0;
}

LRESULT CChildFrame::OnSave(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	Save(true);

	return 0;
}

LRESULT CChildFrame::OnSaveAs(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	SaveAs(true);

	return 0;
}

LRESULT CChildFrame::OnClose(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	PostMessage(WM_CLOSE, 0, 0);
	return 0;
}


LRESULT CChildFrame::OnWordWrapToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(GetTextView()->GetWrapMode() == SC_WRAP_WORD)
		GetTextView()->SetWrapMode( SC_WRAP_NONE );
	else
		GetTextView()->SetWrapMode( SC_WRAP_WORD );
	//GetTextView()->SetWrapMode( UIInvertCheck(wID) ? SC_WRAP_WORD : SC_WRAP_NONE );
	UpdateMenu();

	return 0;
}

LRESULT CChildFrame::OnColouriseToggle(WORD /*wNotifyCode*/, WORD wID, HWND hWndCtl, BOOL& /*bHandled*/)
{
	GetTextView()->EnableHighlighting(UIInvertCheck(wID));

	UpdateMenu();
	
	return 0;
}

LRESULT CChildFrame::OnLineNoToggle(WORD /*wNotifyCode*/, WORD wID, HWND hWndCtl, BOOL& /*bHandled*/)
{
	GetTextView()->ShowLineNumbers(UIInvertCheck(wID));

	return 0;
}

LRESULT CChildFrame::OnIndividualOutputToggle(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	ToggleOutputWindow();

	return 0;
}

LRESULT CChildFrame::OnMarkWhiteSpaceToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	bool bShow = UIInvertCheck(wID);
	
	GetTextView()->SetViewWS((bShow ? SCWS_VISIBLEALWAYS : SCWS_INVISIBLE));

	return 0;
}

LRESULT CChildFrame::OnEOLMarkerToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	GetTextView()->SetViewEOL(UIInvertCheck(wID));

	return 0;
}

LRESULT CChildFrame::OnWriteProtectToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	// Doesn't make any sense with an in-memory non-saveable document
	if (!CanSave())
	{
		UISetChecked(ID_EDITOR_WRITEPROTECT, false);
		return 0;
	}

	bool bReadOnly = !m_bReadOnly;

	setReadOnly(bReadOnly, true);

	return 0;
}

LRESULT CChildFrame::OnWindowCloseAllOther(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::PostMessage(GetMDIFrame(), PN_CLOSEALLOTHER, reinterpret_cast<WPARAM>(m_hWnd), NULL);

	return 0;
}

LRESULT CChildFrame::OnFileCloseAll(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::PostMessage(GetMDIFrame(), WM_COMMAND, ID_FILE_CLOSEALL, 0);
	
	return 0;
}

LRESULT CChildFrame::OnUseAsScript(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(m_pScript != NULL)
		return 0;

	std::string runner;
	if(ScriptRegistry::GetInstanceRef().SchemeScriptsEnabled(GetTextView()->GetCurrentScheme()->GetName(), runner))
	{
		CT2CA scriptName(GetTitle().c_str());
		m_pScript = new DocScript(scriptName, runner.c_str(), m_spDocument);
		ScriptRegistry::GetInstance()->Add("User Scripts", m_pScript);
	}

	return 0;
}

LRESULT CChildFrame::OnHideOutput(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	ToggleOutputWindow(true, false);
	return 0;
}

LRESULT CChildFrame::OnGoto(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	CString caption;
	caption.Format(_T("&Line Number (1 - %d):"), GetTextView()->GetLineCount());
	CGotoDialog g((LPCTSTR)caption);
	if (g.DoModal() == IDOK)
	{
		GetTextView()->GotoLine(g.GetLineNo() - 1);
	}

	return 0;
}

int CChildFrame::GetLinePosition(int line)
{
	return GetTextView()->PositionFromLine(line-1);
}

/**
 * Provide auto-complete handling combined with Definitions
 * to perform Go To Definition when multiple definitions exist
 */
class DefinitionInsertHandler : 
	public Definitions, 
	public BaseAutoCompleteHandler
{
public:
	/// We're deleted as BaseAutoCompleteHandler*
	virtual ~DefinitionInsertHandler(){}

	/// User made a selection
	virtual bool AutoCSelection(Scintilla::SCNotification* notification)
	{
		for (size_t i = 0; i < Prototypes.size(); ++i)
		{
			if (Prototypes[i] == notification->text)
			{
				Windows[i]->GetTextView()->GotoLine(Lines[i] - 1); //lines based on "0"
				break;
			}
		}

		return true;
	}
};

LRESULT CChildFrame::OnGoToDef(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{    
    // Set up buffer for selected keyword:
	std::string sel = GetTextView()->GetCurrentWord(); //GetTextView()->GetSelText2();
	if (sel.size() == 0)
	{
		return 0;
	}
	
	// Setup stuff for getting all possible definitions:
	boost::shared_ptr<BaseAutoCompleteHandler> insertionHandler(new DefinitionInsertHandler);
	Definitions* defs = static_cast<DefinitionInsertHandler*>(insertionHandler.get());
	defs->SearchTerm = sel;
	
	::SendMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, JUMPVIEW_FIND_DEFINITIONS, reinterpret_cast<LPARAM>(defs));
	
    // Handle definitions: 0: not found, 1: go to source, 2: let user choose.
	if (defs->Lines.size() == 0)
    {
		CString Msg;
		CA2CT selconv(sel.c_str());
		Msg.Format(_T("Definition of '%s' not found"), selconv);
		g_Context.m_frame->SetStatusText(Msg);

        return 0;
    }
	else if (defs->Lines.size() == 1)
    {
		defs->Windows[0]->GetTextView()->GotoLine(defs->Lines[0] - 1); //lines based on "0"
    }
    else
    {
		GetTextView()->SetAutoCompleteHandler(insertionHandler);

		std::string autoclist;
		for (std::vector<std::string>::const_iterator i = defs->Prototypes.begin();
			i != defs->Prototypes.end();
			++i)
        {
			autoclist += (*i);
			autoclist += '|';
        }

		if (!autoclist.size())
		{
			return 0;
		}

		// remove stray end separator
		autoclist.resize(autoclist.size()-1);

		int sep = GetTextView()->AutoCGetSeparator();
		GetTextView()->AutoCSetSeparator('|');
		GetTextView()->UserListShow(0, autoclist.c_str());
		GetTextView()->AutoCSetSeparator(sep);
    }

    return 0;
}

LRESULT CChildFrame::OnGotoLine(UINT /*uMsg*/, WPARAM wParam, LPARAM lParam, BOOL& /*bHandled*/)
{
	int line = (int)lParam-1;

	// Put the line we jump to two off the top of the screen...
	//GetTextView()->GotoLineEnsureVisible(line);
	/*int offset = GetTextView()->GetFirstVisibleLine();
	line = GetTextView()->VisibleFromDocLine(line);
	offset = (line - offset) - 2;
	GetTextView()->LineScroll(0, offset);*/
	
	if (IsIconic())
	{
		ShowWindow(SW_RESTORE);
	}

	if (wParam) 
	{
		int lineLength = GetTextView()->LineLength(line);
		std::vector<char> lineBuf(lineLength + 1);
		lineBuf[lineLength] = '\0';
		GetTextView()->GetLine(line, &lineBuf[0]);

		std::string fullText(&lineBuf[0]);
		std::string methodName(reinterpret_cast<char*>(wParam));
		int i = fullText.find(methodName);
		int pos = GetLinePosition((int)lParam); 
		GetTextView()->SetSel((long)(pos + i),(long)(pos + i + methodName.size()));
	}
	else 
	{
		GetTextView()->GotoLine(line);
	}
	
	GetTextView()->EnsureVisibleEnforcePolicy(line);
	
	SetFocus();

	return 0;
}

LRESULT CChildFrame::OnLineEndingsToggle(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if(wID == ID_TOOLS_LECRLF)
		GetTextView()->SetEOLMode((int)PNSF_Windows);
	else if(wID == ID_TOOLS_LELF)
		GetTextView()->SetEOLMode((int)PNSF_Unix);
	else
		GetTextView()->SetEOLMode((int)PNSF_Mac);
	
	if(UIGetChecked(ID_TOOLS_LECONVERT))
		GetTextView()->ConvertEOLs(GetTextView()->GetEOLMode());

	UpdateMenu();
	return 0;
}

LRESULT CChildFrame::OnLineEndingsConvert(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	UIInvertCheck(ID_TOOLS_LECONVERT);	
	
	return 0;
}

LRESULT CChildFrame::OnStopTools(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	// Don't need to wait, we'll assume the user is still using the document.
	ToolOwner::GetInstance()->KillTools(false, this); 

	return 0;
}

LRESULT CChildFrame::OnUseTabs(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	GetTextView()->SetUseTabs(!GetTextView()->GetUseTabs());
	UpdateMenu();

	return 0;
}

LRESULT CChildFrame::OnHeaderSwitch(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& bHandled)
{
	// Check there's a valid filename...
	if(CanSave())
	{
		tstring filename = m_spDocument->GetFileName();
		tstring alternateFile;

		AlternateFiles* afiles = AlternateFiles::GetInstance();
		
		if(	afiles->GetAlternate(filename.c_str(), alternateFile) )
		{
				if( !g_Context.m_frame->CheckAlreadyOpen(alternateFile.c_str(), eSwitch) )
					g_Context.m_frame->Open(alternateFile.c_str());
		}
		else
			g_Context.m_frame->SetStatusText(_T("No alternate file found."));
	}

	return 0;
}

LRESULT CChildFrame::OnEncodingSelect(WORD /*wNotifyCode*/, WORD wID, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	EPNEncoding oldEncoding = GetTextView()->GetEncoding();
	EPNEncoding encoding = (EPNEncoding)(wID - ID_ENCODING_8);
	
	if(oldEncoding != encoding)
	{
		GetTextView()->SetEncoding(encoding);

		SetModifiedOverride(true);

		UpdateMenu();
	}

	return 0;
}

LRESULT CChildFrame::OnViewFileProps(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	tstring fn = GetFileName(FN_FILE).c_str();
	CPropertySheet sheet( fn.c_str(), 0, m_hWnd );
	sheet.m_psh.dwFlags |= (PSH_NOAPPLYNOW | PSH_PROPTITLE | PSH_USEICONID);
	sheet.m_psh.pszIcon = MAKEINTRESOURCE(IDR_MDICHILD);
	DocumentPropSheet docPropPage(this, _T("Properties"));

	sheet.AddPage(docPropPage);
	if(sheet.DoModal() == IDOK)
	{
		UpdateMenu();

		if(docPropPage.ModifiedDocument())
			SetModifiedOverride(true);
	}

	return 0;
}

LRESULT CChildFrame::OnProjectAddFile(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	return 0;
}

LRESULT CChildFrame::OnSplitHorizontal(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	splitSelectedView(true);

	return 0;
}

LRESULT CChildFrame::OnSplitVertical(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	splitSelectedView(false);

	return 0;
}

class ChildOutputWrapper : public ToolWrapperT<CChildFrame, COutputView>
{
typedef ToolWrapperT<CChildFrame, COutputView> baseClass;
public:
	ChildOutputWrapper(CChildFrame* pOwner, COutputView* pView, CChildFrame* pActiveChild, const ToolDefinition& definition)
		: baseClass(pOwner, pView, pActiveChild, definition)
	{}
};

class TextFilterSink
{
public:
	void AddToolOutput(LPCTSTR output, int nLength = -1)
	{
		if(nLength == -1)
			buffer.append(output);
		else
			buffer.append(output, output+nLength);
	}

	void SetToolBasePath(LPCTSTR path)
	{
	}

	void SetToolParser(bool bBuiltIn, const char* customExpression = NULL)
	{
	}

	void ClearOutput()
	{
	}

	const TCHAR* GetBuffer() const
	{
		return buffer.c_str();
	}

private:
	tstring buffer;
};

class ChildTextFilterWrapper : public ToolWrapperT<CChildFrame, TextFilterSink>
{
	typedef ToolWrapperT<CChildFrame, TextFilterSink> baseClass;
	
public:
	ChildTextFilterWrapper(CChildFrame* pOwner, TextFilterSink* pSink, CChildFrame* pActiveChild, const ToolDefinition& definition)
		: baseClass(pOwner, pSink, pActiveChild, definition)
	{
	}

	virtual ~ChildTextFilterWrapper()
	{
	}

	int Wait(DWORD dwWait)
	{
		return m_hFinishedEvent.Wait(dwWait);
	}

	virtual void ShowOutputWindow()
	{
	}

	virtual void OnFinished()
	{
		m_hFinishedEvent.Set();

		baseClass::OnFinished();
	}

private:
	pnutils::threading::ManualResetEvent m_hFinishedEvent;
};

bool CChildFrame::OnRunTool(LPVOID pTool)
{
	ToolDefinition* pToolDef = static_cast<ToolDefinition*>(pTool);

	ToolWrapperPtr pWrapper;
	boost::shared_ptr<TextFilterSink> filter_sink;
	if(	pToolDef->GlobalOutput() )
	{
		pWrapper.reset( g_Context.m_frame->MakeGlobalOutputWrapper(pToolDef) );
	}
	else if( pToolDef->IsTextFilter() )
	{
		filter_sink.reset(new TextFilterSink());
		pWrapper.reset( new ChildTextFilterWrapper(this, filter_sink.get(), this, *pToolDef) );
	}
	else
	{
		if(pToolDef->CaptureOutput())
			EnsureOutputWindow();
		pWrapper.reset( new ChildOutputWrapper(this, GetOutputWindow(), this, *pToolDef) );
	}

	pWrapper->SetNotifyWindow(m_hWnd);

	if( pToolDef->WantStdIn() )
	{
		Scintilla::TextRange tr;

		// We want to pass our selection/whole document to StdIn.
		if(GetTextView()->GetSelLength() > 0)
		{
			// Selection...
			tr.chrg.cpMin = GetTextView()->GetSelectionStart();
			tr.chrg.cpMax = GetTextView()->GetSelectionEnd();
		}
		else
		{
			// Whole Document
			tr.chrg.cpMin = 0;
			tr.chrg.cpMax = GetTextView()->GetLength();
		}

		int buflength = (tr.chrg.cpMax - tr.chrg.cpMin) + 1;
		if(buflength)
		{
			std::vector<unsigned char> buffer(buflength);
			tr.lpstrText = reinterpret_cast<char*>(&buffer[0]);
			GetTextView()->GetTextRange(&tr);
			pWrapper->SwapInStdInBuffer(buffer);
		}
	}

	ToolOwner::GetInstance()->RunTool(pWrapper, this);

	if( pToolDef->IsTextFilter() )
	{
		ChildTextFilterWrapper* pWaitWrapper = static_cast<ChildTextFilterWrapper*>(pWrapper.get());
		bool bAbort = false;
		while(pWaitWrapper->Wait(10000) == WAIT_TIMEOUT)
		{
			if(::MessageBox(m_hWnd, _T("This text filter is taking a long time to complete,\ndo you want to wait longer?"), LS(IDR_MAINFRAME), MB_YESNO) == IDNO)
			{
				bAbort = true;
				break;
			}
		}

		if(!bAbort)
		{
			// We finished running...
			if(!filter_sink.get())
			{
				RETURN_UNEXPECTED(_T("Expected a filter_sink instance here!"), false);
			}

			GetTextView()->BeginUndoAction();
			if(GetTextView()->GetSelLength() == 0)
			{
				GetTextView()->ClearAll();
			}
			
			CT2CA newtext(filter_sink->GetBuffer());
			GetTextView()->ReplaceSel(newtext);
			GetTextView()->EndUndoAction();
		}
	}

	return true;
}

////////////////////////////////////////////////////
// Notify Handlers

LRESULT CChildFrame::OnGetInfoTip(int idCtrl, LPNMHDR pnmh, BOOL& bHandled)
{
	LPNMTBGETINFOTIP pS = (LPNMTBGETINFOTIP)pnmh;

	::LoadString(_Module.m_hInst, pS->iItem, pS->pszText, pS->cchTextMax);

	return 0;
}

/**
 * Set focus to the command box (if it's enabled).
 */
LRESULT CChildFrame::OnFocusCommand(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if (!m_cmdTextBox.m_hWnd)
	{
		return 0;
	}

	m_cmdTextBox.SetFocus();

	return 0;
}

LRESULT CChildFrame::OnCommandGotFocus(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if (!m_cmdTextBox.m_hWnd)
	{
		return 0;
	}

	// Disable blinking...
	GetTextView()->SetCaretPeriod(0);
	
	// Pretend the editor has the focus...
	GetTextView()->SetEditorFocus(true);

	return 0;
}

LRESULT CChildFrame::OnCommandLostFocus(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	if (!m_cmdTextBox.m_hWnd)
	{
		return 0;
	}

	// Re-enable blinking...
	GetTextView()->SetCaretPeriod(500);

	return 0;
}

/**
 * Set focus to the most recent text view.
 */
LRESULT CChildFrame::OnFocusTextView(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	::SetFocus(m_lastTextView->GetHwnd());

	return 0;
}

LRESULT CChildFrame::OnCommandNotify(WORD wNotifyCode, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	// Yes, this is very much prototype code!
	if (wNotifyCode == EN_CHANGE)
	{
		if (m_bHandlingCommand)
		{
			return 0;
		}

		m_bHandlingCommand = true;
		
		extensions::IScriptRunner* python = ScriptRegistry::GetInstance()->GetRunner("python");
		if (python)
		{
			PN::AString str;
			std::string command("glue.evalCommand('");
			
			CWindowText wt(m_cmdTextBox);

			if ((LPCTSTR)wt == NULL)
			{
				m_bHandlingCommand = false;
				return 0;
			}

			CT2CA commandtext(wt);
			command += commandtext;
			command += "')";

			python->Eval(command.c_str(), str);

			CA2CT newwt(str.Get());
			if (_tcscmp(wt, newwt) != 0)
			{
				m_cmdTextBox.SetWindowText(newwt);
				
				int len = _tcslen(newwt);
				m_cmdTextBox.SetSel(len, len);
			}
		}
		
		m_bHandlingCommand = false;
	}

	return 0;
}

LRESULT CChildFrame::OnCommandEnter(WORD /*wNotifyCode*/, WORD /*wID*/, HWND /*hWndCtl*/, BOOL& /*bHandled*/)
{
	extensions::IScriptRunner* python = ScriptRegistry::GetInstance()->GetRunner("python");
	if (python)
	{
		PN::AString str;
		std::string command("glue.evalCommandEnter('");
		
		CWindowText wt(m_cmdTextBox);
		CT2CA commandtext(wt);
		command += commandtext;
		command += "')";

		python->Eval(command.c_str(), str);

		CA2CT newwt(str.Get());
		m_cmdTextBox.SetWindowText(newwt);
	}

	return 0;
}

////////////////////////////////////////////////////
// File Management Methods

void CChildFrame::CheckAge()
{
	if(CanSave())
	{
		FileUtil::FileAttributes_t atts;
		
		uint64_t age = (uint64_t)~0;
		bool readOnly = false;
		if (FileUtil::GetFileAttributes(m_spDocument->GetFileName(), atts))
		{
			readOnly = FileUtil::IsReadOnly(atts);
			age = FileUtil::GetFileAge(atts);
		}

		if(age != m_FileAge)
		{
			if(m_spDocument->FileExists())
			{
				CString msg;
				msg.Format(IDS_MODIFIEDELSEWHERE, m_spDocument->GetFileName(FN_FULL).c_str());
				
				USES_CONVERSION;
				CT2CW message(msg);

				TASKDIALOGCONFIG cfg = { 0 };
				cfg.cbSize = sizeof(cfg);
				cfg.hInstance = _Module.GetResourceInstance();
				cfg.pszWindowTitle = L"File Changed";
				cfg.pszMainIcon = MAKEINTRESOURCEW(TDT_WARNING_ICON);
				cfg.pszContent = message;
				cfg.dwCommonButtons = TDCBF_YES_BUTTON | TDCBF_NO_BUTTON;
				cfg.pszVerificationText = L"Do not check for updates to this file";
				cfg.nDefaultButton = TDCBF_YES_BUTTON;
				cfg.dwFlags = TDF_ALLOW_DIALOG_CANCELLATION;
				cfg.hwndParent = m_hWnd;

				BOOL doNotShowAgain(FALSE);
				int iRes = PNTaskDialogIndirect(&cfg, 0, &doNotShowAgain);

				if (iRes == IDYES)
				{
					Revert();
					// Update tags:
					::SendMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, (WPARAM)JUMPVIEW_FILE_CLOSE, (LPARAM)this);
					//Manuel Sandoval: clear ScintillaImpl autocomplete list for adding new methods:
					this->GetTextView()->ResetAutoComplete();
					::PostMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, (WPARAM)JUMPVIEW_FILE_ADD, (LPARAM)this);
				}
				else
				{
					m_FileAge = age;
					SetModifiedOverride(true);
					setReadOnly(false, false);
				}

				if (doNotShowAgain)
				{
					m_bIgnoreUpdates = true;
				}
			}
			else
			{
				CString msg;
				msg.Format(IDS_FILENOLONGEREXISTS, m_spDocument->GetFileName(FN_FULL).c_str());
				g_Context.m_frame->SetStatusText((LPCTSTR)msg);
				SetModifiedOverride(true);
				setReadOnly(false, false);
			}
		}
		else
		{
			if (m_bReadOnly != readOnly)
			{
				setReadOnly(readOnly, false);
			}
		}
	}
}

void CChildFrame::Revert()
{
	// Check that we have a valid filename first...
	if(CanSave())
	{
		if (GetWriteProtect())
		{
			setReadOnly(false, false);
		}

		GetTextView()->Revert(m_spDocument->GetFileName(FN_FULL).c_str());

		FileUtil::FileAttributes_t atts;
		if (FileUtil::GetFileAttributes(m_spDocument->GetFileName(FN_FULL).c_str(), atts))
		{
			m_FileAge = FileUtil::GetFileAge(atts);
			setReadOnly(FileUtil::IsReadOnly(atts), false);
		}

		SetModifiedOverride(false);
	}
}

bool CChildFrame::GetWriteProtect()
{
	return m_bReadOnly;
}

bool CChildFrame::PNOpenFile(LPCTSTR pathname, Scheme* pScheme, EPNEncoding encoding)
{
	bool bRet = false;

	if(GetTextView()->Load(pathname, pScheme, encoding))
	{
		FileUtil::FileAttributes_t atts;
		if (FileUtil::GetFileAttributes(pathname, atts))
		{
			m_spDocument->SetFileName(pathname);
			m_FileAge = FileUtil::GetFileAge(atts);
			
			SetTitle(GetModified());
			setReadOnly(FileUtil::IsReadOnly(atts), false);
			
			m_spDocument->OnAfterLoad();
			bRet = true;
		}
	}
	else
	{
		HandleFailedFileOp(pathname, true);
	}
	
	// Loading a file may have changed the line endings/text encoding of the
	// document, so we update the menu...
	UpdateMenu();

	return bRet;
}

bool CChildFrame::SaveFile(LPCTSTR pathname, bool ctagsRefresh, bool bStoreFilename, bool bUpdateMRU)
{
	bool bSuccess = false;

	// If this is a user-relevant save, notify the extensions:
	if(bStoreFilename)
	{
		m_spDocument->OnBeforeSave(pathname);
	}

	// Do the save
	if(GetTextView()->Save(pathname, bStoreFilename))
	{
		bSuccess = true;
	}
	else if (bStoreFilename)
	{
		// Only present UI if we're doing a user-visible save
		switch(HandleFailedFileOp(pathname, false))
		{
		case PNID_SAVEAS:
			bSuccess = SaveAs(ctagsRefresh);
			// Don't update MRU or filename after SaveAs:
			bStoreFilename = false;
			bUpdateMRU = false;
			break;
		case PNID_OVERWRITE:
			if(attemptOverwrite(pathname))
			{
				if(GetTextView()->Save(pathname, bStoreFilename))
				{
					bSuccess = true;
				}
				else
				{
					// If we thought we'd done the overwrite, but then failed to save again...
					CString s;
					s.Format(IDS_SAVEREADONLYFAIL, pathname);
					if(PNTaskDialog(m_hWnd, IDR_MAINFRAME, _T(""), (LPCTSTR)s, TDCBF_YES_BUTTON | TDCBF_NO_BUTTON | TDCBF_CANCEL_BUTTON, TDT_WARNING_ICON) == IDYES)
						bSuccess = SaveAs(ctagsRefresh);
				}
			}
			else
			{
				// If the attempt to overwrite failed.
				CString s;
				s.Format(IDS_SAVEREADONLYFAIL, pathname);
				if(PNTaskDialog(m_hWnd, IDR_MAINFRAME, _T(""), (LPCTSTR)s, TDCBF_YES_BUTTON | TDCBF_NO_BUTTON | TDCBF_CANCEL_BUTTON, TDT_WARNING_ICON) == IDYES)
					bSuccess = SaveAs(ctagsRefresh);
			}
			break;
		}
	}

	// Update state...
	if(bSuccess)
	{
		if(bStoreFilename)
		{
			// If bStoreFilename is *not* true it means we were saving the file for
			// some non-user purpose (saving a copy basically)

			m_FileAge = FileAge(pathname);
			SetModifiedOverride(false);
			m_spDocument->SetFileName(pathname);
			m_spDocument->OnAfterSave();

			// We just saved as a new file, so we're not readonly any more
			if (m_bReadOnly)
			{
				setReadOnly(false, false);
			}

			SetTitle();
		}

		if(bUpdateMRU)
		{
			std::wstring fn(m_spDocument->GetFileName(FN_FULL));
			g_Context.m_frame->AddMRUEntry(fn.c_str());
		}

		if(ctagsRefresh)
		{
			// Update tags:
			::SendMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, (WPARAM)JUMPVIEW_FILE_CLOSE, (LPARAM)this);
			//Manuel Sandoval: clear ScintillaImpl autocomplete list for adding new methods:
			this->GetTextView()->ResetAutoComplete();
			::PostMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, (WPARAM)JUMPVIEW_FILE_ADD, (LPARAM)this);
		}
	}

	return bSuccess;
}

bool CChildFrame::attemptOverwrite(LPCTSTR filename)
{
	return FileUtil::RemoveReadOnly(filename);
}

void CChildFrame::handleClose()
{
	m_bClosing = true;

	// NO PostMessage !!
	::SendMessage(g_Context.m_frame->GetJumpViewHandle(), PN_NOTIFY, (WPARAM)JUMPVIEW_FILE_CLOSE, (LPARAM)this);

	if( ToolOwner::HasInstance() )
	{
		ToolOwner::GetInstance()->KillTools(true, this);
	}

	if(m_pScript)
	{
		ScriptRegistry::GetInstance()->Remove("User Scripts", m_pScript);
		delete m_pScript;
		m_pScript = NULL;
	}

	if (IsRecording())
	{
		StopRecord();
	}

	m_spDocument->OnDocClosing();

	m_spDocument->SetValid(false);	
}

const int SaveReadOnlyButtonsCount = 2;
const int SaveElsewhereButtonsCount = 1;

TASKDIALOG_BUTTON SaveReadOnlyButtons[] = {
         { PNID_SAVEAS, L"Save &As..." },
         { PNID_OVERWRITE, L"&Overwrite" },
      };

TASKDIALOG_BUTTON SaveElsewhereButtons[] = {
         { PNID_SAVEAS, L"Save &As..." },
	  };

int CChildFrame::HandleFailedFileOp(LPCTSTR filename, bool bOpen)
{
	int err = GetLastError();
	
	tstring fstr;

	int MBStyle = bOpen ? TDCBF_OK_BUTTON : (TDCBF_CANCEL_BUTTON | TDCBF_YES_BUTTON | TDCBF_NO_BUTTON);
	TASKDIALOG_BUTTON* pItems = NULL;
	int nItems = 0;
	int nDefault = IDCANCEL;

	CFileName cfn(filename);
	tstring fn = cfn.GetFileName();
	
	switch(err)
	{
	case ERROR_ACCESS_DENIED:
		if (bOpen)
		{
			fstr = CFILE_LoadAccessDenied;
		}
		else
		{
			// Special Read-Only handling...
			fstr = LS(IDS_SAVEREADONLY);
			MBStyle = TDCBF_CANCEL_BUTTON;
			pItems = &SaveReadOnlyButtons[0];
			nItems = SaveReadOnlyButtonsCount;
			nDefault = PNID_SAVEAS;
		}
		break;

	case ERROR_NOT_DOS_DISK:
	case ERROR_WRITE_PROTECT:
		if (bOpen )
		{
			fstr = CFILE_LoadAccessDenied;
		}
		else
		{
			fstr = LS(IDS_SAVEACCESSDENIED);
			MBStyle = TDCBF_CANCEL_BUTTON;
			pItems = &SaveElsewhereButtons[0];
			nItems = SaveElsewhereButtonsCount;
			nDefault = PNID_SAVEAS;
		}
		break;
		
	case ERROR_DISK_FULL:
	case ERROR_HANDLE_DISK_FULL:
		if (bOpen)
		{
			fstr = CFILE_CouldNotLoadError;
		}
		else
		{
			fstr = CFILE_SaveDiskFullError;
			MBStyle = TDCBF_CANCEL_BUTTON;
			pItems = &SaveElsewhereButtons[0];
			nItems = SaveElsewhereButtonsCount;
			nDefault = PNID_SAVEAS;
		}
		break;
		
	case ERROR_SHARING_VIOLATION:
	case ERROR_LOCK_VIOLATION:
		if (bOpen)
		{
			fstr = CFILE_LoadShareViolation;
		}
		else
		{
			fstr = LS(IDS_SAVESHAREVIOLATION);
			MBStyle = TDCBF_CANCEL_BUTTON;
			pItems = &SaveElsewhereButtons[0];
			nItems = SaveElsewhereButtonsCount;
			nDefault = PNID_SAVEAS;
		}
		break;
		
	case ERROR_DEV_NOT_EXIST:
	case ERROR_BAD_NETPATH:
	case ERROR_NETWORK_BUSY:
		if (bOpen)
		{
			fstr = CFILE_NetLoadError;
		}
		else
		{
			fstr = CFILE_NetSaveError;
			MBStyle = TDCBF_CANCEL_BUTTON;
			pItems = &SaveElsewhereButtons[0];
			nItems = SaveElsewhereButtonsCount;
			nDefault = PNID_SAVEAS;
		}
		break;
		
	default:
		if (bOpen)
		{
			fstr = CFILE_CouldNotLoadError;
		}
		else
		{
			fstr = CFILE_CouldNotSaveError;
		}
	}

	int bs = fstr.length() + fn.length() + 10;
	
	tstring buffer;
	buffer.resize(bs);
	int finalLength = _sntprintf(&buffer[0], bs, fstr.c_str(), fn.c_str());
	buffer.resize(finalLength);

	CT2CW message(buffer.c_str());
	std::wstring title(LSW(IDR_MAINFRAME));

	TASKDIALOGCONFIG cfg = { 0 };
	cfg.cbSize = sizeof(cfg);
	cfg.hInstance = _Module.GetResourceInstance();
	cfg.pszWindowTitle = title.c_str();
	cfg.pszMainIcon = MAKEINTRESOURCEW(TDT_WARNING_ICON);
	cfg.pszContent = message;
	cfg.dwCommonButtons = MBStyle;
	cfg.pButtons = pItems;
	cfg.cButtons = nItems;
	cfg.nDefaultButton = PNID_SAVEAS;
	cfg.hwndParent = m_hWnd;

	int iRes = PNTaskDialogIndirect(&cfg);

	// If we didn't change the buttons then we need to translate the result code.
	if (MBStyle == (TDCBF_CANCEL_BUTTON | TDCBF_YES_BUTTON | TDCBF_NO_BUTTON))
	{
		switch (iRes)
		{
		case TDCBF_YES_BUTTON:
			return PNID_SAVEAS;
		default:
			return IDCANCEL;
		}
	}
	
	return iRes;
}

bool CChildFrame::CanSave()
{
	return m_spDocument->HasFile();
}

bool CChildFrame::SaveAs(bool ctagsRefresh)
{
	tstring saPath;
	tstring saFileName;
	if(CanSave())
	{
		saPath = m_spDocument->GetFileName(FN_PATH);
		saFileName = m_spDocument->GetFileName(FN_FILE);
	}

	CAutoSaveDialogEx dlgSave(LS(IDS_ALLFILES));
	
	if (saPath.size())
	{
		dlgSave.SetInitialPath(saPath.c_str());
	}

	if (saFileName.size())
	{
		dlgSave.SetInitialFilename(saFileName.c_str());
	}

	bool bRet = true;

	if(dlgSave.DoModal() == IDOK)
	{
		resetSaveDir();

		EPNSaveFormat format = dlgSave.GetSaveFormat();
		if(format != PNSF_NoChange)
		{
			ChangeFormat(format);
		}
		
		if(dlgSave.GetSingleFileName() == NULL)
		{
			RETURN_UNEXPECTED(_T("SaveAs lpstrFile == NULL"), false);
		}
		
		SaveFile(dlgSave.GetSingleFileName(), ctagsRefresh);
	}
	else
	{
		bRet = false;
	}

	// We may have changed the line endings format, so update the menu.
	UpdateMenu();

	return bRet;
}

void CChildFrame::ChangeFormat(EPNSaveFormat format)
{
	GetTextView()->SetEOLMode( (int)format );
	GetTextView()->ConvertEOLs( (int)format );

	UpdateMenu();
}

bool CChildFrame::Save(bool ctagsRefresh)
{
	if(CanSave())
	{
		std::wstring fn(m_spDocument->GetFileName());
		bool bResult = SaveFile(fn.c_str(), ctagsRefresh, true);
		
		m_FileAge = m_spDocument->GetFileAge();
		SetModifiedOverride(false);

		return bResult;
	}
	else
		return SaveAs(ctagsRefresh);
}

////////////////////////////////////////////////////
// Editor Window Methods	

FindNextResult CChildFrame::FindNext(extensions::ISearchOptions* options)
{
	FindNextResult result = (FindNextResult)GetTextView()->FindNext(options);
	if (result == fnReachedStart && options->GetFindTarget() != extensions::elwAllDocs)
	{
		PNTaskDialog(m_hWnd, IDR_MAINFRAME, IDS_FINDLOOPED, _T(""), TDCBF_OK_BUTTON, TDT_INFORMATION_ICON);
	}
	else if (result != fnFound && options->GetFindTarget() == extensions::elwAllDocs)
	{
		// Prevent each doc in turn from trying to look at the next one
		options->SetFindTarget(extensions::elwCurrentDoc);

		// Activate the next child window, and keep going until we find again
		::SendMessage(GetParent(), WM_MDINEXT, 0, 0);
		CChildFrame* pNext = FromHandle(GetCurrentEditor());
		while (pNext != this)
		{
			// See if this window contains our find text
			if (pNext->FindNext(options) == fnFound)
			{
				result = fnFound;
				break;
			}
			
			// Move on to the next window
			::SendMessage(GetParent(), WM_MDINEXT, 0, 0);
			pNext = FromHandle(GetCurrentEditor());
		}

		options->SetFindTarget(extensions::elwAllDocs);

		if (pNext == this)
		{
			// We went all the way around, and couldn't find anything:
			PNTaskDialog(m_hWnd, IDR_MAINFRAME, IDS_ALLFOUND, _T(""), TDCBF_OK_BUTTON, TDT_INFORMATION_ICON);
		}
	}

	return result;
}

void CChildFrame::MarkAll(extensions::ISearchOptions* options)
{
	GetTextView()->MarkAll(options);
}

bool CChildFrame::Replace(extensions::ISearchOptions* options)
{
	if(options->GetFound())
	{
		return GetTextView()->ReplaceOnce(options);
	}
	else
	{
		GetTextView()->FindNext(options);
		if(options->GetFound())
		{
			return GetTextView()->ReplaceOnce(options);
		}
		else
		{
			return false;
		}
	}
}

int CChildFrame::ReplaceAll(extensions::ISearchOptions* options)
{
	return GetTextView()->ReplaceAll(options);
}

int CChildFrame::GetPosition(EGPType type)
{
	if(type == EP_LINE)
		return GetTextView()->LineFromPosition(GetTextView()->GetCurrentPos());
	else
		return GetTextView()->GetColumn(GetTextView()->GetCurrentPos());
}

void CChildFrame::SetPosStatus(CMultiPaneStatusBarCtrl&	stat)
{
	GetTextView()->SetPosStatus(stat);
}

bool CChildFrame::OnSchemeChange(LPVOID pVoid)
{
	SetScheme(static_cast<Scheme*>(pVoid), scfNoViewSettings);

	return true;
}

bool CChildFrame::OnEditorCommand(LPVOID pCommand)
{
	Commands::EditorCommand* cmd = reinterpret_cast<Commands::EditorCommand*>(pCommand);
	cmd->Apply(*GetTextView());
	
	return true;
}

struct SetSchemeVisitor : public Views::Visitor
{
	SetSchemeVisitor(Scheme* scheme, ESchemeChangeFlags flags) : _scheme(scheme), _flags(flags) {}
	virtual void operator ()(Views::View* view)
	{
		if (view->GetType() == Views::vtText)
		{
			static_cast<CTextView*>(view)->SetScheme(_scheme, _flags);
			_flags |= scfNoRestyle; // only need to restyle once.
		}
	}
	Scheme* _scheme;
	int _flags;
};

void CChildFrame::SetScheme(Scheme* pScheme, bool allSettings)
{
	m_primeView->Visit(SetSchemeVisitor(pScheme, allSettings ? scfNone : scfNoViewSettings));
}

void CChildFrame::UpdateTools(Scheme* pScheme)
{
	CSMenuHandle menu(m_hMenu);
	CSMenuHandle tools( menu.GetSubMenu(LS(IDS_MENU_TOOLS)) );

	tstring projid;

	Projects::Workspace* pAW = g_Context.m_frame->GetActiveWorkspace();
	if(pAW)
	{
		Projects::Project* pAP = pAW->GetActiveProject();
		if(pAP)
		{
			Projects::ProjectTemplate* pT = pAP->GetTemplate();
			if(pT)
			{
				projid = pT->GetID();
			}
		}
	}
	
	m_iFirstToolCmd = ToolsManager::GetInstance()->UpdateToolsMenu(
		tools, m_pCmdDispatch, m_iFirstToolCmd, ID_TOOLS_DUMMY, pScheme->GetName(), projid.size() > 0 ? projid.c_str() : NULL
	);
}

/**
 * Called when the scheme is changed.
 */
void CChildFrame::SchemeChanged(Scheme* pScheme)
{
	UpdateTools(pScheme);
	UpdateMenu();
	g_Context.m_frame->SetActiveScheme(m_hWnd, static_cast<LPVOID>(pScheme));

	m_spDocument->OnSchemeChange(pScheme->GetName());
	
	::PostMessage(GetMDIFrame(), PN_NOTIFY, 0, PN_SCHEMECHANGED);
}

void CChildFrame::UpdateMenu()
{
	CSMenuHandle menu(m_hMenu);

	EPNSaveFormat f = (EPNSaveFormat)GetTextView()->GetEOLMode();
	
	menu.CheckMenuItem(ID_TOOLS_LECRLF, f == PNSF_Windows);
	menu.CheckMenuItem(ID_TOOLS_LECR, f == PNSF_Mac);
	menu.CheckMenuItem(ID_TOOLS_LELF, f == PNSF_Unix);
	menu.CheckMenuItem(ID_TOOLS_USETABS, GetTextView()->GetUseTabs());

	EPNEncoding e = GetTextView()->GetEncoding();

	menu.CheckMenuItem(ID_ENCODING_8, e == eUnknown);
	menu.CheckMenuItem(ID_ENCODING_UTF8, e == eUtf8);
	menu.CheckMenuItem(ID_ENCODING_UTF16BE, e == eUtf16BigEndian);
	menu.CheckMenuItem(ID_ENCODING_UTF16LE, e == eUtf16LittleEndian);
	menu.CheckMenuItem(ID_ENCODING_UTF8NOBOM, e == eUtf8NoBOM);

	UISetChecked(ID_EDITOR_WORDWRAP, GetTextView()->GetWrapMode() == SC_WRAP_WORD);
	UISetChecked(ID_EDITOR_EOLCHARS, GetTextView()->GetViewEOL());
	UISetChecked(ID_EDITOR_WHITESPACE, GetTextView()->GetViewWS() == SCWS_VISIBLEALWAYS);
	UISetChecked(ID_EDITOR_LINENOS, GetTextView()->GetMarginWidthN(0) > 0);
	
	bool bToolsRunning = false;
	if( ToolOwner::HasInstance() )
	{
		bToolsRunning = ToolOwner::GetInstance()->HaveRunningTools(this);
		bToolsRunning |= ToolOwner::GetInstance()->HaveRunningTools(g_Context.m_frame);
	}

	menu.EnableMenuItem(ID_TOOLS_STOPTOOLS, bToolsRunning);

	g_Context.m_frame->SetActiveScheme(m_hWnd, GetTextView()->GetCurrentScheme());

	if (GetTextView()->GetCurrentScheme() != NULL)
	{
		const CommentSpecRec& comments = GetTextView()->GetCurrentScheme()->GetCommentSpec();
		menu.EnableMenuItem(ID_COMMENTS_LINE, comments.CommentLineText[0] != NULL);
		menu.EnableMenuItem(ID_COMMENTS_STREAM, (comments.CommentStreamStart[0] != NULL)
			&& (comments.CommentStreamEnd[0] != NULL));
		menu.EnableMenuItem(ID_COMMENTS_BLOCK, (comments.CommentBlockStart[0] != NULL)
			&& (comments.CommentBlockEnd[0] != NULL));

		menu.EnableMenuItem(ID_COMMENTS_UNCOMMENT, (comments.CommentLineText[0] != NULL)
			|| (comments.CommentStreamStart[0] != NULL)
			|| (comments.CommentStreamEnd[0] != NULL)
			|| (comments.CommentBlockStart[0] != NULL)
			|| (comments.CommentBlockEnd[0] != NULL));
	}
}

bool CChildFrame::IsOutputVisible()
{
	if (!m_outputView.get())
	{
		return false;
	}

	return static_cast<Views::SplitView*>(m_outputView->GetParentView().get())->GetSinglePaneMode() == SPLITTER_NORMAL;
}

/**
 * In here we should kill off any windowy things showing like output windows etc.
 */
BOOL CChildFrame::OnEscapePressed()
{
	if(IsOutputVisible())
	{
        ToggleOutputWindow();
		return TRUE;
	}
	else
		return ::SendMessage(GetTopLevelParent(), PN_ESCAPEPRESSED, 0, 0);
}

/**
 * @brief Generic exporter function
 */
void CChildFrame::Export(int type)
{
	FileOutput fout(NULL);
	std::auto_ptr<StylesList> pStyles(GetTextView()->GetCurrentScheme()->CreateStylesList());
	
	std::auto_ptr<BaseExporter> pExp(ExporterFactory::GetExporter(
		(ExporterFactory::EExporterType)type, 
		&fout, GetTextView()->GetCurrentScheme()->GetName(), pStyles.get(), GetTextView()));
	
	if (!pExp.get())
	{
		return;
	}

	tstring guessName;

	if(CanSave())
	{
		guessName = m_spDocument->GetFileName(FN_FILEPART);
	}
	else
	{
		guessName = _T("untitled");
	}	

	guessName += _T(".");
	guessName += pExp->GetDefaultExtension();

	tstring fileMask(pExp->GetFileMask());
	fileMask += LS(IDS_ALLFILES);

	CAutoSaveDialog dlgSave(fileMask.c_str());
	dlgSave.SetDefaultExtension(pExp->GetDefaultExtension());
	dlgSave.SetInitialFilename(guessName.c_str());
	
	if(dlgSave.DoModal() == IDOK)
	{
		resetSaveDir();

		fout.SetFileName(dlgSave.GetSingleFileName());
		if(fout.IsValid())
		{
			pExp->Export(0, -1);
		}
	}
}

void CChildFrame::SetModifiedOverride(bool bVal)
{
	m_bModifiedOverride = bVal;
	SetTitle(GetModified());
	g_Context.m_frame->GetWindow()->SendMessage(PN_NOTIFY, 0, SCN_UPDATEUI);
}

void CChildFrame::PrintSetup()
{
	SS::CPageSetupDialog psd(PSD_INWININIINTLMEASURE|PSD_ENABLEPAGESETUPTEMPLATE, m_hWnd);

	PAGESETUPDLG& pdlg = psd.m_psd;

	// Set margins if valid.
	if (m_po.rcMargins.left != 0 || m_po.rcMargins.right != 0 ||
			m_po.rcMargins.top != 0 || m_po.rcMargins.bottom != 0) 
	{
		psd.SetMargins(&m_po.rcMargins);
	}

	// retrieve cached options from other uses (can't be persisted between sessions)
	pdlg.hDevMode = m_po.hDevMode;
	pdlg.hDevNames = m_po.hDevNames;

	psd.SetHeaderText(m_po.Header);
	psd.SetFooterText(m_po.Footer);

	if ( psd.DoModal() != IDOK )
	{
		return;
	}

	// retrieve margins.
	psd.GetMargins(&m_po.rcMargins, NULL);

	// cache device mode options.
	m_po.hDevMode = pdlg.hDevMode;
	m_po.hDevNames = pdlg.hDevNames;

	m_po.Header = psd.GetHeaderText();
	m_po.Footer = psd.GetFooterText();

	OPTIONS->SavePrintSettings(&m_po);
}

CTextView* CChildFrame::GetTextView()
{
	// TODO: In SetLastView should we upcast to CTextView and store always with that type?
	return static_cast<CTextView*>(m_lastTextView.get());
}

COutputView* CChildFrame::GetOutputWindow()
{
	EnsureOutputWindow();
	return static_cast<COutputView*>(m_outputView.get());
}

HACCEL CChildFrame::GetToolAccelerators()
{
	SchemeTools* pTools = ToolsManager::GetInstance()->GetToolsFor( GetTextView()->GetCurrentScheme()->GetName() );
	return pTools->GetAcceleratorTable();
}

void CChildFrame::resetSaveDir()
{
	if (OPTIONS->Get(PNSK_GENERAL, _T("ResetCurrentDir"), true))
	{
		// Store the directory we opened into, and then set current back to PN
		//TCHAR curPath[MAX_PATH+1];
		//::GetCurrentDirectory(MAX_PATH+1, curPath);
		//m_lastOpenPath = curPath;
		::SetCurrentDirectory(OPTIONS->GetPNPath(PNPATH_PN));
	}
}

void CChildFrame::setReadOnly(bool newValue, bool setAttributes)
{
	if (OPTIONS->Get(PNSK_GENERAL, _T("EditReadOnly"), false))
	{
		return;
	}

	if (setAttributes)
	{
		if (!newValue)
		{
			// Turning off read only, try and remove readonly from the file:
			FileUtil::FileAttributes_t atts;
			if (FileUtil::GetFileAttributes(m_spDocument->GetFileName(), atts) && FileUtil::IsReadOnly(atts))
			{
				if (!FileUtil::RemoveReadOnly(m_spDocument->GetFileName()))
				{
					::MessageBox(m_hWnd, _T("Unable to remove the write-protection from this file, you will need to save as a different file"), LS(IDR_MAINFRAME), MB_OK | MB_ICONINFORMATION);
				}
			}
		}
		else
		{
			FileUtil::SetReadOnly(m_spDocument->GetFileName());
		}
	}
	
	// We should only set ReadOnly if the file isn't modified...
	if (GetModified() && newValue)
	{
		g_Context.m_frame->SetStatusText(_T("Source file has become Read Only, and this document is modified."));
		m_bReadOnly = false;
		GetTextView()->SetReadOnly(false);
	}
	else
	{
		m_bReadOnly = newValue;
		GetTextView()->SetReadOnly(m_bReadOnly);
	}

	SetTitle(GetModified());

	UISetChecked(ID_EDITOR_WRITEPROTECT, m_bReadOnly, true);
	
	m_spDocument->OnWriteProtectChanged(m_bReadOnly);
}

/**
 * Implement find next word under cursor and find previous word under cursor.
 */
void CChildFrame::findNextWordUnderCursor(bool backwards)
{
	std::string word = GetTextView()->GetCurrentWord();

	if (!word.length())
	{
		return;
	}

	CA2CT findtext(word.c_str());

	extensions::ISearchOptions* opts = OPTIONS->GetSearchOptions();
	opts->SetFindText(findtext);
	opts->SetSearchBackwards(backwards);
	opts->SetMatchCase(false);
	opts->SetUseRegExp(false);
	opts->SetMatchWholeWord(false);
	opts->SetUseSlashes(false);
	opts->SetNoCursorMove(false);

	FindNext(opts);

	// We set this to false so that if the user then presses Shift-F3 we continue to move backwards.
	opts->SetSearchBackwards(false);
}

void CChildFrame::SetLastView(Views::ViewPtr& view)
{
	m_focusView = view;
	if (m_focusView->GetType() == Views::vtText)
	{
		m_lastTextView = m_focusView;
	}
}

////////////////////////////////////////////////////
// PoorMansUI

CChildFrame::_PoorMansUIEntry* CChildFrame::GetDefaultUIMap()
{
	static CChildFrame::_PoorMansUIEntry theMap[] =
	{
		{ID_EDITOR_WORDWRAP, PMUI_MINIBAR | PMUI_MENU},
		{ID_EDITOR_COLOURISE, PMUI_MINIBAR | PMUI_MENU},
		{ID_EDITOR_LINENOS, PMUI_MINIBAR | PMUI_MENU},
		{ID_EDITOR_WHITESPACE, PMUI_MINIBAR | PMUI_MENU},
		{ID_EDITOR_EOLCHARS, PMUI_MINIBAR | PMUI_MENU},
		{ID_EDITOR_WRITEPROTECT, PMUI_MINIBAR | PMUI_MENU},
		{ID_VIEW_INDIVIDUALOUTPUT, PMUI_MENU},
		{ID_TOOLS_LECONVERT, PMUI_MENU},
		// note: This one must be at the end.
		{-1, 0}
	};
	return theMap;
}

CChildFrame::_PoorMansUIEntry* CChildFrame::GetPoorMansUIMap()
{
	return m_pUIData;
}

void CChildFrame::UISetChecked(UINT uID, bool bChecked, bool bUpdate)
{
	CChildFrame::_PoorMansUIEntry* pMap = GetPoorMansUIMap();
	while(pMap->nID != -1)
	{
		if(pMap->nID == uID)
		{
			if(bChecked)
				pMap->wState |= PMUI_CHECKED;
			else
				pMap->wState &= ~PMUI_CHECKED;

			if(bUpdate)
			{
				if((pMap->wState & PMUI_MENU) != 0)
					::CheckMenuItem(m_hMenu, uID, MF_BYCOMMAND | mfchecked[bChecked]);
				if(m_hWndToolBar && (pMap->wState & PMUI_MINIBAR) != 0)
					CToolBarCtrl(m_hWndToolBar).CheckButton(uID, bChecked);
			}
			
			break;
		}

		pMap++;
	}

	g_Context.m_frame->GetWindow()->SendMessage(PN_UPDATECHILDUI, 0, 0);
}

bool CChildFrame::UIGetChecked(UINT uID)
{
	CChildFrame::_PoorMansUIEntry* pMap = GetPoorMansUIMap();
	while(pMap->nID != -1)
	{
		if(pMap->nID == uID)
		{
			return (pMap->wState & PMUI_CHECKED) != 0;
		}

		pMap++;
	}

	return false;
}

bool CChildFrame::UIInvertCheck(UINT uID)
{
	CChildFrame::_PoorMansUIEntry* pMap = GetPoorMansUIMap();
	while(pMap->nID != -1)
	{
		if(pMap->nID == uID)
		{
			bool bChecked = (pMap->wState & PMUI_CHECKED) != 0;
			bChecked = !bChecked;

			if(bChecked)
				pMap->wState |= PMUI_CHECKED;
			else
				pMap->wState &= ~PMUI_CHECKED;

			if((pMap->wState & PMUI_MENU) != 0)
				::CheckMenuItem(m_hMenu, uID, MF_BYCOMMAND | mfchecked[bChecked]);
			if(m_hWndToolBar && (pMap->wState & PMUI_MINIBAR) != 0)
				CToolBarCtrl(m_hWndToolBar).CheckButton(uID, bChecked);

			g_Context.m_frame->GetWindow()->SendMessage(PN_UPDATECHILDUI, 0, 0);

			return bChecked;
		}
		
		pMap++;
	}
	
	return false;
}

void CChildFrame::InitUpdateUI()
{
	CChildFrame::_PoorMansUIEntry* pMap = GetDefaultUIMap();
	int so = sizeof(*pMap);
	m_pUIData = new CChildFrame::_PoorMansUIEntry[so];
	memcpy(m_pUIData, pMap, sizeof(_PoorMansUIEntry) * so);
}