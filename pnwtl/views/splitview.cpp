/**
 * @file splitview.cpp
 * @brief Split View
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "splitview.h"

using namespace Views;

SplitView::SplitView(ESplitType splitType, ViewPtr& parent, ViewPtr& view1, ViewPtr& view2) : 
	  View(vtSplit, parent), 
	  m_splitType(splitType),
	  m_w1(view1), 
	  m_w2(view2) 
{
}

/**
 * On destruction we might still be responsible for one or more views,
 * we need to remove them here. We then destroy our window if it still exists.
 *
 * Explanation: If we're the current root child for the child view then when the
 * child view is closed our window gets destroyed before we're destructed.
 */
SplitView::~SplitView()
{
	if (m_w1.get())
	{
		::DestroyWindow(m_w1->GetHwnd());
	}

	if (m_w2.get())
	{
		::DestroyWindow(m_w2->GetHwnd());
	}

	if (::IsWindow(m_wnd))
	{
		m_wnd.DestroyWindow();
	}
}

/**
 * This static factory function exists because the SplitView can't use shared_from_this in its constructor.
 */
boost::shared_ptr<SplitView> SplitView::MakeSplitView(ESplitType splitType, ViewPtr& parent, ViewPtr& view1, ViewPtr& view2)
{
	boost::shared_ptr<SplitView> view(new SplitView(splitType, parent, view1, view2));
	view->init();
	return view;
}

/**
 * Initialize Splitter
 */
void SplitView::init()
{
	PNASSERT(m_w1.get());
	PNASSERT(m_w2.get());

	m_w1->SetParentView(shared_from_this());
	m_w2->SetParentView(shared_from_this());

	m_wnd.SetPanes(m_w1->GetHwnd(), m_w2->GetHwnd(), false);
	m_wnd.DisableSinglePaneMode(false);
	m_wnd.SetHorizontal(m_splitType == splitHorz);
}

HWND SplitView::Create(HWND hWndOwner, LPRECT rc, int controlId)
{
    HWND wnd = m_wnd.Create(hWndOwner, rc, _T("ViewSplitter"), 0, 0, controlId);
	m_wnd.CentreSplit();
	return wnd;
}

/**
 * Get the window handle for this view.
 */
HWND SplitView::GetHwnd()
{
	return m_wnd.m_hWnd;
}

void SplitView::UpdateLayout()
{
	m_wnd.UpdateLayout();
}

/**
 * Swap child windows.
 */
void SplitView::SwapChildren(ViewPtr& oldchild, ViewPtr& newChild)
{
	if (m_w1 == oldchild)
	{
		m_w1 = newChild;
	}
	else if (m_w2 == oldchild)
	{
		m_w2 = newChild;
	}
	else
	{
		throw std::exception("Invalid old child passed to SwapChildren");
	}

	m_wnd.SetPanes(m_w1->GetHwnd(), m_w2->GetHwnd(), true);
}

/**
 * Find out whether the splitter is focusing on only one pane - i.e. not splitting
 */
int SplitView::GetSinglePaneMode() const
{
	return m_wnd.GetSinglePaneMode();
}

/**
 * Set whether the splitter is focusing on one page or neither.
 */
void SplitView::SetSinglePaneMode(int mode)
{
	m_wnd.SetSinglePaneMode(mode);
}

/**
 * Detach one of the panes - do this only just before destruction.
 */
void SplitView::DetachView(ViewPtr& view)
{
	if (m_w1 == view)
	{
		m_w1.reset();
	}
	else if (m_w2 == view)
	{
		m_w2.reset();
	}
	else
	{
		throw std::exception("Invalid view passed to DetachView");
	}
}

/**
 * Get the other child view.
 */
ViewPtr& SplitView::GetOtherChild(ViewPtr& child)
{
	if (m_w1 == child)
	{
		return m_w2;
	}
	else if (m_w2 == child)
	{
		return m_w1;
	}
	else
	{
		throw std::exception("Invalid view passed to GetOtherChild");
	}
}

/**
 * Visit this view and child views.
 */
void SplitView::Visit(Visitor& visitor)
{
	visitor(shared_from_this());
	m_w1->Visit(visitor);
	m_w2->Visit(visitor);
}