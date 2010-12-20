/**
 * @file splitview.h
 * @brief Split View
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef splitview_h_included
#define splitview_h_included

#include "view.h"
#include "..\include\wtlsplitter.h"

class CSimpleSplitter;

namespace Views {

typedef enum { splitHorz, splitVert } ESplitType;

class SplitView;

/**
 * Split View
 */
class SplitView : public View
{
public:
	static boost::shared_ptr<SplitView> MakeSplitView(ESplitType splitType, ViewPtr& parent, ViewPtr& view1, ViewPtr& view2);

	virtual ~SplitView();

	/**
	 * Create the splitter window.
	 */
	HWND Create(HWND hWndOwner, LPRECT rc, int controlId);

	/**
	 * Update layout
	 */
	virtual void UpdateLayout();

	/**
	 * Get the window handle for this view.
	 */
	virtual HWND GetHwnd();

	/**
	 * Swap child windows.
	 */
	void SwapChildren(ViewPtr& oldchild, ViewPtr& newChild);

	/**
	 * Find out whether the splitter is focusing on only one pane - i.e. not splitting
	 */
	int GetSinglePaneMode() const;

	/**
	 * Set whether the splitter is focusing on one page or neither.
	 */
	void SetSinglePaneMode(int mode);

	/**
	 * Detach one of the panes - do this only just before destruction.
	 */
	void DetachView(ViewPtr& view);

	/**
	 * Get the other child view.
	 */
	ViewPtr& GetOtherChild(ViewPtr& child);

	/**
	 * Visit this view and child views.
	 */
	virtual void Visit(Visitor& visitor);
	
private:
	explicit SplitView(ESplitType splitType, ViewPtr& parent, ViewPtr& view1, ViewPtr& view2);
	void init();

	ESplitType m_splitType;
	CSimpleSplitter m_wnd;
	ViewPtr m_w1;
	ViewPtr m_w2;
};

}

#endif // #ifndef splitview_h_included