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

/**
 * Split View
 */
class SplitView : public View
{
public:
	SplitView(ESplitType splitType, ViewPtr& parent, HWND view1, HWND view2);

	/**
	 * Create the splitter window.
	 */
	HWND Create(HWND hWndOwner, LPRECT rc, int controlId);

	/**
	 * Update layout
	 */
	virtual void UpdateLayout();

private:
	ESplitType m_splitType;
	CSimpleSplitter m_wnd;
	HWND m_w1;
	HWND m_w2;
};

}

#endif // #ifndef splitview_h_included