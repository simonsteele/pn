/**
 * @file view.h
 * @brief View Basics
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef view_h_included
#define view_h_included

#include "boost/enable_shared_from_this.hpp"

namespace Views {

typedef enum { vtUnknown, vtText, vtSplit, vtOutput, vtRoot } EViewType;

class View;
typedef boost::shared_ptr<View> ViewPtr;
typedef boost::weak_ptr<View> WeakViewPtr;

/**
 * Abstract base class for View visitors.
 */
struct Visitor
{
	virtual void operator ()(ViewPtr& view) = 0;
};

/**
 * Simple func visitor, useful with lambdas.
 */
struct FuncVisitor : public Visitor
{
	FuncVisitor(std::function<void (ViewPtr& view)> fn) : m_fn(fn){}

	virtual void operator ()(ViewPtr& view)
	{
		m_fn(view);
	}

	std::function<void (ViewPtr& view)> m_fn;
};

/**
 * Base class for nestable views
 */
class View : public boost::enable_shared_from_this<View>
{
public:
	explicit View(EViewType type) : m_type(type) {}

	explicit View(EViewType type, ViewPtr& parent) : m_type(type), m_parent(parent) {}

	virtual ~View();

	/**
	 * Get the type of this view.
	 */
	EViewType GetType() const;

	/**
	 * Get the window handle for this view.
	 */
	virtual HWND GetHwnd() = 0;

	/**
	 * Update window layout
	 */
	virtual void UpdateLayout();

	/**
	 * Notify the parent that you got focused.
	 */
	void NotifyGotFocus();

	/**
	 * Change the parent view.
	 */
	void SetParentView(ViewPtr parent);

	/** 
	 * Get this view's parent.
	 */
	ViewPtr GetParentView();

	/**
	 * Visit this view and child views. If your view contains others,
	 * you should call Visit on them after visitor(this);
	 */
	virtual void Visit(Visitor& visitor);

protected:

	/**
	 * Override to handle focus notifications, make sure to call
	 * base if you're not the bottom view.
	 */
	virtual void NotifyGotFocus(ViewPtr& focused);

private:
	WeakViewPtr m_parent;
	EViewType m_type;
};

}

#endif // #ifndef view_h_included