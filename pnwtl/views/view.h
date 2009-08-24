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

namespace Views {

typedef enum { vtText, vtSplit } EViewType;

class View;
typedef boost::shared_ptr<View> ViewPtr;

/**
 * Base class for nestable views
 */
class View
{
public:
	View(EViewType type, ViewPtr& parent) : m_type(type), m_parent(parent) {}

	virtual ~View();

	/**
	 * Get the type of this view.
	 */
	EViewType GetType() const;

	/**
	 * Update window layout
	 */
	virtual void UpdateLayout();

protected:
	/** 
	 * Get this view's parent.
	 */
	ViewPtr& GetParent() const;

private:
	ViewPtr& m_parent;
	EViewType m_type;
};

}

#endif // #ifndef view_h_included