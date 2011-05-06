/**
 * @file view.cpp
 * @brief View Basics
 * @author Simon Steele
 * @note Copyright (c) 2009+ Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "view.h"

using namespace Views;

View::~View()
{
}

EViewType View::GetType() const
{
	return m_type;
}

void View::SetParentView(ViewPtr parent)
{
	m_parent = parent;
}

ViewPtr View::GetParentView()
{
	return m_parent.lock();
}

void View::UpdateLayout()
{
}

void View::NotifyGotFocus()
{
	ViewPtr sharedThis(shared_from_this());
	NotifyGotFocus(sharedThis);
}

/**
 * Visit this view and child views.
 */
void View::Visit(Visitor& visitor)
{
	visitor(shared_from_this());
}

void View::NotifyGotFocus(ViewPtr& focused)
{
	ViewPtr view(m_parent.lock());
	if (view.get())
	{
		view->NotifyGotFocus(focused);
	}
}