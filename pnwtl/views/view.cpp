/**
 * @file view.cpp
 * @brief View Basics
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
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

ViewPtr& View::GetParent() const
{
	return m_parent;
}

void View::UpdateLayout()
{
}