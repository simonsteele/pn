/**
 * @file mod_debug.cpp
 * @brief boost::python debug module
 * @author Simon Steele
 * @note Copyright (c) 2009 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"

void pyOutputDebugString(const char* message)
{
	OutputDebugString(message);
}

using namespace boost::python;

BOOST_PYTHON_MODULE(debug)
{
	boost::python::docstring_options docstring_options(true);

	def("OutputDebugString", pyOutputDebugString);
}