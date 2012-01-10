/**
 * @file stdafx.h
 * @brief Pre-compiled header file.
 * @author Simon Steele
 * @note Copyright (c) 2004-2011 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 * 
 * Notes: 
 * Include file for standard system include files, or project specific 
 * include files that are used frequently, but are changed infrequently.
 */

#if !defined(pn2_stdafx_h__included)
#define pn2_stdafx_h__included

#include <list>
#include <string>
#include <map>
#include <vector>
#include <stack>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <functional>

//Boosty Goodness
#include <boost/config.hpp>
#include <boost/xpressive/xpressive.hpp>
#include <boost/foreach.hpp>
#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/bind.hpp>
#include <boost/format.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>

#ifdef _MSC_VER // BEGIN WINDOWS ONLY
	#pragma once

#include "../libpeanut/libpeanut/win/core/coreinclude.h"

#endif // END WINDOWS ONLY

#ifdef PLAT_MAC

#include "../libpeanut/libpeanut/mac/core/coreinclude.h"

#endif

typedef std::basic_string<TCHAR> tstring;

template <typename match_type>
bool safe_get_submatch(match_type& match, typename match_type::string_type& expr, typename match_type::char_type const *name)
{
	try
	{
		expr = match[name].str();
		return true;
	}
	catch (boost::xpressive::regex_error&)
	{
		return false;
	}
}

namespace boost { namespace xpressive {
#ifdef _UNICODE
	typedef wsregex tsregex;
	typedef wsmatch tsmatch;
#else
	typedef sregex tsregex;
	typedef smatch tsmatch;
#endif
}} // namespace boost::xpressive

#ifdef _DEBUG
	#define _NO_COPY(x) private: x(x&){;} x& operator = (x& copy){;}
#else
	#define _NO_COPY(x) /##/
#endif

// Lots of good stuff in here...
#include "pn.h"

#endif // !defined(pn2_stdafx_h__included)
