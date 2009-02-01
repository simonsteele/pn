/**
 * @file stdafx.h
 * @brief Pre-Compiled Headers file.
 * @author Simon Steele
 * @note Copyright (c) 2004-2007 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#pragma once

// Disable the CRT deprecation warnings in VS 2005
#define _CRT_SECURE_NO_DEPRECATE

// Windows Header Files:
#define WIN32_LEAN_AND_MEAN		// Exclude rarely-used stuff from Windows headers
#include <stdlib.h>
#include <windows.h>
#include <tchar.h>
#include <assert.h>

#include <vector>

#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>

#define PNASSERT assert

#include "../../allocator.h"
#include "../../pnextstring.h"
#include "../../extiface.h"
