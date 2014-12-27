/**
 * @file modules.h
 * @brief Defines the modules that will be exported to python using Boost Python
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef pypnmodules_h__included
#define pypnmodules_h__included

#ifdef _MSC_VER
	#pragma once
#endif
#  if PY_VERSION_HEX >= 0x03000000
#define PY_INIT_RET PyObject*
#define PY_INIT_NAME(name) PyInit_##name
#else
#define PY_INIT_RET void
#define PY_INIT_NAME(name) init##name
#endif
extern "C"  __declspec(dllexport) PY_INIT_RET PY_INIT_NAME(debug)();
extern "C"  __declspec(dllexport) PY_INIT_RET PY_INIT_NAME(pn)();
extern "C"  __declspec(dllexport) PY_INIT_RET PY_INIT_NAME(scintilla)();

#endif // #ifndef pypnmodules_h__included