/**
 * @file utils.cpp
 * @brief Utilities for working with embedded python
 * @author Simon Steele
 * @note Copyright (c) 2006 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#include "stdafx.h"
#include "utils.h"

std::string extractStringFromPyStr(PyObject* strObj)
{
	std::string ret;
#  if PY_VERSION_HEX >= 0x03000000
	PyObject* bytes = PyUnicode_AsUTF8String(strObj);
	ret = PyBytes_AsString(bytes); 
	if (PyErr_Occurred()) return ""; 
	Py_DECREF(bytes); 
#else
        ret = PyString_AsString(strObj);
#endif
	return ret;
}

bool pyStrCheck(PyObject* strObj)
{
#  if PY_VERSION_HEX >= 0x03000000
	return PyUnicode_Check(strObj);
#else
	return PyString_Check(strObj);
#endif
	
}

std::string getPythonErrorString()
{
    // Extra paranoia...
    if (!PyErr_Occurred())
	{
        return "No Python error";
    }

    PyObject *type, *value, *traceback;
    PyErr_Fetch(&type, &value, &traceback);
    PyErr_Clear();

    std::string message = "Python error: ";
    if (type)
	{
        type = PyObject_Str(type);
		
		message += extractStringFromPyStr(type);
    }
    
	if (value)
	{
        value = PyObject_Str(value);
        message += ": ";
        message += extractStringFromPyStr(value);
    }
    
	Py_XDECREF(type);
    Py_XDECREF(value);
    Py_XDECREF(traceback);

    return message;
}

/*
   Modeled after a function from Mark Hammond.

   Obtains a string from a Python traceback.  This is the exact same string as
   "traceback.print_exception" would return.

   Result is a string which must be free'd using PyMem_Free()
*/
#define TRACEBACK_FETCH_ERROR(what) {errMsg = what; goto done;}

std::string PyTracebackToString(void)
{
	std::string errMsg; /* holds a local error message */
	std::string result; /* a valid, allocated result. */
	
	PyObject *modStringIO = NULL;
	PyObject *modTB = NULL;
	PyObject *obStringIO = NULL;
	PyObject *obResult = NULL;

	PyObject *type, *value, *traceback;

	PyErr_Fetch(&type, &value, &traceback);
	PyErr_NormalizeException(&type, &value, &traceback);
	
#  if PY_VERSION_HEX >= 0x03000000
	modStringIO = PyImport_ImportModule("io");
#else
	modStringIO = PyImport_ImportModule("cStringIO");
#endif

	if (modStringIO==NULL)
		TRACEBACK_FETCH_ERROR("cant import cStringIO\n");

	obStringIO = PyObject_CallMethod(modStringIO, "StringIO", NULL);

	/* Construct a cStringIO object */
	if (obStringIO==NULL)
		TRACEBACK_FETCH_ERROR("cStringIO.StringIO() failed\n");

	modTB = PyImport_ImportModule("traceback");
	if (modTB==NULL)
		TRACEBACK_FETCH_ERROR("cant import traceback\n");

	obResult = PyObject_CallMethod(modTB, "print_exception",
				       "OOOOO",
				       type, value ? value : Py_None,
				       traceback ? traceback : Py_None,
				       Py_None,
				       obStringIO);
				    
	if (obResult==NULL) 
		TRACEBACK_FETCH_ERROR("traceback.print_exception() failed\n");
	Py_DECREF(obResult);

	obResult = PyObject_CallMethod(obStringIO, "getvalue", NULL);
	if (obResult==NULL) 
		TRACEBACK_FETCH_ERROR("getvalue() failed.\n");

	/* And it should be a string all ready to go - duplicate it. */
	if (!pyStrCheck(obResult))
			TRACEBACK_FETCH_ERROR("getvalue() did not return a string\n");

	result = extractStringFromPyStr(obResult);
done:
	
	/* All finished - first see if we encountered an error */
	if (result.empty() && errMsg.size()) {
		result = errMsg;
	}

	Py_XDECREF(modStringIO);
	Py_XDECREF(modTB);
	Py_XDECREF(obStringIO);
	Py_XDECREF(obResult);
	Py_XDECREF(value);
	Py_XDECREF(traceback);
	Py_XDECREF(type);
	
	return result;
}