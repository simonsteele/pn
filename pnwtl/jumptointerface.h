/**
 * @file tools.h
 * @brief Interface definitions for jump to implementations.
 * @author Simon Steele
 * @note Copyright (c) 2004 Simon Steele <s.steele@pnotepad.org>
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef jumptointerface_h__included
#define jumptointerface_h__included

typedef struct tagMethodInfo
{
	int			type;			// i.e. PNMETHOD_FUNCTION, PNMETHOD_PROCEDURE, PNMETHOD_CLASS etc.
	char*		methodName;		// i.e. Tag
	char*		parentName;		// i.e. class name, package name etc.
	char*		fullText;		// i.e. void myfunction(string, banana);
	long		lineNumber;		// line number of method in file.
	short		image;			// i.e. PNMETHODIMAGE_FUNCTION, PNMETHODIMAGE_...
	void* 		userData;		// from where
} METHODINFO, * LPMETHODINFO;

#include "taggers/tagtypes.h"

#endif