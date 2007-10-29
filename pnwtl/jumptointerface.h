/**
 * @file jumptointerface.h
 * @brief Interface definitions for jump to implementations.
 * @author Simon Steele
 * @note Copyright (c) 2004-2007 Simon Steele - http://untidy.net/
 *
 * Programmers Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */
#ifndef jumptointerface_h__included
#define jumptointerface_h__included

#include "taggers/tagtypes.h"

namespace extensions
{

/**
 * Information about a single tag
 */
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

/**
 * Sink for sending tag finding results to
 */
class ITagSink
{
	public:
		virtual void OnFound(int count, LPMETHODINFO methodInfo) = 0;
};

class ITagSource
{
public:
	/**
	 * Get a list of schemes supported
	 */
	virtual const char* GetSchemesSupported() = 0;
	
	/**
	 * Enumerate tags
	 */
	virtual bool FindTags(ITagSink* sink, const wchar_t* filename, void* userData, MASKSTRUCT mask, const char* scheme) = 0;
};

} // namespace extensions

#endif