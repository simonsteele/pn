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
	const char* methodName;		// i.e. Tag
	const char* parentName;		// i.e. class name, package name etc.
	const char* fullText;		// i.e. void myfunction(string, banana);
	long		lineNumber;		// line number of method in file.
	short		image;			// i.e. PNMETHODIMAGE_FUNCTION, PNMETHODIMAGE_...
} METHODINFO, * LPMETHODINFO;

#define TAG_UNKNOWN			0
#define TAG_FUNCTION		1
#define TAG_PROCEDURE		2
#define TAG_CLASS			3
#define TAG_DEFINE			4
#define TAG_ENUM			5
#define TAG_FILENAME		6
#define TAG_ENUMNAME		7
#define TAG_MEMBER			8
#define TAG_PROTOTYPE		9
#define TAG_STRUCTURE		10
#define TAG_TYPEDEF			11
#define TAG_UNION			12
#define TAG_VARIABLE		13

#define TAG_MAX				13

#define TAGM_UNKNOWN		1
#define TAGM_FUNCTION		1 << 1
#define TAGM_PROCEDURE		1 << 2
#define TAGM_CLASS			1 << 3
#define TAGM_DEFINE			1 << 4
#define TAGM_ENUM			1 << 5
#define TAGM_FILENAME		1 << 6
#define TAGM_ENUMNAME		1 << 7
#define TAGM_MEMBER			1 << 8
#define TAGM_PROTOTYPE		1 << 9
#define TAGM_STRUCTURE		1 << 10
#define TAGM_TYPEDEF		1 << 11
#define TAGM_UNION			1 << 12
#define TAGM_VARIABLE		1 << 13
#define TAGM_ALL			~0

static int maskVals[14] = {
	TAGM_UNKNOWN,
	TAGM_FUNCTION,
	TAGM_PROCEDURE,
	TAGM_CLASS,
	TAGM_DEFINE,
	TAGM_ENUM,
	TAGM_FILENAME,
	TAGM_ENUMNAME,
	TAGM_MEMBER,
	TAGM_PROTOTYPE,
	TAGM_STRUCTURE,
	TAGM_TYPEDEF,
	TAGM_UNION,
	TAGM_VARIABLE,
};

static int ltypes[26] = {
	/*a*/ 0,			/*b*/ 0,
	/*c*/ TAG_CLASS,	/*d*/ TAG_DEFINE,
	/*e*/ TAG_ENUM,		/*f*/ TAG_FUNCTION,
	/*g*/ TAG_ENUMNAME,	/*h*/ 0,
	/*i*/ 0,			/*j*/ 0,
	/*k*/ 0,			/*l*/ 0,
	/*m*/ TAG_MEMBER,	/*n*/ 0,
	/*o*/ 0,			/*p*/ TAG_PROTOTYPE,
	/*q*/ 0,			/*r*/ 0,
	/*s*/ TAG_STRUCTURE,/*t*/ TAG_TYPEDEF,
	/*u*/ TAG_UNION,	/*v*/ TAG_VARIABLE,
	/*w*/ 0,			/*x*/ 0,
	/*y*/ 0,			/*z*/ 0
};

static int utypes[26] = {
	/*A*/ 0,			/*B*/ 0,
	/*C*/ 0,			/*D*/ 0,
	/*E*/ 0,			/*F*/ TAG_FILENAME,
	/*G*/ 0,			/*H*/ 0,
	/*I*/ 0,			/*J*/ 0,
	/*K*/ 0,			/*L*/ 0,
	/*M*/ 0,			/*N*/ 0,
	/*O*/ 0,			/*P*/ 0,
	/*Q*/ 0,			/*R*/ 0,
	/*S*/ 0,			/*T*/ 0,
	/*U*/ 0,			/*V*/ 0,
	/*W*/ 0,			/*X*/ 0,
	/*Y*/ 0,			/*Z*/ 0
};

#endif