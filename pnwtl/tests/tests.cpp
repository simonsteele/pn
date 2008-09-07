// tests.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

extern void testString();
extern void testAC();
extern void testRegex();

int _tmain(int argc, _TCHAR* argv[])
{
	testString();
	testAC();
	testRegex();

	return 0;
}

