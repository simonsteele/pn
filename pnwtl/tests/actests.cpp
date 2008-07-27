#include "stdafx.h"

#include "../autocomplete.h"
#include "../pnstrings.h"

void testAC()
{
	DefaultAutoComplete ac(true, true);
	ac.RegisterTag("void GetByte(char b)", "GetByte");
	ac.RegisterTag("int GetByte2(char b, int a)", "GetByte2");
	ac.RegisterTag("char GetByte2(char b, char a)", "GetByte2");
	ac.RegisterTag("void CheeseIt()", "CheeseIt");
	
	// Test basic method selection:
	{
		PN::AString str;
		ac.GetWords(str, "Get", 3, false);
		std::string result = str.Get();
		std::vector<std::string> results;
		StringTokenise(result, results, std::string(" "));

		TESTASSERT( results.size() == 2 );
	}

	PN::AString str;
	ac.GetPrototypes(str, 0x1, "GetByte", 7);
	TESTASSERT( str.GetLength() == 15 );
}