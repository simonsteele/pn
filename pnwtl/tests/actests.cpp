#include "stdafx.h"

#include <boost/test/unit_test.hpp>

#include "../pnstrings.h"
#include "../autocomplete.h"

struct ac_fixture
{
	ac_fixture() : ac(true, true)
	{
		ac.RegisterTag("void GetByte(char b)", "GetByte");
		ac.RegisterTag("int GetByte2(char b, int a)", "GetByte2");
		ac.RegisterTag("char GetByte2(char b, char a)", "GetByte2");
		ac.RegisterTag("void CheeseIt()", "CheeseIt");
	};

	DefaultAutoComplete ac;
};

BOOST_FIXTURE_TEST_SUITE( autocomplete_suite, ac_fixture );

BOOST_AUTO_TEST_CASE( autocomplete_method_selection )
{
	// Test basic method selection:
	PN::AString str;
	ac.GetWords(str, "Get", 3, false);
	std::string result = str.Get();
	std::vector<std::string> results;
	StringTokenise(result, results, std::string(" "));

	BOOST_CHECK( results.size() == 2 );
}

BOOST_AUTO_TEST_CASE( test_autocomplete_get_prototype )
{
	PN::AString str;
	ac.GetPrototypes(str, 0x1, "GetByte", 7);
	BOOST_CHECK( str.GetLength() == 15 );
}

BOOST_AUTO_TEST_SUITE_END();