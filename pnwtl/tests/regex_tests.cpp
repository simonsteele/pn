#include "stdafx.h"

// #define BOOST_TEST_MODULE regex_tests
#include <boost/test/unit_test.hpp>

#include <boost/xpressive/xpressive.hpp>
using namespace boost::xpressive;

BOOST_AUTO_TEST_SUITE( regex_tests );

	BOOST_AUTO_TEST_CASE( end_of_string_expression_does_not_crash )
	{
		// Check this no longer causes an exception
		std::string s("> \"\" ");
		std::string expression("(?P<f>.+):(?P<l>[0-9]+):((?P<c>[0-9]+):)?.*");

		sregex re = sregex::compile(expression);

		regex_search(s, re);
	}

BOOST_AUTO_TEST_SUITE_END();