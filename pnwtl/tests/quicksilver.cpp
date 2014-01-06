#include "stdafx.h"

#include <boost/test/unit_test.hpp>
#include "../include/liquidmetal.h"

using namespace LiquidMetal;

BOOST_AUTO_TEST_SUITE( quicksilver_tests );

BOOST_AUTO_TEST_CASE( quicksilver_nomatch )
{
	BOOST_REQUIRE_EQUAL(0, QuickSilver("blah").Score("MonkeyMonkey"));
}

BOOST_AUTO_TEST_CASE( quicksilver_exactmatch )
{
	BOOST_REQUIRE_EQUAL(SCORE_MATCH, QuickSilver("MonkeyMonkey").Score("MonkeyMonkey"));
}

BOOST_AUTO_TEST_CASE( quicksilver_bettermatch )
{
	QuickSilver lm1("MM");
	QuickSilver lm2("Mo");
	BOOST_REQUIRE_GT(lm1.Score("MonkeyMonkey"), lm2.Score("MonkeyMonkey"));
}

BOOST_AUTO_TEST_SUITE_END();