#include "stdafx.h"

#include <boost/test/unit_test.hpp>
#include "../include/liquidmetal.h"

using namespace LiquidMetal;

BOOST_AUTO_TEST_SUITE( quicksilver_tests );

BOOST_AUTO_TEST_CASE( quicksilver_nomatch )
{
	BOOST_REQUIRE_EQUAL(0, QuickSilver().Score("MonkeyMonkey", "blah"));
}

BOOST_AUTO_TEST_CASE( quicksilver_exactmatch )
{
	BOOST_REQUIRE_EQUAL(SCORE_MATCH, QuickSilver().Score("MonkeyMonkey", "MonkeyMonkey"));
}

BOOST_AUTO_TEST_CASE( quicksilver_bettermatch )
{
	QuickSilver lm;
	BOOST_REQUIRE_GT(lm.Score("MonkeyMonkey", "MM"), lm.Score("MonkeyMonkey", "Mo"));
}

BOOST_AUTO_TEST_SUITE_END();